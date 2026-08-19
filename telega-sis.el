;;; telega-sis.el --- 让 telega 聊天缓冲区随光标位置自动切换输入法（基于 sis） -*- lexical-binding: t; -*-

;; 功能说明：
;;   在 telega 的聊天缓冲区 (chatbuf) 中：
;;     - 光标第一次进入底部输入编辑区（point >= telega-chatbuf--input-marker）时：
;;         自动切为「中文（sis 的 other）」；
;;     - 光标从输入区离开（移动到历史消息区 / 按钮区 / 只读区）时：
;;         自动切为「英文（sis 的 english）」，并记住离开前是否是中文；
;;     - 之后光标再移回输入区时：恢复成「上次离开输入区时的状态」
;;         （即：你在输入区手动切到英文，去翻完消息回来依然是英文；
;;           你手动切到中文，回来就是中文）。
;;   在输入区内部、只读区内部，本插件完全不干预输入法，
;;   你可以自由用 sis-switch / C-\ 在中英文之间来回切，本插件不会再把它改回去。
;;   进入 telega-root（聊天列表）缓冲区时，也会自动切到英文。
;;
;; 依赖：
;;   - telega (telega-chat-mode)，要求存在 `telega-chatbuf--input-marker'
;;   - sis   已正确安装并配置好 ISM（macism/fcitx5/ibus/w32/emp 等）
;;
;; 使用方式：
;;   (with-eval-after-load 'telega-chat
;;     (load-file "/path/to/telega-sis.el")
;;     (telega-sis-mode 1))
;;
;; 开关：
;;   - M-x telega-sis-mode 切换启用 / 关闭
;;   - `telega-sis-rootbuf-switch-to-english'：进入 rootbuf 时是否切英文，默认 t
;;   - `telega-sis-initial-input-state'：首次进入某个 chatbuf 输入区时的初始状态，
;;       可选 'other（默认，中文）/ 'english（英文）/ nil（不切换，保留当前状态）

(require 'sis)
(require 'telega-chat nil t)
(eval-when-compile (require 'cl-lib))

(defgroup telega-sis nil
  "Auto switch input method in telega via `sis'."
  :group 'telega
  :prefix "telega-sis-")

(defcustom telega-sis-rootbuf-switch-to-english t
  "非 nil 时，进入 telega-root（聊天列表）缓冲区也自动切到英文。"
  :type 'boolean
  :group 'telega-sis)

(defcustom telega-sis-initial-input-state 'other
  "首次进入某个聊天输入区时要切到的状态。
可选 'other（中文）、'english（英文）、nil（不自动切换，保持当前输入法）。"
  :type '(choice (const other) (const english) (const nil))
  :group 'telega-sis)

;; ---- 内部状态 -----------------------------------------------------------

;; 每个 chatbuf 独立记住"上次离开输入区时 sis 的状态"
;;   'other   -> 离开前是中文，回来时恢复中文
;;   'english -> 离开前是英文，回来时保持英文
;;   'initial -> 还从未进入过输入区，首次进入时使用 `telega-sis-initial-input-state'
(defvar-local telega-sis--input-last-state 'initial
  "Per-buffer state: sis state last seen inside this chatbuf's input area.
One of 'other / 'english / 'initial.")

;; 全局：记录"上一次 post-command 判定出的区域"，只在区域变化时才切换。
;; 取值：
;;   'input     - chatbuf 输入编辑区
;;   'readonly  - chatbuf 历史消息/按钮等只读区
;;   'root      - telega-root 聊天列表
;;   'other     - 非 telega 缓冲区（或刚启用本模式尚未判定）
(defvar telega-sis--last-zone 'other)

;; 防止在切换动作内部递归触发
(defvar telega-sis--inhibit nil)

;; ---- 判断函数 -----------------------------------------------------------

(defun telega-sis--classify-zone ()
  "返回当前光标所在区域，取值见 `telega-sis--last-zone'。"
  (cond
   ((and (derived-mode-p 'telega-chat-mode)
         (bound-and-true-p telega-chatbuf--input-marker)
         (markerp telega-chatbuf--input-marker)
         (>= (point) telega-chatbuf--input-marker))
    'input)
   ((derived-mode-p 'telega-chat-mode)
    'readonly)
   ((and telega-sis-rootbuf-switch-to-english
         (derived-mode-p 'telega-root-mode))
    'root)
   (t 'other)))

;; ---- 切换动作（带递归保护）---------------------------------------------

(defun telega-sis--switch-to (target)
  "把 sis 切到 TARGET（'english 或 'other），仅在确实需要时才切。
内部做递归保护，避免和 sis 自身 hook 相互触发。"
  (when (and (not telega-sis--inhibit)
             (memq target '(english other)))
    (let ((telega-sis--inhibit t))
      (sis--get)                      ; 同步一次 sis 的真实状态
      (unless (eq sis--current target)
        (if (eq target 'english)
            (sis--set-english)
          (sis--set-other))))))

;; ---- 区域变更时的处理 ---------------------------------------------------

(defun telega-sis--on-zone-change (new-zone)
  "刚从 `telega-sis--last-zone' 切换到 NEW-ZONE，执行对应的输入法切换与状态记录。"
  (let ((old-zone telega-sis--last-zone))
    (cond
     ;; === 进入输入区 ===
     ((eq new-zone 'input)
      ;; 离开输入区前 sis 的真实状态，要在"进入时"取一次，
      ;; 但前提是确实是从 input 内部"走出去再走回来"，
      ;; 否则（比如从 readonly 区域刚切回来）应该用保存的值。
      (let* ((saved telega-sis--input-last-state)
             (target (cond
                      ((eq saved 'initial) telega-sis-initial-input-state)
                      ((memq saved '(english other)) saved)
                      (t telega-sis-initial-input-state))))
        (when target
          (telega-sis--switch-to target))))

     ;; === 离开输入区进入只读区 ===
     ((and (eq new-zone 'readonly)
           (eq old-zone 'input))
      ;; 先记住当前 sis 的真实状态（可能是用户在输入区手动切过的）
      (sis--get)
      (setq telega-sis--input-last-state sis--current)
      (telega-sis--switch-to 'english))

     ;; === 从非输入区进入 rootbuf ===
     ((eq new-zone 'root)
      (telega-sis--switch-to 'english))

     ;; === 离开 telega 到其他 buffer：不做切换，让 sis 自己管理 ===
     ;; （sis-global-context-mode / sis-global-respect-mode 本来就会处理其他 buffer）
     )))

;; ---- post-command 主 hook ----------------------------------------------

(defun telega-sis--post-command ()
  "每条命令执行完，判定区域；仅在区域变化时动输入法。"
  (when (and telega-sis-mode
             (not telega-sis--inhibit)
             (not (minibufferp)))          ; minibuffer 交给 sis 自己
    (let ((new-zone (telega-sis--classify-zone)))
      (unless (eq new-zone telega-sis--last-zone)
        (telega-sis--on-zone-change new-zone)
        (setq telega-sis--last-zone new-zone)))))

;; ---- 在跨 buffer 切换时也主动同步一次 last-zone ------------------------
;; （避免从非 telega buffer 切回来时 last-zone 仍然停留在 'other 导致漏判）

(defun telega-sis--on-buffer-switch ()
  "通过 `buffer-list-update-hook' 覆盖「窗口/缓冲区切换」的场景。"
  (when (and telega-sis-mode
             (not telega-sis--inhibit)
             (not (minibufferp)))
    (let ((new-zone (telega-sis--classify-zone)))
      (unless (eq new-zone telega-sis--last-zone)
        (telega-sis--on-zone-change new-zone)
        (setq telega-sis--last-zone new-zone)))))

;; ---- Minor mode ---------------------------------------------------------

(defun telega-sis--reset-state ()
  "关闭模式时重置内部状态，避免残留影响。"
  (setq telega-sis--last-zone 'other)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'telega-chat-mode)
        (setq telega-sis--input-last-state 'initial)))))

;;;###autoload
(define-minor-mode telega-sis-mode
  "在 telega 中按光标位置自动用 sis 切换中英文输入法。
仅在「跨区域」（输入区 ↔ 只读区 ↔ 聊天列表）时切换一次；
同一区域内完全不干预，你可以自由手动切换输入法。"
  :global t
  :lighter " telega-sis"
  (if telega-sis-mode
      (progn
        (add-hook 'post-command-hook #'telega-sis--post-command)
        (add-hook 'buffer-list-update-hook #'telega-sis--on-buffer-switch))
    (remove-hook 'post-command-hook #'telega-sis--post-command)
    (remove-hook 'buffer-list-update-hook #'telega-sis--on-buffer-switch)
    (telega-sis--reset-state)))

(provide 'telega-sis)
;;; telega-sis.el ends here
