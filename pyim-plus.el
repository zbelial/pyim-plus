;; pyim-plus.el --- Enhancement to pyim.  -*- lexical-binding: t; -*-


(require 'cl-lib)
(require 'seq)
(require 'pyim)

;;; capf
(defun pyim-plus-ascii-string-before-point ()
  "Get valid pinyin string bevore point."
  (let ((start (point))
        (end (point))
        (str "")
        (stop nil)
        char)
    (save-excursion
      (while (not stop)
        (setq char (char-before))
        (when (null char)
          (setq start (point)
                stop t))
        (if (or (and (>= char ?a)
                     (<= char ?z))
                (= char ?'))
            (goto-char (1- (point)))
          (setq start (point))
          (setq stop t)))
      (when (not (= start end))
        (setq str (buffer-substring-no-properties start end))))
    (list start end str)))

(defun pyim-plus-string-to-pinyins (str)
  "Convert STR to a list of Chinese pinyin."
  (let ((scheme (pyim-scheme-get 'quanpin))
        imobjs
        candidates)
    (setq imobjs (pyim-imobjs-create str scheme))
    (setq candidates (pyim-candidates-create imobjs scheme))
    candidates))

(defvar pyim-plus--max-candidates 10)
(defun pyim-capf ()
  "The pyim capf."
  (let* ((bounds (pyim-plus-ascii-string-before-point))
         (start (nth 0 bounds))
         (end (nth 1 bounds))
         (str (nth 2 bounds))
         pinyins
         words)
    (when (length> str 0)
      (setq pinyins (pyim-plus-string-to-pinyins (downcase str)))
      (setq words (seq-take pinyins (min pyim-plus--max-candidates (length pinyins)))))
    (list start end
          (lambda (probe pred action)
            (let (collection)
              (cond
               ((eq action 'metadata) `(metadata (category . pyim-capf)
                                                 (display-sort-function . identity)
                                                 (cycle-sort-function . identity)))               ; metadata
               ((eq (car-safe action) 'boundaries) nil)       ; boundaries
               ;; test-completion: not return exact match so that the selection will
               ;; always be shown
               ((eq action 'lambda)                           ; test-completion
                nil)
               ((null action)                                 ; try-completion
                t)
               ((eq action t)                                 ; all-completions
                words))))
          :exclusive 'no)))

(defvar pyim-plus--company-box-adviced nil)
(defvar-local pyim-plus--capf-enabled nil)

(with-eval-after-load 'company-box
  (defun pyim-plus-zh-font-height ()
    "Get Chinese font height."
    (aref (font-info (font-at 0 nil "中文")) 3))

  (defun pyim-plus-zh-font-width ()
    "Get Chinese font width."
    (aref (font-info (font-at 0 nil "中文")) 2))
  ;;; company-box显示中文候选项时会有部分条目可能显示不了，因为用`frame-char-height'得到的是英文字符的高度。
  (defun pyim-plus-company-box--compute-frame-position (frame)
    (-let* ((window-configuration-change-hook nil)
            ((left top _right _bottom) (company-box--edges))
            (window-tab-line-height (if (fboundp 'window-tab-line-height)
                                        (window-tab-line-height)
                                      0))
            (top (+ top window-tab-line-height))
            (char-height (pyim-plus-zh-font-height))
            (char-width (pyim-plus-zh-font-width))
            (height (* (min company-candidates-length company-tooltip-limit) char-height))
            (space-numbers (if (eq company-show-quick-access 'left) char-width 0))
            (frame-resize-pixelwise t)
            (mode-line-y (company-box--point-bottom))
            ((p-x . p-y) (company-box--prefix-pos))
            (p-y-abs (+ top p-y))
            (y (or (and (> p-y-abs (/ mode-line-y 2))
                        (<= (- mode-line-y p-y) (+ char-height height))
                        (> (- p-y-abs height) 0)
                        (- p-y height))
                   (+ p-y char-height)))
            (height (or (and (> y p-y)
                             (> height (- mode-line-y y))
                             (- mode-line-y y))
                        height))
            (height (- height (mod height char-height)))
            (scrollbar-width (if (eq company-box-scrollbar 'left) (frame-scroll-bar-width frame) 0))
            (x (if (eq company-box-frame-behavior 'point)
                   p-x
                 (if company-box--with-icons-p
                     (- p-x (* char-width (+ company-box-icon-right-margin (if (= company-box--space 2) 2 3))) space-numbers scrollbar-width)
                   (- p-x (if (= company-box--space 0) 0 char-width) space-numbers scrollbar-width)))))
      (setq company-box--x (max (+ x left) 0)
            company-box--top (+ y top)
            company-box--height height
            company-box--chunk-size (/ height char-height))
      (with-current-buffer (company-box--get-buffer)
        (setq company-box--x (max (+ x left) 0)
              company-box--top (+ y top)
              company-box--height height
              company-box--chunk-size (/ height char-height))))))

(defun pyim-plus--complete-or-insert ()
  (interactive)
  (if pyim-plus--capf-enabled
      (call-interactively #'company-complete-selection)
    (call-interactively #'self-insert-command)))

(defvar pyim-plus--enable-count 0)
;;;###autoload
(defun pyim-plus-enable-capf()
  "Enable pyim capf."
  (interactive)
  (add-hook 'completion-at-point-functions 'pyim-capf nil t)
  (unless pyim-plus--capf-enabled
    (setq-local pyim-plus--capf-enabled t)
    (when (= pyim-plus--enable-count 0)
      (define-key company-active-map (kbd "SPC") #'pyim-plus--complete-or-insert))
    (when (and company-box-mode
               (not pyim-plus--company-box-adviced))
      (advice-add 'company-box--compute-frame-position :override #'pyim-plus-company-box--compute-frame-position)
      (setq pyim-plus--company-box-adviced t))
    (setq pyim-plus--enable-count (1+ pyim-plus--enable-count))))

(defun pyim-plus--kill-buffer-hook-func ()
  (when pyim-plus--capf-enabled
    (call-interactively #'pyim-plus-disable-capf)))

(add-hook 'kill-buffer-hook #'pyim-plus--kill-buffer-hook-func)

;;;###autoload
(defun pyim-plus-disable-capf()
  "Disable pyim capf."
  (interactive)
  (when pyim-plus--capf-enabled
    (remove-hook 'completion-at-point-functions #'pyim-capf t)
    (setq-local pyim-plus--capf-enabled nil)
    (setq pyim-plus--enable-count (1- pyim-plus--enable-count))
    (when (= pyim-plus--enable-count 0)
      (define-key company-active-map (kbd "SPC") #'pyim-plus--complete-or-insert t)
      (when pyim-plus--company-box-adviced
        (setq pyim-plus--company-box-adviced nil)
        (advice-remove 'company-box--compute-frame-position #'pyim-plus-company-box--compute-frame-position)))))

;;; evil-escape integration
(with-eval-after-load 'evil-escape
  (defun pyim-plus-self-insert-command (orig-func)
    (interactive "*")
    (let ((first-key (elt evil-escape-key-sequence 0))
          (second-key (elt evil-escape-key-sequence 1)))
      (if (and (local-variable-p 'last-event-time)
               (floatp last-event-time)
               (< (- (float-time) last-event-time) evil-escape-delay))
          (set (make-local-variable 'temp-evil-escape-mode) t)
        (set (make-local-variable 'temp-evil-escape-mode) nil))
      (if (and temp-evil-escape-mode
               (string-prefix-p (char-to-string first-key) (reverse (pyim-entered-get 'point-before)))
               (equal last-command-event second-key))
          (progn
            (push last-command-event unread-command-events)
            ;; (pyim-process-outcome-handle 'pyim-entered)
            (push (pyim-entered-get 'point-before) pyim-outcome--history)
            (pyim-process-terminate))
        (progn
          (call-interactively orig-func)
          (set (make-local-variable 'last-event-time) (float-time))))))
  (advice-add 'pyim-self-insert-command :around #'pyim-plus-self-insert-command)
  )

;;; convert string to Chinese before point
(defvar-local pyim-plus-convert-flag nil)
(defun pyim-plus-convert-string-at-point ()
  (interactive)
  (if (pyim-string-match-p "[[:punct:]：－]" (pyim-char-before-to-string 0))
      (call-interactively 'pyim-punctuation-translate-at-point)
    (when (not (equal current-input-method "pyim"))
      (setq-local pyim-plus-convert-flag t))
    (call-interactively #'pyim-convert-string-at-point)))

(defun pyim-plus-after-pyim-convert-string-at-point ()
  (when pyim-plus-convert-flag
    (setq-local pyim-plus-convert-flag nil)
    (ignore-errors
      (run-at-time 0.1 nil (lambda ()
                             (pyim-deactivate)
                             (deactivate-input-method)
                             )))))
(add-hook 'pyim-process-ui-hide-hook #'pyim-plus-after-pyim-convert-string-at-point)
;; (remove-hook 'pyim-process-ui-hide-hook #'pyim-plus-after-pyim-convert-string-at-point)

(provide 'pyim-plus)
