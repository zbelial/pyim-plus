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
                (and (>= char ?A)
                     (<= char ?Z))
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

(defun pyim-plus-zh-font-height ()
  "Get Chinese font height."
  (aref (font-info (font-at 0 nil "中文")) 3))

(defvar-local pyim-plus--company-box-enabled nil)

;;;###autoload
(defun pyim-plus-enable()
  "Enable pyim capf."
  (interactive)
  (add-hook 'completion-at-point-functions 'pyim-capf nil t)
  (when company-box-mode
    (company-box-mode -1)
    (setq pyim-plus--company-box-enabled t)))

;;;###autoload
(defun pyim-plus-disable()
  "Disable pyim capf."
  (interactive)
  (remove-hook 'completion-at-point-functions #'pyim-capf t)
  (when (and pyim-plus--company-box-enabled
             (fboundp 'company-box-mode))
    (company-box-mode)
    (setq pyim-plus--company-box-enabled nil)))

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
