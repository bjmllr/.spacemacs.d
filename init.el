(let ((default-directory  "~/.spacemacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; Snippets

(defun insert-backticks-for-code-block ()
  (interactive)
  (end-of-line)
  (copy-to-register 'indentation-and-comment-prefix
                    (line-beginning-position) (line-end-position))
  (insert "```")
  (insert (replace-regexp-in-string "-mode" "" (symbol-name major-mode)))
  (insert "\n")
  (insert-register 'indentation-and-comment-prefix)
  (end-of-line)
  (insert "```")
  (previous-line)
  (end-of-line)
  (insert "\n")
  (insert-register 'indentation-and-comment-prefix)
  (end-of-line))
(spacemacs/set-leader-keys "i `" 'insert-backticks-for-code-block)

;;; Surrounding

(defun surround-with-do-end ()
  (push '(?d . ("do" . "end")) evil-surround-pairs-alist))

;;; Window Placement

(defun arrange-windows-two-by-two ()
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (windmove-down)
  (split-window-right)
  (balance-windows)
  (select-window-2) (spacemacs/next-useful-buffer)
  (select-window-3) (spacemacs/next-useful-buffer)
  (select-window-4) (spacemacs/next-useful-buffer)
  (select-window-1)
  )
(spacemacs/set-leader-keys "w 4" 'arrange-windows-two-by-two)

(defun arrange-windows-three-by-two ()
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (split-window-right)
  (windmove-down)
  (split-window-right)
  (split-window-right)
  (balance-windows)
  (select-window-2) (spacemacs/next-useful-buffer)
  (select-window-3) (spacemacs/next-useful-buffer)
  (select-window-4) (spacemacs/next-useful-buffer)
  (select-window-5) (spacemacs/next-useful-buffer)
  (select-window-6) (spacemacs/next-useful-buffer)
  (select-window-1))
(spacemacs/set-leader-keys "w 6" 'arrange-windows-three-by-two)

;; http://stackoverflow.com/questions/8989540/touch-current-file-in-emacs
(defun touch ()
  "updates mtime on the file for the current buffer"
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
  (clear-visited-file-modtime))
(spacemacs/set-leader-keys "f ." 'touch)

;;; JavaScript
(setq-default js2-basic-offset 2)

;;; Crystal
(require 'crystal-mode)
(add-hook 'crystal-mode-hook 'surround-with-do-end)

(defun dominate-compile-specs ()
  "finds a Makefile and runs make spec"
  (interactive)
  (when (locate-dominating-file default-directory "Makefile")
    (with-temp-buffer
      (cd (locate-dominating-file default-directory "Makefile"))
      (compile "make spec"))))
(spacemacs/set-leader-keys "c s" 'dominate-compile-specs)
(define-key crystal-mode-map (kbd "C-c C-c") 'dominate-compile-specs)

(defun crystal-indent-with-compiler ()
  (interactive)
  (shell-command
   (format "crystal tool format %s"
           (shell-quote-argument (buffer-file-name)))
   )
  (revert-buffer t t t))
(define-key crystal-mode-map (kbd "C-c C-f") 'crystal-indent-with-compiler)

;; Ruby
(require 'ruby-mode)
(add-hook 'ruby-mode-hook 'surround-with-do-end)

(defun seeing-is-believing ()
  "Replace the current region (or the whole buffer, if none) with the output
of seeing_is_believing."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max)))
        (origin (point)))
    (shell-command-on-region beg end
                             "seeing_is_believing -x -n 100 -t 10"
                             nil 'replace)
    (goto-char origin)))
(define-key ruby-mode-map (kbd "C-c C-c") 'seeing-is-believing)

(add-hook 'ruby-mode-hook 'flycheck-mode)

;; Delimiter Navigation
(defun next-delimiter ()
  (interactive)
  (re-search-forward "[\[\(\{,]")
  (re-search-forward "[^\]\[\)\(\}\{[:space:]\n,]")
  (backward-char))
(spacemacs/set-leader-keys ", l" 'next-delimiter)
(defun prev-delimiter ()
  (interactive)
  (re-search-backward "[\]\)\},]")
  (re-search-backward "[^\]\[\)\(\}\{[:space:]\n,]")
  (re-search-backward "[\[\(\{,]")
  (re-search-forward "[^\]\[\)\(\}\{[:space:]\n,]")
  (backward-char))
(spacemacs/set-leader-keys ", h" 'prev-delimiter)
(defun transpose-around-comma ()
    (interactive)
    (re-search-forward ",")
    (delete-horizontal-space)
    (delete-backward-char 1)
    (push-mark)
    (backward-sexp 1)
    (kill-region (point) (mark))
    (forward-sexp 1)
    (insert ", ")
    (yank))
(spacemacs/set-leader-keys ", t" 'transpose-around-comma)

;; 80 column line (for screen calibration);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
