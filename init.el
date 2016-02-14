(let ((default-directory  "~/.spacemacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'crystal-mode)

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

;; Ruby

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
(global-set-key (kbd "M-]") 'next-delimiter)
(defun prev-delimiter ()
  (interactive)
  (re-search-backward "[\]\)\},]")
  (re-search-backward "[^\]\[\)\(\}\{[:space:]\n,]")
  (re-search-backward "[\[\(\{,]")
  (re-search-forward "[^\]\[\)\(\}\{[:space:]\n,]")
  (backward-char))
(global-set-key (kbd "M-[") 'prev-delimiter)

;; 80 column line (for screen calibration);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
