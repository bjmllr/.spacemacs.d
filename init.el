(let ((default-directory  "~/.spacemacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; Modeline

;; Hide Minor Modes
(spacemacs|diminish editorconfig-mode nil)
(spacemacs|diminish flycheck-mode nil)
(spacemacs|diminish flyspell-mode nil)
(spacemacs|diminish rubocop-mode nil)
(spacemacs|diminish smartparens-mode nil)
(spacemacs|diminish super-save-mode nil)
(spacemacs|diminish which-key-mode nil)

;;; Sessions

(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

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

;;; Saving

(super-save-mode +1)
(setq super-save-auto-save-when-idle t)
(setq auto-save-default nil)
(setq super-save-remote-files nil)

;;; Surrounding

(defun surround-with-do-end ()
  (push '(?d . ("do" . "end")) evil-surround-pairs-alist))

;;; Version Control

(spacemacs/set-leader-keys "p m" 'magit-blame)

;;; Window Placement

(defun current-buffer-favorite-p ()
  (-none-p (lambda (re)
             (string-match-p re (buffer-name)
                             ))
           spacemacs-useless-buffers-regexp)
  )

(defun cycle-favorite-buffers (steps)
  (dotimes (i steps)
    (next-buffer)
    (while (not (current-buffer-favorite-p))
      (next-buffer))
    ))

(defun scratch ()
  (interactive)
  (if (string= "*scratch*" (buffer-name))
      (cycle-favorite-buffers 1)
    (switch-to-buffer "*scratch*"))
  )
(spacemacs/set-leader-keys "w SPC" 'scratch)

(defun arrange-windows-two-by-two ()
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (windmove-down)
  (split-window-right)
  (balance-windows)
  (winum-select-window-2) (cycle-favorite-buffers 1)
  (winum-select-window-3) (cycle-favorite-buffers 2)
  (winum-select-window-4) (cycle-favorite-buffers 3)
  (winum-select-window-1)
  )
(spacemacs/set-leader-keys "w 4" 'arrange-windows-two-by-two)

(defun arrange-windows-three-by-two ()
  (interactive)
  ;; reset window layout
  (delete-other-windows)

  ;; set number of windows to 6 (columns > rows)
  (split-window-right)
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)

  (winum-select-window-1) (window-resize nil 15 t)
  (winum-select-window-3) (window-resize nil 15 t)

  (winum-select-window-5)
  (winum-select-window-2) (cycle-favorite-buffers 1)
  (winum-select-window-3) (cycle-favorite-buffers 2)
  (winum-select-window-4) (cycle-favorite-buffers 3)
  (winum-select-window-5) (cycle-favorite-buffers 4)
  (winum-select-window-6) (cycle-favorite-buffers 5)

  (winum-select-window-5) (evil-window-set-width 80)

  (dotimes (number 3)
    (progn
     (winum-select-window-1) (evil-window-set-width 80)
     (winum-select-window-2) (evil-window-set-width 80)
     (winum-select-window-3) (evil-window-set-width 80)
     (winum-select-window-4) (evil-window-set-width 80)
     )
    )
  (winum-select-window-1) (evil-window-set-width 80)
  )
(spacemacs/set-leader-keys "w 6" 'arrange-windows-three-by-two)

(defun swap-buffer-window-follow-1 () "move this buffer and focus to window 1"
       (interactive) (swap-buffer-window-no-follow-1) (winum-select-window-1))
(defun swap-buffer-window-follow-2 () "move this buffer and focus to window 2"
       (interactive) (swap-buffer-window-no-follow-2) (winum-select-window-2))
(defun swap-buffer-window-follow-3 () "move this buffer and focus to window 3"
       (interactive) (swap-buffer-window-no-follow-3) (winum-select-window-3))
(defun swap-buffer-window-follow-4 () "move this buffer and focus to window 4"
       (interactive) (swap-buffer-window-no-follow-4) (winum-select-window-4))
(defun swap-buffer-window-follow-5 () "move this buffer and focus to window 5"
       (interactive) (swap-buffer-window-no-follow-5) (winum-select-window-5))
(defun swap-buffer-window-follow-6 () "move this buffer and focus to window 6"
       (interactive) (swap-buffer-window-no-follow-6) (winum-select-window-6))

(spacemacs/set-leader-keys "w e 1" 'swap-buffer-window-follow-1)
(spacemacs/set-leader-keys "w e 2" 'swap-buffer-window-follow-2)
(spacemacs/set-leader-keys "w e 3" 'swap-buffer-window-follow-3)
(spacemacs/set-leader-keys "w e 4" 'swap-buffer-window-follow-4)
(spacemacs/set-leader-keys "w e 5" 'swap-buffer-window-follow-5)
(spacemacs/set-leader-keys "w e 6" 'swap-buffer-window-follow-6)

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

(defun crystal-compile-specs ()
  "finds shard.yml and runs crystal spec"
  (interactive)
  (save-buffer)
  (when (locate-dominating-file default-directory "shard.yml")
    (with-temp-buffer
      (cd (locate-dominating-file default-directory "shard.yml"))
      (compile "crystal spec"))))
(define-key crystal-mode-map (kbd "C-c C-c") 'crystal-compile-specs)

(defun crystal-indent-with-compiler ()
  (interactive)
  (save-buffer)
  (shell-command
   (format "crystal tool format %s"
           (shell-quote-argument (buffer-file-name)))
   )
  (revert-buffer t t t))
(define-key crystal-mode-map (kbd "C-c C-f") 'crystal-indent-with-compiler)

;; Ruby
(require 'ruby-mode)
(add-hook 'ruby-mode-hook 'surround-with-do-end)
(setq ruby-insert-encoding-magic-comment nil)

(defun ruby-block-end-comment ()
  "Add a comment to an end keyword describing how the block started"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " ")
    (if (string= (thing-at-point 'word) "end")
        (bjmllr/ruby-beginning-of-block)
      (progn
        (bjmllr/ruby-end-of-block)
        (bjmllr/ruby-beginning-of-block)
        )
      )
    (push-mark (point))
    (end-of-line)
    (kill-ring-save (mark) (point))
    (bjmllr/ruby-end-of-block)
    (forward-word)
    (push-mark (point))
    (end-of-line)
    (delete-region (mark) (point))
    (insert " # " (pop kill-ring))
    )
  )
(define-key ruby-mode-map (kbd "C-c C-e") 'ruby-block-end-comment)

;; show-smartparens-mode

(defun bjmllr/ruby-beginning-of-block ()
  (interactive)
  (let* ((original-position (point)))
    (condition-case err (ruby-beginning-of-block)
      (error (if (eq (nth 1 err) 'number-or-marker-p)
                 (progn
                   (goto-char original-position)
                   (sp-ruby-backward-sexp)
                   (bjmllr/ruby-beginning-of-block)
                   )
               (signal (car err) (cdr err)))))))

(defun bjmllr/ruby-end-of-block ()
  (interactive)
  (let* ((original-position (point)))
    (condition-case err (ruby-end-of-block)
      (error (if (eq (nth 1 err) 'number-or-marker-p)
                 (progn
                   (goto-char original-position)
                   (sp-ruby-forward-sexp)
                   (bjmllr/ruby-end-of-block)
                   )
               (signal (car err) (cdr err)))))))

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
