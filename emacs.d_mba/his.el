(require `color-theme)
(color-theme-initialize)
(color-theme-taylor)

;; setting for emacs 
;;; setting for backup and save file
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq delete-auto-save-files t)

;; set truncation
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;;; setting for font-lock
(global-font-lock-mode t)

;; ;; setting for frame
(set-frame-parameter nil 'fullscreen 'maximized)
;; (setq default-frame-alist
;;       (append (list '(width . 211)
;; 		    '(height . 55)
;; 		    '(top . 0)
;; 		    '(left . 0))
;; 	      default-frame-alist))
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar nil)

;;; setting for paren
(show-paren-mode 1);
(setq show-paren-style 'mixed);

(defun swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
(global-set-key [f2] 'swap-screen)
(global-set-key [S-f2] 'swap-screen-with-cursor)

;; setting for flyspell mode
(eval-when-compile
  (require 'flyspell)
  (require 'ispell))
(if (file-executable-p "/usr/bin/aspell")
    (setq-default ispell-program-name "aspell")
  (setq-default ispell-program-name "ispell"))

;; auto-complete-mode
(require 'auto-complete)
(global-auto-complete-mode t)
;; (require 'auto-complete-config)
;; (require 'auto-complete-latex)

;;; global key-bind
(global-set-key "\C-h" 'delete-backward-char) ;
(global-set-key "\M-g" 'goto-line) ;
(define-key global-map "\C-cc" 'comment-region) ; comment-region
;; (global-set-key "\C-x\C-f" 'anything-my-find-files)

;;; Inline compile for LaTeX
(setq tex-start-commands nil)
(setq latex-run-command "pdflatex -shell-escape")
(setq tex-bibtex-command "bibtex")
(setq platex-run-command "platex -shell-escape")
(setq tex-dvipdf-command "dvipdfmx")
(setq tex-pbibtex-command "pbibtex")
(setq tex-view-pdf-command "evince")

(setq tex-fontify-script nil)

(defun tex-get-temp-cmd ()
  "Check the head line of the current buffer, and if it begins by %#!, return the command following it."
  (let ((head-of-buffer (car (split-string (buffer-string) "\n"))))
    (if (and (>= (length head-of-buffer) 3)
             (string= (substring head-of-buffer 0 3) "%#!"))
        (substring head-of-buffer 3))))

(defun my-tex-bibtex-file (&optional arg)
  "Run BibTeX on the current buffer's file."
  (interactive "P")
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (let* (shell-dirtrack-verbose
         (source-file (tex-main-file))
         (tex-out-file
          (tex-append (file-name-nondirectory source-file) ""))
         (file-dir (file-name-directory (expand-file-name source-file))))
    (tex-send-command tex-shell-cd-command file-dir)
    (if (consp arg)
        (tex-send-command tex-pbibtex-command tex-out-file)
      (tex-send-command tex-bibtex-command tex-out-file)))
  (tex-display-shell))

(defun my-latex-file (&optional arg)
  "Prompt to save all buffers and run LaTeX on current buffer's file.
This function is more useful than \\[tex-buffer] when you need the
`.aux' file of LaTeX to have the correct name."
  (interactive "P")
  (when tex-offer-save
    (save-some-buffers))
  (let* ((source-file (tex-main-file))
         (file-dir (file-name-directory (expand-file-name source-file))))
    (if (tex-shell-running)
        (tex-kill-job)
      (tex-start-shell))
    (setq tex-temp-cmd (tex-get-temp-cmd))
    (cond (tex-temp-cmd
           (if (<= (length (split-string tex-temp-cmd)) 1)
               (setq tex-temp-cmd (concat tex-temp-cmd " " source-file)))
           (tex-send-tex-command tex-temp-cmd file-dir))
          (t
           (if (not (consp arg))
               (tex-start-tex latex-run-command source-file file-dir)
             (tex-start-tex platex-run-command source-file file-dir)
             (setq tex-print-file (expand-file-name source-file))
             (let ((print-file-name-dvi (tex-append tex-print-file ".dvi")))
               (tex-send-command tex-dvipdf-command print-file-name-dvi nil)))))))

(defun tex-view-pdf (&optional arg)
  "Run PDF viewer."
  (interactive "P")
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (let ((view-file-name-pdf (tex-append (buffer-file-name) ".pdf")))
    (tex-send-command tex-view-pdf-command view-file-name-pdf t)))

;; tex-mode hook
(add-hook 'latex-mode-hook
	  (lambda ()
	    (flyspell-mode)
            (ac-flyspell-workaround)
            (ac-l-setup)
	    (turn-on-reftex)
            (define-key latex-mode-map "\C-c\C-c" 'comment-region)
            (define-key latex-mode-map "\C-c\C-f" 'my-latex-file)
            (define-key latex-mode-map "\C-c\C-i" 'my-tex-bibtex-file)
            (define-key latex-mode-map "\C-c\C-v" 'tex-view-pdf)
	    (define-key latex-mode-map "\C-cx" 'reftex-reset-mode)))

;;; reftex-default-bib
(setq reftex-default-bibliography '("/home/hishigami/Dropbox/texwork/hishigami.bib"))

;; bibtex-mode hook
(add-hook 'bibtex-mode-hook
	  (lambda ()
	    (turn-on-reftex)
	    (flyspell-mode)
            (ac-flyspell-workaround)
            (ac-l-setup)))

;;;
;;; end of file
;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
