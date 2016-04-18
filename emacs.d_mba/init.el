;; setting for character code
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;;; global key-bind
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-g" 'goto-line)
(define-key global-map "\C-c\C-p" nil)
;; (global-set-key "\C-x\C-f" 'anything-my-find-files)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq delete-auto-save-files t)
(setq-default indent-tabs-mode nil)
(set-scroll-bar-mode nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar nil)
(show-paren-mode t)
(column-number-mode t)
(transient-mark-mode -1)
(setq use-dialog-box nil)
;(setq-default cursor-type 'hbar)
(global-linum-mode)

;; setting for frame
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(when (require `color-theme)
  (color-theme-initialize)
  (color-theme-taylor))
(setq default-frame-alist
      (append (list
	'(width . 200)  ;;
	'(height . 55)  ;;
	'(alpha . (100 100 100 100))
	)
	default-frame-alist))

;; font
(global-font-lock-mode t)
(setq initial-default-font-size 12)
;; (setq default-font-size initial-default-font-size)
;; (set-font-size)

;; set truncation
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;;; set paren
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

;; show spaces at the end of lines
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t))
;; and delete
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; text-mode
(add-hook 'text-mode-hook
          (lambda()
            (flyspell-mode)
            (ac-flyspell-workaround)))

;; C-mode
(add-hook 'c-mode-hook
          (lambda()
            (c-set-style "stroustrup")))

;; auto-complete-mode
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(ac-config-default)
(setq ac-modes
      (append '(text-mode tex-mode latex-mode sty-mode maxima-mode)
              ac-modes))
;(require 'auto-complete-latex)

;; ispell
(setq-default ispell-extra-args '("--lang=en"))

;; (La)TeX
(setq tex-default-mode 'latex-mode)
(setq tex-start-commands nil)
(setq tex-run-command "pdflatex -shell-escape")
(setq latex-run-command "pdflatex -shell-escape")
(setq tex-bibtex-command "bibtex")
(setq tex-view-command "/usr/bin/open -a Preview.app")
(setq tex-view-pdf-command "/usr/bin/open -a Preview.app")
(setq tex-view-pdf-command-alt "/usr/bin/open -a Preview.app")
;; obsolete
(setq ptex-run-command "ptex")
(setq platex-run-command "platex")
(setq tex-dvipdf-command "dvipdfmx")
(setq tex-pbibtex-command "pbibtex")

(setq tex-fontify-script nil)

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

(defun tex-view-pdf (&optional arg)
  "Run PDF viewer."
  (interactive "P")
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (let ((view-file-name-pdf (tex-append (buffer-file-name) ".pdf")))
      (if (consp arg)
          (tex-send-command tex-view-pdf-command-alt view-file-name-pdf t)
        (tex-send-command tex-view-pdf-command view-file-name-pdf t))))

(defun tex-get-temp-cmd ()
  "Check the head line of the current buffer, and if it begins by %#!, return the command following it."
  (let ((head-of-buffer (car (split-string (buffer-string) "\n"))))
    (if (and (>= (length head-of-buffer) 3)
             (string= (substring head-of-buffer 0 3) "%#!"))
        (substring head-of-buffer 3))))

(defun my-tex-file (&optional arg)
  "Prompt to save all buffers and run TeX on current buffer's file.
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
               (tex-start-tex tex-run-command source-file file-dir)
             (tex-start-tex ptex-run-command source-file file-dir)
             (setq tex-print-file (expand-file-name source-file))
             (let ((print-file-name-dvi (tex-append tex-print-file ".dvi")))
               (tex-send-command tex-dvipdf-command print-file-name-dvi nil)))))))

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

(defun my-latex-file2 (&optional arg)
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

(add-hook 'tex-mode-hook
          (lambda ()
            (turn-on-reftex)
            (flyspell-mode)
            (ac-flyspell-workaround)
            (ac-l-setup)
            (define-key tex-mode-map "\C-c\C-f" 'my-tex-file)
            (define-key tex-mode-map "\C-c\C-v" 'tex-view-pdf)
            (define-key tex-mode-map "\C-c\C-i" 'my-tex-bibtex-file)
            (define-key tex-mode-map "\C-c\C-b" nil)
            (define-key tex-mode-map "\C-c\C-q" nil)
            (define-key tex-mode-map "\C-c\C-r" nil)
            ))
            ;; (define-key tex-mode-map "\C-j" 'skk-kakutei)))

(add-hook 'latex-mode-hook
          (lambda ()
            (turn-on-reftex)
            (define-key latex-mode-map "\C-c\C-f" 'my-latex-file)
;;            (define-key latex-mode-map "\C-c\C-g" 'my-latex-file2)
            (define-key latex-mode-map "\C-c\C-v" 'tex-view-pdf)
            (define-key latex-mode-map "\C-c\C-p" 'tex-view-pdf)
            (define-key latex-mode-map "\C-c\C-i" 'my-tex-bibtex-file)
            ))

;(setq reftex-label-alist '(AMSTeX))
(setq reftex-default-bibliography '("~/Dropbox/texwork/hishigami.bib"
                                    "~/Dropbox/texwork/jhishigami.bib"))

;; (defun my-reftex-format-ref-function (label form)
;;   "This custom function removes ~ at the head of form if exists."
;;   (if (string= (substring form 0 1) "~")
;;       (setq form (substring form 1)))
;;   (format form label label))
;; (setq reftex-format-ref-function 'my-reftex-format-ref-function)

;; (setq latex-block-names
;;       '("equation*" "gather" "gather*" "multline" "multline*" "multlined"
;;         "align" "align*" "split" "exampleblock" "alertblock" "block"
;;         "pmatrix" "bmatrix" "vmatrix" "cases" "dcases" "dcases*"
;;         "theorem" "proposition" "lemma" "corollary" "remark" "example"
;;         "frame" "block" "subequations" "tikzpicture"))

;; (autoload 'sty-mode "sty-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.sty\\'" . sty-mode))
;; (add-hook 'sty-mode-hook
;;           (lambda ()
;;             (flyspell-mode-off)))

;; (add-to-list 'auto-mode-alist '("\\.w\\'" . c-mode))

;; (add-hook 'bibtex-mode-hook
;;           (lambda ()
;;             (turn-on-reftex)
;;             (flyspell-mode)
;;             (define-key bibtex-mode-map "\C-j" 'skk-kakutei)))

;; count-words-region
;; from http://d.hatena.ne.jp/techie-t/20070302/1172882428

(defun recursive-count-words (region-end)
  (let ((count 0))
    (while (and (< (point) region-end)
                (re-search-forward "\\w+\\W*" region-end t))
      (setq count (+ count 1))
      )
    count
    )
  )

(defun count-words-region (beginning end)
  "Print number of words in the region.

Words are defined as at least one word-constituent
character followed by at least one character that is
not a word-constituent.  The buffer's syntax table
determines which characters these are."
  (interactive "r")
  (message "Counting words in region ... ")
  (save-excursion
    (goto-char beginning)
    (let ((count (recursive-count-words end)))
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))

;; ;; setting for flyspell mode
;; (eval-when-compile
;;   (require 'flyspell)
;;   (require 'ispell))
;; (if (file-executable-p "/usr/bin/aspell")
;;     (setq-default ispell-program-name "aspell")
;;   (setq-default ispell-program-name "ispell"))

;; ;;; Inline compile for LaTeX
;; (setq tex-start-commands nil)
;; (setq latex-run-command "pdflatex -shell-escape")
;; (setq tex-bibtex-command "bibtex")
;; (setq platex-run-command "platex -shell-escape")
;; (setq tex-dvipdf-command "dvipdfmx")
;; (setq tex-pbibtex-command "pbibtex")
;; (setq tex-view-pdf-command "/usr/bin/open")

;; (setq tex-fontify-script nil)

;; (defun tex-get-temp-cmd ()
;;   "Check the head line of the current buffer, and if it begins by %#!, return the command following it."
;;   (let ((head-of-buffer (car (split-string (buffer-string) "\n"))))
;;     (if (and (>= (length head-of-buffer) 3)
;;              (string= (substring head-of-buffer 0 3) "%#!"))
;;         (substring head-of-buffer 3))))

;; (defun my-tex-bibtex-file (&optional arg)
;;   "Run BibTeX on the current buffer's file."
;;   (interactive "P")
;;   (if (tex-shell-running)
;;       (tex-kill-job)
;;     (tex-start-shell))
;;   (let* (shell-dirtrack-verbose
;;          (source-file (tex-main-file))
;;          (tex-out-file
;;           (tex-append (file-name-nondirectory source-file) ""))
;;          (file-dir (file-name-directory (expand-file-name source-file))))
;;     (tex-send-command tex-shell-cd-command file-dir)
;;     (if (consp arg)
;;         (tex-send-command tex-pbibtex-command tex-out-file)
;;       (tex-send-command tex-bibtex-command tex-out-file)))
;;   (tex-display-shell))

;; (defun my-latex-file (&optional arg)
;;   "Prompt to save all buffers and run LaTeX on current buffer's file.
;; This function is more useful than \\[tex-buffer] when you need the
;; `.aux' file of LaTeX to have the correct name."
;;   (interactive "P")
;;   (when tex-offer-save
;;     (save-some-buffers))
;;   (let* ((source-file (tex-main-file))
;;          (file-dir (file-name-directory (expand-file-name source-file))))
;;     (if (tex-shell-running)
;;         (tex-kill-job)
;;       (tex-start-shell))
;;     (setq tex-temp-cmd (tex-get-temp-cmd))
;;     (cond (tex-temp-cmd
;;            (if (<= (length (split-string tex-temp-cmd)) 1)
;;                (setq tex-temp-cmd (concat tex-temp-cmd " " source-file)))
;;            (tex-send-tex-command tex-temp-cmd file-dir))
;;           (t
;;            (if (not (consp arg))
;;                (tex-start-tex latex-run-command source-file file-dir)
;;              (tex-start-tex platex-run-command source-file file-dir)
;;              (setq tex-print-file (expand-file-name source-file))
;;              (let ((print-file-name-dvi (tex-append tex-print-file ".dvi")))
;;                (tex-send-command tex-dvipdf-command print-file-name-dvi nil)))))))

;; (defun tex-view-pdf (&optional arg)
;;   "Run PDF viewer."
;;   (interactive "P")
;;   (if (tex-shell-running)
;;       (tex-kill-job)
;;     (tex-start-shell))
;;   (let ((view-file-name-pdf (tex-append (buffer-file-name) ".pdf")))
;;     (tex-send-command tex-view-pdf-command view-file-name-pdf t)))

;; ;; tex-mode hook
;; (add-hook 'latex-mode-hook
;; 	  (lambda ()
;; 	    (flyspell-mode)
;;             (ac-flyspell-workaround)
;;             (ac-l-setup)
;; 	    (turn-on-reftex)
;;             (define-key latex-mode-map "\C-c\C-f" 'my-latex-file)
;;             (define-key latex-mode-map "\C-c\C-i" 'my-tex-bibtex-file)
;; ;            (define-key latex-mode-map "\C-c\C-x" 'tex-view-pdf)
;; 	    (define-key latex-mode-map "\C-cx" 'reftex-reset-mode)))

;; ;; bibtex-mode hook
;; (add-hook 'bibtex-mode-hook
;; 	  (lambda ()
;; 	    (turn-on-reftex)
;; 	    (flyspell-mode)
;;             (ac-flyspell-workaround)
;;             (ac-l-setup)))

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
