;; Sudhir's Emacs customization file
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp" t)

(setq message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
	  '(lambda()
	     (setq gc-cons-threshold 800000
		   gc-cons-percentage 0.1)
	     (garbage-collect)) t)

;; No more environment.plist after Lion - need to setup exec path explicitly
(defun set-env-from-shell (env-var)
  (let ((val (shell-command-to-string (concat ". ~/.bash_profile; echo -n $" env-var))))
    (when (and (not (null val)) (not (string-equal val "")))
      (setenv env-var val))))
(when (eq window-system 'ns)
  (set-env-from-shell "PATH")
  (set-env-from-shell "PYTHONPATH")
  (set-env-from-shell "MANPATH")
  (setq exec-path (cdr (parse-colon-path (getenv "PATH"))))

  (add-to-list 'exec-path "/Applications/Emacs.app/Contents/MacOS/libexec")
  (add-to-list 'exec-path "/Applications/Emacs.app/Contents/MacOS/bin")

  (setq ns-pop-up-frames nil)
  (define-key global-map [ns-drag-file] 'ns-find-file))

(when (or (null default-directory)
	  (string-equal default-directory "/")
	  (string-equal default-directory ""))
  (setq default-directory (concat (getenv "HOME") "/")))

(require 'package)	       ; assume emacs version > 24
(setq package-enable-at-startup nil)
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(require 'use-package)
(setq use-package-verbose t)

;; Enable Unicode
(set-language-environment "UTF-8")
(setq-default buffer-file-coding-system 'utf-8-unix)

;;;; MacOS specific stuff
(when (or (eq window-system 'ns)	; Emacs 24 onwards
	  (eq window-system 'mac))	; Mitsuhara Yamamoto's Mac port
  (setq default-frame-alist (append default-frame-alist
				    '((top . 20) (left . 100) (width . 100) (height . 50)
                                      ;; (foreground-color . "darkgoldenrod3")
                                      ;; (background-color . "black")
                                      ;; (border-color . "gray")
				      (mouse-color . "yellow")
				      (cursor-color . "yellow"))))
  (set-scroll-bar-mode nil)
  (setq ns-command-modifier 'super    ;; these are set by default anyway
	ns-option-modifier 'meta)
  ;; Show file and path in title bar
  (setq frame-title-format
	(list (format "%s %%S: %%j " (system-name))
	      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))))

;;;; Emacs behaviours
(setq inhibit-startup-screen t
      garbage-collection-messages t
      ;; Search / replace
      case-fold-search t
      search-highlight t
      query-replace-highlight t
      ;; trailing white space etc
      show-trailing-whitespace t
      next-line-add-newlines nil
      require-final-newline 'ask
      transient-mark-mode t
      mark-even-if-inactive t
      mouse-yank-at-point t
      ;; formatting and display
      tab-width 4
      visible-bell t
      line-number-mode t
      column-number-mode t
      size-indication-mode t
      ;; misc
      auto-window-vscroll nil
      completion-auto-help t
      completion-auto-exit t
      auto-compression-mode t
      ps-paper-type 'a4)
(setq-default indent-tabs-mode nil)     ; to keep Python happy
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))                 ; turn off toolbar permanently
(fset 'yes-or-no-p 'y-or-n-p)		; replace yes/no qns. by y/n
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Set up automatic re-indent after a yank
(defadvice yank (after indent-region activate)
  (if (member major-mode '(lisp-mode c-mode c++-mode objc-mode LaTeX-mode TeX-mode cperl-mode))
      (indent-region (region-beginning) (region-end) nil)))

;;;;
;;;; Package settings
;;;;
(use-package cc-mode
  :defer t
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'" . c-mode)
         ("\\.mm\\'" . c++-mode))
  :bind (:map c++-mode-map
              ("<" . self-insert-command)
              (">" . self-insert-command))
  :bind (:map c-mode-base-map
              ("#" . self-insert-command)
              ("{" . self-insert-command)
              ("}" . self-insert-command)
              ("/" . self-insert-command)
              ("*" . self-insert-command)
              (";" . self-insert-command)
              ("," . self-insert-command)
              (":" . self-insert-command)
              ("(" . self-insert-command)
              (")" . self-insert-command)
              ("<return>" . newline-and-indent)
              ("M-q" . c-fill-paragraph)
              ("M-j"))
  :preface
  (defun my-c-mode-common-hook ()
    (require 'flycheck)
    (flycheck-mode 1)
    (setq-local flycheck-check-syntax-automatically nil)
    (setq-local flycheck-highlighting-mode nil)

    (set (make-local-variable 'parens-require-spaces) nil)

    (let ((bufname (buffer-file-name)))
      (when bufname
        (cond
         ((string-match "/ledger/" bufname)
          (c-set-style "ledger"))
         ((string-match "/edg/" bufname)
          (c-set-style "edg"))
         (t
          (c-set-style "clang")))))

    (font-lock-add-keywords
     'c++-mode '(("\\<\\(assert\\|DEBUG\\)(" 1 font-lock-warning-face t))))

  :hook (c-mode-common . my-c-mode-common-hook)
  :config
  (add-to-list 'c-style-alist
               '("edg"
                 (indent-tabs-mode . nil)
                 (c-basic-offset . 2)
                 (c-comment-only-line-offset . (0 . 0))
                 (c-hanging-braces-alist
                  . ((substatement-open before after)
                     (arglist-cont-nonempty)))
                 (c-offsets-alist
                  . ((statement-block-intro . +)
                     (knr-argdecl-intro . 5)
                     (substatement-open . 0)
                     (substatement-label . 0)
                     (label . 0)
                     (case-label . +)
                     (statement-case-open . 0)
                     (statement-cont . +)
                     (arglist-intro . +)
                     (arglist-close . +)
                     (inline-open . 0)
                     (brace-list-open . 0)
                     (topmost-intro-cont
                      . (first c-lineup-topmost-intro-cont
                               c-lineup-gnu-DEFUN-intro-cont))))
                 (c-special-indent-hook . c-gnu-impose-minimum)
                 (c-block-comment-prefix . "")))

  (add-to-list 'c-style-alist
               '("clang"
                 (indent-tabs-mode . nil)
                 (c-basic-offset . 2)
                 (c-comment-only-line-offset . (0 . 0))
                 (c-hanging-braces-alist
                  . ((substatement-open before after)
                     (arglist-cont-nonempty)))
                 (c-offsets-alist
                  . ((statement-block-intro . +)
                     (knr-argdecl-intro . 5)
                     (substatement-open . 0)
                     (substatement-label . 0)
                     (label . 0)
                     (case-label . 0)
                     (statement-case-open . 0)
                     (statement-cont . +)
                     (arglist-intro . +)
                     (arglist-close . +)
                     (inline-open . 0)
                     (brace-list-open . 0)
                     (topmost-intro-cont
                      . (first c-lineup-topmost-intro-cont
                               c-lineup-gnu-DEFUN-intro-cont))))
                 (c-special-indent-hook . c-gnu-impose-minimum)
                 (c-block-comment-prefix . ""))))

;; Add / remove commas in numbers
(use-package commify
  :defer t
  :commands commify-toggle)

;; Command interpreter
(use-package comint-mode
  :defer t
  :init
  (add-hook 'comint-mode-hook
            (lambda ()
              (interactive)
              (setq comint-scroll-show-maximum-output t)
              (local-set-key [(control up)] 'previous-multiframe-window)
              (local-set-key [(control down)] 'next-multiframe-window)
              (local-set-key [up] 'comint-previous-input)
              (local-set-key [down] 'comint-next-input)
              (local-set-key "\C-a" 'comint-bol)	;make C-a aware of prompt
              (local-set-key "\C-c\C-a" 'beginning-of-line))))

(use-package company
  :defer 5
  :commands (company-mode company-indent-or-complete-common)
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  c-mode-common-hook))
    (add-hook hook
              (lambda ()
                (local-set-key (kbd "<tab>") #'company-indent-or-complete-common))))
  :config
  ;; from https://github.com/company-mode/company-mode/issues/87
  ;; see also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))
  (defun check-expansion ()
    (save-excursion
      (if (outline-on-heading-p t)
          nil
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil))))))
  (define-key company-mode-map [tab]
    '(menu-item "maybe-company-expand" nil
                :filter (lambda (&optional _)
                          (when (check-expansion)
                            #'company-complete-common))))
  (global-company-mode 1))

(use-package cperl-mode
  :defer t
  :mode "\\.p[lm]\\'"
  :config
  ;; (setq cperl-hairy t)
  (setq cperl-indent-level 4)
  ;; (make-variable-buffer-local 'cperl-mode)
  ;; (setq-default cperl-mode nil)	       ; is this buffer in cperl-mode?
  (defun my-cperl-mode-hook ()
    (setq cperl-mode t)
    (define-key cperl-mode-map "\C-c'" 'cperl-toggle-abbrev)
    (define-key cperl-mode-map "\C-c;" 'cperl-toggle-auto-newline)
    (define-key cperl-mode-map "\C-c)" (deftoggle cperl-electric-parens-mark))
    (define-key cperl-mode-map "\C-c("
      (deftoggle cperl-electric-parens
        cperl-val
        (macro lambda (sym val) `(setq ,sym (if ,val t 'null))) nil nil
        ((setq my-cperl-electric-parens (cperl-val 'cperl-electric-parens)))))
    (setq my-cperl-electric-parens (cperl-val 'cperl-electric-parens))
    (require 'cl)
    (unless (eq (caar minor-mode-alist) 'my-cperl-electric-parens)
      (mapc #'(lambda (x)
                (setq minor-mode-alist
                      (cons x (delete x minor-mode-alist))))
            (nreverse '((my-cperl-electric-parens   (cperl-mode "("))
                        (abbrev-mode                (cperl-mode "'"))
                        (cperl-auto-newline         (cperl-mode ";"))
                        (cperl-electric-parens-mark (cperl-mode ")"))))))
    (define-key cperl-mode-map "\C-hf" nil)
    (define-key cperl-mode-map "\C-hv" nil)
    (define-key cperl-mode-map "\C-c\C-v" 'send-perldb-command)
    (define-key cperl-mode-map "\C-c\M-+" 'cperl-beautify-regexp)
    (define-key cperl-mode-map "\C-c "    'cperl-find-bad-style)
    (define-key cperl-mode-map "\C-c="    'cperl-lineup)
    (define-key cperl-mode-map "\C-cc"    'cperl-check-syntax)
    (define-key cperl-mode-map "\C-cd"    'perldb-break)
    (define-key cperl-mode-map "\C-ch"    'cperl-find-pods-heres)
    (define-key cperl-mode-map "\C-cp"    'send-perldb-command) ; "print"
    (define-key cperl-mode-map "\C-x`"    'perldb-next-error))
  (add-hook 'cperl-mode-hook 'my-cperl-mode-hook))

(use-package csv-mode
  :defer t
  :mode "\\.csv\\'")

(use-package ediff
  :defer t
  :init
  (setq ediff-split-window-function 'split-window-vertically
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-completion-native-enable nil
        python-shell-completion-native-disabled-interpreters
        '("python" "pypy" "ipython" "jupyter"))
  :config
  (electric-indent-local-mode -1)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules)
  (add-hook 'elpy-mode-hook 'flycheck-mode)

  (define-key elpy-mode-map (kbd "C-<up>") nil)
  (define-key elpy-mode-map (kbd "C-<down>") nil)
  (define-key elpy-mode-map (kbd "C-<prior>") 'elpy-nav-backward-block)
  (define-key elpy-mode-map (kbd "C-<next>") 'elpy-nav-forward-block)

  (defun ha/elpy-go-to-definition ()
    (interactive)
    (condition-case err
        (elpy-goto-definition)
      ('error (xref-find-definitions (symbol-name (symbol-at-point))))))
  :bind
  (:map elpy-mode-map ([remap elpy-goto-definition] . ha/elpy-goto-definition)))

(use-package erlang-mode
  :defer t
  :mode "\\.erl\\'"
  :config	     
  (setq erlang-root-dir "/usr/local/lib/erlang")
  (add-hook 'erlang-mode-hook
            (lambda ()
              ;; when starting an Erlang shell in Emacs, default in the node name
              (setq inferior-erlang-machine-options '("-sname" "emacs")))))

(use-package eshell
  :defer t
  :commands (eshell eshell-command)
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return] 'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace] 'eshell-isearch-delete-char)
      (define-key map [delete] 'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in eshell")
  :config
  (defun eshell-key-bindings ()
    (define-key eshell-mode-map (read-kbd-macro "<up>") nil)
    (define-key eshell-mode-map (read-kbd-macro "<down>") nil)
    (define-key eshell-mode-map (read-kbd-macro "C-<up>") nil)
    (define-key eshell-mode-map (read-kbd-macro "C-<down>") nil)
    (define-key eshell-mode-map (read-kbd-macro "M-<up>") 'eshell=previous-input)
    (define-key eshell-mode-map (read-kbd-macro "M-<down>") 'eshell-next-input))
  (add-hook 'eshell-first-time-mode-hook 'eshell-key-bindings))

(use-package evil
  :defer t
  :commands (evil-mode))

(use-package flycheck
  :defer t
  :commands (flycheck-mode flycheck-next-error flycheck-previous-error)
  :init
  (dolist (where '((emacs-lisp-mode-hook . emacs-lisp-mode-map)
                   (elpy-mode-hook . elpy-mode-map)
                   (js2-mode-hook . js2-mode-map)
                   (c-mode-common-hook . c-mode-base-map)))
    (add-hook (car where)
              `(lambda ()
                 (bind-key "M-n" #'flycheck-next-error ,(cdr where))
                 (bind-key "M-p" #'flycheck-previous-error ,(cdr where)))))
  :config
  (defalias 'show-error-at-point-soon 'flycheck-show-error-at-point)
  
  (defun magnars/adjust-eagerness ()
    "Adjust how often errors are checked based on whether there are any."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 0.3 3.0)))
  ;; each buffer gets its own idle-change-delay because of the buffer
  ;; sensitive adjustment above
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (add-hook 'flycheck-after-syntax-check-hook 'magnars/adjust-eagerness)

  ;; Remove newline checks since they would trigger an immediate check
  ;; and we want the idle-change-delay to be in effect when editing
  (setq-default flycheck-check-syntax-automatically '(save idle-change mode-enabled))

  (defun flycheck-handle-idle-change ()
    "Handle an expired idle time since last change"
    (flycheck-clear-idle-change-timer)
    (flycheck-buffer-automatically 'idle-change)))

(use-package flyspell
  :defer t
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :config
  (defun my-flyspell-maybe-correct-transposition (beg end candidates)
    (unless (let (case-fold-search)
              (string-match "\\`[A-Z0-9]+\\'"
                            (buffer-substring-no-properties beg end)))
      (flyspell-maybe-correct-transposition beg end candidates))))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package isearch
  :no-require t
  :bind (("C-M-r" . isearch-backward-other-window)
         ("C-M-s" . isearch-forward-other-window))
  :bind (:map isearch-mode-map
              ("C-c" . isearch-toggle-case-fold)
              ("C-t" . isearch-toggle-regexp)
              ("C-^" . isearch-edit-string)
              ("C-i" . isearch-complete))
  :preface
  (defun isearch-backward-other-window ()
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (call-interactively 'isearch-backward))

  (defun isearch-forward-other-window ()
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (call-interactively 'isearch-forward))
  
  (defun isearch-yank-symbolic-word-or-char ()
    (interactive)
    (isearch-yank-internal
     (lambda ()
       (let ((distance (skip-syntax-forward "w_")))
	 (when (zerop distance) (forward-char 1))
	 (point))))))

(use-package ispell
  :no-require t
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region))
  :config
  ;; Use English (not American) spellings for English
  (setq-default ispell-program-name "aspell")
  (setq ispell-local-dictionary "british"))

(use-package js2-mode
  :defer t
  :mode "\\.js\\'"
  :config
  (setq js2-highlight-level 3)
  (add-to-list 'flycheck-disabled-checkers #'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-mode 1))

(use-package json-mode
  :defer t
  :mode "\\.json\\'")

(use-package json-reformat
  :defer t
  :after json-mode)

(use-package json-snatcher
  :defer t
  :after json-mode)

(use-package lisp-mode
  :defer t
  :hook ((emacs-lisp-mode lisp-mode)
         . (lambda () (add-hook 'after-save-hook 'check-parens nil t)))
  :init
  (dolist (mode '(ielm-mode
                  inferior-emacs-lisp-mode
                  inferior-lisp-mode
                  lisp-interaction-mode
                  lisp-mode
                  emacs-lisp-mode))
    (font-lock-add-keywords
     mode
     '(("(\\(lambda\\)\\>"
        (0 (ignore
            (compose-region (match-beginning 1)
                            (match-end 1) ?λ))))
       ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face
           nil t)))))
  :config
  (add-hook 'lisp-mode-hook
	    (lambda ()
	      (set (make-local-variable lisp-indent-function)
		   'common-lisp-indent-function)
	      (make-local-variable 'isearch-mode-map)
	      (define-key isearch-mode-map "\C-w" 'isearch-yank-symbolic-word-or-char)))
  ;; additional keywords for font-lock
  (dolist (sym '(with-open-file with-input-from-string with-output-to-string
		  with-open-socket handler-bind))
    (put sym 'lisp-indent-function 'defun))

  ;; Get rid of "unsafe local variable" warnings
  (put 'package 'safe-local-variable 'symbolp)
  (put 'syntax 'safe-local-variable 'symbolp)
  (put 'base 'safe-local-variable 'integerp)
  (put 'Package 'safe-local-variable 'symbolp)
  (put 'Syntax 'safe-local-variable 'symbolp)
  (put 'Base 'safe-local-variable 'integerp))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix))
  :bind (:map magit-mode-map
              ("U" . magit-unstage-all)
              ("M-h") ("M-s") ("M-m") ("M-w"))
  :bind (:map magit-file-section-map ("<C-return>"))
  :bind (:map magit-hunk-section-map ("<C-return>"))
  :preface
  ;; History can be viewed with:
  ;; git log refs/snapshots/$(git symbolic-ref HEAD)
  (defun magit-monitor (&optional no-display)
    "Start git-monitor in the current directory."
    (interactive)
    (let* ((path (file-truename
                  (directory-file-name
                   (expand-file-name default-directory))))
           (name (format "*git-monitor: %s*"
                         (file-name-nondirectory path))))
      (unless (and (get-buffer name)
                   (with-current-buffer (get-buffer name)
                     (string= path (directory-file-name default-directory))))
        (with-current-buffer (get-buffer-create name)
          (cd path)
          (ignore-errors
            (start-process "*git-monitor*" (current-buffer)
                           "git-monitor" "-d" path))))))

  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))

  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+?\\)\\(\\.git\\)?\\'" "\\1"
              (magit-get "remote" (magit-get-remote) "url"))
             (magit-get-current-branch))))

  :hook (magit-mode . hl-line-mode)
  :config
  (use-package magit-commit
    :config
    (use-package git-commit))

  (use-package magit-files
    :config
    (global-magit-file-mode))

  (add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t)))

  (define-key magit-mode-map "G" #'endless/visit-pull-request-url)

  (eval-after-load 'magit-pull
    '(transient-insert-suffix 'magit-pull "p"
                              '("F" "default" magit-fetch-from-upstream)))

  (eval-after-load 'magit-push
    '(transient-insert-suffix 'magit-push "p"
                              '("P" "default" magit-push-current-to-upstream))))

(use-package paredit
  :defer t
  :diminish
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :bind (:map paredit-mode-map
              ("[")
              ("M-k"   . paredit-raise-sexp)
              ("M-I"   . paredit-splice-sexp)
              ("C-M-l" . paredit-recentre-on-sexp)
              ("C-c ( n"   . paredit-add-to-next-list)
              ("C-c ( p"   . paredit-add-to-previous-list)
              ("C-c ( j"   . paredit-join-with-next-list)
              ("C-c ( J"   . paredit-join-with-previous-list))
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline))
  :hook (paredit-mode
         . (lambda ()
             (unbind-key "M-r" paredit-mode-map)
             (unbind-key "M-s" paredit-mode-map)))
  :config
  (require 'eldoc)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round)
  (define-key paredit-mode-map (read-kbd-macro "C-<left>") nil)
  (define-key paredit-mode-map (read-kbd-macro "C-<right>") nil)
  (define-key paredit-mode-map (read-kbd-macro "M-<left>") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (read-kbd-macro "M-<right>") 'paredit-forward-slurp-sexp))

(use-package python-mode
  :defer t
  :mode "\\.py\\'"
  :interpreter "python"
  :bind (:map python-mode-map
              ("C-c c")
              ("C-c C-z" . python-shell))
  :config
  (defvar python-mode-initialized nil)

  (defun my-python-mode-hook ()
    (unless python-mode-initialized
      (setq python-mode-initialized t)

      (info-lookup-add-help
       :mode 'python-mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec
       '(("(python)Python Module Index" )
         ("(python)Index"
          (lambda
            (item)
            (cond
             ((string-match
               "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
              (format "%s.%s" (match-string 2 item)
                      (match-string 1 item)))))))))

    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indent-tabs-mode nil))

  (add-hook 'python-mode-hook 'my-python-mode-hook))

(use-package recentf ;; save recent files
  :defer 2
  :config
  (setq recentf-save-file (concat user-emacs-directory "recentf")
        recentf-max-saved-items 200
        recentf-max-menu-items 15))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :defer 2
  :init
  ;; activate it for all buffers
  (setq-default save-place t)
  :config	     
  (setq save-place-file (concat user-emacs-directory "saveplace")))

(use-package slime
  :defer t
  :commands slime
  :init
  ;; (unless (memq major-mode
  ;;               '(emacs-lisp-mode inferior-emacs-lisp-mode ielm-mode))
  ;;   ("M-q" . slime-reindent-defun)
  ;;   ("M-l" . slime-selector))

  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy slime-indentation))
  :config
  (setq slime-lisp-implementations
	`((sbcl ,(list (expand-file-name "~/bin/sbcl")) :coding-system utf-8-unix)
	  (ccl ,(list (expand-file-name "~/bin/ccl")) :coding-system utf-8-unix)))
  (setq slime-default-lisp 'sbcl
	common-lisp-hyperspec-root (expand-file-name "~/Documents/Programming/Lisp/HyperSpec/"))

  (setq lisp-lambda-list-keyword-parameter-alignment t
	lisp-lambda-list-keyword-alignment t
	slime-log-events nil)		; turn off logging in slime-events buffer

  ;; Shortcuts for invoking
  (defun ccl () "Start Clozure CL in SLIME" (interactive) (slime 'ccl "utf-8-unix"))
  (defun sbcl () "Start SBCL in SLIME" (interactive) (slime 'sbcl "utf-8-unix"))

  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (add-hook 'slime-mode-hook
	    (lambda ()
	      (slime-autodoc-mode t)
	      (setq slime-truncate-lines t)))
  (with-eval-after-load "slime-repl"
    (define-key slime-repl-mode-map [(meta up)] 'slime-repl-previous-input)
    (define-key slime-repl-mode-map [(meta down)] 'slime-repl-next-input)
    (define-key slime-repl-mode-map [(control up)] nil)
    (define-key slime-repl-mode-map [(control down)] nil))
  (setq lisp-simple-loop-indentation 1
	lisp-loop-keyword-indentation 6
	lisp-loop-forms-indentation 6))

;; SQLite3
(use-package sql
  :defer t
  :config
  (setq sql-sqlite-options "--interactive"))

(use-package text-mode
  :defer t
  :config
  (add-hook 'text-mode-hook
            (lambda ()
              (turn-on-auto-fill)
              (flyspell-mode))))

;; Meaningful names for buffers with the same name
(use-package uniquify
  :defer 2
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

;; Emacs table package
;; (require 'table)

;; Wikipedia editing
;; (require 'mediawiki)
;; (setq mediawiki-site-alist '(("Wikipedia"
;; 			      "http://en.wikipedia.org/w/"
;; 			      "Sshenoy"
;; 			      "1qazxsw2"
;; 			      "Main Page")))
;; (setq mediawiki-site "Wikipedia")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Useful macros and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmacro deftoggle (sym &optional get set comment before after message)
;;   "Define a function my-toggle-SYM to toggle SYM on and off.
;;    GET and SET are either nil in which case SYM and (setq SYM) are used,
;;    functions (eg default-value and set-default) called with SYM and SYM VAL,
;;    or (macro lambda (SYM) ...) and (macro lambda (SYM VAL) ...) respectively.
;;    COMMENT is additional comment for my-toggle-SYM,
;;    BEFORE and AFTER are lists of additional forms around the toggle code,
;;    MESSAGE is a (macro lambda (SYM VAL) ...) replacing the normal \"SYM is VAL.\""
;;   (cond ((null get) (setq get sym))
;;         ((symbolp get) (setq get `(,get (quote ,sym))))
;;         ((setq get (macroexpand (list get sym)))))
;;   (let ((val `(if arg (> (prefix-numeric-value arg) 0) (not ,get))))
;;     (cond ((null set) (setq set `(setq ,sym ,val)))
;;           ((symbolp set) (setq set `(,set (quote ,sym) ,val)))
;;           ((setq set (macroexpand (list set sym val)))))
;;     `(defun ,(intern (concat "my-toggle-" (symbol-name sym))) (&optional arg)
;;        ,(concat "Toggle " (symbol-name sym) ". Return the new value.
;;    With positive ARG set it, with nonpositive ARG reset it."
;;                 (if comment (concat "\n" comment)))
;;        (interactive "P")
;;        ,@before
;;        ,set
;;        ,@after
;;        (when (interactive-p)
;;          ,(if message (macroexpand (list message sym get))
;;             `(message "%s is %s" (quote ,sym) ,get)))
;;        ,get)))


;; (defun indent-or-expand (arg)
;;   "Either indent according to mode, or expand the word preceding point."
;;   (interactive "*P")
;;   (if (and
;;        (or (bobp) (= ?w (char-syntax (char-before))))
;;        (or (eobp) (not (= ?w (char-syntax (char-after))))))
;;       (dabbrev-expand arg)
;;     (indent-according-to-mode)))
;; (defun my-tab-fix ()
;;   (local-set-key [tab] 'indent-or-expand))
;; ;; add hooks for modes you want to use the tab completion for:
;; ;; (add-hook 'c-mode-hook 'my-tab-fix)
;; ;; (add-hook 'sh-mode-hook 'my-tab-fix)
;; ;; (add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
;; ;; (add-hook 'clojure-mode-hook 'my-tab-fix)

;; (defun replace-string* (from to)
;;   (while (search-forward from (point-max) t)
;;     (backward-char 1)
;;     (let ((props (text-properties-at (point))))
;;       (delete-char (length from))
;;       (let ((string (format "%s" to)))
;; 	(set-text-properties 0 (length string) props string)
;; 	(insert string)))))

;; (defun my-copy-line (arg)
;;   "Copy n lines without moving point"
;;   (interactive "p")
;;   (kill-ring-save (line-beginning-position)
;; 		  (line-beginning-position (+ 1 arg)))
;;   (message "%d line%s copied" arg (if (= arg 1) "" "s")))

;; (defun my-eval-and-replace ()
;;   "Replace the preceding sexp with it's computed value"
;;   (interactive)
;;   (backward-kill-sexp)
;;   (condition-case nil
;;       (prin1 (eval (read (current-kill 0)))
;; 	     (current-buffer))
;;     (error (message "Invalid expression")
;; 	   (insert (current-kill 0)))))

;; (defun faces:save-as-html ()
;;   (interactive)
;;   (let* ((buffer-name (format "*HTML:%s*" (buffer-name)))
;; 	 (buffer (get-buffer (buffer-name)))
;; 	 (buffer-html (or (get-buffer buffer-name)
;; 			  (generate-new-buffer buffer-name))))
;;     (save-excursion
;;       (set-buffer buffer-html)
;;       (insert-buffer buffer)
;;       (save-excursion
;; 	(goto-char (point-min))
;; 	(replace-string* "&" "&amp;"))
;;       (save-excursion
;; 	(goto-char (point-min))
;; 	(replace-string* "<" "&lt;"))
;;       (save-excursion
;; 	(goto-char (point-min))
;; 	(replace-string* ">" "&gt;"))
;;       (let ((current-colour nil))
;; 	(insert "<body text=\"white\" bgcolor=\"black\"><pre>\n")
;; 	(while (not (eobp))
;; 	  (let ((colour (let ((face (get-text-property (point) 'face)))
;; 			  (if face
;; 			      (face-foreground (if (consp face) (first face) face))
;; 			    nil))))
;; 	    (if (not (eq current-colour colour))
;; 		(progn
;; 		  (when current-colour
;; 		    (insert "</font>"))
;; 		  (when colour
;; 		    (insert (format "<font color=\"%s\">" colour)))
;; 		  (setq current-colour colour)))
;; 	    (forward-char 1)))
;; 	(when current-colour
;; 	  (insert "</font>")))
;;       (insert "</pre></body>\n"))
;;     (display-buffer buffer-html)))

(defun what-face (pos)
  "Prints the face at point"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (maessage "No face at %d" pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Key mappings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\M-r" 'isearch-backward-regexp)

(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [delete] 'delete-char)
(global-set-key [insert] 'yank)
(global-set-key [(meta tab)] 'other-frame)
;;(global-set-key [(control shift tab)] (lambda () (interactive)
;;					(select-frame (previous-frame))))
(global-set-key [(control kp-home)] 'beginning-of-buffer)
(global-set-key [(control kp-end)] 'end-of-buffer)
(global-set-key [(control up)] 'previous-multiframe-window)
(global-set-key [(control down)] 'next-multiframe-window)
(global-set-key [(control kp-delete)] 'kill-region)
(global-set-key [(control kp-insert)] 'yank-pop)

(global-set-key [f1] 'delete-other-windows)
(global-set-key [f2] 'split-window-vertically)
(global-set-key [f3] 'isearch-forward-regexp)
(global-set-key [f4] 'query-replace)
(global-set-key [(control f3)] 'isearch-backward-regexp)
(global-set-key [(control f4)] 'query-replace-regexp)
(global-set-key [f5] 'what-face)
(global-set-key [f12] 'find-file-literally)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("1f126eb4a1e5d6b96b3faf494c8c490f1d1e5ad4fc5a1ce120034fe140e77b88" "843a82ff3b91bec5430f9acdd11de03fc0f7874b15c1b6fbb965116b4c7bf830" default)))
 '(tool-bar-mode nil)
 '(user-mail-address "sudhir.shenoy@outlook.com"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :width normal :foundry "unknown" :family "Consolas"
                        :foreground "goldenrod")))))
