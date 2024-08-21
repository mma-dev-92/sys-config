(defvar mma/org-config-path "/home/mmazurek/Projects/sys-config/emacs-config/config.org")

(defvar mma/default-font-size 180)
(defvar mma/documents-font-size 210)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Refresh the package list if it is not already downloaded
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if it's not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Load use-package
(require 'use-package)
(setq use-package-always-ensure t)

;remove the startup message (make emacs welcome screen disapear)
(setq inhibit-startup-message t)
;disable scroll bar
(scroll-bar-mode -1)
;disable toolbar
(tool-bar-mode -1)
;disable tooltip
(tooltip-mode -1)
;set some margins
(set-fringe-mode 15)
;disable menu bar
(menu-bar-mode -1)
;instead of the bell sound set visible bell (as blinks)
(setq visible-bell t)
;disable line numbers for some modes (like terminal and org-mode)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
              treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode(lambda() (display-line-numbers-mode 0))))

(use-package eshell-prompt-extras
  :ensure t
  :after eshell
  :config
  (with-eval-after-load 'esh-opt
    (require 'eshell-prompt-extras)
    (setq eshell-highlight-prompt t
          eshell-prompt-function 'epe-theme-lambda)))

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height mma/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height mma/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height mma/documents-font-size :weight 'regular)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-badger t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 50))

(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)

(use-package all-the-icons)

(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  :bind (("C-s" . swiper) ;searching in real time!
         :map ivy-minibuffer-map ;C+vim-like key bindings in the minibuffer
         ("TAB" . ivy-alt-done) 	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map ;same hear
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)))

(use-package ivy-rich
  :init
  (ivy-rich-mode t))

(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x b") 'counsel-ibuffer)

;Hint: use C-h n and C-h p to navigate the pop-up with key bindings
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package counsel
  :bind (
	 :map minibuffer-local-map
	 ("C-r" . "counsel-minibuffer-history")))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-varuable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; Use visual line motions even outside of visual-line-mode buffers
  ;; If the cursor is at the top / bottom of the current view it will go to the previous / next line
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;;There is a variable evil-collection-mode-list, that contains the list of all modes
;;to which evil-collection defines key-bindings, if some bindings are not satisfying,
;;just remove the mode from the evil-collection-mode-list.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer mma/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (mma/leader-keys
    "tt" '(counsel-load-theme :which-key "choose theme")
    "ts" '(hydra-text-scale/body :which-key "scale text")))

;; Basic indentation and line-breaking behavior
(defun mma/org-mode-setup ()
  (setq org-adapt-indentation t)
  (org-indent-mode t)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Replace list hyphen with dot
(defun mma/org-bullet-list-setup ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org
  :hook (org-mode . mma/org-mode-setup)
  :hook (org-mode . mma/org-bullet-list-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers nil) ;can hide the * for bold text, ~ for the code, ... it can get messy
  (setq org-agenda-start-with-log-mode t) ;it will present a log of all tasks I am working today
  (setq org-log-done 'time) ;it will track the time, when the task was switched to "DONE" (C-c C-t)
  (setq org-log-into-drawer t) ;it will show the log of the task state evolution (in org-agenda-list)
  (setq org-agenda-files
	'("~/Projects/sys-config/emacs-config/org-files/tasks.org"))

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

  ;;font size adjustment to the heading levels + set font to cantarell, so the documents look like documents
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 0.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)

    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))))

(defun mma/org-mode-visual-fill ()
   (setq visual-fill-column-center-text t)
   (setq visual-fill-column-extra-text-width '(30 . 30))
   (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mma/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp .t)
    (python . t)))
;; unix like config *.ini files syntax support
(push '("conf-unix" . conf-unix) org-src-lang-modes)
;; python command for evaluating python code blocks
(setq org-babel-python-command "/usr/bin/python3.10")

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("conf" . "src conf-unix"))

;If any programming language mode starts (prog-mode), enable rainbow mode
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;Projectile key-bindings can be found with C-c p (as I have set the projectile-command-map below)
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ;Allows to easily create a key-binding for the projectile package
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;After C-c p p and hitting M-o there are many actions, that can be invoked on a project
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  ;; display diff in the same window
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; show nav at the top of each project file
(defun mma/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . mma/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  :custom (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
  
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package python-mode
  :ensure nil
  :hook
  (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "/usr/bin/python3.10"))

;; virtual envirnoment management
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

;; force lsp-mode to run global pylsp server each time
(setq lsp-pylsp-server-command "pylsp")
;; but to use local python interpreter from the selected .venv
(setq lsp-pylsp-python-executable-cmd "python")

;update dynamically given emacs *.el file on save to the path specified in the #+PROPERTY at the beginning of the file
(defun mma/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name mma/org-config-path)) 
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'mma/org-babel-tangle-config)))
