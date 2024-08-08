(setq inhibit-startup-message t)

(scroll-bar-mode -1)                                     ; Disable visible scrollbar
(tool-bar-mode -1)                                        ; Disable the toolbar
(tooltip-mode -1)                                         ; Disable tooltips
(set-fringe-mode 10)                                      ; Some extra space

(menu-bar-mode -1)                                        ; Disable the menu bar

(setq visible-bell t)                                     ; Set up the "visual" alarm bell

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;Mode for displaying line-numbers in emacs
(column-number-mode)
(global-display-line-numbers-mode t)

;Disable line numbers for some modes (like terminal and org-mode)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode(lambda() (display-line-numbers-mode 0))))
;Hook is a variable that contains a list of functions, that are triggered
;every time when a given mode is activated. To do something extra, when
;given mode starts, you need to add some functions to its hook.

;Above code is equivalent to
;(add-hook org-mode-hook(lambda() (display-line-numbers-mode 0)))
;(add-hook term-mode-hook(lambda() (display-line-numbers-mode 0)))
;(add-hook eshel-mode-hook(lambda() (display-line-numbers-mode 0)))

;Tip: you can quickly evaluate emacs-lisp expression by M-:

;Be very carefull not to add the same hook multiple times (performance).

;; Font Configuration ----------------------------------------------------------
(defvar mma/default-font-size 180)

;; Default font and font size
(set-face-attribute 'default nil :font "Fira Code Retina" :height mma/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 180)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 210 :weight 'regular)

;; This is the package that can display keys I am hitting
(use-package command-log-mode)

;; This is a must have - very usefull for everything, on Ubuntu it is possible to install it via apt install
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


(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 50))

(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)

(use-package all-the-icons)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-material-dark t)
  ;other nice themes I have found are: doom-badger, doom-gruvbox, doom-city-lights

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;If any programming language mode starts (prog-mode), enable rainbow mode
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;Hint: use C-h n and C-h p to navigate the pop-up with key bindings
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;Tip: when using ivy browing a list of items, you can press M-o, and you will get five options
;for example by hitting d, you get a definition of a function related to the currently highlited item
(use-package ivy-rich
  :init
  (ivy-rich-mode t))

(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x b") 'counsel-ibuffer)

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

;To go back to emacs-state (without evil key bindings): C-z
;Window management is under the C-w (with which-key you will learn fast)
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  ;another key-binding for switching between the insert mode an normal mode - I do not need it
  ;(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) 
  ;vim backspace is C-h (it is nice)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  ;; If the cursor is at the top / bottom of the current view it will go to the previous / next line
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;;Kinda-default key-bindings for many different mode that will
;;most likely be very intuitive for a vim users

;;Important: load evil-collection after the evil mode!

;;There is a variable evil-collection-mode-list, that contains the list of all modes
;;to which evil-collection defines key-bindings, if some bindings are not satisfying,
;;just remove the mode from the evil-collection-mode-list.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;For creating custom key-bindings "namespaces" and key-bindings within it
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

;TODO: Find out how to force emacs not to brake text lines, but to go right beond the right edge of the window...
;      It is importamt, default behavior annoys me


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

;For better ivy <-> projectile integration
;After C-c p p and hitting M-o there are many actions, that can be invoked on a project
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;TODO: remove the emacs backup files from the ivy search

;;AWERSOME EMACS (COUNSEL-PROJECTILE) FUNCTIONALITY!!!
;counsel-projectile-rg uses ripgrep to find a given string in all project files
;C-c C-o will transform the pop-up buffer containing the search results
;into a "normal" buffer that wont go away, it still will be possible to go up and down
;and of course to navigate to the given found position by hitting ENTER
;TODO: create a key-binding for it, I will use it probably very often

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1));display diff in the same window

;evil-magit is injected in evil-collections

;;The most important command for the magit package is magit-status (X-x g)
;;s - stage one file, u - unstage one file
;;S - stage all files, U - unstage all files
;it is also possible to partially stage the change (by selecting, what lines I want to stage
;and pressing s, the same is possible for the unstage operation)

;if I want to commit what is staged, I need to press cc, then I need to type the commit message
;C-c C-k to exit the "creating a commit process"
;C-c C-c to create a new commit with given commit message

;TODO: read about the spinoff, and also read about how to configure emacs to type the ssh-key passwd
;inside the emacs, generaly, it is a good idea to learn this thing :)0

;to push to remote, just P p

;after magit-status, by hitting ? available options / operations will pop up

;;ORG-MODE!!!

(use-package org)

;TAB - toggle the current indent
;SHIFT + TAB - change the visibility level
;C + ENTER creates header of order equal to the current header but after the current section content
;M + ENTER creates header of order equal to the current header before the current section content
;M + ARROW (J | K) will move headers up and down
;C c l - insert a link (to anything, can be http, https, latex, ...)
;C c o - open a link

(defun mma/org-mode-setup ()
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq left-margin-width 20)
  (setq right-margin-width 20))

(use-package org
  :hook (org-mode . mma/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;Font size adjustment to the heading levels + set font to cantarell, so the documents look like documents
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(defun mma/org-mode-visual-fill () 
   (setq visual-fill-column-center-text t)
   (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mma/org-mode-visual-fill))
