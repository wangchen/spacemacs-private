;; -*- mode: emacs-lisp -*-
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     emoji
     themes-megapack
     ;; Tools
     helm
     (osx :varijs2-mode-toggle-warnings-and-errorsables osx-command-as 'super)
     (chinese :variables
              chinese-enable-youdao-dict t)
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t
                      better-defaults-move-to-end-of-code-first nil)
     command-log
     ;; Documentation
     (org :variables org-enable-github-support t)
     (markdown :variables markdown-live-preview-engine 'vmd)
     ;; Version Control
     (version-control :variables
                      version-control-diff-tool 'git-gutter
                      version-control-diff-side 'left)
     git
     github
     ;; Program
     (auto-completion :variables auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      :disabled-for org markdown)
     syntax-checking
     ;; Languages
     emacs-lisp
     shell-scripts
     sql
     yaml
     nginx
     html
     javascript
     python
     )
   dotspacemacs-additional-packages '(vue-mode)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(
                                    google-translate
                                    org-projectile ;; Add on 20180115 for https://github.com/syl20bnr/spacemacs/issues/9374
                                    ;; 'chinese' layer
                                    pyim
                                    fcitx
                                    pangu-spacing ;; Actually I like physical space bettwen chinese and english
                                    find-by-pinyin-dired ;;
                                    ace-pinyin
                                    ;; 'version-control' layer
                                    git-gutter+
                                    diff-hl
                                    ;; 'javascript' layer
                                    coffee-mode
                                    )
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'random
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(rebecca
                         spacemacs-dark
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Mononoki"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  (setq configuration-layer--elpa-archives
        '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
          ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
  (setq python-fill-column 80)
  (setq-default git-magit-status-fullscreen t)
  )

(defun dotspacemacs/user-config ()
  (setq powerline-default-separator nil)
  (spacemacs//set-monospaced-font "Mononoki"   "STHeiti" 14 16)
  (global-hungry-delete-mode)
  (global-set-key (kbd "s-1") 'winum-select-window-1)
  (global-set-key (kbd "s-2") 'winum-select-window-2)
  (global-set-key (kbd "s-3") 'winum-select-window-3)
  (global-set-key (kbd "s-4") 'winum-select-window-4)
  (global-set-key (kbd "s-t") 'helm-projectile-find-file)
  (global-set-key (kbd "s-k") 'kill-this-buffer)
  (global-set-key (kbd "s-{") 'previous-buffer)
  (global-set-key (kbd "s-}") 'next-buffer)
  (global-set-key (kbd "s-o") 'ffap)
  (global-set-key (kbd "s-e") 'er/expand-region)
  (global-set-key (kbd "M-u") 'upcase-dwim)
  (global-set-key (kbd "M-l") 'downcase-dwim)
  (global-set-key (kbd "M-c") 'capitalize-dwim)
  ;; Editing
  (setq org-startup-truncated nil)
  ;; Javascript
  (setq-default js2-basic-offset 2 js-indent-level 2)
  ;; Org-mode
  (setq org-todo-keywords
        '((sequence "TODO" "NEXT(n)" "WAITTING(w)" "SOMEDAY(s)"
                    "|"
                    "DONE(d@/!)" "ABORT(A@/!)")))
  (setq-default org-capture-templates  
                '(
                  ("t" "Todo" entry (file+headline "~/Dropbox/Org/gtd.org" "Inbox")
                   "* TODO %?\n  %i\n  %a")
                  ("j" "Journal" entry (file+datetree "~/Dropbox/Org/journal.org")
                   "* %?\nEntered on %U\n  %i\n  %a")
                  )
                org-agenda-files '("~/Dropbox/Org/gtd.org"))
  )
(defun my/set-frame-size-1x()
  (interactive)
  (set-frame-width (selected-frame) 100)
  )

(defun my/set-frame-size-2x()
  (interactive)
  (set-frame-width (selected-frame) 200))

(defun my/use-js-executables-from-node-modules ()
  "Set executables of JS checkers from local node modules."
  (-when-let* ((file-name (buffer-file-name))
               (root (locate-dominating-file file-name "node_modules"))
               (module-directory (expand-file-name "node_modules" root)))
    (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                           (javascript-eslint . "eslint")
                                           (javascript-jscs   . "jscs")))
      (let ((package-directory (expand-file-name module module-directory))
            (executable-var (flycheck-checker-executable-variable checker)))
        (when (file-directory-p package-directory)
          (set (make-local-variable executable-var)
               (expand-file-name (concat "bin/" module ".js")
                                 package-directory)))))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (emoji-cheat-sheet-plus company-emoji highlight skewer-mode simple-httpd markdown-mode pythonic command-log-mode insert-shebang fish-mode company-shell edit-indirect ssass-mode vue-html-mode multiple-cursors anaconda-mode avy smartparens evil flycheck yasnippet company projectile helm helm-core alert org-plus-contrib magit magit-popup git-commit ghub with-editor async hydra js2-mode s bind-key dash darktooth-theme zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme youdao-dictionary yapfify yaml-mode ws-butler winum which-key web-mode web-beautify vue-mode volatile-highlights vmd-mode vi-tilde-fringe uuidgen use-package unfill toc-org tagedit sql-indent spaceline smeargle slim-mode scss-mode sass-mode reveal-in-osx-finder restart-emacs rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode popwin pip-requirements persp-mode pcre2el pbcopy paradox ox-gfm osx-trash osx-dictionary orgit org-present org-pomodoro org-mime org-download org-bullets open-junk-file nginx-mode neotree mwim move-text markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode launchctl json-mode js2-refactor js-doc indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fuzzy flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav dumb-jump diminish cython-mode company-web company-tern company-statistics company-anaconda column-enforce-mode clean-aindent-mode auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:background "#292a44" :foreground "#f1eff8")))))
