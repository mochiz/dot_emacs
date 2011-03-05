;;; -*- mode: emacs-lisp; indent-tabs-mode: nil -*-

;;; init_global.el --- global

;; $BMxMQ$9$k4D6-6&DL$N@_Dj(B

;;; Code:

;;; $B=i4|0LCV(B
;; (cd "~/")

;; $B%m%0$ND9$5$rL58B$K(B
(setq message-log-max 't)
;; $B%m%0$r=P$5$J$$(B
;; (setq message-log-max nil)


;; Carbon Emacs$B$N@_Dj$GF~$l$i$l$?(B. $B%a%K%e!<$r1#$7$?$j!%(B
(custom-set-variables
 '(display-time-mode t)
 '(safe-local-variable-values (quote ((c-hanging-comment-ender-p))))
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; menubar
;(menu-bar-mode nil)
;;; toolbar
(tool-bar-mode 0)

;; $B7Y9p$r;k3PE*$K$9$k(B
(setq visible-bell t)

;; $B%U%!%$%k$rJT=8$7$?>l9g%3%T!<$K$F%P%C%/%"%C%W$9$k(B
;; inode $BHV9f$rJQ99$7$J$$(B
(setq backup-by-copying t)
;;; $B%P%C%/%"%C%W%U%!%$%k$NJ]B80LCV;XDj(B[2002/03/02]
;; !path!to!file-name~ $B$GJ]B8$5$l$k(B
(setq backup-directory-alist
      '(
        ("^/etc/" . "~/.emacs.d/var/etc")
        ("." . "~/.emacs.d/var/emacs")
        ))

;; emacsclient $B$rMxMQ$9$k$?$a$K%5!<%P5/F0(B
;; $B%5!<%P$,5/F0$7$F$$$?>l9g$O@h$K5/F0$7$F$$$?J}$rM%@h(B
;; (require 'server)
;; (unless (server-running-p) (server-start))
;; (setq server-visit-hook
;;       '(lambda () 
;;          ;; Frame $B$rA0LL$K$9$k(B
;;          (raise-frame (selected-frame))
;;          ;; $B%-!<%\!<%I%U%)!<%+%9$rA*Br$7$F$$$k(BFrame$B$K$9$k(B
;;          (x-focus-frame (selected-frame))))

;;$B5/F0;~$N(Bmessage$B$rI=<($7$J$$(B
(setq inhibit-startup-message t)
;; scratch $B$N%a%C%;!<%8$r6u$K$9$k(B
(setq initial-scratch-message nil)

; $B0u:~$N@_Dj(B
(setq ps-multibyte-buffer 'non-latin-printer)

; $B<+F02~9T4XO"(B
(setq-default auto-fill-mode nil)
(setq-default fill-column 300)
(setq text-mode-hook 'turn-off-auto-fill)

; $B:o=|%U%!%$%k$r(BOS$B$N$4$_H"$X(B
;(setq delete-by-moving-to-trash t)

;;; help key$BJQ99(B
;; BackSpace$B$r(BC-h$B$KJQ99(B
;(load-library "obsolete/keyswap")
(global-set-key "\M-?" 'help-for-help)
;; keyswap $B$O(B obsolete$B$J$N$G0J2<$N@_Dj$,NI$$(B
(global-set-key "\C-h" 'backward-delete-char)

;; $BJT=84XO"(B

;; $B%b!<%I%i%$%s$K%i%$%s?t!"%+%i%`?tI=<((B
(line-number-mode t)
(column-number-mode t)

;; $B%j!<%8%g%s$r(B kill-ring $B$KF~$l$J$$$G:o=|$G$-$k$h$&$K$9$k(B
(delete-selection-mode t)

;; TAB $B$O%9%Z!<%9(B 4 $B8D$V$s$r4pK\(B
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)


;; $BBP1~$9$k%+%C%3$r?'I=<($9$k(B
;; $BFC$K?'$r$D$1$J$/$F$b(BC-M-p$B!"(BC-M-n $B$rMxMQ$9$l$PBP1~$9$k%+%C%3Ey$K0\F0$G$-$k(B
(show-paren-mode t)
;; $B%+%C%3BP1~I=<($N%9%?%$%k(B
;; $B%+%C%3$=$NJ*$K?'$,IU$/(B($B%G%U%)%k%H(B)
;; (setq show-paren-style 'parenthesis)
;; $B%+%C%3Fb$K?'$,IU$/(B
;; (setq show-paren-style 'expression)
;; $B2hLLFb$K<}$^$k>l9g$O%+%C%3$N$_!"2hLL30$KB8:_$9$k>l9g$O%+%C%3FbA4BN$K?'$,IU$/(B
;; (setq show-paren-style 'mixed)

;;$BF0E*N,8lE83+$GBgJ8;z>.J8;z$r6hJL(B
(setq dabbrev-case-fold-search nil)

;;$B?75,9T$r:n@.$7$J$$(B
;;emacs21$B$G$O%G%U%)%k%H$G@_Dj$5$l$F$$$^$9!#(B
(setq next-line-add-newlines nil)

;; $B%9%/%m!<%k$N%^!<%8%s(B
;; $B0l9T$:$D%9%/%m!<%k$9$k(B
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)
(setq comint-scroll-show-maximum-output t)
;(setq next-screen-context-lines 3)

;; $B=*N;;~$KJ9$/(B
(setq confirm-kill-emacs 'y-or-n-p)

;; $B0BA4$J<B9T$N$?$a$N6&DL7O4X?t(B

;; @see http://www.sodan.org/~knagano/emacs/dotemacs.html
(defmacro eval-safe (&rest body)
  "$B0BA4$JI>2A!#I>2A$K<:GT$7$F$b$=$3$G;_$^$i$J$$!#(B"
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))
(defun load-safe (loadlib)
  "$B0BA4$J(B load$B!#FI$_9~$_$K<:GT$7$F$b$=$3$G;_$^$i$J$$!#(B"
  ;; missing-ok $B$GFI$s$G$_$F!"%@%a$J$i$3$C$=$j(B message $B$G$b=P$7$F$*$/(B
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "[load-safe] failed %s" loadlib)))
    load-status))
(defun autoload-if-found (functions file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (if (not (listp functions))
      (setq functions (list functions)))
  (and (locate-library file)
       (progn
         (dolist (function functions)
           (autoload function file docstring interactive type))
         t )))


(provide 'init_global)
;;; init_global.el ends here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; add to init_global.el
;; $B<B9T%Q%9$X(Bsvn$BMQ%Q%9$rDI2C$7$^$9(B
(setq exec-path (cons "/opt/local/bin" exec-path)) ;; $BITMW!)(B

;; Cntl-h$B$X(BBackspace$B$r3d$jEv$F$^$9(B
(global-set-key "\C-h" 'backward-delete-char)

;; add to init_global.el
;; window$B%5%$%:$r:GBg2=$7$^$9(B
;; (when (eq window-system 'mac)
;;   (add-hook 'window-setup-hook
;;             (lambda ()
;;               ;; (setq mac-autohide-menubar-on-maximize t)
;;               (set-frame-parameter nil 'fullscreen 'fullboth) 
;;               )))

(defun mac-toggle-max-window ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

;; Carbon Emacs$B$N@_Dj$GF~$l$i$l$?(B. $B%a%K%e!<$r1#$7$?$j!%(B
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(safe-local-variable-values (quote ((c-hanging-comment-ender-p))))
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; $B%P%C%/%"%C%W%U%!%$%k$r:n@.$7$J$$(B
(setq backup-inhibited t)

;;yes or no"$B$NBe$o$j$K(B"y or n"$B$r;H$&(B
(fset 'yes-or-no-p 'y-or-n-p)

;;$B3+$$$F$$$k%U%!%$%k$,99?7$5$l$?$i<+F0E*$K:FFI$_9~$_$r9T$&(B
(global-auto-revert-mode t)

;;$B3+$$$F$$$k%&%#%s%I%&$r0l$DLa$k(B
(defun previous-window ()
  (interactive)
  (other-window -1))
(global-set-key "\C-xp" 'previous-window)

;; $B%&%#%s%I%&I}<+F0JQ99(B 
(require 'widen-window)
(global-widen-window-mode t)

;; Color
(if window-system (progn
                    (set-background-color "Black")
                    (set-foreground-color "LightGray")
                    (set-cursor-color "Gray")
                    ;;   (set-frame-parameter nil 'alpha 75)
                    ))


;; $B9T$XHt$V(B(command+g)
(global-set-key "\M-g" 'goto-line)
(show-paren-mode 1)
(transient-mark-mode 1)

;;(setq pc-select-selection-keys-only t)
(pc-selection-mode 1)

;; $B8=:_9T$r6/D4$9$k(B
(require 'hl-line)
(global-hl-line-mode)

;;$B9TKv$N6uGr$r6/D4I=<((B
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t))

;;(global-set-key "\C-m" 'reindent-then-newline-and-indent)

;; add to init_shell.el
;; $B%7%'%k$N;XDj(B
(setq shell-file-name "/opt/local/bin/zsh")

;; add to init_global.el end
(global-font-lock-mode t)
(require 'font-lock)

;; iswitchb$B$rM-8z$K$9$k(B
;; (iswitchb-mode 1)

;; $B%P%C%U%!A*Br$K%+!<%=%k%-!<!"(BC-f$B!"(BC-b$B!"%9%Z!<%9%-!<$r3d$jEv$F$k(B
;; (defun iswitchb-my-keys ()
;;   "Add my keybindings for iswitchb."
;;   (define-key iswitchb-mode-map [right] 'iswitchb-next-match)
;;   (define-key iswitchb-mode-map [left] 'iswitchb-prev-match)
;;   (define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
;;   (define-key iswitchb-mode-map " " 'iswitchb-next-match)
;;   (define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match))
;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-my-keys)

;; $B%P%C%U%!A*BrCf$KA*Br%P%C%U%!$NFbMF$r%W%l%S%e!<$9$k(B
;; (defadvice iswitchb-exhibit
;;   (after
;;    iswitchb-exhibit-with-display-buffer
;;    activate)
;;   "$BA*Br$7$F$$$k(B buffer $B$r(B window $B$KI=<($7$F$_$k!#(B"
;;   (when (and
;;          (eq iswitchb-method iswitchb-default-method)
;;          iswitchb-matches)
;;     (select-window
;;      (get-buffer-window (cadr (buffer-list))))
;;     (let ((iswitchb-method 'samewindow))
;;       (iswitchb-visit-buffer
;;        (get-buffer (car iswitchb-matches))))
;;     (select-window (minibuffer-window))))

;; $B%P%C%U%!A*BrCf$KA*Br%P%C%U%!$N%U%!%$%kL>$^$?$O%b!<%I$rI=<($9$k(B
;; (defadvice iswitchb-completions (after
;;                                  iswitchb-completions-with-file-name
;;                                  activate)
;;   "$BA*Br$7$F$k$H$-$K%U%!%$%kL>$H$+$r=P$7$F$_$k!#(B"
;;   (when iswitchb-matches
;;     (save-excursion
;;       (set-buffer (car iswitchb-matches))
;;       (setq ad-return-value
;;             (concat ad-return-value
;;                     "\n"
;;                     (cond ((buffer-file-name)
;;                            (concat "file: "
;;                                    (expand-file-name (buffer-file-name))))
;;                           ((eq major-mode 'dired-mode)
;;                            (concat "directory: "
;;                                    (expand-file-name dired-directory)))
;;                           (t
;;                            (concat "mode: " mode-name " Mode"))))))))


;; add to init_ac.el
;;$B%*!<%H%3%s%W%j!<%H(B
;; 2010-03-16 auto-complete.el v1.2$B$r%$%s%9%H!<%k$7$^$7$?(B
;;(add-to-list 'load-path "~/.emacs.d/elisp/")
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
;;(ac-config-default)

;;$B%*!<%H%3%s%W%j!<%H(B
(require 'auto-complete)
(global-auto-complete-mode t)

;;$BBP1~$9$k3g8L$K%8%c%s%W(B
;; (progn
;;   (defvar com-point nil
;;     "Remember com point as a marker. \(buffer specific\)")
;;   (set-default 'com-point (make-marker))
;;   (defun getcom (arg)
;;     "Get com part of prefix-argument ARG."
;;     (cond ((null arg) nil)
;;           ((consp arg) (cdr arg))
;;           (t nil)))
;;   (defun paren-match (arg)
;;     "Go to the matching parenthesis."
;;     (interactive "P")
;;     (let ((com (getcom arg)))
;;       (if (numberp arg)
;;           (if (or (> arg 99) (< arg 1))
;;               (error "Prefix must be between 1 and 99.")
;;             (goto-char
;;              (if (> (point-max) 80000)
;;                  (* (/ (point-max) 100) arg)
;;                (/ (* (point-max) arg) 100)))
;;             (back-to-indentation))
;;         (cond ((looking-at "[\(\[{]")
;;                (if com (move-marker com-point (point)))
;;                (forward-sexp 1)
;;                (if com
;;                    (paren-match nil com)
;;                  (backward-char)))
;;               ((looking-at "[])]}")
;;                (forward-char)
;;                (if com (move-marker com-point (point)))
;;                (backward-sexp 1)
;;                (if com (paren-match nil com)))
;;               (t (error ""))))))
;;   (define-key ctl-x-map "%" 'paren-match))

;; add to init_ac.el end

;; add to init_javascript.el
;; JavaScript
;; (when (locate-library "js2-mode")
;;   (autoload 'js2-mode "js2" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;   (setq js2-basic-offset 2)
;;   (setq js2-use-font-lock-faces t))

;; $BA43Q%9%Z!<%9!"%?%V$K?'$r$D$1$k(B
;; (defface my-face-b-1
;;   '((t (:background "gray")))
;;   nil)
;; (defface my-face-b-2
;;   '((t (:background "dim gray")))
;;   nil)
;; (defvar my-face-b-1
;;   'my-face-b-1)
;; (defvar my-face-b-2
;;   'my-face-b-2)

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-b-2 append)
     ("$B!!(B" 0 my-face-b-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode) (ad-activate 'font-lock-mode)


(defun toggle-truncate-lines ()
  "$B@^$jJV$7I=<($r%H%0%kF0:n$7$^$9(B."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))

(global-set-key "\C-c\C-l" 'toggle-truncate-lines) ; $B@^$jJV$7I=<((BON/OFF
(setq truncate-partial-width-windows nil)


;; add to init_php.el
;; php-mode$B@_Dj$r3d$jEv$F$^$9(B
(load-library "php-mode")
(require 'php-mode)
(add-hook 'php-mode-user-hook
          '(lambda ()
             (setq php-completion-file "~/.emacs.d/dictionary/php.dict")
             (define-key php-mode-map [(C-t)] 'php-complete-function)
             (global-set-key "\C-t" 'dabbrev-expand)
             ))

;; template$B$rJT=8$9$k$?$a$K(B
;; mmm-mode in php
;; (require 'mmm-mode)
;; (setq mmm-global-mode 'maybe)
;; (set-face-background 'mmm-default-submode-face nil) ;$BGX7J?'$,ITMW$J>l9g(B
;; (mmm-add-mode-ext-class nil "\\.php?\\'" 'html-php)
;; (mmm-add-classes
;;  '((html-php
;;     :submode php-mode
;;     :front "<\\?\\(php\\)?"
;;     :back "\\?>")))
;; (add-to-list 'auto-mode-alist '("\\.php?\\'" . xml-mode))

;;$B%*!<%H%3%s%W%j!<%HBP>]$K(Bphp-mode$B$rDI2C(B
(when (boundp 'ac-modes)
  (setq ac-modes
    (append ac-modes
      (list 'php-mode 'javascript-mode 'css-mode 'smarty-mode ))))

;; '.class'$B$O(Bphp-mode$B$G3+$/(B
;;(setq auto-mode-alist
;;(cons '("\\.class$" . php-mode) auto-mode-alist))

;; '.thtml'$B$O(Bphp-mode$B$G3+$/(B
;;(setq auto-mode-alist
;;(cons '("\\.thtml$" . php-mode) auto-mode-alist))

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.thtml$" . html-helper-mode) auto-mode-alist))

;; add to init_php.el
;; php-exec
(defun php-exec ()
  (interactive)
  (shell-command (concat "php " (buffer-file-name))))

;; php$B9=J8%A%'%C%/(B
(defun php-lint ()
  "Performs a PHP lint-check on the current file."
  (interactive)
  (shell-command (concat "php -l " (buffer-file-name))))
(add-hook 'php-mode-hook
          (lambda ()
            (define-key gtags-mode-map "\M-p" 'gtags-pop-stack)
            (gtags-mode)
          ))

;; php_codesniffer$B$G9=J8%A%'%C%/(B
(defun phpcs ()
  "Performs a PHP CodeSniffer-check on the current file."
  (interactive)
  (shell-command (concat "phpcs --standard=$HOME/Sites/mochiz_tools/Serverworks " (buffer-file-name))))

(require 'php-completion)

;; php$B%b!<%I$r(B
(add-hook 'php-mode-hook
          (lambda ()
             (php-completion-mode t)
             (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)))

;; (add-hook  'php-mode-hook
;;            (lambda ()
;;              (when (require 'auto-complete)
;;                (make-variable-buffer-local 'ac-sources)
;;                (add-to-list 'ac-sources 'ac-source-php-completion)
;;                ;; if you like patial match,
;;                ;; use `ac-source-php-completion-patial' instead of `ac-source-php-completion'.
;;                ;; I recommend this way.
;;                ;; (add-to-list 'ac-sources 'ac-source-php-completion-patial)
;;                (auto-complete-mode t))))

;; add to init_php.el
;; Flymake PHP Extension
(require 'flymake)
(require 'flymake-php)
(add-hook 'php-mode-hook 'flymake-php-load)

;; $B%U%!%$%kL>0J30$r0E$/I=<((B
(file-name-shadow-mode t)

;; $BHsJd40BP>]$+$i30$9(B
(setq completion-ignored-extensions (delete ".log" completion-ignored-extensions))
(setq completion-ignored-extensions (delete ".class" completion-ignored-extensions))

;; redo
(require 'redo)
;;(define-key global-map "\C-," 'redo)

;; Global
(require 'gtags)
(global-set-key "\M-," 'gtags-find-rtag)
(global-set-key "\M-s" 'gtags-find-tag)
(define-key gtags-mode-map "\M-p" 'gtags-pop-stack)
(add-hook 'php-mode-hook
          (lambda ()
            (define-key gtags-mode-map "\M-," 'gtags-find-rtag)
            (define-key gtags-mode-map "\M-s" 'gtags-find-tag)
            (define-key gtags-mode-map "\M-p" 'gtags-pop-stack)
            (gtags-mode)
          ))

;; ack
(defun ack ()
  (interactive)
  (let ((grep-find-command "ack --nocolor --nogroup "))
    (call-interactively 'grep-find)))

;; $BA*BrHO0O$r3HD%$9$k(B
;; sense-region
(autoload 'sense-region-on "sense-region"
          "System to toggle region and rectangle." t nil)
(sense-region-on)

;; $B9THV9f$rI=<($9$k(B
;; wb-line-number
(set-scroll-bar-mode nil)
(require 'wb-line-number)
(setq truncate-partial-width-windows t)
(setq wb-line-number-scroll-bar nil)
(setq wb-line-number-text-width 4)
;; (wb-line-number-toggle)


;; $B%;%C%7%g%sJ]B8(B
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;; $B%;%C%7%g%s>pJs$rJ]B8$9$k%G%#%l%/%H%j(B
;; (setq session-save-file (concat user-emacs-directory "session"))
;; (setq session-save-file "~/.emacs.d/session")
;; $B%U%!%$%k$rA02sJ]B8$7$?;~$G$O$J$/!"JD$8$?;~$N%+!<%=%k0LCV$r5-O?$9$k(B
(setq session-undo-check -1)
;; M-x $B$G<B9T$7$?%3%^%s%IMzNr$NJ]B87o?t(B
(setq history-length 1000000)
(setq session-initialize '(de-saveplace session menus places))
;; $B%;%C%7%g%s4IM}$9$k>pJs$N@_Dj(B
(setq session-globals-include '((kill-ring 100)            ;; $B%-%k%j%s%0(B100$B7o(B
                                (session-file-alist 100 t) ;; $B%+!<%=%k0LCV(B100$B2U=j(B
                                (session-globals-max-size 100000)
                                (file-name-history 300)))  ;; $B3+$$$?%U%!%$%k$N%Q%9(B300$B7o(B



;; bookmark$B$N@_Dj(B
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)
(global-set-key "\M-1" 'bm-toggle)
(global-set-key "\M-2" 'bm-previous)
(global-set-key "\M-3" 'bm-next)

;; $B9T?tI=<((B
(line-number-mode t)

;; $B%9%?!<%H%"%C%W%Z!<%8$rI=<($7$J$$(B
(setq inhibit-startup-message t)

;; $B%U%)%s%H%;%C%H:n@.(B
;;(create-fontset-from-mac-roman-font
;; "-apple-m+ 2m-medium-r-normal--14-*-*-*-*-*-iso10646-1" nil "mplus")
;;(set-fontset-font "fontset-mplus" 'japanese-jisx0208
;;                  '("m+ 2m light" . "iso10646"))
;;(set-fontset-font "fontset-mplus" (cons (make-char 'japanese-jisx0208 #x30 #x20)
;;                                        (make-char 'japanese-jisx0208 #x74 #x7f))
;;                  '("$B%R%i%.%N3Q%4(B pro w3" . "jisx0208.1983"))
;;(set-fontset-font "fontset-mplus" 'katakana-jisx0201
;;                  '("m+ 1m light" . "iso10646"))
;;
;; (create-fontset-from-mac-roman-font
;;  "-apple-m+ 2m-medium-r-normal--12-*-*-*-*-*-iso10646-1" nil "mplus")
;; (set-fontset-font "fontset-mplus" 'japanese-jisx0208
;;                   '("m+ 2m light" . "iso10646"))
;; (set-fontset-font "fontset-mplus" (cons (make-char 'japanese-jisx0208 #x30 #x20)
;;                                         (make-char 'japanese-jisx0208 #x74 #x7f))
;;                   '("$B%R%i%.%N3Q%4(B pro w3" . "jisx0208.1983"))
;; (set-fontset-font "fontset-mplus" 'katakana-jisx0201
;;                   '("m+ 1m light" . "iso10646"))

;; $B%U%)%s%H@_Dj(B
;; (add-to-list 'default-frame-alist '(font . "fontset-mplus"))
;; (setq fixed-width-rescale nil)

;; $B%&%#%s%I%&@_Dj(B
(if window-system (progn
  (setq initial-frame-alist '((width . 110) (height . 60) (top . 50)))
  (set-background-color "Black")
  (set-foreground-color "White")
  (set-cursor-color "Gray")
))

;; $B%&%#%s%I%&$rF)L@2=(B
(add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))


;; Tramp
;; (require 'tramp)
;; (setq tramp-default-method "sshx")
;; (add-to-list  'tramp-multi-connection-function-alist
;;   '("sshx" tramp-multi-connect-rlogin "ssh -t %h -l %u /bin/sh%n"))
;; (add-to-list  'tramp-multi-connection-function-alist
;;   '("sshp10023" tramp-multi-connect-rlogin "ssh -t %h -l %u -p 10022 /bin/sh%n"))



;;
;; pdf-preview $B$N@_Dj(B
;;

;; $BI8=`$G$O!"J8;z$,$A$g$C$H>.$5$a$J$N$G(B
(setq pdf-preview-font-rescale-factor 1.1)
(load "~/.emacs.d/elisp/pdf-preview")

;; $BJ8;z?'$rCV$-49$($k$?$a$NDj5A(B
(defun ps-set-color (color)
    ;;  "$B%P%C%/%0%i%&%s%I$r9u$G;H$C$F$$$k$H!"J8;z?'$OL@$k$/$J$k!#(B
    ;; $B$7$+$7!"0u:~$7$?;~$O%U%)%s%H?'$,L@$k2a$.$F8+$K$/$$!#(B
    ;; $B$"$^$j$h$/$J$$$1$I!"4X?t=q$-49$($F?'$r0E$/$9$k!#(B
    ;; $B?tCM!V(B1.5$B!W$r>.$5$/$9$l$P?'$,L@$k$/!"Bg$-$/$9$l$P?'$,0E$/$J$k!#(B
    ;; $BF0:n3NG'(B: ps-print.el ver.4.1.4"
  (setq ps-current-color (or color ps-default-fg))
  (ps-output (format ps-color-format
                     (/ (nth 0 ps-current-color) 2.0)
                     (/ (nth 1 ps-current-color) 2.0)
                     (/ (nth 2 ps-current-color) 2.0))
             " FG\\n"))

;; uniquify - $BF1L>%U%!%$%k$N%P%C%U%!L>$K?F%G%#%l%/%H%jL>$r4^$a$k(B
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; css-mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
     (cons '("\\.css\\'" . css-mode) auto-mode-alist))
;;css-mode $B%?%VI}$r(B4$B$K(B
(setq cssm-indent-level 4)
;;css-mode $B%$%s%G%s%H$r(Bc-style$B$K$9$k(B
(setq cssm-indent-function #'cssm-c-style-indenter)

;; $B%3%a%s%H%l%$%"%&%HJQ99(B
(setq comment-style 'multi-line)

;; $B%P%C%U%!$r<+F0J]B8(B
(require 'auto-save-buffers)
(run-with-idle-timer 10 t 'auto-save-buffers)

;; dsvn$B$G(Bsvn$B%3%^%s%I$r;HMQ$9$k(B
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

;;-----------------------------------------------------------------
;; psvn.el
;;-----------------------------------------------------------------
;; (require 'psvn)

;; (define-key svn-status-mode-map "q" 'egg-self-insert-command)
;; (define-key svn-status-mode-map "Q" 'svn-status-bury-buffer)
;; (define-key svn-status-mode-map "p" 'svn-status-previous-line)
;; (define-key svn-status-mode-map "n" 'svn-status-next-line)
;; (define-key svn-status-mode-map "<" 'svn-status-examine-parent)

;; (add-hook 'dired-mode-hook
;;           '(lambda ()
;;              (require 'dired-x)
;;              ;;(define-key dired-mode-map "V" 'cvs-examine)
;;              (define-key dired-mode-map "V" 'svn-status)
;;              (turn-on-font-lock)
;;              ))

;; (setq svn-status-hide-unmodified t)

;; (setq process-coding-system-alist
;;       (cons '("svn" . euc-jp) process-coding-system-alist))

;; add to init_markdown.el
;; markdown-mode
;; http://jblevins.org/projects/markdown-mode/
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.mdml$" . markdown-mode) auto-mode-alist))
 
;; markdown-mode$B$GMxMQ$9$k%3%^%s%I(B
(setq markdown-command "~/Application/markdown/markdown.php")
;;(setq markdown-command "~/Application/markdown/md_wrapper.php")

;; shell$B%b!<%I$NJ8;z2=$1BP:v(B
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;; svn st$B$GG[2<$N%G%#%l%/%H%j$+$iJQ99$r<hF@(B
(defun svnst ()
  (interactive)
  (shell-command "st"))
  ;; (shell-command "st | sort | awk '{print $2 $3 \" \\\"}' > ~/Sites/svnlogs"))

(put 'set-goal-column 'disabled nil)


;; shell-command$B$NJd405!G=$rM-8z$K$9$k(B
(require 'shell-command)
(shell-command-completion-mode)

;; @zen-codeing
;;   zen-coding.el$B$rM-8z$K$9$k(B
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; anything.el
(require 'anything-config)
(add-to-list 'anything-sources 'anything-c-source-emacs-commands)

(global-set-key "\C-xb" 'anything)

(define-key anything-map "\C-p" 'anything-previous-line)
(define-key anything-map "\C-n" 'anything-next-line)
(define-key anything-map "\C-v" 'anything-next-page)
(define-key anything-map "\M-v" 'anything-previous-page)


;; ruby-mode
;; @see http://curiosity-drives.me/programming/rails/rails_emacs_rinari_yasnippet_flymake/
(add-to-list 'load-path "~/.emacs.d/vendor/ruby-mode")
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

;; rinari
(add-to-list 'load-path "~/.emacs.d/vendor/rinari")
(require 'rinari)
(setq rinari-tags-file-name "TAGS")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/rhtml"))
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
(lambda () (rinari-launch)))

; yasnippet
;; (add-to-list 'load-path "~/.emacs.d/yasnippet")
;; (require 'yasnippet) ;; not yasnippet-bundle
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/yasnippet/snippets")
;; ;; rails-snippets
;; (yas/load-directory "~/.emacs.d/yasnippets-rails/rails-snippets")

;; yasnippet
;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/snippets")
;; (yas/load-directory "~/.emacs.d/rails-snippets")

;; ruby-electric.el --- electric editing commands for ruby files
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))


;; flymake for ruby
(require 'flymake)
;; Invoke ruby with '-c$,1ry(B to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
(add-hook
 'ruby-mode-hook
 '(lambda ()
    ;; Don$,1ry(Bt want flymake mode for ruby regions in rhtml files
    (if (not (null buffer-file-name)) (flymake-mode))
    ;; $B%(%i!<9T$G(B C-c d $B$9$k$H%(%i!<$NFbMF$r%_%K%P%C%U%!$GI=<($9$k(B
    (define-key ruby-mode-map "\C-cd" 'credmp/flymake-display-err-minibuf)))

(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
(setq count (1- count)))))


;; peepopen
(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
(require 'textmate)
(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'peepopen)
(textmate-mode)

;; textexpander
(setq ns-alternate-modifier 'alt)
(define-key global-map [(alt ?v)] 'scroll-down)
(define-key global-map [(meta ?v)] 'yank)

;; cucumber
;; (add-to-list 'load-path "~/.emacs.d/vendor/cucumber.el")
;; (autoload 'feature-mode "feature-mode" "Mode for editing cucumber files" t)
;; (add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))
;; (require 'feature-mode)

