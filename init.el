
;;; -*- mode: emacs-lisp; indent-tabs-mode: nil -*-

;;init.el -- Emacs init setting elisp file

(setq user-full-name "mochiz")
(setq user-mail-address "mochi.hiro43@gmail.com")

;; $(B>o;~%G%P%C%0>uBV
(B(setq debug-on-error t)

;; Emacs $(B@_Dj%G%#%l%/%H%j$r@_Dj!#(BEmacs 22$(B0J2<MQ

(B;; Emacs 23.1 $(B0J>e$G$O(B user-emacs-directory $(BJQ?t$,MQ0U$5$l$F$$$k$N$G$=$l$rMxMQ
(B(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")))

;; $(B0z?t$r(B load-path $(B$XDI2C
(B;; normal-top-level-add-subdirs-to-load-path $(B$O%G%#%l%/%H%jCf$NCf$G
(B;; [A-Za-z] $(B$G3+;O$9$kJ*$@$1DI2C$9$k$N$G!"DI2C$7$?$/$J$$J*$O(B . $(B$d(B _ $(B$r@hF,$KIUM?$7$F$*$1$P%m!<%I$7$J$$
(B;; dolist $(B$O(B Emacs 21 $(B$+$iI8=`4X?t$J$N$G@Q6KE*$KMxMQ$7$FNI$$
(B(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; Emacs Lisp $(B$N(BPath$(B$rDL$9
(B(add-to-load-path "elisp"
                  ;; $(BJQ99$7$?$j!"<+:n$N(B Emacs Lisp
                  "local-lisp"
                  ;; private $(BFb$K$O<+J,@lMQ$NJ*$,$O$$$C$F$$$k!#0MB8$O(B private $(BFb$G407k$9$k$h$&$K$7$F$$$k
(B                  "private"
                  ;; vendor
                  "vendor"
                  ;; $(B=i4|@_Dj%U%!%$%k
(B                  "site-start.d")

;; Emacs $(B$N<oN`%P!<%8%g%s$rH=JL$9$k$?$a$NJQ?t$rDj5A
(B;; @see http://github.com/elim/dotemacs/blob/master/init.el
(defun x->bool (elt) (not (not elt)))
(defvar emacs22-p (equal emacs-major-version 22))
(defvar emacs23-p (equal emacs-major-version 23))
(defvar darwin-p (eq system-type 'darwin))
(defvar carbon-p (and (eq window-system 'mac) emacs22-p))
(defvar mac-p (and (eq window-system 'mac) emacs23-p))
(defvar linux-p (eq system-type 'gnu/linux))

;; $(BJ8;z%3!<%I
(B;;(set-language-environment 'Japanese)
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)

;; $(BA44D6-6&DL@_Dj
(B(require 'init_global)
(require 'init_howm)

;; $(B4D6-0MB8@_Dj
(B;; (cond
;;  (mac-p (require 'init_main))
;;  (carbon-p (require 'init_syscarbon))
;;  )

;; $(B=*N;;~%P%$%H%3%s%Q%$%k
(B;; (add-hook 'kill-emacs-query-functions
;;           (lambda ()
;;             (if (file-newer-than-file-p (concat user-emacs-directory "init.el")
;;                                         (concat user-emacs-directory "init.elc"))
;;                 (byte-compile-file (concat user-emacs-directory "init.el")))
;;             (byte-recompile-directory (concat user-emacs-directory "local-lisp") 0)
;;             (byte-recompile-directory (concat user-emacs-directory "private") 0)
;;             (byte-recompile-directory (concat user-emacs-directory "site-start.d") 0)
;;             ))

;; $(B5/F0;~4V7WB,(B $(BL\I8$O>o$K(B 3000ms $(B7wFb(B(dump-emacs $(B$9$l$P2DG=$@$,$7$F$J$$(B)
;; (when emacs23-p
;;   (defun message-startup-time ()
;;     (message "Emacs loaded in %dms"
;;              (/ (- (+ (third after-init-time) (* 1000000 (second after-init-time)))
;;                    (+ (third before-init-time) (* 1000000 (second before-init-time))))
;;                 1000)))
;;   (add-hook 'after-init-hook 'message-startup-time))



