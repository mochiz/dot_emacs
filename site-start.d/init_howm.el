;; ;;; -*- mode: emacs-lisp; indent-tabs-mode: nil -*-

;; ;;; init_howm.el --- Emacs howm setting

;; ;; Copyright (C) 2010  sakito

;; ;; Author: sakito <sakito@sakito.com>
;; ;; Keywords: tools

;; ;; This file is free software; you can redistribute it and/or modify
;; ;; it under the terms of the GNU General Public License as published by
;; ;; the Free Software Foundation; either version 3, or (at your option)
;; ;; any later version.

;; ;; This file is distributed in the hope that it will be useful,
;; ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; ;; GNU General Public License for more details.

;; ;; You should have received a copy of the GNU General Public License
;; ;; along with GNU Emacs; see the file COPYING.  If not, write to
;; ;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; ;; Boston, MA 02110-1301, USA.

;; ;;; Commentary:

;; ;; howm の設定

;; ;;; Code:
;; (require 'howm)
;; ;; elscreen-howm を利用
;; (require 'elscreen-howm)
;; ;; メニュー表示を日本語とする
;; (setq howm-menu-lang 'ja)
;; ;; 「最近のメモ」表示にタイトルを表示
;; (setq howm-list-recent-title t)
;; ;; 全てのメモを表示時にタイトルを表示
;; (setq howm-list-all-title t)

;; ;; rstでメモを書く
;; (setq auto-mode-alist
;;       (append '(
;;                 ("\\.howm$" . rst-mode)
;;                 )
;;               auto-mode-alist))

;; ;; howm 以外から *.howm を開いたときも常に howm-mode
;; (add-hook 'find-file-hooks
;;           (lambda ()
;;             (when (string-match "\\.howm$" (buffer-file-name))
;;               (howm-mode t))))

;; ;; テンプレートの形式を変更
;; (setq howm-template
;;       (concat howm-view-title-header " %title%cursor\n========================================\n\n"))

;; ;; RET でファイルを開く際, 一覧バッファを消す
;; ;; C-u RET なら残る
;; (setq howm-view-summary-persistent nil)

;; ;; GNU grepを使用する
;; ;; (setq howm-view-use-grep t)
;; ;; (setq howm-view-grep-command "grep")
;; ;; (setq howm-view-fgrep-command "grep")
;; ;; (setq howm-view-grep-extended-option "-E")
;; ;; (setq howm-view-grep-fixed-option "-F")

;; ;; howm の時は auto-fill にする
;; ;(add-hook 'howm-mode-on-hook 'auto-fill-mode)
;; ;; howm の時は auto-fill にしない
;; (add-hook 'howm-mode-on-hook 'turn-off-auto-fill)

;; ;; grep のオプション デフォルトは -Hnr --exclude-dir=RCS --exclude-dir=CVS --exclude-dir=.svn --exclude-dir=.git --exclude-dir=_darcs
;; (setq howm-view-grep-option "-Hnr --exclude-dir=RCS --exclude-dir=CVS --exclude-dir=.svn --exclude-dir=.git --exclude-dir=_darcs --exclude-dir=.hg --include=*.howm --include=*.rst --include=*.txt")

(provide 'init_howm)
;; ;;; init_howm.el ends here

;; add to init_howm.el
;; hown
(setq howm-menu-lang 'ja)
(require 'howm-mode)

(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(mapc
 (lambda (f)
   (autoload f     "howm" "Hitori Otegaru Wiki Modoki" t))
 '(howm-menu howm-list-all howm-list-recent
             howm-list-grep howm-create
             howm-keyword-to-kill-ring))

;; リンクを TAB で辿る
(eval-after-load "howm-mode"
  '(progn
     (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
     (define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)))
;; 「最近のメモ」一覧時にタイトル表示
(setq howm-list-recent-title t)
;; 全メモ一覧時にタイトル表示
(setq howm-list-all-title t)
;; メニューを 2 時間キャッシュ
(setq howm-menu-expiry-hours 2)

;;Text mode and Auto Fill mode
;;Auto Fill modeで折り返し
;;１行の文字数80文字に
(setq default-fill-column 180 )

;; howm の時は auto-fill で
(add-hook 'howm-mode-on-hook 'auto-fill-mode)

;; RET でファイルを開く際, 一覧バッファを消す
;; C-u RET なら残る
(setq howm-view-summary-persistent nil)

;; メニューの予定表の表示範囲
;; 10 日前から
(setq howm-menu-schedule-days-before 10)
;; 3 日後まで
(setq howm-menu-schedule-days 3)

;; howm のファイル名
;; 以下のスタイルのうちどれかを選んでください
;; で，不要な行は削除してください
;; 1 メモ 1 ファイル (デフォルト)
(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.howm")
;; 1 日 1 ファイルであれば
;;(setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")

(setq howm-view-grep-parse-line
      "^\\(\\([a-zA-Z]:/\\)?[^:]*\\.howm\\):\\([0-9]*\\):\\(.*\\)$")
;; 検索しないファイルの正規表現
(setq
 howm-excluded-file-regexp
 "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$")

;; いちいち消すのも面倒なので
;; 内容が 0 ならファイルごと削除する
(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (string-match "\\.howm" (buffer-file-name (current-buffer)))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))

;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer
;; C-cC-c で保存してバッファをキルする
(defun my-save-and-kill-buffer ()
  (interactive)
  (when (and
         (buffer-file-name)
         (string-match "\\.howm"
                       (buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))
(eval-after-load "howm"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-c" 'my-save-and-kill-buffer)))

 ;; メニューを自動更新しない
(setq howm-menu-refresh-after-save nil)
;; 下線を引き直さない
(setq howm-refresh-after-save nil)
;; 保存先ディレクトリをDropboxへ変更する
(setq howm-directory "~/Dropbox/Document/howm/") 

;; howm-color タイトルが見づらいので、文字色変更
(set-face-background 'howm-mode-title-face "Black")
(set-face-foreground 'howm-mode-title-face "LawnGreen")

;; hown一覧時にウィンドウ分割を壊さない設定
(setq riffle-keep-window t)
(setq riffle-window-initializer nil)
