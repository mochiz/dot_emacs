;; e-palette.el
;;
;; This is free software
;;
;; * Install
;;
;; (autoload 'e-palette "e-palette" nil)
;; (define-key global-map [f2] 'e-palette)

(require 'custom)

(defun e-palette-mode ()
  (setq mode-name "color palette mode"
	major-mode 'e-palette-mode)
  (use-local-map e-palette-keymap))

;;(when (boundp 'show-trailing-whitespace) (setq-default show-trailing-whitespace t))

(defvar e-palette-keymap nil)
(unless e-palette-keymap
  (setq e-palette-keymap (make-sparse-keymap))
  (define-key e-palette-keymap "\C-c\C-c" 'e-palette-quit)
  (define-key e-palette-keymap "q"        'e-palette-quit)
  (define-key e-palette-keymap "l"        'e-palette-forward-char)
  (define-key e-palette-keymap "h"        'e-palette-backward-char)
  (define-key e-palette-keymap "j"        'next-line)
  (define-key e-palette-keymap "k"        'previous-line)
  (define-key e-palette-keymap "e"        'end-of-line)
  (define-key e-palette-keymap "a"        'beginning-of-line)
  (define-key e-palette-keymap "n"        'e-palette-next)
  (define-key e-palette-keymap "p"        'e-palette-previous)
  (define-key e-palette-keymap "\C-m"     'e-palette-insert-color)
  (define-key e-palette-keymap "g"        'e-palette-select)
  ;;(define-key e-palette-keymap "<"        'e-palette-column-down)
  ;;(define-key e-palette-keymap ">"        'e-palette-column-up)
  )

(defvar *e-palette-window-data* nil "window data")
(defvar *e-palette-buffer-name* "*e-palette*")
(defvar *e-palette-current*     nil "current color palette")
(defvar *e-palette-current-column* nil "current column")

(defcustom e-palette-table
  '((*visibone*   . 16)
    (*visibone-2* . 16)
    (*greens*     . 16)
    (*plasma*     . 16)
    (*web*        . 8)
    (*named-colors* . 16)
    (*royal*      . 16)
    (*grayViolet* . 16)
    (*cranes*     . 16)
    (*bears*      . 16)
    )
  "color palette table ((*e-palette-name*  .  column) ...)"
  :group 'e-palette)

(defcustom e-palette-default
  '*visibone-2* "Color palette default"
  :group 'e-palette)

(defcustom e-palette-window-max
  0.6 "Color palette window max height"
  :group 'e-palette)

(defcustom e-palette-width
  nil "Color palette color width"
  :group 'e-palette)

(defcustom e-palette-insert-format
  "#%.2X%.2X%.2X" "color palette insert format"
  :group 'e-palette)


;;; * Code

(defun e-palette (palette)
  (interactive "P")
  (setq *e-palette-window-data* (current-window-configuration))
  (with-edit-temp-buffer *e-palette-buffer-name*
			 e-palette-window-max)
  (let ((data (e-palette-read)))
    (e-palette-display (car (e-palette-read palette)))
    (e-palette-mode)))

(defun e-palette-read (&optional palette)
  (let* ((palette (if palette (intern
			       (completing-read "Color palette: "
						(e-palette-table-symbol-string e-palette-table)))
		    e-palette-default)))
    (assoc palette e-palette-table)))

(defun e-palette-display (palette-name &optional column)
  (setq buffer-read-only nil)
  (erase-buffer)
  (let* ((data (assq palette-name e-palette-table))
	 (column (or column (cdr data)))
	 (color-lst (symbol-value (car data)))
	 (width (or e-palette-width
		   (/ (window-width) column)))
	 (height (/ (window-height) column))
	 (count 0) s)
    (dolist (color color-lst)
      (unless (< count column)
	(insert "\n")
	(setq count 0))
      (setq s (point))
      (insert (make-string width ?\ ))
      (e-palette-background-set s color)
      (setq count (+ count 1)))
    (kill-all-local-variables)
    (goto-char (point-min)))
  (setq *e-palette-current* palette-name)
  (e-palette-mode)
  (setq buffer-read-only t))

(defun e-palette-select ()
  (interactive)
  (let ((data (e-palette-read t)))
    (e-palette-display (car data))
    ))

;;; * Util

(defun e-palette-background-set (s color)
  (put-text-property s (point)
		     'face
		     (cons 'background-color color)))

(defun e-palette-foreground-set (s color)
  (put-text-property s (point)
		     'face
		     (cons 'foreground-color color)))

(defun e-palette-split (color-str)
  (mapcar (lambda (x)
	    (string-to-number x 16))
	  (list (substring color-str 1 3)
		(substring color-str 3 5)
		(substring color-str 5 7))))

(defun e-palette-table-symbol-string (table)
  (mapcar (lambda (x)
	    (cons (symbol-name (car x))
		  (cdr x)))
	  e-palette-table))

(defun e-palette-index (palette-name)
  (let ((max (length e-palette-table))
	(index 0))
    (catch 'return
      (while (< index max)
	(if (eq (car (nth index e-palette-table))
		palette-name)
	    (throw 'return index)
	  (setq index (+ index 1)))))))

(defun e-palette-index-palette (index)
  (let ((max (length e-palette-table)))
    (if (and (< index max) (<= 0 index))
	(car (nth index e-palette-table))
      nil)))

(defun e-palette-last-p (palette)
  (if (= (e-palette-index palette) (1- (length e-palette-table))) t nil))

(defun e-palette-previous-p (palette)
  (if (= (e-palette-index palette) 0) t nil))

(defun e-palette-refresh ()
  (interactive)
  (e-palette-display *e-palette-current*))

(defun e-palette-get-format ()
  (mapcar (lambda (x)  (insert (concat "\"" x "\"" "\n")))
	  (mapcar (lambda (x) (let ((color (mapcar (lambda (x) (string-to-number x)) (split-string (car x) " "))))
			    (format "#%.2X%.2X%.2X" (nth 0 color)
				    (nth 1 color)
				    (nth 2 color))))
		  (regex-match-list "^\\(.+\\)$" (point)))))

;;; * Motion

(defun e-palette-forward-char ()
  (interactive)
  (goto-char (next-property-change (point))))

(defun e-palette-backward-char ()
  (interactive)
  (goto-char (previous-property-change (point))))

(defun e-palette-insert-color (arg)
  (interactive "P")
  (let ((color (e-palette-split (cdr (get-text-property (point) 'face)))))
    (e-palette-quit)
    (insert (apply (lambda (r g b) (format e-palette-insert-format r g b))
		   (if arg
		       (mapcar (lambda (x) (- 255 x)) color)
		     color)))))

(defun e-palette-next ()
  (interactive)
  (e-palette-display 
   (let ((index (e-palette-index *e-palette-current*)))
     (if (e-palette-last-p *e-palette-current*)
	 (e-palette-index-palette 0)
       (e-palette-index-palette (1+ index))))))

(defun e-palette-previous ()
  (interactive)
  (e-palette-display
   (let ((index (e-palette-index *e-palette-current*)))
     (if (e-palette-previous-p *e-palette-current*)
	 (e-palette-index-palette (1- (length e-palette-table)))
       (e-palette-index-palette (1- index))))))

(defun e-palette-quit ()
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration *e-palette-window-data*))

(defun with-edit-temp-buffer (buffer-name &optional high)
  (let* ((cur-win (selected-window))
	 (size (if high (truncate (* (window-height cur-win) high))
		 window-min-height))
	 (wincfg (current-window-configuration))
	 (buffer (generate-new-buffer buffer-name)))
    (condition-case nil
	(split-window cur-win (if (> size 0) size window-min-height))
      (error
       (delete-other-windows)
       (split-window cur-win (- (window-height cur-win)
				10))))
    (select-window (next-window))
    (let ((pop-up-windows nil))
      (switch-to-buffer buffer)
      (set-buffer buffer))
    buffer)
  )


;;; * Color data

(defvar *visibone*
  '("#FFFFFF" "#CCCCCC" "#999999" "#666666" "#333333" "#000000"
    "#0033FF" "#0066FF" "#0099FF" "#00CCFF" "#000000" "#333333"
    "#666666" "#999999" "#CCCCCC" "#FFFFFF" "#000000" "#000000"
    "#000000" "#000000" "#000000" "#000000" "#3366FF" "#6699FF"
    "#66CCFF" "#33CCFF" "#000000" "#000000" "#000000" "#000000"
    "#000000" "#000000" "#000000" "#000000" "#000000" "#000000"
    "#000000" "#000000" "#0033CC" "#3366CC" "#3399CC" "#0099CC"
    "#000000" "#000000" "#000000" "#000000" "#000000" "#000000"
    "#000000" "#000000" "#000033" "#000066" "#000099" "#0000CC"
    "#0000FF" "#003399" "#006699" "#003333" "#006666" "#009999"
    "#00CCCC" "#00FFFF" "#000000" "#000000" "#3300FF" "#6633FF"
    "#3300CC" "#333366" "#333399" "#3333CC" "#3333FF" "#003366"
    "#000000" "#336666" "#339999" "#33CCCC" "#33FFFF" "#00CC99"
    "#33FFCC" "#00FFCC" "#9900FF" "#CC66FF" "#9933CC" "#660099"
    "#666699" "#6666CC" "#6666FF" "#336699" "#0066CC" "#669999"
    "#66CCCC" "#66FFFF" "#009966" "#33CC99" "#66FFCC" "#00FF99"
    "#9900FF" "#CC66FF" "#9933CC" "#660099" "#000000" "#9999CC"
    "#9999FF" "#6699CC" "#3399FF" "#99CCCC" "#99FFFF" "#000000"
    "#009933" "#33CC66" "#66FF99" "#00FF66" "#CC00FF" "#CC33FF"
    "#9900CC" "#330066" "#6600CC" "#9933FF" "#CCCCFF" "#99CCFF"
    "#000000" "#CCFFFF" "#66CC99" "#339966" "#006633" "#00CC33"
    "#33FF66" "#00FF33" "#FF00FF" "#000000" "#000000" "#663399"
    "#9966CC" "#CC99FF" "#000000" "#000000" "#000000" "#000000"
    "#99FFCC" "#33FF99" "#00CC66" "#000000" "#336633" "#003300"
    "#CC00CC" "#FF33FF" "#FF66FF" "#000000" "#000000" "#000000"
    "#000000" "#000000" "#000000" "#000000" "#000000" "#000000"
    "#99CC99" "#669966" "#339933" "#006600" "#990099" "#CC33CC"
    "#CC66CC" "#FF99FF" "#FFCCFF" "#000000" "#000000" "#000000"
    "#000000" "#000000" "#000000" "#CCFFCC" "#99FF99" "#66CC66"
    "#33CC33" "#009900" "#660066" "#993399" "#996699" "#CC99CC"
    "#000000" "#000000" "#000000" "#000000" "#000000" "#000000"
    "#000000" "#000000" "#000000" "#66FF66" "#33FF33" "#00CC00"
    "#330033" "#663366" "#000000" "#CC0066" "#FF3399" "#FF99CC"
    "#000000" "#000000" "#000000" "#000000" "#CCFF99" "#99CC66"
    "#669933" "#000000" "#000000" "#00FF00" "#FF00CC" "#FF33CC"
    "#CC0099" "#660033" "#993366" "#CC6699" "#FFCCCC" "#000000"
    "#FFCC99" "#FFFFCC" "#99FF33" "#66CC00" "#336600" "#33CC00"
    "#66FF33" "#33FF00" "#FF0099" "#FF66CC" "#CC3399" "#990066"
    "#000000" "#FF9999" "#CC9999" "#FF9933" "#CC9966" "#FFFF99"
    "#CCCC99" "#000000" "#339900" "#66CC33" "#99FF66" "#66FF00"
    "#FF0066" "#FF6699" "#CC3366" "#990033" "#FF6666" "#CC6666"
    "#996666" "#CC6600" "#996633" "#FFFF66" "#CCCC66" "#999966"
    "#669900" "#99CC33" "#CCFF66" "#99FF00" "#FF0033" "#FF3366"
    "#CC0033" "#FF3333" "#CC3333" "#993333" "#663333" "#000000"
    "#663300" "#FFFF33" "#CCCC33" "#999933" "#666633" "#99CC00"
    "#CCFF33" "#CCFF00" "#000000" "#000000" "#FF0000" "#CC0000"
    "#990000" "#660000" "#330000" "#993300" "#996600" "#FFFF00"
    "#CCCC00" "#999900" "#666600" "#333300" "#000000" "#000000"
    "#000000" "#000000" "#000000" "#000000" "#000000" "#000000"
    "#CC3300" "#CC6633" "#CC9933" "#CC9900" "#000000" "#000000"
    "#000000" "#000000" "#000000" "#000000" "#000000" "#000000"
    "#000000" "#000000" "#000000" "#000000" "#FF6633" "#FF9966"
    "#FFCC66" "#FFCC33" "#000000" "#000000" "#000000" "#000000"
    "#000000" "#000000" "#FFFFFF" "#CCCCCC" "#999999" "#666666"
    "#333333" "#000000" "#FF3300" "#FF6600" "#FF9900" "#FFCC00"
    "#000000" "#333333" "#666666" "#999999" "#CCCCCC" "#FFFFFF"))

(defvar *visibone-2*
  '("#FFFFFF" "#CCCCCC" "#999999" "#666666" "#333333" "#000000"
    "#FFCC00" "#FF9900" "#FF6600" "#FF3300" "#000000" "#333333"
    "#666666" "#999999" "#CCCCCC" "#FFFFFF" "#99CC00" "#000000"
    "#000000" "#000000" "#000000" "#CC9900" "#FFCC33" "#FFCC66"
    "#FF9966" "#FF6633" "#CC3300" "#000000" "#000000" "#000000"
    "#000000" "#CC0033" "#CCFF00" "#CCFF33" "#333300" "#666600"
    "#999900" "#CCCC00" "#FFFF00" "#CC9933" "#CC6633" "#330000"
    "#660000" "#990000" "#CC0000" "#FF0000" "#FF3366" "#FF0033"
    "#99FF00" "#CCFF66" "#99CC33" "#666633" "#999933" "#CCCC33"
    "#FFFF33" "#996600" "#993300" "#663333" "#993333" "#CC3333"
    "#FF3333" "#CC3366" "#FF6699" "#FF0066" "#66FF00" "#99FF66"
    "#66CC33" "#669900" "#999966" "#CCCC66" "#FFFF66" "#996633"
    "#663300" "#996666" "#CC6666" "#FF6666" "#990033" "#CC3399"
    "#FF66CC" "#FF0099" "#33FF00" "#66FF33" "#339900" "#66CC00"
    "#99FF33" "#CCCC99" "#FFFF99" "#CC9966" "#CC6600" "#CC9999"
    "#FF9999" "#FF3399" "#CC0066" "#990066" "#FF33CC" "#FF00CC"
    "#00CC00" "#33CC00" "#336600" "#669933" "#99CC66" "#CCFF99"
    "#FFFFCC" "#FFCC99" "#FF9933" "#FFCCCC" "#FF99CC" "#CC6699"
    "#993366" "#660033" "#CC0099" "#330033" "#33CC33" "#66CC66"
    "#00FF00" "#33FF33" "#66FF66" "#99FF99" "#CCFFCC" "#000000"
    "#000000" "#000000" "#CC99CC" "#996699" "#993399" "#990099"
    "#663366" "#660066" "#006600" "#336633" "#009900" "#339933"
    "#669966" "#99CC99" "#000000" "#000000" "#000000" "#FFCCFF"
    "#FF99FF" "#FF66FF" "#FF33FF" "#FF00FF" "#CC66CC" "#CC33CC"
    "#003300" "#00CC33" "#006633" "#339966" "#66CC99" "#99FFCC"
    "#CCFFFF" "#3399FF" "#99CCFF" "#CCCCFF" "#CC99FF" "#9966CC"
    "#663399" "#330066" "#9900CC" "#CC00CC" "#00FF33" "#33FF66"
    "#009933" "#00CC66" "#33FF99" "#99FFFF" "#99CCCC" "#0066CC"
    "#6699CC" "#9999FF" "#9999CC" "#9933FF" "#6600CC" "#660099"
    "#CC33FF" "#CC00FF" "#00FF66" "#66FF99" "#33CC66" "#009966"
    "#66FFFF" "#66CCCC" "#669999" "#003366" "#336699" "#6666FF"
    "#6666CC" "#666699" "#330099" "#9933CC" "#CC66FF" "#9900FF"
    "#00FF99" "#66FFCC" "#33CC99" "#33FFFF" "#33CCCC" "#339999"
    "#336666" "#006699" "#003399" "#3333FF" "#3333CC" "#333399"
    "#333366" "#6633CC" "#9966FF" "#6600FF" "#00FFCC" "#33FFCC"
    "#00FFFF" "#00CCCC" "#009999" "#006666" "#003333" "#3399CC"
    "#3366CC" "#0000FF" "#0000CC" "#000099" "#000066" "#000033"
    "#6633FF" "#3300FF" "#00CC99" "#000000" "#000000" "#000000"
    "#000000" "#0099CA" "#33CCFF" "#66CCFF" "#6699FF" "#3366FF"
    "#0033CC" "#000000" "#000000" "#000000" "#000000" "#3300CC"
    "#FFFFFF" "#CCCCCC" "#999999" "#666666" "#333333" "#000000"
    "#00CCFF" "#0099FF" "#0066FF" "#0033FF" "#000000" "#333333"
    "#666666" "#999999" "#CCCCCC" "#FFFFFF"))

(defvar *greens*
  '("#000000" "#000000" "#000800" "#000C00" "#001000" "#001400"
    "#001800" "#001C00" "#002000" "#002400" "#002800" "#002C00"
    "#003000" "#003400" "#003800" "#003C00" "#004000" "#004400"
    "#004800" "#004C00" "#005000" "#005400" "#005800" "#005C00"
    "#006000" "#006400" "#006800" "#006C00" "#007000" "#007400"
    "#007800" "#007C00" "#008000" "#008400" "#008800" "#008C00"
    "#009000" "#009400" "#009800" "#009C00" "#00A000" "#00A400"
    "#00A800" "#00AC00" "#00B000" "#00B400" "#00B800" "#00BC00"
    "#00C000" "#00C400" "#00C800" "#00CC00" "#00D000" "#00D400"
    "#00D800" "#00DC00" "#00E000" "#00E400" "#00E800" "#00EC00"
    "#00F000" "#00F400" "#00F800" "#00FC00" "#08FC00" "#0CFC00"
    "#10FC00" "#18FC00" "#1CFC00" "#20FC00" "#28FC00" "#2CFC00"
    "#30FC00" "#38FC00" "#3CFC00" "#40FC00" "#48FC00" "#4CFC00"
    "#50FC00" "#54FC00" "#5CFC00" "#60FC00" "#64FC00" "#6CFC00"
    "#70FC00" "#74FC00" "#7CFC00" "#80FC00" "#84FC00" "#8CFC00"
    "#90FC00" "#94FC00" "#9CFC00" "#A0FC00" "#A4FC00" "#A8FC00"
    "#B0FC00" "#B4FC00" "#B8FC00" "#C0FC00" "#C4FC00" "#C8FC00"
    "#D0FC00" "#D4FC00" "#D8FC00" "#E0FC00" "#E4FC00" "#E8FC00"
    "#F0FC00" "#F4FC00" "#F8FC00" "#FCFC00" "#FCFC08" "#FCFC0C"
    "#FCFC10" "#FCFC18" "#FCFC1C" "#FCFC20" "#FCFC28" "#FCFC2C"
    "#FCFC30" "#FCFC38" "#FCFC3C" "#FCFC40" "#FCFC48" "#FCFC4C"
    "#FCFC50" "#FCFC54" "#FCFC5C" "#FCFC60" "#FCFC64" "#FCFC6C"
    "#FCFC70" "#FCFC74" "#FCFC7C" "#FCFC80" "#FCFC84" "#FCFC8C"
    "#FCFC90" "#FCFC94" "#FCFC9C" "#FCFCA0" "#FCFCA4" "#FCFCA8"
    "#FCFCB0" "#FCFCB4" "#FCFCB8" "#FCFCC0" "#FCFCC4" "#FCFCC8"
    "#FCFCD0" "#FCFCD4" "#FCFCD8" "#FCFCE0" "#FCFCE4" "#FCFCE8"
    "#FCFCF0" "#FCFCF4" "#FCFCF8" "#FCFCFC" "#F8FCF4" "#F8FCEC"
    "#F4FCE4" "#F4FCDC" "#F0FCD4" "#F0FCCC" "#F0FCC4" "#ECFCBC"
    "#ECFCB4" "#E8FCAC" "#E8FCA4" "#E8FC9C" "#E4FC94" "#E4FC8C"
    "#E0FC84" "#E0FC7C" "#E0FC74" "#DCFC6C" "#DCFC64" "#D8FC5C"
    "#D8FC54" "#D8FC4C" "#D4FC44" "#D4FC3C" "#D0FC34" "#D0FC2C"
    "#D0FC24" "#CCFC1C" "#CCFC14" "#C8FC0C" "#C8FC04" "#C8FC00"
    "#C0F800" "#B8F800" "#B4F400" "#ACF400" "#A8F000" "#A0F000"
    "#9CF000" "#94EC00" "#8CEC00" "#88E800" "#80E800" "#7CE800"
    "#74E400" "#70E400" "#68E000" "#64E000" "#5CE000" "#54DC00"
    "#50DC00" "#48D800" "#44D800" "#3CD800" "#38D400" "#30D400"
    "#28D000" "#24D000" "#1CD000" "#18CC00" "#10CC00" "#0CC800"
    "#04C800" "#00C800" "#00C000" "#00B800" "#00B400" "#00AC00"
    "#00A400" "#00A000" "#009800" "#009400" "#008C00" "#008400"
    "#008000" "#007800" "#007400" "#006C00" "#006400" "#006000"
    "#005800" "#005000" "#004C00" "#004400" "#004000" "#003800"
    "#003000" "#002C00" "#002400" "#002000" "#001800" "#001000"
    "#000C00" "#000400" "#000000" "#000000"))

(defvar *plasma*
  '("#00F000" "#00E010" "#00D020" "#00C030" "#00B040" "#00A050"
    "#009060" "#008070" "#007080" "#006090" "#0050A0" "#0040B0"
    "#0030C0" "#0020D0" "#0010E0" "#0000F0" "#10E000" "#10D010"
    "#10C420" "#10B430" "#10A83C" "#10984C" "#108C5C" "#107C6C"
    "#107078" "#106088" "#105498" "#1044A8" "#1038B4" "#1028C4"
    "#101CD4" "#1010E0" "#20D000" "#20C410" "#20B81C" "#20AC2C"
    "#20A038" "#209448" "#208854" "#207C64" "#207070" "#206480"
    "#20588C" "#204C9C" "#2040A8" "#2034B8" "#2028C4" "#2020D0"
    "#30C000" "#30B410" "#30AC1C" "#30A028" "#309834" "#309040"
    "#308450" "#307C5C" "#307068" "#306874" "#306080" "#305490"
    "#304C9C" "#3040A8" "#3038B4" "#3030C0" "#40B000" "#40A80C"
    "#40A018" "#409824" "#409030" "#40883C" "#408048" "#407854"
    "#407460" "#406C6C" "#406478" "#405C84" "#405490" "#404C9C"
    "#4044A8" "#4040B0" "#50A000" "#50980C" "#509418" "#509020"
    "#50882C" "#508438" "#508040" "#50784C" "#507458" "#507060"
    "#50686C" "#506478" "#506080" "#50588C" "#505498" "#5050A0"
    "#609000" "#608C0C" "#608814" "#608420" "#608028" "#608030"
    "#607C3C" "#607844" "#607450" "#607058" "#607060" "#606C6C"
    "#606874" "#606480" "#606088" "#606090" "#708000" "#707C0C"
    "#707C14" "#707C1C" "#707824" "#70782C" "#707834" "#70783C"
    "#707448" "#707450" "#707458" "#707460" "#707068" "#707070"
    "#707078" "#707080" "#807000" "#807408" "#807410" "#807418"
    "#807820" "#807828" "#807830" "#807838" "#807C3C" "#807C44"
    "#807C4C" "#807C54" "#80805C" "#808064" "#80806C" "#808070"
    "#906000" "#906408" "#906810" "#906C14" "#90701C" "#907020"
    "#907428" "#907830" "#907C34" "#90803C" "#908040" "#908448"
    "#908850" "#908C54" "#90905C" "#909060" "#A05000" "#A05808"
    "#A05C0C" "#A06010" "#A06818" "#A06C1C" "#A07020" "#A07828"
    "#A07C2C" "#A08030" "#A08838" "#A08C3C" "#A09040" "#A09848"
    "#A09C4C" "#A0A050" "#B04000" "#B04808" "#B0500C" "#B05810"
    "#B06014" "#B06818" "#B0701C" "#B07820" "#B07C24" "#B08428"
    "#B08C2C" "#B09430" "#B09C34" "#B0A438" "#B0AC3C" "#B0B040"
    "#C03000" "#C03C04" "#C04408" "#C0500C" "#C05810" "#C06010"
    "#C06C14" "#C07418" "#C0801C" "#C08820" "#C09020" "#C09C24"
    "#C0A428" "#C0B02C" "#C0B830" "#C0C030" "#D02000" "#D02C04"
    "#D03808" "#D04408" "#D0500C" "#D05C0C" "#D06810" "#D07410"
    "#D08014" "#D08C14" "#D09818" "#D0A418" "#D0B01C" "#D0BC1C"
    "#D0C820" "#D0D020" "#E01000" "#E02004" "#E02C04" "#E03C04"
    "#E04808" "#E05808" "#E06408" "#E07408" "#E0800C" "#E0900C"
    "#E09C0C" "#E0AC0C" "#E0B810" "#E0C810" "#E0D410" "#E0E010"
    "#F00000" "#F01000" "#F02000" "#F03000" "#F04000" "#F05000"
    "#F06000" "#F07000" "#F08000" "#F09000" "#F0A000" "#F0B000"
    "#F0C000" "#F0D000" "#F0E000" "#F0F000"))

(defvar *web*
  '("#000000" "#000033" "#000066" "#000099" "#0000CC" "#0000FF"
		"#003300" "#003333" "#003366" "#003399" "#0033CC" "#0033FF"
		"#006600" "#006633" "#006666" "#006699" "#0066CC" "#0066FF"
		"#009900" "#009933" "#009966" "#009999" "#0099CC" "#0099FF"
		"#00CC00" "#00CC33" "#00CC66" "#00CC99" "#00CCCC" "#00CCFF"
		"#00FF00" "#00FF33" "#00FF66" "#00FF99" "#00FFCC" "#00FFFF"
		"#330000" "#330033" "#330066" "#330099" "#3300CC" "#3300FF"
		"#333300" "#333333" "#333366" "#333399" "#3333CC" "#3333FF"
		"#336600" "#336633" "#336666" "#336699" "#3366CC" "#3366FF"
		"#339900" "#339933" "#339966" "#339999" "#3399CC" "#3399FF"
		"#33CC00" "#33CC33" "#33CC66" "#33CC99" "#33CCCC" "#33CCFF"
		"#33FF00" "#33FF33" "#33FF66" "#33FF99" "#33FFCC" "#33FFFF"
		"#660000" "#660033" "#660066" "#660099" "#6600CC" "#6600FF"
		"#663300" "#663333" "#663366" "#663399" "#6633CC" "#6633FF"
		"#666600" "#666633" "#666666" "#666699" "#6666CC" "#6666FF"
		"#669900" "#669933" "#669966" "#669999" "#6699CC" "#6699FF"
		"#66CC00" "#66CC33" "#66CC66" "#66CC99" "#66CCCC" "#66CCFF"
		"#66FF00" "#66FF33" "#66FF66" "#66FF99" "#66FFCC" "#66FFFF"
		"#990000" "#990033" "#990066" "#990099" "#9900CC" "#9900FF"
		"#993300" "#993333" "#993366" "#993399" "#9933CC" "#9933FF"
		"#996600" "#996633" "#996666" "#996699" "#9966CC" "#9966FF"
		"#999900" "#999933" "#999966" "#999999" "#9999CC" "#9999FF"
		"#99CC00" "#99CC33" "#99CC66" "#99CC99" "#99CCCC" "#99CCFF"
		"#99FF00" "#99FF33" "#99FF66" "#99FF99" "#99FFCC" "#99FFFF"
		"#CC0000" "#CC0033" "#CC0066" "#CC0099" "#CC00CC" "#CC00FF"
		"#CC3300" "#CC3333" "#CC3366" "#CC3399" "#CC33CC" "#CC33FF"
		"#CC6600" "#CC6633" "#CC6666" "#CC6699" "#CC66CC" "#CC66FF"
		"#CC9900" "#CC9933" "#CC9966" "#CC9999" "#CC99CC" "#CC99FF"
		"#CCCC00" "#CCCC33" "#CCCC66" "#CCCC99" "#CCCCCC" "#CCCCFF"
		"#CCFF00" "#CCFF33" "#CCFF66" "#CCFF99" "#CCFFCC" "#CCFFFF"
		"#FF0000" "#FF0033" "#FF0066" "#FF0099" "#FF00CC" "#FF00FF"
		"#FF3300" "#FF3333" "#FF3366" "#FF3399" "#FF33CC" "#FF33FF"
		"#FF6600" "#FF6633" "#FF6666" "#FF6699" "#FF66CC" "#FF66FF"
		"#FF9900" "#FF9933" "#FF9966" "#FF9999" "#FF99CC" "#FF99FF"
		"#FFCC00" "#FFCC33" "#FFCC66" "#FFCC99" "#FFCCCC" "#FFCCFF"
		"#FFFF00" "#FFFF33" "#FFFF66" "#FFFF99" "#FFFFCC" "#FFFFFF"))

(defvar *named-colors*
  '("#90EE90" "#8B0000" "#8B008B" "#008B8B" "#00008B" "#A9A9A9"
    "#A9A9A9" "#8B7B8B" "#CDB5CD" "#EED2EE" "#FFE1FF" "#5D478B"
    "#8968CD" "#9F79EE" "#AB82FF" "#551A8B" "#7D26CD" "#912CEE"
    "#9B30FF" "#68228B" "#9A32CD" "#B23AEE" "#BF3EFF" "#7A378B"
    "#B452CD" "#D15FEE" "#E066FF" "#8B668B" "#CD96CD" "#EEAEEE"
    "#FFBBFF" "#8B4789" "#CD69C9" "#EE7AE9" "#FF83FA" "#8B008B"
    "#CD00CD" "#EE00EE" "#FF00FF" "#8B2252" "#CD3278" "#EE3A8C"
    "#FF3E96" "#8B1C62" "#CD2990" "#EE30A7" "#FF34B3" "#8B475D"
    "#CD6889" "#EE799F" "#FF82AB" "#8B5F65" "#CD8C95" "#EEA2AD"
    "#FFAEB9" "#8B636C" "#CD919E" "#EEA9B8" "#FFB5C5" "#8B3A62"
    "#CD6090" "#EE6AA7" "#FF6EB4" "#8B0A50" "#CD1076" "#EE1289"
    "#FF1493" "#8B0000" "#CD0000" "#EE0000" "#FF0000" "#8B2500"
    "#CD3700" "#EE4000" "#FF4500" "#8B3626" "#CD4F39" "#EE5C42"
    "#FF6347" "#8B3E2F" "#CD5B45" "#EE6A50" "#FF7256" "#8B4500"
    "#CD6600" "#EE7600" "#FF7F00" "#8B5A00" "#CD8500" "#EE9A00"
    "#FFA500" "#8B5742" "#CD8162" "#EE9572" "#FFA07A" "#8B4C39"
    "#CD7054" "#EE8262" "#FF8C69" "#8B2323" "#CD3333" "#EE3B3B"
    "#FF4040" "#8B1A1A" "#CD2626" "#EE2C2C" "#FF3030" "#8B4513"
    "#CD661D" "#EE7621" "#FF7F24" "#8B5A2B" "#CD853F" "#EE9A49"
    "#FFA54F" "#8B7E66" "#CDBA96" "#EED8AE" "#FFE7BA" "#8B7355"
    "#CDAA7D" "#EEC591" "#FFD39B" "#8B4726" "#CD6839" "#EE7942"
    "#FF8247" "#8B3A3A" "#CD5555" "#EE6363" "#FF6A6A" "#8B6969"
    "#CD9B9B" "#EEB4B4" "#FFC1C1" "#8B6508" "#CD950C" "#EEAD0E"
    "#FFB90F" "#8B6914" "#CD9B1D" "#EEB422" "#FFC125" "#8B7500"
    "#CDAD00" "#EEC900" "#FFD700" "#8B8B00" "#CDCD00" "#EEEE00"
    "#FFFF00" "#8B8B7A" "#CDCDB4" "#EEEED1" "#FFFFE0" "#8B814C"
    "#CDBE70" "#EEDC82" "#FFEC8B" "#8B864E" "#CDC673" "#EEE685"
    "#FFF68F" "#6E8B3D" "#A2CD5A" "#BCEE68" "#CAFF70" "#698B22"
    "#9ACD32" "#B3EE3A" "#C0FF3E" "#458B00" "#66CD00" "#76EE00"
    "#7FFF00" "#008B00" "#00CD00" "#00EE00" "#00FF00" "#008B45"
    "#00CD66" "#00EE76" "#00FF7F" "#548B54" "#7CCD7C" "#90EE90"
    "#9AFF9A" "#2E8B57" "#43CD80" "#4EEE94" "#54FF9F" "#698B69"
    "#9BCD9B" "#B4EEB4" "#C1FFC1" "#458B74" "#66CDAA" "#76EEC6"
    "#7FFFD4" "#528B8B" "#79CDCD" "#8DEEEE" "#97FFFF" "#008B8B"
    "#00CDCD" "#00EEEE" "#00FFFF" "#00868B" "#00C5CD" "#00E5EE"
    "#00F5FF" "#53868B" "#7AC5CD" "#8EE5EE" "#98F5FF" "#668B8B"
    "#96CDCD" "#AEEEEE" "#BBFFFF" "#7A8B8B" "#B4CDCD" "#D1EEEE"
    "#E0FFFF" "#68838B" "#9AC0CD" "#B2DFEE" "#BFEFFF" "#6E7B8B"
    "#A2B5CD" "#BCD2EE" "#CAE1FF" "#6C7B8B" "#9FB6CD" "#B9D3EE"
    "#C6E2FF" "#607B8B" "#8DB6CD" "#A4D3EE" "#B0E2FF" "#4A708B"
    "#6CA6CD" "#7EC0EE" "#87CEFF" "#00688B" "#009ACD" "#00B2EE"
    "#00BFFF" "#36648B" "#4F94CD" "#5CACEE" "#63B8FF" "#104E8B"
    "#1874CD" "#1C86EE" "#1E90FF" "#00008B" "#0000CD" "#0000EE"
    "#0000FF" "#27408B" "#3A5FCD" "#436EEE" "#4876FF" "#473C8B"
    "#6959CD" "#7A67EE" "#836FFF" "#838B8B" "#C1CDCD" "#E0EEEE"
    "#F0FFFF" "#8B7D7B" "#CDB7B5" "#EED5D2" "#FFE4E1" "#8B8386"
    "#CDC1C5" "#EEE0E5" "#FFF0F5" "#838B83" "#C1CDC1" "#E0EEE0"
    "#F0FFF0" "#8B8B83" "#CDCDC1" "#EEEEE0" "#FFFFF0" "#8B8878"
    "#CDC8B1" "#EEE8CD" "#FFF8DC" "#8B8970" "#CDC9A5" "#EEE9BF"
    "#FFFACD" "#8B795E" "#CDB38B" "#EECFA1" "#FFDEAD" "#8B7765"
    "#CDAF95" "#EECBAD" "#FFDAB9" "#8B7D6B" "#CDB79E" "#EED5B7"
    "#FFE4C4" "#8B8378" "#CDC0B0" "#EEDFCC" "#FFEFDB" "#8B8682"
    "#CDC5BF" "#EEE5DE" "#FFF5EE" "#8B8989" "#CDC9C9" "#EEE9E9"
    "#FFFAFA" "#D8BFD8" "#9370DB" "#A020F0" "#8A2BE2" "#9400D3"
    "#9932CC" "#BA55D3" "#DA70D6" "#DDA0DD" "#EE82EE" "#FF00FF"
    "#D02090" "#C71585" "#B03060" "#DB7093" "#FFB6C1" "#FFC0CB"
    "#FF1493" "#FF69B4" "#FF0000" "#FF4500" "#FF6347" "#F08080"
    "#FF7F50" "#FF8C00" "#FFA500" "#FFA07A" "#FA8072" "#E9967A"
    "#A52A2A" "#B22222" "#D2691E" "#D2B48C" "#F4A460" "#F5DEB3"
    "#F5F5DC" "#DEB887" "#CD853F" "#A0522D" "#8B4513" "#CD5C5C"
    "#BC8F8F" "#B8860B" "#DAA520" "#EEDD82" "#FFD700" "#FFFF00"
    "#FFFFE0" "#FAFAD2" "#EEE8AA" "#F0E68C" "#BDB76B" "#6B8E23"
    "#228B22" "#9ACD32" "#32CD32" "#ADFF2F" "#00FA9A" "#7FFF00"
    "#00FF00" "#7CFC00" "#00FF7F" "#98FB98" "#20B2AA" "#3CB371"
    "#2E8B57" "#8FBC8F" "#556B2F" "#006400" "#7FFFD4" "#66CDAA"
    "#5F9EA0" "#E0FFFF" "#00FFFF" "#40E0D0" "#48D1CC" "#00CED1"
    "#AFEEEE" "#B0E0E6" "#ADD8E6" "#B0C4DE" "#4682B4" "#87CEFA"
    "#87CEEB" "#00BFFF" "#1E90FF" "#0000FF" "#4169E1" "#0000CD"
    "#8470FF" "#7B68EE" "#6A5ACD" "#483D8B" "#6495ED" "#000080"
    "#191970" "#D3D3D3" "#BEBEBE" "#778899" "#708090" "#696969"
    "#2F4F4F" "#000000" "#FFFFFF" "#FFE4E1" "#FFF0F5" "#E6E6FA"
    "#F0F8FF" "#F0FFFF" "#F5FFFA" "#F0FFF0" "#FFF5EE" "#FFFACD"
    "#FFFFF0" "#FFF8DC" "#FFE4B5" "#FFDEAD" "#FFDAB9" "#FFE4C4"
    "#FFEBCD" "#FFEFD5" "#FAEBD7" "#FAF0E6" "#FDF5E6" "#FFFAF0"
    "#DCDCDC" "#F5F5F5" "#F8F8FF" "#FFFAFA"))

(defvar *royal*
  '("#3C0050" "#3C0050" "#3C0050" "#3C0050" "#40084C" "#440C4C"
    "#48104C" "#4C1448" "#4C1848" "#501C48" "#542044" "#582444"
    "#582844" "#5C2C40" "#603040" "#643440" "#68383C" "#683C3C"
    "#6C403C" "#704438" "#744838" "#744C38" "#785034" "#7C5434"
    "#805834" "#845C30" "#846030" "#886430" "#8C682C" "#906C2C"
    "#90702C" "#947428" "#987828" "#9C7C28" "#A08024" "#A08424"
    "#A48824" "#A88C24" "#AC9020" "#AC9420" "#B09820" "#B49C1C"
    "#B8A01C" "#B8A41C" "#BCA818" "#C0AC18" "#C4B018" "#C8B414"
    "#C8B814" "#CCBC14" "#D0C010" "#D4C410" "#D4C810" "#D8CC0C"
    "#DCD00C" "#E0D40C" "#E4D808" "#E4DC08" "#E8E008" "#ECE404"
    "#F0E804" "#F0EC04" "#F4F000" "#F8F400" "#FCF800" "#FCFC00"
    "#FCFC04" "#FCFC08" "#FCFC0C" "#FCFC10" "#FCFC14" "#FCFC18"
    "#FCFC1C" "#FCFC20" "#FCFC24" "#FCFC28" "#FCFC2C" "#FCFC30"
    "#FCFC34" "#FCFC38" "#FCFC3C" "#FCFC40" "#FCFC44" "#FCFC48"
    "#FCFC4C" "#FCFC50" "#FCFC54" "#FCFC58" "#FCFC5C" "#FCFC60"
    "#FCFC64" "#FCFC68" "#FCFC6C" "#FCFC70" "#FCFC74" "#FCFC78"
    "#FCFC7C" "#FCFC80" "#FCFC84" "#FCFC88" "#FCFC8C" "#FCFC90"
    "#FCFC94" "#FCFC98" "#FCFC9C" "#FCFCA0" "#FCFCA4" "#FCFCA8"
    "#FCFCAC" "#FCFCB0" "#FCFCB4" "#FCFCB8" "#FCFCBC" "#FCFCC0"
    "#FCFCC4" "#FCFCC8" "#FCFCCC" "#FCFCD0" "#FCFCD4" "#FCFCD8"
    "#FCFCDC" "#FCFCE0" "#FCFCE4" "#FCFCE8" "#FCFCEC" "#FCFCF0"
    "#FCFCF4" "#FCFCF8" "#FCFCFC" "#FCFCFC" "#F8F8F8" "#F4F4F8"
    "#F4F0F4" "#F0ECF4" "#F0E8F4" "#ECE4F0" "#ECE0F0" "#E8DCF0"
    "#E8D8EC" "#E4D4EC" "#E4D0EC" "#E0CCE8" "#E0C8E8" "#DCC4E4"
    "#DCC0E4" "#D8BCE4" "#D8B8E0" "#D4B4E0" "#D4B0E0" "#D0ACDC"
    "#D0A8DC" "#CCA4DC" "#CCA0D8" "#C89CD8" "#C898D8" "#C494D4"
    "#C490D4" "#C08CD0" "#C088D0" "#BC84D0" "#BC80CC" "#B87CCC"
    "#B478CC" "#B474C8" "#B070C8" "#B06CC8" "#AC68C4" "#AC64C4"
    "#A860C0" "#A85CC0" "#A458C0" "#A454BC" "#A050BC" "#A04CBC"
    "#9C48B8" "#9C44B8" "#9840B8" "#983CB4" "#9438B4" "#9434B4"
    "#9030B0" "#902CB0" "#8C28AC" "#8C24AC" "#8820AC" "#881CA8"
    "#8418A8" "#8414A8" "#8010A4" "#800CA4" "#7C08A4" "#7C04A0"
    "#7800A0" "#7800A0" "#74009C" "#74009C" "#74009C" "#740098"
    "#700098" "#700098" "#700094" "#700094" "#6C0094" "#6C0090"
    "#6C0090" "#6C0090" "#68008C" "#68008C" "#68008C" "#680088"
    "#640088" "#640088" "#640084" "#640084" "#600084" "#600084"
    "#600080" "#600080" "#600080" "#5C007C" "#5C007C" "#5C007C"
    "#5C0078" "#580078" "#580078" "#580074" "#580074" "#540074"
    "#540070" "#540070" "#540070" "#50006C" "#50006C" "#50006C"
    "#500068" "#4C0068" "#4C0068" "#4C0068" "#4C0064" "#4C0064"
    "#480064" "#480060" "#480060" "#480060" "#44005C" "#44005C"
    "#44005C" "#440058" "#400058" "#400058" "#400054" "#400054"
    "#3C0054" "#3C0050" "#3C0050" "#000000"))

(defvar *grayViolet*
  '("#080C0C" "#101818" "#142024" "#082420" "#241C2C" "#1C2028"
    "#381C34" "#301C30" "#4C183C" "#381C34" "#5C1840" "#4C183C"
    "#601C44" "#602048" "#5C1840" "#64244C" "#601C44" "#64284C"
    "#602048" "#682C50" "#64244C" "#683054" "#64284C" "#6C3458"
    "#682C50" "#6C385C" "#683054" "#703C5C" "#6C3458" "#704060"
    "#6C385C" "#744464" "#703C5C" "#744868" "#704060" "#784C6C"
    "#744464" "#78506C" "#744868" "#7C5470" "#784C6C" "#7C5874"
    "#78506C" "#805C78" "#7C5470" "#80607C" "#7C5874" "#84647C"
    "#805C78" "#846880" "#80607C" "#886C84" "#84647C" "#887088"
    "#846880" "#8C748C" "#886C84" "#8C7890" "#887088" "#907C90"
    "#8C748C" "#908094" "#8C7890" "#948498" "#907C90" "#94889C"
    "#908094" "#988CA0" "#948498" "#9890A0" "#94889C" "#9C94A4"
    "#988CA0" "#9C98A8" "#9890A0" "#A09CAC" "#9C94A4" "#A0A0B0"
    "#9C98A8" "#A4A4B0" "#A09CAC" "#A4A8B4" "#A0A0B0" "#A8ACB8"
    "#A4A4B0" "#A8B0BC" "#A4A8B4" "#ACB4C0" "#A8ACB8" "#ACB8C0"
    "#A8B0BC" "#B0BCC4" "#ACB4C0" "#B0C0C8" "#ACB8C0" "#B4C4CC"
    "#B0BCC4" "#B4C8D0" "#B0C0C8" "#B4C8D0" "#B4C4CC" "#B8CCD4"
    "#B4C8D0" "#BCD0D8" "#B4C8D0" "#C0D0D8" "#B8CCD4" "#C4D4DC"
    "#BCD0D8" "#C4D4DC" "#B4CCD8" "#C8D4E0" "#BCD0D8" "#C8D8E0"
    "#C0D0DC" "#CCD8E4" "#C4D4DC" "#D0DCE8" "#C8D4E0" "#D4DCE8"
    "#C8D8E0" "#D8E0EC" "#CCD8E4" "#DCE4EC" "#D0DCE8" "#DCE4F0"
    "#D4DCE8" "#E0E8F4" "#D8E0EC" "#E4E8F4" "#DCE4EC" "#E8ECF8"
    "#DCE4F0" "#ECECF8" "#E0E8F4" "#F0F0FC" "#E4E8F4" "#F0F0FC"
    "#D4DCE8" "#D4DCE8" "#D0D8E4" "#D0D8E4" "#CCD4E0" "#CCD4E0"
    "#C8D0DC" "#C8D0DC" "#C4CCD8" "#C4CCD8" "#C0C8D4" "#C0C8D4"
    "#C0C4D0" "#BCC4D0" "#BCC0CC" "#B8C0CC" "#B8BCC8" "#B4BCC8"
    "#B4BCC4" "#B0B8C4" "#B0B8C0" "#ACB4C0" "#ACB4BC" "#ACB0BC"
    "#A8B0B8" "#A8ACB8" "#A4ACB4" "#A4A8B4" "#A0A8B0" "#A0A4B0"
    "#9CA4AC" "#9CA0AC" "#9CA0A8" "#989CA8" "#989CA4" "#949CA4"
    "#9498A0" "#9098A0" "#90949C" "#8C949C" "#8C9098" "#889098"
    "#888C94" "#888C94" "#848890" "#848890" "#80848C" "#80848C"
    "#7C8088" "#7C8088" "#787C84" "#787C84" "#747C80" "#747880"
    "#74787C" "#70747C" "#707478" "#6C7078" "#6C7074" "#686C74"
    "#686C70" "#646870" "#64686C" "#60646C" "#606468" "#606068"
    "#5C6064" "#5C6064" "#585C60" "#585C60" "#54585C" "#54585C"
    "#505458" "#505458" "#4C5054" "#4C5054" "#4C4C50" "#484C50"
    "#48484C" "#44484C" "#444448" "#404448" "#404044" "#3C4044"
    "#3C4040" "#383C40" "#383C3C" "#38383C" "#343838" "#343438"
    "#303434" "#303034" "#2C3030" "#2C2C30" "#282C2C" "#28282C"
    "#282828" "#242428" "#242424" "#202024" "#202020" "#1C2020"
    "#1C1C1C" "#181C1C" "#181818" "#141818" "#141414" "#141414"
    "#101010" "#101010" "#0C0C0C" "#0C0C0C" "#080808" "#080808"
    "#040404" "#040404" "#000000" "#000000"))

(defvar *cranes*
  '("#948C7C" "#88B074" "#B49474" "#C47474" "#2C1808" "#402C38"
    "#587454" "#747060" "#C45054" "#CCD0C8" "#8C8474" "#848C8C"
    "#686854" "#7C7460" "#C4A070" "#241C28" "#809470" "#38302C"
    "#648C58" "#5C8050" "#CCCCBC" "#2C2828" "#808064" "#CCBC98"
    "#A4B0B8" "#60785C" "#647450" "#600814" "#546054" "#94A4AC"
    "#D4D4D0" "#C4C8C4" "#B8BCBC" "#98ACB8" "#B8B8B0" "#9CA8B0"
    "#909CA0" "#889898" "#747C78" "#000000" "#887858" "#848074"
    "#687464" "#D0B884" "#70985C" "#948C74" "#88846C" "#688054"
    "#B84850" "#80786C" "#506C50" "#4C604C" "#A8243C" "#803034"
    "#484C44" "#382838" "#000000" "#587448" "#58644C" "#485C44"
    "#48503C" "#C01C24" "#381428" "#3C3838" "#000000" "#000004"
    "#00385C" "#280000" "#000000" "#B83414" "#94B46C" "#948864"
    "#2C0808" "#54444C" "#2C0C38" "#3C1440" "#443044" "#382C38"
    "#480C34" "#584C64" "#4C6038" "#808084" "#E0E4E0" "#8C8060"
    "#445448" "#D06C64" "#C0C4AC" "#ACC098" "#606848" "#A45854"
    "#602038" "#84386C" "#285438" "#808C78" "#94846C" "#A0A49C"
    "#8C6C58" "#ACB4AC" "#686054" "#D89C90" "#6C5C5C" "#3C3C40"
    "#88AC84" "#6C6C74" "#344820" "#94BC7C" "#88AC64" "#D8DCD0"
    "#CCC4A8" "#A4BC84" "#40542C" "#C46058" "#381C28" "#843424"
    "#646068" "#7C0C08" "#58104C" "#888C80" "#505054" "#948064"
    "#746C54" "#A49474" "#5C5450" "#C86860" "#3C4C40" "#884044"
    "#201450" "#282C1C" "#4C4438" "#D8D0B0" "#D0D4C4" "#80A870"
    "#240828" "#104408" "#585860" "#D47C80" "#98484C" "#D47C70"
    "#703440" "#382818" "#4C3C54" "#2C2808" "#506844" "#082430"
    "#D06458" "#8C9880" "#140C30" "#48484C" "#988C6C" "#503840"
    "#D48C80" "#CCB08C" "#3C4038" "#78706C" "#789464" "#545C48"
    "#909894" "#746860" "#A49C84" "#CCAC78" "#181C14" "#8C7C68"
    "#2C2838" "#6C5044" "#6C0C30" "#78806C" "#949C8C" "#B4A884"
    "#A88464" "#442434" "#847464" "#DCB4A4" "#C4B080" "#BC3848"
    "#503034" "#3C3044" "#BCB4A0" "#6C9064" "#643060" "#443838"
    "#C0C890" "#94A098" "#606054" "#9C7858" "#70644C" "#B4A078"
    "#5C5440" "#303834" "#182428" "#381818" "#505448" "#C0C0B8"
    "#749C68" "#98A0A4" "#707458" "#8CA070" "#D8CCA0" "#443C44"
    "#143034" "#940834" "#BC0858" "#B0A890" "#ACB09C" "#9C2C44"
    "#B0B4B4" "#20081C" "#7C7458" "#645C48" "#9CACA4" "#382828"
    "#40382C" "#A8444C" "#801438" "#805450" "#2C1418" "#78A074"
    "#889488" "#505440" "#9CB090" "#300818" "#84283C" "#C09070"
    "#B49064" "#A03C3C" "#081C14" "#C4BCA8" "#A8A8AC" "#2C2818"
    "#7CA060" "#9CB07C" "#341C30" "#68685C" "#2C1428" "#300828"
    "#380830" "#0C0820" "#440C10" "#2C1C28" "#D8DCD8" "#A09480"
    "#90886C" "#D4C494" "#44443C" "#9C9474" "#9C8C74" "#2C1C18"
    "#80684C" "#606C5C" "#18080C" "#5C6854" "#688460" "#708C58"
    "#504844" "#C0A480" "#C0B090" "#080808"))

(defvar *bears*
  '("#000000" "#000004" "#00385C" "#280000" "#000000" "#CC0814"
    "#08202C" "#88504C" "#A49474" "#CCC4B0" "#B47454" "#780C38"
    "#840C0C" "#AC6050" "#C48858" "#2C4850" "#54281C" "#845410"
    "#380C38" "#8C5444" "#1C2410" "#5C4C20" "#545048" "#883440"
    "#CC9864" "#5C5854" "#4C1C18" "#9C3C2C" "#BC7C44" "#CCD8CC"
    "#846410" "#5C1828" "#CCD4BC" "#605C64" "#B8A880" "#C4C0AC"
    "#A8603C" "#702838" "#74300C" "#242828" "#804848" "#604008"
    "#0C3C48" "#ACA090" "#949094" "#743840" "#382438" "#70242C"
    "#90603C" "#D4CCC0" "#AC7024" "#944044" "#683C20" "#484448"
    "#38383C" "#B4C8BC" "#C0B490" "#90584C" "#701428" "#9CA08C"
    "#BCB8A0" "#945C2C" "#504438" "#78480C" "#945038" "#2C2810"
    "#2C2438" "#88888C" "#D0C0A0" "#441C38" "#804040" "#68383C"
    "#DCDCD0" "#94280C" "#4C341C" "#980808" "#901030" "#98805C"
    "#9C5408" "#502438" "#A4A0A4" "#944818" "#AC9C80" "#6C3834"
    "#A05448" "#BC8060" "#A4B0C0" "#C0C0C0" "#602C24" "#240C30"
    "#382C38" "#2C281C" "#684830" "#B8B4B4" "#5C0C30" "#7C4C40"
    "#A06848" "#845840" "#B8683C" "#2C0828" "#584850" "#8C1C4C"
    "#7C243C" "#2C1C28" "#AC5028" "#BC8C60" "#B0AC9C" "#E4E4DC"
    "#784C44" "#680C30" "#88442C" "#080830" "#8C7454" "#A06454"
    "#9C3808" "#F8FCEC" "#E8E0B0" "#98A0AC" "#C0A884" "#48382C"
    "#180828" "#704C3C" "#642838" "#7C3C14" "#A87448" "#603C30"
    "#E8F0EC" "#2C2828" "#744438" "#884C40" "#2C1818" "#5C3838"
    "#B05C0C" "#D8CCA8" "#AC805C" "#F0F4D4" "#483840" "#78482C"
    "#7C2408" "#4C2C38" "#7C7884" "#9C705C" "#7C5844" "#8C8C9C"
    "#A47454" "#ACACAC" "#503C44" "#580C3C" "#502808" "#986048"
    "#CC8810" "#B48868" "#7C382C" "#E4E4CC" "#5C4438" "#DCD0B8"
    "#582C38" "#740824" "#786040" "#986C50" "#44242C" "#B01C08"
    "#64200C" "#906848" "#805438" "#380824" "#704444" "#280818"
    "#5C282C" "#1C1828" "#70080C" "#442C1C" "#2C0808" "#B84008"
    "#684438" "#8C8494" "#845C50" "#382818" "#684844" "#2C1428"
    "#7C2420" "#480C24" "#706874" "#502C30" "#886448" "#500C30"
    "#8C282C" "#706C60" "#948870" "#949480" "#ECE8C0" "#381828"
    "#180C1C" "#C07814" "#80544C" "#180808" "#440C2C" "#5C3844"
    "#580820" "#DCC898" "#D8AC74" "#B89870" "#D0B888" "#9898A0"
    "#3C3434" "#A0A8B8" "#E0D8B0" "#3C080C" "#8C3810" "#946050"
    "#381818" "#60504C" "#A07C64" "#C4906C" "#E4D094" "#543838"
    "#382828" "#806854" "#E0DCC0" "#84847C" "#907460" "#747080"
    "#600808" "#E4D4A4" "#94A4B8" "#CCB898" "#909CB0" "#8898AC"
    "#E0C488" "#9098A8" "#CCAC7C" "#8490A4" "#808498" "#807C8C"
    "#78748C" "#68646C" "#DCB87C" "#CCA070" "#88806C" "#787474"
    "#A48868" "#807460" "#686054" "#08081C" "#70544C" "#584844"
    "#442C38" "#AC745C" "#8C6858" "#54382C" "#746050" "#685444"
    "#483838" "#50080C" "#442C2C" "#080808"))

(provide 'e-palette)

;;; e-palette.el ends here.
