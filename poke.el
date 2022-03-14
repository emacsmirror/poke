;;; poke.el --- Emacs interface to GNU poke

;; Copyright (C) 2022  Jose E. Marchesi
;; Author: Jose E. Marchesi <jemarch@gnu.org>
;; Maintainer: Jose E. Marchesi <jemarch@gnu.org>
;; URL: https://www.jemarch.net/poke
;; Package-Requires: ((emacs "25"))
;; Version: 1.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file implements a Emacs interface to GNU poke, the extensible
;; editor for structured binary data.
;;
;; It uses the poked (GNU poke daemon) in order to act as a set of
;; pokelets:
;;
;; poke-out
;;      connected to the poked output channel 1.
;; poke-cmd
;;      connected to the poked input channel 2.
;; poke-code
;;      connected to the poked input channel 3.
;; poke-vu
;;      connected to the poked output channel 2.
;; poke-elval
;;      connected to the poked output channel 100.

;;; Code:

(require 'comint)
(require 'subr-x)
(require 'tabulated-list)

;;;; First, some utilities

(defun poke-decode-u64-le (seq)
  (logior (ash (aref seq 7) 56)
          (ash (aref seq 6) 48)
          (ash (aref seq 5) 40)
          (ash (aref seq 4) 32)
          (ash (aref seq 3) 24)
          (ash (aref seq 2) 16)
          (ash (aref seq 1) 8)
          (aref seq 0)))

;;;; Faces

(defface poke-integer-face '((t :foreground "green"))
  "Face for printing Poke integer values.")
(defface poke-string-face '((t :inherit font-lock-string-face))
  "Face for printing Poke string values.")
(defface poke-offset-face '((t :foreground "yellow"))
  "Face for printing Poke offsets.")
(defface poke-struct-field-name-face '((t :underline t))
  "Face for printing Poke struct field names.")
(defface poke-vu-addr-face '((t :bold t))
  "Face for printing line addresses in VU mode.")
(defface poke-vu-ascii-face '((t :foreground "red"))
  "Face for printing ascii in VU mode.")
(defface poke-diff-field-name-face '((t :underline t))
  "Face for printing thunk field names.")
(defface poke-diff-thunk-header-face '((t :bold t))
  "Face for thunk headers.")
(defface poke-diff-minus-face '((t :foreground "red"))
  "Face for deletion thunk lines.")
(defface poke-diff-plus-face '((t :foreground "green"))
  "Face for addition thunk lines.")
(defface poke-iter-string-face '((t :bold t))
  "Face for iteration separator in *poke-out* buffer.")
(defface poke-error-face '((t :bold t :foreground "red"))
  "Face for error messages.")
(defface poke-warning-face '((t :foreground "yellow"))
  "Face for warning messages.")

;;;; Poke styling classes

;; GNU poke uses named styling classes in order to style the output we
;; get through the pokelets.  The following variable associates Poke
;; styling class names with Emacs faces.

(defvar poke-styling-faces
  '(("integer" poke-integer-face)
    ("string" poke-string-face)
    ("offset" poke-offset-face)
    ("struct-field-name" poke-struct-field-name-face)
    ("diff-thunk-header"  poke-diff-thunk-header-face)
    ("diff-minus" poke-diff-minus-face)
    ("diff-plus" poke-diff-plus-face)
    ("error" poke-error-face)
    ("warning" poke-warning-face)))

;;;; poked

(defvar poke-poked-program "poked")
(defvar poke-poked-process nil)
(defvar poked-socket "/tmp/poked.ipc")

(defun poke-poked ()
  "Start a poke daemon process"
  (interactive)
  (when (not (process-live-p poke-poked-process))
    (setq poke-poked-process
          (make-process :name "poked"
                        :buffer "*poked*"
                        :command (list poke-poked-program)))
    (set-process-query-on-exit-flag poke-poked-process nil)))

;;;; pokelet protocol

;; The filter function below implements the poke daemon message
;; protocol.  The pokelet processes are required to have the following
;; attributes in their alist:
;;
;;  pokelet-state
;;    One of the POKE_STATE_* values below.  Initially must
;;    be POKE_STATE_LENGTH.
;;
;;  pokelet-buf
;;    This is a string that accumulates the input received
;;    by the pokelet.  Initially "".
;;
;;  pokelet-msg-length
;;    Lenght of the message being processed.  Initially 0.
;;
;;  pokelet-msg-handler
;;    Function that gets the process, a command number
;;    and a command argument.  This function can error
;;    if there is a protocol error.

(defconst PLET_STATE_LENGTH 0) ; Collecting length bytes.
(defconst PLET_STATE_MSG 1) ; Collecting message data.

(defun poke-pokelet-filter (proc string)
  (process-put proc 'pokelet-buf
               (concat (process-get proc 'pokelet-buf) string))
  (while (or (and (= (process-get proc 'pokelet-state) PLET_STATE_LENGTH)
                  (>= (length (process-get proc 'pokelet-buf)) 2))
             (and (= (process-get proc 'pokelet-state) PLET_STATE_MSG)
                  (>= (length (process-get proc 'pokelet-buf))
                      (process-get proc 'pokelet-msg-length))))
    (if (= (process-get proc 'pokelet-state) PLET_STATE_LENGTH)
        (let ((pokelet-buf (process-get proc 'pokelet-buf)))
          ;; The message lenght is encoded as an unsigned
          ;; little-endian 16 bit number.  Collect and skipt it.
          (process-put proc
                       'pokelet-msg-length
                       (logior (ash (aref pokelet-buf 1) 8)
                               (aref pokelet-buf 0)))
          (process-put proc 'pokelet-buf (substring pokelet-buf 2))
          ;; We are now waiting for the message data.
          (process-put proc 'pokelet-state PLET_STATE_MSG))
      ;; We are collecting message data.
      (when (>= (length (process-get proc 'pokelet-buf))
                (process-get proc 'pokelet-msg-length))
	;; Action on the message according to the command.
        (let ((cmd (aref (process-get proc 'pokelet-buf) 0))
              (msg-data (substring (process-get proc 'pokelet-buf)
                                   1
                                   (- (process-get proc 'pokelet-msg-length) 1))))
          (apply (process-get proc 'pokelet-msg-handler) (list proc cmd msg-data)))
	;; Discard used portion of the buffer and go back to waiting
        ;; for a message length.
        (process-put proc
                     'pokelet-buf
                     (substring (process-get proc 'pokelet-buf)
                                (process-get proc 'pokelet-msg-length)))
        (process-put proc 'pokelet-state PLET_STATE_LENGTH)))))

(defun poke-make-pokelet-process-new (name ctrl msg-handler)
   (let ((proc (make-network-process :name name
                                     :buffer (concat "*" name "*")
                                     :family 'local
                                     :service poked-socket)))
     (process-put proc 'pokelet-state PLET_STATE_LENGTH)
     (process-put proc 'pokelet-buf "")
     (process-put proc 'pokelet-msg-length 0)
     (process-put proc 'pokelet-msg-handler msg-handler)
     (set-process-query-on-exit-flag proc nil)
     (set-process-filter proc #'poke-pokelet-filter)
     (process-send-string proc ctrl)
     proc))

(defun poke-make-pokelet-process (name ctrl)
   (let ((proc (make-network-process :name name
                                     :buffer (concat "*" name "*")
                                     :family 'local
                                     :service poked-socket)))
    (process-send-string proc ctrl)
    proc))

;;;; poke-out pokelet

(defconst poke-out-iter-string
  (propertize (char-to-string 8594) 'font-lock-face 'poke-iter-string-face))

(defvar poke-out-process nil)

(defun poke-out-stylize (styles string)
  (let ((propertized-string string))
    (mapcar (lambda (style)
              (let* ((face-ass (assoc style poke-styling-faces))
                     (face (when face-ass (cadr face-ass))))
                (setq propertized-string
                      (if face
                          (propertize propertized-string 'font-lock-face face)
                        propertized-string))))
            (reverse styles))
    propertized-string))

(defun poke-out-handle-cmd (proc cmd data)
  (pcase cmd
    (1 ;; Iteration begin
     (process-put proc 'poke-out-eval "")
     (when (buffer-live-p (process-buffer proc))
       (with-current-buffer (process-buffer proc)
         (let ((buffer-read-only nil))
           (goto-char (point-max))
           (process-put proc 'poke-out-iter-begin (point))))))
    (3 ;; Iteration end
     (when (buffer-live-p (process-buffer proc))
       (with-current-buffer (process-buffer proc)
         (save-excursion
           (unless (equal (process-get proc 'poke-out-iter-begin)
                          (point-max))
             (narrow-to-region (process-get proc 'poke-out-iter-begin)
                               (point-max)))
           (let ((buffer-read-only nil))
             (mapcar (lambda (window)
                       (set-window-point window (point-max)))
                     (get-buffer-window-list))))))
     (process-put proc 'poke-out-emitted-iter-string nil)
     (when (process-live-p poke-repl-process)
       (poke-repl-end-of-iteration (process-get proc 'poke-out-eval))))
    (2 ;; Process terminal poke output
     (let ((output (poke-out-stylize
                    (process-get proc 'poke-out-styles) data)))
       (when (buffer-live-p (process-buffer proc))
         (with-current-buffer (process-buffer proc)
           (save-excursion
             (let ((buffer-read-only nil))
               (goto-char (point-max))
               (unless (process-get proc 'poke-out-emitted-iter-string)
                 (insert (concat poke-out-iter-string "\n"))
                 (process-put proc 'poke-out-emitted-iter-string t))
               (insert output)))))))
    (6 ;; Process eval poke output
     (let ((output (poke-out-stylize
                    (process-get proc 'poke-out-styles) data)))
       ;; Append the output to the global variable which will be
       ;; handled at the end of the iteration.
       (process-put proc 'poke-out-eval
                    (concat (process-get proc 'poke-out-eval) output))
       ;; If there is no repl, output this in the *poke-out*
       ;; buffer prefixed with >
       (when (not (process-live-p poke-repl-process))
         (when (buffer-live-p (process-buffer proc))
           (with-current-buffer (process-buffer proc)
             (let ((buffer-read-only nil))
               (goto-char (point-max))
               (insert (concat ">" output))))))))
    (7 ;; Error output
     (let ((output (poke-out-stylize
                    (process-get proc 'poke-out-styles) data)))
       ;; Append to the eval output for now.
       (process-put proc 'poke-out-eval
                    (concat (process-get proc 'poke-out-eval) output))
       ;; If there is no repl, output this in the *poke-out*
       ;; buffer prefixed with error>
       (when (not (process-live-p poke-repl-process))
         (when (buffer-live-p (process-buffer proc))
           (with-current-buffer (process-buffer proc)
             (let ((buffer-read-only nil))
               (goto-char (point-max))
               (insert (concat "error>" output))))))))
    (4 ;; Styling class begin
     (let ((style data))
       (process-put proc
                    'poke-out-styles
                    (cons style (process-get proc 'poke-out-styles)))))
    (5 ;; Styling class end
     (let ((style data)
           (styles (process-get proc 'poke-out-styles)))
       (if (or (not styles)
               (not (equal (car styles) style)))
           (error "closing a mismatched style")
         (process-put proc
                      'poke-out-styles (cdr styles)))))
    (_ ;; Protocol error
     (process-put proc 'pokelet-buf "")
     (process-put proc 'pokelet-msg-lenght 0)
     (error "pokelet protocol error"))))

(defvar poke-out-font-lock nil
  "Font lock entries for `poke-vu-mode'.")

(defun poke-out-mode ()
  "A major mode for Poke out buffers.

Commands:
\\{poke-out-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq poke-out-mode-map (make-keymap))
  (use-local-map  poke-out-mode-map)
  (setq-local font-lock-defaults '(poke-out-font-lock))
  (setq mode-name "poke-out")
  (setq major-mode 'poke-out-mode)
  (read-only-mode t))

(defun poke-out ()
  (interactive)
  (when (not (process-live-p poke-out-process))
    (setq poke-out-process
          (poke-make-pokelet-process-new "poke-out" "\x81"
                                         #'poke-out-handle-cmd))
    (process-put poke-out-process 'poke-out-styles nil)
    (process-put poke-out-process 'poke-out-iter-begin 1)
    (process-put poke-out-process 'poke-out-eval nil)
    (process-put poke-out-process 'poke-out-emitted-iter-string nil)
    (save-excursion
      (set-buffer "*poke-out*")
      (poke-out-mode)))
  (when (called-interactively-p)
    (switch-to-buffer-other-window "*poke-out*")))

;;;; poke-cmd pokelet

(defvar poke-cmd-process nil)

(defun poke-cmd-send (string)
  ;; Send the lenght of string in a 16-bit little-endian unsigned
  ;; integer, followed by string, to poke-cmd-process.
  (if (process-live-p poke-cmd-process)
      (progn
        (let* ((string-length (length string)))
          (process-send-string poke-cmd-process
                               (unibyte-string (logand string-length #xff)
                                               (logand (ash string-length -8) #xff)))
          (process-send-string poke-cmd-process string)))
    (error "poke-cmd is not running")))

(defun poke-cmd ()
  (interactive)
  (when (not (process-live-p poke-cmd-process))
    (setq poke-cmd-process (poke-make-pokelet-process
                            "poke-cmd" "\x02"))
    (set-process-query-on-exit-flag poke-cmd-process nil))
  (when (called-interactively-p)
    (switch-to-buffer-other-window "*poke-cmd*")))

;;;; poke-code pokelet

(defvar poke-code-process nil)

(defun poke-code-send (string)
  ;; Send the lenght of string in a 16-bit little-endian unsigned
  ;; integer, followed by string, to poke-code-process.
  (if (process-live-p poke-code-process)
      (progn
        (let* ((string-length (length string)))
          (process-send-string poke-code-process
                               (unibyte-string (logand string-length #xff)
                                               (logand (ash string-length -8) #xff)))
          (process-send-string poke-code-process string)))
    (error "poke-code is not running")))

(defun poke-code ()
  (interactive)
  (when (not (process-live-p poke-code-process))
    (setq poke-code-process (poke-make-pokelet-process
                            "poke-code" "\x01"))
    (set-process-query-on-exit-flag poke-code-process nil))
  (when (called-interactively-p)
    (switch-to-buffer-other-window "*poke-code*")))

;;;; poke-vu pokelet

(defvar poke-vu-process nil)

(defun poke-vu-handle-cmd (proc cmd data)
  (pcase cmd
    (1 ;; RESET
     (when (buffer-live-p (process-buffer proc))
       (with-current-buffer (process-buffer proc)
         (let ((buffer-read-only nil))
           (delete-region (point-min) (point-max))))))
    (2 ;; APPEND
     (process-put proc 'poke-vu-output
                  (concat (process-get proc 'poke-vu-output) data)))
    (3 ;; HIGHLIGHT
     ;; XXX
     )
    (4 ;; FILTER
     ;; XXX
     )
    (5 ;; FINISH
     (when (buffer-live-p (process-buffer proc))
       (with-current-buffer (process-buffer proc)
         (let ((buffer-read-only nil))
           (delete-region (point-min) (point-max))
           (insert (process-get proc 'poke-vu-output))
           (goto-char (point-min)))))
     (process-put proc 'poke-vu-output ""))
    (_ ;; Protocol error
     (process-put proc 'pokelet-buf "")
     (process-put proc 'pokelet-msg-lenght 0)
     (error "pokelet protocol error"))))

(defvar poke-vu-font-lock
  `(("^[0-9a-zA-Z]+:" . 'poke-vu-addr-face)
    ("  .*$" . 'poke-vu-ascii-face)
    )
  "Font lock entries for `poke-vu-mode'.")

(defun poke-vu-prev-line ()
  (interactive)
  (if (equal (line-number-at-pos) 1)
      (progn
        (setq-local start-byte-offset (- start-byte-offset #x10))
        (poke-vu-refresh))
    (previous-line)))

(defun poke-vu-next-line ()
  (interactive)
  (if (save-excursion
        (end-of-line)
        (forward-char)
        (eobp))
      (progn
        (setq-local start-byte-offset (+ start-byte-offset #x10))
        (poke-vu-refresh)
        (end-of-buffer)
        (previous-line))
    (next-line)))

(defun poke-vu-page-down ()
  (interactive)
  (save-excursion
    (let ((window (get-buffer-window (current-buffer))))
      (setq-local start-byte-offset
                  (+ start-byte-offset (* (- (window-height) 1) #x10)))
      (poke-vu-refresh))))

(defun poke-vu-page-up ()
  (interactive)
  (save-excursion
    (let ((window (get-buffer-window (current-buffer))))
      (setq-local start-byte-offset
                  (- start-byte-offset (* (- (window-height) 1) #x10)))
      (poke-vu-refresh))))

(defun poke-vu-goto-byte (offset)
  (interactive "nGoto byte: ")
  (save-excursion
    (set-buffer "*poke-vu*")
    (setq-local start-byte-offset offset)
    (poke-vu-refresh)))

(defun poke-vu-mode ()
  "A major mode for Poke vu output.

Commands:
\\{poke-vu-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq poke-vu-mode-map (make-keymap))
  (use-local-map  poke-vu-mode-map)
  (define-key poke-vu-mode-map "\C-n" 'poke-vu-next-line)
  (define-key poke-vu-mode-map "\C-p" 'poke-vu-prev-line)
  (define-key poke-vu-mode-map "\C-v" 'poke-vu-page-down)
  (define-key poke-vu-mode-map "\M-v" 'poke-vu-page-up)
  (define-key poke-vu-mode-map "\C-cg" 'poke-vu-goto-byte)
  (setq-local font-lock-defaults '(poke-vu-font-lock))
  (setq-local start-byte-offset 0)
  (setq-local header-line-format
              "76543210  0011 2233 4455 6677 8899 aabb ccdd eeff  0123456789ABCDEF")
  (setq mode-name "poke-vu")
  (setq major-mode 'poke-vu-mode)
  (read-only-mode t))

(defun poke-vu ()
  (interactive)
  (when (not (process-live-p poke-vu-process))
    (setq poke-vu-process
          (poke-make-pokelet-process-new "poke-vu" "\x82"
                                         #'poke-vu-handle-cmd))
    (process-put poke-vu-process 'poke-vu-output "")
    (save-excursion
     (set-buffer "*poke-vu*")
     (poke-vu-mode)))
  (when (called-interactively-p)
    (switch-to-buffer-other-window "*poke-vu*")))

(defun poke-vu-erase ()
  (let ((buffer (get-buffer "*poke-vu*")))
    (when (and (process-live-p poke-vu-process)
               buffer)
      (let ((buffer-read-only nil))
        (delete-region (point-min) (point-max))))))

(defun poke-vu-refresh ()
  (let* ((buffer (get-buffer "*poke-vu*"))
         (window (get-buffer-window buffer)))
    (when (and (process-live-p poke-vu-process)
               window)
      ;; Note we are assuming each VU line contains 0x10 bytes.
      (poke-code-send (concat "{vu "
                              ":from " (number-to-string
                                        (buffer-local-value 'start-byte-offset buffer))
                              "#B "
                              ":size " (number-to-string (* (- (window-height window) 1)
                                                            #x10)) "#B"
                              ";} ?! E_no_ios;")))))

(add-hook 'window-size-change-functions
          (lambda (window)
            (let (buffer (window-buffer window))
              (when (equal (buffer-name buffer) "*poke-vu*")
                (poke-vu-refresh)))))

;;;; poke-elval

(defconst poke-elval-init-pk
  "\
var PLET_ELVAL_CMD_EVAL = 0UB;

fun plet_elval = (string s) void:
{
  var c = byte[s'length] ();

  stoca (s, c);
  chan_send (100,  [PLET_ELVAL_CMD_EVAL] + c);
  //print \"elval: sent '\" + s + \"'\n\";
}
")

(defvar poke-elval-process nil)

(defun poke-elval-handle-cmd (proc cmd data)
  (pcase cmd
    (0 ;; EVAL
     (ignore-errors
       (eval (car (read-from-string data)))))
    (_ ;; Protocol error
     (process-put proc 'pokelet-buf "")
     (process-put proc 'pokelet-msg-lenght 0)
     (error "pokelet protocol error"))))

(defun poke-elval ()
  (interactive)
  (when (not (process-live-p poke-elval-process))
    (poke-code)
    (poke-code-send poke-elval-init-pk)
    (setq poke-elval-process
          (poke-make-pokelet-process-new "poke-elval" "\xe4"
                                         #'poke-elval-handle-cmd))))

;;;; poke-repl

(defconst poke-repl-default-prompt "#!poke!# ")
(defvar poke-repl-prompt poke-repl-default-prompt)
(defvar poke-repl-process nil)
(defvar poke-repl-seq 0)

(define-derived-mode poke-repl-mode comint-mode "poke"
  "poke-repl mode."
  (setq comint-prompt-regexp (concat "^" (regexp-quote poke-repl-prompt)))
  (setq comint-input-sender 'poke-repl-input-sender)
  (setq poke-repl-process
        (condition-case nil
            (start-process "poke-repl-process" (current-buffer) "hexl")
          (file-error (start-process "poke-repl-process" (current-buffer) "cat"))))
  (set-process-query-on-exit-flag poke-repl-process nil)
  (set-marker
   (process-mark poke-repl-process) (point))
  (comint-output-filter poke-repl-process poke-repl-prompt))

(defun poke-repl-end-of-iteration (valstring)
  (with-current-buffer "*poke-repl*"
    (let ((buffer-read-only nil))
      (save-excursion
        (re-search-backward
         (regexp-quote (concat "#" (number-to-string poke-repl-seq)))
         nil t)
        (delete-region (point) (line-end-position))
        (if (> (length valstring) 0)
            (insert valstring)
          (unless (equal (point) (point-max))
            (delete-char 1))))
      (setq poke-repl-seq (1+ poke-repl-seq)))))

(defun poke-repl-set-prompt (string)
  (setq poke-repl-prompt string)
  (when (process-live-p poke-repl-process)
    (comint-output-filter poke-repl-process "\n")
    (comint-output-filter poke-repl-process poke-repl-prompt)))

(defun poke-repl-input-sender (proc input)
  (unless (string-blank-p input)
    (let ((id (number-to-string poke-repl-seq))
          (buffer-read-only nil)
          (lb (- (line-beginning-position) 5)))
      (comint-output-filter poke-repl-process (format "#%s\n" id))
      (if (string-match "^[ \t]*\\(var\\|type\\|unit\\|fun\\) " input)
          (poke-code-send (concat input ";"))
        (poke-cmd-send (concat input ";")))))
  (poke-vu-refresh)
  (comint-output-filter poke-repl-process poke-repl-prompt))

(defun poke-repl ()
  (interactive)
  (poke-out)
  (poke-cmd)
  (poke-code)
  (when (not (process-live-p poke-repl-process))
    (setq poke-repl-seq 0)
    (let ((buf (get-buffer-create "*poke-repl*")))
      (with-current-buffer  buf
        (poke-repl-mode))))
  (poke-code-send "poke_el_banner;")
  (when (called-interactively-p)
    (switch-to-buffer-other-window "*poke-repl*")))

;;;; poke-ios

(defvar poke-ios-alist nil
  "List of IO spaces currently open.")

(defun poke-ios-set-ios ()
  "Set the current IOS in poke to the entry selected in the
*poke-ios* buffer."
  (interactive)
  (let ((ios-id (tabulated-list-get-id)))
    (poke-code-send "set_ios (" + (number-to-string ios-id) ")")))

(defun poke-ios-open (ios iohandler ioflags iosize)
  (unless (assoc ios poke-ios-alist)
    (setq poke-ios-alist (cons (list ios iohandler ioflags iosize)
                               poke-ios-alist)))
  (poke-ios-populate))

(defun poke-ios-close (ios)
  (setq poke-ios-alist (assq-delete-all ios poke-ios-alist))
  (poke-ios-populate)
  ;; If there is no more open IO spaces, set the *poke-repl* prompt
  ;; to the default value, also delete the *poke-vu* buffer if it
  ;; exists.
  (when (equal (length poke-ios-alist) 0)
    (poke-repl-set-prompt poke-repl-default-prompt)
    (poke-vu-erase)))

(defun poke-ios-set (ios)
  ;; Select the right line in *poke-ios*.
  ;; Change prompt in *poke-repl*.
  (let ((ios-data (assoc ios poke-ios-alist)))
    (when ios-data
      (poke-repl-set-prompt (concat "#!" (cadr ios-data) "!# ")))))

(defvar poke-ios-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'poke-ios-set-ios)
    map)
  "Local keymap for `poke-ios-mode' buffers.")

(define-derived-mode poke-ios-mode tabulated-list-mode "Poke IOS List"
  "Major mode for summarizing the open IO spaces in poke.
\\<poke-ios-mode-map>
\\{poke-ios-mode-map}"
  (setq tabulated-list-format nil)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun poke-ios-populate ()
  "Populate a `poke-ios-mode' buffer with the data in `poke-ios-alist."
  (when (get-buffer "*poke-ios*")
    (save-excursion
      (set-buffer "*poke-ios*")
      (let ((headers [("Id" 5 t) ("Handler" 10 nil) ("Flags" 8 nil)
                      ("Size" 6 t)])
            (entries (mapcar
                      (lambda (ios)
                        (let ((ios-id (car ios))
                              (ios-handler (cadr ios))
                              (ios-flags (caddr ios))
                              (ios-size (cadddr ios)))
                          ;; XXX interpret flags.
                          (list ios-id (vector (number-to-string ios-id)
                                               ios-handler
                                               (number-to-string ios-flags)
                                               (concat (number-to-string ios-size) "#B")))))
                      poke-ios-alist)))
        (setq tabulated-list-format headers)
        (setq tabulated-list-padding 2)
        (tabulated-list-init-header)
        (setq tabulated-list-entries entries)
        (tabulated-list-print nil)))))

(defun poke-ios ()
  (interactive)
  (let ((buf (get-buffer-create "*poke-ios*")))
    (with-current-buffer buf
      (poke-ios-mode)
      (poke-ios-populate)))
  (when (called-interactively-p)
    (switch-to-buffer-other-window "*poke-ios*")))

;;;; Main interface

(defconst poke-pk
  "\
fun poke_el_banner = void:
{
  /* XXX include libpoke version.  */
  print \"Welcome to GNU poke.\\n\";
}

fun poke_el_ios_open = (int<32> ios) void:
{
  var cmd = format (\"(poke-ios-open %i32d %v %u64d %u64d)\",
                    ios, iohandler (ios), ioflags (ios), iosize (ios)/#B);
  plet_elval (cmd);
}

fun poke_el_ios_close = (int<32> ios) void:
{
  var cmd = format (\"(poke-ios-close %i32d)\", ios);
  plet_elval (cmd);
}

fun poke_el_ios_set = (int<32> ios) void:
{
  var cmd = format (\"(poke-ios-set %i32d)\", ios);
  plet_elval (cmd);
}

ios_open_hook += [poke_el_ios_open];
ios_close_hook += [poke_el_ios_close];
ios_set_hook += [poke_el_ios_set];

fun quit = void:
{
  plet_elval (\"(poke-exit)\");
}")

(defun poke-open-file (filename)
  (interactive "fFile to open: ")
  ;; XXX: quote filename if needed
  (poke-code-send
   (concat "{ set_ios (open (\"" filename "\")); } ?! E_io;")))

(defun poke-load-file (filename)
  (interactive "fPickle to load: ")
  (poke-code-send (concat "load \"" filename "\";")))

(defun poke-set-omode ()
  (interactive)
  (let* ((omode (completing-read "Output mode: " '("VM_OMODE_PLAIN" "VM_OMODE_TREE") nil t)))
    (poke-code-send (concat "vm_set_omode (" omode ");"))))

(defun poke-set-pretty-print ()
  (interactive)
  (let* ((pprint (completing-read "Pretty-print: " '("yes" "no") nil t)))
    (poke-code-send (concat "vm_set_opprint ("
                            (if (equal pprint "yes")
                                (number-to-string 1)
                              (number-to-string 0)) ");"))))

(defun poke ()
  (interactive)
  (when (not (process-live-p poke-poked-process))
    (setq poke-ios-alist nil)
    (poke-poked)
    (sit-for 0.2))
  (poke-elval)
  (poke-code-send poke-pk)
  (poke-repl)
  (poke-vu)
  (delete-other-windows)
  (switch-to-buffer "*poke-vu*")
  (switch-to-buffer-other-window "*poke-out*")
  (switch-to-buffer-other-window "*poke-repl*"))

(defun poke-exit ()
  (interactive)
  (when (or (not poke-ios-alist)
            (yes-or-no-p "There are open IO spaces.  Exit anyway? "))
    ;; Note that killing the buffers will also kill the
    ;; associated processes if they are running.
    (mapcar
     (lambda (bufname)
       (let ((buf (get-buffer bufname)))
         (when buf (kill-buffer buf))))
     '("*poke-out*" "*poke-cmd*" "*poke-code*"
       "*poke-vu*" "*poke-repl*" "*poke-elval*" "*poked*"))
    (setq poke-repl-prompt poke-repl-default-prompt)
    (setq poke-ios-alist nil)))

;;; poke.el ends here
