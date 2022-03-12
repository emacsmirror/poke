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

(defvar poke-styling-faces
  '(("integer" poke-integer-face)
    ("string" poke-string-face)
    ("offset" poke-offset-face)
    ("struct-field-name" poke-struct-field-name-face)
    ("diff-thunk-header"  poke-diff-thunk-header-face)
    ("diff-minus" poke-diff-minus-face)
    ("diff-plus" poke-diff-plus-face)))

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

;;;; generic pokelet stuff

(defun poke-make-pokelet-process (name ctrl)
  (let ((proc (make-network-process :name name
                                    :buffer (concat "*" name "*")
                                    :family 'local
                                    :service poked-socket)))
    (process-send-string proc ctrl)
    proc))

;;;; poke-out pokelet

(defvar poke-out-process nil)
(defvar poke-out-buf "")
(defvar poke-out-length 0)
(defvar poke-out-eval "")
(defvar poke-out-styles nil)
(defvar poke-out-emitted-iter-string nil)
(defvar poke-out-iter-string
  (propertize (char-to-string 8594) 'font-lock-face 'poke-iter-string-face))
(defvar poke-out-iter-begin nil)

(defconst poke-out-state-waiting-for-length 0)
(defconst poke-out-state-waiting-for-msg 1)
(defvar poke-out-state poke-out-state-waiting-for-length)

(defun poke-out-stylize (string)
  (let ((propertized-string string))
    (mapcar (lambda (style)
              (let* ((face-ass (assoc style poke-styling-faces))
                     (face (when face-ass (cadr face-ass))))
                (setq propertized-string
                      (if face
                          (propertize propertized-string 'font-lock-face face)
                        propertized-string))))
            (reverse poke-out-styles))
    propertized-string))

(defun poke-out-filter (proc string)
  (setq poke-out-buf (concat poke-out-buf string))
  (while (or (and (= poke-out-state poke-out-state-waiting-for-length)
                  (>= (length poke-out-buf) 2))
             (and (= poke-out-state poke-out-state-waiting-for-msg)
                  (>= (length poke-out-buf) poke-out-length)))
    (if (= poke-out-state poke-out-state-waiting-for-length)
        (progn
          (setq poke-out-length
                (logior (ash (aref poke-out-buf 1) 8) (aref poke-out-buf 0)))
          (setq poke-out-buf (substring poke-out-buf 2))
          (setq poke-out-state poke-out-state-waiting-for-msg))
      ;; state is poke-out-state-waiting-for-msg.
      (when (>= (length poke-out-buf) poke-out-length)
	;; Action on the message according to the command.
	(pcase (aref poke-out-buf 0)
	  (1 ;; Iteration begin
           (setq poke-out-eval "")
           (when (buffer-live-p (process-buffer proc))
             (with-current-buffer (process-buffer proc)
               (let ((buffer-read-only nil))
                 (goto-char (point-max))
                 (setq poke-out-iter-begin (point))))))
          (3 ;; Iteration end
           (when (buffer-live-p (process-buffer proc))
             (with-current-buffer (process-buffer proc)
               (save-excursion
                 (unless (equal poke-out-iter-begin (point-max))
                   (narrow-to-region poke-out-iter-begin (point-max)))
                 (let ((buffer-read-only nil))
                   (mapcar (lambda (window)
                             (set-window-point window (point-max)))
                           (get-buffer-window-list))))))
           (setq poke-out-emitted-iter-string nil)
           (when (process-live-p poke-repl-process)
             (poke-repl-end-of-iteration)))
	  (2 ;; Process terminal poke output
           (let ((output (poke-out-stylize
                          (substring poke-out-buf 1 (- poke-out-length 1)))))
             (when (buffer-live-p (process-buffer proc))
               (with-current-buffer (process-buffer proc)
                 (save-excursion
                   (let ((buffer-read-only nil))
                     (goto-char (point-max))
                     (unless poke-out-emitted-iter-string
                       (insert (concat poke-out-iter-string "\n"))
                       (setq  poke-out-emitted-iter-string t))
                     (insert output)))))))
          (6 ;; Process eval poke output
           (let ((output (poke-out-stylize
                          (substring poke-out-buf 1 (- poke-out-length 1)))))
             ;; Append the output to the global variable which will be
             ;; handled at the end of the iteration.
             (setq poke-out-eval
                   (concat poke-out-eval output))
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
                          (substring poke-out-buf 1 (- poke-out-length 1)))))
             ;; Append to the eval output for now.
             (setq poke-out-eval
                   (concat poke-out-eval output))
             ;; If there is no repl, output this in the *poke-out*
             ;; buffer prefixed with error>
             (when (not (process-live-p poke-repl-process))
               (when (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc)
                   (let ((buffer-read-only nil))
                     (goto-char (point-max))
                     (insert (concat "error>" output))))))))
          (4 ;; Styling class begin
           (let ((style (substring poke-out-buf 1 (- poke-out-length 1))))
             (setq poke-out-styles (cons style poke-out-styles))))
          (5 ;; Styling class end
           (let ((style (substring poke-out-buf 1 (- poke-out-length 1))))
             (if (or (not poke-out-styles)
                     (not (equal (car poke-out-styles) style)))
                 (error "closing a mismatched style")
               (setq poke-out-styles (cdr poke-out-styles)))))
	  (_ ;; Protocol error
	   (setq poke-out-buf "")
	   (setq poke-out-length 0)
	   (error "pokelet protocol error"))))
      ;; Discard used portion of poke-out-buf and reset state.
      (setq poke-out-buf (substring poke-out-buf poke-out-length))
      (setq poke-out-state poke-out-state-waiting-for-length))))

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
    (setq poke-out-state poke-out-state-waiting-for-length)
    (setq poke-out-buf "")
    (setq poke-out-length 0)
    (setq poke-out-styles nil)
    (setq poke-out-emitted-iter-string nil)
    (setq poke-out-process
          (poke-make-pokelet-process "poke-out" "\x81"))
    (set-process-query-on-exit-flag poke-out-process nil)
    (set-process-filter poke-out-process #'poke-out-filter)
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
(defvar poke-vu-buf "")
(defvar poke-vu-length 0)
(defvar poke-vu-output "")

(defconst poke-vu-state-waiting-for-length 0)
(defconst poke-vu-state-waiting-for-msg 1)
(defvar poke-vu-state poke-vu-state-waiting-for-length)

(defun poke-vu-filter (proc string)
  (setq poke-vu-buf (concat poke-vu-buf string))
  (while (or (and (= poke-vu-state poke-vu-state-waiting-for-length)
                  (>= (length poke-vu-buf) 2))
             (and (= poke-vu-state poke-vu-state-waiting-for-msg)
                  (>= (length poke-vu-buf) poke-vu-length)))
    (if (= poke-vu-state poke-vu-state-waiting-for-length)
        (progn
          (setq poke-vu-length
                (logior (ash (aref poke-vu-buf 1) 8) (aref poke-vu-buf 0)))
          (setq poke-vu-buf (substring poke-vu-buf 2))
          (setq poke-vu-state poke-vu-state-waiting-for-msg))
      ;; state is poke-vu-state-waiting-for-msg.
      (when (>= (length poke-vu-buf) poke-vu-length)
	;; Action on the message according to the command.
	(pcase (aref poke-vu-buf 0)
	  (1 ;; RESET
           (when (buffer-live-p (process-buffer proc))
             (with-current-buffer (process-buffer proc)
               (let ((buffer-read-only nil))
                 (delete-region (point-min) (point-max))))))
          (2 ;; APPEND
           (setq poke-vu-output
                 (concat poke-vu-output
                         (substring poke-vu-buf 1 (- poke-vu-length 1)))))
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
                 (insert poke-vu-output)
                 (goto-char (point-min)))))
           (setq poke-vu-output ""))
	  (_ ;; Protocol error
	   (setq poke-vu-buf "")
	   (setq poke-vu-length 0)
	   (error "pokelet protocol error")))
	;; Discard used portion of poke-vu-buf and reset state.
        (setq poke-vu-buf (substring poke-vu-buf poke-vu-length))
        (setq poke-vu-state poke-vu-state-waiting-for-length)))))

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
    (setq poke-vu-state poke-vu-state-waiting-for-length)
    (setq poke-vu-buf "")
    (setq poke-vu-output "")
    (setq poke-vu-length 0)
    (setq poke-vu-process
          (poke-make-pokelet-process "poke-vu" "\x82"))
    (set-process-query-on-exit-flag poke-vu-process nil)
    (set-process-filter poke-vu-process #'poke-vu-filter)
    (save-excursion
     (set-buffer "*poke-vu*")
     (poke-vu-mode)))
  (when (called-interactively-p)
    (switch-to-buffer-other-window "*poke-vu*")))

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

(defvar poke-elval-process nil)
(defvar poke-elval-buf "")
(defvar poke-elval-length 0)

(defconst poke-elval-state-waiting-for-length 0)
(defconst poke-elval-state-waiting-for-msg 1)
(defvar poke-elval-state poke-elval-state-waiting-for-length)

(defconst poke-elval-init-pk
  "\
var PLET_ELVAL_CMD_EVAL = 0UB;

fun plet_elval = (string s) void:
{
  var c = byte[s'length] ();

  stoca (s, c);
  chan_send (100,  [PLET_ELVAL_CMD_EVAL] + c);
}
")

(defun poke-elval-filter (proc string)
  (setq poke-elval-buf (concat poke-elval-buf string))
  (while (or (and (= poke-elval-state poke-elval-state-waiting-for-length)
                  (>= (length poke-elval-buf) 2))
             (and (= poke-elval-state poke-elval-state-waiting-for-msg)
                  (>= (length poke-elval-buf) poke-elval-length)))
    (if (= poke-elval-state poke-elval-state-waiting-for-length)
        (progn
          (setq poke-elval-length
                (logior (ash (aref poke-elval-buf 1) 8) (aref poke-elval-buf 0)))
          (setq poke-elval-buf (substring poke-elval-buf 2))
          (setq poke-elval-state poke-elval-state-waiting-for-msg))
      ;; state is poke-elval-state-waiting-for-msg.
      (when (>= (length poke-elval-buf) poke-elval-length)
	;; Action on the message according to the command.
	(pcase (aref poke-elval-buf 0)
          (0 ;; EVAL
           (ignore-errors
             (let* ((code (substring poke-elval-buf 1 (- poke-elval-length 1)))
                    (form (read-from-string code)))
               (eval (car form)))))
          (_ ;; Protocol error
	   (setq poke-elval-buf "")
	   (setq poke-elval-length 0)
	   (error "pokelet protocol error")))
	;; Discard used portion of poke-elval-buf and reset state.
        (setq poke-elval-buf (substring poke-elval-buf poke-elval-length))
        (setq poke-elval-state poke-elval-state-waiting-for-length)))))

(defun poke-elval ()
  (interactive)
  (when (not (process-live-p poke-elval-process))
    (poke-code)
    (poke-code-send poke-elval-init-pk)
    (setq poke-elval-buf "")
    (setq poke-elval-length 0)
    (setq poke-elval-process
          (poke-make-pokelet-process "poke-elval" "\xe4"))
    (set-process-query-on-exit-flag poke-elval-process nil)
    (set-process-filter poke-elval-process #'poke-elval-filter)))

;;;; poke-repl

(defconst poke-repl-prompt "#!poke!# ")
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

(defun poke-repl-end-of-iteration ()
  (with-current-buffer "*poke-repl*"
    (let ((buffer-read-only nil))
      (save-excursion
        (re-search-backward
         (regexp-quote (concat "#" (number-to-string poke-repl-seq)))
         nil t)
        (delete-region (point) (line-end-position))
        (if (> (length poke-out-eval) 0)
            (insert poke-out-eval)
          (unless (equal (point) (point-max))
            (delete-char 1))))
      (setq poke-repl-seq (1+ poke-repl-seq)))))

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
        (insert "Welcome to GNU poke.\n")
        (poke-repl-mode))))
  (when (called-interactively-p)
    (switch-to-buffer-other-window "*poke-repl*")))

;;;; Main interface

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
    (poke-poked)
    (sit-for 0.2))
  (poke-elval)
  (poke-repl)
  (poke-vu)
  (delete-other-windows)
  (switch-to-buffer "*poke-vu*")
  (switch-to-buffer-other-window "*poke-out*")
  (switch-to-buffer-other-window "*poke-repl*"))

(defun poke-exit ()
  (interactive)
  ;; Note that killing the buffers will also kill the
  ;; associated processes if they are running.
  (mapcar
   (lambda (bufname)
     (let ((buf (get-buffer bufname)))
       (when buf (kill-buffer buf))))
   '("*poke-out*" "*poke-cmd*" "*poke-code*"
     "*poke-vu*" "*poke-repl*" "*poke-elval*" "*poked*")))

;;; poke.el ends here
