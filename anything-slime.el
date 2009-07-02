;;; anything-slime.el --- anything-sources and some utilities for SLIME.

;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;; Author: Takeshi Banse <takebi@laafc.net>
;; Keywords: convenience, anything, slime

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Some Anything and SLIME Configurations for using SLIME within the
;; Anything interface. (The `ascsa-' prefix comes here.)

;;; Installation:
;;
;; Put the anything-slime.el, anything.el and anything-complete.el to your
;; load-path.
;; Set up the SLIME properly.
;; Call `slime-setup' and include 'anything-slime as the arguments:
;;
;;   (slime-setup '([others contribs ...] anything-slime))
;;
;; or simply require anything-slime.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-slime-complete'
;;    Select a symbol from the SLIME's completion systems.
;;  `anything-slime-list-connections'
;;    Yet another `slime-list-connections' with `anything'.
;;  `anything-slime-apropos'
;;    Yet another `slime-apropos' with `anything'.
;;

;;; Code:

(require 'anything)
(require 'anything-complete)
(require 'slime)
(require 'slime-c-p-c)
(require 'slime-fuzzy)
(require 'slime-repl)

(defun ascsa-symbol-position-funcall (f)
  (let* ((end (move-marker (make-marker) (point)))
         (beg (move-marker (make-marker) (slime-symbol-start-pos))))
    (unwind-protect
        (progn (funcall f beg end))
      (set-marker end nil)
      (set-marker beg nil))))

(define-anything-type-attribute 'anything-slime-complete
  '((action
     . (("Insert" . ac-insert)
        ("Describe symbol" . slime-describe-symbol)
        ("Edit definition" . slime-edit-definition)))
    (persistent-action . slime-describe-symbol)
    (volatile))
  "SLIME complete.")

;; These sources are private for the use of the `anything-slime-complete'
;; command, so I should not make `anything-c-source-*' symbols.
(defvar anything-slime-simple-complete-source
  '((name . "SLIME simple complete")
    (candidates
     . (lambda ()
         (car (slime-simple-completions anything-complete-target))))
    (type . anything-slime-complete)))
(defvar anything-slime-fuzzy-complete-source
  '((name . "SLIME fuzzy complete")
    (candidates
     . (lambda ()
         (mapcar #'car
                 (car (slime-fuzzy-completions anything-complete-target)))))
    (type . anything-slime-complete)))
(defvar anything-slime-compound-complete-source
  '((name . "SLIME compound complete")
    (candidates
     . (lambda ()
         (with-current-buffer anything-current-buffer
           (car (ascsa-symbol-position-funcall
                 #'slime-contextual-completions)))))
    (type . anything-slime-complete)))

(defvar anything-slime-complete-sources
  '(anything-slime-simple-complete-source
    anything-slime-fuzzy-complete-source
    anything-slime-compound-complete-source))

(defun anything-slime-complete ()
  "Select a symbol from the SLIME's completion systems."
  (interactive)
  (anything-complete anything-slime-complete-sources
                     (ascsa-symbol-position-funcall
                      #'buffer-substring-no-properties)))

(defvar anything-c-source-slime-connection
  '((name . "SLIME connections")
    (candidates
     . (lambda ()
         (let* ((default slime-default-connection)
                (collect (lambda (con)
                           (cons
                            (if (eq default con)
                              (concat "* " (slime-connection-name con))
                              (slime-connection-name con))
                            con))))
           (mapcar collect (reverse slime-net-processes)))))
    (action
     . (("Go to repl"
         . (lambda (con)
             (let ((slime-dispatching-connection con))
               (switch-to-buffer (slime-output-buffer)))))
        ("Set default" . slime-select-connection)
        ("Restart" . slime-restart-connection-at-point)
        ("Quit" . slime-quit-connection-at-point)))))

(defun anything-slime-list-connections ()
  "Yet another `slime-list-connections' with `anything'."
  (interactive)
  (anything 'anything-c-source-slime-connection))

(defadvice anything-slime-update-connection-list (around ignore activate)
  "Don't call slime-update-connection-list if anythinging. (This is iffy.)"
  (when (not anything-source-name)
    ad-do-it))

(define-anything-type-attribute 'anything-slime-apropos
  '((action
     . (("Describe symbol" . slime-describe-symbol)
        ("Edit definition" . slime-edit-definition)))
    (persistent-action . slime-describe-symbol)
    (requires-pattern . 2))
    ;;(volatile)
  "SLIME apropos.")

(defun ascsa-apropos-source (name slime-expressions)
  `((name . ,name)
    (candidates
     . (lambda ()
         (with-current-buffer anything-current-buffer
           (loop for plist in (slime-eval ,slime-expressions)
                 collect (plist-get plist :designator)))))
    (type . anything-slime-apropos)))
(defvar anything-c-source-slime-apropos-symbol-current-package
  (ascsa-apropos-source "SLIME apropos (current package)"
                        (quote
                         `(swank:apropos-list-for-emacs
                           ,anything-pattern
                           nil
                           nil
                           ,(or slime-buffer-package
                                (slime-current-package))))))
(defvar anything-c-source-slime-apropos-symbol-external-package
  (ascsa-apropos-source "SLIME apropos (external package)"
                        (quote
                         `(swank:apropos-list-for-emacs
                           ,anything-pattern
                           t
                           nil
                           ,(or slime-buffer-package
                                (slime-current-package))))))
(defvar anything-c-source-slime-apropos-symbol-all-external-package
  (ascsa-apropos-source "SLIME apropos (all external package)"
                        (quote
                         `(swank:apropos-list-for-emacs
                           ,anything-pattern
                           t
                           nil
                           nil))))
(defvar anything-c-source-slime-apropos-symbol-all-package
  (ascsa-apropos-source "SLIME apropos (all package)"
                        (quote
                         `(swank:apropos-list-for-emacs
                           ,anything-pattern
                           nil
                           nil
                           nil))))
(defvar anything-slime-apropos-sources
  '(anything-c-source-slime-apropos-symbol-current-package
    anything-c-source-slime-apropos-symbol-external-package
    anything-c-source-slime-apropos-symbol-all-external-package
    anything-c-source-slime-apropos-symbol-all-package))

(defun anything-slime-apropos ()
  "Yet another `slime-apropos' with `anything'."
  (interactive)
  (anything anything-slime-apropos-sources
            nil nil nil nil "*anything SLIME apropos*"))

(defun anything-slime-init ()
  (run-hooks 'anything-slime-init-hook))

(provide 'anything-slime)
