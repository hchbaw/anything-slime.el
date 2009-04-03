;;; anything-slime.el --- anything-sources and some utilities for slime.

;; Copyright (C) 2009 Takeshi Banse <hchbaw@laafc.net>

;; Author: Takeshi Banse <hchbaw@laafc.net>
;; Keywords: anything, slime
;; URL: http://www.emacswiki.org/cgi-bin/wiki/anything-slime.el

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;;; Installation:
;;
;; Put anything-slime.el, anything.el and anything-complete.el to your
;; load-path. Also set up slime properly. Finally call `slime-setup' and
;; include 'anything-slime as argument:
;;
;;   (slime-setup '(anything-slime [others contribs ...]))
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-slime-complete'
;;    Select a symbol from slime's completion systems.
;;  `anything-slime-list-connections'
;;    Yet another `slime-list-connections' with `anything'.
;;

(require 'slime-fuzzy)
(require 'slime-c-p-c)
(require 'anything)
(require 'anything-complete)

(defmacro with-anything-slime-symbol-position (beg end &rest body)
  `(let* ((,end (move-marker (make-marker) (slime-symbol-end-pos)))
          (,beg (move-marker (make-marker) (slime-symbol-start-pos))))
     (unwind-protect (progn ,@body)
       (set-marker ,end nil)
       (set-marker ,beg nil))))
(put 'with-anything-slime-symbol-position 'lisp-indent-function 2)
(defun anything-slime-symbol-position-funcall (f)
  (with-anything-slime-symbol-position beg end (funcall f beg end)))

(add-to-list 'anything-type-attributes
             '(anything-slime-complete
               (action
                . (("Insert" . ac-insert)
                   ("Describe symbol" . slime-describe-symbol)
                   ("Edit definition" . slime-edit-definition)))
               (persistent-action . slime-describe-symbol)
               (volatile)))

(defvar anything-c-source-slime-simple-complete
  '((name . "slime simple complete")
    (candidates
     . (lambda ()
         (car (slime-simple-completions anything-complete-target))))
    (type . anything-slime-complete)))
(defvar anything-c-source-slime-compound-complete
  '((name . "slime compound complete")
    (candidates
     . (lambda ()
         (with-current-buffer anything-current-buffer
           (car (anything-slime-symbol-position-funcall
                 #'slime-contextual-completions)))))
    (type . anything-slime-complete)))
(defvar anything-c-source-slime-fuzzy-complete
  '((name . "slime fuzzy complete")
    (candidates
     . (lambda ()
         (mapcar 'car
                 (car (slime-fuzzy-completions anything-complete-target)))))
    (type . anything-slime-complete)))

(defvar anything-slime-complete-sources
  '(anything-c-source-slime-simple-complete
    anything-c-source-slime-compound-complete
    anything-c-source-slime-fuzzy-complete))

(defun anything-slime-complete ()
  "Select a symbol from slime's completion systems."
  (interactive)
  (anything-complete anything-slime-complete-sources
                     (anything-slime-symbol-position-funcall
                      #'buffer-substring-no-properties)))

(defvar anything-c-source-slime-connection
  '((name . "slime connections")
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

(defadvice anything-slime-update-connection-list
  (around slime-update-connection-list)
  "Don't call slime-update-connection-list if anythinging. (This is iffy.)"
  (when (not anything-source-name)
    ad-do-it))
(ad-activate 'anything-slime-update-connection-list)

(defun anything-slime-init ()
  (run-hooks 'anything-slime-init-hook))

(provide 'anything-slime)
