(require 'slime)
(require 'slime-fuzzy)
(require 'slime-c-p-c)
(require 'anything)
(require 'anything-complete)

(defun anything-slime-describe-symbol (sym)
  (when anything-in-persistent-action
    (let ((tmp (or (get-buffer-window-and-frame "*SLIME Description*")
                   (list
                    (select-window (split-window-vertically
                                    (* (/ (window-height) 3) 2)))
                    (selected-frame)))))
      (apply #'select-window-and-frame tmp))
    (slime-describe-symbol sym)))

(add-to-list 'anything-type-attributes
             '(slime-anything
               (action
                . (("Insert" . ac-insert)
                   ("Describe symbol" . anything-slime-describe-symbol)
                   ("Edit definition" . slime-edit-definition)))
               (persistent-action . anything-slime-describe-symbol)
               (volatile)))

;; (progn
;;   (define-key anything-map "\C-d" (lambda ()
;;                                     (interactive)
;;                                     (anything-execute-persistent-action
;;                                      'persistent-action-2)))
;;   (dolist (x anything-slime-complete-sources)
;;     (add-to-list x
;;                  '(persistent-action-2 . slime-edit-definition))))

(defvar anything-c-source-slime-simple-complete
  '((name . "slime simple complete")
    (candidates
     . (lambda ()
         (car (slime-simple-completions anything-complete-target))))
    (type . slime-anything)))
(defvar anything-c-source-slime-compound-complete
  '((name . "slime compound complete")
    (candidates
     . (lambda ()
         (with-current-buffer anything-current-buffer
           (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
                  (beg (move-marker (make-marker) (slime-symbol-start-pos))))
             (car (slime-contextual-completions beg end))))))
    (type . slime-anything)))
(defvar anything-c-source-slime-fuzzy-complete
  '((name . "slime fuzzy complete")
    (candidates
     . (lambda ()
         (mapcar 'car
                 (car (slime-fuzzy-completions anything-complete-target)))))
    (type . slime-anything)))

(defvar anything-slime-complete-sources
  '(anything-c-source-slime-simple-complete
    anything-c-source-slime-compound-complete
    anything-c-source-slime-fuzzy-complete))

(defun anything-slime-complete ()
  "Select a symbol from slime's completion systems."
  (interactive)
  (let ((neg #'(lambda (source)
                 (when (symbolp source)
                       (setq source (symbol-value source)))
                 (anything-aif (assoc-default 'sa-ignore-connections source)
                               (member (slime-connection-name) it)))))
    (anything-complete
     (remove-if neg anything-slime-complete-sources)
     (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
            (beg (move-marker (make-marker) (slime-symbol-start-pos))))
       (buffer-substring-no-properties beg end)))))

(defun slime-anything-init ()
  "Initialize builtin slime anything environment."
  (interactive)
  (slime-anything-bind-keys)
  (add-to-list 'anything-c-source-slime-fuzzy-complete
               '(sa-ignore-connections . ("clojure"))))

(defun slime-anything-bind-keys ()
  (progn
    (define-key slime-mode-map "\C-o" 'anything-slime-complete)
    (define-key slime-repl-mode-map "\C-o" 'anything-slime-complete)))

(provide 'slime-anything)
