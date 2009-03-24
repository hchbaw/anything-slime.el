;;
;; TODO
;; - Narrow slime-describe-symbol's pop-up window when invoked through
;;   anything-persistent-action.
;;

(require 'slime)
(require 'slime-fuzzy)
(require 'slime-c-p-c)
(require 'anything)
(require 'anything-complete)

(add-to-list 'anything-type-attributes
             '(slime-anything
               (action . (("Insert" . ac-insert)
                          ("Describe symbol" . slime-describe-symbol)))
               (persistent-action . (lambda (sym)
                                      (with-anything-window
                                        (slime-describe-symbol sym))))
               (volatile)))

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
  (anything-complete
   (if (eq major-mode 'clojure-mode)
     (remove 'anything-c-source-slime-fuzzy-complete
             anything-slime-complete-sources)
     anything-slime-complete-sources)
   (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
          (beg (move-marker (make-marker) (slime-symbol-start-pos))))
     (buffer-substring-no-properties beg end))))

;; (define-key slime-mode-map "\C-o" 'anything-slime-complete)

(provide 'slime-anything)
