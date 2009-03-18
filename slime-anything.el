(require 'slime)
(require 'slime-fuzzy)
(require 'anything)
(require 'anything-complete)


(defvar anything-slime-simple-complete-candidates nil)
(defvar anything-slime-simple-complete-source
  '((name . "slime simple complete")
    (init . (lambda ()
              (setq anything-slime-simple-complete-candidates
                    (car
                     (slime-simple-completions anything-complete-target)))))
    (candidates . anything-slime-simple-complete-candidates)
    (action . ac-insert)
    (volatile)))

(defvar anything-slime-fuzzy-complete-candidates nil)
(defvar anything-slime-fuzzy-complete-source
  '((name . "slime fuzzy complete")
    (init . (lambda ()
              (setq anything-slime-fuzzy-complete-candidates
                    (mapcar
                     'car
                     (car
                      (slime-fuzzy-completions anything-complete-target))))))
    (candidates . anything-slime-fuzzy-complete-candidates)
    (type . ac-insert)
    (volatile)))

(defun anything-slime-complete ()
  (interactive)
  (anything-complete
   '(anything-slime-simple-complete-source
     anything-slime-fuzzy-complete-source)
   (anything-aif (symbol-at-point) (symbol-name it) "")))

(provide 'slime-anything)
