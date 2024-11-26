;;; syncthing-widgets.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'widget)

(defun syncthing-collapsible-value-create (widget)
  "Insert WIDGET's value without ignoring propertizing."
  (insert (widget-value widget)))

(define-widget 'syncthing-collapsible 'visibility
  "Collapsible widget with :items as children."
  :format "%[%v %t%]"
  :value-create
  (lambda (widget &rest _)
    ;; "super"
    (apply 'widget-visibility-value-create (list widget))

    ;; :value for opening as in 'toggle
    (let ((shown (widget-value widget)))
	  (when shown
        (save-excursion
          ;; move to new line and separate
          (re-search-forward (widget-get widget :tag) nil t 1)
          (forward-line 1)
	      (insert ?\n)

          (let ((spacing (or (widget-get widget :spacing) 0)))
            ;; calculate spacing
            (unless (widget-get widget :spacing)
              (dolist (child (widget-get widget :items))
                (setq spacing
                      (max spacing (length (or (widget-get child :tag) ""))))))

            ;; render children (':children' is reserved)
            (dolist (child (widget-get widget :items))
              ;; tag + spacing, so the navigation in values isn't prefixed
              (widget-put
               child
               :tag (concat (widget-get child :tag)
                            (propertize
                             " " 'display `(space :align-to ,spacing))))

              ;; default :value-create ignores propertizing of :value
              (widget-put
               child :value-create 'syncthing-collapsible-value-create)
              (widget-create-child widget child))))))))

(provide 'syncthing-widgets)
;;; syncthing-widgets.el ends here
