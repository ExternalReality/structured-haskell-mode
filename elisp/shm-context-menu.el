;;; package --- Summary

;;; Commentary:

;;; Code:
(require 'shm)
(require 'popup)

(if (eq window-system 'x)
    (define-key shm-map (kbd "M-<return>") 'shm/present-actions-for-node)
  (define-key shm-map (kbd "M-]") 'shm/present-actions-for-node))

(defun shm-get-refactors (current-node)
  "Get a vector of possible refactorings for the (CURRENT-NODE)."
  (shm-lint-ast "decl"
                (shm-node-start current-node)
                (shm-node-end current-node)))

(defun shm-import-decl-p (node-cons)
  (string= "ImportDecl" node-cons))

(defun shm-module-name-p (node-cons)
  (string= "ModuleName" node-cons))

(defun shm-has-parent-with-matching-type-p (node-pair)
  (let* ((current (cdr node-pair))
         (parent-pair (shm-node-parent node-pair (shm-node-type current)))
         (parent (cdr parent-pair)))
    (if parent
        (if (string= (shm-node-type current)
                     (shm-node-type parent)) t))))

(defun shm/present-actions-for-node ()
  "Display menu of possible actions for node."
  (interactive)
  (let* ((pair (shm-current-node-pair))
         (current (cdr pair))
         (cons (shm-node-cons current))
         (refactors (shm-get-refactors current))
         (menu (cond ((shm-refactors-available-p refactors)
                      (shm-items-for-refactors refactors))
                     ((shm-top-level-type-decl-p pair)
                      (shm-item-for-top-level-type-decl))
                     ((shm-import-decl-p cons) (shm-item-for-import-decl))
                     ((shm-has-parent-with-matching-type-p pair)
                      (shm-item-for-child-nodes-with-matching-parent))
                     ((and (shm-module-name-p cons)
                           (fboundp (quote haskell-mode-tag-find)))
                      (shm-item-for-module-name)))))
        (if menu (progn 
                   (cancel-timer shm-parsing-timer)
                   (unwind-protect 
                       (shm-invoke-action-for-menu-item (popup-menu* menu))
                     (setq shm-parsing-timer
                         (run-with-idle-timer shm-idle-timeout t 'shm-reparsing-timer)))))))
                    

(defun shm-refactors-available-p (refactors)
 "Check to see if the (REFACTORS) vector is populated."
  (if (> (length refactors) 0) t))

(defun shm-items-for-refactors (refactors)
 "Create a popup menu items from (REFACTORS)."
  (mapcar 'shm-item-for-refactor refactors))

(defun shm-invoke-hlint-suggestion (refactor)
  "Replace the current node with the suggestion from the (REFACTOR)."
  (let* ((current-node (shm-current-node))
         (start (shm-node-start current-node))
         (end (shm-node-end current-node)))
    (save-excursion
       (delete-region start end)
       (goto-char start)      
       (insert (elt refactor 3)))))

(defun shm-item-for-refactor (refactor)
  "Create the menu item for a particular (REFACTOR)."
  (popup-make-item (concat "⚒ " (elt refactor 1)) :value (cons "hlint suggestion"  refactor)))

(defun shm-item-for-import-decl ()
  (list (popup-make-item "✎ qualify import" :value "qualify import")))

(defun shm-item-for-child-nodes-with-matching-parent ()
  (list (popup-make-item "⚒ raise" :value "raise child")))

(defun shm-item-for-module-name ()
  (list (popup-make-item "✈ visit module"  :value "visit module definition")))

(defun shm-item-for-top-level-type-decl ()
  (list (popup-make-item "✎ add type constraint" :value "add type constraint")))

(defun shm-invoke-action-for-menu-item (item-value)
  "Invoke function on (ITEM-VALUE) chosen from the context menu."
  (cond ((selected-item-value-p item-value "qualify import") (invoke-with-suggestion 'shm/qualify-import))
        ((selected-item-value-p item-value "raise child") (invoke-with-suggestion 'shm/raise))
        ((selected-item-value-p item-value "visit module definition") (invoke-with-suggestion 'haskell-mode-tag-find))
        ((selected-item-value-p item-value "hlint suggestion") (invoke-with-suggestion 'shm-invoke-hlint-suggestion (cdr item-value))) ;
        ((selected-item-value-p item-value "add type constraint") (invoke-with-suggestion 'shm/modify-type-constraint))))

(defun selected-item-value-p (value match)
 "Extract String from (VALUE) and check for string equality against (MATCH)."
 (or (and (stringp value) (string= value match))
     (string= (car value) match)))

(defun invoke-with-suggestion (function &optional arg)
  "Invoke (FUNCTION) with on (ARG) and show its key binding in mini buffer if it has one."
  (if arg (funcall function arg) (funcall function))
  (let ((binding (where-is-internal function shm-map t)))
    (when binding
      (with-temp-message
          (format "You can run the command `%s' with %s"
                  function (key-description binding))
        (sit-for (if (numberp suggest-key-bindings)
                     suggest-key-bindings
                   2))))))

(provide 'shm-context-menu)

;;; shm-context-menu.el ends here
