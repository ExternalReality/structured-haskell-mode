(require 'shm)
(require 'shm-test)
(require 'popup)

(if (eq window-system 'x)
    (define-key shm-map (kbd "M-<return>") 'shm/present-actions-for-node)
  (define-key shm-map (kbd "M-]") 'shm/present-actions-for-node))

(defun shm-import-decl-p (node-cons)
  (string= "ImportDecl" node-cons))

(defun shm-module-name-p (node-cons)
  (string= "ModuleName" node-cons))



(defun shm/present-actions-for-node ()
  "Display menu of possible actions for node"
  (interactive)
  (let* ((pair (shm-current-node-pair))
         (cons (shm-node-cons (cdr pair)))
         (menu (cond ((shm-top-level-type-decl-p pair)
                      (shm-item-for-top-level-type-decl))
                     ((shm-import-decl-p cons) (shm-item-for-import-decl))
                     ((shm-has-parent-with-matching-type-p pair)
                      (shm-item-for-child-nodes-with-matching-parent))
                     ((and (shm-module-name-p cons)
                           (fboundp (quote haskell-mode-tag-find)))
                      (shm-item-for-module-name)))))
    (if menu (progn (shm-invoke-action-for-menu-item (popup-menu* menu))))))

(defun shm-item-for-import-decl ()
  (list (popup-make-item "✎ qualify import" :value "qualify import")))

(defun shm-item-for-child-nodes-with-matching-parent ()
  (list (popup-make-item "⚒ raise" :value "raise child")))

(defun shm-item-for-module-name ()
  (list (popup-make-item "✈ visit module" :value "visit module definition")))

(defun shm-item-for-top-level-type-decl ()
  (list (popup-make-item "✎ add type constraint" :value "add type constraint")))

(defun shm-invoke-action-for-menu-item (n)
  "invoke the function chosen from the context menu"
  (cond ((string= n "qualify import") (invoke-with-suggestion 'shm/qualify-import))
        ((string= n "raise child") (invoke-with-suggestion 'shm/raise))
        ((string= n "visit module definition") (invoke-with-suggestion 'haskell-mode-tag-find))
        ((string= n "add type constraint") (invoke-with-suggestion 'shm/modify-type-constraint))))



(defun invoke-with-suggestion (function)
  (funcall function)
  (let ((binding (where-is-internal function shm-map t)))
    (when binding
      (with-temp-message
          (format "You can run the command `%s' with %s"
                  function (key-description binding))
        (sit-for (if (numberp suggest-key-bindings)
                     suggest-key-bindings
                   2))))))

(provide 'shm-context-menu)
