(require 'shm)
(require 'shm-test)
(require 'popup)

(if (eq window-system 'x)
    (define-key shm-map (kbd "M-<return>") 'shm/present-actions-for-node)
  (define-key shm-map (kbd "M-]") 'shm/present-actions-for-node))

(defun shm-node-syntax-contains-regex (regex node)
  "check the syntax of a node for an occurrence of pattern"
  (let ((node-concrete-syntax (shm-concrete-syntax-for-node node)))
    (string-match-p regex node-concrete-syntax)))

(defun shm-concrete-syntax-for-node (node)
  (buffer-substring-no-properties 
   (shm-node-start (shm-current-node))
   (shm-node-end (shm-current-node))))

(defun shm/modify-type-constraint ()
  "Modify a type signitures constraint"
  (interactive)
  (let* ((pair (shm-current-node-pair))
         (current-node (cdr pair)))         
    (if (shm-type-declaration-with-constraint-p pair)
        (shm-add-additional-type-constraint current-node)
      (add-initial-type-constraint current-node))))

 (defun shm-constraint-has-parens-p (node)
   (let* ((syntax (shm-concrete-syntax-for-node node))
          (constraint-syntax (car (split-string syntax "=>"))))
     (string-match-p ")" constraint-syntax)))


(defun shm-add-additional-type-constraint (node)
  (if (shm-constraint-has-parens-p node)
      (progn
        (shm-goto-end-of-constraint node)
        (backward-char 1)
        (insert ", "))
    (goto-char (shm-node-start node))
    (insert "(")
    (shm-goto-end-of-constraint node)
    (insert ", )")            
    (backward-char 1)))

(defun add-initial-type-constraint (node)
  (goto-char (shm-node-start node))
  (insert " => ") (backward-char 4))

(defun shm-type-declaration-with-constraint-p (pair)
  (let ((current-node (cdr pair)))
    (and (shm-top-level-type-decl-p pair)
         (shm-node-syntax-contains-regex "=>" current-node))))

(defun shm-goto-end-of-constraint (node)
  "Set point to the first white-space character between the end of the type constraint and the '=>'"
  (goto-char (+ (shm-node-start node) (shm-node-syntax-contains-regex "=>" node)))
  (re-search-backward "^\\|[^[:space:]]") (goto-char (+ (point) 1)))

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

(defun shm-type-declaration-with-constraint-p (pair)
  (let ((current-node (cdr pair)))
    (and (shm-top-level-type-decl-p pair)
         (shm-node-syntax-contains-regex "=>" current-node))))


(defun shm-top-level-type-decl-p (node-pair)
  (let ((current-node (cdr node-pair)))
    (if (and (not (shm-has-parent-with-matching-type-p node-pair))
             (string= "Type SrcSpanInfo" (shm-node-type current-node))) t)))

(defun add-initial-type-constraint (node)
  (goto-char (shm-node-start node))
  (insert " => ") (backward-char 4))


(defun shm-has-parent-with-matching-type-p (node-pair)
  (let* ((current (cdr node-pair))
         (parent-pair (shm-node-parent node-pair (shm-node-type current)))
         (parent (cdr parent-pair)))
    (if parent
        (if (string= (shm-node-type current)
                     (shm-node-type parent)) t))))


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
