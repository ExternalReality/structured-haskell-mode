;;; package --- Summary

;;; Commentary:
(require 'shm)

;;; Code:
(defun shm-get-refactors (current-node)
  "Get a vector of possible refactorings for the (CURRENT-NODE)."
  (shm-lint-ast "decl"
                (shm-node-start current-node)
                (shm-node-end current-node)))

(defun shm-get-parent-top-level-decl (node-pair)
  (shm-node-parent node-pair "Decl SrcSpanInfo"))

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

(defun shm-node-lambda-p (node)
  (string= (shm-node-cons node) "Lambda"))

(defun shm-refactors-available-p (refactors)
  "Check to see if the (REFACTORS) vector is populated."
  (if (> (length refactors) 0) t))


(provide 'shm-queries)
