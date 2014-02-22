;;; package --- Summary

;;; Commentary:
(require 'shm)

(defconst query-program-name "haskell-src-query")

(defun haskell-src-query (query node &optional filePath)
  (let ((message-log-max nil)
        (end (shm-node-end node))
        (start (shm-node-start node))
        (buffer (current-buffer))
        (srcPath (if filePath filePath "")))
    (when (> end (1+ start))
      (with-temp-buffer
        (let ((temp-buffer (current-buffer)))
          (with-current-buffer buffer
            (condition-case e
                (call-process-region start end
                                     query-program-name
                                     nil
                                     temp-buffer
                                     nil
                                     query srcPath)
              ((file-error)
               (error "cannot find haskell-src-query executable")))))
        (read (buffer-string))))))

(defmacro shm-query (name query)
  `(defun ,name (node &optional filePath)
     (haskell-src-query ,query node filePath)))

;;; Code:
(defun shm-get-refactors (node)
  "Get a vector of possible refactorings for the (CURRENT-NODE)."
  (shm-lint-ast "decl"
                (shm-node-start node)
                (shm-node-end node)))

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

(provide 'shm-query)
