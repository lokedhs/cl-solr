(in-package :cl-solr)

(declaim #.*compile-decl*)

(defun value-by-xpath (expression node &key (default-value nil default-value-assigned-p))
  (let ((result (xpath:evaluate expression node)))
    (if (xpath:node-set-empty-p result)
        (if default-value-assigned-p
            default-value
            (error "No value found for expression: ~s" expression))
        (dom:node-value (xpath:first-node result)))))

(defun debug-print-dom (doc &optional (stream *standard-output*))
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-character-stream-sink stream)) doc))
