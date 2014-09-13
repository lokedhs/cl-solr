(in-package :cl-solr)

(declaim #.*compile-decl*)

(defmacro print-unreadable-safely ((&rest slots) object stream &body body)
  "A version of PRINT-UNREADABLE-OBJECT and WITH-SLOTS that is safe to use with unbound slots"
  (let ((object-copy (gensym "OBJECT"))
        (stream-copy (gensym "STREAM")))
    `(let ((,object-copy ,object)
           (,stream-copy ,stream))
       (symbol-macrolet ,(mapcar #'(lambda (slot-name)
                                     `(,slot-name (if (and (slot-exists-p ,object-copy ',slot-name)
                                                           (slot-boundp ,object-copy ',slot-name))
                                                      (slot-value ,object-copy ',slot-name)
                                                      :not-bound)))
                                 slots)
         (print-unreadable-object (,object-copy ,stream-copy :type t :identity nil)
           ,@body)))))

(defun value-by-xpath (expression node &key (default-value nil default-value-assigned-p))
  (let ((result (xpath:evaluate expression node)))
    (if (xpath:node-set-empty-p result)
        (if default-value-assigned-p
            default-value
            (error "No value found for expression: ~s" expression))
        (dom:node-value (xpath:first-node result)))))

(defun debug-print-dom (doc &optional (stream *standard-output*))
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-character-stream-sink stream)) doc))
