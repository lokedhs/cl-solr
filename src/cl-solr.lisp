(in-package :cl-solr)

(declaim #.*compile-decl*)

(defvar *solr-url-path* "http://localhost:8983/solr/collection1")

(defun send-request (url)
  (multiple-value-bind (content code return-headers url-reply stream need-close reason-string)
      (drakma:http-request (concatenate 'string *solr-url-path* url)
                           :parameters '(("wt" . "xml"))
                           :want-stream t)
    (declare (ignore content return-headers url-reply))
    (unwind-protect
         (progn
           (unless (= code 200)
             (error "Solr error: ~a. Reason=~a" code reason-string))
           (let ((doc (cxml:parse-stream stream (cxml-dom:make-dom-builder))))
             doc))
      (when need-close
        (close stream)))))

(defun make-xml-doc ())

(defun foo ()
  )

(defun debug-print-dom (doc &optional (stream *standard-output*))
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-character-stream-sink stream)) doc))
