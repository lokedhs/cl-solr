(defpackage :cl-solr
  (:use :cl)
  (:documentation "Common Lisp interface to Apache Solr")
  (:export #:*solr-url-path*
           #:update-data
           #:query
           #:ping
           #:ping-solr-server
           #:query-noparse
           #:response/num-found
           #:response/start
           #:response/documents
           #:delete-data
           #:solr-response-error
           #:solr-error
           #:solr-response-error/code
           #:solr-response-error/text))

(in-package :cl-solr)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
