(defpackage :cl-solr
  (:use :cl)
  (:documentation "Common Lisp interface to Apache Solr")
  (:export
   #:*solr-url-path*
   #:update-data
   #:query))

(in-package :cl-solr)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
