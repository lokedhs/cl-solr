(asdf:defsystem #:cl-solr
  :description "Common Lisp interface to Apache Solr"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :serial t
  :depends-on (:drakma
               :alexandria
               :cxml
               :xpath
               :string-case)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (:file "misc")
                                     (:file "cl-solr")))))
