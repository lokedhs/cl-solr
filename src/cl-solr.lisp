(in-package :cl-solr)

(declaim #.*compile-decl*)

(defvar *solr-url-path* "http://localhost:8983/solr/collection1")

(defclass response ()
  ((num-found :type integer
              :initarg :num-found
              :reader response/num-found)
   (start     :type integer
              :initarg :start
              :reader response/start)
   (documents :type list
              :initarg :documents
              :reader response/documents))
  (:documentation "Class holding the search results"))

(defmethod print-object ((obj response) stream)
  (print-unreadable-safely (num-found start documents) obj stream
    (format stream "NUM-FOUND ~s START ~s DOCUMENTS ~s" num-found start (let ((v documents))
                                                                          (if (listp v)
                                                                              (length v)
                                                                              :not-bound)))))

(defun display-stream-if-debug (stream)
  (format *debug-io* "~&====== ERROR OUTPUT ======~%")
  (let ((input (flexi-streams:make-flexi-stream stream
                                                :external-format :UTF8
                                                :element-type 'character)))
    (loop
       for s = (read-line input nil nil)
       while s
       do (format *debug-io* "~a~%" s)))
  (format *debug-io* "~&====== END OF ERROR OUTPUT ======~%"))

(defun send-request (url &key (method :get) content parameters)
  (multiple-value-bind (content code return-headers url-reply stream need-close reason-string)
      (drakma:http-request (concatenate 'string *solr-url-path* url)
                           :method method
                           :content content
                           :content-type "application/xml"
                           :parameters (cons '("wt" . "xml") parameters)
                           :want-stream t
                           :force-binary t
                           :external-format-out :utf-8)
    (declare (ignore content return-headers url-reply))
    (unwind-protect
         (progn
           (unless (= code 200)
             (display-stream-if-debug stream)
             (error "Solr error: ~a. Reason=~a" code reason-string))
           (let ((doc (cxml:parse-stream stream (cxml-dom:make-dom-builder))))
             doc))
      (when need-close
        (close stream)))))

(defun make-field-node (doc key value)
  (let ((node (dom:create-element doc "field")))
    (dom:set-attribute node "name" key)
    (let ((text-node (dom:create-text-node doc value)))
      (dom:append-child node text-node))
    node))

(defun make-doc-node (doc data)
  (let ((solr-doc (dom:create-element doc "doc")))
    (loop
       for (key . value) in data
       do (dom:append-child solr-doc (make-field-node doc key value)))
    solr-doc))

(defun update-data (data &key soft-commit)
  (let* ((doc (cxml-dom:create-document))
         (add-node (dom:create-element doc "add")))
    (dolist (entry data)
      (dom:append-child add-node (make-doc-node doc entry)))
    (let ((commit-element (dom:create-element doc "commit")))
      (when soft-commit
        (dom:set-attribute commit-element "softCommit" "true"))
      (dom:append-child add-node commit-element))
    (dom:append-child doc add-node)
    (send-request "/update"
                  :method :post
                  :content #'(lambda (s)
                               (dom:map-document (cxml:make-namespace-normalizer (cxml:make-octet-stream-sink s :encoding :utf-8)) doc)))))

(defun delete-data (query)
  (let* ((doc (cxml-dom:create-document))
         (delete-node (dom:create-element doc "delete")))
    (dom:append-child doc delete-node)
    (let ((query-node (dom:create-element doc "query")))
      (dom:append-child query-node (dom:create-text-node doc query))
      (dom:append-child delete-node query-node))
    (send-request "/update"
                  :method :post
                  :parameters '(("commit" . "true"))
                  :content #'(lambda (s)
                               (dom:map-document (cxml:make-namespace-normalizer (cxml:make-octet-stream-sink s :encoding :utf-8)) doc)))))

(defun get-text-content-from-node (node)
  (if (dom:has-child-nodes node)
      (let ((text (dom:first-child node)))
        (unless (dom:text-node-p text)
          (error "Content node is not text"))
        (dom:node-value text))
      ;; No child nodes, return nil
      nil))

(defun parse-result-doc-node (result-node)
  (cons (dom:get-attribute result-node "name") ;(intern (dom:get-attribute result-node "name") "KEYWORD")
        (let ((text (get-text-content-from-node result-node)))
          (string-case:string-case ((dom:node-name result-node))
            ("str" text)
            ("long" (parse-integer text))
            ("date" text)))))

(defun parse-result-doc (doc-node)
  (loop
     with children = (dom:child-nodes doc-node)
     with length = (dom:length children)
     for i from 0 below length
     for node = (dom:item children i)
     when (dom:node-p node)
     collect (parse-result-doc-node node)))

(defun query-noparse (string &key parameters)
  (send-request "/select" :parameters (cons `("q" . ,string) parameters)))

(defun process-query-result (result)
  (let ((result-xpath-values (xpath:evaluate "/response/result[@name='response']" result)))
    (when (xpath:node-set-empty-p result-xpath-values)
      (error "No result in response document"))
    (let ((result-element (xpath:first-node result-xpath-values)))
      (make-instance 'response
                     :num-found (parse-integer (dom:get-attribute result-element "numFound"))
                     :start (parse-integer (dom:get-attribute result-element "start"))
                     :documents (loop
                                   with children = (dom:child-nodes result-element)
                                   with length = (dom:length children)
                                   for i from 0 below length
                                   for node = (dom:item children i)
                                   when (string= (dom:node-name node) "doc")
                                   collect (parse-result-doc node))))))

(defun query (string &key parameters)
  (let ((result (query-noparse string :parameters parameters)))
    (process-query-result result)))

(defun ping-solr-server ()
  (let ((result (send-request "/admin/ping")))
    (value-by-xpath "/response/str[@name='status']/text()" result :default-value nil)))
