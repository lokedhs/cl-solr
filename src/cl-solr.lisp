(in-package :cl-solr)

(declaim #.*compile-decl*)

(defvar *solr-url-path* "http://localhost:8983/solr/collection1")

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

(defun update-data (data)
  (let* ((doc (cxml-dom:create-document))
         (add-node (dom:create-element doc "add")))
    (dolist (entry data)
      (dom:append-child add-node (make-doc-node doc entry)))
    (dom:append-child doc add-node)
    (send-request "/update"
                  :method :post
                  :content #'(lambda (s)
                               (dom:map-document (cxml:make-namespace-normalizer (cxml:make-octet-stream-sink s :encoding :utf-8)) doc)))))

(defun query (string)
  (send-request "/select"
                :method :get
                :parameters `(("q" . ,string))))

(defun ping-solr-server ()
  (let ((result (send-request "/admin/ping")))
    (value-by-xpath "/response/str[@name='status']/text()" result :default-value nil)))
