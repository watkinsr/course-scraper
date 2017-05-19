(use-modules
 (web client)
 (web uri)
 (web response)
 (htmlprag)
 (sxml xpath)
 (ice-9 receive)
 (ice-9 binary-ports))

(define pdf-regexp (make-regexp "pdf"))
(define m-lectures
  (lambda (sxml)
    ((compose
      (sxpath '(// (a (@ (href)))))
      (sxpath '(// (tr)))
      (sxpath '(// (div (@ (equal? (class "maintabletemplate"))))))) sxml)))

(define sicp-base-url "https://ocw.mit.edu")
(define sicp-lecture-root "/home/ryan/lectures/")
(define counter 0)
(define inc*
  (lambda ()
    (set! counter (1+ counter)) counter))

(define get-lecture-name
  (lambda (root)
    (string-append root "lec" (number->string (inc*)) ".pdf")))

(define force-get-html-body
  (lambda (proc)
    (receive (a b) (proc) b)))

(define sicp-sxml
  (html->sxml (force-get-html-body (lambda ()
                                     (http-get (string->uri (string-append sicp-base-url "/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/lecture-notes/")))))))

(define extract-urls
  (lambda (x)
    (map (lambda (x) (string-append sicp-base-url (car (cdr (car (cdr (car (cdr x)))))))) x)))

(define lecture-urls (extract-urls (m-lectures sicp-sxml)))

(define download-lectures
  (lambda (rootdir urls)
    (for-each
     (lambda (x)
      (save-lecture
        (get-lecture-name rootdir)
        (lambda () (http-get (string->uri x)))))
     urls)))

(define save-lecture
  (lambda (fname proc)
    (call-with-output-file fname
      (lambda (port)
        (put-bytevector port (force-get-html-body proc))))))

(download-lectures sicp-lecture-root lecture-urls)
(display "Finished downloading lectures!")
