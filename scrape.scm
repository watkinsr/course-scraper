(use-modules
 (web client)
 (web uri)
 (web response)
 (htmlprag)
 (sxml xpath)
 (sxml match)
 (ice-9 receive)
 (ice-9 binary-ports)
 (srfi srfi-1)
 (srfi srfi-9))

;; General Module data structure :: Record - url, urls, savedir
(define-record-type <module>
  (make-module url urls savedir)
  module?
  (url module-url)
  (urls module-urls)
  (savedir module-savedir))

(define basedir "/home/ryan/lectures/")

(define counter 0)
(define inc-counter!
  (lambda (x) (set! x (1+ x))))

(define extract-href
  (lambda (baseurl)
    (lambda (x)
      (map (lambda (x) (string-append baseurl (car (cdr (car (cdr (car (cdr x)))))))) x))))

(define force-get-html-body
  (lambda (proc)
    (receive (a b) (proc) b)))

(define get-sxml
  (lambda (x)
    (html->sxml (force-get-html-body
      (lambda () (http-get (string->uri x)))))))

(define form-module-hashmap
  (lambda ()
    (define h (make-hash-table))
    (hashq-set! h 'sicp
                (create-ocw-module "https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/lecture-notes/"))
    (hashq-set! h 'physics-1
      (create-ocw-module "https://ocw.mit.edu/courses/physics/8-01-physics-i-fall-2003/lecture-notes/"))
    h))

(define create-ocw-module
  (lambda (url)
    (make-module url (ocw-get-urls url) (get-course-savedir! url))))

(define m-href (lambda (x) (car (cdr (cdr (car (cdr x)))))))
(define m-url (lambda (xs)
  (map (lambda (x)
    (append-string "https://ocw.mit.edu" (car (cdr (m-href x)))) xs))))

(define ocw-get-courses
  (lambda ()
    (let* ((e-href (lambda (x) (car (cdr (cdr (car (cdr x)))))))
           (extractor (lambda (xs)
              (map (lambda (x)
                     (string-append "https://ocw.mit.edu" (car (cdr (m-href x))))) xs)))
           ;; (transform (lambda (x) ))
           (matcher (compose
            ;; (sxpath '(// (a (@ (href)))))
            ;; (sxpath '(// (td)))
            (sxpath '(// (tr)))
            (sxpath '(// (tbody)))
            (sxpath '(// (table (@ (equal? (class "courseList")))))))))
    ((compose
      ;; delete-duplicates
      ;; extractor
      matcher) (get-sxml "https://ocw.mit.edu/courses/")))))

(define x (ocw-get-courses))

(define ocw-get-urls
  (lambda (url)
    (let ((base "https://ocw.mit.edu")
        (matcher (lambda (sxml)
            ((compose
              (sxpath '(// (a (@ (href)))))
              (sxpath '(// (tr)))
              (sxpath '(// (div (@ (equal? (class "maintabletemplate"))))))) sxml))))
  ((compose (extract-href base) matcher get-sxml) url))))

(define fifth
  (lambda (xs) (car (cdr (cdr (cdr (cdr xs)))))))

(define sixth
  (lambda (xs) (car (cdr (cdr (cdr (cdr (cdr xs))))))))

(define get-course-savedir!
  (lambda (url)
    (let* ((xs (string-split url #\/))
           (type (fifth xs))
           (module (sixth xs))
           (save-type-dir (string-append basedir type "/"))
           (save-module-dir (string-append save-type-dir module "/")))
    (if (not (file-exists? save-type-dir)) (mkdir save-type-dir))
    (if (not (file-exists? save-module-dir)) (mkdir save-module-dir))
    save-module-dir)))

(define save-file
  (lambda (fname proc)
    (call-with-output-file fname
      (lambda (port)
        (put-bytevector port (force-get-html-body proc))))))

(define download-lectures
  (lambda (m)
    (let ((savedir (module-savedir m))
          (urls (module-urls m))
          (counter 1))
      (for-each
       (lambda (url)
         (display (string-append "File:" (number->string counter) " " url))
         (newline)
         (newline)
         (save-file
           (string-append savedir "lec" (number->string counter) ".pdf")
           (lambda () (http-get (string->uri url))))
         (set! counter (1+ counter)))
       urls))))

(define init
  (lambda ()
    (hash-fold
     (lambda (k v seed)
       (download-lectures v)) 0 (form-module-hashmap))
    (display "Finished downloading lectures!")
    (newline)))

;; (init)
