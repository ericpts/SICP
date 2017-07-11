(define (attach-tag type-tag datum)
  (cons type-tag datum))

(define (type-tag datum)
  (car datum))

(define (contents datum)
  (cadr datum))

(define (install-division-X-package)
  ;; internal procedures
  (define (get-record personnel-file employee-name)
    0)
  (define (get-personnel-file)
    0)

  (define (get-salary employee-record)
    0)
  ;; exported procedures
  (put 'get-personnel-file '(division X)
       (lambda () (attach-tag '(division X)
                              (get-personnel-file))))
  (put 'get-record '(division X)
       (lambda (personnel-file employee-name)
         (attach-tag '(division X)
                     (get-record personnel-file employee-name))))
  (put 'get-salary '(division X) get-salary))

(define (get-record personnel-file employee-name)
  (let ((proc (get 'get-record (type-tag personnel-file))))
    (if proc
        (proc (contents personnel-file)
              employee-name)
        (error "Invalid type tags:
               GET-RECORD: " (type-tag personel-file)))))

(define (get-salary employee-record)
  (let ((proc (get 'get-salary (type-tag employee-record))))
    (if proc
        (proc (contents employee-record))
        (error "Invalid type tags:
               GET-SALARY: " (type-tag employee-record)))))

(define (find-employee-record division-files employee-name )
  (if (null? division-files)
      '()
      (let ((record (get-record (car division-files)
                                employee-name)))
        (if record
            record
            (find-employee-record (cdr division-files)
                                  employee-name)))))
