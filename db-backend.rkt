                      #lang racket/base
(require racket/list
         db
         racket/trace)


(define (initialize-patientlist! dbname user)
  (define db (postgresql-connect #:user user
                                 #:database dbname))
  db)

(define (get-patients arg-db)
  (query-rows
   arg-db
   "SELECT id, lastname, firstname FROM donor;"))

(define (get-extended-patient arg-db arg-patient-id)
  (query-row
   arg-db
   (bind-prepared-statement (prepare arg-db
                                     "SELECT * FROM donor where id=($1);")
                            (list arg-patient-id)))
  )

(define (search-patient arg-db arg-patient-attribute-list)
  (define nullattrs (map (lambda (item) (if (string=? "" item) sql-null item)) arg-patient-attribute-list))
  (define nonnullcount (- (length nullattrs) (count sql-null? nullattrs)))
  (display nullattrs)
  (define querystring "SELECT * FROM donor")
  (define querylist (list " firstname like ($~a::varchar)" " lastname like ($~a::varchar)" " bloodtype~~~~($~a::varchar)"))
  (define argcount 1)
  (cond
    [(= 0 nonnullcount) (set! querystring (string-append querystring ";"))]
    [else
     (begin
       (set! querystring (string-append querystring " WHERE"))
       (for ([item (in-list nullattrs)]
           [j (in-range 0 4)])
       (if (sql-null? item)
           null
           (if (not (= 0 nonnullcount))
               (begin
                 (set! querystring (string-append querystring
                                                  (format (list-ref querylist j) argcount)))
                 
                 (if (= 1 nonnullcount)
                     null
                     (set! querystring (string-append querystring " and")))
                 (set! nonnullcount (- nonnullcount 1))
                 (set! argcount (+ argcount 1))
                 )
               (set! querystring (string-append querystring ";")))))
       )
     ])
               
  (display querystring)
  (print (remove* (list sql-null) nullattrs))
  (print (bind-prepared-statement (prepare arg-db
                                     querystring)
                            (remove* (list sql-null) nullattrs)))
  (query-rows
   arg-db
   (bind-prepared-statement (prepare arg-db
                                     querystring)
                            (remove* (list sql-null) nullattrs))))

(define (donor-search arg-db bloodtype)
  (define querystring "SELECT * FROM donor")
  (cond
    [(string=? "O-" bloodtype) (set! querystring (string-append querystring " WHERE bloodtype ~~ 'O-';"))]
    [(string=? "O+" bloodtype) (set! querystring (string-append querystring " WHERE bloodtype ~~ 'O+' or bloodtype ~~ 'O-';"))]
    [(string=? "A-" bloodtype) (set! querystring (string-append querystring " WHERE bloodtype ~~ 'O-' or bloodtype ~~ 'A-';"))]
    [(string=? "A+" bloodtype) (set! querystring (string-append querystring " WHERE bloodtype ~~ 'O+' or bloodtype ~~ 'A+' or bloodtype ~~ 'O-' or bloodtype ~~ 'A-';"))]
    [(string=? "B-" bloodtype) (set! querystring (string-append querystring " WHERE bloodtype ~~ 'B-' or bloodtype ~~ 'O-';"))]
    [(string=? "B+" bloodtype) (set! querystring (string-append querystring " WHERE bloodtype ~~ 'O+' or bloodtype ~~ 'B+' or bloodtype ~~ 'O-' or bloodtype ~~ 'B-';"))]
    [(string=? "AB-" bloodtype) (set! querystring (string-append querystring " WHERE bloodtype ~~ 'O-' or bloodtype ~~ 'A-' or bloodtype ~~ 'B-' or bloodtype ~~ 'AB-';"))]
    [(string=? "AB+" bloodtype) (set! querystring (string-append querystring ";"))])
  (query-rows
   arg-db
   querystring)
  )

(define (get-patient-bloodtype arg-db patient-id)
  (query-value
   arg-db
   (bind-prepared-statement (prepare arg-db
                                     "SELECT bloodtype FROM donor WHERE id=($1);")
                            (list (string->number patient-id)))))

(trace search-patient)
(trace donor-search)
(provide get-patients
         get-extended-patient
         get-patient-bloodtype
         search-patient
         donor-search
         initialize-patientlist!
         sql-null?)

