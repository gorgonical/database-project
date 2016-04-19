                      #lang racket/base
(require racket/list
         db
         db/util/postgresql
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

(define (get-patient-fullname arg-db patient-id)
  (query-row
   arg-db
   (bind-prepared-statement (prepare arg-db
                                     "SELECT firstname, lastname FROM donor WHERE id=($1);")
                            (list (string->number patient-id)))))

;; Expected order of update-values: fname, lname, bloodtype, address, tests, diseases
(define (update-donor-backend arg-db arg-patient-id update-values)
  ;; convert "null" to <sql-null>
  (define nullvals (map (lambda (value) (if (and (not (pg-array? value)) (string=? "null" value)) sql-null value)) update-values))
  (query-exec
   arg-db
   (bind-prepared-statement (prepare arg-db
                                     "UPDATE donor SET firstname = $1, lastname = $2, bloodtype = $3, address = $4, testsdone = $5, knowndiseases = $6 where id=$7;")
                            (flatten (list nullvals arg-patient-id)))))

(define (delete-donor-backend arg-db arg-patient-id)
  (query-exec
   arg-db
   (bind-prepared-statement (prepare arg-db
                                     "DELETE FROM donor WHERE id=$1;")
                            (list (string->number arg-patient-id)))))

;; Order for donor-values is: last name, first name, blood type, address, tests done, known diseases, date of last donation, phone number
(define (new-donor-backend arg-db donor-values)
  (define nullvals (map (lambda (value) (if (or (and (pg-array? value)
                                                     (= 0 (pg-array-dimensions value)))
                                                (and (not (pg-array? value))
                                                     (string=? "null" value))) sql-null value)) donor-values))
  (query-exec
   arg-db
   (bind-prepared-statement (prepare arg-db
                                     "INSERT INTO donor VALUES(default, $1, $2, $3, $4, $5, $6, $7, $8);")
                            nullvals)))

(trace search-patient)
(trace donor-search)
(trace update-donor-backend)
(trace new-donor-backend)
(provide get-patients
         get-extended-patient
         get-patient-bloodtype
         get-patient-fullname
         update-donor-backend
         delete-donor-backend
         new-donor-backend
         search-patient
         donor-search
         initialize-patientlist!
         sql-null?
         pg-array->list
         pg-array?
         pg-array-dimensions
         list->pg-array)

