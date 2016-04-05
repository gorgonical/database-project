#lang racket/base
(require racket/list
         db)

(define (initialize-patientlist! dbname user)
  (define db (postgresql-connect #:user user
                                 #:database dbname))
  db)

(define (get-patients arg-db)
  (query-rows
   arg-db
   "SELECT id, firstname, lastname FROM donor;"))

(define (get-extended-patient arg-db arg-patient-id)
  (query-row
   arg-db
   (bind-prepared-statement (prepare arg-db
                                     "SELECT * FROM donor where id=($1);")
                            (list arg-patient-id)))
  )

(define (search-patient arg-db arg-patient-attribute-list)
  ;(define wildcardattrs (map (lambda (item) (string-append "%" item "%")) arg-patient-attribute-list))
  ;(set! wildcardattrs (list (car wildcardattrs)))
  (query-rows
   arg-db
   (bind-prepared-statement (prepare arg-db
                                     "SELECT * FROM donor WHERE firstname like ($1::varchar) or lastname like ($2::varchar) or bloodtype like ($3::varchar) or phone like ($4::char);")
                                     ;"SELECT * FROM donor WHERE firstname like ($1::varchar);")
                            ;wildcardattrs)))
                            arg-patient-attribute-list)))

(provide get-patients
         get-extended-patient
         search-patient
         initialize-patientlist!
         sql-null?)

