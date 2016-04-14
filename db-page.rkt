#lang racket

(require web-server/formlets
         web-server/servlet
         "db-backend.rkt")

(provide/contract (start (request? . -> . response?)))

;;
;; Support/helper functions
;;

(define (start request)
  (define db (initialize-patientlist! "bloodbank" "nmg"))
  (render-patients-page db (get-patients db) request))

(define (user-search arg-db arg-patient-attributes)
  (search-patient arg-db arg-patient-attributes))

(define search-patient-formlet (formlet
                                (div "First Name:" ,{(to-string (default (string->bytes/utf-8 "") (text-input))) . => . fname}(br)
                                     "Last Name:" ,{(to-string (default (string->bytes/utf-8 "") (text-input))) . => . lname}(br)
                                     "Bloodtype: " ,{(select-input '("" "O+" "O-" "A+" "A-" "B+" "B-" "AB+" "AB-")) . => . bloodtype }
                                     )
                                (list fname lname bloodtype)))

(define new-donor-formlet (formlet
                           "hello!";;formlet body todo
                           null;;formlet return values todo
                           ))

;;
;; Functions responsible for rendering pages
;;

;; Main-page rendering function
(define (render-patients-page arg-db arg-patients request)
  (define (insert-donor-handler request)
    (insert-donor arg-db request))
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Patient List"))
            (body
             (h1 ((class "titlehead")) "Patient Display")
             (a ((href ,(embed/url insert-donor-handler))) "Add new donor")
             ,(display-patients arg-db arg-patients embed/url)))))
  (send/suspend/dispatch response-generator))

;; Displays the aggregate patient list, for viewing and searching for patients.
(define (display-patients arg-db arg-patients embed/url)
  (define (patient-list-handler request)
    (cond
      [(exists-binding? 'details (request-bindings request)) (show-user-details-handler request)]
      [(exists-binding? 'donors (request-bindings request)) (donor-search-handler request)]))  
  (define (user-search-handler request)
    (define patientattrs
      (formlet-process search-patient-formlet request))
    (render-patients-page arg-db (user-search arg-db patientattrs) request))
  (define (donor-search-handler request)
    (show-user-details arg-db
                       (donor-search arg-db (get-patient-bloodtype arg-db (get-patient-id (request-bindings request))))
                       request))
  (define (get-patient-id bindings)
    (extract-binding/single 'patientbutton bindings))
  (define (show-user-details-handler request)
    (show-user-details arg-db
                       (list (get-extended-patient arg-db
                                                   (string->number (get-patient-id (request-bindings request)))))
                       request))
  (define (render-patient patient)
    `(tr (td (input ((type "radio") (name "patientbutton") (value ,(number->string (vector-ref patient 0))))))             
         (td ,(vector-ref patient 1))
         (td ,(vector-ref patient 2))))
  ;; Function body to render list of patients
  `(div ((class "patientlist"))
        
           (form ([action
                   ,(embed/url user-search-handler)])
                 ,@(formlet-display search-patient-formlet)
                 (input ([type "submit"]
                         [value "Search for patient"])))
           
           (form ((action
                   ,(embed/url patient-list-handler)))
                 (table ((style "overflow:scroll;"))
                        (input ((type "submit") (value "View more details") (name "details")))
                        (input ((type "submit") (value "Find compatible donors") (name "donors")))
                        (tr (th "Select")
                            (th "Last Name")
                            (th "First Name"))
                        ,@(map render-patient arg-patients)))))

(define (show-user-details arg-db arg-patients request)
  (define (render-extended-patient arg-patients)
    ;; (define patientinfo (get-extended-patient arg-db arg-patients))
    `(table ((style "width:75%"))
            (tr (th "Last Name")
                (th "First Name")
                (th "Blood Type")
                (th "Address")            
                (th "Known Diseases")
                (th "Tests Performed")
                (th "Date of Last Donation")
                (th "Phone Number"))
            ,@(map (lambda (patient)
                     `(tr ,@(for/list ([i (in-range 1 9)])
                             `(td ,(if (sql-null? (vector-ref patient i))
                                       "null"
                                       (vector-ref patient i))))))
                   arg-patients)))
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Patient Details"))
            (body
             (a ((href ,(embed/url back-handler))) "Back to all patients")
              ,(render-extended-patient arg-patients)
             (a ((href ,(embed/url back-handler))) "Back to all patients")))))
  (define (back-handler request)
    (render-patients-page arg-db (get-patients arg-db) request))
  (send/suspend/dispatch response-generator))

(define (insert-donor arg-db request)
  (define (back-handler request)
    (render-patients-page arg-db (get-patients arg-db) request))
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Adding New Donor"))
            (body
             (h1 "Inserting New Donor")
             (form ([action
                     ;;action consequence
                     ""])
                   ,@(formlet-display new-donor-formlet)
                   (input ([type "submit"])))
             (a ((href ,(embed/url back-handler))) "Abort and go back")))
     ))
  (send/suspend/dispatch response-generator))

;;
;; Making this file runnable as a servlet stuff
;;

(require web-server/servlet-env)
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 3000
               #:extra-files-paths (list (build-path "static"))
               #:servlet-path "/servlets/db-page.rkt")
