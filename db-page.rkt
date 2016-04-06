#lang racket

(require web-server/formlets
         web-server/servlet
         "db-backend.rkt")

(provide/contract (start (request? . -> . response?)))

(define (start request)
  (render-patients-page (initialize-patientlist! "bloodbank" "nmg") request))

(define (render-patients-page arg-db request)
  (response/xexpr
   `(html (head (title "Patient List"))
          (body
           (h1 ((class "titlehead")) "Patient Display")
           ,(display-patients arg-db (get-patients arg-db))))))

(define search-patient-formlet (formlet
                                (div "First Name:" ,{(to-string (default (string->bytes/utf-8 "") (text-input))) . => . fname}(br)
                                     "Last Name:" ,{(to-string (default (string->bytes/utf-8 "") (text-input))) . => . lname}(br)
                                     "Bloodtype: " ,{(select-input '("" "O+" "O-" "A+" "A-" "B+" "B-" "AB+" "AB-")) . => . bloodtype }
                                     )
                                (list fname lname bloodtype)))

(define (display-patients arg-db arg-patients)
  (define (response-generator embed/url)
    (response/xexpr
     `(div ((class "patientlist"))
           ;; (form ([action
           ;;         ,(embed/url show-user-details-handler)])
           ;;       ,@(formlet-display select-patient-formlet)
           ;;       (input ([type "submit"])))
           (form ([action
                   ,(embed/url user-search-handler)])
                 ,@(formlet-display search-patient-formlet)
                 (input ([type "submit"]
                         [value "Search for patient"])))
           
           (form ((action
                   ,(embed/url show-user-details-handler)))
                 (table ((style "overflow:scroll;"))
                        (input ((type "submit") (value "View more details")))
                        (tr (th "Select")
                            (th "Last Name")
                            (th "First Name"))
                        ,@(map render-patient arg-patients))
                 ))
     )
    )
  (define (user-search-handler request)
    (define patientattrs
      (formlet-process search-patient-formlet request))
    (display-patients arg-db (user-search arg-db patientattrs)))
  (define (get-patient-id bindings)
    (extract-binding/single 'patientbutton bindings))
  (define (show-user-details-handler request)
    (show-user-details arg-db
                       (get-extended-patient arg-db
                                                    (string->number (get-patient-id (request-bindings request))))
                       request))
  (define (render-patient patient)
    `(tr (td (input ((type "radio") (name "patientbutton") (value ,(number->string (vector-ref patient 0))))))             
         (td ,(vector-ref patient 1))
         (td ,(vector-ref patient 2))))
  (send/suspend/dispatch response-generator))

(define (user-search arg-db arg-patient-attributes)
  (search-patient arg-db arg-patient-attributes)
  )

(define (show-user-details arg-db arg-patient request)
  (define (render-extended-patient arg-patient)
    ;; (define patientinfo (get-extended-patient arg-db arg-patient))
    `(table ((style "width:75%"))
            (tr (th "Last Name")
                (th "First Name")
                (th "Blood Type")
                (th "Address")            
                (th "Known Diseases")
                (th "Tests Performed")
                (th "Date of Last Donation")
                (th "Phone Number"))
             (tr ,@(for/list ([i (in-range 1 9)])
                     `(td ,(if (sql-null? (vector-ref arg-patient i))
                               "null"
                               (vector-ref arg-patient i))
                             )))
            )
    )
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Patient Details"))
            (body
             ,(render-extended-patient arg-patient)
             (a ((href ,(embed/url back-handler))) "Back to all patients")))))
  (define (back-handler request)
    (render-patients-page arg-db request))
  (send/suspend/dispatch response-generator))

(require web-server/servlet-env)
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 3000
               #:extra-files-paths (list (build-path "static"))
               #:servlet-path "/servlets/db-page.rkt")
