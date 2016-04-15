#lang racket

(require web-server/formlets
         web-server/servlet
         "db-backend.rkt")

(provide/contract (start (request? . -> . response?)))

;;
;; Support/helper functions
;;

;; What the Racket web server sends when you visit.
(define (start request)
  (define db (initialize-patientlist! "bloodbank" "nmg"))
  (render-patients-page db (get-patients db) request))

;; Maybe superfluous function to search user list.
(define (user-search arg-db arg-patient-attributes)
  (search-patient arg-db arg-patient-attributes))

;; Formlet for searching for a patient.
(define search-patient-formlet (formlet
                                (div "First Name:" ,{(to-string (default (string->bytes/utf-8 "") (text-input))) . => . fname}(br)
                                     "Last Name:" ,{(to-string (default (string->bytes/utf-8 "") (text-input))) . => . lname}(br)
                                     "Bloodtype: " ,{(select-input '("" "O+" "O-" "A+" "A-" "B+" "B-" "AB+" "AB-")) . => . bloodtype }
                                     )
                                (list fname lname bloodtype)))

;; Formlet for entering a new patient.
(define new-donor-formlet (formlet
                           "hello!";;formlet body todo
                           null;;formlet return values todo
                           ))

;; Formlet for editing an existing patient.
;(define edit-donor-formlet
(define (edit-donor-formlet [arg-patientinfo null])
  (if (null? arg-patientinfo)
      (formlet
       (div ((class "patientupdate"))
            "First Name:" ,{(to-string (default (string->bytes/utf-8 "null") (text-input))) . => . fname}(br)        
            "Last Name:" ,{(to-string (default (string->bytes/utf-8 "null") (text-input))) . => . lname}(br)
            "Bloodtype:" ,{(select-input '("O+" "O-" "A+" "A-" "B+" "B-" "AB+" "AB-")) . => . bloodtype}(br)
            "Address:" ,{(to-string (default (string->bytes/utf-8 "null") (text-input))) . => . address}
            )                           
       (list fname lname bloodtype address))
      (formlet
       (div ((class "patientupdate"))
            "First Name:" ,{(to-string (default (string->bytes/utf-8 (list-ref arg-patientinfo 1)) (text-input #:value (string->bytes/utf-8 (list-ref arg-patientinfo 1))))) . => . fname}(br)        
            "Last Name:" ,{(to-string (default (string->bytes/utf-8 (list-ref arg-patientinfo 2)) (text-input #:value (string->bytes/utf-8 (list-ref arg-patientinfo 2))))) . => . lname}(br)
            "Bloodtype:" ,{(select-input '("O+" "O-" "A+" "A-" "B+" "B-" "AB+" "AB-") #:selected? (lambda (value) (string=? (list-ref arg-patientinfo 3) value))) . => . bloodtype}(br)
            "Address:" ,{(to-string (default (string->bytes/utf-8 (list-ref arg-patientinfo 4)) (text-input #:value (string->bytes/utf-8 (list-ref arg-patientinfo 4))))) . => . address}
                                                                                                        )
       (list fname lname bloodtype address))
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
      [(exists-binding? 'donors (request-bindings request)) (donor-search-handler request)]
      [(exists-binding? 'update (request-bindings request)) (update-donor-handler request)]
      [(exists-binding? 'delete (request-bindings request)) (delete-donor-handler request)]))
  (define (user-search-handler request)
    (define patientattrs
      (formlet-process search-patient-formlet request))
    (render-patients-page arg-db (user-search arg-db patientattrs) request))
  (define (donor-search-handler request)
    (show-donor-list arg-db
                        (donor-search arg-db (get-patient-bloodtype arg-db (get-patient-id (request-bindings request))))
                        (get-patient-id (request-bindings request))
                        request))
  (define (update-donor-handler request)
    (update-donor arg-db
                  (string->number (get-patient-id (request-bindings request)))
                  request))
  (define (delete-donor-handler request)
    (delete-donor arg-db
                  (string->number (get-patient-id (request-bindings request)))
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
                        (input ((type "submit") (value "View more details") (name "details")))(br)
                        (input ((type "submit") (value "Find compatible donors") (name "donors")))(br)
                        (input ((type "submit") (value "Update this patient") (name "update")))(br)
                        (input ((type "submit") (value "Delete this patient") (name "delete")))(br)
                        (tr (th "Select")
                            (th "Last Name")
                            (th "First Name"))
                        ,@(map render-patient arg-patients)))))

;; Separate page that renders a single donor's full details.
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
              ,(render-extended-patient arg-patients)
             (a ((href ,(embed/url back-handler))) "Back to all patients")))))
  (define (back-handler request)
    (render-patients-page arg-db (get-patients arg-db) request))
  (send/suspend/dispatch response-generator))

;; A very similar page to show-user-details, but with better accessibility, and for displaying the compatible donor list for a given patient.
(define (show-donor-list arg-db arg-patients arg-recipient-patient-id request)
  (define (render-extended-patient arg-patients)
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
    (define patientinfo (get-extended-patient arg-db (string->number arg-recipient-patient-id)))
    (define fname (vector-ref patientinfo 1))
    (define lname (vector-ref patientinfo 2))
    (response/xexpr
     `(html (head (title "Compatible Donor Details"))
            (body
             (h1 ,(format "Donor matches for ~a ~a, ID #~a" fname lname arg-recipient-patient-id))
             (a ((href ,(embed/url back-handler))) "Back to all patients")
              ,(render-extended-patient arg-patients)
             (a ((href ,(embed/url back-handler))) "Back to all patients")))))
  (define (back-handler request)
    (render-patients-page arg-db (get-patients arg-db) request))
  (send/suspend/dispatch response-generator))

;; Separate page to host the new donor formlet.
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

;; Separate page to host the update donor formlet. Populates the input boxes with the details already there.
(define (update-donor arg-db arg-patient-id request)
  (define (back-handler request)
    (render-patients-page arg-db (get-patients arg-db) request))
  (define (submit-changes-handler request)
    (update-donor-backend arg-db arg-patient-id (formlet-process (edit-donor-formlet null) request))
    (render-patients-page arg-db (get-patients arg-db) (redirect/get)))
  (define (response-generator embed/url)
    (define patientinfo (map (lambda (item) (if (sql-null? item) "null" item)) (vector->list (get-extended-patient arg-db arg-patient-id))))
    (response/xexpr
     `(html (head (title "Editing Donor Details"))
            (h1 ,(format "Editing details for ~a ~a" (list-ref patientinfo 2) (list-ref patientinfo 1)))
            (form ([action ,(embed/url submit-changes-handler)])            
                  ,@(formlet-display (edit-donor-formlet patientinfo))
                  (input ([type "submit"])))
            (a ((href ,(embed/url back-handler))) "Abort and go back")
       )))
  (send/suspend/dispatch response-generator))

(define (delete-donor arg-db arg-patient-id request)
  (define (yes-handler request)
    (delete-donor-backend arg-db (number->string arg-patient-id))
    (render-patients-page arg-db (get-patients arg-db) (redirect/get)))  
  (define (no-handler request)
    (render-patients-page arg-db (get-patients arg-db) request))
  (define (response-generator embed/url)
    (define patientinfo (vector->list (get-patient-fullname arg-db (number->string arg-patient-id))))
    (response/xexpr
     `(html (head (title "Delete Patient"))
            ,(format "Delete patient #~a, ~a ~a?" arg-patient-id (list-ref patientinfo 0) (list-ref patientinfo 1))
            (br)
            (a ((href ,(embed/url yes-handler))) "Yes, delete donor")
            (br)
            (a ((href ,(embed/url no-handler))) "No, nevermind"))
     )
    )
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
