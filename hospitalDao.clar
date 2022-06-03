;; Hosptital DAO
;; 

;; errors
;;
(define-constant UNAUTH_CALLER (err u200))
(define-constant AUTH_CALLER_ALREADY_EXISTS (err u201))
(define-constant AUTH_CALLER_DOESNT_EXIST (err u202))
(define-constant PATIENT_ALREADY_EXISTS (err u203))
(define-constant PATIENT_DOESNT_EXISTS (err u204))
(define-constant INVALID_GENDER (err u205))
(define-constant FULL_APP (err u206))
(define-constant EMPTY_APP (err u207))
(define-constant APP_ALREADY_EXISTS (err u208))
(define-constant APP_DOESNT_EXISTS (err u209))

;; constants
;;
(define-constant admin 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)

(define-constant radiology_principal 'STNHKEPYEPJ8ET55ZZ0M5A34J0R3N5FM2CMMMAZ6) ;;wallet 9
(define-constant dentist_principal 'ST3PF13W7Z0RRM42A8VZRVFQ75SV1K26RXEP8YGKJ) ;; wallet 7
(define-constant general_physician_principal 'ST3NBRSFKX28FQ2ZJ1MAKX58HKHSDGNV5N7R21XCP) ;; wallet 8


;; data maps and vars
;;
(define-map authorised_callers {address: principal} {authorised: bool})
(map-set authorised_callers {address: 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM} {authorised: true})

(define-map patient_data {id: principal} {name: (string-ascii 20), gender: (string-ascii 1), age: uint, contactNo: uint})


(define-data-var radiology_app_number uint u0)
(define-data-var dentist_app_number uint u0)
(define-data-var general_physician_app_number uint u0)

(define-map radiology {appointment_number: uint} {id: principal, time: uint, date: (string-ascii 10)} )
(define-map dentist {appointment_number: uint} {id: principal, time: uint, date: (string-ascii 10)} )
(define-map general_physician {appointment_number: uint} {id: principal, time: uint, date: (string-ascii 10)} )


;; private functions
;;
(define-private (is_auth_caller)

(if (is-eq (get authorised (unwrap! (map-get? authorised_callers {address: tx-sender}) false)) true)

true

false
) 


)
(define-private (is_valid_gender (gender (string-ascii 1)) )

(if (is-eq gender "m") true

(if (is-eq gender "f") true 

(if (is-eq gender "o") true

false)))

)

(define-private (app_fee_transfer (recipient principal)) 

(stx-transfer?  u99 tx-sender recipient)

)
;; public functions
;;

(map-set authorised_callers {address: 'ST3NBRSFKX28FQ2ZJ1MAKX58HKHSDGNV5N7R21XCP } {authorised: true})
(map-set authorised_callers {address: 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5 } {authorised: false})


(define-public (add_authosrised_caller (address principal) (authorised bool))
(begin

;; check if the funtion is called by admin or not
(asserts! (is-eq tx-sender admin) UNAUTH_CALLER)

;; checks if auth caller already exists or not 
(asserts! (is-eq (is-none (map-get? authorised_callers {address: address})) true) AUTH_CALLER_ALREADY_EXISTS)

(ok (map-set authorised_callers {address: address} {authorised: authorised}))

)
)

(define-public (delete_authosrised_caller (address principal) (authorised bool))
(begin

;; check if the funtion is called by admin or not
(asserts! (is-eq tx-sender admin) UNAUTH_CALLER)

;; checks if auth caller already exists or not 
(asserts! (is-eq (is-some (map-get? authorised_callers {address: address})) true) AUTH_CALLER_DOESNT_EXIST)

(ok (map-set authorised_callers {address: address} {authorised: authorised}))

)
)


(define-public (add_patient_data (id principal) (name (string-ascii 20)) (gender (string-ascii 1)) (age uint) (contactNo uint))
(begin 

;; only allows authorised users to add patient data
(asserts! (is-eq (is_auth_caller) true) UNAUTH_CALLER)

;; checks if patient records already exists or not 
(asserts! (is-eq (is-none (map-get? patient_data {id: id})) true) PATIENT_ALREADY_EXISTS)

;; checks if a valid gender option is entered
(asserts! (is_valid_gender gender) INVALID_GENDER)

(ok (map-set patient_data {id: id} {name: name, gender: gender, age: age, contactNo: contactNo}))

)
)

(define-public (edit_patient_data (id principal) (name (string-ascii 20)) (gender (string-ascii 1)) (age uint) (contactNo uint))
(begin 

;; only allows authorised users to add patient data
(asserts! (is-eq (is_auth_caller) true) UNAUTH_CALLER)

;; checks if patient records already exists or not 
(asserts! (is-eq (is-some (map-get? patient_data {id: id})) true) PATIENT_DOESNT_EXISTS)

(ok (map-set patient_data {id: id} {name: name, gender: gender, age: age, contactNo: contactNo}))

)
)

(define-public (delete_patient_data (id principal))
(begin 

;; only allows authorised users to add patient data
(asserts! (is-eq (is_auth_caller) true) UNAUTH_CALLER)

;; checks if patient records already exists or not 
(asserts! (is-eq (is-some (map-get? patient_data {id: id})) true) PATIENT_DOESNT_EXISTS)

(ok (map-delete patient_data {id: id}))

)
)



(define-public (set-appointment (app_num uint) (name principal) (department uint) (date (string-ascii 10)) (time uint ))

    (begin
        ;; makes sure patient is already registered
        (asserts! (is-eq (is-some (map-get? patient_data {id: name}) ) true) PATIENT_DOESNT_EXISTS) 
        
        ;; nested if statements to check which department is selected
        ;; if-1
        (if (is-eq department u0)

        ;; if-1 TRUE
        (begin
            ;; checks if whether appointments are already 20 or not
            (asserts! (> u20 (var-get radiology_app_number )) FULL_APP)

            ;; checks if specific appointment number is already occupied or not
            (asserts!  (is-eq (is-none (map-get? radiology {appointment_number: app_num})) true) APP_ALREADY_EXISTS)

            
            (map-set radiology {appointment_number: app_num} {id: name, time: time, date: date})
            (var-set radiology_app_number (+ (var-get radiology_app_number) u1))
            (app_fee_transfer radiology_principal)
            
        )
        ;; if-1 FALSE
        ;; if-2
        (if (is-eq department u1)

        ;; if-2 TRUE
        (begin 
            ;; checks if whether appointments are already 20 or not
            (asserts! (> u20 (var-get dentist_app_number )) FULL_APP)

            ;; checks if specific appointment number is already occupied or not
            (asserts!  (is-eq (is-none (map-get? dentist {appointment_number: app_num})) true) APP_ALREADY_EXISTS)

            (map-set dentist {appointment_number: app_num} {id: name, time: time, date: date})
            (var-set dentist_app_number (+ (var-get dentist_app_number) u1))
            (app_fee_transfer dentist_principal)
        )
        ;; if-3 FALSE
        ;; if-3
        (if (is-eq department u2)

        ;; if-3 TRUE
        (begin  
            ;; checks if whether appointments are already 20 or not
            (asserts! (> u20 (var-get general_physician_app_number )) FULL_APP)

            ;; checks if specific appointment number is already occupied or not
            (asserts!  (is-eq (is-none (map-get? general_physician {appointment_number: app_num})) true) APP_ALREADY_EXISTS)

            (map-set general_physician {appointment_number: app_num} {id: name, time: time, date: date})
            (var-set general_physician_app_number (+ (var-get general_physician_app_number) u1))
            (app_fee_transfer general_physician_principal)
        )
        ;; if-3 FALSE
            (err u210)
        ) ;; if-3 END
        ) ;; if-2 END
        ) ;; if-1 END

    )
)

(define-public (delete-appointment (department uint) (app_num uint))

    (begin
        ;; nested if statements to check which department is selected
        ;; if-1
        (if (is-eq department u0)
        
        ;; if-1 TRUE
        (begin

            (asserts! (< u0 (var-get radiology_app_number )) EMPTY_APP)
            (asserts!  (is-eq (is-some (map-get? radiology {appointment_number: app_num})) true) APP_DOESNT_EXISTS)

            (map-delete radiology {appointment_number: app_num})
            (var-set radiology_app_number (- (var-get radiology_app_number) u1))
            (ok true)
        )
        ;; if-1 FALSE
        ;; if-2
        (if (is-eq department u1)
        
        ;; if-2 TRUE
        (begin 

            (asserts! (< u0 (var-get dentist_app_number )) EMPTY_APP)
            (asserts!  (is-eq (is-some (map-get? dentist {appointment_number: app_num})) true) APP_DOESNT_EXISTS)

            (map-delete dentist {appointment_number: app_num})
            (var-set dentist_app_number (- (var-get dentist_app_number) u1))
            (ok true)   
        )
        ;; if-2 FASLE
        ;; if-3            
        (if (is-eq department u2)
        
        ;; if-3 TRUE
        (begin

            (asserts! (< u0 (var-get general_physician_app_number )) EMPTY_APP)  
            (asserts!  (is-eq (is-none (map-get? general_physician {appointment_number: app_num})) true) APP_DOESNT_EXISTS)

            (map-delete general_physician {appointment_number: app_num})
            (var-set general_physician_app_number (- (var-get general_physician_app_number) u1))
            (ok true)
        )

        ;; if-3 FALSE
            (err u210)
        ) ;; if-3 END
        ) ;; if-2 END
        ) ;; if-1 END

    )
)


;; read-only funtions

(define-read-only (read_patient_data (id principal))
(begin

(asserts! (is-eq (is_auth_caller) true) UNAUTH_CALLER)
(ok (unwrap! (map-get? patient_data {id: id}) PATIENT_DOESNT_EXISTS))

)
)

(define-read-only (radiology_app_details (app_num uint))
(begin

(asserts! (is-eq (is_auth_caller) true) UNAUTH_CALLER)
(ok (unwrap! (map-get? radiology {appointment_number: app_num}) APP_DOESNT_EXISTS))

)
)

(define-read-only (dentist_app_details (app_num uint))
(begin

(asserts! (is-eq (is_auth_caller) true) UNAUTH_CALLER)
(ok (unwrap! (map-get? dentist {appointment_number: app_num}) APP_DOESNT_EXISTS))

)
)

(define-read-only (general_physician_app_details (app_num uint))
(begin

(asserts! (is-eq (is_auth_caller) true) UNAUTH_CALLER)
(ok (unwrap! (map-get? general_physician {appointment_number: app_num}) APP_DOESNT_EXISTS))

)
)

(define-read-only (check_radiology_app (app_num uint))

(if (is-eq (is-none (map-get? radiology {appointment_number: app_num})) true)

;; if TRUE return
true
;; if FALSE return
false
)
)


(define-read-only (check_dentist_app (app_num uint))

(if (is-eq (is-none (map-get? dentist {appointment_number: app_num})) true)

;; if TRUE return
true
;; if FALSE return
false
)
)


(define-read-only (check_general_physician_app (app_num uint))

(if (is-eq (is-none (map-get? general_physician {appointment_number: app_num})) true)

;; if TRUE return
true
;; if FALSE return
false
)
)


(define-read-only (read_radiology_app_number) 

(var-get radiology_app_number)

)


(define-read-only (read_dentist_app_number) 

(var-get dentist_app_number)

)


(define-read-only (read_general_physician_app_number) 

(var-get general_physician_app_number)

)

