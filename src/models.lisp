(defpackage #:mito-email-auth/models
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:mailgun)
  (:import-from #:local-time
                #:timestamp<
                #:timestamp+
                #:now)
  (:import-from #:uuid
                #:make-v4-uuid)
  (:import-from #:mito
                #:create-dao)
  (:export
   #:user-with-email))
(in-package mito-email-auth/models)


(defclass user-with-email ()
  ((email :col-type (or (:varchar 255)
                        :null)
          :initarg :email
          :initform nil
          :reader get-email))
  (:metaclass mito:dao-table-mixin)
  (:unique-keys email))


(define-condition code-unknown (error)
  ((code :initarg :code
         :reader get-code))
  (:documentation "Возникает, когда код не найден в базе."))


(define-condition code-expired (error)
  ((code :initarg :code
         :reader get-code))
  (:documentation "Возникает, когда код найден в базе, но срок его действия истёк."))


(defclass registration-code ()
  ((email :col-type (:varchar 255)
          :initarg :email
          :reader get-email
          :documentation "Email, на которой выслали код.")
   (code :col-type (:varchar 255)
         :initarg :code
         :reader get-code
         :documentaion "Хэш, который мы вставим в письмо, чтобы залогинить пользователя.")
   (valid-until :col-type :timestamp
                :initarg :valid-until
                :reader get-valid-until
                :documentation "Время до наступления которого нужно использовать код."))
  
  (:documentation "Модель для хранения кода, высылаемого на email для регистрации или аутентификации пользователя.")
  (:metaclass mito:dao-table-class))


(defmethod print-object ((obj registration-code) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "email=~A code=~A"
            (get-email obj)
            (get-code obj))))


(defun make-registration-code (email &key (ttl (* 60 60) ;; by default code will expire in 1 hour
                                      ))
  (let* ((now (now))
         (valid-until (timestamp+ now ttl :sec))
         (code (uuid:print-bytes nil (make-v4-uuid))))
    (create-dao 'registration-code
                :email email
                :code code
                :valid-until valid-until)))


(defun send-code (email)
  (let* ((code (make-registration-code email))
         ;; (url (make-uri (format nil
         ;;                        "/login?code=~A"
         ;;                        (get-code code))))
         )
    (log:info "New registration code" code)
    ;; (mailgun:send ("Ultralisp <noreply@ultralisp.org>" email "The code to log into the Ultralisp.org")
    ;;   (:p ("To log into [Ultralisp.org](~A), follow [this link](~A)"
    ;;        url
    ;;        url))
    ;;   (:p "Hurry up! Link is will expire in one hour."))
    ))


(defun authenticate (code)
  "Authenticates a user.

   If code wasn't found, then condition code-unknown will be raised.
   If code is expired, then condition code-expired will be raised.
   
   Возвращает пользователя, а вторым значением t если пользователь уже был
   и nil если это новая учётка."

  (let ((registration-code (mito:find-dao 'registration-code
                                          :code code)))
    (unless registration-code
      (error 'code-unknown :code code))

    ;; Теперь проверим, что код не просрочен
    (when (timestamp< (get-valid-until registration-code)
                      (now))
      (error 'code-expired :code code))

    ;; Ну а теперь можно попробовать залогинить пользователя
    ;; Для начала, попробуем найти его по email
    (let* ((email (get-email registration-code))
           ;; (user (get-user-by-email email))
           )
      email
      ;; user
      )))

