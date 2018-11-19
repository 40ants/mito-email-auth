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
                #:find-dao
                #:create-dao)
  (:export
   #:user-with-email
   #:anonymous-p
   #:get-email
   #:*user-class*
   #:authenticate
   #:*send-code-callback*
   #:get-code
   #:code-expired
   #:code-unknown
   #:get-user-by-email))
(in-package mito-email-auth/models)


(defclass user-with-email ()
  ((email :col-type (or (:varchar 255)
                        :null)
          :initarg :email
          :initform nil
          :reader get-email))
  (:metaclass mito:dao-table-mixin)
  (:unique-keys email))


(defvar *user-class*)
(setf (documentation '*user-class* 'variable)
      "Set this variable to a concrete class derived from `user-with-email'.")


(defvar *send-code-callback*)
(setf (documentation '*send-code-callback* 'variable)
      "Set this variable to a function of one argument of type `registration-code'.
It should send a registration code using template, suitable for your website.")


(defun get-user-by-email (email)
  "Returns an instance of concrete class inherited from
   user-with-email.
   The class should be bound to global variable `*user-class*'."
  (unless (boundp '*user-class*)
    (error "Please, bind *user-class* symbol to a concrete class, derived from 'user-with-email"))
  
  (find-dao *user-class* :email email))


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


(defun send-code (email &key retpath)
  (let* ((code (make-registration-code email)))
    (cond
      ((boundp '*send-code-callback*)
       (funcall *send-code-callback*
                code
                :retpath retpath))
      (t (log:info "New registration code" code "Please set *send-code-callback*!")))))


(defgeneric authenticate (code-or-user)
  (:documentation "Processes user authentication. First called with the code, and makes inner call to itself with a user bound to this code."))


(defmethod authenticate ((code string))
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
           (user (or (get-user-by-email email)
                     (mito:create-dao *user-class*
                                      :email email))))
      (authenticate user))))


(defmethod authenticate ((user user-with-email))
  "Authenticates a user."
  (error "Please, define an authenticate method for you a concrete class, derived from user-with-email."))


(defgeneric anonymous-p (user)
  (:documentation "Returns t if user is not authenticated.")
  
  (:method ((user t))
    t)
  (:method ((user user-with-email))
    (unless (get-email user)
      t)))
