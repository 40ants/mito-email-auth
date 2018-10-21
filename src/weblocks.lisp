(defpackage #:mito-email-auth/weblocks
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:weblocks/session)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/widget
                #:widget
                #:get-css-classes
                #:render
                #:defwidget)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks-ui/form
                #:render-button
                #:with-html-form)
  (:import-from #:mito-email-auth/models
                #:code-expired
                #:code-unknown
                #:authenticate
                #:send-code)
  (:import-from #:weblocks/dependencies
                #:get-dependencies)
  (:import-from #:weblocks/request
                #:get-parameter)
  (:import-from #:weblocks/response
                #:redirect)
  (:import-from #:weblocks/page
                #:get-title)
  (:export
   #:make-login-widget
   #:make-login-processor
   #:make-logout-processor))
(in-package mito-email-auth/weblocks)


(defwidget login ()
  ((sent :documentation "If true, then we've sent a code to an email."
         :initform nil
         :accessor get-sent
         ;; TODO: this was broken in a redesigned weblocks. Need to fix it.
         :affects-dirty-status-p t)
   (email :documentation "Email entered by user."
          :initform nil
          :accessor get-email))
  (:documentation "Виджет для отображения формы логина."))


(defun make-login-widget ()
  (make-instance 'login))


;; (defun show-login-form-p ()
;;   "Предикат, определяющий, нужно ли отображать форму логина.

;;    Если пользователь уже залогинен, то не нужно"
;;   (app.models.user:anonymous-p
;;    (app.models.user:get-current-user)))


(defmethod render ((widget login))
  (cond
    ;; Код отправлен
    ((get-sent widget)
     (with-html
       (:h1 "Link was sent to the email")
       (:p
        "Login link was sent to the " (:strong (get-email widget)) ".")))
    ;; If code wasn't sent to, then we need to show a form
    (t
     (with-html-form
         (:POST
          (lambda (&key email &allow-other-keys)
            (send-code email)
            (setf (get-sent widget)
                  t
                  (get-email widget)
                  email)
            ;; TODO: remove when Weblocks' affects-dirty-status-p attribute will be fixed
            (weblocks/widget:update widget)
            
            (reach-goal widget "LOGIN-FORM-FILLED")))

       (:h1 "Login")
       (:p "Please, login.")
       (:p (:label "Enter email and we'll send a link with a code to login:" 
                   (:input :type "text"
                           :name "email")))
       (:p (render-button :send-code :value "Send a link")))
     ;; Сделаем hit по цели
     (reach-goal widget "LOGIN-FORM-SHOWED"))))


(defmethod get-css-classes ((widget login))
  (append
   (when (get-sent widget)
     (list :sent))

   (call-next-method)))


(defparameter *css-code*
  (list
   (weblocks-lass:make-dependency
     '(.login
       :padding 0.4rem
       
       (h1 :font-size 2rem
           :line-height 2rem)
       
       ;; Уберём лишний отступ после последнего абзаца
       ((:and p :last-child)
        :margin-bottom 0)))

   ;; Если форма показывается в фиде сказок, то её нужно выделить
   ;; сделав фон поярче
   (weblocks-lass:make-dependency
     '((:and .login .in-feed)
       :background-color "#ffbaba"))

   ;; После отправки, у плашки должен быть позитивный зелёненький цвет :)
   (weblocks-lass:make-dependency
     '((:and .login .in-feed .sent)
       :background-color "#baffba"))

   ;; И у виджета, который показывается в фиде сказок, заголовки должны быть выровняны по центру
   (weblocks-lass:make-dependency
     '((:and .login .in-feed)
       (h1 :text-align center)))))


(defmethod get-dependencies ((widget login))
  (append *css-code*
          (call-next-method)))


;; Процессинг логина


(defwidget login-processor ()
  ()
  (:documentation "Этот виджет мы показываем, когда обрабатываем логин
                   пользователя посредством кода из письма или нужно отрисовать форму логина."))


(defwidget logout-processor ()
  ()
  (:documentation "Этот виджет мы показываем, когда нужно разлогинить пользователя."))


(defun make-login-processor ()
  (make-instance 'login-processor))


(defun make-logout-processor ()
  (make-instance 'logout-processor))


(defgeneric reach-goal (widget goal-name &key survive-redirect-p)
  (:method ((widget t) goal-name &key survive-redirect-p)
    (declare (ignorable widget survive-redirect-p))
    (log:info "Goal" goal-name "was reached")))


(defmethod render ((widget login-processor))
  (let ((code (get-parameter "code")))
    (if code
        (handler-case
            (multiple-value-bind (user existing-p)
                (authenticate code)
              (log:info "User logged in" user)

              ;; Если логин удался, то надо вернуть пользователя на главную страницу
              (unless existing-p
                (reach-goal widget "REGISTERED" :survive-redirect-p t))

              ;; Ну и в любом случае, зафиксируем факт логина
              (reach-goal widget "LOGGED-IN" :survive-redirect-p t)

              (redirect "/"))
          (code-unknown ()
            (setf (get-title)
                  "Код не найден")
            
            (with-html
              (:p (format nil "Код ~A не найден" code)))

            (reach-goal widget "LOGIN-CODE-NOT-FOUND"))

          ;; TODO: надо сделать так, чтобы можно было в один клик получить новый
          (code-expired ()
            (setf (get-title)
                  "Код просрочен")
            
            (with-html
              (:p (format nil "Код ~A просрочен" code)))

            (reach-goal widget "LOGIN-CODE-EXPIRED")))
        
        ;; Если кода нет, значит надо просто форму логина нарисовать
        (let ((login-widget (make-login-widget)))
          (render login-widget)))))


(defmethod render ((widget logout-processor))
  (weblocks/session:reset)
  (redirect "/"))
