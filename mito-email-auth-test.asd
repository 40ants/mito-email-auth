(defsystem mito-email-auth-test
           :author "Alexander Artemenko"
           :license ""
           :class :package-inferred-system
           :pathname "t"
           :depends-on (:mito-email-auth
                        "mito-email-auth-test/core")
           :description "Test system for mito-email-auth"

           :perform (test-op :after (op c)
                             (symbol-call :rove :run c)
                             (clear-system c)))
