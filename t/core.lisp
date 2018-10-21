(defpackage #:mito-email-auth-test/core
  (:use #:cl
        #:mito-email-auth/core
        #:rove
        #:hamcrest/rove))
(in-package mito-email-auth-test/core)


(deftest test-some-staff
    (testing "Replace this test with real staff."
      (assert-that (foo 1 2)
                   (contains 1 2))))
