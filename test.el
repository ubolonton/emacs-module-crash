(require 'testing)

(defmacro test (name)
  `(ert-deftest ,(intern (format "test-%s" name)) ()
     (let ((x (funcall (intern (format "testing/%s" ,name)))))
       (message "
 Type: %s
Value: %s
" (type-of x) x)
       (print x))))

;; ;;; Crashes
;; (test "crash")

(test "ok-1")
(test "ok-2")
(test "ok-3")
(test "ok-4")
