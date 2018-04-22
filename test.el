(require 'testing)

(defmacro test (name)
  `(ert-deftest ,(intern (format "test-%s" name)) ()
     (let ((x (funcall (intern (format "testing/%s" ,name)))))
       (message "
 Type: %s
Value: %s
" (type-of x) x)
       (print x))))

;;; Crashes.
;; (test "crash")

;;; Works
(test "stack-allocated")
(test "gc-after-construction")

;;; Crashes on Linux. Works on macOS.
;; (test "gc-after-passing-back")
;; (test "gc-after-printing")
