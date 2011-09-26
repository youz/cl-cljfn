
(defsystem #:cl-cljfn
  :name "cl-cljfn"
  :author "Yousuke Ushiki <citrus.yubeshi@gmail.com>"
  :version "0.1"
  :licence "BSD"
  :description "Clojure-style anonymous function reader"
  :components ((:file "cljfn")))


(defsystem #:cl-cljfn-test
  :depends-on (:cl-cljfn #+:sbcl #:sb-rt #-:sbcl #:rt)
  :components ((:file "cljfn-test")))

