;;
;; testing alaudplay, glmintool and gltextscrn
;;

(add-load-path "." :relative)
(display #\cr)(flush) ; allocate console for windows
(use gauche.test)

(test-start "alaudplay")
(use alaudplay)
(test-module 'alaudplay)
(test-end)

(test-start "glmintool")
(use glmintool)
(test-module 'glmintool)
(test-end)

(test-start "gltextscrn")
(use gltextscrn)
(test-module 'gltextscrn)
(test-end)

(print "HIT ENTER KEY!")
(flush)
(read-line)

