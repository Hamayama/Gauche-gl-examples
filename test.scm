;;
;; testing alaudplay, glmintool and gltextscrn
;;

(add-load-path "." :relative)
(display #\cr)(flush) ; allocate console for windows
(use gauche.test)

(test-start "alaudplay")
(use alaudplay)
(test-module 'alaudplay
             :allow-undefined
             '(alut-init alut-exit
               al-gen-source al-gen-buffer al-buffer-data
               al-delete-sources al-delete-buffers
               alut-load-wav-file alut-load-wav-memory
               al-source-play al-source-pause al-source-stop al-source-rewind
               al-source al-get-source
               write-wav get-wav-size))
(test-end)

(test-start "glmintool")
(use glmintool)
(test-module 'glmintool)
(test-end)

(test-start "gltextscrn")
(use gltextscrn)
(with-module gltextscrn
  (define *font-bitmap-1* #f)
  (define *font-stroke-1* #f))
(test-module 'gltextscrn)
(test-end)

(print "HIT ENTER KEY!")
(flush)
(read-line)

