;; -*- coding: utf-8 -*-
;;
;; ���y���t�̃e�X�g
;; 2016-9-26
;;
(add-load-path "." :relative)
(display #\cr)(flush) ; allocate console for windows
(use glmintool)
(use alaudplay)

;; �A�v���̃f�B���N�g���̃p�X��
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

;; ���y�f�[�^�N���X�̃C���X�^���X����
(define *adata1* (make <auddata> :waittime 200))

;; ������
(aud-init)
;(aud-init #f)

;; �t�@�C���̓ǂݍ���
(auddata-load-wav-file *adata1* (make-fpath *app-dpath* "sound/abcde.wav"))
(auddata-set-prop *adata1* AL_GAIN  1.0)
(auddata-set-prop *adata1* AL_PITCH 1.0)

;; �Đ�
(auddata-play *adata1*)
(sys-sleep 1)
(auddata-pause *adata1*)
(sys-sleep 1)
(auddata-play *adata1*)
(until (= (auddata-stat *adata1*) AL_STOPPED)
  (sys-nanosleep (* 100 1000000))) ; 100msec
(sys-sleep 1)
(auddata-play *adata1*)
(sys-sleep 1)
(auddata-rewind *adata1*)
(auddata-play *adata1*)
(sys-sleep 1)
(auddata-stop *adata1*)

;; ��Ԏ擾
(print "GAIN  = " (auddata-get-prop *adata1* AL_GAIN))
(print "PITCH = " (auddata-get-prop *adata1* AL_PITCH))

;; �I��
(sys-sleep 1) ; �����҂��Ȃ��ƃm�C�Y���o��
(auddata-free *adata1*)
(aud-end)

(print "HIT ENTER KEY!")
(flush)
(read-line)

