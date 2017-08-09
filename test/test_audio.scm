;; -*- coding: utf-8 -*-
;;
;; 音楽演奏のテスト
;; 2017-8-10
;;
(add-load-path "../lib" :relative)
(display #\cr)(flush) ; allocate console for windows
(use glmintool)
(use alaudplay)
;(use mmlproc)

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

;; 音楽データクラスのインスタンス生成
(define *adata1* (make <auddata> :waittime 200))

;; 初期化
(aud-init)
;(aud-init #f)

;; 音楽データの読み込み
(auddata-load-wav-file *adata1* (make-fpath *app-dpath* "../sound/abcde.wav"))
;(auddata-load-pcm-raw *adata1* (mml->pcm "@500cdefgab>c"))
(auddata-set-prop *adata1* AL_GAIN  1.0)
(auddata-set-prop *adata1* AL_PITCH 1.0)

;; 再生
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

;; 状態取得
(print "GAIN  = " (auddata-get-prop *adata1* AL_GAIN))
(print "PITCH = " (auddata-get-prop *adata1* AL_PITCH))

;; 終了
(sys-sleep 1) ; 少し待たないとノイズが出る
(auddata-free *adata1*)
(aud-end)

(print "HIT ENTER KEY!")
(flush)
(read-line)

