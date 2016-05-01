;; -*- coding: utf-8 -*-
;;
;; alaudplay.scm
;; 2016-5-1 v1.05
;;
;; ＜内容＞
;;   Gauche-al を使って音楽を演奏するためのモジュールです。
;;   Gauche-al モジュールが存在しない場合には、
;;   各手続きは何もしません(エラーにもなりません)。
;;
(define-module alaudplay
  ;(use al)
  (define-module al)
  (import al)
  (use gauche.uvector)
  (use gauche.vport) ; open-output-uvector用
  ;(use mmlproc)     ; write-wav,get-wav-size用
  (define-module mmlproc)
  (import mmlproc)
  (export
    aud-enabled? aud-init aud-end
    <auddata> auddata-load-wav-file auddata-load-pcm-raw auddata-free
    auddata-play auddata-pause auddata-stop auddata-rewind
    auddata-stat auddata-set-prop auddata-get-prop
    AL_PITCH AL_GAIN AL_MAX_DISTANCE AL_ROLLOFF_FACTOR AL_REFERENCE_DISTANCE
    AL_MIN_GAIN AL_MAX_GAIN
    AL_CONE_OUTER_GAIN AL_CONE_INNER_ANGLE AL_CONE_OUTER_ANGLE
    AL_POSITION AL_VELOCITY AL_DIRECTION AL_SOURCE_RELATIVE
    AL_LOOPING AL_BUFFER
    AL_SOURCE_STATE AL_INITIAL AL_PLAYING AL_PAUSED AL_STOPPED
    AL_BUFFERS_QUEUED AL_BUFFERS_PROCESSED
    ))
(select-module alaudplay)

;; 定数のダミー設定用マクロ
(define-syntax define-dummy-constants
  (syntax-rules ()
    ((_ name ...)
     (begin (define name 0) ...))))
(define-syntax overwrite-module-constants
  (syntax-rules ()
    ((_ mod name ...)
     (begin (set! name (with-module mod name)) ...))))

;; Gauche-al モジュールの定数のダミー設定
(define-macro (al-constants func . args)
  `(,func
    ,@args
    AL_PITCH AL_GAIN AL_MAX_DISTANCE AL_ROLLOFF_FACTOR AL_REFERENCE_DISTANCE
    AL_MIN_GAIN AL_MAX_GAIN
    AL_CONE_OUTER_GAIN AL_CONE_INNER_ANGLE AL_CONE_OUTER_ANGLE
    AL_POSITION AL_VELOCITY AL_DIRECTION AL_SOURCE_RELATIVE
    AL_LOOPING AL_BUFFER
    AL_SOURCE_STATE AL_INITIAL AL_PLAYING AL_PAUSED AL_STOPPED
    AL_BUFFERS_QUEUED AL_BUFFERS_PROCESSED
    ))
(al-constants define-dummy-constants)

;; Gauche-al モジュールのロード
(define *al-loaded*
  (rlet1 ret (load "al" :error-if-not-found #f)
    ;; Gauche-al モジュールの定数の上書き
    (if ret (al-constants overwrite-module-constants al))))

;; mmlproc モジュールのロード
(define *mmlproc-loaded*
  (load "mmlproc" :error-if-not-found #f))

;; 音楽演奏機能の有効/無効状態
(define *aud-enabled* #t)

;; 音楽演奏機能が有効かどうか
(define (aud-enabled?)
  (and *aud-enabled* *al-loaded*))


;; 音楽演奏機能の初期化
(define (aud-init :optional (enabled #t))
  (set! *aud-enabled* enabled)
  (if (aud-enabled?)
    (alut-init '())))

;; 音楽演奏機能の終了
(define (aud-end)
  (if (aud-enabled?)
    (alut-exit)))


;; 音楽データクラス
(define-class <auddata> ()
  ((src      :init-value #f) ; 音声ソース
   (buf      :init-value #f) ; 音声バッファ
   (waitdata :init-value 0)  ; 時間測定用(msec)
   (waittime :init-keyword :waittime :init-value 100) ; 連続演奏禁止時間(msec)
   ))

;; wavファイルの読み込み
(define-method auddata-load-wav-file ((a <auddata>) wav-file)
  (when (aud-enabled?)
    (set! (~ a 'buf) (al-gen-buffer))
    (al-buffer-data (~ a 'buf) (alut-load-wav-file wav-file))
    (unless (~ a 'src) (set! (~ a 'src) (al-gen-source)))
    (al-source (~ a 'src) AL_BUFFER (~ a 'buf))))

;; PCMの生データの読み込み
(define-method auddata-load-pcm-raw ((a <auddata>) (pcm-raw <s16vector>))
  (if (aud-enabled?)
    (let* ((wav-raw (make-u8vector (get-wav-size pcm-raw)))
           (out     (open-output-uvector wav-raw)))
      (unwind-protect
          (begin
            (write-wav pcm-raw out)
            (set! (~ a 'buf) (al-gen-buffer))
            (al-buffer-data (~ a 'buf) (alut-load-wav-memory wav-raw))
            (unless (~ a 'src) (set! (~ a 'src) (al-gen-source)))
            (al-source (~ a 'src) AL_BUFFER (~ a 'buf)))
        (close-output-port out)))))

;; 音楽データの解放
(define-method auddata-free ((a <auddata>))
  (when (aud-enabled?)
    (when (~ a 'src)
      (al-delete-sources (u32vector (~ a 'src)))
      (set! (~ a 'src) #f))
    (when (~ a 'buf)
      (al-delete-buffers (u32vector (~ a 'buf)))
      (set! (~ a 'buf) #f))
    ))

;; 音楽データの演奏開始
;; (連続演奏禁止時間のチェックあり)
(define-method auddata-play ((a <auddata>))
  (if (aud-enabled?)
    (let* ((tnow      (current-time))
           (tnowmsec  (+ (* (~ tnow 'second) 1000) (quotient (~ tnow 'nanosecond) 1000000)))
           (tdiffmsec (- tnowmsec (~ a 'waitdata))))
      (unless (and (>= tdiffmsec 0) (< tdiffmsec (~ a 'waittime)))
        (al-source-play (~ a 'src))
        (set! (~ a 'waitdata) tnowmsec)))))

;; 音楽データの演奏一時停止
(define-method auddata-pause ((a <auddata>))
  (if (aud-enabled?)
    (al-source-pause (~ a 'src))))

;; 音楽データの演奏停止
(define-method auddata-stop ((a <auddata>))
  (if (aud-enabled?)
    (al-source-stop (~ a 'src))))

;; 音楽データの演奏巻き戻し
(define-method auddata-rewind ((a <auddata>))
  (if (aud-enabled?)
    (al-source-rewind (~ a 'src))))

;; 音楽データの演奏状態取得
(define-method auddata-stat ((a <auddata>))
  (if (aud-enabled?)
    (al-get-source (~ a 'src) AL_SOURCE_STATE)
    AL_STOPPED))

;; 音楽データのプロパティ設定
(define-method auddata-set-prop ((a <auddata>) prop val)
  (if (aud-enabled?)
    (al-source (~ a 'src) prop val)))

;; 音楽データのプロパティ取得
(define-method auddata-get-prop ((a <auddata>) prop)
  (if (aud-enabled?)
    (al-get-source (~ a 'src) prop)))

;(define *adata1* (make <auddata>)) ; インスタンス生成例
;(auddata-load-wav *adata1* "sound/sound1.wav")


