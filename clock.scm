;; -*- coding: utf-8 -*-
;;
;; clock.scm
;; 2017-5-4 v1.03
;;
;; ＜内容＞
;;   Gauche-gl を使用した、アナログ時計を表示するサンプルです。
;;   ESCキーを押すと終了します。
;;
(add-load-path "." :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use glmintool)
(use gltextscrn)
(use srfi-19) ; current-date用

(define *wait*     100) ; ウェイト(msec)
(define *title* "clock") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *csize*    300) ; 時計の大きさ/2
(define *hour*       0) ; 時
(define *minute*     0) ; 分
(define *second*     0) ; 秒
(define *nanosecond* 0) ; ナノ秒
(define *backcolor*   #f32(0.0 0.0 0.0 1.0)) ; 背景色
(define *centercolor* #f32(0.7 0.7 0.7 1.0)) ; 中心の色
(define *linecolor1*  #f32(1.0 0.0 0.0 1.0)) ; 秒針の色
(define *linecolor2*  #f32(0.0 0.0 1.0 1.0)) ; 長針の色
(define *linecolor3*  #f32(0.0 0.0 1.0 1.0)) ; 短針の色
(define *framecolor*  #f32(1.0 1.0 1.0 1.0)) ; 枠と目盛の色
(define *numbercolor* #f32(1.0 1.0 1.0 1.0)) ; 数字の色

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 2) (* *ht/2* 2))

;; ウェイト時間調整クラスのインスタンス生成
(define *wcinfo* (make <waitcalcinfo> :waittime *wait*))


;; 時計の表示
(define (disp-clock x y r :optional (hour 0) (minute 0) (second 0) (nanosecond 0))
  ;; 座標とサイズの計算
  (define x1   (win-x *win* x)) ; ウィンドウ上の中心のX座標(px)
  (define y1   (win-y *win* y)) ; ウィンドウ上の中心のY座標(px)
  (define r1   (win-h *win* r)) ; ウィンドウ上の時計の大きさ/2(px)
  ;; 秒、分、時の計算
  (define s1   (+ second (/. nanosecond 1e9))) ; 秒(小数部もあり)
  (define m1   (+ minute (/. s1 60)))          ; 分(小数部もあり)
  (define h1   (+ hour   (/. m1 60)))          ; 時(小数部もあり)
  ;; 針の角度の計算
  (define rad1 (- (* s1 (/. 2pi 60)) pi/2)) ; 秒針の角度(rad)
  (define rad2 (- (* m1 (/. 2pi 60)) pi/2)) ; 長針の角度(rad)
  (define rad3 (- (* h1 (/. 2pi 12)) pi/2)) ; 短針の角度(rad)

  ;; 中心
  (gl-color *centercolor*)
  (draw-win-circle x1 y1 (* r1 0.04) *width* *height*)
  ;; 秒針
  (gl-color *linecolor1*)
  (draw-win-line x1 y1
                 (+ x1 (* r1 0.89 (cos rad1)))
                 (+ y1 (* r1 0.89 (sin rad1))) *width* *height*)
  ;; 長針
  (gl-color *linecolor2*)
  (draw-win-poly x1 y1
                 (vector
                  (f32vector (* r1 0.91 (cos rad2))          (* r1 0.91 (sin rad2)))
                  (f32vector (* r1 0.04 (cos (+ rad2 pi/2))) (* r1 0.04 (sin (+ rad2 pi/2))))
                  (f32vector (* r1 0.04 (cos (- rad2 pi/2))) (* r1 0.04 (sin (- rad2 pi/2)))))
                 *width* *height*)
  ;; 短針
  (gl-color *linecolor3*)
  (draw-win-poly x1 y1
                 (vector
                  (f32vector (* r1 0.65 (cos rad3))          (* r1 0.65 (sin rad3)))
                  (f32vector (* r1 0.04 (cos (+ rad3 pi/2))) (* r1 0.04 (sin (+ rad3 pi/2))))
                  (f32vector (* r1 0.04 (cos (- rad3 pi/2))) (* r1 0.04 (sin (- rad3 pi/2)))))
                 *width* *height*)
  ;; 枠
  (gl-color *framecolor*)
  (draw-win-circle-line x1 y1 r1 *width* *height*)
  ;(draw-win-rect-line (- x1 r1) (- y1 r1) (* r1 2) (* r1 2) *width* *height*)
  ;(draw-win-poly-line x1 y1 (vector (f32vector 0 (- r1)) (f32vector (- r1) r1) (f32vector r1 r1)) *width* *height*)
  ;; 目盛
  (do ((i   0 (+ i 1))
       (rad 0 (+ rad (/. 2pi 60))))
      ((>= i 60) #f)
    (gl-line-width (if (= (mod i 5) 0) 3.5 1.5))
    (draw-win-line (+ x1 (* r1 0.97 (cos rad)))
                   (+ y1 (* r1 0.97 (sin rad)))
                   (+ x1 (* r1 0.94 (cos rad)))
                   (+ y1 (* r1 0.94 (sin rad))) *width* *height*))
  ;; 数字
  (gl-color *numbercolor*)
  (do ((i   0 (+ i 1))
       (rad (- (/. 2pi 6)) (+ rad (/. 2pi 12))))
      ((>= i 12) #f)
    (draw-stroke-text (x->string (+ i 1))
                      (+ x1 (* r1 0.82 (cos rad)))
                      (+ y1 (* r1 0.82 (sin rad)) (* r1 -0.1))
                      *width* *height* (* r1 0.2) 'center))
  )


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 線の太さとアンチエイリアスの設定
  (gl-enable GL_LINE_SMOOTH)
  (gl-enable GL_BLEND)
  (gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (gl-hint GL_LINE_SMOOTH_HINT GL_DONT_CARE)
  (gl-line-width 1.5)
  )

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  ;; 時計の表示
  (disp-clock 0 0 *csize* *hour* *minute* *second* *nanosecond*)
  ;(disp-clock (if (= *width* 0) 0 (* 100 (/. *height* *width*))) 100 80
  ;            (- *hour* 9) *minute* *second* *nanosecond*)
  ;; 背景の表示
  (gl-color *backcolor*)
  (draw-win-rect 0 0 *width* *height* *width* *height* 'left)
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  w)
  (set! *height* (min w h))
  (win-update-size *win* *width* *height*)
  ;; 縦横比を変えずにリサイズ
  (gl-viewport 0 (quotient (- h *height*) 2) *width* *height*))

;; キー入力
(define (keyboard key x y)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit 0))
   ;; [g]キーでGC実行(デバッグ用)
   ((or (= key (char->integer #\g)) (= key (char->integer #\G)))
    (gc) (print (gc-stat)))
   ))

;; タイマー
(define (timer val)
  ;; 現在時刻の取得
  (let1 d1 (current-date)
    (set! *hour*       (date-hour       d1))
    (set! *minute*     (date-minute     d1))
    (set! *second*     (date-second     d1))
    (set! *nanosecond* (date-nanosecond d1))
    )
  ;; 画面表示
  (glut-post-redisplay)
  ;; ウェイト時間調整
  (glut-timer-func (waitcalc *wcinfo*) timer 0))

;; メイン処理
(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size *width* *height*)
  (glut-init-window-position 100 100)
  (glut-create-window *title*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-timer-func *wait* timer 0)
  ;; コールバック内エラー対策
  (guard (ex (else (report-error ex) (exit 1)))
    (glut-main-loop))
  0)

