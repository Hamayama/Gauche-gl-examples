;; -*- coding: utf-8 -*-
;;
;; shadow.scm
;; 2021-6-24 v1.04
;;
;; ＜内容＞
;;   Gauche-gl を使用した、影のある星を表示するプログラムです。
;;   スペースキーを押すと、2D表示と3D表示を切り換えます。
;;   ESCキーを押すと終了します。
;;
(add-load-path "lib" :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use glmintool)
(use gltextscrn)

(define *wait*      20) ; ウェイト(msec)
(define *title* "shadow") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (* (/. *vangle* 2) pi/180))) ; 視野角/2のタンジェント(計算用)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *zd/2*     400) ; 画面奥行き/2

(define *r1*       260) ; 星の半径
(define *x2*         0) ; 影の中心のX座標(2D表示用)(計算で求める)
(define *r2*         0) ; 影の半径       (2D表示用)(計算で求める)
(define *x3*      *r1*) ; 影の端点のX座標(2D表示用)
(define *d1*         0) ; 影の端点の(XZ平面上の)角度
(define *vd1*        (* 0.3 pi/180)) ; 影の端点の(XZ平面上の)角速度
(define *fillmode*   0) ; 塗りつぶしモード(=0:出現中,=1:消失中)
(define *3d-disp*    0) ; 3D表示(=0:OFF,=1:ON)

(define *backcolor*  #f32(0.0 0.0 0.0 1.0)) ; 背景色
(define *starcolor1* #f32(1.0 1.0 0.2 1.0)) ; 星の色
(define *starcolor2* #f32(0.2 0.2 0.2 1.0)) ; 影の色(2D表示用)

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 2) (* *ht/2* 2))

;; 時間待ちクラスのインスタンス生成
(define *twinfo* (make <timewaitinfo> :waitinterval *wait*))

;; ウェイト時間調整クラスのインスタンス生成
(define *wcinfo* (make <waitcalcinfo> :waittime *wait*))


;; クリッピング解除
(define (clip-off)
  (define xoffset (quotient (- (glut-get GLUT_WINDOW_WIDTH)  *width*)  2))
  (define yoffset (quotient (- (glut-get GLUT_WINDOW_HEIGHT) *height*) 2))
  (%win-clip (win-x *win* (- *wd/2*))
             (win-y *win* *ht/2*)
             (win-w *win* (* *wd/2* 2))
             (win-h *win* (* *ht/2* 2))
             xoffset yoffset))

;; 星の表示(2D表示)
(define (disp-star-2d)
  (define xoffset (quotient (- (glut-get GLUT_WINDOW_WIDTH)  *width*)  2))
  (define yoffset (quotient (- (glut-get GLUT_WINDOW_HEIGHT) *height*) 2))
  ;; 星の表示(半円)
  (%win-clip (win-x *win* (if (= *fillmode* 0) 0 (- *wd/2*)))
             (win-y *win* *ht/2*)
             (win-w *win* *wd/2*)
             (win-h *win* (* *ht/2* 2))
             xoffset yoffset)
  (gl-color *starcolor1*)
  (draw-win-circle (win-x *win* 0)
                   (win-y *win* 0)
                   (win-h *win* *r1*)
                   *width* *height* 1 1 'center 0.0 200)
  ;; 影の表示(半円)
  (%win-clip (win-x *win* (if (= *fillmode* 0) (- *wd/2*) 0))
             (win-y *win* *ht/2*)
             (win-w *win* *wd/2*)
             (win-h *win* (* *ht/2* 2))
             xoffset yoffset)
  (gl-color *starcolor2*)
  (draw-win-circle (win-x *win* 0)
                   (win-y *win* 0)
                   (win-h *win* *r1*)
                   *width* *height* 1 1 'center 0.0 200)
  ;; 星/影のふくらみの部分を表示
  (unless (= *x3* 0)
    (set! *x2* (/. (- (* *x3* *x3*) (* *r1* *r1*)) (* 2 *x3*)))
    (set! *r2* (sqrt (+ (* *x2* *x2*) (* *r1* *r1*))))
    (%win-clip (win-x *win* (if (> *x3* 0) 0 (- *wd/2*)))
               (win-y *win* *ht/2*)
               (win-w *win* *wd/2*)
               (win-h *win* (* *ht/2* 2))
               xoffset yoffset)
    (gl-color (if (or (and      (> *x3* 0)  (not (= *fillmode* 0)))
                      (and (not (> *x3* 0))      (= *fillmode* 0)))
                *starcolor1*
                *starcolor2*))
    (draw-win-circle (win-x *win* *x2*)
                     (win-y *win* 0)
                     (win-h *win* *r2*)
                     *width* *height* 1 1 'center 0.1 200))
  ;; クリッピング解除
  (clip-off))

;; 星の表示(3D表示)
(define (disp-star-3d)
  ;; 色
  (gl-material GL_FRONT GL_DIFFUSE *starcolor1*)
  (gl-material GL_FRONT GL_AMBIENT #f32(0.4 0.4 0.4 1.0))
  ;; 球
  (glut-solid-sphere *r1* 200 200))

;; 影の移動
;;   ・戻り値は、モード移行した場合に #t を返す
(define (move-shadow)
  (define ret #f)
  (set! *d1* (+ *d1* *vd1*))
  (when (or (< *d1* 0) (> *d1* pi))
    (set! *d1* (if (< *d1* 0) pi 0))
    (set! *fillmode* (- 1 *fillmode*))
    (set! ret #t))
  ;; 2D表示では、影の端点を移動する
  (set! *x3* (* *r1* (cos *d1*)))
  ;; 3D表示では、光源の方を移動する
  (let ((d1 (+ *d1* (- pi/2) (* *fillmode* pi))))
    (gl-light GL_LIGHT0 GL_POSITION (f32vector (cos d1) 0.0 (sin d1) 0.0)))
  ret)


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 光源設定
  ;(gl-light  GL_LIGHT0 GL_POSITION #f32(1.0 1.0 1.0 0.0))
  (gl-light  GL_LIGHT0 GL_POSITION #f32(-1.0 1.0 1.0 0.0))
  ;(gl-light  GL_LIGHT0 GL_AMBIENT  #f32( 0.5 0.5 0.5 1.0)) ; 環境光
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_NORMALIZE)
  ;; 材質設定
  ;(gl-material GL_FRONT GL_SPECULAR #f32(1.0 1.0 1.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 10.0)
  ;; クリッピング設定
  (gl-enable GL_SCISSOR_TEST))

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 星の表示
  (if (= *3d-disp* 0)
    (disp-star-2d)
    (disp-star-3d))
  ;; 背景の表示
  (gl-color *backcolor*)
  (draw-win-rect 0 0 *width* *height* *width* *height* 'left -0.999999)
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  (min w h))
  (set! *height* (min w h))
  (win-update-size *win* *width* *height*)
  ;; 縦横比を変えずにリサイズ
  (gl-viewport (quotient (- w *width*) 2) (quotient (- h *height*) 2) *width* *height*)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (let1 z1 (/. *ht/2* *tanvan*)
    ;; 透視射影する範囲を設定
    (glu-perspective *vangle* (/. *width* *height*) (- z1 *zd/2*) (+ z1 *zd/2*))
    ;; 視点の位置と方向を設定
    (glu-look-at 0 0 z1 0 0 0 0 1 0))
  ;; クリッピング解除
  (clip-off))

;; キー入力
(define (keyboard key x y)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit 0))
   ;; スペースキーで2D表示と3D表示を切り換える
   ((= key (char->integer #\space))
    (set! *3d-disp* (- 1 *3d-disp*)))
   ;; [g]キーでGC実行(デバッグ用)
   ((or (= key (char->integer #\g)) (= key (char->integer #\G)))
    (gc) (print (gc-stat)))
   ))

;; タイマー
(define (timer val)
  (cond
   ;; 待ち状態のとき
   ((timewait-waiting? *twinfo*)
    (timewait-timer *twinfo*))
   ;; 待ち状態でないとき
   (else
    ;; 影の移動
    (when (move-shadow)
      ;; モード移行を遅延する(完了の余韻を残すため)
      (timewait *twinfo* 1500
                (lambda () (timewait-clear *twinfo*))))))
  ;; 画面表示
  (glut-post-redisplay)
  ;; ウェイト時間調整
  (glut-timer-func (waitcalc *wcinfo*) timer 0))

;; メイン処理
(define (main args)
  (set! *3d-disp* (x->integer (list-ref args 1 0)))
  (glut-init '())
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size *width* *height*)
  (glut-init-window-position 100 100)
  (glut-create-window *title*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-timer-func *wait* timer 0)
  (glut-show-window)
  ;; コールバック内エラー対策
  (guard (ex (else (report-error ex) (exit 1)))
    (glut-main-loop))
  0)

