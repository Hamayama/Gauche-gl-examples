;; -*- coding: utf-8 -*-
;;
;; planet.scm
;; 2017-2-3 v1.12
;;
;; ＜内容＞
;;   Gauche-gl を使って、星を表示するサンプルです。
;;   ESCキーを押すと終了します。
;;
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use math.mt-random)

(define *wait*   20) ; ウェイト(msec)
(define *title* "planet") ; ウィンドウのタイトル
(define *snum*  200) ; 星の数
(define *ssize*   4) ; 星の大きさ
(define *sspeed*  1) ; 星の速度
(define *cvec*   #f) ; 星の色番号(ベクタ)
(define *xvec*   #f) ; 星のX座標(ユニフォームベクタ(f32vector))
(define *yvec*   #f) ; 星のY座標(ユニフォームベクタ(f32vector))
(define *zvec*   #f) ; 星のZ座標(ユニフォームベクタ(f32vector))
(define *zmin*  300) ; 星のZ座標の最小値
(define *zmax* 3000) ; 星のZ座標の最大値
(define *vangle* 45) ; 視野角(度)
(define *tanvan*  (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)

;; 乱数
;;   (randint n1 n2)でn1以上n2以下の整数の乱数を取得する(n1,n2は整数であること)
(define randint
  (let1 m (make <mersenne-twister> :seed (sys-time))
    (lambda (n1 n2)
      (if (> n1 n2) (let1 t n1 (set! n1 n2) (set! n2 t)))
      (+ (mt-random-integer m (+ (- n2 n1) 1)) n1))))

;; 色情報(16色)
(define *color-table*
  #(#f32(0.0 0.0 0.0 1.0) #f32(0.0 0.0 0.5 1.0) #f32(0.0 0.5 0.0 1.0) #f32(0.0 0.5 0.5 1.0)
    #f32(0.5 0.0 0.0 1.0) #f32(0.5 0.0 0.5 1.0) #f32(0.5 0.5 0.0 1.0) #f32(0.75 0.75 0.75 1.0)
    #f32(0.5 0.5 0.5 1.0) #f32(0.0 0.0 1.0 1.0) #f32(0.0 1.0 0.0 1.0) #f32(0.0 1.0 1.0 1.0)
    #f32(1.0 0.0 0.0 1.0) #f32(1.0 0.0 1.0 1.0) #f32(1.0 1.0 0.0 1.0) #f32(1.0 1.0 1.0 1.0)))

;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 光源設定
  (gl-light  GL_LIGHT0 GL_POSITION #f32(1.0 1.0 1.0 0.0))
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  ;; 材質設定
  (gl-material GL_FRONT GL_SPECULAR #f32(1.0 1.0 1.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 10.0)
  ;; ベクタの領域を確保する
  (set! *cvec* (make-vector    *snum* 0))
  (set! *xvec* (make-f32vector *snum* 0))
  (set! *yvec* (make-f32vector *snum* 0))
  (set! *zvec* (make-f32vector *snum* 0))
  ;; 星の色と座標の初期化
  (do ((i 0 (+ i 1)))
      ((>= i *snum*) #f)
    (let1 z (randint *zmin* *zmax*)
      (vector-set!    *cvec* i (randint 10 15))
      (f32vector-set! *zvec* i (- z))
      (f32vector-set! *xvec* i (randint (truncate->exact (- (* *zmax* *tanvan*)))
                                        (truncate->exact    (* *zmax* *tanvan*))))
      (f32vector-set! *yvec* i (randint (truncate->exact (- (* z *tanvan*)))
                                        (truncate->exact    (* z *tanvan*))))
      ))
  )

;; タイマー
(define (timer val)
  ;; 星の座標を更新
  (do ((i 0 (+ i 1)))
      ((>= i *snum*) #f)
    (let1 x (+ (f32vector-ref *xvec* i) *sspeed*)
      (if (> x (truncate->exact (* *zmax* *tanvan*)))
        (let1 z (randint *zmin* *zmax*)
          (vector-set!    *cvec* i (randint 10 15))
          (f32vector-set! *zvec* i (- z))
          (f32vector-set! *xvec* i (truncate->exact (- (* *zmax* *tanvan*))))
          (f32vector-set! *yvec* i (randint (truncate->exact (- (* z *tanvan*)))
                                            (truncate->exact    (* z *tanvan*))))
          )
        (f32vector-set! *xvec* i x))
      ))
  ;; 画面表示
  (glut-post-redisplay)
  ;; ウェイト
  (glut-timer-func *wait* timer 0))

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 星を表示
  (do ((i 0 (+ i 1)))
      ((>= i *snum*) #f)
    (gl-material GL_FRONT GL_DIFFUSE (~ *color-table* (~ *cvec* i)))
    (gl-push-matrix)
    (gl-translate (f32vector-ref *xvec* i)
                  (f32vector-ref *yvec* i)
                  (f32vector-ref *zvec* i))
    (glut-solid-sphere *ssize* 20 20)
    (gl-pop-matrix)
    )
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  ;; 透視射影する範囲を設定
  (glu-perspective *vangle* (/. w h) 1 *zmax*))

;; キー入力
(define (keyboard key x y)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit 0))
   ;; [g]キーでGC実行(デバッグ用)
   ((or (= key (char->integer #\g)) (= key (char->integer #\G)))
    (gc) (print (gc-stat)))
   ))

;; メイン処理
(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size 480 480)
  (glut-init-window-position 100 100)
  (glut-create-window *title*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-timer-func *wait* timer 0)
  ;; コールバック内エラー対策
  (guard (ex (else (report-error ex) (exit 0)))
    (glut-main-loop))
  0)

