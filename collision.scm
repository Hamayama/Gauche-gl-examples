;; -*- coding: utf-8 -*-
;;
;; collision.scm
;; 2020-5-5 v1.01
;;
;; ＜内容＞
;;   Gauche-gl を使用した、物体(球)の衝突をシミュレートするプログラムです。
;;   (一部計算が正しくないところがあります)
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
(define *title* "collision") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (* (/. *vangle* 2) pi/180))) ; 視野角/2のタンジェント(計算用)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *zd/2*     100) ; 画面奥行き/2

(define *N*         10) ; 物体の数
(define *C*          1) ; 反発係数(0-1)
(define *bval*       1) ; 物体のふちの調整値(この分だけめりこむ)
(define *rx*         (make-f64vector *N* 0))  ; 物体のX座標
(define *ry*         (make-f64vector *N* 0))  ; 物体のY座標
(define *rv*         (make-f64vector *N* 0))  ; 物体の速度
(define *rc*         (make-f64vector *N* 0))  ; 物体の角度
(define *rr*         (make-f64vector *N* 0))  ; 物体の半径
(define *rm*         (make-f64vector *N* 0))  ; 物体の質量
(define *rcol*       (make-vector    *N* 0))  ; 物体の色番号
(define *rhit*       (make-vector    *N* #f)) ; 衝突フラグ
(define *eng0*       0) ; 全体の運動エネルギーの初期値
(define *eng1*       0) ; 全体の運動エネルギーの現在値

(define *backcolor*  #f32(0.2 0.2 0.2 1.0)) ; 背景色

;; 色情報(16色)
(define *color-table*
  #(#f32(0.0 0.0 0.0 1.0) #f32(0.0 0.0 0.5 1.0) #f32(0.0 0.5 0.0 1.0) #f32(0.0 0.5 0.5 1.0)
    #f32(0.5 0.0 0.0 1.0) #f32(0.5 0.0 0.5 1.0) #f32(0.5 0.5 0.0 1.0) #f32(0.75 0.75 0.75 1.0)
    #f32(0.5 0.5 0.5 1.0) #f32(0.0 0.0 1.0 1.0) #f32(0.0 1.0 0.0 1.0) #f32(0.0 1.0 1.0 1.0)
    #f32(1.0 0.0 0.0 1.0) #f32(1.0 0.0 1.0 1.0) #f32(1.0 1.0 0.0 1.0) #f32(1.0 1.0 1.0 1.0)))

;; ウェイト時間調整クラスのインスタンス生成
(define *wcinfo* (make <waitcalcinfo> :waittime *wait*))


;; 運動エネルギーの計算
(define (calc-energy)
  (do ((i 0 (+ i 1))
       (eng 0 (+ eng (* (/. 1 2) (~ *rm* i) (expt (~ *rv* i) 2)))))
      ((>= i *N*) eng)))

;; 物体の初期化
(define (init-ball)
  ;; 球
  (do ((i 0 (+ i 1)))
      ((>= i *N*))
    (set! (~ *rx* i)   (randint (- *wd/2*) *wd/2*))
    (set! (~ *ry* i)   (randint (- *ht/2*) *ht/2*))
    (set! (~ *rv* i)   (randint 3 6))
    (set! (~ *rc* i)   (* (randint 0 359) pi/180))
    (set! (~ *rr* i)   (randint 30 90))
    (set! (~ *rm* i)   (* (/. 4 3) pi (expt (~ *rr* i) 3)))
    (set! (~ *rcol* i) (randint 10 15))
    (set! (~ *rhit* i) #f)))

;; 物体の表示
(define (disp-ball)
  ;; 球
  (do ((i 0 (+ i 1)))
      ((>= i *N*))
    ;; 色
    (gl-material GL_FRONT GL_DIFFUSE (~ *color-table* (~ *rcol* i)))
    (gl-material GL_FRONT GL_AMBIENT #f32(0.5 0.5 0.5 1.0))
    ;; 座標設定と表示
    (gl-push-matrix)
    (gl-translate (~ *rx* i) (~ *ry* i) 0)
    (glut-solid-sphere (~ *rr* i) 20 20)
    (gl-pop-matrix)))

;; 物体の座標計算
(define (calc-ball)
  ;; 球
  (do ((i 0 (+ i 1)))
      ((>= i *N*))
    ;; 移動
    (inc! (~ *rx* i) (* (~ *rv* i) (cos (~ *rc* i))))
    (inc! (~ *ry* i) (* (~ *rv* i) (sin (~ *rc* i))))
    ;; 外壁を超えたら反対側に移動する
    ;(set! (~ *rx* i) (wrap-range (~ *rx* i) (- *wd/2*) *wd/2*))
    ;(set! (~ *ry* i) (wrap-range (~ *ry* i) (- *ht/2*) *ht/2*))
    ;; 外壁に触れたら跳ね返る
    (let ((minx (+ (- *wd/2*) (~ *rr* i) (- *bval*)))
          (maxx (-    *wd/2*  (~ *rr* i) (- *bval*)))
          (miny (+ (- *ht/2*) (~ *rr* i) (- *bval*)))
          (maxy (-    *ht/2*  (~ *rr* i) (- *bval*))))
      (when (or (< (~ *rx* i) minx)
                (> (~ *rx* i) maxx))
        (set! (~ *rc* i) (+ (- (~ *rc* i)) pi)))
      (when (or (< (~ *ry* i) miny)
                (> (~ *ry* i) maxy))
        (set! (~ *rc* i)    (- (~ *rc* i))))
      (set! (~ *rx* i) (clamp (~ *rx* i) minx maxx))
      (set! (~ *ry* i) (clamp (~ *ry* i) miny maxy))
      (set! (~ *rc* i) (wrap-range (~ *rc* i) 0 2pi)))
    ))

;; 物体の衝突処理
;; (現状、3個以上の物体が重なった場合には、正しい結果にならない)
(define (collision-ball)
  ;; 衝突フラグOFF
  (do ((i 0 (+ i 1)))
      ((>= i *N*))
    (set! (~ *rhit* i) #f))
  ;; 球と球の衝突処理(2重ループ)
  ;;   球1の番号i1  球2の番号i2
  ;;        0        1 ... N-1
  ;;        1        2 ... N-1
  ;;        :            :
  ;;       N-2          N-1
  (let loop ((i1 0) (i2 1))
    ;; 衝突の判定
    (let ((r1 (+ (expt (- (~ *rx* i1) (~ *rx* i2)) 2)
                 (expt (- (~ *ry* i1) (~ *ry* i2)) 2)))
          (r2 (expt (+ (~ *rr* i1) (~ *rr* i2) (- *bval*)) 2)))
      (when (< r1 r2)
        (let* (;; 衝突軸の角度を求める
               (cc    (atan (- (~ *ry* i2) (~ *ry* i1))
                            (- (~ *rx* i2) (~ *rx* i1))))
               ;; 衝突前の速度を求める(衝突軸を基準(x軸)とする直行座標で計算する)
               (v1cx1 (* (~ *rv* i1) (cos (- (~ *rc* i1) cc))))
               (v1cy1 (* (~ *rv* i1) (sin (- (~ *rc* i1) cc))))
               (v1cx2 (* (~ *rv* i2) (cos (- (~ *rc* i2) cc))))
               (v1cy2 (* (~ *rv* i2) (sin (- (~ *rc* i2) cc))))
               ;; 衝突後の速度を求める(衝突軸を基準(x軸)とする直行座標で計算する)
               (v2cx1 (/. (+ (* *C* (~ *rm* i2) (- v1cx2 v1cx1))
                             (* (~ *rm* i1) v1cx1)
                             (* (~ *rm* i2) v1cx2))
                          (+ (~ *rm* i1) (~ *rm* i2))))
               (v2cy1 v1cy1)
               (v2cx2 (/. (+ (* *C* (~ *rm* i1) (- v1cx1 v1cx2))
                             (* (~ *rm* i1) v1cx1)
                             (* (~ *rm* i2) v1cx2))
                          (+ (~ *rm* i1) (~ *rm* i2))))
               (v2cy2 v1cy2)
               ;; 衝突軸上の座標を計算
               (cx1   (* (sqrt (+ (expt (~ *rx* i1) 2) (expt (~ *ry* i1) 2)))
                         (cos  (- (atan (~ *ry* i1) (~ *rx* i1)) cc))))
               (cx2   (* (sqrt (+ (expt (~ *rx* i2) 2) (expt (~ *ry* i2) 2)))
                         (cos  (- (atan (~ *ry* i2) (~ *rx* i2)) cc))))
               )
          ;; 衝突フラグON
          (set! (~ *rhit* i1) #t)
          (set! (~ *rhit* i2) #t)
          ;; 速度の方向をチェックして、近づくケースでは速度を反転する
          ;; (現実には、衝突後に2個の物体が同じ方向に進むケースもあるため、正しくない。
          ;;  しかし、めりこみ時の振動状態を簡単に解消するために、このようにした)
          (cond
           ((< cx1 cx2)
            (when (> v2cx1 0) (set! v2cx1 (- v2cx1)))
            (when (< v2cx2 0) (set! v2cx2 (- v2cx2))))
           (else
            (when (< v2cx1 0) (set! v2cx1 (- v2cx1)))
            (when (> v2cx2 0) (set! v2cx2 (- v2cx2)))))
          ;; 極座標の速度と角度に戻す
          (set! (~ *rv* i1) (sqrt (+ (expt v2cx1 2) (expt v2cy1 2))))
          (set! (~ *rc* i1) (+ (atan v2cy1 v2cx1) cc))
          (set! (~ *rv* i2) (sqrt (+ (expt v2cx2 2) (expt v2cy2 2))))
          (set! (~ *rc* i2) (+ (atan v2cy2 v2cx2) cc))
          )))
    (cond
     ((< i2 (- *N* 1)) (loop i1 (+ i2 1)))
     ((< i1 (- *N* 2)) (loop (+ i1 1) (+ i1 2))))
    ))


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 光源設定
  ;(gl-light  GL_LIGHT0 GL_POSITION #f32(1.0 1.0 1.0 0.0))
  (gl-light  GL_LIGHT0 GL_POSITION #f32(-1.0 1.0 1.0 0.0))
  ;(gl-light  GL_LIGHT0 GL_AMBIENT  #f32( 0.5 0.5 0.5 1.0)) ; 環境光
  (gl-light  GL_LIGHT0 GL_AMBIENT  #f32( 0.35 0.35 0.35 1.0)) ; 環境光
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_NORMALIZE)
  ;; 材質設定
  (gl-material GL_FRONT GL_SPECULAR #f32(1.0 1.0 1.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 30.0)
  ;; 物体の初期化
  (init-ball)
  ;; 全体の運動エネルギーの初期値を計算
  (set! *eng0* (calc-energy)))

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 文字表示
  (let ((str1 (format "Energy=~D (~D%)"
                      (truncate->exact *eng1*)
                      (truncate-n (* (/. *eng1* *eng0*) 100) 1)))
        (z1 0.9))
    (gl-color 0.0 1.0 0.0 1.0)
    (draw-stroke-text str1 0 0 *width* *height* 24 'left z1))
  ;; 物体の表示
  (disp-ball)
  ;; 背景の表示
  (gl-color *backcolor*)
  (draw-win-rect 0 0 *width* *height* *width* *height* 'left -0.999999)
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  w)
  (set! *height* h)
  ;; 画面幅の変化に追従
  (set! *wd/2* (* (/. *width* *height*) *ht/2*))
  ;; 画面のリサイズ
  (gl-viewport 0 0 *width* *height*)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (let1 z1 (/. *ht/2* *tanvan*)
    ;; 透視射影する範囲を設定
    (glu-perspective *vangle* (/. *width* *height*) (- z1 *zd/2*) (+ z1 *zd/2*))
    ;; 視点の位置と方向を設定
    (glu-look-at 0 0 z1 0 0 0 0 1 0)))

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
  ;; 物体の座標計算
  (calc-ball)
  ;; 物体の衝突処理
  (collision-ball)
  ;; 全体の運動エネルギーの現在値を計算
  (set! *eng1* (calc-energy))
  ;; 画面表示
  (glut-post-redisplay)
  ;; ウェイト時間調整
  (glut-timer-func (waitcalc *wcinfo*) timer 0))

;; メイン処理
(define (main args)
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

