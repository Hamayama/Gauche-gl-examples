;; -*- coding: utf-8 -*-
;;
;; pendulum.scm
;; 2018-8-9 v1.02
;;
;; ＜内容＞
;;   Gauche-gl を使用した、振り子シミュレータです。
;;   ラグランジュの運動方程式とルンゲクッタ法により
;;   近似解を求めています。
;;   ESCキーを押すと終了します。
;;
(add-load-path "lib" :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use gauche.sequence)
(use gauche.array)
(use math.const)
(use glmintool)
(use gltextscrn)
(use glmodelkit)

(define *wait*      20) ; ウェイト(msec)
(define *title* "pendulum") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (* (/. *vangle* 2) pi/180))) ; 視野角/2のタンジェント(計算用)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *zd/2*     100) ; 画面奥行き/2

(define *N*          4) ; 振り子の数
(define *ox*         0) ; 支点のX座標
(define *oy*       200) ; 支点のY座標
(define *or*         8) ; 支点の半径
(define *sr*         (make-f64vector *N* (/. 400 *N*))) ; ひもの長さ
(define *sc*         (make-f64vector *N* 0))  ; ひもの角度(ラジアン)
(define *sv*         (make-f64vector *N* 0))  ; ひもの角度の速度
(define *r*          (make-f64vector *N* 25)) ; 重りの半径
(define *m*          (make-f64vector *N* 10)) ; 重りの質量
(define *g*         10) ; 重力加速度
(define *t*          0) ; 時間
(define *h*       0.38) ; 時間の増分

;; ひもの角度の初期値を設定
(do ((i 0 (+ i 1))) ((>= i *N*) #f)
  (f64vector-set! *sc* i (* (/. 80 *N*) pi/180 (+ i 1))))

(define *forecolor*  #f32(1.0 1.0 1.0 1.0)) ; 振り子の色
(define *backcolor*  #f32(0.2 0.2 0.2 1.0)) ; 背景色

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 2) (* *ht/2* 2))

;; ウェイト時間調整クラスのインスタンス生成
(define *wcinfo* (make <waitcalcinfo> :waittime *wait*))


;; 振り子の表示
(define (disp-pendulum)
  ;; 色
  (gl-material GL_FRONT GL_DIFFUSE *forecolor*)
  (gl-material GL_FRONT GL_AMBIENT #f32(0.5 0.5 0.5 1.0))
  ;; 支点
  (gl-push-matrix)
  (gl-translate *ox* *oy* 0)
  (glut-solid-sphere *or* 20 20)
  (gl-pop-matrix)
  (do ((i   0    (+ i 1))
       (x1  *ox* x2)
       (y1  *oy* y2)
       (sr1 0    0)
       (sc1 0    0)
       (x2  0    0)
       (y2  0    0))
      ((>= i *N*) #f)
    (set! sr1 (f64vector-ref *sr* i))
    (set! sc1 (f64vector-ref *sc* i))
    (set! x2  (+ x1 (* sr1 (cos (+ sc1 pi/2)))))
    (set! y2  (- y1 (* sr1 (sin (+ sc1 pi/2)))))
    ;; ひも
    (gl-push-matrix)
    (gl-translate x1 y1 0)
    (gl-rotate (- (* sc1 180/pi)) 0 0 1)
    (cylinder 2 (/. sr1 2) 20)
    (gl-pop-matrix)
    ;; 重り
    (gl-push-matrix)
    (gl-translate x2 y2 0)
    (glut-solid-sphere (f64vector-ref *r* i) 20 20)
    (gl-pop-matrix)))

;; 振り子の座標計算
(define (calc-pendulum)
  ;; ルンゲクッタ法
  (runge *N* *t* *sc* *sv* ffunc gfunc *h*))

;; ひもの角度の速度
(define (ffunc N t sc sv sv_ret)
  (f64vector-copy! sv_ret 0 sv))

;; ひもの角度の加速度
;; (運動方程式 A x SA = B について、
;;  Aの逆行列Cを両辺にかけることで、
;;  加速度SAを求める)
(define (gfunc N t sc sv sa)
  (define A  (make-f64array (shape 0 N 0 N) 0))
  (define B  (make-f64array (shape 0 N 0 1) 0))
  (define C  #f)
  (define SA #f)
  ;; 質量の和
  (define (msum start end)
    (do ((i     start (+ i 1))
         (msum1 0     (+ msum1 (f64vector-ref *m* i))))
        ((>= i end) msum1)))

  ;; 行列Aを計算
  ;;   a[i][j] = (m[Max(i,j)]+...+m[N-1])*sr[j]*cos(sc[i]-sc[j])
  (array-for-each-index
   A
   (lambda (i j)
     (array-set! A i j (* (msum (max i j) N)
                          (f64vector-ref *sr* j)
                          (cos (- (f64vector-ref sc i) (f64vector-ref sc j)))))))
  ;; ベクトルBを計算
  ;;   b[i][0] = Sum(j=0...N-1){
  ;;               -(m[Max(i,j)]+...+m[N-1])*sr[j]*sv[j]*sv[j]*sin(sc[i]-sc[j])
  ;;             }
  ;;             -(m[i]...m[N-1])*g*sin(sc[i])
  (array-for-each-index
   B
   (lambda (i dummy)
     (do ((j 0 (+ j 1)))
         ((>= j N) #f)
       (array-set! B i 0 (- (array-ref B i 0)
                            (* (msum (max i j) N)
                               (f64vector-ref *sr* j)
                               (f64vector-ref sv   j)
                               (f64vector-ref sv   j)
                               (sin (- (f64vector-ref sc i) (f64vector-ref sc j)))))))
     (array-set! B i 0 (- (array-ref B i 0)
                          (* (msum i N)
                             *g*
                             (sin (f64vector-ref sc i)))))))
  ;; 逆行列Cを計算
  (set! C (array-inverse A))
  (when C
    ;; 加速度SAを計算
    (set! SA (array-mul C B))
    (array-for-each-index
     SA
     (lambda (i dummy)
       (f64vector-set! sa i (array-ref SA i 0))))))

;; ルンゲクッタ法(4次)(2N+1変数)
(define (runge N t sc sv ffunc gfunc h)
  (define sc-temp (make-f64vector N 0)) ; ひもXの角度(計算用)
  (define sv-temp (make-f64vector N 0)) ; ひもXの角度の速度(計算用)
  (define c1 (make-f64vector N 0)) ; ひもXの角度の増分1
  (define c2 (make-f64vector N 0)) ; ひもXの角度の増分2
  (define c3 (make-f64vector N 0)) ; ひもXの角度の増分3
  (define c4 (make-f64vector N 0)) ; ひもXの角度の増分4
  (define v1 (make-f64vector N 0)) ; ひもXの角度の速度の増分1
  (define v2 (make-f64vector N 0)) ; ひもXの角度の速度の増分2
  (define v3 (make-f64vector N 0)) ; ひもXの角度の速度の増分3
  (define v4 (make-f64vector N 0)) ; ひもXの角度の速度の増分4
  ;; 増分1を計算
  (ffunc N t sc sv c1)
  (gfunc N t sc sv v1)
  (set! c1 (f64vector-mul! c1 h))
  (set! v1 (f64vector-mul! v1 h))
  ;; 増分2を計算
  (f64vector-copy! sc-temp 0 sc)
  (f64vector-copy! sv-temp 0 sv)
  (set! sc-temp (f64vector-add! sc-temp (f64vector-div c1 2)))
  (set! sv-temp (f64vector-add! sv-temp (f64vector-div v1 2)))
  (ffunc N (+ t (/. h 2)) sc-temp sv-temp c2)
  (gfunc N (+ t (/. h 2)) sc-temp sv-temp v2)
  (set! c2 (f64vector-mul! c2 h))
  (set! v2 (f64vector-mul! v2 h))
  ;; 増分3を計算
  (f64vector-copy! sc-temp 0 sc)
  (f64vector-copy! sv-temp 0 sv)
  (set! sc-temp (f64vector-add! sc-temp (f64vector-div c2 2)))
  (set! sv-temp (f64vector-add! sv-temp (f64vector-div v2 2)))
  (ffunc N (+ t (/. h 2)) sc-temp sv-temp c3)
  (gfunc N (+ t (/. h 2)) sc-temp sv-temp v3)
  (set! c3 (f64vector-mul! c3 h))
  (set! v3 (f64vector-mul! v3 h))
  ;; 増分4を計算
  (f64vector-copy! sc-temp 0 sc)
  (f64vector-copy! sv-temp 0 sv)
  (set! sc-temp (f64vector-add! sc-temp c3))
  (set! sv-temp (f64vector-add! sv-temp v3))
  (ffunc N (+ t h) sc-temp sv-temp c4)
  (gfunc N (+ t h) sc-temp sv-temp v4)
  (set! c4 (f64vector-mul! c4 h))
  (set! v4 (f64vector-mul! v4 h))
  ;; 増分1-4の平均を加算
  (do ((i 0 (+ i 1)))
      ((>= i N) #f)
    (f64vector-set! sc i (+ (f64vector-ref sc i)
                            (/. (+ (f64vector-ref c1 i)
                                   (* (f64vector-ref c2 i) 2)
                                   (* (f64vector-ref c3 i) 2)
                                   (f64vector-ref c4 i))
                                6)))
    (f64vector-set! sv i (+ (f64vector-ref sv i)
                            (/. (+ (f64vector-ref v1 i)
                                   (* (f64vector-ref v2 i) 2)
                                   (* (f64vector-ref v3 i) 2)
                                   (f64vector-ref v4 i))
                                6)))))


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 光源設定
  ;(gl-light  GL_LIGHT0 GL_POSITION #f32(1.0 1.0 1.0 0.0))
  (gl-light  GL_LIGHT0 GL_POSITION #f32(-1.0 1.0 1.0 0.0))
  (gl-light  GL_LIGHT0 GL_AMBIENT  #f32( 0.5 0.5 0.5 1.0)) ; 環境光
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_NORMALIZE)
  ;; 材質設定
  (gl-material GL_FRONT GL_SPECULAR #f32(1.0 1.0 1.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 30.0))

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 振り子の表示
  (disp-pendulum)
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
  ;; 振り子の座標計算
  (calc-pendulum)
  ;; 時間を進める
  (set! *t* (+ *t* *h*))
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

