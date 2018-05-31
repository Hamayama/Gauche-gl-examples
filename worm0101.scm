;; -*- coding: utf-8 -*-
;;
;; worm0101.scm
;; 2018-5-31 v1.31
;;
;; ＜内容＞
;;   Gauche-gl を使用した、ワームシミュレータです。
;;   (末尾から移動していくタイプ)
;;   矢印キーかマウスボタン1でカーソルを移動します。
;;   空腹状態のワームはカーソルを追跡します。
;;   ESCキーを押すと終了します。
;;
(add-load-path "lib" :relative)
(add-load-path "model" :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use gauche.sequence)
(use math.const)
(use glmintool)
(use gltextscrn)
(use glmodelkit)
(use glwormkit)
;(use model0501)

(define *wait*      20) ; ウェイト(msec)
(define *title* "worm0101") ; ウィンドウのタイトル
(define *width*    624) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)
(define *aratio*     (/. *width* *height*)) ; アスペクト比(計算用)

(define *wd/2*     520) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *zd/2*     100) ; 画面奥行き/2
(define *minx*       (- *wd/2*)) ; X座標の最小値
(define *maxx*       *wd/2*)     ; X座標の最大値
(define *miny*       (- *ht/2*)) ; Y座標の最小値
(define *maxy*       *ht/2*)     ; Y座標の最大値
(define *cx*         0) ; カーソルのX座標
(define *cy*         0) ; カーソルのY座標
(define *cr*        20) ; カーソルの半径
(define *cd*         8) ; カーソルの移動量
(define *wnum*       2) ; ワームの数
(define *wlen*       8) ; ワームの長さ(関節の数)
(define *waku*       4) ; 当たり判定調整用
(define *backcolor*  #f32(0.2 0.2 0.2 1.0)) ; 背景色

(define *flatdisp*   0) ; フラット表示(=0:OFF,=1:ON)

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 2) (* *ht/2* 2))

;; マウス状態管理クラスのインスタンス生成
(define *msinfo* (make <mousestateinfo>))

;; キー入力状態管理クラスのインスタンス生成
(define *ksinfo* (make <keystateinfo>))

;; ウェイト時間調整クラスのインスタンス生成
(define *wcinfo* (make <waitcalcinfo> :waittime *wait*))

;; ワームクラス
;; (ワーム0101クラスを継承)
(define-class <worm> (<worm0101>)
  ((state  :init-value      0)  ; 状態(=0:追跡中,=1:食事中,=2:ランダム動作中)
   (count1 :init-value      0)  ; 動作カウンタ1
   (count2 :init-value      0)  ; 動作カウンタ2
   (wtime1 :init-value   1000)  ; 食事時間(msec)
   (wtime2 :init-value   8000)  ; ランダム動作時間最小値(msec)
   (wtime3 :init-value  15000)  ; ランダム動作時間最大値(msec)
   (wtime4 :init-value   2000)  ; ランダム動作切換時間最小値(msec)
   (wtime5 :init-value   8000)  ; ランダム動作切換時間最大値(msec)
   ))
;; ワームの初期化
;;   anum  関節の数
;;   x     (末尾の)X座標
;;   y     (末尾の)Y座標
;;   c     (末尾の)角度(度)
(define-method worm-init ((w1 <worm>) (anum <integer>) (x <real>) (y <real>) (c <real>))
  (next-method w1 anum x y c))
;; ワームの目標設定
;;   gx  目標のX座標
;;   gy  目標のY座標
(define-method worm-set-goal ((w1 <worm>) (gx <real>) (gy <real>))
  (next-method w1 gx gy))
;; ワームの目標到達チェック
(define-method worm-goal? ((w1 <worm>))
  (worm-goal? w1 *waku*))
;; ワームの移動
(define-method worm-move ((w1 <worm>))
  (define state  (~ w1 'state))
  (define count1 (~ w1 'count1))
  (define count2 (~ w1 'count2))
  ;; 状態によって場合分け
  (case state
    ((0 1) ; 追跡中/食事中
     (set! state 0)
     (worm-set-goal w1 *cx* *cy*)
     (%worm-calc-angle w1)
     (cond
      ((worm-goal? w1)
       (set! state 1)
       (set! count1 (+ count1 *wait*))
       (when (>= count1 (~ w1 'wtime1))
         (set! state 2)))
      (else
       (%worm-move-tail w1)))
     (%worm-calc-point w1)
     ;; 3分で強制移行(永久パターン防止のため)
     (set! count2 (+ count2 *wait*))
     (when (>= count2 180000)
       (set! state 2))
     (when (= state 2)
       (set! count1 (randint (~ w1 'wtime2) (~ w1 'wtime3)))
       (set! count2 (randint (~ w1 'wtime4) (~ w1 'wtime5)))
       (worm-set-goal w1 (randint *minx* *maxx*) (randint *miny* *maxy*))
       ))
    ((2) ; ランダム動作中
     (%worm-calc-angle w1)
     (if (worm-goal? w1)
       (set! count2 0)
       (%worm-move-tail w1))
     (%worm-calc-point w1)
     (set! count1 (- count1 *wait*))
     (set! count2 (- count2 *wait*))
     (cond
      ((<= count1 0)
       (set! state 0)
       (set! count1 0)
       (set! count2 0))
      ((<= count2 0)
       (set! count2 (randint (~ w1 'wtime4) (~ w1 'wtime5)))
       (worm-set-goal w1(randint *minx* *maxx*) (randint *miny* *maxy*))
       ))))
  (set! (~ w1 'state)  state)
  (set! (~ w1 'count1) count1)
  (set! (~ w1 'count2) count2))
;; ワームの表示
(define-method worm-disp ((w1 <worm>))
  (define color (case (~ w1 'state)
                  ((0) (if (= *flatdisp* 0)
                         #f32(0.5 0.0 0.0 1.0)
                         #f32(0.9 0.5 0.2 1.0)))
                  ((1) (if (= *flatdisp* 0)
                         #f32(0.0 0.7 0.3 1.0)
                         #f32(0.0 0.8 0.4 1.0)))
                  ((2) #f32(1.0 1.0 1.0 1.0))))
  (define wedge (case (~ w1 'state)
                  ((0) 120)
                  ((1) (randint 0 90))
                  ((2) 70)))
  (if (= *flatdisp* 0)
    (worm-disp w1 color wedge)
    (worm-disp-flat w1 *win* color wedge)))
;; ワームクラスのインスタンス生成
(define *worms* (make-vector-of-class *wnum* <worm>))
(for-each
 (lambda (w1)
   (worm-init w1
              *wlen*
              (randint *minx* *maxx*)
              (randint *miny* *maxy*)
              (randint -180 179)))
 *worms*)


;; カーソルの表示
(define (disp-cursor)
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 0.0 1.0))
  (gl-material GL_FRONT GL_AMBIENT #f32(0.5 0.5 0.0 1.0))
  (gl-push-matrix)
  (gl-translate *cx* *cy* 0)
  (cross-cursor *cr* (/. *cr* 10))
  (gl-pop-matrix))

;; カーソルの移動
(define (move-cursor)
  (cond
   ((mouse-button? *msinfo* GLUT_LEFT_BUTTON)
    (set! *cx* (clamp (win-gl-x *win* (mouse-x *msinfo*)) (- *wd/2*) *wd/2*))
    (set! *cy* (clamp (win-gl-y *win* (mouse-y *msinfo*)) (- *ht/2*) *ht/2*)))
   (else
    (let ((vx 0) (vy 0))
      (if (spkey-on? *ksinfo* GLUT_KEY_LEFT)  (set! vx (- vx *cd*)))
      (if (spkey-on? *ksinfo* GLUT_KEY_RIGHT) (set! vx (+ vx *cd*)))
      (if (spkey-on? *ksinfo* GLUT_KEY_DOWN)  (set! vy (- vy *cd*)))
      (if (spkey-on? *ksinfo* GLUT_KEY_UP)    (set! vy (+ vy *cd*)))
      (unless (= vx 0) (set! *cx* (wrap-range (+ *cx* vx) (- *wd/2*) *wd/2*)))
      (unless (= vy 0) (set! *cy* (wrap-range (+ *cy* vy) (- *ht/2*) *ht/2*))))
    )))


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
  ;; カーソルの表示
  (disp-cursor)
  ;; ワームの表示
  (for-each (lambda (w1) (worm-disp w1)) *worms*)
  ;; 背景の表示
  (gl-color *backcolor*)
  (draw-win-rect 0 0 *width* *height* *width* *height* 'left -0.999999)
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  (min w (truncate->exact (*  h *aratio*))))
  (set! *height* (min h (truncate->exact (/. w *aratio*))))
  (win-update-size *win* *width* *height*)
  (mouse-update-offset *msinfo* (quotient (- w *width*)  2) (quotient (- h *height*) 2))
  ;; 縦横比を変えずにリサイズ
  (gl-viewport (quotient (- w *width*) 2) (quotient (- h *height*) 2) *width* *height*)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (let1 z1 (/. *ht/2* *tanvan*)
    ;; 透視射影する範囲を設定
    (glu-perspective *vangle* (/. *width* *height*) (- z1 *zd/2*) (+ z1 *zd/2*))
    ;; 視点の位置と方向を設定
    (glu-look-at 0 0 z1 0 0 0 0 1 0)))

;; キー入力ON
(define (keyboard key x y)
  (key-on *ksinfo* key)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit 0))
   ;; [g]キーでGC実行(デバッグ用)
   ((or (= key (char->integer #\g)) (= key (char->integer #\G)))
    (gc) (print (gc-stat)))
   ))

;; キー入力OFF
(define (keyboardup key x y)
  (key-off *ksinfo* key))

;; 特殊キー入力ON
(define (specialkey key x y)
  (spkey-on *ksinfo* key))

;; 特殊キー入力OFF
(define (specialkeyup key x y)
  (spkey-off *ksinfo* key))

;; マウスボタン
(define (mouse button state x y)
  (mouse-button *msinfo* button state)
  (mouse-move *msinfo* x y))

;; マウスドラッグ
(define (mousedrag x y)
  (mouse-move *msinfo* x y))

;; タイマー
(define (timer val)
  ;; カーソルの移動
  (move-cursor)
  ;; ワームの移動
  (for-each (lambda (w1) (worm-move w1)) *worms*)
  ;; 画面表示
  (glut-post-redisplay)
  ;; ウェイト時間調整
  (glut-timer-func (waitcalc *wcinfo*) timer 0))

;; メイン処理
(define (main args)
  (set! *flatdisp* (x->integer (list-ref args 1 0)))
  (glut-init '())
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size *width* *height*)
  (glut-init-window-position 100 100)
  (glut-create-window *title*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-keyboard-up-func keyboardup)
  (glut-special-func specialkey)
  (glut-special-up-func specialkeyup)
  (glut-mouse-func mouse)
  (glut-motion-func mousedrag)
  (glut-timer-func *wait* timer 0)
  ;; コールバック内エラー対策
  (guard (ex (else (report-error ex) (exit 1)))
    (glut-main-loop))
  0)

