;; -*- coding: utf-8 -*-
;;
;; worm.scm
;; 2018-5-2 v1.00
;;
;; ＜内容＞
;;   Gauche-gl を使用した、ワームシミュレータです。
;;   矢印キーかマウスボタン1でカーソルを移動します。
;;   ワームはカーソルを追跡します。
;;   ESCキーを押すと終了します。
;;
(add-load-path "lib" :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use gauche.sequence)
(use math.const)
(use glmintool)
(use gltextscrn)
(use glmodelkit)

(define *wait*      20) ; ウェイト(msec)
(define *title* "worm") ; ウィンドウのタイトル
(define *width*    624) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)
(define *aratio*     (/. *width* *height*)) ; アスペクト比(計算用)

(define *wd/2*   52000) ; 画面幅/2
(define *ht/2*   40000) ; 画面高さ/2
(define *zd/2*   10000) ; 画面奥行き/2
(define *cx*     41600) ; カーソルのX座標
(define *cy*    -32000) ; カーソルのY座標
(define *cr*      2000) ; カーソルの半径
(define *cd*       800) ; カーソルの移動量
(define *wnum*       2) ; ワームの数
(define *waku*     400) ; 当たり判定調整用
(define *backcolor*  #f32(0.2 0.2 0.2 1.0)) ; 背景色

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 2) (* *ht/2* 2))

;; キー入力状態管理クラスのインスタンス生成
(define *ksinfo* (make <keystateinfo>))

;; ウェイト時間調整クラスのインスタンス生成
(define *wcinfo* (make <waitcalcinfo> :waittime *wait*))

;; マウスボタン1状態
(define *mouse-button1* #f)

;; ワームクラス
(define-class <worm> ()
  ((rx    :init-value    0)  ; 末尾のX座標
   (ry    :init-value    0)  ; 末尾のY座標
   (rr    :init-value 2000)  ; 末尾の半径
   (rv    :init-value  100)  ; 末尾の速度
   (rcv   :init-value  1.0)  ; 末尾の角速度(度)
   (anum  :init-value    8)  ; 関節のパーツの数
   (ax    :init-value    #f) ; 関節のパーツのX座標(ベクタ)
   (ay    :init-value    #f) ; 関節のパーツのY座標(ベクタ)
   (ar    :init-value 1000)  ; 関節のパーツの半径
   (al    :init-value 4000)  ; 関節のパーツの距離
   (ac    :init-value    #f) ; 関節のパーツの角度(ベクタ)
   (acv   :init-value  0.2)  ; 関節のパーツの角速度(度)
   (maxac :init-value   45)  ; 関節のパーツの角度の最大値(度)
   (fx    :init-value    0)  ; 先端のX座標
   (fy    :init-value    0)  ; 先端のY座標
   (fr    :init-value 2000)  ; 先端の半径
   (fc    :init-value    0)  ; 先端の角度(度)
   ))
;; ワームの初期化
(define-method worm-init ((w1 <worm>) (rx <real>) (ry <real>) (rc <real>))
  (define anum  (~ w1 'anum))
  (set! (~ w1 'rx)   rx)
  (set! (~ w1 'ry)   ry)
  (set! (~ w1 'ax)   (make-vector (+ anum 1) 0))
  (set! (~ w1 'ay)   (make-vector (+ anum 1) 0))
  (set! (~ w1 'ac)   (make-vector (+ anum 1) 0))
  (set! (~ w1 'ac 0) rc))
;; ワームの角度計算
(define-method worm-calc-angle ((w1 <worm>) (gx <real>) (gy <real>))
  (define anum  (~ w1 'anum))
  (define rcv   (~ w1 'rcv))
  (define acv   (~ w1 'acv))
  (define maxac (~ w1 'maxac))
  (define fx    (~ w1 'fx))
  (define fy    (~ w1 'fy))
  (define (get-ax i) (if (>= i 0) (~ w1 'ax i) (~ w1 'rx)))
  (define (get-ay i) (if (>= i 0) (~ w1 'ay i) (~ w1 'ry)))
  ;; 先端から順番に目標に近づけていく
  (do ((i anum (- i 1)))
      ((< i 0) #f)
    (let* ((gx1   (- gx (get-ax (- i 1))))
           (gy1   (- gy (get-ay (- i 1))))
           (fx1   (- fx (get-ax (- i 1))))
           (fy1   (- fy (get-ay (- i 1))))
           (c1    (* (atan gy1 gx1) 180/pi))
           (c2    (* (atan fy1 fx1) 180/pi))
           (diffc (wrap-range (- c1 c2) -180 180))
           (ac1   0))
      (cond
       ((= i 0)
        ;; 末尾の角度を計算
        (set! diffc (clamp diffc (- rcv) rcv))
        (set! (~ w1 'ac 0) (wrap-range (+ (~ w1 'ac 0) diffc) -180 180)))
       (else
        ;; 関節のパーツの角度を計算
        (set! diffc (clamp diffc (- acv) acv))
        (set! ac1 (+ (~ w1 'ac i) diffc))
        (when (> (abs ac1) maxac)
          (set! ac1 (clamp ac1 (- maxac) maxac))
          (set! diffc (- ac1 (~ w1 'ac i))))
        (set! (~ w1 'ac i) ac1)))
      ;; 先端の座標を補正して繰り返す
      (set! fx (+ (get-ax (- i 1))
                  (- (* fx1 (cos (* diffc pi/180)))
                     (* fy1 (sin (* diffc pi/180))))))
      (set! fy (+ (get-ay (- i 1))
                  (+ (* fx1 (sin (* diffc pi/180)))
                     (* fy1 (cos (* diffc pi/180))))))
      )))
;; ワームの座標計算
(define-method worm-calc-point ((w1 <worm>) (gx <real>) (gy <real>))
  (define acsum -90)
  (define anum  (~ w1 'anum))
  (define al    (~ w1 'al))
  (define fx    (~ w1 'fx))
  (define fy    (~ w1 'fy))
  (define rv    (~ w1 'rv))
  (define (get-ax i) (if (>= i 0) (~ w1 'ax i) (~ w1 'rx)))
  (define (get-ay i) (if (>= i 0) (~ w1 'ay i) (~ w1 'ry)))
  ;; 末尾を移動
  (unless (recthit? (- gx *waku*) (- gy *waku*) (* *waku* 2) (* *waku* 2)
                    (- fx *waku*) (- fy *waku*) (* *waku* 2) (* *waku* 2))
    (set! (~ w1 'rx) (clamp (+ (~ w1 'rx) (* rv (cos (* (- (~ w1 'ac 0) 90) pi/180))))
                            (- *wd/2*) *wd/2*))
    (set! (~ w1 'ry) (clamp (+ (~ w1 'ry) (* rv (sin (* (- (~ w1 'ac 0) 90) pi/180))))
                            (- *ht/2*) *ht/2*))
    )
  ;; 関節のパーツと先端の座標を計算
  (do ((i 0 (+ i 1)))
      ((> i anum) #f)
    (set! acsum (+ acsum (~ w1 'ac i)))
    (set! (~ w1 'ax i) (+ (get-ax (- i 1)) (* al (cos (* acsum pi/180)))))
    (set! (~ w1 'ay i) (+ (get-ay (- i 1)) (* al (sin (* acsum pi/180)))))
    )
  (set! (~ w1 'fx) (~ w1 'ax anum))
  (set! (~ w1 'fy) (~ w1 'ay anum))
  (set! (~ w1 'fc) acsum))
;; ワームの表示
(define-method worm-disp ((w1 <worm>))
  (define anum  (~ w1 'anum))
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 1.0 1.0))
  (gl-material GL_FRONT GL_AMBIENT #f32(0.5 0.5 0.5 1.0))
  ;; 末尾
  (gl-push-matrix)
  (gl-translate (~ w1 'rx) (~ w1 'ry) 0)
  (glut-solid-sphere (~ w1 'rr) 20 20)
  (gl-pop-matrix)
  (do ((i 0 (+ i 1)))
      ((> i anum) #f)
    (gl-push-matrix)
    (cond
     ((< i anum)
      ;; 関節のパーツ
      (gl-translate (~ w1 'ax i) (~ w1 'ay i) 0)
      (glut-solid-sphere (~ w1 'ar) 20 20))
     (else
      ;; 先端
      (gl-translate (~ w1 'fx) (~ w1 'fy) 0)
      (gl-rotate (- (~ w1 'fc) 45) 0 0 1)
      (gl-rotate -90 1 0 0)
      (model0501 (~ w1 'fr) 20 20)))
    (gl-pop-matrix)
    ))
;; ワームクラスのインスタンス生成
(define *worms* (make-vector-of-class *wnum* <worm>))
(for-each
 (lambda (w1) (worm-init w1 (randint (- *wd/2*) *wd/2*)
                            (randint (- *ht/2*) *ht/2*)
                            (randint -180 180))
              (worm-calc-point w1 (~ w1 'fx) (~ w1 'fy)))
 *worms*)


;; モデル0501(欠けた球)(中心に原点あり)
;;   r      半径
;;   slice  y軸のまわりの分割数
;;   stack  y軸に垂直な分割数
;; (define (model0501 r slice stack) ...)
(load (make-fpath *app-dpath* "model/model0501.scm"))

;; カーソル(中心に原点あり)
(define (cursor r)
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 0.0 1.0))
  (gl-material GL_FRONT GL_AMBIENT #f32(0.0 0.0 0.0 1.0))
  (cross-cursor r (/. r 10)))

;; カーソルの移動
(define (move-cursor)
  (let ((vx 0) (vy 0))
    (if (spkey-on? *ksinfo* GLUT_KEY_LEFT)  (set! vx (- *cd*)))
    (if (spkey-on? *ksinfo* GLUT_KEY_RIGHT) (set! vx    *cd*))
    (if (spkey-on? *ksinfo* GLUT_KEY_DOWN)  (set! vy (- *cd*)))
    (if (spkey-on? *ksinfo* GLUT_KEY_UP)    (set! vy    *cd*))
    (unless (= vx 0) (set! *cx* (wrap-range (+ *cx* vx) (- *wd/2*) *wd/2*)))
    (unless (= vy 0) (set! *cy* (wrap-range (+ *cy* vy) (- *ht/2*) *ht/2*)))
    ))


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
  (gl-material GL_FRONT GL_SHININESS 10.0)
  ;; 透過設定
  (gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (gl-enable GL_BLEND))

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; カーソルの表示
  (gl-push-matrix)
  (gl-translate *cx* *cy* 0)
  (cursor *cr*)
  (gl-pop-matrix)
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
  (when (= button GLUT_LEFT_BUTTON)
    (set! *mouse-button1* (= state GLUT_DOWN)))
  (mousedrag x y))

;; マウスドラッグ
(define (mousedrag x y)
  (when *mouse-button1*
    (set! *cx* (clamp (win-gl-x *win* x) (- *wd/2*) *wd/2*))
    (set! *cy* (clamp (win-gl-y *win* y) (- *ht/2*) *ht/2*))))

;; タイマー
(define (timer val)
  ;; カーソルの移動
  (move-cursor)
  ;; ワームの移動
  (for-each
   (lambda (w1) (worm-calc-angle w1 *cx* *cy*)
                (worm-calc-point w1 *cx* *cy*))
   *worms*)
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

