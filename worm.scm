;; -*- coding: utf-8 -*-
;;
;; worm.scm
;; 2018-5-4 v1.09
;;
;; ＜内容＞
;;   Gauche-gl を使用した、ワームシミュレータです。
;;   矢印キーかマウスボタン1でカーソルを移動します。
;;   空腹状態のワームはカーソルを追跡します。
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
(define *cx*         0) ; カーソルのX座標
(define *cy*         0) ; カーソルのY座標
(define *cr*      2000) ; カーソルの半径
(define *cd*       800) ; カーソルの移動量
(define *wnum*       2) ; ワームの数
(define *wlen*       8) ; ワームの長さ(関節の数)
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

;; マウスドラッグ用
(define *mouse-button1* #f)
(define *mouse-offsetx* 0)
(define *mouse-offsety* 0)

;; ワームクラス
(define-class <worm> ()
  ((state  :init-value     0)  ; 状態(=0:追跡中,=1:食事中,=2:ランダム動作中)
   (count1 :init-value     0)  ; 動作カウンタ1
   (count2 :init-value     0)  ; 動作カウンタ2
   (rgx    :init-value     0)  ; ランダム動作中の目標のX座標
   (rgy    :init-value     0)  ; ランダム動作中の目標のY座標
   (wtime1 :init-value  1000)  ; 食事時間(msec)
   (wtime2 :init-value  8000)  ; ランダム動作時間最小値(msec)
   (wtime3 :init-value 15000)  ; ランダム動作時間最大値(msec)
   (wtime4 :init-value  2000)  ; ランダム動作切換時間最小値(msec)
   (wtime5 :init-value  8000)  ; ランダム動作切換時間最大値(msec)
   ;; 以下は多関節表示用
   (rx     :init-value     0)  ; 末尾のX座標
   (ry     :init-value     0)  ; 末尾のY座標
   (rr     :init-value  2000)  ; 末尾の半径
   (rv     :init-value   100)  ; 末尾の速度
   (rcv    :init-value   1.0)  ; 末尾の角速度(度)
   (anum   :init-value     8)  ; 関節の数
   (ax     :init-value     #f) ; 関節のX座標(ベクタ)
   (ay     :init-value     #f) ; 関節のY座標(ベクタ)
   (ar     :init-value  1000)  ; 関節の半径
   (al     :init-value  4000)  ; 関節の距離
   (ac     :init-value     #f) ; 関節の角度(度)(ベクタ)
   (acv    :init-value   0.2)  ; 関節の角速度(度)
   (maxac  :init-value    45)  ; 関節の角度の最大値(度)
   (fx     :init-value     0)  ; 先端のX座標
   (fy     :init-value     0)  ; 先端のY座標
   (fr     :init-value  2000)  ; 先端の半径
   (fc     :init-value     0)  ; 先端の角度(度)
   ))
;; ワームの初期化
;;   anum  関節の数
;;   rx    末尾のX座標
;;   ry    末尾のY座標
;;   rc    末尾の角度(度)
(define-method worm-init ((w1 <worm>) (anum <integer>) (rx <real>) (ry <real>) (rc <real>))
  (set! (~ w1 'anum) anum)
  (set! (~ w1 'rx)   rx)
  (set! (~ w1 'ry)   ry)
  (set! (~ w1 'ax)   (make-vector (+ anum 1) 0))
  (set! (~ w1 'ay)   (make-vector (+ anum 1) 0))
  (set! (~ w1 'ac)   (make-vector (+ anum 1) 0))
  (set! (~ w1 'ac 0) rc)
  (%worm-calc-point w1))
;; ワームの移動
;;   gx  目標のX座標
;;   gy  目標のY座標
(define-method worm-move ((w1 <worm>) (gx <real>) (gy <real>))
  (define state  (~ w1 'state))
  (define count1 (~ w1 'count1))
  (define count2 (~ w1 'count2))
  (define rgx    (~ w1 'rgx))
  (define rgy    (~ w1 'rgy))
  (define wtime1 (~ w1 'wtime1))
  (define wtime2 (~ w1 'wtime2))
  (define wtime3 (~ w1 'wtime3))
  (define wtime4 (~ w1 'wtime4))
  (define wtime5 (~ w1 'wtime5))
  ;; 状態によって場合分け
  (case state
    ((0 1) ; 追跡中/食事中
     (set! state 0)
     (%worm-calc-angle w1 gx gy)
     (when (%worm-move-tail w1 gx gy)
       (set! state 1)
       (set! count1 (+ count1 *wait*))
       (when (>= count1 wtime1)
         (set! state 2)))
     (%worm-calc-point w1)
     (set! count2 (+ count2 *wait*))
     ;; 3分で強制移行
     ;; (まれにS字になって回り続けるケースがあるため)
     (when (>= count2 180000)
       (set! state 2))
     (when (= state 2)
       (set! count1 (randint wtime2 wtime3))
       (set! count2 (randint wtime4 wtime5))
       (set! (~ w1 'rgx) (randint (- *wd/2*) *wd/2*))
       (set! (~ w1 'rgy) (randint (- *ht/2*) *ht/2*))))
    ((2) ; ランダム動作中
     (%worm-calc-angle w1 rgx rgy)
     (when (%worm-move-tail w1 rgx rgy)
       (set! count2 0))
     (%worm-calc-point w1)
     (set! count1 (- count1 *wait*))
     (set! count2 (- count2 *wait*))
     (cond
      ((<= count1 0)
       (set! state 0)
       (set! count1 0)
       (set! count2 0))
      ((<= count2 0)
       (set! count2 (randint wtime4 wtime5))
       (set! (~ w1 'rgx) (randint (- *wd/2*) *wd/2*))
       (set! (~ w1 'rgy) (randint (- *ht/2*) *ht/2*))))))
  (set! (~ w1 'state)  state)
  (set! (~ w1 'count1) count1)
  (set! (~ w1 'count2) count2))
;; ワームの角度計算(内部処理用)
;;   gx  目標のX座標
;;   gy  目標のY座標
(define (%worm-calc-angle w1 gx gy)
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
        ;; 関節の角度を計算
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
;; ワームの末尾移動(内部処理用)
;;   gx  目標のX座標
;;   gy  目標のY座標
;;   戻り値  目標に到達していれば #t を返す。そうでなければ #f を返す。
(define (%worm-move-tail w1 gx gy)
  (define fx    (~ w1 'fx))
  (define fy    (~ w1 'fy))
  (define rv    (~ w1 'rv))
  ;; 目標に到達していなければ、末尾を移動する
  (cond
   ((recthit? (- gx *waku*) (- gy *waku*) (* *waku* 2) (* *waku* 2)
              (- fx *waku*) (- fy *waku*) (* *waku* 2) (* *waku* 2))
    #t)
   (else
    (set! (~ w1 'rx) (clamp (+ (~ w1 'rx) (* rv (cos (* (- (~ w1 'ac 0) 90) pi/180))))
                            (- *wd/2*) *wd/2*))
    (set! (~ w1 'ry) (clamp (+ (~ w1 'ry) (* rv (sin (* (- (~ w1 'ac 0) 90) pi/180))))
                            (- *ht/2*) *ht/2*))
    #f)))
;; ワームの座標計算(内部処理用)
(define (%worm-calc-point w1)
  (define acsum -90)
  (define anum  (~ w1 'anum))
  (define al    (~ w1 'al))
  (define (get-ax i) (if (>= i 0) (~ w1 'ax i) (~ w1 'rx)))
  (define (get-ay i) (if (>= i 0) (~ w1 'ay i) (~ w1 'ry)))
  ;; 関節と先端の座標を計算
  ;; (末尾の方から順番に計算していく)
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
  (define ar    (~ w1 'ar))
  (case (~ w1 'state)
    ((0) (gl-material GL_FRONT GL_DIFFUSE #f32(0.5 0.0 0.0 1.0)))
    ((1) (gl-material GL_FRONT GL_DIFFUSE #f32(0.0 0.7 0.3 1.0)))
    ((2) (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 1.0 1.0))))
  (gl-material GL_FRONT GL_AMBIENT #f32(0.5 0.5 0.5 1.0))
  ;; 末尾
  (gl-push-matrix)
  (gl-translate (~ w1 'rx) (~ w1 'ry) 0)
  (glut-solid-sphere (~ w1 'rr) 20 20)
  (gl-pop-matrix)
  ;; 関節
  (do ((i 0 (+ i 1)))
      ((>= i anum) #f)
    (gl-push-matrix)
    (gl-translate (~ w1 'ax i) (~ w1 'ay i) 0)
    (glut-solid-sphere ar 20 20)
    (gl-pop-matrix))
  ;; 先端
  (gl-push-matrix)
  (gl-translate (~ w1 'fx) (~ w1 'fy) 0)
  (gl-rotate (~ w1 'fc) 0 0 1)
  (gl-rotate -90 1 0 0)
  (model0501 (~ w1 'fr) 20 20
             (case (~ w1 'state)
               ((0) 120)
               ((1) (randint 0 90))
               ((2) 70)))
  (gl-pop-matrix))
;; ワームクラスのインスタンス生成
(define *worms* (make-vector-of-class *wnum* <worm>))
(for-each
 (lambda (w1)
   (worm-init w1 *wlen*
              (randint (- *wd/2*) *wd/2*)
              (randint (- *ht/2*) *ht/2*)
              (randint -180 180)))
 *worms*)


;; モデル0501(欠けた球)(中心に原点あり)
;;   r      半径
;;   slice  y軸のまわりの分割数
;;   stack  y軸に垂直な分割数
;;   wedge  欠けの大きさ(角度)
;; (define (model0501 r slice stack wedge) ...)
(load (make-fpath *app-dpath* "model/model0501.scm"))

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
  (let ((vx 0) (vy 0))
    (if (spkey-on? *ksinfo* GLUT_KEY_LEFT)  (set! vx (- vx *cd*)))
    (if (spkey-on? *ksinfo* GLUT_KEY_RIGHT) (set! vx (+ vx *cd*)))
    (if (spkey-on? *ksinfo* GLUT_KEY_DOWN)  (set! vy (- vy *cd*)))
    (if (spkey-on? *ksinfo* GLUT_KEY_UP)    (set! vy (+ vy *cd*)))
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
  (set! *mouse-offsetx* (quotient (- w *width*)  2))
  (set! *mouse-offsety* (quotient (- h *height*) 2))
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
    (set! *cx* (clamp (win-gl-x *win* (- x *mouse-offsetx*)) (- *wd/2*) *wd/2*))
    (set! *cy* (clamp (win-gl-y *win* (- y *mouse-offsety*)) (- *ht/2*) *ht/2*))))

;; タイマー
(define (timer val)
  ;; カーソルの移動
  (move-cursor)
  ;; ワームの移動
  (for-each (lambda (w1) (worm-move w1 *cx* *cy*)) *worms*)
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

