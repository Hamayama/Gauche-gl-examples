;; -*- coding: utf-8 -*-
;;
;; worm0201.scm
;; 2018-5-17 v1.05
;;
;; ＜内容＞
;;   Gauche-gl を使用した、ワームシミュレータです。
;;   (先端から移動していくタイプ)
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
(define *title* "worm0201") ; ウィンドウのタイトル
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
(define *wlen*       9) ; ワームの長さ(関節の数)(末尾も含む)
(define *waku*     400) ; 当たり判定調整用
(define *backcolor*  #f32(0.2 0.2 0.2 1.0)) ; 背景色

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

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
   (fx     :init-value     0)  ; 先端のX座標
   (fy     :init-value     0)  ; 先端のY座標
   (fr     :init-value  2500)  ; 先端の半径
   (fv     :init-value   700)  ; 先端の速度
   (fc     :init-value     0)  ; 先端の角度(度)
   (fcv    :init-value     5)  ; 先端の角速度(度)
   (anum   :init-value     9)  ; 関節の数(末尾も含む)
   (ar     :init-value  1000)  ; 関節の半径
   (al     :init-value  4000)  ; 関節の距離
   (adf    :init-value     #f) ; 関節の遅延フレーム係数
   (axque  :init-value     #f) ; 関節のX座標の遅延キュー
   (ayque  :init-value     #f) ; 関節のY座標の遅延キュー
   (rr     :init-value  2000)  ; 末尾の半径
   ))
;; ワームの初期化
;;   anum  関節の数
;;   fx    先端のX座標
;;   fy    先端のY座標
;;   fc    先端の角度(度)
(define-method worm-init ((w1 <worm>) (anum <integer>) (fx <real>) (fy <real>) (fc <real>))
  (set! (~ w1 'anum)  anum)
  (set! (~ w1 'fx)    fx)
  (set! (~ w1 'fy)    fy)
  (set! (~ w1 'fc)    fc)
  (set! (~ w1 'adf)   (round->exact (/. (~ w1 'al) (~ w1 'fv))))
  (set! (~ w1 'axque) (make <quedata>))
  (set! (~ w1 'ayque) (make <quedata>))
  (quedata-init (~ w1 'axque) (+ (* (~ w1 'anum) (~ w1 'adf)) 1) fx)
  (quedata-init (~ w1 'ayque) (+ (* (~ w1 'anum) (~ w1 'adf)) 1) fy))
;; ワームの移動
;;   gx  目標のX座標
;;   gy  目標のY座標
(define-method worm-move ((w1 <worm>) (gx <real>) (gy <real>))
  (define state  (~ w1 'state))
  (define count1 (~ w1 'count1))
  (define count2 (~ w1 'count2))
  (define rgx    (~ w1 'rgx))
  (define rgy    (~ w1 'rgy))
  ;; 状態によって場合分け
  (case state
    ((0 1) ; 追跡中/食事中
     (set! state 0)
     (cond
      ((%worm-goal? w1 gx gy)
       (set! state 1)
       (set! count1 (+ count1 *wait*))
       (when (>= count1 (~ w1 'wtime1))
         (set! state 2)))
      (else
       (%worm-move-front w1 gx gy)))
     ;; 3分で強制移行(永久パターン防止のため)
     (set! count2 (+ count2 *wait*))
     (when (>= count2 180000)
       (set! state 2))
     (when (= state 2)
       (set! count1 (randint (~ w1 'wtime2) (~ w1 'wtime3)))
       (set! count2 (randint (~ w1 'wtime4) (~ w1 'wtime5)))
       (set! (~ w1 'rgx) (randint (- *wd/2*) *wd/2*))
       (set! (~ w1 'rgy) (randint (- *ht/2*) *ht/2*))))
    ((2) ; ランダム動作中
     (if (%worm-goal? w1 rgx rgy)
       (set! count2 0)
       (%worm-move-front w1 rgx rgy))
     (set! count1 (- count1 *wait*))
     (set! count2 (- count2 *wait*))
     (cond
      ((<= count1 0)
       (set! state 0)
       (set! count1 0)
       (set! count2 0))
      ((<= count2 0)
       (set! count2 (randint (~ w1 'wtime4) (~ w1 'wtime5)))
       (set! (~ w1 'rgx) (randint (- *wd/2*) *wd/2*))
       (set! (~ w1 'rgy) (randint (- *ht/2*) *ht/2*))))))
  (set! (~ w1 'state)  state)
  (set! (~ w1 'count1) count1)
  (set! (~ w1 'count2) count2))
;; ワームの目標到達チェック(内部処理用)
;;   gx  目標のX座標
;;   gy  目標のY座標
(define (%worm-goal? w1 gx gy)
  (define fx    (~ w1 'fx))
  (define fy    (~ w1 'fy))
  (recthit? (- gx *waku*) (- gy *waku*) (* *waku* 2) (* *waku* 2)
            (- fx *waku*) (- fy *waku*) (* *waku* 2) (* *waku* 2)))
;; ワームの先端移動(内部処理用)
;;   gx  目標のX座標
;;   gy  目標のY座標
(define (%worm-move-front w1 gx gy)
  (define fx    (~ w1 'fx))
  (define fy    (~ w1 'fy))
  (define fv    (~ w1 'fv))
  (define fcv   (~ w1 'fcv))
  ;; 先端の角度を目標に近づける
  (let* ((c1    (* (atan (- gy fy) (- gx fx)) 180/pi))
         (diffc (wrap-range (- c1 (~ w1 'fc)) -180 180)))
    (when (> (abs diffc) fcv)
      (set! diffc (clamp diffc (- fcv) fcv))
      ;; 追跡中に10秒経過したら乱数を加算
      (when (and (= (~ w1 'state) 0) (>= (~ w1 'count2) 10000))
        (set! diffc (+ diffc (randint (- fcv) fcv)))))
    (set! (~ w1 'fc) (wrap-range (+ (~ w1 'fc) diffc) -180 180))
    ;; 先端の座標を計算
    (set! (~ w1 'fx) (wrap-range (+ fx (* fv (cos (* (~ w1 'fc) pi/180))))
                                 (- *wd/2*) *wd/2*))
    (set! (~ w1 'fy) (wrap-range (+ fy (* fv (sin (* (~ w1 'fc) pi/180))))
                                 (- *ht/2*) *ht/2*))
    ;; 遅延キューに座標を追加
    (quedata-push (~ w1 'axque) (~ w1 'fx))
    (quedata-push (~ w1 'ayque) (~ w1 'fy))
    ))
;; ワームの表示
(define-method worm-disp ((w1 <worm>))
  (define anum  (~ w1 'anum))
  (define ar    (~ w1 'ar))
  (define rr    (~ w1 'rr))
  (case (~ w1 'state)
    ((0) (gl-material GL_FRONT GL_DIFFUSE #f32(0.5 0.0 0.0 1.0)))
    ((1) (gl-material GL_FRONT GL_DIFFUSE #f32(0.0 0.7 0.3 1.0)))
    ((2) (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 1.0 1.0))))
  (gl-material GL_FRONT GL_AMBIENT #f32(0.5 0.5 0.5 1.0))
  ;; 先端
  (gl-push-matrix)
  (gl-translate (~ w1 'fx) (~ w1 'fy) 0)
  (gl-rotate (~ w1 'fc) 0 0 1)
  (gl-rotate -90 1 0 0)
  (model0501 (~ w1 'fr) 15 10
             (case (~ w1 'state)
               ((0) 120)
               ((1) (randint 0 90))
               ((2) 70)))
  (gl-pop-matrix)
  ;; 関節(末尾も含む)
  (do ((i 1 (+ i 1)))
      ((> i anum) #f)
    (gl-push-matrix)
    ;; 遅延キューの座標を参照
    (gl-translate (quedata-ref (~ w1 'axque) (* i (~ w1 'adf)))
                  (quedata-ref (~ w1 'ayque) (* i (~ w1 'adf)))
                  0)
    (glut-solid-sphere (if (< i anum) ar rr) 20 20)
    (gl-pop-matrix)))
;; ワームクラスのインスタンス生成
(define *worms* (make-vector-of-class *wnum* <worm>))
(for-each
 (lambda (w1)
   (worm-init w1 *wlen*
              (randint (- *wd/2*) *wd/2*)
              (randint (- *ht/2*) *ht/2*)
              (randint -180 179)))
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

