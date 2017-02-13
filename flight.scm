;; -*- coding: utf-8 -*-
;;
;; flight.scm
;; 2017-2-13 v1.05
;;
;; ＜内容＞
;;   Gauche-gl を使用した、簡単なフライトゲームです。
;;   スペースキーを押し続けると加速します。(上昇中はほとんど加速できません)
;;   矢印キーの上下で方向を変えます。
;;   (矢印キーの上で反時計回り、下で時計回りに角度を変えます)
;;   画面右上のチェックポイント(オレンジ色の円)に触れてから、地面に着陸するとゴールです。
;;   着陸の際は、地面への進入角が30度以下である必要があります。
;;   (進入角が30度より大きいとバウンドしてしまいます)
;;   [r]キーを押すとリセットします。
;;   ESCキーを押すと終了します。
;;
(add-load-path "." :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use gauche.sequence)
(use math.const)
(use glmintool)
(use gltextscrn)
(use alaudplay)

(define *wait*      20) ; ウェイト(msec)
(define *title* "flight") ; ウィンドウのタイトル
(define *width*    600) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)
(define *aratio*     (/. *width* *height*)) ; アスペクト比(計算用)

(define *wd/2*   50000) ; 画面幅/2
(define *ht/2*   40000) ; 画面高さ/2
(define *mysize*  5000) ; 自機のサイズ
(define *maxx*       *wd/2*)     ; 自機のX座標最大値
(define *minx*       (- *maxx*)) ; 自機のX座標最小値
(define *maxy*   77000) ; 自機のY座標最大値
(define *miny*   -3000) ; 自機のY座標最小値
(define *maxv*     800) ; 自機の速度最大値
(define *x*     -40000) ; 自機のX座標
(define *y*          0) ; 自機のY座標
(define *v*          0) ; 自機の速度
(define *a*          0) ; 自機の加速度
(define *g*         10) ; 重力加速度
(define *vx*         0) ; X方向速度
(define *vy*         0) ; Y方向速度
(define *angle*      0) ; 角度(度)
(define *landing*    0) ; 着陸フラグ
(define *cx*     40000) ; チェックポイントのX座標
(define *cy*     69000) ; チェックポイントのY座標
(define *cr*      5000) ; チェックポイントの半径
(define *goal*       0) ; ゴール情報(=0:通常,=1:チェックポイント通過,=2:ゴール)
(define *sc*         0) ; スコア
(define *hs*         0) ; ハイスコア
(define *scene*      0) ; シーン情報(=0:スタート画面,=1:プレイ中,=2:プレイ終了)
(define *backcolor*   #f32(0.0 0.0 0.3 1.0)) ; 背景色
(define *floorcolor*  #f32(0.7 0.2 0.0 1.0)) ; 地面色
(define *checkcolor1* #f32(1.0 0.6 0.2 1.0)) ; チェックポイント色1
(define *checkcolor2* #f32(0.7 0.4 0.1 1.0)) ; チェックポイント色2

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

;; 音楽データクラスのインスタンス生成
(define *adata-start* (make <auddata>))
(define *adata-point* (make <auddata>))
(define *adata-end*   (make <auddata>))

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 2) (* *ht/2* 2))

;; キー入力状態管理クラスのインスタンス生成
(define *ksinfo* (make <keystateinfo>))

;; キー入力待ちクラスのインスタンス生成
(define *kwinfo* (make <keywaitinfo> :keystate (~ *ksinfo* 'keystate)))

;; 時間待ちクラスのインスタンス生成
(define *twinfo* (make <timewaitinfo> :waitinterval *wait*))

;; ウェイト時間調整クラスのインスタンス生成
(define *wcinfo* (make <waitcalcinfo> :waittime *wait*))


;; 直方体(上面に原点あり)
;; (define (box x y z) ...)
;;
;; モデル0301(簡易飛行機)(胴体と羽の交点に原点あり)(胴体の長さ80)
;; (define (model0301) ...)
;;
(load (make-fpath *app-dpath* "model/model0301.scm"))

;; 自機の表示
(define (disp-mychr)
  (gl-push-matrix)
  (gl-translate *x* (calc-by-ratio *y* *miny* *maxy* (- *ht/2*) *ht/2*) 0)
  (let1 scl (/. *mysize* 80)
    (gl-scale scl scl scl))
  (gl-rotate (+ *angle* -90) 0 0 1)
  (gl-rotate (+ *angle* -90) 0 1 0)
  (model0301)
  (gl-pop-matrix)
  )

;; 自機の移動
(define (move-mychr)
  ;; 方向転換
  (if (spkey-on? *ksinfo* GLUT_KEY_UP)   (set! *angle* (+ *angle* 3)))
  (if (spkey-on? *ksinfo* GLUT_KEY_DOWN) (set! *angle* (- *angle* 3)))
  (if (<  *angle* 0)   (set! *angle* (+ *angle* 360)))
  (if (>= *angle* 360) (set! *angle* (- *angle* 360)))
  ;; 加速
  (if (and (key-on? *ksinfo* #\space)
           (not (and (= *goal* 1) *landing*)))
    (set! *a* 3)
    (set! *a* 0))
  (let1 rad (/. (* *angle* pi) 180)
    (set! *vx* (* (+ *v* *a*) (cos rad)))
    (set! *vy* (* (+ *v* *a*) (sin rad))))
  (if (> *y* 0) (set! *vy* (- *vy* *g*))) ; 要検討
  (set! *v* (clamp (sqrt (+ (* *vx* *vx*) (* *vy* *vy*))) 0 *maxv*))
  (set! *vy* (- *vy* *g*)) ; 要検討
  ;; 座標更新
  (set! *x* (+ *x* *vx*))
  (set! *y* (+ *y* *vy*))
  (if (< *x* *minx*) (set! *x* (+ *x* (- *maxx* *minx*))))
  (if (> *x* *maxx*) (set! *x* (- *x* (- *maxx* *minx*))))
  (set! *y* (clamp *y* 0 *maxy*))
  ;; 着陸
  (set! *landing* #f)
  (when (<= *y* 0)
    (cond
     ((and (>= *angle* 330) (<  *angle* 360))
      (set! *angle* 0)
      (set! *landing* #t))
     ((and (>  *angle* 90)  (<= *angle* 210))
      (set! *angle* 180)
      (set! *landing* #t))
     ((and (>= *angle* 0)   (<= *angle* 90))
      (set! *angle* 0)
      (set! *landing* #t))
     ((and (>  *angle* 210) (<  *angle* 330))
      (set! *angle* (- 360 *angle*))
      (auddata-play *adata-point*))
     ))
  (when *landing*
    (cond
     ((= *goal* 1)
      (set! *v* (* *v* 0.95)))
     ((= *a* 0)
      (set! *v* (* *v* 0.99)))
     (else
      (set! *v* (* *v* 0.995)))
     )
    (when (and (= *goal* 1) (< *v* 10))
      (set! *goal* 2))
    )
  )

;; チェックポイントの表示
(define (disp-check-point)
  (if (= *goal* 0)
    (gl-color *checkcolor1*)
    (gl-color *checkcolor2*))
  (draw-win-circle (win-x *win* *cx*)
                   (win-y *win* (calc-by-ratio *cy* *miny* *maxy* (- *ht/2*) *ht/2*))
                   (win-w *win* *cr*)
                   *width* *height* 1 1 'center -0.999999))

;; チェックポイントの判定
(define (check-point?)
  (let ((dx (- *cx* *x*))
        (dy (- *cy* *y*))
        (dr (+ *cr* (* *mysize* 0.45))))
    (<= (+ (* dx dx) (* dy dy)) (* dr dr))))


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 光源設定
  ;(gl-light  GL_LIGHT0 GL_POSITION #f32(1.0 1.0 1.0 0.0))
  (gl-light  GL_LIGHT0 GL_POSITION #f32(-1.0 1.0 1.0 0.0))
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_NORMALIZE)
  ;; 材質設定
  (gl-material GL_FRONT GL_SPECULAR #f32(1.0 1.0 1.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 10.0)
  ;; 音楽データの初期化
  (auddata-load-wav-file *adata-start* (make-fpath *app-dpath* "sound/appear1.wav"))
  (auddata-set-prop *adata-start* AL_GAIN  0.07)
  (auddata-set-prop *adata-start* AL_PITCH 3.0)
  (auddata-load-wav-file *adata-point* (make-fpath *app-dpath* "sound/decide2.wav"))
  (auddata-set-prop *adata-point* AL_GAIN  0.4)
  (auddata-set-prop *adata-point* AL_PITCH 1.1)
  (auddata-load-wav-file *adata-end*   (make-fpath *app-dpath* "sound/decide10.wav"))
  (auddata-set-prop *adata-end*   AL_GAIN  0.4)
  (auddata-set-prop *adata-end*   AL_PITCH 0.9)
  )

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 文字表示
  (let ((y1 36)
        (y2 49)
        (str1 "") (str2 "") (str3 "") (str4 "") (str5 "") (str6 "") (str7 "") (str8 ""))
    ;; シーン情報で場合分け
    (case *scene*
      ((0) ; スタート画面
       (set! str1 "== FLIGHT ==")
       (set! str2 "PRESS [SPACE] KEY")
       (set! y2 48))
      ((1) ; プレイ中
       (when (<= *sc* 5000)
         (set! str2 "USE [SPACE] [UP] [DOWN] KEY")
         (set! y2 48)))
      ((2) ; プレイ終了
       (set! str1 "== GOAL!! ==")
       (set! y1 30)
       (set! str8 (format "TIME : ~A ~A"
                          (make-time-text *sc*)
                          (if (= *sc* *hs*) "(1st!!)" "")))
       (if (timewait-finished? *twinfo*) (set! str2 "HIT [D] KEY")))
      )
    (set! str3 (format "TIME : ~A" (make-time-text *sc*)))
    (set! str4 (format "1st : ~A"  (make-time-text *hs*)))
    (set! str5 (if (= *goal* 0) "CHECK POINT" "CHECK OK "))
    (set! str6 (format "(X=~D Y=~D ANGLE=~D)"
                       (truncate->exact *x*) (truncate->exact *y*) (truncate->exact *angle*)))
    (set! str7 (format "(V=~D VX=~D VY=~D)"
                       (truncate-n *v* 1) (truncate-n *vx* 1) (truncate-n *vy* 1)))
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text str1 (win-w-r *win* 1/2) (win-h-r *win* y1 100) *width* *height* (win-h-r *win* 1/13) 'center)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str2 (+ (win-w-r *win* 1/2) (win-h-r *win* 1/100)) (win-h-r *win* y2 100) *width* *height*
                      (win-h-r *win* 1/18) 'center)
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text str3 0 0 *width* *height* (win-h-r *win* 1/22))
    (gl-color 1.0 0.0 1.0 1.0)
    (draw-stroke-text str4 (win-w-r *win* 1/2) 0 *width* *height* (win-h-r *win* 1/22) 'center)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str5 *width* 0 *width* *height* (win-h-r *win* 1/22) 'right)
    (gl-color 0.0 1.0 0.0 1.0)
    (draw-stroke-text str6 (win-h-r *win* 1/100) (win-h-r *win*  6/100) *width* *height* (win-h-r *win* 1/24))
    (draw-stroke-text str7 (win-h-r *win* 1/100) (win-h-r *win* 11/100) *width* *height* (win-h-r *win* 1/24))
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str8 (win-w-r *win* 1/2) (win-h-r *win* 39/100) *width* *height*
                      (win-h-r *win* 1/13) 'center)
    )
  ;; 自機の表示
  (disp-mychr)
  ;; チェックポイントの表示
  (disp-check-point)
  ;; 地面の表示
  (gl-color *floorcolor*)
  (draw-win-rect 0 (win-y *win* (calc-by-ratio 0 *miny* *maxy* (- *ht/2*) *ht/2*))
                 *width* (win-y *win* (calc-by-ratio *miny* *miny* *maxy* (- *ht/2*) *ht/2*))
                 *width* *height* 'left -0.999999)
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
    (glu-perspective *vangle* (/. *width* *height*) (- z1 10000) (+ z1 10000))
    ;; 視点の位置と方向を設定
    (glu-look-at 0 0 z1 0 0 0 0 1 0)))

;; キー入力ON
(define (keyboard key x y)
  (key-on *ksinfo* key)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit-main-loop 0))
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

;; タイマー
(define (timer val)
  (cond
   ;; 待ち状態のとき
   ((or (keywait-waiting? *kwinfo*) (timewait-waiting? *twinfo*))
    (keywait-timer  *kwinfo*)
    (timewait-timer *twinfo*)
    )
   ;; 待ち状態でないとき
   (else
    ;; シーン情報で場合分け
    (case *scene*
      ((0) ; スタート画面
       ;; 初期化
       (set! *x*  -40000)
       (set! *y*       0)
       (set! *v*       0)
       (set! *a*       0)
       (set! *vx*      0)
       (set! *vy*      0)
       (set! *angle*   0)
       (set! *landing* 0)
       (set! *goal*    0)
       (set! *sc*      0)
       ;; キー入力待ち
       (keywait *kwinfo* '(#\space)
                (lambda ()
                  (set! *scene* 1)
                  (auddata-play *adata-start*)
                  (keywait-clear *kwinfo*)))
       )
      ((1) ; プレイ中
       ;; スコア処理
       (when (< *goal* 2)
         (set! *sc* (+ *sc* *wait*))
         (if (> *sc* 1800000) (set! *sc* 1800000)))
       ;; 自分の移動
       (move-mychr)
       ;; チェックポイントの判定
       (when (and (check-point?) (= *goal* 0))
         (set! *goal* 1)
         (auddata-play *adata-point*))
       ;; 終了判定
       (when (= *goal* 2)
         (set! *scene* 2)
         (if (or (= *hs* 0) (< *sc* *hs*)) (set! *hs* *sc*))
         (auddata-play *adata-end*))
       ;; リセット
       (if (key-on? *ksinfo* '(#\r #\R))
         (set! *scene* 0))
       )
      ((2) ; プレイ終了
       ;; 時間待ち
       (timewait *twinfo* 1500
                 (lambda ()
                   ;; キー入力待ち
                   (keywait *kwinfo* '(#\d #\D #\r #\R)
                            (lambda ()
                              (set! *scene* 0)
                              (timewait-clear *twinfo*)
                              (keywait-clear  *kwinfo*)))))
       )
      )
    )
   )
  ;; 画面表示
  (glut-post-redisplay)
  ;; ウェイト時間調整
  (glut-timer-func (waitcalc *wcinfo*) timer 0))

;; 終了
(define (exit-main-loop code)
  (aud-end)
  (exit code))

;; メイン処理
(define (main args)
  (aud-init (> (x->integer (list-ref args 1 0)) 0))
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
  (glut-timer-func *wait* timer 0)
  ;; コールバック内エラー対策
  (guard (ex (else (report-error ex) (exit-main-loop 1)))
    (glut-main-loop))
  0)

