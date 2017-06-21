;; -*- coding: utf-8 -*-
;;
;; batting.scm
;; 2017-6-21 v1.62
;;
;; ＜内容＞
;;   Gauche-gl を使用した、バッティングゲームです。
;;   矢印キーでカーソルを左右に移動し、
;;   スペースキーでボールを打ちます。
;;   引き付けるほど飛びます(最大199m)。ただし見逃しは0mです。
;;   ESCキーを押すと終了します。
;;
(add-load-path "." :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use glmintool)
(use gltextscrn)
(use alaudplay)
(use alauddata)
(use glmodelkit)

(define *wait*      15) ; ウェイト(msec)
(define *title* "batting") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*   100) ; 視野角(度)
(define *tanvan*     (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)

(define *wd/2*     120) ; 画面幅/2
(define *ht/2*     120) ; 画面高さ/2
(define *zstart* -1000) ; ボールの初期位置のZ座標
(define *zend*    -100) ; ボールの最終位置のZ座標
(define *r*         12) ; ボールの半径
(define *x*          0) ; ボールのX座標
(define *y*          (- *r*)) ; ボールのY座標
(define *z*          *zstart*) ; ボールのZ座標
(define *vx*         0) ; ボールのX方向速度
(define *vy*         0) ; ボールのY方向速度
(define *vz*         0) ; ボールのZ方向速度
(define *vx2*        0) ; ボールのX方向速度2
(define *vy2*        0) ; ボールのY方向速度2
(define *cx*         0) ; カーソルのX座標
(define *cy*         (- (/. *ht/2* 2))) ; カーソルのY座標
(define *cz*         *zend*) ; カーソルのZ座標
(define *gdy*     -100) ; 地面のY座標
(define *hit*        0) ; 当たり判定(=0:なし,=1:空振り,=2:当たり,=3:大当たり)
(define *foul*      #f) ; ファールフラグ
(define *sc*         0) ; スコア
(define *hs*         0) ; ハイスコア
(define *scene*      0) ; シーン情報(=0:スタート画面,=1:打撃前,=2:打撃後,=3:結果画面)
(define *playcount*  0) ; プレイ数
(define *scsum*      0) ; スコア累積

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

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


;; ボール(中心に原点あり)
(define (ball r)
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 1.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 2.0)
  (glut-solid-sphere r 20 20))

;; カーソル(中心に原点あり)
(define (cursor r)
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 0.0 0.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 10.0)
  (gl-push-matrix)
  (gl-translate 0 r 0)
  (box-model 1 r 1)
  (gl-pop-matrix)
  (gl-push-matrix)
  (gl-translate 0 1 0)
  (box-model r 1 1)
  (gl-pop-matrix)
  )

;; 空(正射影で表示)
(define (sky)
  (gl-color 0.0 0.0 1.0 1.0)
  (draw-win-rect 0 0 *width* *height* *width* *height* 'left -0.99999))

;; 地面(上面に原点あり)
(define (ground)
  (gl-material GL_FRONT GL_DIFFUSE #f32(0.0 0.75 0.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 10.0)
  (box-model 40000 40000 10000))


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 光源設定
  (gl-light  GL_LIGHT0 GL_POSITION #f32(1.0 1.0 1.0 0.0))
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_NORMALIZE)
  ;; 材質設定
  (gl-material GL_FRONT GL_SPECULAR #f32(1.0 1.0 1.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 10.0)
  ;; 音楽データの初期化
  (init-auddata *app-dpath*)
  )

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 文字表示
  (let ((str1 "") (str2 "") (str3 "") (str4 "") (str5 "") (y2 39))
    ;; シーン情報で場合分け
    (case *scene*
      ((0) ; スタート画面
       (set! str2 "HIT [S] KEY")
       (set! y2 40))
      ((1) ; 打撃前
       (set! str2 "MOVE : [<-] [->]  HIT : [SPACE]")
       (set! y2 92))
      ((2) ; 打撃後
       )
      ((3) ; 結果画面
       (set! str1 (format "SCORE : ~Dm" *sc*))
       (if (= *hit* 3) (set! str1 (string-append str1 " (MAXIMUM!!)")))
       (if *foul*      (set! str1 (string-append str1 " (FOUL!!)")))
       (if (timewait-finished? *twinfo*) (set! str2 "HIT [D] KEY")))
      )
    (set! str3 (format "TOP=~Dm AVG=~Dm PLAY=~D"
                       *hs*
                       (if (= *playcount* 0) 0.0 (round-n (/. *scsum* *playcount*) 1))
                       *playcount*))
    (set! str4 (format "(X=~D Y=~D Z=~D)"
                       (truncate->exact *x*) (truncate->exact *y*) (truncate->exact *z*)))
    (set! str5 (format "(VX=~D VY=~D VZ=~D)"
                       (if (= *scene* 0) 0.0 (truncate-n *vx* 2))
                       (truncate-n *vy* 2)
                       (truncate-n *vz* 2)))
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text str1 (win-w-r *win* 1/2) (win-h-r *win* 26/100) *width* *height* (win-h-r *win* 1/13) 'center)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str2 (+ (win-w-r *win* 1/2) (win-h-r *win* 1/100)) (win-h-r *win* y2 100) *width* *height*
                      (win-h-r *win* 1/18) 'center)
    (gl-color 1.0 0.0 1.0 1.0)
    (draw-stroke-text str3 (win-h-r *win* 1/100) (win-h-r *win*  1/100) *width* *height* (win-h-r *win* 1/19))
    (gl-color 0.0 1.0 0.0 1.0)
    (draw-stroke-text str4 (win-h-r *win* 1/100) (win-h-r *win*  7/100) *width* *height* (win-h-r *win* 1/24))
    (draw-stroke-text str5 (win-h-r *win* 1/100) (win-h-r *win* 12/100) *width* *height* (win-h-r *win* 1/24))
    )
  ;; ボールの表示
  (gl-push-matrix)
  (gl-translate *x* *y* *z*)
  (ball *r*)
  (gl-pop-matrix)
  ;; カーソルの表示
  (gl-push-matrix)
  (gl-translate *cx* *cy* *cz*)
  (cursor *r*)
  (gl-pop-matrix)
  ;; 空の表示
  (sky)
  ;; 地面の表示
  (gl-push-matrix)
  (gl-translate 0 *gdy* 0)
  (ground)
  (gl-pop-matrix)
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  w)
  (set! *height* (min w h))
  (win-update-size *win* *width* *height*)
  ;; 縦横比を変えずにリサイズ
  (gl-viewport 0 (quotient (- h *height*) 2) *width* *height*)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  ;; 透視射影する範囲を設定
  (glu-perspective *vangle* (/. *width* *height*) 1 10000))

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
       (set! *x*     0)
       (set! *y*     (- *r*))
       (set! *z*     *zstart*)
       (set! *vz*    5)
       (set! *vx*    (* (/. (randint (- (quotient *wd/2* 2)) (quotient *wd/2* 2))
                            (- *zend* *zstart*))
                        *vz*))
       (set! *vy*    (* (/. (- *r* (/. *ht/2* 2))
                            (- *zend* *zstart*))
                        *vz*))
       (set! *vx2*   0)
       (set! *vy2*   0)
       (set! *cx*    0)
       (set! *cy*    (- (/. *ht/2* 2)))
       (set! *cz*    *zend*)
       (set! *hit*   0)
       (set! *foul* #f)
       (set! *sc*    0)
       ;; キー入力待ち
       (keywait *kwinfo* '(#\s #\S)
                (lambda ()
                  (set! *scene* 1)
                  (auddata-play *adata-start2*)
                  (keywait-clear *kwinfo*)))
       )
      ((1) ; 打撃前
       (set! *x* (+ *x* *vx*))
       (set! *y* (+ *y* *vy*))
       (set! *z* (+ *z* *vz*))
       ;; カーソル移動
       (if (spkey-on? *ksinfo* GLUT_KEY_LEFT)  (set! *cx* (+ *cx* -1)))
       (if (spkey-on? *ksinfo* GLUT_KEY_RIGHT) (set! *cx* (+ *cx*  1)))
       (set! *cx* (clamp *cx* (- *wd/2*) *wd/2*))
       ;; 打撃判定
       (when (and (> *z* (- *zend* 100))
                  (key-on? *ksinfo* #\space)
                  (= *hit* 0))
         (set! *hit* 1)
         (when (<= (abs (- *x* *cx*)) (* *r* 2))
           (set! *hit* (if (= *z* *zend*) 3 2))
           (set! *vx2* (- *x* *cx*))
           (set! *vy2* (* (/. (* *ht/2* 2 3.7) 100)
                          (+ 1 (/. (- *z* *zend*) 30))))
           (if (< *vy2* 1) (set! *vy2* 1))
           ))
       (when (>= *z* *zend*)
         (set! *scene* 2)
         (when (>= *hit* 2)
           (auddata-play *adata-hit3*)
           (set! *vx* *vx2*)
           (set! *vy* *vy2*)
           (set! *vz* -5))
         )
       )
      ((2) ; 打撃後
       (cond
        ((< *hit* 2)
         (set! *scene* 3))
        (else
         (set! *x*  (+ *x*  *vx*))
         (set! *y*  (+ *y*  *vy*))
         (set! *z*  (+ *z*  *vz*))
         (set! *vy* (- *vy* (/. (* *ht/2* 2 3.7) 20000)))
         (when (and (<= *vy* 0) (<= *y* (- *r*)))
           (set! *scene* 3))
         (when (>= (abs *x*) (abs (* *z* *tanvan*)))
           (set! *scene* 3)
           (set! *hit*   2)
           (set! *foul* #t))
         )
        )
       (when (= *scene* 3)
         (set! *sc* (truncate->exact (/. (- *zend* *z*) 10)))
         (if (= *hit* 3)    (set! *sc* 199))
         (if *foul*         (set! *sc* 0))
         (if (< *sc*  0)    (set! *sc* 0))
         (if (> *sc*  *hs*) (set! *hs* *sc*))
         (inc! *playcount*)
         (set! *scsum* (+ *scsum* *sc*))
         (if (= *hit* 3)
           (auddata-play *adata-end2*)
           (auddata-play *adata-end1*))
         )
       )
      ((3) ; 結果画面
       ;; 時間待ち
       (timewait *twinfo* 1500
                 (lambda ()
                   ;; キー入力待ち
                   (keywait *kwinfo* '(#\d #\D)
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

