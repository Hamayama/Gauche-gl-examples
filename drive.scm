;; -*- coding: utf-8 -*-
;;
;; drive.scm
;; 2017-3-26 v1.51
;;
;; ＜内容＞
;;   Gauche-gl を使用した、簡単なドライブゲームです。
;;   矢印キーで左右移動。
;;   スペースキーでブレーキをかけます。
;;   アクセルは自動です。スピードが上がると曲がりにくくなります。
;;   ゴールするかコースアウトするとゲーム終了です。
;;   (道路の端が画面の中心に来ると、コースアウトと判定されます)
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

(define *wait*      25) ; ウェイト(msec)
(define *minwait*   25) ; ウェイト最小値(msec)
(define *maxwait*   50) ; ウェイト最大値(msec)
(define *title* "drive") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    90) ; 視野角(度)
(define *tanvan*     (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *chw*       16) ; 文字の幅
(define *chh*       32) ; 文字の高さ
(define *x*          0) ; 自分のX座標
(define *spd*      100) ; 自分の速度
(define *minspd*   100) ; 自分の最低速度
(define *maxspd*   300) ; 自分の最高速度
(define *rnum*      30) ; 道路の境界マークの数
(define *rx1*        (- *wd/2*)) ; 道路の境界マークのX座標1(左側)
(define *rx2*           *wd/2*)  ; 道路の境界マークのX座標2(右側)
(define *ry*         (- (/. *ht/2* 2)))  ; 道路の境界マークのY座標
(define *rr*        63) ; 道路の境界マークの大きさ
(define *rzmax*   -200) ; 道路の境界マークのZ座標の最大値
(define *rz*         (make-vector *rnum*)) ; 道路の境界マークのZ座標
(do ((i 0 (+ i 1))) ((>= i *rnum*) #f)
  ;; (きれいに見えるように少し引き延ばしている)
  ;(set! (~ *rz* i) (- *rzmax* (* (- *rnum* i 1) 80))))
  (set! (~ *rz* i) (- *rzmax* (* (- *rnum* i 1) 80 (+ 1 (* (- *rnum* i 1) 0.2))))))
(define *rcx1*       0) ; 道路のX方向の曲がり量1
(define *rcx2*       0) ; 道路のX方向の曲がり量2(目標値)
(define *rcy1*       0) ; 道路のY方向の曲がり量1
(define *rcy2*       0) ; 道路のY方向の曲がり量2(目標値)
(define *rdx*        0) ; 道路のX方向の差分
(define *scz*     -400) ; スクリーンのZ座標
(define *stg*        1) ; ステージ
(define *goal*       0) ; ゴール情報(=0:未ゴール,=1-3:ウィニングラン,=4:ゴール終了)
(define *sc*         0) ; スコア
(define *hs*         0) ; ハイスコア
(define *ssc*        0) ; 制御カウンタ
(define *scene*      0) ; シーン情報(=0:スタート画面,=1:プレイ中,=2:プレイ終了)
(define *backcolor*  #f32(0.2 0.2 0.2 1.0)) ; 背景色
(define *hlinecolor* #f32(0.5 0.5 0.5 1.0)) ; 地平線の色
(define *roadcolor*     ; 道路の境界マークの色(明暗の3色 x 5ステージ分)
  #(#(#f32(0.8 0.8 0.8 1.0) #f32(0.5 0.5 0.5 1.0) #f32(0.2 0.2 0.2 1.0))
    #(#f32(0.0 0.9 0.0 1.0) #f32(0.0 0.6 0.0 1.0) #f32(0.0 0.3 0.0 1.0))
    #(#f32(0.0 0.9 0.9 1.0) #f32(0.0 0.6 0.6 1.0) #f32(0.0 0.3 0.3 1.0))
    #(#f32(0.9 0.0 0.9 1.0) #f32(0.6 0.0 0.6 1.0) #f32(0.3 0.0 0.3 1.0))
    #(#f32(0.9 0.9 0.0 1.0) #f32(0.6 0.6 0.0 1.0) #f32(0.3 0.3 0.0 1.0))))

;; コース番号から疑似乱数によりステージ情報を生成する
(define *courceno*   3) ; コース番号(=疑似乱数のシード値)
(define *maxstg*    24) ; ステージ数
(define *rcx*        (make-vector (+ *maxstg* 1))) ; 各ステージの道路のX方向の曲がり量
(define *rcy*        (make-vector (+ *maxstg* 1))) ; 各ステージの道路のY方向の曲がり量
;; 疑似乱数クラスのインスタンス生成
(define *xrand1* (make <xrand>))
(xrand-init *xrand1* *courceno*)
;; 疑似乱数によりステージの情報を生成
(do ((i 2 (+ i 1))) ((> i *maxstg*) #f)
  (set! (~ *rcx* i) (* (xrand-randint *xrand1* -10 10)  0.000004))
  (set! (~ *rcy* i) (* (xrand-randint *xrand1* -15  5) -0.000004)))

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

;; 音楽データクラスのインスタンス生成
(define *adata-start* (make <auddata>))
(define *adata-brake* (make <auddata>))
(define *adata-end1*  (make <auddata>))
(define *adata-end2*  (make <auddata>))

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


;; 曲がり量から差分を計算
;; (ここではカーブの曲線をZ座標の2次関数としている)
(define (calc-curv c z)
  (* c z z))

;; 道路の表示
;; (Z座標はプレイヤーの視点が原点で奥方向をマイナスとする。
;;  そして、z=*scz* の位置に 幅 *wd* x 高さ *ht* のスクリーンがあるものとして、
;;  投影座標 (scx,scy) を計算する。
;;  ただし、OpenGLを使う場合には、ライブラリが投影変換(および表示)を行ってくれるので、
;;  投影座標 (scx,scy) を自前で計算する必要はない。
;;  しかし、今回は、下り坂の視点から見えない部分を非表示にするために、
;;  scy だけは自前でも計算している。)
(define (disp-road)
  (let ((sczr  0)   ; 投影座標計算用(Z座標による縮小倍率)
        (rdy1  0)   ; 道路のY座標の差分1
        (rdy2  0)   ; 道路のY座標の差分2
        (scy1  0)   ; 投影Y座標1
        (scy2  0)   ; 投影Y座標2
        (rline #f)) ; 地平線表示フラグ
    ;; Z座標の奥の方から順番に処理していく
    (do ((i 0 (+ i 1))) ((>= i *rnum*) #f)
      ;; 道路の境界マークの座標を計算
      (set! sczr  (/. *scz* (~ *rz* i)))
      (set! *rdx* (+ *x* (calc-curv *rcx1* (- *rzmax* (~ *rz* i)))))
      (set! rdy1         (calc-curv *rcy1* (- *rzmax* (~ *rz* i))))
      (set! scy1  (* (+ *ry* rdy1) sczr))
      ;; 次の境界マークのY座標も取得
      (when (< i (- *rnum* 1))
        (set! sczr (/. *scz* (~ *rz* (+ i 1))))
        (set! rdy2 (calc-curv *rcy1* (- *rzmax* (~ *rz* (+ i 1)))))
        (set! scy2 (* (+ *ry* rdy2) sczr))
        )
      ;; Y座標が増加するときは、視点から見えないと判断して非表示にする
      (when (or (= i (- *rnum* 1)) (>= scy1 scy2))
        ;; 地平線の表示(正射影で表示)
        (unless rline
          (set! rline #t)
          (gl-color *hlinecolor*)
          (draw-win-line 0 (win-y *win* scy1) *width* (win-y *win* scy1)
                         *width* *height* -0.99999)
          )
        ;; 道路の境界マークの表示
        (gl-material GL_FRONT GL_DIFFUSE
                     (~ *roadcolor*
                        (if (> *goal* 0) 0 (modulo (- *stg* 1) 5))
                        (modulo (+ *ssc* (- *rnum* i 1)) 3)))
        (gl-material GL_FRONT GL_SPECULAR #f32(0.2 0.2 0.2 1.0))
        ;(gl-material GL_FRONT GL_SHININESS 10.0)
        (gl-push-matrix)
        (gl-translate (+ *rx1* *rdx*) (+ *ry* rdy1) (~ *rz* i))
        (glut-solid-sphere *rr* 20 20)
        (gl-pop-matrix)
        (gl-push-matrix)
        (gl-translate (+ *rx2* *rdx*) (+ *ry* rdy1) (~ *rz* i))
        (glut-solid-sphere *rr* 20 20)
        (gl-pop-matrix)
        )
      )
    ))

;; 道路の状態更新
(define (update-road)
  ;; ステージの更新
  (when (= (modulo *ssc* 100) 0)
    (cond
     ((< *stg* *maxstg*)
      (inc! *stg*)
      (set! *rcx2* (~ *rcx* *stg*))
      (set! *rcy2* (~ *rcy* *stg*)))
     (else
      (inc! *goal*)
      (set! *rcx2* 0)
      (set! *rcy2* 0)
      (if (= *goal* 1) (auddata-play *adata-end2*)))
     ))
  ;; 道路の曲がり量の更新(少しずつ変化させる)
  ;(let1 rdc 0.0000007
  (let1 rdc 0.0000005
    (if (< (abs (- *rcx1* *rcx2*)) rdc)
      (set! *rcx1* *rcx2*)
      (set! *rcx1* (+ *rcx1* (if (< *rcx1* *rcx2*) rdc (- rdc)))))
    (if (< (abs (- *rcy1* *rcy2*)) rdc)
      (set! *rcy1* *rcy2*)
      (set! *rcy1* (+ *rcy1* (if (< *rcy1* *rcy2*) rdc (- rdc)))))
    ))

;; 自分の移動
(define (move-mychr)
  ;; 速度の更新
  (cond
   ((key-on? *ksinfo* #\space) (set! *spd* (- *spd* 6))
                               (auddata-play *adata-brake*))
   ((< *spd* *maxspd*)         (set! *spd* (+ *spd* 2)))
   (else                       (set! *spd* (- *spd* 1))))
  (set! *spd* (clamp *spd* *minspd* *maxspd*))
  ;; X座標の更新
  (let ((dx1 0) (dx2 0))
    ;; ハンドル操作による差分を計算(速度に反比例)
    (if (spkey-on? *ksinfo* GLUT_KEY_LEFT)  (set! dx1 (+ dx1  1)))
    (if (spkey-on? *ksinfo* GLUT_KEY_RIGHT) (set! dx1 (+ dx1 -1)))
    (set! dx1 (* dx1 (remap-range *spd* *minspd* *maxspd* 16  8)))
    ;; 道路の曲がり量による差分を計算(速度に正比例)
    (set! dx2 (calc-curv *rcx1* (- *rzmax* *scz*)))
    (set! dx2 (* dx2 (remap-range *spd* *minspd* *maxspd*  5 16)))
    ;; 差分を反映
    (set! *x* (+ *x* dx1 dx2))
    ))


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
  (auddata-load-wav-file *adata-start* (make-fpath *app-dpath* "sound/appear1.wav"))
  (auddata-set-prop *adata-start* AL_GAIN  0.07)
  (auddata-set-prop *adata-start* AL_PITCH 3.0)
  (auddata-load-wav-file *adata-brake* (make-fpath *app-dpath* "sound/cursor4.wav"))
  (auddata-set-prop *adata-brake* AL_GAIN  0.15)
  (auddata-load-wav-file *adata-end1*  (make-fpath *app-dpath* "sound/pattern05.wav"))
  (auddata-set-prop *adata-end1*  AL_GAIN  0.2)
  (auddata-set-prop *adata-end1*  AL_PITCH 1.3)
  (auddata-load-wav-file *adata-end2*  (make-fpath *app-dpath* "sound/pattern03.wav"))
  (auddata-set-prop *adata-end2*  AL_GAIN  0.3)
  )

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 文字表示
  (let ((str1 "") (str2 "") (str3 "") (str4 "") (str5 "") (str6 "") (str7 "")
        (y1 25) (y2 36))
    ;; シーン情報で場合分け
    (case *scene*
      ((0) ; スタート画面
       (set! str1 "== DRIVE ==")
       (set! str2 "HIT [S] KEY")
       (set! y2 37))
      ((1) ; プレイ中
       (if (<= *sc* 4000) (set! str2 "MOVE : [<-] [->]  BRAKE : [SPACE]")))
      ((2) ; プレイ終了
       (if (timewait-finished? *twinfo*) (set! str2 "HIT [D] KEY")))
      )
    (when (or (= *scene* 1) (= *scene* 2))
      (cond
       ((> *goal* 0)
        (set! str1 "== GOAL!! ==")
        (set! y1 19)
        (set! str7 (format "TIME : ~A ~A"
                           (make-time-text *sc*)
                           (if (= *sc* *hs*) "(1st!!)" ""))))
       ((= *scene* 2)
        (set! str1 "COURSE OUT!!"))
       ))
    (set! str3 (format "TIME : ~A" (make-time-text *sc*)))
    (set! str4 (format "1st : ~A"  (make-time-text *hs*)))
    (set! str5 (format "STAGE : ~D/~D" *stg* *maxstg*))
    (set! str6 (format "~Dkm/h" (if (= *scene* 0) 0 *spd*)))
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
    (draw-stroke-text str6 *width* (win-h-r *win* 5/100) *width* *height* (win-h-r *win* 1/22) 'right)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str7 (win-w-r *win* 1/2) (win-h-r *win* 27/100) *width* *height* (win-h-r *win* 1/13) 'center)
    )
  ;; 道路の表示
  (disp-road)
  ;; 背景の表示
  (gl-color *backcolor*)
  (draw-win-rect 0 0 *width* *height* *width* *height* 'left -0.99999)
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
  (glu-perspective *vangle* (/. *width* *height*) 1 20000))

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
       (set! *x*       0)
       (set! *spd*   100)
       (set! *rcx1*    0)
       (set! *rcx2*    0)
       (set! *rcy1*    0)
       (set! *rcy2*    0)
       (set! *rdx*     0)
       (set! *stg*     1)
       (set! *goal*    0)
       (set! *sc*      0)
       (set! *ssc*     0)
       ;; キー入力待ち
       (keywait *kwinfo* '(#\s #\S)
                (lambda ()
                  (set! *scene* 1)
                  (auddata-play *adata-start*)
                  (keywait-clear *kwinfo*)))
       )
      ((1) ; プレイ中
       ;; ウェイト時間の調整(速度に反比例)
       (set! *wait* (truncate->exact
                     (remap-range *spd* *minspd* *maxspd* *maxwait* *minwait*)))
       (waitcalc-set-wait *wcinfo* *wait*)
       ;; スコアと制御カウンタの処理等
       (cond
        ((= *goal* 0)
         (set! *sc* (+ *sc* *wait*))
         (if (> *sc* 1000000) (set! *sc* 1000000)))
        (else
         (if (or (= *hs* 0) (< *sc* *hs*)) (set! *hs* *sc*))))
       (inc! *ssc*)
       (if (> *ssc* 1000000) (set! *ssc* 1))
       ;; 道路の状態更新
       (update-road)
       ;; 自分の移動
       (move-mychr)
       ;; 終了判定
       (when (or (> (+ *rx1* *rdx*) 0)
                 (< (+ *rx2* *rdx*) 0)
                 (>= *goal* 4))
         (set! *scene* 2)
         (if (= *goal* 0) (auddata-play *adata-end1*)))
       )
      ((2) ; プレイ終了
       ;; ウェイト時間の復旧
       (set! *wait* *minwait*)
       (waitcalc-set-wait *wcinfo* *wait*)
       ;; 時間待ち
       ;(timewait *twinfo* 1500
       (timewait *twinfo* (if (> *goal* 0) 1000 1500)
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

