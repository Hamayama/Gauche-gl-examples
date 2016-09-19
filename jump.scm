;; -*- coding: utf-8 -*-
;;
;; jump.scm
;; 2016-9-19 v1.04
;;
;; ＜内容＞
;;   Gauche-gl を使用した、簡単なジャンプアクションゲームです。
;;   矢印キーで左右移動。
;;   スペースキーでジャンプします(長押しすると高く飛びます)。
;;   雲には乗ることができます。
;;   一定時間で出たり消えたりするゴールに触れるとステージクリアです。
;;   飛んでいる敵に触れるとゲームオーバーです。
;;   (ただし、敵の頭には乗ることができ、乗ると色が変わって無力化します)
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

(define *wait*      30) ; ウェイト(msec)
(define *title* "jump") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *chw*       16) ; 文字の幅
(define *chh*       32) ; 文字の高さ
(define *maxx*       (- *wd/2* *chw*))           ; 自分のX座標最大値
(define *minx*       (- *maxx*))                 ; 自分のX座標最小値
(define *maxy*       (- *ht/2* *chh*))           ; 自分のY座標最大値
(define *miny*       (+ (- *ht/2*) (* *chh* 3))) ; 自分のY座標最小値
(define *x*          0)      ; 自分のX座標
(define *y*          *miny*) ; 自分のY座標
(define *vx*         0)      ; 自分のX方向速度
(define *vy*         0)      ; 自分のY方向速度
(define *jp*         1) ; ジャンプ状態(=0:ジャンプ不可,=1:ジャンプ可,=2:ジャンプ延長中)
(define *jpc*        0) ; ジャンプ中カウンタ
(define *waku*       5) ; 当たり判定調整用
(define *gx*         0) ; ゴールのX座標
(define *gy*         0) ; ゴールのY座標
(define *gt1*      350) ; ゴールの表示カウンタ1
(define *gt2*      350) ; ゴールの表示カウンタ2
(define *gt3*      150) ; ゴールの表示カウンタ3
(define *maxrnum*   10) ; 敵の最大数
(define *rnum*       1) ; 敵の数
(define *cnum*       2) ; 雲の数
(define *sc*         0) ; スコア
(define *hs*         0) ; ハイスコア
(define *ssc*        0) ; 制御カウンタ
(define *scene*      0) ; シーン情報(=0:スタート画面,=1:プレイ中,=2:ステージクリア,=3:プレイ終了)
(define *stage*      1) ; ステージ番号
(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色
(define *floorcolor* #f32(0.7 0.2 0.0 1.0)) ; 地面色

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

;; 音楽データクラスのインスタンス生成
(define *adata-start* (make <auddata>))
(define *adata-goal*  (make <auddata>))
(define *adata-end*   (make <auddata>))

;; テクスチャの配列(u32vector)
(define *tex* #f)

;; キー入力状態管理クラスのインスタンス生成
(define *ksinfo* (make <keystateinfo>))

;; キー入力待ちクラスのインスタンス生成
(define *kwinfo* (make <keywaitinfo> :keystate (~ *ksinfo* 'keystate)))

;; 時間待ちクラスのインスタンス生成
(define *twinfo* (make <timewaitinfo> :waitinterval *wait*))

;; ウェイト時間調整クラスのインスタンス生成
(define *wcinfo* (make <waitcalcinfo> :waittime *wait*))

;; テキスト画面クラスのインスタンス生成
;; (雲)
(define *tscrn-cloud* (make <textscrn>))
(textscrn-init *tscrn-cloud* 5 1)
(textscrn-line *tscrn-cloud* 0 0 4 0 "@")

;; 雲クラス
(define-class <cloud> ()
  ((x    :init-value 0)  ; X座標
   (y    :init-value 0)  ; Y座標
   (vx   :init-value 0)  ; X方向の速度
   (vx2  :init-value 0)  ; X方向の速度2
   (minx :init-value 0)  ; X座標の最小値
   (maxx :init-value 0)  ; X座標の最大値
   ))
(define-method cloud-init ((c <cloud>) x y vx minx maxx)
  (set! (~ c 'x)    x)
  (set! (~ c 'y)    y)
  (set! (~ c 'vx)   vx)
  (set! (~ c 'vx2)  vx)
  (set! (~ c 'minx) minx)
  (set! (~ c 'maxx) maxx))
;; 雲クラスのインスタンス生成
(define *clouds* (make-vector *cnum* #f))
(do ((i 0 (+ i 1))) ((>= i (vector-length *clouds*)) #f)
  (set! (~ *clouds* i) (make <cloud>)))

;; 敵クラス
(define-class <enemy> ()
  ((useflag :init-value #f) ; 使用フラグ
   (state   :init-value 0)  ; 状態(=0:通常,=1:無力化)
   (x       :init-value 0)  ; X座標
   (y       :init-value 0)  ; Y座標
   (degree  :init-value 0)  ; 角度(度)
   (speed   :init-value 0)  ; 速度
   (ddeg    :init-value 0)  ; 角度の変化量(度)
   (vx      :init-value 0)  ; X方向の速度
   (vy      :init-value 0)  ; Y方向の速度
   (minx    :init-value 0)  ; X座標の最小値
   (maxx    :init-value 0)  ; X座標の最大値
   (miny    :init-value 0)  ; Y座標の最小値
   (maxy    :init-value 0)  ; Y座標の最大値
   ))
;; 敵クラスのインスタンス生成
(define *enemies* (make-vector *maxrnum* #f))
(do ((i 0 (+ i 1))) ((>= i (vector-length *enemies*)) #f)
  (set! (~ *enemies* i) (make <enemy>)))


;; ウィンドウ上のX座標を取得
(define (get-win-x x)
  (+ (/. (* x *width*) (* *wd/2* 2))
     (/. *width* 2)))

;; ウィンドウ上のY座標を取得
(define (get-win-y y)
  (+ (/. (* (- y) *height*) (* *ht/2* 2))
     (/. *height* 2)))

;; ウィンドウ上の幅を取得
(define (get-win-w w)
  (/. (* w *width*) (* *wd/2* 2)))

;; ウィンドウ上の高さを取得
(define (get-win-h h)
  (/. (* h *height*) (* *ht/2* 2)))

;; 自分の表示
(define (disp-mychr)
  (let1 tno (if (= *scene* 3) 1 0)
    (draw-texture-rect (~ *tex* tno) (get-win-x (- *x* *chw*)) (get-win-y *y*)
                       (get-win-w (* *chw* 2)) (get-win-h (* *chh* 2))
                       *width* *height* 'left 0 0.75 0.75)
    ))

;; 自分の操作
(define (control-mychr)
  (set! *vx* 0)
  (if (spkey-on? *ksinfo* GLUT_KEY_LEFT)  (set! *vx* -15))
  (if (spkey-on? *ksinfo* GLUT_KEY_RIGHT) (set! *vx* +15))
  (cond
   ;; ジャンプ開始
   ((and (key-on? *ksinfo* #\space) (= *jp* 1))
    (set! *vy*  23)
    (set! *jpc*  9)
    (set! *jp*   2))
   ;; ジャンプ延長
   ((and (key-on? *ksinfo* #\space) (= *jp* 2) (> *jpc* 0))
    (set! *vy*  23)
    (dec! *jpc*))
   ;; 自由落下
   (else
    (set! *vy* (max (- *vy* 5) -30))
    (set! *jp* 0)))
  (set! *x* (+ *x* *vx*))
  (set! *y* (+ *y* *vy*))

  ;; 物体の上に乗る処理
  ;; (高い位置にあるものを優先とする)
  (let ((ride-flag #f)
        (vx1 0)
        (vy1 0))
    ;; 地面の判定
    (when (<= *y* *miny*)
      (set! ride-flag #t)
      (set! vx1 0)
      (set! vy1 0)
      (set! *y* *miny*))
    ;; 雲の判定
    (for-each
     (lambda (c1)
       (when (and (recthit? (- *x* *chw*) (- *y*) (* *chw* 2) (* *chh* 2)
                            (~ c1 'x) (- (~ c1 'y)) (* *chw* 10) *chh*)
                  (>= (abs *vy*)
                      (abs (- *y* (+ (~ c1 'y) (* *chh* 2))))))
         (let1 y1 (+ (~ c1 'y) (* *chh* 2))
           (when (or (not ride-flag) (> y1 *y*))
             (set! ride-flag #t)
             (set! vx1 (~ c1 'vx))
             (set! vy1 0)
             (set! *y* y1)))
         ))
     *clouds*)
    ;; 敵の判定
    (for-each
     (lambda (e1)
       (when (and (~ e1 'useflag)
                  (recthit? (- (- *x* *waku*) *chw*) (- *y*)
                            (+ (* *chw* 2) (* *waku* 2)) (* *chh* 2)
                            (- (~ e1 'x) *chw*) (- (~ e1 'y)) (* *chw* 2) (* *chh* 2))
                  (>= (+ (abs *vy*) (~ e1 'speed))
                      (abs (- *y* (+ (~ e1 'y) (* *chh* 2))))))
         (set! (~ e1 'state) 1)
         (let1 y1 (+ (~ e1 'y) (* *chh* 2))
           (when (or (not ride-flag) (> y1 *y*))
             (set! ride-flag #t)
             (set! vx1 (~ e1 'vx))
             (set! vy1 (- (~ e1 'speed)))
             (set! *y* y1)))
         ))
     *enemies*)
    ;; 最終的に何かの上に乗れたとき
    (when ride-flag
      (set! *x*  (+ *x* vx1))
      (set! *vy* vy1)
      (set! *jp* 1))
    )
  (set! *x* (clamp *x* *minx* *maxx*))
  (set! *y* (clamp *y* *miny* *maxy*)))

;; 雲の表示
(define (disp-clouds)
  (for-each
   (lambda (c1)
     (textscrn-disp-texture *tscrn-cloud*
                            (get-win-x (~ c1 'x)) (get-win-y (~ c1 'y))
                            *width* *height*
                            (get-win-w (* *chw* 2)) (get-win-h *chh*)))
   *clouds*))

;; 雲の移動
(define (move-clouds)
  (for-each
   (lambda (c1)
     (set! (~ c1 'vx) (~ c1 'vx2))
     (set! (~ c1 'x)  (+ (~ c1 'x) (~ c1 'vx)))
     (if (or (<= (~ c1 'x) (~ c1 'minx)) (>= (~ c1 'x) (~ c1 'maxx)))
       (set! (~ c1 'vx2) (- (~ c1 'vx2))))
     (set! (~ c1 'x) (clamp (~ c1 'x) (~ c1 'minx) (~ c1 'maxx))))
   *clouds*))

;; ゴールの表示
(define (disp-goal)
  (when (< *gt1* *gt3*)
    (draw-texture-rect (~ *tex* 5) (get-win-x *gx*) (get-win-y *gy*)
                       (get-win-w (* *chw* 4)) (get-win-h (* *chh* 3))
                       *width* *height* 'left 0 0.75 0.5625)
    ))

;; ゴールの移動
(define (move-goal)
  (dec! *gt1*)
  (if (< *gt1* 0) (set! *gt1* *gt2*))
  (when (= *gt1* *gt3*)
    (set! *gx* (randint (- *wd/2*) (- *wd/2* (* *chw* 4))))
    (set! *gy* (randint (- *ht/2* (* *chh* 3)) (- *ht/2* (* *chh* 6))))
    ))

;; ゴールの判定
(define (check-goal?)
  (and (< *gt1* *gt3*)
       (recthit? (- (+ *x* *waku*) *chw*) (- (- *y* *waku*))
                 (- (* *chw* 2) (* *waku* 2)) (- (* *chh* 2) (* *waku* 2))
                 *gx* (- *gy*) (* *chw* 4) (* *chh* 3))))

;; 敵の初期化
(define (init-enemies)
  (for-each (lambda (e1) (set! (~ e1 'useflag) #f)) *enemies*))

;; 敵の生成
(define (make-enemies)
  (let1 i (find-index (lambda (e1) (not (~ e1 'useflag))) *enemies*)
    (if (and i (< i *rnum*))
      (let1 e1 (~ *enemies* i)
        (set! (~ e1 'useflag) #t)
        (set! (~ e1 'state)   0)
        (set! (~ e1 'x)       (randint (- *wd/2*) *wd/2*))
        (set! (~ e1 'y)       *ht/2*)
        (set! (~ e1 'degree)  -90)
        (set! (~ e1 'speed)   8)
        (set! (~ e1 'ddeg)    (/. (randint -30 30) 10))
        (set! (~ e1 'vx)      0)
        (set! (~ e1 'vy)      -8)
        (set! (~ e1 'minx)    (- *wd/2*))
        (set! (~ e1 'maxx)    *wd/2*)
        (set! (~ e1 'miny)    (- *ht/2*))
        (set! (~ e1 'maxy)    *ht/2*)
        ))))

;; 敵の表示
(define (disp-enemies)
  (for-each
   (lambda (e1)
     (when (~ e1 'useflag)
       (let1 tno (if (= (~ e1 'state) 0) 2 3)
         (draw-texture-rect (~ *tex* tno)
                            (get-win-x (- (~ e1 'x) *chw*))
                            (get-win-y (~ e1 'y))
                            (get-win-w (* *chw* 2))
                            (get-win-h (* *chh* 2))
                            *width* *height* 'left 0 0.75 0.75)
         )))
   *enemies*))

;; 敵の移動
(define (move-enemies)
  (for-each
   (lambda (e1)
     (when (~ e1 'useflag)
       ;; 次の座標を計算
       (set! (~ e1 'vx) (* (~ e1 'speed) (%cos (* (~ e1 'degree) pi/180))))
       (set! (~ e1 'vy) (* (~ e1 'speed) (%sin (* (~ e1 'degree) pi/180))))
       (set! (~ e1 'x) (+ (~ e1 'x) (~ e1 'vx)))
       (set! (~ e1 'y) (+ (~ e1 'y) (~ e1 'vy)))
       ;; 座標の範囲チェック
       (if (not (and (<= (~ e1 'minx) (~ e1 'x) (~ e1 'maxx))
                     (<= (~ e1 'miny) (~ e1 'y) (~ e1 'maxy))))
         ;; 範囲外なら未使用にする
         (set! (~ e1 'useflag) #f))
       ;; 角度の更新
       (set! (~ e1 'degree) (+ (~ e1 'degree) (~ e1 'ddeg)))
       ))
   *enemies*))

;; 敵の当たり判定
(define (hit-enemies?)
  (rlet1 ret #f
    (for-each
     (lambda (e1)
       (if (and (~ e1 'useflag)
                (= (~ e1 'state) 0)
                (recthit? (- (+ *x* *waku*) *chw*) (- (- *y* *waku*))
                          (- (* *chw* 2) (* *waku* 2)) (- (* *chh* 2) (* *waku* 2))
                          (- (~ e1 'x) *chw*) (- (~ e1 'y)) (* *chw* 2) (* *chh* 2)))
         (set! ret #t)))
     *enemies*)))

;; 文字列の上書き表示(背景を塗りつぶしてから、その上に表示する)
(define (draw-stroke-text-over str x y size :optional (align 'left))
  (when (not (equal? str ""))
    (draw-stroke-text str x y *width* *height* size align)
    (gl-color *backcolor*)
    (fill-win-rect x (- y (* size 0.1)) (* size (string-length str) 0.67) (* size 1.2)
                   *width* *height* 'center)
    ))

;; スコア加算
(define (add-score s)
  (set! *sc* (+ *sc* s))
  (if (> *sc* 1000000) (set! *sc* 1000000))
  (if (> *sc* *hs*)    (set! *hs* *sc*)))


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
  ;; 透過設定
  ;(gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;(gl-enable GL_BLEND)
  (gl-alpha-func GL_GREATER 0.5)
  (gl-enable GL_ALPHA_TEST)
  ;; テクスチャ設定
  ;(gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_REPLACE)
  ;(gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
  (set! *tex* (gl-gen-textures 6))
  (load-texture-bitmap-file (~ *tex* 0) (make-fpath *app-dpath* "image/char0101.bmp") '(0 0 0))
  (load-texture-bitmap-file (~ *tex* 1) (make-fpath *app-dpath* "image/char0102.bmp") '(0 0 0))
  (load-texture-bitmap-file (~ *tex* 2) (make-fpath *app-dpath* "image/char0103.bmp") '(0 0 0))
  (load-texture-bitmap-file (~ *tex* 3) (make-fpath *app-dpath* "image/char0104.bmp") '(0 0 0))
  (load-texture-bitmap-file (~ *tex* 4) (make-fpath *app-dpath* "image/char0105.bmp") '(0 0 0))
  (load-texture-bitmap-file (~ *tex* 5) (make-fpath *app-dpath* "image/char0106.bmp") '(0 0 0))
  (set-char-texture #\@ (~ *tex* 4) 0 0 0.75 0.75)
  ;; 音楽データの初期化
  (auddata-load-wav-file *adata-start* (make-fpath *app-dpath* "sound/appear1.wav"))
  (auddata-set-prop *adata-start* AL_GAIN  0.07)
  (auddata-set-prop *adata-start* AL_PITCH 3.0)
  (auddata-load-wav-file *adata-goal*  (make-fpath *app-dpath* "sound/decide2.wav"))
  (auddata-set-prop *adata-goal*  AL_GAIN  0.4)
  (auddata-set-prop *adata-goal*  AL_PITCH 1.05)
  (auddata-load-wav-file *adata-end*   (make-fpath *app-dpath* "sound/pattern05.wav"))
  (auddata-set-prop *adata-end*   AL_GAIN  0.2)
  (auddata-set-prop *adata-end*   AL_PITCH 1.3)
  )

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 文字表示
  (let ((str1 "") (str2 "") (str3 "") (str4 "") (str5 "") (str6 "") (y2 49))
    ;; シーン情報で場合分け
    (case *scene*
      ((0) ; スタート画面
       (when (= *stage* 1)
         (set! str1 "== JUMP ==")
         (set! str2 "HIT [S] KEY")
         (set! y2 50)))
      ((1) ; プレイ中
       )
      ((2) ; ステージクリア
       (set! str6 "STAGE CLEAR!!"))
      ((3) ; プレイ終了
       (set! str6 "GAME OVER")
       (if (timewait-finished? *twinfo*) (set! str2 "HIT [D] KEY")))
      )
    (set! str3 (format "SCORE : ~D"    *sc*))
    (set! str4 (format "HI-SCORE : ~D" *hs*))
    (set! str5 (format "STAGE : ~D"    *stage*))
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text-over str1 (/. *width* 2) (/. (* *height* 36) 100) (/. *height* 13) 'center)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text-over str2 (+ (/. *width* 2) (/. *width* 130))
                           (/. (* *height* y2) 100) (/. *height* 18) 'center)
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text str3 0 0 *width* *height* (/. *height* 22))
    (gl-color 1.0 0.0 1.0 1.0)
    (draw-stroke-text str4 (/. *width* 2) 0 *width* *height* (/. *height* 22) 'center)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str5 *width* 0 *width* *height* (/. *height* 22) 'right)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str6 (/. *width* 2) (/. (* *height* 36) 100) *width* *height*
                      (/. *height* 15) 'center)
    )
  ;; 画面上部(スコア表示領域)のマスク
  (gl-color *backcolor*)
  (fill-win-rect (/. *width* 2) 0 *width* (get-win-h *chh*) *width* *height* 'center)
  ;; 自分の表示
  (disp-mychr)
  ;; 敵の表示
  (disp-enemies)
  ;; 雲の表示
  (disp-clouds)
  ;; ゴールの表示
  (disp-goal)
  ;; 地面の表示
  (gl-color *floorcolor*)
  (fill-win-rect (/. *width* 2) (get-win-y (+ (- *ht/2*) *chh*))
                 *width* (get-win-h *chh*) *width* *height* 'center)
  ;; 背景の表示
  (gl-color *backcolor*)
  (fill-win-rect (/. *width* 2) 0 *width* *height* *width* *height* 'center)
  ;(gl-flush)
  (glut-swap-buffers)
  )

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  (min w h))
  (set! *height* (min w h))
  ;; 縦横比を変えずにリサイズ
  (if (< w h)
    (gl-viewport 0 (quotient (- h w) 2) *width* *height*)
    (gl-viewport (quotient (- w h) 2) 0 *width* *height*))
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  ;; 透視射影する範囲を設定
  (glu-perspective *vangle* (/. *width* *height*) 1 2000)
  ;; 視点の位置と方向を設定
  (glu-look-at 0 0 (/. *wd/2* *tanvan*) 0 0 0 0 1 0)
  )

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
       (set! *y*     *miny*)
       (set! *vx*    0)
       (set! *vy*    0)
       (set! *jp*    1)
       (set! *jpc*   0)
       (set! *gt1* 350)
       (set! *gt2* 350)
       (set! *gt3* 150)
       (set! *rnum* (min *stage* *maxrnum*))
       (let ((minx (- *wd/2*))
             (maxx (- *wd/2* (* *chw* 10))))
         (cloud-init (~ *clouds* 0) minx -175  5 minx maxx)
         (cloud-init (~ *clouds* 1) maxx    0 -5 minx maxx))
       (init-enemies)
       (cond
        ((= *stage* 1)
         (set! *sc*  0)
         (set! *ssc* 0)
         ;; キー入力待ち
         (keywait *kwinfo* '(#\s #\S)
                  (lambda ()
                    (set! *scene* 1)
                    (auddata-play *adata-start*)
                    (keywait-clear *kwinfo*))))
        (else
         (set! *scene* 1)
         (auddata-play *adata-start*))
        )
       )
      ((1) ; プレイ中
       ;; スコアと制御カウンタの処理等
       (if (not (= *vy* 0)) (add-score 1))
       (inc! *ssc*)
       (if (> *ssc* 1000000) (set! *ssc* 1))
       ;; ゴールの移動
       (move-goal)
       ;; 雲の移動
       (move-clouds)
       ;; 敵の生成
       (if (= (modulo *ssc* 12) 0) (make-enemies))
       ;; 敵の移動
       (move-enemies)
       ;; 自分の操作
       (control-mychr)
       ;; ゴールの判定
       (when (check-goal?)
         (add-score (* *stage* 100))
         (set! *scene* 2)
         (auddata-play *adata-goal*))
       ;; 敵の当たり判定
       (when (hit-enemies?)
         (set! *scene* 3)
         (auddata-play *adata-end*))
       )
      ((2) ; ステージクリア
       ;; 時間待ち
       (timewait *twinfo* 1800
                 (lambda ()
                   (set! *scene* 0)
                   (inc! *stage*)
                   (timewait-clear *twinfo*)))
       )
      ((3) ; プレイ終了
       ;; 時間待ち
       (timewait *twinfo* 1500
                 (lambda ()
                   ;; キー入力待ち
                   (keywait *kwinfo* '(#\d #\D)
                            (lambda ()
                              (set! *scene* 0)
                              (set! *stage* 1)
                              (timewait-clear *twinfo*)
                              (keywait-clear  *kwinfo*)))))
       )
      )
    )
   )
  ;; 画面表示
  (glut-post-redisplay)
  ;; ウェイト時間調整
  (glut-timer-func (waitcalc *wcinfo*) timer 0)
  )

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

