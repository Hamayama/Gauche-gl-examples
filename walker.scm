;; -*- coding: utf-8 -*-
;;
;; walker.scm
;; 2017-8-15 v1.15
;;
;; ＜内容＞
;;   Gauche-gl を使用した、簡単な探索ゲームです。
;;   矢印キーで上下左右移動。
;;   迷路の出口に到達すればクリアです。
;;   迷路のサイズは、5 x 5 = 25 部屋分となっています。
;;   また、迷路の上下左右はつながっており、外周は存在しません。
;;   スペースキーを押すと、壁にマーク(アルファベット)を付加できます。
;;   マークは最大5つまでで、それをこえると古いものから消えていきます。
;;   [r]キーを押すとゲームをリセットします。
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
(use alaudplay)
(use alauddata)
(use glmazekit)

(define *wait*      27) ; ウェイト(msec)
(define *title* "walker") ; ウィンドウのタイトル
(define *width*    624) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *aratio*     (/. *width* *height*)) ; アスペクト比(計算用)

(define *mw*         5) ; 迷路の幅  (=水平方向のブロック数)
(define *mh*         5) ; 迷路の高さ(=垂直方向のブロック数)
(define *sx*         0) ; スタートのX座標
(define *sy*         0) ; スタートのY座標
(define *gx*         (quotient *mw* 2)) ; ゴールのX座標
(define *gy*         (quotient *mh* 2)) ; ゴールのY座標

(define *wd/2*     520) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *x*          0) ; 自分のX座標(固定)
(define *y*          (- (/. (* *ht/2* 3) 5))) ; 自分のY座標(固定)
(define *dx*         5) ; 自分のX方向の移動量
(define *movkind*    0) ; 移動種別(=0:左右移動,=1:上下移動(階段))
(define *movdir*     0) ; 移動方向(=1:上,=2:右,=4:下,=8:左)
(define *chrdir*     1) ; キャラの向き(=1:右向き,=-1:左向き)
(define *anime*      #(0 1 1 1 1 0 0 0 0)) ; アニメフレーム(テクスチャ番号のベクタ)
(define *frame*      0) ; アニメフレーム番号
(define *rx*         0) ; 部屋上のX座標
(define *ry*         0) ; 部屋上のY座標
(define *mx*         *sx*) ; 迷路上のX座標
(define *my*         *sy*) ; 迷路上のY座標
(define *mywA*      86) ; キャラの幅A
(define *mywB*      43) ; キャラの幅B
(define *myh*      228) ; キャラの高さ

(define *rw*         (/. (* *wd/2* 9) 5)) ; 部屋の幅
(define *rh*         (/. (* *ht/2* 7) 5)) ; 部屋の高さ
(define *steps*     12) ; 階段の段数

(define *gw*       106) ; ゴールの幅
(define *gh*       224) ; ゴールの高さ
(define *grx*      106) ; ゴールの部屋上のX座標

(define *mk*         "ABCDE") ; マークの文字
(define *mknum*      (string-length *mk*)) ; マークの数
(define *mkno*       0) ; マークの現在番号
(define *mkflag*     #f) ; マークフラグ

(define *reset*      #f) ; リセットフラグ
(define *goal*       0) ; ゴール情報(=0:通常,=1:ゴール)
(define *sc*         0) ; スコア
(define *hs*         0) ; ハイスコア
(define *scene*      0) ; シーン情報(=0:スタート画面,=1:プレイ中,=2:プレイ終了)
(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色
(define *roomcolor*  #f32(1.0 1.0 1.0 1.0)) ; 部屋色

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

;; テクスチャデータクラスのインスタンス生成
(define *tex* (make-vector-of-class 2 <texdata>))

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

;; 迷路クラスのインスタンス生成
(define *maze* (make <maze>))

;; マーククラス
(define-class <mark> ()
  ((useflag :init-value #f) ; 使用フラグ
   (st      :init-value "") ; 表示文字列
   (mx      :init-value 0)  ; 迷路上のX座標
   (my      :init-value 0)  ; 迷路上のY座標
   (rx      :init-value 0)  ; 部屋上のX座標
   (ry      :init-value 0)  ; 部屋上のY座標
   ))
;; マーククラスのインスタンス生成
(define *marks* (make-vector-of-class *mknum* <mark>))
(do ((i 0 (+ i 1)))
    ((>= i *mknum*) #f)
  (set! (~ *marks* i 'st) (substring *mk* i (+ i 1))))


;; 自分の表示
(define (disp-mychr)
  (let1 tno (~ *anime* *frame*)
    (draw-texture-rect (~ *tex* tno) (win-x *win* *x*) (win-y *win* (+ *y* *myh*))
                       (win-w *win* *mywA*) (win-w *win* *myh*)
                       *width* *height* 'center 0 1.0 1.0 *chrdir* 1.0
                       (if (= *chrdir* 1) 0.0 1.0) 0.0)))

;; 自分の移動
(define (move-mychr mz)
  (define mw    (~ mz 'width))  ; 迷路の幅
  (define mh    (~ mz 'height)) ; 迷路の高さ
  (define mdata (~ mz 'data))   ; 迷路データ
  (define (pt x y) (+ (* y mw) x)) ; 配列番号への変換
  (define (pxadd x dx) (wrap-range (+ x dx) 0 mw)) ; X座標加算(端を超えたら反対側に移動する)
  (define (pyadd y dy) (wrap-range (+ y dy) 0 mh)) ; Y座標加算(端を超えたら反対側に移動する)

  (let ((movflag #f) ; 移動中フラグ
        (sx1     0)  ; 階段の始点のX座標
        (sxd1    0)) ; 階段の始点のX座標からの距離
    ;; キー入力
    (cond
     ((spkey-on? *ksinfo* GLUT_KEY_LEFT) (set! *movdir* 8) (set! movflag #t))
     ((spkey-on? *ksinfo* GLUT_KEY_RIGHT)(set! *movdir* 2) (set! movflag #t))
     ((spkey-on? *ksinfo* GLUT_KEY_UP)   (set! *movdir* 1) (set! movflag #t))
     ((spkey-on? *ksinfo* GLUT_KEY_DOWN) (set! *movdir* 4) (set! movflag #t))
     )
    ;; 移動種別で場合分け
    (case *movkind*
      ((0) ; 左右移動
       ;; 移動方向で場合分け
       (case *movdir*
         ((8) ; 左
          (set! *chrdir* -1)
          (set! *rx* (- *rx* *dx*)))
         ((2) ; 右
          (set! *chrdir*  1)
          (set! *rx* (+ *rx* *dx*)))
         ((1) ; 上 (階段の近くなら、階段まで移動する)
          (set! sx1  (+ (- (/. *rh* 2)) (- (/. *mywB* 2)) (/. *rh* *steps*)))
          (set! sxd1 (abs (- sx1 *rx*)))
          (cond
           ((or (logtest (~ mdata (pt *mx* *my*)) 1) (> sxd1 (* *mywA* 2)))
            (set! movflag #f))
           (else
            (cond
             ((< sxd1 *dx*)
              (set! *chrdir*  1)
              (set! *rx* sx1)
              (set! *movkind* 1))
             (else
              (set! *chrdir*  (if (> sx1 *rx*) 1 -1))
              (set! *rx* (+ *rx* (* *dx* (if (> sx1 *rx*) 1 -1)))))
             ))
           ))
         ((4) ; 下 (階段の近くなら、階段まで移動する)
          (set! sx1  (- (/. *rh* 2) (/. *mywB* 2)))
          (set! sxd1 (abs (- sx1 *rx*)))
          (cond
           ((or (logtest (~ mdata (pt *mx* *my*)) 4) (> sxd1 (* *mywA* 2)))
            (set! movflag #f))
           (else
            (cond
             ((< sxd1 *dx*)
              (set! *chrdir* -1)
              (set! *rx* sx1)
              (set! *ry* *rh*)
              (set! *my* (pyadd *my* 1))
              (set! *movkind* 1))
             (else
              (set! *chrdir*  (if (> sx1 *rx*) 1 -1))
              (set! *rx* (+ *rx* (* *dx* (if (> sx1 *rx*) 1 -1)))))
             ))
           ))
         )
       ;; ゴール判定
       (when (and (logtest (~ mdata (pt *mx* *my*)) 128)
                  (<= (abs (- *rx* *grx*)) *dx*))
         (set! movflag #f)
         (set! *frame* 0)
         (set! *goal*  1))
       ;; 壁判定
       (when (and (logtest (~ mdata (pt *mx* *my*)) 8)
                  (< *rx* (+ (- (/. *rw* 2)) (/. *mywA* 2))))
         (set! *rx* (+ (- (/. *rw* 2)) (/. *mywA* 2)))
         (set! movflag #f))
       (when (and (logtest (~ mdata (pt *mx* *my*)) 2)
                  (> *rx* (+ (/. *rw* 2) (- (/. *mywA* 2)))))
         (set! *rx* (+ (/. *rw* 2) (- (/. *mywA* 2))))
         (set! movflag #f))
       ;; となりの部屋へ移動
       (when (<  *rx* (- (/. *rw* 2)))
         (set! *mx* (pxadd *mx* -1))
         (set! *rx* (+ *rx* *rw*)))
       (when (>= *rx*    (/. *rw* 2))
         (set! *mx* (pxadd *mx*  1))
         (set! *rx* (- *rx* *rw*)))
       )
      ((1) ; 上下移動(階段)
       ;; 移動方向で場合分け
       (case *movdir*
         ((1 2) ; 上/右
          (set! *chrdir*  1)
          (set! sx1  (+ (- (/. *rh* 2)) (- (/. *mywB* 2))))
          (set! *rx* (+ *rx* *dx*))
          (set! *ry* (* (div (* (- *rx* sx1) *steps*) *rh*) (/. *rh* *steps*)))
          (when (< (abs (- *ry* *rh*)) 5)
            (set! *ry* 0)
            (set! *my* (pyadd *my* -1))
            (set! *movkind* 0)))
         ((4 8) ; 下/左
          (set! *chrdir* -1)
          (set! sx1  (+ (- (/. *rh* 2)) (- (/. *mywB* 2))))
          (set! *rx* (- *rx* *dx*))
          (set! *ry* (* (div (* (- *rx* sx1) *steps*) *rh*) (/. *rh* *steps*)))
          (when (< (abs *ry*) 5)
            (set! *ry* 0)
            (set! *movkind* 0)))
         )
       )
      )
    ;; アニメフレーム更新
    (when (or movflag (> *frame* 0))
      (inc! *frame*)
      (if (> *frame* 8)
        (cond
         (movflag
          (set! *frame*  1))
         (else
          (set! *frame*  0)
          (set! *movdir* 0)))
        ))
    ;; マークを書く
    (cond
     (*mkflag*
      (unless (key-on? *ksinfo* #\space)
        (set! *mkflag* #f)))
     (else
      (when (key-on? *ksinfo* #\space)
        (set! *mkflag* #t)
        (set! (~ *marks* *mkno* 'useflag) #t)
        (set! (~ *marks* *mkno* 'mx) *mx*)
        (set! (~ *marks* *mkno* 'my) *my*)
        (set! (~ *marks* *mkno* 'rx) *rx*)
        (set! (~ *marks* *mkno* 'ry) (+ *ry* (* *myh* 1.2)))
        (inc! *mkno*)
        (if (>= *mkno* *mknum*) (set! *mkno* 0)))))
    ))

;; 部屋の表示
(define (disp-rooms mz)
  (define mw    (~ mz 'width))  ; 迷路の幅
  (define mh    (~ mz 'height)) ; 迷路の高さ
  (define mdata (~ mz 'data))   ; 迷路データ
  (define (pt x y) (+ (* y mw) x)) ; 配列番号への変換
  (define (pxadd x dx) (wrap-range (+ x dx) 0 mw)) ; X座標加算(端を超えたら反対側に移動する)
  (define (pyadd y dy) (wrap-range (+ y dy) 0 mh)) ; Y座標加算(端を超えたら反対側に移動する)

  ;; 上下左右の部屋も表示する
  (gl-color *roomcolor*)
  (gl-line-width 2)
  (%win-ortho-on *width* *height*)
  (let loop ((i -2) (j -1))
    (let ((ox  (win-w *win* (- (* *rw* j) *rx*)))
          (oy  (win-h *win* (+ (* *rh* i) *ry*)))
          (mx1 (pxadd *mx* j))
          (my1 (pyadd *my* i)))
      (gl-push-matrix)
      (%win-translate ox oy *width* *height*)
      ;; 壁
      (%draw-win-line (win-x *win* (- (/. *rw* 2)))
                      (win-y *win* (+ *y* *rh*))
                      (win-x *win* (- (/. *rw* 2)))
                      (win-y *win* (if (logtest (~ mdata (pt mx1 my1)) 8)
                                     *y*
                                     (+ *y* (/. *rh* 2))))
                      *width* *height*)
      ;; 床
      (%draw-win-line (win-x *win* (- (/. *rw* 2)))
                      (win-y *win*    *y*)
                      (win-x *win*    (/. *rw* 2))
                      (win-y *win*    *y*)
                      *width* *height*)
      ;; 階段
      (unless (logtest (~ mdata (pt mx1 my1)) 1)
        (do ((i2 0 (+ i2 1)))
            ((>= i2 *steps*) #f)
          (let ((sx1 (+ (- (/. *rh* 2)) (/. (* *rh* i2) *steps*)))
                (sy1 (+ *y* (/. (* *rh* i2) *steps*)))
                (sx2 (+ (- (/. *rh* 2)) (/. (* *rh* (+ i2 1)) *steps*)))
                (sy2 (+ *y* (/. (* *rh* (+ i2 1)) *steps*))))
            (%draw-win-line (win-x *win* sx1) (win-y *win* sy1)
                            (win-x *win* sx2) (win-y *win* sy1) *width* *height*)
            (%draw-win-line (win-x *win* sx2) (win-y *win* sy1)
                            (win-x *win* sx2) (win-y *win* sy2) *width* *height*)
            )))
      ;; ゴール
      (if (logtest (~ mdata (pt mx1 my1)) 128)
        (%draw-win-rect-line (win-x *win* (+ (- (/. *gw* 2)) *grx*))
                             (win-y *win* (+ *y* *gh*))
                             (win-w *win* *gw*)
                             (win-w *win* *gh*)
                             *width* *height*))
      ;; マーク
      (do ((i2 0 (+ i2 1)))
          ((>= i2 *mknum*) #f)
        (if (and (~ *marks* i2 'useflag)
                 (= mx1 (~ *marks* i2 'mx))
                 (= my1 (~ *marks* i2 'my)))
          (%draw-stroke-text (~ *marks* i2 'st)
                             (win-x *win* (~ *marks* i2 'rx))
                             (win-y *win* (+ *y* (~ *marks* i2 'ry)))
                             *width* *height* (win-h-r *win* 1/13) 'center)))
      (gl-pop-matrix)
      (cond
       ((< i 1) (loop (+ i 1) j))
       ((< j 1) (loop -2 (+ j 1))))
      ))
  (%win-ortho-off)
  (gl-line-width 1))


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 透過設定
  ;(gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;(gl-enable GL_BLEND)
  (gl-alpha-func GL_GREATER 0.1)
  (gl-enable GL_ALPHA_TEST)
  ;; テクスチャ設定
  ;(gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_REPLACE)
  ;(gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
  (load-texture-bitmap-file (~ *tex* 0) (make-fpath *app-dpath* "image/char0201.bmp") '(0 0 0))
  (load-texture-bitmap-file (~ *tex* 1) (make-fpath *app-dpath* "image/char0202.bmp") '(0 0 0))
  ;; 音楽データの初期化
  (init-auddata *app-dpath*))

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  ;; 文字表示
  (let ((str1 "") (str2 "") (str3 "") (str4 "") (str5 "") (str6 "")
        (x2 1) (y1 30) (y2 43))
    ;; シーン情報で場合分け
    (case *scene*
      ((0) ; スタート画面
       (set! str1 "== WALKER ==")
       (set! str2 "HIT [S] KEY")
       (set! x2  0)
       (set! y2 42))
      ((1) ; プレイ中
       )
      ((2) ; プレイ終了
       (set! str1 "== GOAL!! ==")
       (set! y1 24)
       (set! str6 (format "TIME : ~A ~A"
                          (make-time-text *sc*)
                          (if (= *sc* *hs*) "(1st!!)" "")))
       (if (timewait-finished? *twinfo*) (set! str2 "HIT [D] KEY")))
      )
    (set! str3 (format "TIME : ~A" (make-time-text *sc*)))
    (set! str4 (format "1st : ~A"  (make-time-text *hs*)))
    (set! str5 (format "(MX=~D MY=~D)" *mx* *my*))
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text-over str1 (win-w-r *win* 1/2) (win-h-r *win* y1 100) *width* *height*
                           (win-h-r *win* 1/13) 'center 0 #f *backcolor*)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text-over str2 (+ (win-w-r *win* 1/2) (win-h-r *win* x2 100)) (win-h-r *win* y2 100) *width* *height*
                           (win-h-r *win* 1/18) 'center 0 #f *backcolor*)
    (gl-color 0.0 1.0 0.0 1.0)
    (draw-stroke-text str3 0 0 *width* *height* (win-h-r *win* 1/22))
    (gl-color 1.0 0.0 1.0 1.0)
    (draw-stroke-text str4 (win-w-r *win* 1/2) 0 *width* *height* (win-h-r *win* 1/22) 'center)
    ;(gl-color 0.0 1.0 0.0 1.0)
    ;(draw-stroke-text str5 (win-h-r *win* 1/100) (win-h-r *win*  6/100) *width* *height* (win-h-r *win* 1/24))
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text-over str6 (win-w-r *win* 1/2) (win-h-r *win* 33/100) *width* *height*
                           (win-h-r *win* 1/13) 'center 0 #f *backcolor*)
    )
  ;; 自機の表示
  (disp-mychr)
  ;; 部屋の表示
  (if (= (~ *maze* 'maze-state) 1) (disp-rooms *maze*))
  ;; 背景の表示
  (gl-color *backcolor*)
  (draw-win-rect 0 0 *width* *height* *width* *height*)
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  (min w (truncate->exact (*  h *aratio*))))
  (set! *height* (min h (truncate->exact (/. w *aratio*))))
  (win-update-size *win* *width* *height*)
  ;; 縦横比を変えずにリサイズ
  (gl-viewport (quotient (- w *width*) 2) (quotient (- h *height*) 2) *width* *height*))

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
       (cond
        (*reset*
         ;; リセット中
         (unless (key-on? *ksinfo* '(#\r #\R))
           (set! *reset* #f)))
        (else
         ;; 初期化
         (set! *movkind* 0)
         (set! *movdir*  0)
         (set! *chrdir*  1)
         (set! *frame*   0)
         (set! *rx*      0)
         (set! *ry*      0)
         (set! *mx*      *sx*)
         (set! *my*      *sy*)
         (set! *mkno*    0)
         (set! *mkflag*  #f)
         (set! *goal*    0)
         (set! *sc*      0)
         (for-each (lambda (m) (set! (~ m 'useflag) #f)) *marks*)
         ;; 迷路の生成
         (maze-init      *maze* *mw* *mh*)
         (maze-generate  *maze*)
         (maze-set-start *maze* *sx* *sy*)
         (maze-set-goal  *maze* *gx* *gy*)
         ;; キー入力待ち
         (keywait *kwinfo* '(#\s #\S #\r #\R)
                  (lambda ()
                    (keywait-clear *kwinfo*)
                    (case (~ *kwinfo* 'hitkey)
                      ((#\r #\R)
                       (set! *reset* #t))
                      (else
                       (set! *scene* 1)
                       (auddata-play *adata-start1*)))))))
       )
      ((1) ; プレイ中
       ;; スコア処理
       (when (< *goal* 2)
         (set! *sc* (+ *sc* *wait*))
         (if (> *sc* 1800000) (set! *sc* 1800000)))
       ;; 自分の移動
       (move-mychr *maze*)
       ;; 終了判定
       (when (= *goal* 1)
         (set! *scene* 2)
         (if (or (= *hs* 0) (< *sc* *hs*)) (set! *hs* *sc*))
         (auddata-play *adata-end3*))
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

