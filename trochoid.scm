;; -*- coding: utf-8 -*-
;;
;; trochoid.scm
;; 2015-8-3 v1.08
;;
;; ＜内容＞
;;   Gauche-gl を使って、内トロコイド曲線を描くサンプルです。
;;   スペースキーを押すと、パラメータを乱数で生成して次を表示します。
;;   ESCキーを押すと終了します。
;;
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use math.mt-random)

(define *rc* 100)         ; 外側の円の半径
(define *rm*  48)         ; 内側の円の半径
(define *rd*  44)         ; 内側の円の鉛筆の位置
(define *color-index* 15) ; 色番号(0-15)
(define *vnum* 0)         ; 頂点数(計算するので設定不要)
(define *vvec* #f)        ; 頂点座標格納用(ユニフォームベクタ(f32vector))
(define *cvec* #f)        ; 頂点色格納用  (ユニフォームベクタ(f32vector))

;; 乱数
;;   (randint n1 n2)でn1以上n2以下の整数の乱数を取得する(n1,n2は整数でn1<n2であること)
(define randint
  (let1 m (make <mersenne-twister> :seed (sys-time))
    (lambda (n1 n2) (+ (mt-random-integer m (+ (- n2 n1) 1)) n1))))

;; 色情報(16色)
(define *color-table*
  #(#(0.0 0.0 0.0) #(0.0 0.0 0.5) #(0.0 0.5 0.0) #(0.0 0.5 0.5)
    #(0.5 0.0 0.0) #(0.5 0.0 0.5) #(0.5 0.5 0.0) #(0.75 0.75 0.75)
    #(0.5 0.5 0.5) #(0.0 0.0 1.0) #(0.0 1.0 0.0) #(0.0 1.0 1.0)
    #(1.0 0.0 0.0) #(1.0 0.0 1.0) #(1.0 1.0 0.0) #(1.0 1.0 1.0)))

;; 内トロコイド曲線の座標と色を設定
(define (setup-pointers rc rm rd color-index vvec cvec vec-offset)
  (let* ((vnum 0)
         (t1   (-  rc rm))
         (t2   (/. t1 rm))
         (n    (/. rm (gcd t1 rm))) ; 何周すれば始点に戻るか(←t2を約分したときの分母に等しい)
         (x1   0)
         (y1   0)
         (colr (~ *color-table* color-index 0))
         (colg (~ *color-table* color-index 1))
         (colb (~ *color-table* color-index 2)))
    ;; 座標と色を計算して、ユニフォームベクタに格納する
    (do ((i  0 (+ i (/. 2pi 360)))
         (i2 (* vec-offset 2) (+ i2 2))
         (i3 (* vec-offset 3) (+ i3 3)))
        ((>= i (* 2pi n)) #f)
      (set! x1 (+ (* t1 (cos i)) (* rd     (cos (* t2 i)))))
      (set! y1 (+ (* t1 (sin i)) (* (- rd) (sin (* t2 i)))))
      (f32vector-set! vvec i2       x1)   ; X座標
      (f32vector-set! vvec (+ i2 1) y1)   ; Y座標
      (f32vector-set! cvec i3       colr) ; R
      (f32vector-set! cvec (+ i3 1) colg) ; G
      (f32vector-set! cvec (+ i3 2) colb) ; B
      (inc! vnum))
    ;; 頂点数を返す
    vnum))

;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  ;; 線の太さとアンチエイリアスの指定
  (gl-line-width 1.5)
  (gl-enable GL_LINE_SMOOTH)
  (gl-enable GL_BLEND)
  (gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; ユニフォームベクタの領域を確保する
  (set! *vvec* (make-f32vector (* (+ (* 360 *rc*) 1) 2) 0))
  (set! *cvec* (make-f32vector (* (+ (* 360 *rc*) 1) 3) 0))
  ;; 内トロコイド曲線の座標と色を設定
  (set! *vnum* (setup-pointers *rc* *rm* *rd* *color-index* *vvec* *cvec* 0))
  ;; OpenGLの配列に設定する
  (gl-enable-client-state GL_VERTEX_ARRAY)
  (gl-enable-client-state GL_COLOR_ARRAY)
  (gl-vertex-pointer 2 *vvec*)
  (gl-color-pointer  3 *cvec*))

;; 画面表示
(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  ;; 設定した座標と色で線を描く
  (gl-draw-arrays GL_LINE_LOOP 0 *vnum*)
  (gl-flush))

;; 画面のリサイズ
(define (reshape w h)
  ;; 縦横比を変えずにリサイズ
  (if (<= w h)
    (gl-viewport 0 (quotient (- h w) 2) w w)
    (gl-viewport (quotient (- w h) 2) 0 h h))
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  ;; 正射影する座標の範囲を設定
  (gl-ortho -100 100 -100 100 -1.0 1.0))

;; キー入力
(define (keyboard key x y)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit 0))
   ;; スペースキーで次を表示(パラメータを乱数で生成)
   ((= key (char->integer #\space))
    (set! *rm*          (randint 11 (- *rc* 1)))
    (set! *rd*          (randint 10 *rm*))
    (set! *color-index* (randint 10 15))
    ;; 内トロコイド曲線の座標と色を設定
    (set! *vnum* (setup-pointers *rc* *rm* *rd* *color-index* *vvec* *cvec* 0))
    ;; タイトル文字列を更新
    (glut-set-window-title (make-title *rm* *rd*))
    ;; 表示に時間がかかる場合は disp を glut-post-redisplay にする
    (disp))
   ;; [g]キーでGC実行(デバッグ用)
   ((or (= key (char->integer #\g)) (= key (char->integer #\G)))
    (gc) (print (gc-stat)))
   ))

;; タイトル文字列の生成
(define (make-title rm rd)
  (format #f "trochoid (~d,~d)" rm rd))

;; メイン処理
(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB))
  (glut-init-window-size 480 480)
  (glut-init-window-position 100 100)
  (glut-create-window (make-title *rm* *rd*))
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  ;; コールバック内エラー対策
  (guard (ex (else (report-error ex) (exit 0)))
    (glut-main-loop))
  0)

