;; -*- coding: utf-8 -*-
;;
;; gltextscrn.scm
;; 2016-4-29 v1.12
;;
;; ＜内容＞
;;   Gauche-gl を使って文字列の表示等を行うためのモジュールです。
;;   (日本語表示不可)
;;
(define-module gltextscrn
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use gauche.sequence)
  (use srfi-13) ; string-fold,string-for-each用
  (export
    draw-bitmap-text draw-stroke-text
    fill-win-rect fill-win-circle
    <textscrn> textscrn-init textscrn-disp
    textscrn-cls textscrn-pset textscrn-pget
    textscrn-line textscrn-box textscrn-fbox
    textscrn-circle textscrn-fcircle textscrn-poly textscrn-fpoly
    textscrn-check-str textscrn-disp-check-str
    ))
(select-module gltextscrn)

;; Gauche-gl のテスト時のSEGVエラー対策
;;   ・フォントに直接アクセスするとSEGVエラーになるので、
;;     test-module を実行する前に以下の変数を #f にすること
;;     (Gauche-gl の開発最新版では修正済み)
(define *font-bitmap-1* GLUT_BITMAP_TIMES_ROMAN_24)
(define *font-stroke-1* GLUT_STROKE_ROMAN)

;; 正射影設定ON/OFF(内部処理用)
;;   ・投影方法を正射影に設定し、また、
;;     画面の座標系を左上を原点として (0,0)-(*width*,*height*) の範囲に設定する
;;   ・光源は無効に設定する
;;   ・ONとOFFは常にセットで使用する
(define (gl-ortho-on *width* *height*)
  (gl-disable GL_LIGHTING)
  (gl-matrix-mode GL_PROJECTION)
  (gl-push-matrix)
  (gl-load-identity)
  (gl-ortho 0 *width* 0 *height* -1.0 1.0)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-push-matrix)
  (gl-load-identity))
(define (gl-ortho-off)
  (gl-pop-matrix)
  (gl-matrix-mode GL_PROJECTION)
  (gl-pop-matrix)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-enable GL_LIGHTING))

;; 文字列表示(ビットマップフォント)
;;   ・座標 (x,y) は左上を原点として (0,0)-(*width*,*height*) の範囲で指定する
;;     (図形表示とは座標系が異なるので注意)
;;   ・日本語表示不可
;;   ・文字のサイズはフォントにより固定
(define (draw-bitmap-text str x y *width* *height*
                          :optional (size 24) (align 'left) (font *font-bitmap-1*))
  (gl-ortho-on *width* *height*)
  (let ((stw (string-fold (lambda (ch n) (+ (glut-bitmap-width font (char->integer ch)) n)) 0 str))
        (x1  x)
        (y1  (- *height* y size)))
    (cond
     ((eq? align 'center) ; 中央寄せ
      (set! x1 (- x1 (/. stw 2))))
     ((eq? align 'right)  ; 右寄せ
      (set! x1 (- x1 stw))))
    (gl-raster-pos x1 y1)
    (string-for-each (lambda (ch) (glut-bitmap-character font (char->integer ch))) str)
    )
  (gl-ortho-off))

;; 文字列表示(ストロークフォント)
;;   ・座標 (x,y) は左上を原点として (0,0)-(*width*,*height*) の範囲で指定する
;;     (図形表示とは座標系が異なるので注意)
;;   ・日本語表示不可
;;   ・文字のサイズは指定可能
(define (draw-stroke-text str x y *width* *height*
                          :optional (size 24) (align 'left) (font *font-stroke-1*))
  (gl-ortho-on *width* *height*)
  (let ((stw     (string-fold (lambda (ch n) (+ (glut-stroke-width font (char->integer ch)) n)) 0 str))
        (x1      x)
        (y1      (- *height* y size))
        (scale   (/. size (+ 152.38 20)))
        (xoffset 0)
        (yoffset (+ 33.33 10)))
    (cond
     ((eq? align 'center) ; 中央寄せ
      (set! xoffset (- (/. stw 2))))
     ((eq? align 'right)  ; 右寄せ
      (set! xoffset (- stw))))
    (gl-translate x1 y1 0)
    (gl-scale scale scale 1)
    (gl-translate xoffset yoffset 0)
    (string-for-each (lambda (ch) (glut-stroke-character font (char->integer ch))) str)
    )
  (gl-ortho-off))

;; 長方形の塗りつぶし
;;   ・長方形 (x,y,w,h) の塗りつぶし表示を行う
;;   ・座標は、左上を原点として (0,0)-(*width*,*height*) の範囲で指定する
;;     (図形表示とは座標系が異なるので注意)
(define (fill-win-rect x y w h *width* *height* :optional (align 'left))
  (gl-ortho-on *width* *height*)
  (let ((x1 x)
        (y1 (- *height* y)))
    (cond
     ((eq? align 'center) ; 中央寄せ
      (set! x1 (- x1 (/. w 2))))
     ((eq? align 'right)  ; 右寄せ
      (set! x1 (- x1 w))))
    (gl-translate x1 y1 0)
    ;; Gauche-gl の gl-rect の不具合対策
    ;; (Gauche-gl の開発最新版では修正済み)
    ;(gl-rect 0 0 w (- h))
    (gl-rect (f32vector 0 0) (f32vector w (- h)))
    )
  (gl-ortho-off))

;; 円の塗りつぶし
;;   ・円 (x,y,r,a,b) -> (x*x)/(a*a)+(y*y)/(b*b)=r*r の塗りつぶし表示を行う
;;   ・座標は、左上を原点として (0,0)-(*width*,*height*) の範囲で指定する
;;     (図形表示とは座標系が異なるので注意)
(define (fill-win-circle x y r a b *width* *height* :optional (align 'left))
  (gl-ortho-on *width* *height*)
  (let ((x1 x)
        (y1 (- *height* y))
        (q  (make <glu-quadric>)))
    (if (= a 0) (set! a 1))
    (if (= b 0) (set! b 1))
    (cond
     ((eq? align 'left)  ; 左寄せ
      (set! x1 (+ x1 r)))
     ((eq? align 'right) ; 右寄せ
      (set! x1 (- x1 r))))
    (gl-translate x1 y1 0)
    (gl-scale (/. 1 a) (/. 1 b) 1)
    (glu-disk q 0 r 40 1)
    )
  (gl-ortho-off))


;; テキスト画面クラス
(define-class <textscrn> ()
  ((width  :init-value 0) ; テキスト画面の幅(単位:文字)
   (height :init-value 0) ; テキスト画面の高さ(単位:文字)
   (data   :init-form (make-u32vector 0)) ; テキスト画面のデータ(u32vector)
   ))

;; 初期化
(define-method textscrn-init ((ts <textscrn>) (w <integer>) (h <integer>))
  (set! (~ ts 'width)  w)
  (set! (~ ts 'height) h)
  (set! (~ ts 'data)   (make-u32vector (* w h) 0))
  ;(do ((i 0 (+ i 1))) ((>= i (* w h)) #f)
  ;  (set! (~ ts 'data i) (+ (modulo i (- 128 32)) 32)))
  )

;; 文字情報テーブル(文字列の一括表示用)
(define-class <char-info> () (xscale yscale xoffset yoffset))
(define *char-info-table* (make-vector 128 #f))
(define *char-info-table-init*
  (delay
    (do ((i 0 (+ i 1)))
        ((>= i (vector-length *char-info-table*)) #f)
      (let* ((c1     (make <char-info>))
             (fchw-1 (glut-stroke-width *font-stroke-1* i))
             (fchw   (if (<= fchw-1 0) 104.76 fchw-1))
             (fchh   (+ 152.38 20))
             (xscale&xoffset
              ;; X方向の倍率とオフセット値を設定
              (case i
                ;; !  ,  .  :  ;
                ((33 44 46 58 59) '(0.4  . 16))
                ;; "
                ((34)  '(0.4  . 28))
                ;; '
                ((39)  '(0.4  . 12))
                ;; `
                ((96)  '(0.4  . 42))
                ;; その他の文字
                (else  '(0.9  .  0))))
             (yscale&yoffset
              ;; Y方向の倍率とオフセット値を設定
              (case i
                ;; #  $  (  )  ,  .  /  :  ;  [  \  ]  ^  _  g   j   p   q   y   {   |   }  phi
                ((35 36 40 41 44 46 47 58 59 91 92 93 94 95 103 106 112 113 121 123 124 125 127)
                 `(0.9 . ,(+ 33.33 10)))
                ;; @ (特に小さいので特別扱い)
                ((64)  '(2.0  . -6))
                ;; i (jと高さを合わせるために特別扱い)
                ((105) '(1.18 . 10))
                ;; その他の文字
                (else  '(1.3  . 10)))))
        (set! (~ c1 'xscale)  (/. (car xscale&xoffset) fchw))
        (set! (~ c1 'yscale)  (/. (car yscale&yoffset) fchh))
        (set! (~ c1 'xoffset) (cdr xscale&xoffset))
        (set! (~ c1 'yoffset) (cdr yscale&yoffset))
        (set! (~ *char-info-table* i) c1)))))

;; 文字列の一括表示(フォントは固定)
;;   ・文字ごとに倍率とオフセット値を適用して、等幅フォントのように表示する
;;   ・座標 (x,y) は左上を原点として (0,0)-(*width*,*height*) の範囲で指定する
;;     (図形表示とは座標系が異なるので注意)
;;   ・日本語表示不可
;;   ・文字のサイズは、幅 chw と高さ chh の指定が必要
(define-method textscrn-disp ((ts <textscrn>)
                              (x <real>) (y <real>)
                              (*width* <real>) (*height* <real>)
                              (chw <real>) (chh <real>)
                              :optional (align 'left))
  (force *char-info-table-init*)
  (gl-ortho-on *width* *height*)
  (let ((x1 0)
        (x2 x)
        (y1 (- *height* y))
        (w  (~ ts 'width))
        (i  0))
    (cond
     ((eq? align 'center) ; 中央寄せ
      (set! x2 (- x2 (/. (* w chw) 2))))
     ((eq? align 'right)  ; 右寄せ
      (set! x2 (- x2 (* w chw)))))
    (for-each
     (lambda (c)
       (let1 c1 (~ *char-info-table* c)
         (when (= i 0)
           (set! x1 x2)
           (set! y1 (- y1 chh)))
         (gl-load-identity)
         (gl-translate x1 y1 0)
         (gl-scale (* (~ c1 'xscale) chw) (* (~ c1 'yscale) chh) 1)
         (gl-translate (~ c1 'xoffset) (~ c1 'yoffset) 0)
         (glut-stroke-character *font-stroke-1* c)
         (set! x1 (+ x1 chw))
         (inc! i)
         (if (>= i w) (set! i 0))
         ))
     (~ ts 'data))
    )
  (gl-ortho-off))

;; 文字列のクリア処理
(define-method textscrn-cls ((ts <textscrn>))
  (u32vector-fill! (~ ts 'data) 0))

;; 文字列の点設定処理
(define-method textscrn-pset ((ts <textscrn>) (x <integer>) (y <integer>) (str <string>))
  (textscrn-over-sub ts x y (string->u32vector str)))

;; 文字列の点取得処理
(define-method textscrn-pget ((ts <textscrn>) (x <integer>) (y <integer>) :optional (n 1))
  (let* ((w  (~ ts 'width))
         (h  (~ ts 'height))
         (x1 (max 0 x))
         (x2 (min (- w 1) (+ x n)))
         (i1 (* y w)))
    (if (and (>= y 0) (< y h) (< x1 x2))
      (u32vector->string (~ ts 'data) (+ i1 x1) (+ i1 x2))
      "")))

;; 文字列のライン表示処理
(define-method textscrn-line ((ts <textscrn>)
                              (x1 <integer>) (y1 <integer>) (x2 <integer>) (y2 <integer>)
                              (str <string>))
  (textscrn-line-sub ts x1 y1 x2 y2 (string->u32vector str)))

;; 文字列の四角形表示処理
(define-method textscrn-box ((ts <textscrn>)
                             (x1 <integer>) (y1 <integer>) (x2 <integer>) (y2 <integer>)
                             (str <string>))
  (let ((strdata (string->u32vector str))
        (x3 0) (y3 0) (x4 0) (y4 0))
    (cond
     ((> x1 x2) (set! x3 x2) (set! x4 x1))
     (else      (set! x3 x1) (set! x4 x2)))
    (cond
     ((> y1 y2) (set! y3 y2) (set! y4 y1))
     (else      (set! y3 y1) (set! y4 y2)))
    (let ((strdata1 (textscrn-repeat-sub ts strdata (+ (- x4 x3) 1)))
          (c        (~ strdata 0)))
      (textscrn-over-sub ts x3 y3 strdata1)
      (textscrn-over-sub ts x3 y4 strdata1)
      (do ((i y3 (+ i 1)))
          ((> i y4) #f)
        (textscrn-pset-sub ts x3 i c)
        (textscrn-pset-sub ts x4 i c))
      )
    ))

;; 文字列の四角形塗りつぶし表示処理
(define-method textscrn-fbox ((ts <textscrn>)
                              (x1 <integer>) (y1 <integer>) (x2 <integer>) (y2 <integer>)
                              (str <string>))
  (let ((strdata (string->u32vector str))
        (x3 0) (y3 0) (x4 0) (y4 0))
    (cond
     ((> x1 x2) (set! x3 x2) (set! x4 x1))
     (else      (set! x3 x1) (set! x4 x2)))
    (cond
     ((> y1 y2) (set! y3 y2) (set! y4 y1))
     (else      (set! y3 y1) (set! y4 y2)))
    (let1 strdata1 (textscrn-repeat-sub ts strdata (+ (- x4 x3) 1))
      (do ((i y3 (+ i 1)))
          ((> i y4) #f)
        (textscrn-over-sub ts x3 i strdata1))
      )
    ))

;; 文字列の円表示処理
(define-method textscrn-circle ((ts <textscrn>)
                                (x1 <integer>) (y1 <integer>) (r1 <integer>)
                                (a  <integer>) (b  <integer>) (str <string>))
  (let ((rr       (* r1 r1))
        (aaxx     0)
        (bb       0)
        (drawflag #f)
        (xold     0)
        (yold     0)
        (strdata  (string->u32vector str)))
    (if (< a 1) (set! a 1))
    (if (< b 1) (set! b 1))
    (set! bb (* b b))
    (let loop ((x2 0) (y2 0))
      ;; 円周のx座標を計算(円の内側になるように調整)
      (set! aaxx (- rr (* bb y2 y2)))
      (set! x2 (if (> aaxx 0)
                 (ceiling->exact (- (/. (sqrt aaxx) a) 1))
                 -1))
      (when (>= x2 0)
        (let1 c (~ strdata 0)
          ;; 両端の点を表示
          (textscrn-pset-sub ts (- x1 x2) (- y1 y2) c)
          (textscrn-pset-sub ts (+ x1 x2) (- y1 y2) c)
          (textscrn-pset-sub ts (- x1 x2) (+ y1 y2) c)
          (textscrn-pset-sub ts (+ x1 x2) (+ y1 y2) c))
        (if drawflag
          (let1 strdata1 (textscrn-repeat-sub ts strdata (- xold x2))
            ;; 前回の足りない部分を水平線で追加表示
            (textscrn-over-sub ts (- x1 xold) (- y1 yold) strdata1)
            (textscrn-over-sub ts (+ x1 x2 1) (- y1 yold) strdata1)
            (textscrn-over-sub ts (- x1 xold) (+ y1 yold) strdata1)
            (textscrn-over-sub ts (+ x1 x2 1) (+ y1 yold) strdata1))
          )
        (set! drawflag #t)
        (set! xold x2)
        (set! yold y2)
        (inc! y2)
        (if (<= y2 r1) (loop 0 y2))
        ))
    (if drawflag
      (let1 strdata1 (textscrn-repeat-sub ts strdata (+ (* xold 2) 1))
        ;; 上下の最後の部分を水平線で追加表示
        (textscrn-over-sub ts (- x1 xold) (- y1 yold) strdata1)
        (textscrn-over-sub ts (- x1 xold) (+ y1 yold) strdata1))
      )
    ))

;; 文字列の円塗りつぶし表示処理
(define-method textscrn-fcircle ((ts <textscrn>)
                                 (x1 <integer>) (y1 <integer>) (r1 <integer>)
                                 (a  <integer>) (b  <integer>) (str <string>))
  (let ((rr      (* r1 r1))
        (aaxx    0)
        (bb      0)
        (strdata (string->u32vector str)))
    (if (< a 1) (set! a 1))
    (if (< b 1) (set! b 1))
    (set! bb (* b b))
    (let loop ((x2 0) (y2 0))
      ;; 円周のx座標を計算(円の内側になるように調整)
      (set! aaxx (- rr (* bb y2 y2)))
      (set! x2 (if (> aaxx 0)
                 (ceiling->exact (- (/. (sqrt aaxx) a) 1))
                 -1))
      (when (>= x2 0)
        (let1 strdata1 (textscrn-repeat-sub ts strdata (+ (* x2 2) 1))
          ;; 両端を結ぶ水平線を表示
          (textscrn-over-sub ts (- x1 x2) (- y1 y2) strdata1)
          (textscrn-over-sub ts (- x1 x2) (+ y1 y2) strdata1))
        (inc! y2)
        (if (<= y2 r1) (loop 0 y2))
        ))
    ))

;; 文字列の多角形表示処理
;;   ・point は ((x1 y1) (x2 y2) ...) という頂点座標のリスト
(define-method textscrn-poly ((ts <textscrn>)
                              (point <list>) (str <string>)
                              :optional (notclose #f))
  (let* ((pnum    (length point))
         (lnum    (if notclose (- pnum 1) pnum))
         (strdata (string->u32vector str)))
    (do ((i 0 (+ i 1)))
        ((>= i lnum) #f)
      (let1 j (modulo (+ i 1) pnum)
        (cond
         ;; 左右対称になるように調整(ただし上下対称にはならない)
         ((<= (~ point i 1) (~ point j 1))
          (textscrn-line-sub ts (~ point i 0) (~ point i 1) (~ point j 0) (~ point j 1) strdata))
         (else
          (textscrn-line-sub ts (~ point j 0) (~ point j 1) (~ point i 0) (~ point i 1) strdata)))
        ))
    ))

;; 文字列の多角形塗りつぶし表示処理
;;   ・point は ((x1 y1) (x2 y2) ...) という頂点座標のリスト
(define-class <edgeinfo> () (a ydir y1 y2 x)) ; 辺情報クラス
(define-method textscrn-fpoly ((ts <textscrn>)
                               (point <list>) (str <string>))
  (let* ((pnum      (length point))
         (x1 0) (y1 0) (x2 0) (y2 0) (miny 0) (maxy 0)
         (e1        #f)  ; 辺情報
         (edgelist  '()) ; 辺情報のリスト
         (enum      0)   ; 辺の数
         (wn        0)   ; 巻き数
         (linestart #f)  ; 線分開始フラグ
         (strdata   (string->u32vector str)))

    ;; 辺情報の取得
    (set! miny (~ point 0 1))
    (set! maxy (~ point 0 1))
    (do ((i 0 (+ i 1)))
        ((>= i pnum) #f)
      (set! x1 (~ point i 0))
      (set! y1 (~ point i 1))
      (if (< y1 miny) (set! miny y1))
      (if (> y1 maxy) (set! maxy y1))
      (let1 j (modulo (+ i 1) pnum)
        (set! x2 (~ point j 0))
        (set! y2 (~ point j 1))
        ;; 水平な辺は登録しない
        (when (not (= y1 y2))
          (set! e1 (make <edgeinfo>))
          (set! (~ e1 'a) (/. (- x2 x1) (- y2 y1))) ; Yが1増えたときのXの増分
          (cond
           ((< y1 y2)
            (set! (~ e1 'ydir)  1)   ; Y方向の向き
            (set! (~ e1 'y1)   y1)   ; 始点のY座標
            (set! (~ e1 'y2)   y2)   ; 終点のY座標
            (set! (~ e1 'x)    x1))  ; X座標
           (else
            (set! (~ e1 'ydir) -1)   ; Y方向の向き
            (set! (~ e1 'y1)   y2)   ; 始点のY座標
            (set! (~ e1 'y2)   y1)   ; 終点のY座標
            (set! (~ e1 'x)    x2))) ; X座標
          (push! edgelist e1)
          )))
    ;(print edgelist)
    (set! enum (length edgelist))

    ;; 各辺について、後の辺とY方向の向きが等しい場合は、Y方向の長さを1だけ短くする
    ;; (つなぎ目を2重にカウントして描画領域が反転するのを防ぐため)
    (do ((i 0 (+ i 1)))
        ((>= i enum) #f)
      (set! e1 (~ edgelist i))
      (let1 j (modulo (+ i 1) enum)
        (if (= (~ e1 'ydir) (~ edgelist j 'ydir))
          (cond
           ((= (~ e1 'ydir) 1)
            (dec! (~ e1 'y2)))
           (else
            (inc! (~ e1 'y1))
            (set! (~ e1 'x) (+ (~ e1 'x) (~ e1 'a)))))
          )
        ))

    ;; 描画処理1(ライン表示)
    ;; (ライン表示をしないと、水平線の抜けや飛び飛びの表示が発生するため必要)
    (do ((i 0 (+ i 1)))
        ((>= i pnum) #f)
      (let1 j (modulo (+ i 1) pnum)
        (cond
         ;; 左右対称になるように調整(ただし上下対称にはならない)
         ((<= (~ point i 1) (~ point j 1))
          (textscrn-line-sub ts (~ point i 0) (~ point i 1) (~ point j 0) (~ point j 1) strdata))
         (else
          (textscrn-line-sub ts (~ point j 0) (~ point j 1) (~ point i 0) (~ point i 1) strdata)))
        ))

    ;; 描画処理2(塗りつぶし)
    (do ((y1 miny (+ y1 1)))
        ((> y1 maxy) #f)
      ;; 辺情報をX座標でソートする
      (set! edgelist (sort! edgelist < (cut ~ <> 'x)))
      ;; 水平線と各辺の交点を順番に処理する
      (set! wn 0)
      (set! linestart #f)
      (do ((i 0 (+ i 1)))
          ((>= i enum) #f)
        (set! e1 (~ edgelist i))
        ;; 交点があるとき
        (when (and (>= y1 (~ e1 'y1)) (<= y1 (~ e1 'y2)))
          ;; 辺のY方向の向きから巻き数を計算して、0になるまでの領域を塗りつぶす
          (set! wn (+ wn (~ e1 'ydir)))
          (cond
           ((not linestart)
            (set! linestart #t)
            ;; 左右対称になるように調整
            (set! x1 (if (> (~ e1 'a) 0)
                       (round->exact (~ e1 'x))
                       (- (round->exact (- (~ e1 'x))))))
            )
           ((= wn 0)
            (set! linestart #f)
            ;; 左右対称になるように調整
            (set! x2 (if (> (~ e1 'a) 0)
                       (round->exact (~ e1 'x))
                       (- (round->exact (- (~ e1 'x))))))
            ;; 両端を結ぶ水平線を表示
            (textscrn-over-sub ts x1 y1 (textscrn-repeat-sub ts strdata (+ (- x2 x1) 1)))
            ))
          ;; 次回の水平線と辺の交点のX座標を計算
          ;; (現状程度のスケールであれば、浮動小数点の加算を繰り返しても精度は大丈夫そう)
          (set! (~ e1 'x) (+ (~ e1 'x) (~ e1 'a)))
          )))
    ))

;; 文字列の判定処理
;;   ・テキスト画面の指定範囲に文字列 str のいずれかの文字があれば #t を返す
(define-method textscrn-check-str ((ts <textscrn>)
                                   (str <string>)
                                   (x1 <integer>) (y1 <integer>) (x2 <integer>) (y2 <integer>))
  (let ((w       (~ ts 'width))
        (h       (~ ts 'height))
        (data    (~ ts 'data))
        (strdata (string->u32vector str))
        (ret   #f))
    (let loop ((x3 x1) (y3 y1))
      (if (and (>= y3 0) (< y3 h) (>= x3 0) (< x3 w))
        (let1 c1 (~ data (+ (* y3 w) x3))
          (set! ret (find (lambda (c2) (= c1 c2)) strdata))))
      (if (not ret)
        (cond
         ((< x3 x2) (loop (+ x3 1) y3))
         ((< y3 y2) (loop x1 (+ y3 1)))))
      )
    (if ret (set! ret #t))
    ;(print ret " " x1 " " y1 " " x2 " " y2)
    ret))

;; 文字列の画面上の判定処理
;;   ・画面上の座標範囲 (x1,y1)-(x2,y2) を指定して、textscrn-check-str を実行する
;;   ・1文字の幅 chw と高さ chh も指定が必要
(define-method textscrn-disp-check-str ((ts <textscrn>)
                                        (str <string>)
                                        (x1 <real>) (y1 <real>) (x2 <real>) (y2 <real>)
                                        (chw <real>) (chh <real>)
                                        :optional (xoffset 0) (yoffset 0) (align 'left))
  (let1 w (~ ts 'width)
    (cond
     ((eq? align 'center) ; 中央寄せ
      (set! xoffset (- xoffset (/. (* w chw) 2))))
     ((eq? align 'right)  ; 右寄せ
      (set! xoffset (- xoffset (* w chw)))))
    (let ((x3 (floor->exact (/. (- x1 xoffset) chw)))
          (y3 (floor->exact (/. (- y1 yoffset) chh)))
          (x4 (- (ceiling->exact (/. (- x2 xoffset) chw)) 1))
          (y4 (- (ceiling->exact (/. (- y2 yoffset) chh)) 1)))
      (textscrn-check-str ts str x3 y3 x4 y4)
      )))

;; 文字列の上書き処理サブ(内部処理用)
(define-method textscrn-over-sub ((ts <textscrn>) (x <integer>) (y <integer>) (strdata <u32vector>))
  (let* ((strdatalen (u32vector-length strdata))
         (w  (~ ts 'width))
         (h  (~ ts 'height))
         (i1 (+ (* y w) (clamp x 0 (- w 1))))
         (i2 (clamp (- x)   0 strdatalen))
         (i3 (clamp (- w x) 0 strdatalen)))
    (if (and (>= y 0) (< y h) (< i2 i3))
      (u32vector-copy! (~ ts 'data) i1 strdata i2 i3))))

;; 文字列の繰り返し処理サブ(内部処理用)
(define-method textscrn-repeat-sub ((ts <textscrn>) (strdata <u32vector>) (count <integer>))
  (let ((strdatalen (u32vector-length strdata))
        (strdata1   (make-u32vector count)))
    (let loop ((i 0))
      (cond
       ((< (+ i strdatalen) count)
        (u32vector-copy! strdata1 i strdata 0 strdatalen)
        (loop (+ i strdatalen)))
       (else
        (u32vector-copy! strdata1 i strdata 0 (- count i))))
      )
    strdata1))

;; 文字列の点設定処理サブ(内部処理用)
(define-method textscrn-pset-sub ((ts <textscrn>) (x <integer>) (y <integer>) (c <integer>))
  (let* ((w    (~ ts 'width))
         (h    (~ ts 'height))
         (data (~ ts 'data))
         (i    (+ (* y w) x)))
    (if (and (>= y 0) (< y h) (>= x 0) (< x w))
      (set! (~ data i) c))
    ))

;; 文字列のライン表示処理サブ(内部処理用)
(define-method textscrn-line-sub ((ts <textscrn>)
                                  (x1 <integer>) (y1 <integer>) (x2 <integer>) (y2 <integer>)
                                  (strdata <u32vector>))
  (let ((strdatalen (u32vector-length strdata))
        (x3 x1) (y3 y1) (dx 0) (dy 0) (sx 0) (sy 0) (e1 0))
    (cond
     ((< x1 x2) (set! dx (- x2 x1)) (set! sx  1))
     ((> x1 x2) (set! dx (- x1 x2)) (set! sx -1)))
    (cond
     ((< y1 y2) (set! dy (- y2 y1)) (set! sy  1))
     ((> y1 y2) (set! dy (- y1 y2)) (set! sy -1)))
    (cond
     ((>= dx dy)
      (set! e1 (- dx))
      (do ((i 0 (+ i 1))
           (c (~ strdata 0) (~ strdata (modulo (+ i 1) strdatalen))))
          ((> i dx) #f)
        (textscrn-pset-sub ts x3 y3 c)
        (set! x3 (+ x3 sx))
        (set! e1 (+ e1 (* dy 2)))
        (when (>= e1 0)
          (set! y3 (+ y3 sy))
          (set! e1 (- e1 (* dx 2))))
        )
      )
     (else
      (set! e1 (- dy))
      (do ((i 0 (+ i 1))
           (c (~ strdata 0) (~ strdata (modulo (+ i 1) strdatalen))))
          ((> i dy) #f)
        (textscrn-pset-sub ts x3 y3 c)
        (set! y3 (+ y3 sy))
        (set! e1 (+ e1 (* dx 2)))
        (when (>= e1 0)
          (set! x3 (+ x3 sx))
          (set! e1 (- e1 (* dy 2))))
        )
      )
     )
    ))

;(define *tscrn1* (make <textscrn>)) ; インスタンス生成例
;(textscrn-init *tscrn1* 50 25)


