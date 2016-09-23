;; -*- coding: utf-8 -*-
;;
;; gltextscrn.scm
;; 2016-9-23 v1.42
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
  (use binary.pack)
  (export
    <wininfo> win-init win-update-size
    win-x win-y win-w win-h win-w-r win-h-r
    draw-bitmap-text draw-bitmap-text-over
    draw-stroke-text draw-stroke-text-over
    draw-win-line draw-win-rect draw-win-circle draw-win-poly
    <textscrn> textscrn-init textscrn-disp
    textscrn-cls textscrn-pset textscrn-pget
    textscrn-line textscrn-box textscrn-fbox
    textscrn-circle textscrn-fcircle textscrn-poly textscrn-fpoly
    textscrn-check-str textscrn-disp-check-str
    load-texture-bitmap-file draw-texture-rect
    set-char-texture textscrn-disp-texture
    ))
(select-module gltextscrn)

;; Gauche-gl のテスト時のSEGVエラー対策
;;   ・フォントに直接アクセスするとSEGVエラーになるので、
;;     test-module を実行する前に以下の変数を #f にすること
;;     (Gauche-gl の開発最新版では修正済み)
(define *font-bitmap-1* GLUT_BITMAP_TIMES_ROMAN_24)
(define *font-stroke-1* GLUT_STROKE_ROMAN)


;; ウィンドウ情報クラス
;;   ・ウィンドウ上の画面幅 width と画面高さ height の情報を保持して、
;;     ウィンドウ上の座標を計算可能とする
;;   ・glut-reshape-func のコールバックで win-update-size を呼び出して、
;;     ウィンドウのサイズ変更に追従する必要がある
(define-class <wininfo> ()
  ((width  :init-value 0) ; ウィンドウ上の画面幅(px)
   (height :init-value 0) ; ウィンドウ上の画面高さ(px)
   (wd     :init-value 0) ; OpenGL上の画面幅
   (ht     :init-value 0) ; OpenGL上の画面高さ
   ))
(define-method win-init ((wn <wininfo>)
                         (width <real>) (height <real>) (wd <real>) (ht <real>))
  (set! (~ wn 'width)  width)
  (set! (~ wn 'height) height)
  (set! (~ wn 'wd)     wd)
  (set! (~ wn 'ht)     ht))
(define-method win-update-size ((wn <wininfo>) (width <real>) (height <real>))
  (set! (~ wn 'width)  width)
  (set! (~ wn 'height) height))
;; ウィンドウ上のX座標を計算
(define-method win-x ((wn <wininfo>) (x <real>))
  (+ (/. (* x (~ wn 'width)) (~ wn 'wd))
     (/. (~ wn 'width) 2)))
;; ウィンドウ上のY座標を計算
(define-method win-y ((wn <wininfo>) (y <real>))
  (+ (/. (* (- y) (~ wn 'height)) (~ wn 'ht))
     (/. (~ wn 'height) 2)))
;; ウィンドウ上の幅を計算
(define-method win-w ((wn <wininfo>) :optional (w #f))
  (if w (/. (* w (~ wn 'width)) (~ wn 'wd))
        (~ wn 'width)))
;; ウィンドウ上の高さを計算
(define-method win-h ((wn <wininfo>) :optional (h #f))
  (if h (/. (* h (~ wn 'height)) (~ wn 'ht))
        (~ wn 'height)))
;; ウィンドウ上の幅を、比を指定して計算
(define-method win-w-r ((wn <wininfo>) (n1 <real>) :optional (n2 #f))
  (if n2 (/. (* (~ wn 'width) n1) n2)
         (* (~ wn 'width) (exact n1))))
;; ウィンドウ上の高さを、比を指定して計算
(define-method win-h-r ((wn <wininfo>) (n1 <real>) :optional (n2 #f))
  (if n2 (/. (* (~ wn 'height) n1) n2)
         (* (~ wn 'height) (exact n1))))


;; 正射影設定ON/OFF(内部処理用)
;;   ・投影方法を正射影に設定し、また、
;;     画面の座標系を「左下」を原点として (0,0)-(width,height) の範囲に設定する
;;     (ここで「左上」を原点にはしない (モデルもすべて上下反転してしまうため)
;;      個別の表示ルーチンの方で、必要に応じて、height - y として反転するようにする)
;;     (width,height はウィンドウ上の座標であり、OpenGLとは座標系が異なるので注意)
;;   ・光源は無効に設定する
;;   ・ONとOFFは常にセットで使用する
(define-method gl-ortho-on ((wn <wininfo>))
  (gl-disable GL_LIGHTING)
  (gl-matrix-mode GL_PROJECTION)
  (gl-push-matrix)
  (gl-load-identity)
  (gl-ortho 0 (win-w wn) 0 (win-h wn) -1.0 1.0)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-push-matrix)
  (gl-load-identity))
(define-method gl-ortho-off ((wn <wininfo>))
  (gl-pop-matrix)
  (gl-matrix-mode GL_PROJECTION)
  (gl-pop-matrix)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-enable GL_LIGHTING))

;; 文字列の幅を取得(ビットマップフォント)(内部処理用)
(define (get-bitmap-text-width str font)
  (string-fold (lambda (ch n) (+ (glut-bitmap-width font (char->integer ch)) n)) 0 str))

;; 文字列表示(ビットマップフォント)
;;   ・座標 (x,y) は左上を原点として (0,0)-(width,height) の範囲で指定する
;;     (width,height はウィンドウ上の座標であり、OpenGLとは座標系が異なるので注意)
;;   ・日本語表示不可
;;   ・文字のサイズはフォントにより固定
(define-method draw-bitmap-text ((wn <wininfo>)
                                 (str <string>) (x <real>) (y <real>)
                                 :optional (size 24) (align 'left)
                                 (font *font-bitmap-1*))
  (unless (equal? str "")
    (gl-ortho-on wn)
    (let ((x1 x)
          (y1 (- (win-h wn) y size)))
      (if (memq align '(center right))
        (let1 stw (get-bitmap-text-width str font)
          (set! x1 (if (eq? align 'center)
                     (- x (/. stw 2)) ; 中央寄せ
                     (- x stw)))))    ; 右寄せ
      (gl-raster-pos x1 y1)
      (string-for-each (lambda (ch) (glut-bitmap-character font (char->integer ch))) str)
      )
    (gl-ortho-off wn)))

;; 文字列の上書き表示(ビットマップフォント)
;;   ・背景を塗りつぶしてから draw-bitmap-text を実行する
(define-method draw-bitmap-text-over ((wn <wininfo>)
                                      (str <string>) (x <real>) (y <real>)
                                      :optional (size 24) (align 'left)
                                      (fore-color #f32(1.0 1.0 1.0 1.0))
                                      (back-color #f32(0.0 0.0 0.0 1.0))
                                      (back-scalex 1.2) (back-scaley 1.2)
                                      (z 0) (font *font-bitmap-1*))
  (unless (equal? str "")
    (if fore-color (gl-color fore-color))
    (draw-bitmap-text wn str x y size align font)
    (if back-color (gl-color back-color))
    (let* ((stw     (get-bitmap-text-width str font))
           (xoffset (case align
                      ((left)  (- (* stw (/. (- back-scalex 1.0) 2))))
                      ((right)    (* stw (/. (- back-scalex 1.0) 2)))
                      (else    0)))
           (yoffset (+ (- (* size (/. (- back-scaley 1.0) 2)))
                       (* size (/. 33.33 152.38)))))
      (draw-win-rect wn (+ x xoffset) (+ y yoffset)
                     (* stw back-scalex) (* size back-scaley) align z)
      )))

;; 文字列の幅を取得(ストロークフォント)(内部処理用)
(define (get-stroke-text-width str font)
  (string-fold (lambda (ch n) (+ (glut-stroke-width font (char->integer ch)) n)) 0 str))

;; 文字列表示(ストロークフォント)
;;   ・座標 (x,y) は左上を原点として (0,0)-(width,height) の範囲で指定する
;;     (width,height はウィンドウ上の座標であり、OpenGLとは座標系が異なるので注意)
;;   ・日本語表示不可
;;   ・文字のサイズは指定可能
(define-method draw-stroke-text ((wn <wininfo>)
                                 (str <string>) (x <real>) (y <real>)
                                 :optional (size 24) (align 'left)
                                 (font *font-stroke-1*))
  (unless (equal? str "")
    (gl-ortho-on wn)
    (let ((x1      x)
          (y1      (- (win-h wn) y size))
          (scale   (/. size (+ 152.38 20)))
          (xoffset 0)
          (yoffset (+ 33.33 10)))
      (if (memq align '(center right))
        (let1 stw (get-stroke-text-width str font)
          (set! xoffset (if (eq? align 'center)
                          (- (/. stw 2)) ; 中央寄せ
                          (- stw)))))    ; 右寄せ
      (gl-translate x1 y1 0)
      (gl-scale scale scale 1)
      (gl-translate xoffset yoffset 0)
      (string-for-each (lambda (ch) (glut-stroke-character font (char->integer ch))) str)
      )
    (gl-ortho-off wn)))

;; 文字列の上書き表示(ストロークフォント)
;;   ・背景を塗りつぶしてから draw-stroke-text を実行する
(define-method draw-stroke-text-over ((wn <wininfo>)
                                      (str <string>) (x <real>) (y <real>)
                                      :optional (size 24) (align 'left)
                                      (fore-color #f32(1.0 1.0 1.0 1.0))
                                      (back-color #f32(0.0 0.0 0.0 1.0))
                                      (back-scalex 1.2) (back-scaley 1.2)
                                      (z 0) (font *font-stroke-1*))
  (unless (equal? str "")
    (if fore-color (gl-color fore-color))
    (draw-stroke-text wn str x y size align font)
    (if back-color (gl-color back-color))
    (let* ((stw     (get-stroke-text-width str font))
           (w1      (* stw (/. size (+ 152.38 20))))
           (xoffset (case align
                      ((left)  (- (* w1 (/. (- back-scalex 1.0) 2))))
                      ((right)    (* w1 (/. (- back-scalex 1.0) 2)))
                      (else    0)))
           (yoffset (- (* size (/. (- back-scaley 1.0) 2)))))
      (draw-win-rect wn (+ x xoffset) (+ y yoffset)
                     (* w1 back-scalex) (* size back-scaley) align z)
      )))

;; 線の表示
;;   ・線 (x1,y1)-(x2,y2) の表示を行う
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
;;     (width,height はウィンドウ上の座標であり、OpenGLとは座標系が異なるので注意)
(define-method draw-win-line ((wn <wininfo>)
                              (x1 <real>) (y1 <real>) (x2 <real>) (y2 <real>)
                              :optional (z 0))
  (gl-ortho-on wn)
  (let ((y3 (- (win-h wn) y1))
        (y4 (- (win-h wn) y2)))
    (gl-translate 0 0 z)
    (gl-begin GL_LINES)
    (gl-vertex (f32vector x1 y3))
    (gl-vertex (f32vector x2 y4))
    (gl-end)
    )
  (gl-ortho-off wn))

;; 長方形の表示
;;   ・長方形 (x,y,w,h) の表示を行う
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
;;     (width,height はウィンドウ上の座標であり、OpenGLとは座標系が異なるので注意)
(define-method draw-win-rect ((wn <wininfo>)
                              (x <real>) (y <real>) (w <real>) (h <real>)
                              :optional (align 'left) (z 0))
  (gl-ortho-on wn)
  (let ((x1 (case align
              ((center) (- x (/. w 2))) ; 中央寄せ
              ((right)  (- x w))        ; 右寄せ
              (else     x)))
        (y1 (- (win-h wn) y)))
    (gl-translate x1 y1 z)
    ;; Gauche-gl の gl-rect の不具合対策
    ;; (Gauche-gl の開発最新版では修正済み)
    ;(gl-rect 0 0 w (- h))
    (gl-rect (f32vector 0 0) (f32vector w (- h)))
    )
  (gl-ortho-off wn))

;; 円の表示
;;   ・円 (x,y,r,a,b) -> (x*x)/(a*a)+(y*y)/(b*b)=r*r の表示を行う
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
;;     (width,height はウィンドウ上の座標であり、OpenGLとは座標系が異なるので注意)
(define-method draw-win-circle ((wn <wininfo>)
                                (x <real>) (y <real>) (r <real>)
                                :optional (a 1) (b 1) (align 'center) (z 0))
  (gl-ortho-on wn)
  (let ((x1 (case align
              ((left)  (+ x r)) ; 左寄せ
              ((right) (- x r)) ; 右寄せ
              (else     x)))
        (y1 (- (win-h wn) y))
        (q  (make <glu-quadric>)))
    (if (= a 0) (set! a 1))
    (if (= b 0) (set! b 1))
    (gl-translate x1 y1 z)
    (gl-scale (/. 1 a) (/. 1 b) 1)
    (glu-disk q 0 r 40 1)
    )
  (gl-ortho-off wn))

;; 多角形の表示
;;   ・頂点の座標(f32vector x y)を複数格納したベクタvvecを渡して、多角形の表示を行う
;;     (面は頂点が反時計回りになる方が表になる)
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
;;     (width,height はウィンドウ上の座標であり、OpenGLとは座標系が異なるので注意)
(define-method draw-win-poly ((wn <wininfo>)
                              (x <real>) (y <real>) (vvec <vector>)
                              :optional (z 0))
  (gl-ortho-on wn)
  (let1 y1 (- (win-h wn) y)
    (gl-translate x y1 z)
    (gl-scale 1 -1  1)
    (gl-begin GL_POLYGON)
    (for-each gl-vertex vvec)
    (gl-end)
    )
  (gl-ortho-off wn))


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

;; 文字情報テーブル(文字列の一括表示用)(内部処理用)
;;   ・文字ごとに倍率とオフセット値を設定して、等幅フォントのように表示可能とする
;;   ・フォントは固定
(define-class <char-info> () (xscale/fchw yscale/fchh xoffset yoffset)) ; 文字情報クラス
(define *char-info-num*   128)
(define *char-info-table*
  ;; Gauche-gl を初期化するまでは実行できないので、実行を遅延する
  (delay
    (rlet1 tbl (make-vector *char-info-num* #f)
      (do ((i 0 (+ i 1)))
          ((>= i *char-info-num*) #f)
        (let* ((c1     (make <char-info>))
               (fchw-1 (glut-stroke-width *font-stroke-1* i))
               (fchw   (if (<= fchw-1 0) 104.76 fchw-1))
               (fchh   (+ 152.38 20))
               (xscale&xoffset
                ;; X方向の倍率とオフセット値を設定
                (case i
                  ;; !  ,  .  :  ; (幅が狭いので特別扱い)
                  ((33 44 46 58 59) '(0.4  . 16))
                  ;; " (幅が狭いので特別扱い)
                  ((34)  '(0.4  . 28))
                  ;; ' (幅が狭いので特別扱い)
                  ((39)  '(0.4  . 12))
                  ;; ` (幅が狭いので特別扱い)
                  ((96)  '(0.4  . 42))
                  ;; その他の文字
                  (else  '(0.9  .  0))))
               (yscale&yoffset
                ;; Y方向の倍率とオフセット値を設定
                (case i
                  ;; (ベースラインの分だけ上にずらす)
                  ;; #  $  (  )  ,  .  /  :  ;  [  \  ]  ^  _  g   j   p   q   y   {   |   }  phi
                  ((35 36 40 41 44 46 47 58 59 91 92 93 94 95 103 106 112 113 121 123 124 125 127)
                   `(0.9  . ,(+ 33.33 10)))
                  ;; @ (特に小さいので特別扱い)
                  ((64)  '(2.0  . -6))
                  ;; i (jと高さを合わせるために特別扱い)
                  ((105) '(1.18 . 10))
                  ;; その他の文字
                  (else  '(1.3  . 10)))))
          (set! (~ c1 'xscale/fchw) (/. (car xscale&xoffset) fchw))
          (set! (~ c1 'yscale/fchh) (/. (car yscale&yoffset) fchh))
          (set! (~ c1 'xoffset) (cdr xscale&xoffset))
          (set! (~ c1 'yoffset) (cdr yscale&yoffset))
          (set! (~ tbl i) c1))))))

;; 文字列の一括表示
;;   ・文字ごとに倍率とオフセット値を適用して、等幅フォントのように表示する
;;   ・座標 (x,y) は左上を原点として (0,0)-(width,height) の範囲で指定する
;;     (width,height はウィンドウ上の座標であり、OpenGLとは座標系が異なるので注意)
;;   ・日本語表示不可
;;   ・文字のサイズは、幅 chw と高さ chh の指定が必要
;;   ・フォントは固定
(define-method textscrn-disp ((ts <textscrn>)
                              (wn <wininfo>)
                              (x <real>) (y <real>)
                              (chw <real>) (chh <real>)
                              :optional (align 'left))
  (gl-ortho-on wn)
  (let* ((w1 (~ ts 'width))
         (x1 (case align
               ((center) (- x (/. (* w1 chw) 2))) ; 中央寄せ
               ((right)  (- x (* w1 chw)))        ; 右寄せ
               (else     x)))
         (x2 x1)
         (y1 (- (win-h wn) y chh))
         (i  0))
    (for-each
     (lambda (c)
       (if (and (>= c 0) (< c *char-info-num*))
         (let1 c1 (~ (force *char-info-table*) c)
           (gl-load-identity)
           (gl-translate x1 y1 0)
           (gl-scale (* (~ c1 'xscale/fchw) chw) (* (~ c1 'yscale/fchh) chh) 1)
           (gl-translate (~ c1 'xoffset) (~ c1 'yoffset) 0)
           (glut-stroke-character *font-stroke-1* c)))
       (set! x1 (+ x1 chw))
       (inc! i)
       (when (>= i w1)
         (set! i  0)
         (set! x1 x2)
         (set! y1 (- y1 chh)))
       )
     (~ ts 'data))
    )
  (gl-ortho-off wn))

;; 文字列のクリア処理
(define-method textscrn-cls ((ts <textscrn>))
  (u32vector-fill! (~ ts 'data) 0))

;; 文字列の点設定処理
(define-method textscrn-pset ((ts <textscrn>) (x <integer>) (y <integer>) (str <string>))
  (textscrn-over-sub ts x y (string->u32vector str)))

;; 文字列の点取得処理
(define-method textscrn-pget ((ts <textscrn>) (x <integer>) (y <integer>) :optional (n 1))
  (let* ((w1 (~ ts 'width))
         (h1 (~ ts 'height))
         (x1 (max 0 x))
         (x2 (min (- w1 1) (+ x n)))
         (i1 (* y w1)))
    (if (and (>= y 0) (< y h1) (< x1 x2))
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
  (let* ((strdata  (string->u32vector str))
         (x3       (min x1 x2))
         (y3       (min y1 y2))
         (x4       (max x1 x2))
         (y4       (max y1 y2))
         (strdata1 (textscrn-repeat-sub ts strdata (+ (- x4 x3) 1)))
         (c        (~ strdata 0)))
    (textscrn-over-sub ts x3 y3 strdata1)
    (textscrn-over-sub ts x3 y4 strdata1)
    (do ((i y3 (+ i 1)))
        ((> i y4) #f)
      (textscrn-pset-sub ts x3 i c)
      (textscrn-pset-sub ts x4 i c))
    ))

;; 文字列の四角形塗りつぶし表示処理
(define-method textscrn-fbox ((ts <textscrn>)
                              (x1 <integer>) (y1 <integer>) (x2 <integer>) (y2 <integer>)
                              (str <string>))
  (let* ((strdata  (string->u32vector str))
         (x3       (min x1 x2))
         (y3       (min y1 y2))
         (x4       (max x1 x2))
         (y4       (max y1 y2))
         (strdata1 (textscrn-repeat-sub ts strdata (+ (- x4 x3) 1))))
    (do ((i y3 (+ i 1)))
        ((> i y4) #f)
      (textscrn-over-sub ts x3 i strdata1))
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
  (let ((pnum      (length point))
        (x1 0) (y1 0) (x2 0) (y2 0) (miny 0) (maxy 0)
        (e1        #f)  ; 辺情報
        (edgelist  '()) ; 辺情報のリスト
        (enum      0)   ; 辺の数
        (wnum      0)   ; 巻き数
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
      (set! wnum 0)
      (set! linestart #f)
      (do ((i 0 (+ i 1)))
          ((>= i enum) #f)
        (set! e1 (~ edgelist i))
        ;; 交点があるとき
        (when (and (>= y1 (~ e1 'y1)) (<= y1 (~ e1 'y2)))
          ;; 辺のY方向の向きから巻き数を計算して、0になるまでの領域を塗りつぶす
          (set! wnum (+ wnum (~ e1 'ydir)))
          (cond
           ((not linestart)
            (set! linestart #t)
            ;; 左右対称になるように調整
            (set! x1 (if (> (~ e1 'a) 0)
                       (round->exact (~ e1 'x))
                       (- (round->exact (- (~ e1 'x))))))
            )
           ((= wnum 0)
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
  (let ((ret     #f)
        (w1      (~ ts 'width))
        (h1      (~ ts 'height))
        (data    (~ ts 'data))
        (strdata (string->u32vector str)))
    (let loop ((x3 x1) (y3 y1))
      (if (and (>= y3 0) (< y3 h1) (>= x3 0) (< x3 w1))
        (let1 c1 (~ data (+ (* y3 w1) x3))
          (for-each (lambda (c2) (if (= c1 c2) (set! ret #t))) strdata)))
      (if (not ret)
        (cond
         ((< x3 x2) (loop (+ x3 1) y3))
         ((< y3 y2) (loop x1 (+ y3 1)))))
      )
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
  (if (memq align '(center right))
    (let1 w1 (~ ts 'width)
      (set! xoffset (if (eq? align 'center)
                      (- xoffset (/. (* w1 chw) 2)) ; 中央寄せ
                      (- xoffset (* w1 chw))))))    ; 右寄せ
  (let ((x3 (floor->exact (/. (- x1 xoffset) chw)))
        (y3 (floor->exact (/. (- y1 yoffset) chh)))
        (x4 (- (ceiling->exact (/. (- x2 xoffset) chw)) 1))
        (y4 (- (ceiling->exact (/. (- y2 yoffset) chh)) 1)))
    (textscrn-check-str ts str x3 y3 x4 y4)
    ))

;; 文字列の上書き処理サブ(内部処理用)
(define-method textscrn-over-sub ((ts <textscrn>) (x <integer>) (y <integer>) (strdata <u32vector>))
  (let* ((strdatalen (u32vector-length strdata))
         (w1         (~ ts 'width))
         (h1         (~ ts 'height))
         (i1         (+ (* y w1) (clamp x 0 (- w1 1))))
         (i2         (clamp (- x)    0 strdatalen))
         (i3         (clamp (- w1 x) 0 strdatalen)))
    (if (and (>= y 0) (< y h1) (< i2 i3))
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
  (let* ((w1   (~ ts 'width))
         (h1   (~ ts 'height))
         (data (~ ts 'data))
         (i    (+ (* y w1) x)))
    (if (and (>= y 0) (< y h1) (>= x 0) (< x w1))
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


;; 画像データクラス(内部処理用)
(define-class <imgdata> ()
  ((width  :init-value 0) ; 画像データの幅(単位:px)
   (height :init-value 0) ; 画像データの高さ(単位:px)
   (data   :init-form (make-u8vector 0)) ; 画像データ(u8vector)(1画素は要素4個(RGBA))
   ))

;; ビットマップファイルを読み込み 画像データを生成する(内部処理用)
;;   ・ビットマップファイルは、24bitカラーで無圧縮のもののみ使用可能
;;   ・画像サイズは、幅も高さも 2のべき乗 である必要がある
;;   ・透明色はオプション引数に '(R G B) のリストで指定する(各色は0-255の値)
(define-method imgdata-load-bitmap-file ((img <imgdata>)
                                         (file <string>) :optional (trans-color #f))
  (define (err msg . rest)
    (apply errorf (cons (format "bitmap file load error (file=~a)\n~a" file msg)
                        rest)))
  (define (get-param header index)
    (let1 p (~ header index)
      (if (eof-object? p)
        (err "file size is too small")
        p)))
  (define (read-one-data in)
    (let1 d (read-byte in)
      (if (eof-object? d)
        (err "file size is too small")
        d)))
  (call-with-input-file file
    (lambda (in)
      ;; ファイルヘッダーの読み込み
      (let* ((file-header (unpack "nVvvV" :input in))
             (ftype       (get-param file-header 0))
             (fsize       (get-param file-header 1))
             (foffbits    (get-param file-header 4))
             (pos         0))
        (unless (= ftype #x424D)
          (err "file type is invalid (ftype=~4,'0Xh)" ftype))
        ;; (fsize はチェックしない)
        ;(unless (>= fsize 0)
        ;  (err "file size is invalid (fsize=~d)" fsize))
        (unless (>= foffbits 0)
          (err "file offset is invalid (foffbits=~d)" foffbits))
        (set! pos (+ pos 14))
        ;; 情報ヘッダーの読み込み
        (let* ((info-header  (unpack "VV!V!vvVVV!V!VV" :input in))
               (isize        (get-param info-header 0))
               (iwidth       (get-param info-header 1))
               (iheight      (get-param info-header 2))
               (ibitcount    (get-param info-header 4))
               (icompression (get-param info-header 5))
               (isizeimage   (get-param info-header 6)))
          ;; (サイズの大きい拡張版が存在する)
          ;(unless (= isize 40)
          (unless (>= isize 40)
            (err "can't load this type of bitmap (isize=~d)" isize))
          (unless (>= iwidth 0)
            (err "can't load this type of bitmap (iwidth=~d)" iwidth))
          (unless (>= iheight 0)
            (err "can't load this type of bitmap (iheight=~d)" iheight))
          (unless (= ibitcount 24)
            (err "can't load this type of bitmap (ibitcount=~d)" ibitcount))
          (unless (= icompression 0)
            (err "can't load this type of bitmap (icompression=~d)" icompression))
          ;; (isizeimage はチェックしない)
          ;(unless (>= isizeimage 0)
          ;  (err "can't load this type of bitmap (isizeimage=~d)" isizeimage))
          (set! pos (+ pos 40))
          ;; データの読み込み
          ;; (上下反転しているので、ここで戻す)
          (let* ((data-size (* iwidth iheight))
                 (data      (make-u8vector (* data-size 4) 0))
                 (trans-r   (list-ref trans-color 0 -1))
                 (trans-g   (list-ref trans-color 1 -1))
                 (trans-b   (list-ref trans-color 2 -1)))
            (do ((i pos (+ i 1))
                 (j 0 j)
                 (k (* iwidth 4 (- iheight 1)) k)
                 (c 0 c)
                 (x 0 x))
                ;; (fsize と isizeimage はチェックしない)
                ;((or (>= i fsize) (>= j isizeimage) (>= c data-size)) #f)
                ((>= c data-size) #f)
              ;(print i " " fsize " " j " " isizeimage " " c " " data-size)
              (cond
               ;; オフセットの位置まで読み飛ばす
               ((< i foffbits)
                (read-one-data in))
               ;; 1ライン読み込み後は、4バイト境界まで読み飛ばす
               ((and (>= x iwidth) (not (= (modulo j 4) 0)))
                (read-one-data in)
                (inc! j))
               ;; 1画素分のデータを読み込む
               (else
                (let* ((b (read-one-data in))
                       (g (read-one-data in))
                       (r (read-one-data in)))
                  (set! (~ data k)       r)
                  (set! (~ data (+ k 1)) g)
                  (set! (~ data (+ k 2)) b)
                  (set! (~ data (+ k 3))
                        (if (and (= r trans-r) (= g trans-g) (= b trans-b)) 0 255))
                  (set! j (+ j 3))
                  (set! k (+ k 4))
                  (inc! c)
                  (inc! x)
                  (when (and (>= x iwidth) (= (modulo j 4) 0))
                    (set! x 0)
                    (set! k (- k (* iwidth 4 2))))
                  )))
              )
            ;; 戻り値をセット
            ;(print iwidth " " iheight " " data)
            (set! (~ img 'width)  iwidth)
            (set! (~ img 'height) iheight)
            (set! (~ img 'data)   data))
          ))
      )))

;; テクスチャに画像データを設定する(内部処理用)
;;   各種パラメータは決め打ち
;;   画像サイズは、幅も高さも 2のべき乗 である必要がある
(define-method imgdata-set-texture ((img <imgdata>) tex)
  (gl-bind-texture  GL_TEXTURE_2D tex)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (gl-tex-image-2d  GL_TEXTURE_2D 0 GL_RGBA
                    (~ img 'width) (~ img 'height) 0
                    GL_RGBA GL_UNSIGNED_BYTE (~ img 'data))
  )

;; ビットマップファイルを読み込み テクスチャに設定する
;;   ・ビットマップファイルは、24bitカラーで無圧縮のもののみ使用可能
;;   ・画像サイズは、幅も高さも 2のべき乗 である必要がある
;;   ・透明色はオプション引数に '(R G B) のリストで指定する(各色は0-255の値)
(define (load-texture-bitmap-file tex file :optional (trans-color #f))
  (let1 img (make <imgdata>)
    (imgdata-load-bitmap-file img file trans-color)
    (imgdata-set-texture img tex)
    ))

;; テクスチャ付き長方形の表示
;;   ・テクスチャ tex を貼り付けた長方形 (x,y,w,h) の表示を行う
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
;;     (width,height はウィンドウ上の座標であり、OpenGLとは座標系が異なるので注意)
(define-method draw-texture-rect ((wn <wininfo>)
                                  (tex <integer>)
                                  (x <real>) (y <real>)
                                  (w <real>) (h <real>)
                                  :optional (align 'left) (z 0)
                                  (xcrd 1.0) (ycrd 1.0))
  (gl-ortho-on wn)
  (gl-enable GL_TEXTURE_2D)
  (let ((x1 (case align
              ((center) (- x (/. w 2))) ; 中央寄せ
              ((right)  (- x w))        ; 右寄せ
              (else     x)))
        (y1 (- (win-h wn) y)))
    (gl-bind-texture GL_TEXTURE_2D tex)
    (gl-translate x1 y1 z)
    (gl-begin GL_QUADS)
    ;; (面は頂点が反時計回りになる方が表になる)
    (gl-tex-coord 0.0  0.0)  (gl-vertex (f32vector 0 0     0))
    (gl-tex-coord 0.0  ycrd) (gl-vertex (f32vector 0 (- h) 0))
    (gl-tex-coord xcrd ycrd) (gl-vertex (f32vector w (- h) 0))
    (gl-tex-coord xcrd 0.0)  (gl-vertex (f32vector w 0     0))
    (gl-end))
  (gl-disable GL_TEXTURE_2D)
  (gl-ortho-off wn))

;; 文字テクスチャ割り付け用テーブル(テクスチャの一括表示用)(内部処理用)
(define-class <tex-info> () (tex xoffset yoffset xcrd ycrd)) ; テクスチャ情報クラス
(define *char-tex-table* (make-hash-table 'eqv?))

;; 文字にテクスチャを割り付ける(テクスチャの一括表示用)
(define (set-char-texture ch tex :optional (xoffset 0) (yoffset 0)
                          (xcrd 1.0) (ycrd 1.0))
  (let1 t (make <tex-info>)
    (set! (~ t 'tex)     tex)
    (set! (~ t 'xoffset) xoffset)
    (set! (~ t 'yoffset) yoffset)
    (set! (~ t 'xcrd)    xcrd)
    (set! (~ t 'ycrd)    ycrd)
    (hash-table-put! *char-tex-table* (char->integer ch) t)
    ))

;; 文字に割り付けたテクスチャの一括表示
;;   ・座標 (x,y) は左上を原点として (0,0)-(width,height) の範囲で指定する
;;     (width,height はウィンドウ上の座標であり、OpenGLとは座標系が異なるので注意)
;;   ・1文字あたりの表示サイズは、幅 chw と高さ chh の指定が必要
(define-method textscrn-disp-texture ((ts <textscrn>)
                                      (wn <wininfo>)
                                      (x <real>) (y <real>)
                                      (chw <real>) (chh <real>)
                                      :optional (align 'left) (z 0))
  (gl-ortho-on wn)
  (gl-enable GL_TEXTURE_2D)
  (let* ((w1 (~ ts 'width))
         (x1 (case align
               ((center) (- x (/. (* w1 chw) 2))) ; 中央寄せ
               ((right)  (- x (* w1 chw)))        ; 右寄せ
               (else     x)))
         (x2 x1)
         (y1 (- (win-h wn) y))
         (i  0))
    (for-each
     (lambda (c)
       (if-let1 t (hash-table-get *char-tex-table* c #f)
         (let ((tex  (~ t 'tex))
               (x3   (+ x1 (~ t 'xoffset)))
               (y3   (- y1 (~ t 'yoffset)))
               (xcrd (~ t 'xcrd))
               (ycrd (~ t 'ycrd)))
           (gl-bind-texture GL_TEXTURE_2D tex)
           (gl-load-identity)
           (gl-translate x3 y3 z)
           (gl-begin GL_QUADS)
           ;; (面は頂点が反時計回りになる方が表になる)
           (gl-tex-coord 0.0  0.0)  (gl-vertex (f32vector 0   0       0))
           (gl-tex-coord 0.0  ycrd) (gl-vertex (f32vector 0   (- chh) 0))
           (gl-tex-coord xcrd ycrd) (gl-vertex (f32vector chw (- chh) 0))
           (gl-tex-coord xcrd 0.0)  (gl-vertex (f32vector chw 0       0))
           (gl-end)))
       (set! x1 (+ x1 chw))
       (inc! i)
       (when (>= i w1)
         (set! i  0)
         (set! x1 x2)
         (set! y1 (- y1 chh)))
       )
     (~ ts 'data))
    )
  (gl-disable GL_TEXTURE_2D)
  (gl-ortho-off wn))


