;; -*- coding: utf-8 -*-
;;
;; gltextscrn.scm
;; 2023-1-10 v2.08
;;
;; ＜内容＞
;;   Gauche-gl を使って文字列の表示等を行うためのモジュールです。
;;   (日本語表示不可)
;;
(define-module gltextscrn
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use gauche.collection)
  (use gauche.record)
  (use math.const)
  (use srfi-13) ; string-fold,string-for-each用
  (use binary.pack)
  (export
    *font-bitmap-1* *font-stroke-1* *use-gl-texture-rectangle*
    ;; 正射影設定ON/OFF
    %win-ortho-on %win-ortho-off
    ;; 拡大縮小、回転、平行移動
    %win-scale %win-rotate %win-translate
    ;; クリッピング
    %win-clip %win-clip-off
    ;; 各種描画
    ;; (「%」付きの手続きは、内部で 正射影設定ON/OFF をしない
    ;;  (このため、呼び出し側で 正射影設定ON/OFF を行う必要がある))
    %draw-bitmap-text       draw-bitmap-text
    %draw-bitmap-text-over  draw-bitmap-text-over
    %draw-stroke-text       draw-stroke-text
    %draw-stroke-text-over  draw-stroke-text-over
    %draw-win-line          draw-win-line
    %draw-win-rect          draw-win-rect
    %draw-win-circle        draw-win-circle
    %draw-win-poly          draw-win-poly
    %draw-win-rect-line     draw-win-rect-line
    %draw-win-circle-line   draw-win-circle-line
    %draw-win-poly-line     draw-win-poly-line
    ;; テキスト画面クラス
    <textscrn> textscrn-init
    %textscrn-disp          textscrn-disp
    textscrn-cls textscrn-pset textscrn-pget
    textscrn-line textscrn-box textscrn-fbox
    textscrn-circle textscrn-fcircle textscrn-poly textscrn-fpoly
    textscrn-check-str textscrn-disp-check-str textscrn-disp-check-str2
    ;; テクスチャデータクラス
    <texdata> load-texture-bitmap-file
    %draw-texture-rect      draw-texture-rect
    ;; 文字-テクスチャデータ割り付けクラス
    <char-texture> set-char-texture
    %textscrn-disp-texture  textscrn-disp-texture
    ;; 文字-描画手続き割り付けクラス
    <char-drawer> set-char-drawer
    %textscrn-disp-drawer   textscrn-disp-drawer
    ))
(select-module gltextscrn)

;; Gauche-gl のテスト時のSEGVエラー対策
;;   ・フォントに直接アクセスするとSEGVエラーになるので、
;;     test-module を実行する前に以下の変数を #f にすること
;;     (Gauche-gl の開発最新版では修正済み)
(define *font-bitmap-1* GLUT_BITMAP_TIMES_ROMAN_24)
(define *font-stroke-1* GLUT_STROKE_ROMAN)

;; OpenGLのテクスチャ系関数のターゲットに GL_TEXTURE_RECTANGLE を使用する設定
;;   ・#t にすると、テクスチャの 幅と高さが 2のべき乗 でなくてもよくなる
;;     (ただし、テクスチャのリピート指定が効かない、MIPMAPが使用できない等の
;;      制約もあるもよう)
(define *use-gl-texture-rectangle* #t)
(define GL_TEXTURE_RECTANGLE
  (global-variable-ref 'gl 'GL_TEXTURE_RECTANGLE #x84f5))


;; 正射影設定ON/OFF(内部処理用)
;;   ・投影方法を正射影に設定し、また、
;;     画面の座標系を「左下」を原点として (0,0)-(width,height) の範囲に設定する
;;     (ここで「左上」を原点にはしない (モデルもすべて上下反転してしまうため)
;;      個別の表示ルーチンの方で、必要に応じて、height - y として反転するようにする)
;;   ・Z座標の範囲は -1.0 ～ 1.0 に設定 (カメラに近い側が正)
;;   ・光源は無効に設定する
;;   ・ONとOFFは常にセットで使用すること
(define (%win-ortho-on width height)
  (gl-disable GL_LIGHTING)
  (gl-matrix-mode GL_PROJECTION)
  (gl-push-matrix)
  (gl-load-identity)
  (gl-ortho 0 width 0 height -1.0 1.0)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-push-matrix)
  (gl-load-identity))
(define (%win-ortho-off)
  (gl-enable GL_LIGHTING)
  (gl-matrix-mode GL_PROJECTION)
  (gl-pop-matrix)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-pop-matrix))


;; 拡大縮小設定
;;   ・X,Y方向の倍率 (sx,sy) と中心座標 (ox,oy) を指定して拡大縮小を行う
;;   ・座標 (ox,oy) は、左上を原点として (0,0)-(width,height) の範囲で指定する
(define (%win-scale sx sy ox oy width height)
  (let1 oy1 (- height oy)
    (gl-translate ox oy1 0)
    (gl-scale sx sy 1)
    (gl-translate (- ox) (- oy1) 0)
    ))

;; 回転設定
;;   ・回転角 angle (単位:度) と中心座標 (ox,oy) を指定して回転を行う
;;   ・座標 (ox,oy) は、左上を原点として (0,0)-(width,height) の範囲で指定する
(define (%win-rotate angle ox oy width height)
  (let1 oy1 (- height oy)
    (gl-translate ox oy1 0)
    (gl-rotate angle 0 0 1)
    (gl-translate (- ox) (- oy1) 0)
    ))

;; 平行移動設定
;;   ・X,Y方向の移動量 (dx,dy) を指定して平行移動を行う
;;   ・移動量 (dx,dy) は、左上を原点として (0,0)-(width,height) の範囲で指定する
(define (%win-translate dx dy width height)
  (gl-translate dx (- dy) 0))


;; クリッピング設定
;;   ・長方形 (x,y,w,h) の範囲を、クリッピングエリアに設定する (wとhは幅と高さ)
;;     (以後、クリッピングエリアの外側は、描画されない)
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
;;   ・事前に (gl-enable GL_SCISSOR_TEST) を実行しておく必要がある
;;   ・gl-viewport を使用していた場合、ビューポートの左上座標をオフセットに指定する必要あり
(define (%win-clip x y width height :optional (xoffset 0) (yoffset 0))
  (gl-scissor (truncate->exact (+ x xoffset))
              (truncate->exact (+ y yoffset))
              (truncate->exact width)
              (truncate->exact height)))

;; クリッピング解除
(define (%win-clip-off)
  (gl-scissor 0 0 (glut-get GLUT_WINDOW_WIDTH) (glut-get GLUT_WINDOW_HEIGHT)))


;; 文字列の幅を取得(ビットマップフォント)(内部処理用)
(define (get-bitmap-text-width str font)
  (string-fold (lambda (ch n) (+ (glut-bitmap-width font (char->integer ch)) n)) 0 str))

;; 文字列表示(ビットマップフォント)
;;   ・座標 (x,y) は左上を原点として (0,0)-(width,height) の範囲で指定する
;;   ・日本語表示不可
;;   ・文字のサイズはフォントにより固定
(define (%draw-bitmap-text str x y width height
                           :optional (size 24) (align 'left) (z 0)
                           (font *font-bitmap-1*))
  (unless (equal? str "")
    (let* ((stw (get-bitmap-text-width str font))
           (x1  (case align
                  ((center) (- x (/. stw 2))) ; 中央寄せ
                  ((right)  (- x stw))        ; 右寄せ
                  (else     x)))
           (y1  (- height y size)))
      (gl-raster-pos x1 y1 z)
      (string-for-each (lambda (ch) (glut-bitmap-character font (char->integer ch))) str)
      )))
(define (draw-bitmap-text str x y width height
                          :optional (size 24) (align 'left) (z 0)
                          (font *font-bitmap-1*))
  (%win-ortho-on width height)
  (%draw-bitmap-text str x y width height size align z font)
  (%win-ortho-off))

;; 文字列の上書き表示(ビットマップフォント)
;;   ・背景を塗りつぶしてから draw-bitmap-text を実行する
(define (%draw-bitmap-text-over str x y width height
                                :optional (size 24) (align 'left) (z 0)
                                (fore-color #f32(1.0 1.0 1.0 1.0))
                                (back-color #f32(0.0 0.0 0.0 1.0))
                                (back-scalex 1.2) (back-scaley 1.2)
                                (font *font-bitmap-1*))
  (unless (equal? str "")
    (if fore-color (gl-color fore-color))
    (%draw-bitmap-text str x y width height size align z font)
    (if back-color (gl-color back-color))
    (let* ((stw     (get-bitmap-text-width str font))
           (xoffset (case align
                      ((left)  (- (* stw (/. (- back-scalex 1.0) 2)))) ; 左寄せ
                      ((right)    (* stw (/. (- back-scalex 1.0) 2)))  ; 右寄せ
                      (else    0)))
           (yoffset (+ (- (* size (/. (- back-scaley 1.0) 2)))
                       (* size (/. 33.33 152.38)))))
      (%draw-win-rect (+ x xoffset) (+ y yoffset)
                      (* stw back-scalex) (* size back-scaley)
                      width height align z)
      )))
(define (draw-bitmap-text-over str x y width height
                               :optional (size 24) (align 'left) (z 0)
                               (fore-color #f32(1.0 1.0 1.0 1.0))
                               (back-color #f32(0.0 0.0 0.0 1.0))
                               (back-scalex 1.2) (back-scaley 1.2)
                               (font *font-bitmap-1*))
  (%win-ortho-on width height)
  (%draw-bitmap-text-over str x y width height size align z
                          fore-color back-color back-scalex back-scaley font)
  (%win-ortho-off))

;; 文字列の幅を取得(ストロークフォント)(内部処理用)
(define (get-stroke-text-width str font)
  (string-fold (lambda (ch n) (+ (glut-stroke-width font (char->integer ch)) n)) 0 str))

;; 文字列表示(ストロークフォント)
;;   ・座標 (x,y) は左上を原点として (0,0)-(width,height) の範囲で指定する
;;   ・日本語表示不可
;;   ・文字のサイズは指定可能
(define (%draw-stroke-text str x y width height
                           :optional (size 24) (align 'left) (z 0)
                           (font *font-stroke-1*))
  (unless (equal? str "")
    (let* ((stw     (get-stroke-text-width str font))
           (y1      (- height y size))
           (scale   (/. size (+ 152.38 20)))
           (xoffset (case align
                      ((center) (- (/. stw 2))) ; 中央寄せ
                      ((right)  (- stw))        ; 右寄せ
                      (else     0)))
           (yoffset (+ 33.33 10)))
      (gl-push-matrix)
      (gl-translate x y1 z)
      (gl-scale scale scale 1)
      (gl-translate xoffset yoffset 0)
      (string-for-each (lambda (ch) (glut-stroke-character font (char->integer ch))) str)
      (gl-pop-matrix)
      )))
(define (draw-stroke-text str x y width height
                          :optional (size 24) (align 'left) (z 0)
                          (font *font-stroke-1*))
  (%win-ortho-on width height)
  (%draw-stroke-text str x y width height size align z font)
  (%win-ortho-off))

;; 文字列の上書き表示(ストロークフォント)
;;   ・背景を塗りつぶしてから draw-stroke-text を実行する
(define (%draw-stroke-text-over str x y width height
                                :optional (size 24) (align 'left) (z 0)
                                (fore-color #f32(1.0 1.0 1.0 1.0))
                                (back-color #f32(0.0 0.0 0.0 1.0))
                                (back-scalex 1.2) (back-scaley 1.2)
                                (font *font-stroke-1*))
  (unless (equal? str "")
    (if fore-color (gl-color fore-color))
    (%draw-stroke-text str x y width height size align z font)
    (if back-color (gl-color back-color))
    (let* ((stw     (get-stroke-text-width str font))
           (w1      (* stw (/. size (+ 152.38 20))))
           (xoffset (case align
                      ((left)  (- (* w1 (/. (- back-scalex 1.0) 2)))) ; 左寄せ
                      ((right)    (* w1 (/. (- back-scalex 1.0) 2)))  ; 右寄せ
                      (else    0)))
           (yoffset (- (* size (/. (- back-scaley 1.0) 2)))))
      (%draw-win-rect (+ x xoffset) (+ y yoffset)
                      (* w1 back-scalex) (* size back-scaley)
                      width height align z)
      )))
(define (draw-stroke-text-over str x y width height
                               :optional (size 24) (align 'left) (z 0)
                               (fore-color #f32(1.0 1.0 1.0 1.0))
                               (back-color #f32(0.0 0.0 0.0 1.0))
                               (back-scalex 1.2) (back-scaley 1.2)
                               (font *font-stroke-1*))
  (%win-ortho-on width height)
  (%draw-stroke-text-over str x y width height size align z
                          fore-color back-color back-scalex back-scaley font)
  (%win-ortho-off))


;; 線の表示
;;   ・線 (x1,y1)-(x2,y2) の表示を行う
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
(define (%draw-win-line x1 y1 x2 y2 width height :optional (z 0))
  (let ((y3 (- height y1))
        (y4 (- height y2)))
    (gl-push-matrix)
    (gl-translate 0 0 z)
    (gl-begin GL_LINES)
    (gl-vertex (f32vector x1 y3))
    (gl-vertex (f32vector x2 y4))
    (gl-end)
    (gl-pop-matrix)
    ))
(define (draw-win-line x1 y1 x2 y2 width height :optional (z 0))
  (%win-ortho-on width height)
  (%draw-win-line x1 y1 x2 y2 width height z)
  (%win-ortho-off))

;; 長方形の表示
;;   ・長方形 (x,y,w,h) の表示を行う (wとhは幅と高さ)
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
(define (%draw-win-rect x y w h width height :optional (align 'left) (z 0))
  (let ((x1 (case align
              ((center) (- x (/. w 2))) ; 中央寄せ
              ((right)  (- x w))        ; 右寄せ
              (else     x)))
        (y1 (- height y)))
    (gl-push-matrix)
    (gl-translate x1 y1 z)
    ;; Gauche-gl の gl-rect の不具合対策
    ;; (Gauche-gl の開発最新版では修正済み)
    ;(gl-rect 0 0 w (- h))
    (gl-rect (f32vector 0 0) (f32vector w (- h)))
    (gl-pop-matrix)
    ))
(define (draw-win-rect x y w h width height :optional (align 'left) (z 0))
  (%win-ortho-on width height)
  (%draw-win-rect x y w h width height align z)
  (%win-ortho-off))

;; 円の表示
;;   ・円 (x,y,r,a,b) -> (x'-x)*(x'-x)*(a*a)+(y'-y)*(y'-y)*(b*b)=r*r の表示を行う
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
(define (%draw-win-circle x y r width height :optional (a 1) (b 1) (align 'center) (z 0) (slices 40))
  (let ((x1  (case align
               ((left)  (+ x r)) ; 左寄せ
               ((right) (- x r)) ; 右寄せ
               (else    x)))
        (y1  (- height y))
        (scx (if (= a 0) 1 (/. 1 a)))
        (scy (if (= b 0) 1 (/. 1 b)))
        (q   (make <glu-quadric>)))
    (gl-push-matrix)
    (gl-translate x1 y1 z)
    (gl-scale scx scy 1)
    (glu-disk q 0 r slices 1)
    (gl-pop-matrix)
    ))
(define (draw-win-circle x y r width height :optional (a 1) (b 1) (align 'center) (z 0) (slices 40))
  (%win-ortho-on width height)
  (%draw-win-circle x y r width height a b align z slices)
  (%win-ortho-off))

;; 多角形の表示
;;   ・頂点の座標(f32vector x y)を複数格納したシーケンスvvecを渡して、多角形の表示を行う
;;     (面は頂点が反時計回りになる方が表になる)
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
(define (%draw-win-poly x y vvec width height :optional (z 0))
  (let1 y1 (- height y)
    (gl-push-matrix)
    (gl-translate x y1 z)
    (gl-scale 1 -1  1)
    (gl-begin GL_TRIANGLE_FAN)
    (for-each gl-vertex vvec)
    (gl-end)
    (gl-pop-matrix)
    ))
(define (draw-win-poly x y vvec width height :optional (z 0))
  (%win-ortho-on width height)
  (%draw-win-poly x y vvec width height z)
  (%win-ortho-off))


;; 長方形の輪郭の表示
;;   ・長方形 (x,y,w,h) の輪郭の表示を行う (wとhは幅と高さ)
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
(define (%draw-win-rect-line x y w h width height :optional (align 'left) (z 0))
  (let ((x1 (case align
              ((center) (- x (/. w 2))) ; 中央寄せ
              ((right)  (- x w))        ; 右寄せ
              (else     x)))
        (y1 (- height y)))
    (gl-push-matrix)
    (gl-translate x1 y1 z)
    (gl-begin GL_LINE_LOOP)
    (gl-vertex (f32vector 0 0))
    (gl-vertex (f32vector 0 (- h)))
    (gl-vertex (f32vector w (- h)))
    (gl-vertex (f32vector w 0))
    (gl-end)
    (gl-pop-matrix)
    ))
(define (draw-win-rect-line x y w h width height :optional (align 'left) (z 0))
  (%win-ortho-on width height)
  (%draw-win-rect-line x y w h width height align z)
  (%win-ortho-off))

;; 円の輪郭の表示
;;   ・円 (x,y,r,a,b) -> (x'-x)*(x'-x)*(a*a)+(y'-y)*(y'-y)*(b*b)=r*r の輪郭の表示を行う
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
(define (%draw-win-circle-line x y r width height :optional (a 1) (b 1) (align 'center) (z 0) (slices 40))
  (define slices1 (if (and (> slices 0) (<= slices 1000)) slices 40))
  (let ((x1   (case align
                ((left)  (+ x r)) ; 左寄せ
                ((right) (- x r)) ; 右寄せ
                (else    x)))
        (y1   (- height y))
        (r1   (if (= a 0) r (/. r a)))
        (r2   (if (= b 0) r (/. r b)))
        (step (/. 2pi slices1)))
    (gl-push-matrix)
    (gl-translate x1 y1 z)
    (gl-begin GL_LINE_LOOP)
    (do ((i   0 (+ i 1))
         (rad 0 (+ rad step)))
        ((>= i slices1) #f)
      (gl-vertex (f32vector (* r1 (cos rad)) (* r2 (sin rad)))))
    (gl-end)
    (gl-pop-matrix)
    ))
(define (draw-win-circle-line x y r width height :optional (a 1) (b 1) (align 'center) (z 0) (slices 40))
  (%win-ortho-on width height)
  (%draw-win-circle-line x y r width height a b align z slices)
  (%win-ortho-off))

;; 多角形の輪郭の表示
;;   ・頂点の座標(f32vector x y)を複数格納したシーケンスvvecを渡して、多角形の輪郭の表示を行う
;;     (面は頂点が反時計回りになる方が表になる)
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
(define (%draw-win-poly-line x y vvec width height :optional (z 0))
  (let1 y1 (- height y)
    (gl-push-matrix)
    (gl-translate x y1 z)
    (gl-scale 1 -1  1)
    (gl-begin GL_LINE_LOOP)
    (for-each gl-vertex vvec)
    (gl-end)
    (gl-pop-matrix)
    ))
(define (draw-win-poly-line x y vvec width height :optional (z 0))
  (%win-ortho-on width height)
  (%draw-win-poly-line x y vvec width height z)
  (%win-ortho-off))


;; テキスト画面クラス
(define-class <textscrn> ()
  ((width  :init-value 0) ; テキスト画面の幅(単位:文字)
   (height :init-value 0) ; テキスト画面の高さ(単位:文字)
   (data   :init-form (make-u32vector 0)) ; テキスト画面のデータ(u32vector)
   ))

;; テキスト画面の初期化
(define-method textscrn-init ((ts <textscrn>) (w <integer>) (h <integer>))
  (set! (~ ts 'width)  w)
  (set! (~ ts 'height) h)
  (set! (~ ts 'data)   (make-u32vector (* w h) 0))
  ;(do ((i 0 (+ i 1))) ((>= i (* w h)) #f)
  ;  (set! (~ ts 'data i) (+ (modulo i (- 128 32)) 32)))
  )

;; 文字情報レコード型(内部処理用)
;; (高速化のためクラスではなくレコード型(実体はベクタ)とする)
(define-record-type (charinfo (pseudo-rtd <vector>)) #t #t
  xscale/fchw ; X方向の倍率/フォント幅
  yscale/fchh ; Y方向の倍率/フォント幅
  xoffset     ; X方向のオフセット値
  yoffset     ; Y方向のオフセット値
  )

;; 文字情報テーブル(文字列の一括表示用)(内部処理用)
;;   ・文字ごとに倍率とオフセット値を設定して、等幅フォントのように表示可能とする
;;   ・フォントは固定
(define *char-info-num*   128)
(define *char-info-table*
  ;; Gauche-gl を初期化するまでは実行できないので、実行を遅延する
  (delay
    (rlet1 tbl (make-vector *char-info-num* #f)
      (do ((i 0 (+ i 1)))
          ((>= i *char-info-num*) #f)
        (let* ((fchw-1 (glut-stroke-width *font-stroke-1* i))
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
          (vector-set! tbl i (make-charinfo (/. (car xscale&xoffset) fchw)
                                            (/. (car yscale&yoffset) fchh)
                                            (cdr xscale&xoffset)
                                            (cdr yscale&yoffset)))
          )))))

;; 文字列の一括表示
;;   ・文字ごとに倍率とオフセット値を適用して、等幅フォントのように表示する
;;   ・座標 (x,y) は左上を原点として (0,0)-(width,height) の範囲で指定する
;;   ・日本語表示不可
;;   ・文字のサイズは、幅 chw と高さ chh の指定が必要
;;   ・フォントは固定
(define-method %textscrn-disp ((ts <textscrn>)
                               (x <real>) (y <real>)
                               (width <real>) (height <real>)
                               (chw <real>) (chh <real>)
                               :optional (align 'left) (z 0))
  (let* ((char-info-table (force *char-info-table*))
         (w1 (~ ts 'width))
         (x1 (case align
               ((center) (- x (/. (* w1 chw) 2))) ; 中央寄せ
               ((right)  (- x (* w1 chw)))        ; 右寄せ
               (else     x)))
         (x2 x1)
         (y1 (- height y chh))
         (i  0))
    (gl-push-matrix)
    (for-each
     (lambda (c)
       (if-let1 c1 (vector-ref char-info-table c #f)
         (begin
           (gl-load-identity)
           (gl-translate x1 y1 z)
           (gl-scale (* (charinfo-xscale/fchw c1) chw) (* (charinfo-yscale/fchh c1) chh) 1)
           (gl-translate (charinfo-xoffset c1) (charinfo-yoffset c1) 0)
           (glut-stroke-character *font-stroke-1* c)))
       (set! x1 (+ x1 chw))
       (inc! i)
       (when (>= i w1)
         (set! i  0)
         (set! x1 x2)
         (set! y1 (- y1 chh)))
       )
     (~ ts 'data))
    (gl-pop-matrix)
    ))
(define-method textscrn-disp ((ts <textscrn>)
                              (x <real>) (y <real>)
                              (width <real>) (height <real>)
                              (chw <real>) (chh <real>)
                              :optional (align 'left) (z 0))
  (%win-ortho-on width height)
  (%textscrn-disp ts x y width height chw chh align z)
  (%win-ortho-off))

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
          (textscrn-pset-sub ts (+ x1 x2) (+ y1 y2) c)
          )
        (if drawflag
          (let1 strdata1 (textscrn-repeat-sub ts strdata (- xold x2))
            ;; 前回の足りない部分を水平線で追加表示
            (textscrn-over-sub ts (- x1 xold) (- y1 yold) strdata1)
            (textscrn-over-sub ts (+ x1 x2 1) (- y1 yold) strdata1)
            (textscrn-over-sub ts (- x1 xold) (+ y1 yold) strdata1)
            (textscrn-over-sub ts (+ x1 x2 1) (+ y1 yold) strdata1)
            ))
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
        (textscrn-over-sub ts (- x1 xold) (+ y1 yold) strdata1)
        ))
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
          (textscrn-over-sub ts (- x1 x2) (+ y1 y2) strdata1)
          )
        (inc! y2)
        (if (<= y2 r1) (loop 0 y2))
        ))
    ))

;; 文字列の多角形表示処理
;;   ・point は ((x1 y1) (x2 y2) ...) という頂点座標のシーケンス
(define-method textscrn-poly ((ts <textscrn>) (point <sequence>) (str <string>)
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
;;   ・point は ((x1 y1) (x2 y2) ...) という頂点座標のシーケンス
(define-class <edgeinfo> () (a ydir y1 y2 x)) ; 辺情報クラス
(define-method textscrn-fpoly ((ts <textscrn>) (point <sequence>) (str <string>))
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
          )))

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
(define-method textscrn-check-str ((ts <textscrn>) (str <string>)
                                   (x1 <integer>) (y1 <integer>) (x2 <integer>) (y2 <integer>))
  (let ((x3 (min x1 x2))
        (y3 (min y1 y2))
        (x4 (max x1 x2))
        (y4 (max y1 y2)))
    (textscrn-check-str-sub ts str x3 y3 x4 y4)
    ))

;; 文字列の画面上の判定処理
;;   ・画面上の座標範囲 (x1,y1)-(x2,y2) を指定して、textscrn-check-str を実行する
;;     戻り値は #t か #f になる
;;   ・1文字の幅 chw と高さ chh も指定が必要
(define-method textscrn-disp-check-str ((ts <textscrn>) (str <string>)
                                        (x1 <real>) (y1 <real>) (x2 <real>) (y2 <real>)
                                        (chw <real>) (chh <real>)
                                        :optional (xoffset 0) (yoffset 0) (align 'left))
  (let* ((w1       (~ ts 'width))
         (xoffset1 (case align
                     ((center) (- xoffset (/. (* w1 chw) 2))) ; 中央寄せ
                     ((right)  (- xoffset (* w1 chw)))        ; 右寄せ
                     (else     xoffset)))
         (x3       (floor->exact (/. (- (min x1 x2) xoffset1) chw)))
         (y3       (floor->exact (/. (- (min y1 y2) yoffset)  chh)))
         (x4       (- (ceiling->exact (/. (- (max x1 x2) xoffset1) chw)) 1))
         (y4       (- (ceiling->exact (/. (- (max y1 y2) yoffset)  chh)) 1)))
    (textscrn-check-str-sub ts str x3 y3 x4 y4)
    ))

;; 文字列の画面上の判定処理2
;;   ・textscrn-disp-check-str と同様の処理を行うが、ヒットした座標と文字をすべて返す
;;     戻り値は ((x1 y1 ch1) (x2 y2 ch2) ...) というリストになる
;;   ・1文字の幅 chw と高さ chh も指定が必要
;;   ・sortmode には、複数の座標がヒットした場合のソート方法を、番号で指定する
;;     0を指定すると、昇順でY座標優先でソートした結果を返す
;;     1を指定すると、昇順でX座標優先でソートした結果を返す
;;     2を指定すると、降順でY座標優先でソートした結果を返す
;;     3を指定すると、降順でX座標優先でソートした結果を返す
(define-method textscrn-disp-check-str2 ((ts <textscrn>) (str <string>)
                                         (x1 <real>) (y1 <real>) (x2 <real>) (y2 <real>)
                                         (chw <real>) (chh <real>)
                                         :optional (xoffset 0) (yoffset 0) (align 'left)
                                         (sortmode 0))
  (let* ((ret      '())
         (w1       (~ ts 'width))
         (xoffset1 (case align
                     ((center) (- xoffset (/. (* w1 chw) 2))) ; 中央寄せ
                     ((right)  (- xoffset (* w1 chw)))        ; 右寄せ
                     (else     xoffset)))
         (x3       (floor->exact (/. (- (min x1 x2) xoffset1) chw)))
         (y3       (floor->exact (/. (- (min y1 y2) yoffset)  chh)))
         (x4       (- (ceiling->exact (/. (- (max x1 x2) xoffset1) chw)) 1))
         (y4       (- (ceiling->exact (/. (- (max y1 y2) yoffset)  chh)) 1)))
    (set! ret (textscrn-check-str-sub2 ts str x3 y3 x4 y4))
    (unless (null? ret)
      (case sortmode
        ((0) (set! ret (reverse! ret)))
        ((1) (set! ret (sort! ret (lambda (a b)
                                    (if (= (~ a 0) (~ b 0))
                                      (< (~ a 1) (~ b 1))
                                      (< (~ a 0) (~ b 0)))))))
        ;; 2は処理不要
        ((3) (set! ret (sort! ret (lambda (a b)
                                    (if (= (~ a 0) (~ b 0))
                                      (> (~ a 1) (~ b 1))
                                      (> (~ a 0) (~ b 0)))))))
        ))
    ret))

;; 文字列の判定処理サブ(内部処理用)
;;   ・ヒットした場合に #t を返す。そうでなければ #f を返す
(define-method textscrn-check-str-sub ((ts <textscrn>) (str <string>)
                                       (x1 <integer>) (y1 <integer>) (x2 <integer>) (y2 <integer>))
  (let ((ret     #f)
        (w1      (~ ts 'width))
        (h1      (~ ts 'height))
        (data    (~ ts 'data))
        (strdata (string->u32vector str)))
    (let loop ((x3 x1) (y3 y1))
      (if (and (>= y3 0) (< y3 h1) (>= x3 0) (< x3 w1))
        (let1 c1 (~ data (+ (* y3 w1) x3))
          (if (find (lambda (c2) (= c1 c2)) strdata)
            (set! ret #t))))
      (if (not ret)
        (cond
         ((< x3 x2) (loop (+ x3 1) y3))
         ((< y3 y2) (loop x1 (+ y3 1)))))
      )
    ;(print ret " " x1 " " y1 " " x2 " " y2)
    ret))

;; 文字列の判定処理サブ2(内部処理用)
;;   ・ヒットした座標と文字をリストにして返す
(define-method textscrn-check-str-sub2 ((ts <textscrn>) (str <string>)
                                        (x1 <integer>) (y1 <integer>) (x2 <integer>) (y2 <integer>))
  (let ((ret     '())
        (w1      (~ ts 'width))
        (h1      (~ ts 'height))
        (data    (~ ts 'data))
        (strdata (string->u32vector str)))
    (let loop ((x3 x1) (y3 y1))
      (if (and (>= y3 0) (< y3 h1) (>= x3 0) (< x3 w1))
        (let1 c1 (~ data (+ (* y3 w1) x3))
          (for-each
           (lambda (c2) (if (= c1 c2) (push! ret (list x3 y3 (integer->char c1)))))
           strdata)
          ))
      (cond
       ((< x3 x2) (loop (+ x3 1) y3))
       ((< y3 y2) (loop x1 (+ y3 1))))
      )
    ;(print ret " " x1 " " y1 " " x2 " " y2)
    ret))

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
  ((width  :init-value 0) ; 画像の幅(px)
   (height :init-value 0) ; 画像の高さ(px)
   (data   :init-form (make-u8vector 0))
   ;                      ; 画像データ(u8vector)(1ピクセルは4バイト(RGBA))
   ))

;; ビットマップファイルを読み込み、画像データを生成して返す(内部処理用)
;;   ・ファイルは、24 bit カラーで無圧縮のもののみ読み込み可能
;;   ・透明色はオプション引数に '(R G B) というリストで指定する(各色は0-255の値)
(define (load-bitmap-file file :optional (trans-color #f))
  (define (err msg . rest)
    (apply errorf (format "bitmap file load error (file=~a)\n~a" file msg) rest))
  (define (get-one-param header index)
    (rlet1 p (~ header index)
      (if (eof-object? p) (err "file size is too small"))))
  (define (read-one-data in)
    (rlet1 d (read-byte in)
      (if (eof-object? d) (err "file size is too small"))))
  (define img (make <imgdata>))
  (call-with-input-file file
    (lambda (in)
      ;; ファイルヘッダーの読み込み
      (let* ((file-header (unpack "nVvvV" :input in))
             (ftype       (get-one-param file-header 0))
             (fsize       (get-one-param file-header 1))
             (foffbits    (get-one-param file-header 4))
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
               (isize        (get-one-param info-header 0))
               (iwidth       (get-one-param info-header 1))
               (iheight      (get-one-param info-header 2))
               (ibitcount    (get-one-param info-header 4))
               (icompression (get-one-param info-header 5))
               (isizeimage   (get-one-param info-header 6)))
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
          ;; 画像データの読み込み
          ;; (上下反転しているので、ここで戻す)
          (let* ((data-size (* iwidth iheight))
                 (data      (make-u8vector (* data-size 4) 0))
                 (trans-r   (list-ref trans-color 0 -1))
                 (trans-g   (list-ref trans-color 1 -1))
                 (trans-b   (list-ref trans-color 2 -1)))
            (do ((i pos i)
                 (j 0   j)
                 (k (* iwidth 4 (- iheight 1)) k)
                 (c 0   c)
                 (x 0   x))
                ;; (fsize と isizeimage はチェックしない)
                ;((or (>= i fsize) (>= j isizeimage) (>= c data-size)) #f)
                ((>= c data-size) #f)
              ;(print i " " fsize " " j " " isizeimage " " c " " data-size)
              (cond
               ;; オフセットの位置まで読み飛ばす
               ((< i foffbits)
                (read-one-data in)
                (inc! i))
               ;; 1ライン読み込み後は、4バイト境界まで読み飛ばす
               ((>= x iwidth)
                (cond
                 ((= (modulo j 4) 0)
                  (set! x 0)
                  (set! k (- k (* iwidth 4 2))))
                 (else
                  (read-one-data in)
                  (inc! i)
                  (inc! j))))
               ;; 1ピクセル分のデータを読み込む
               (else
                (let* ((b (read-one-data in))
                       (g (read-one-data in))
                       (r (read-one-data in))
                       (a (if (and (= r trans-r) (= g trans-g) (= b trans-b)) 0 255)))
                  (set! (~ data k)       r)
                  (set! (~ data (+ k 1)) g)
                  (set! (~ data (+ k 2)) b)
                  (set! (~ data (+ k 3)) a)
                  (set! i (+ i 3))
                  (set! j (+ j 3))
                  (set! k (+ k 4))
                  (inc! c)
                  (inc! x)
                  ))))
            ;; 戻り値をセット
            ;(print iwidth " " iheight " " data)
            (set! (~ img 'width)  iwidth)
            (set! (~ img 'height) iheight)
            (set! (~ img 'data)   data)
            )))
      ))
  img)

;; テクスチャデータクラス
(define-class <texdata> ()
  ((tex    :init-value #f) ; テクスチャ(integer)
   (width  :init-value 0)  ; テクスチャの幅(px)
   (height :init-value 0)  ; テクスチャの高さ(px)
   ))

;; テクスチャデータに画像データを設定する(内部処理用)
;;   ・各種パラメータは決め打ち
(define-method set-texture-imgdata ((td <texdata>) (img <imgdata>))
  (let1 target (if *use-gl-texture-rectangle*
                 GL_TEXTURE_RECTANGLE
                 GL_TEXTURE_2D)
    (gl-bind-texture  target (~ td 'tex))
    (gl-tex-parameter target GL_TEXTURE_WRAP_S GL_REPEAT)
    (gl-tex-parameter target GL_TEXTURE_WRAP_T GL_REPEAT)
    (gl-tex-parameter target GL_TEXTURE_MAG_FILTER GL_NEAREST)
    (gl-tex-parameter target GL_TEXTURE_MIN_FILTER GL_NEAREST)
    (gl-tex-image-2d  target 0 GL_RGBA
                      (~ img 'width) (~ img 'height) 0
                      GL_RGBA GL_UNSIGNED_BYTE (~ img 'data))
    ))

;; ビットマップファイルを読み込み、テクスチャデータに設定する
;;   ・ファイルは、24 bit カラーで無圧縮のもののみ読み込み可能
;;   ・透明色はオプション引数に '(R G B) というリストで指定する(各色は0-255の値)
(define-method load-texture-bitmap-file ((td <texdata>) (file <string>)
                                         :optional (trans-color #f))
  (let ((tex (gl-gen-textures 1))
        (img (load-bitmap-file file trans-color)))
    (set! (~ td 'tex)    (~ tex 0))
    (set! (~ td 'width)  (~ img 'width))
    (set! (~ td 'height) (~ img 'height))
    (set-texture-imgdata td img)
    ))

;; テクスチャ付き長方形の表示
;;   ・テクスチャデータ td を貼り付けた長方形 (x,y,w,h) の表示を行う (wとhは幅と高さ)
;;   ・座標は、左上を原点として (0,0)-(width,height) の範囲で指定する
(define-method %draw-texture-rect ((td <texdata>)
                                   (x <real>) (y <real>) (w <real>) (h <real>)
                                   (width <real>) (height <real>)
                                   :optional (align 'left) (z 0)
                                   (xcrd 1.0) (ycrd 1.0)
                                   (width-r 1.0) (height-r 1.0)
                                   (xoffset-r 0.0) (yoffset-r 0.0))
  (let* ((target (if *use-gl-texture-rectangle*
                   GL_TEXTURE_RECTANGLE
                   GL_TEXTURE_2D))
         (xcrd1  (if *use-gl-texture-rectangle*
                   (* xcrd (~ td 'width))
                   xcrd))
         (ycrd1  (if *use-gl-texture-rectangle*
                   (* ycrd (~ td 'height))
                   ycrd))
         (x1     (case align
                   ((center) (- x (/. w 2))) ; 中央寄せ
                   ((right)  (- x w))        ; 右寄せ
                   (else     x)))
         (y1     (- height y))
         (x2     (+ x1 (* w xoffset-r)))
         (y2     (- y1 (* h yoffset-r)))
         (w1     (* w width-r))
         (h1     (* h height-r)))
    (gl-enable target)
    (gl-bind-texture target (~ td 'tex))
    (gl-push-matrix)
    (gl-translate x2 y2 z)
    (gl-begin GL_QUADS)
    ;; (面は頂点が反時計回りになる方が表になる)
    (gl-tex-coord 0.0   0.0)   (gl-vertex (f32vector 0  0      0))
    (gl-tex-coord 0.0   ycrd1) (gl-vertex (f32vector 0  (- h1) 0))
    (gl-tex-coord xcrd1 ycrd1) (gl-vertex (f32vector w1 (- h1) 0))
    (gl-tex-coord xcrd1 0.0)   (gl-vertex (f32vector w1 0      0))
    (gl-end)
    (gl-pop-matrix)
    (gl-disable target)
    ))
(define-method draw-texture-rect ((td <texdata>)
                                  (x <real>) (y <real>) (w <real>) (h <real>)
                                  (width <real>) (height <real>)
                                  :optional (align 'left) (z 0)
                                  (xcrd 1.0) (ycrd 1.0)
                                  (width-r 1.0) (height-r 1.0)
                                  (xoffset-r 0.0) (yoffset-r 0.0))
  (%win-ortho-on width height)
  (%draw-texture-rect td x y w h width height align z
                      xcrd ycrd width-r height-r xoffset-r yoffset-r)
  (%win-ortho-off))


;; 文字-テクスチャデータ割り付けクラス(テクスチャの一括表示用)
(define-class <char-texture> ()
  ((table :init-form (make-hash-table 'eqv?))))

;; テクスチャデータレコード型(内部処理用)
;; (高速化のためクラスではなくレコード型(実体はベクタ)とする)
(define-record-type (texdata (pseudo-rtd <vector>)) #t #t
  ;; 以下はテクスチャデータクラスの内容保持用(高速化のため)
  tex       ; テクスチャ(integer)
  width     ; テクスチャの幅(px)
  height    ; テクスチャの高さ(px)
  ;; 以下はテクスチャの一括表示用
  xcrd      ; テクスチャ空間上のX座標指定(テクスチャの幅が1.0に相当)
  ycrd      ; テクスチャ空間上のY座標指定(テクスチャの高さが1.0に相当)
  width-r   ; テクスチャ表示時のX方向の幅(表示サイズに対する倍率で指定)
  height-r  ; テクスチャ表示時のY方向の高さ(表示サイズに対する倍率で指定)
  xoffset-r ; テクスチャ表示時のX方向のオフセット(表示サイズに対する倍率で指定)
  yoffset-r ; テクスチャ表示時のY方向のオフセット(表示サイズに対する倍率で指定)
  )

;; 文字にテクスチャデータを割り付ける(テクスチャの一括表示用)
(define-method set-char-texture ((chtex <char-texture>) (ch <char>) (td <texdata>)
                                 :optional (xcrd 1.0) (ycrd 1.0)
                                 (width-r 1.0) (height-r 1.0)
                                 (xoffset-r 0.0) (yoffset-r 0.0))
  (hash-table-put! (~ chtex 'table)
                   (char->integer ch)
                   (make-texdata (~ td 'tex) (~ td 'width) (~ td 'height)
                                 xcrd ycrd width-r height-r xoffset-r yoffset-r)))

;; 文字に割り付けたテクスチャの一括表示
;;   ・テキスト画面クラスの各文字に対応するテクスチャを一括表示する
;;   ・座標 (x,y) は左上を原点として (0,0)-(width,height) の範囲で指定する
;;   ・1文字あたりの表示サイズは、幅 chw と高さ chh の指定が必要
(define-method %textscrn-disp-texture ((chtex <char-texture>) (ts <textscrn>)
                                       (x <real>) (y <real>)
                                       (width <real>) (height <real>)
                                       (chw <real>) (chh <real>)
                                       :optional (align 'left) (z 0))
  (let* ((target (if *use-gl-texture-rectangle*
                   GL_TEXTURE_RECTANGLE
                   GL_TEXTURE_2D))
         (w1     (~ ts 'width))
         (x1     (case align
                   ((center) (- x (/. (* w1 chw) 2))) ; 中央寄せ
                   ((right)  (- x (* w1 chw)))        ; 右寄せ
                   (else     x)))
         (x2     x1)
         (y1     (- height y))
         (i      0))
    (gl-enable target)
    (gl-push-matrix)
    (for-each
     (lambda (c)
       (if-let1 td1 (hash-table-get (~ chtex 'table) c #f)
         (let ((xcrd1 (if *use-gl-texture-rectangle*
                        (* (texdata-xcrd td1) (texdata-width  td1))
                        (texdata-xcrd td1)))
               (ycrd1 (if *use-gl-texture-rectangle*
                        (* (texdata-ycrd td1) (texdata-height td1))
                        (texdata-ycrd td1)))
               (x3    (+ x1 (* chw (texdata-xoffset-r td1))))
               (y3    (- y1 (* chh (texdata-yoffset-r td1))))
               (w2    (* chw (texdata-width-r  td1)))
               (h2    (* chh (texdata-height-r td1))))
           (gl-bind-texture target (texdata-tex td1))
           (gl-load-identity)
           (gl-translate x3 y3 z)
           (gl-begin GL_QUADS)
           ;; (面は頂点が反時計回りになる方が表になる)
           (gl-tex-coord 0.0   0.0)   (gl-vertex (f32vector 0  0      0))
           (gl-tex-coord 0.0   ycrd1) (gl-vertex (f32vector 0  (- h2) 0))
           (gl-tex-coord xcrd1 ycrd1) (gl-vertex (f32vector w2 (- h2) 0))
           (gl-tex-coord xcrd1 0.0)   (gl-vertex (f32vector w2 0      0))
           (gl-end)))
       (set! x1 (+ x1 chw))
       (inc! i)
       (when (>= i w1)
         (set! i  0)
         (set! x1 x2)
         (set! y1 (- y1 chh)))
       )
     (~ ts 'data))
    (gl-pop-matrix)
    (gl-disable target)
    ))
(define-method textscrn-disp-texture ((chtex <char-texture>) (ts <textscrn>)
                                      (x <real>) (y <real>)
                                      (width <real>) (height <real>)
                                      (chw <real>) (chh <real>)
                                      :optional (align 'left) (z 0))
  (%win-ortho-on width height)
  (%textscrn-disp-texture chtex ts x y width height chw chh align z)
  (%win-ortho-off))


;; 文字-描画手続き割り付けクラス(モデルの一括表示用)
(define-class <char-drawer> ()
  ((table :init-form (make-hash-table 'eqv?))))

;; 文字に描画手続きを割り付ける(モデルの一括表示用)
;;   ・drawer には引数 x y width height chw chh z を取る手続きを渡すこと
(define-method set-char-drawer ((chdraw <char-drawer>) (ch <char>) (drawer <procedure>))
  (hash-table-put! (~ chdraw 'table) (char->integer ch) drawer))

;; 文字に割り付けた描画手続きによるモデルの一括表示
;;   ・テキスト画面クラスの各文字に対応するモデルを一括表示する
;;   ・座標 (x,y) は左上を原点として (0,0)-(width,height) の範囲で指定する
;;   ・1文字あたりの表示サイズは、幅 chw と高さ chh の指定が必要
(define-method %textscrn-disp-drawer ((chdraw <char-drawer>) (ts <textscrn>)
                                      (x <real>) (y <real>)
                                      (width <real>) (height <real>)
                                      (chw <real>) (chh <real>)
                                      :optional (align 'left) (z 0))
  (let* ((w1 (~ ts 'width))
         (x1 (case align
               ((center) (- x (/. (* w1 chw) 2))) ; 中央寄せ
               ((right)  (- x (* w1 chw)))        ; 右寄せ
               (else     x)))
         (x2 x1)
         (y1 y)
         (i  0))
    (gl-push-matrix)
    (for-each
     (lambda (c)
       (if-let1 drawer (hash-table-get (~ chdraw 'table) c #f)
         (drawer x1 y1 width height chw chh z))
       (set! x1 (+ x1 chw))
       (inc! i)
       (when (>= i w1)
         (set! i  0)
         (set! x1 x2)
         (set! y1 (+ y1 chh)))
       )
     (~ ts 'data))
    (gl-pop-matrix)
    ))
(define-method textscrn-disp-drawer ((chdraw <char-drawer>) (ts <textscrn>)
                                     (x <real>) (y <real>)
                                     (width <real>) (height <real>)
                                     (chw <real>) (chh <real>)
                                     :optional (align 'left) (z 0))
  (%win-ortho-on width height)
  (%textscrn-disp-drawer chdraw ts x y width height chw chh align z)
  (%win-ortho-off))


