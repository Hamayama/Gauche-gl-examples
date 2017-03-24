;; -*- coding: utf-8 -*-
;;
;; maze.scm
;; 2017-3-24 v1.06
;;
;; ＜内容＞
;;   Gauche-gl を使用した、迷路を自動生成して表示するサンプルです。
;;   スタート(水色)からゴール(赤色)までのルートも探索して表示します。
;;   生成される迷路の上下左右はつながっています。
;;   スペースキーを押すと、次の迷路を表示します。
;;   ESCキーを押すと終了します。
;;
;;   参考「古くて新しい自動迷路生成アルゴリズム」
;;   http://d.hatena.ne.jp/yaneurao/20130125
;;
(add-load-path "." :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use glmintool)
(use gltextscrn)

(define *title* "maze") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *wsize*     20) ; 壁の長さ
(define *wwide*      3) ; 壁の幅
(define *mw*        30) ; 迷路の幅  (=水平方向のブロック数=水平方向の柱の数)
(define *mh*        30) ; 迷路の高さ(=垂直方向のブロック数=垂直方向の柱の数)
(define *msize*      (* *mw* *mh*)) ; 迷路のサイズ(=ブロックの総数=柱の総数)
(define *mdata*      (make-vector *msize* 0)) ; 迷路データ
;                                             ; (=1:上側に壁あり,=2:右,=4:下,=8:左,
;                                             ;  =32:探索ルート,=64:スタート,=128:ゴール)
(define *sx*         1) ; スタートのX座標
(define *sy*         1) ; スタートのY座標
(define *gx*         (quotient (+ *mw* 1) 2)) ; ゴールのX座標
(define *gy*         (quotient (+ *mh* 1) 2)) ; ゴールのY座標
(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色
(define *wallcolor*  #f32(1.0 1.0 1.0 1.0)) ; 壁の色
(define *startcolor* #f32(0.0 0.7 1.0 1.0)) ; スタートの色
(define *goalcolor*  #f32(1.0 0.0 0.0 1.0)) ; ゴールの色
(define *routecolor* #f32(1.0 1.0 0.0 1.0)) ; 探索ルートの色

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 2) (* *ht/2* 2))


;; 座標計算用
(define (pt x y) (+ (* y *mw*) x)) ; 配列番号への変換
(define (pxadd x dx) (wrap-range (+ x dx) 0 *mw*)) ; X座標加算(端を超えたら反対側に移動する)
(define (pyadd y dy) (wrap-range (+ y dy) 0 *mh*)) ; Y座標加算(端を超えたら反対側に移動する)

;; 迷路の生成
(define (make-maze)
  (define pdata (make-vector *msize* 0))  ; 柱ごとの壁データ
  ;                                       ; (=1:上方向に伸びる壁あり,=2:右,=4:下,=8:左,
  ;                                       ;  =128:壁はないが処理済み)
  (define pflag (make-vector *msize* #f)) ; 柱の処理中フラグ
  (define pnum  0)                        ; 処理中の柱の数
  (define pind  0)                        ; 処理中の柱の注目番号(ここから壁を伸ばす)
  (define px    (make-vector *msize* 0))  ; 処理中の柱のX座標
  (define py    (make-vector *msize* 0))  ; 処理中の柱のY座標
  (define rd    0)                        ; 乱数
  (define rdnum 0)                        ; 乱数の範囲決定用
  (define rdtbl (make-vector 4 0))        ; 乱数から方向への変換テーブル
  (define draw-counter 0)                 ; 表示間引き用カウンタ

  ;; 最初の柱だけは固定で処理済みにしておく
  ;; (処理済みの柱が1本もないとアルゴリズムが停止しないため)
  (set! (~ pdata (pt 0 0)) 128)
  ;; 柱の数だけループする
  (let loop ((x1 0) (y1 0))
    ;; 未処理の柱のとき
    (when (= (~ pdata (pt x1 y1)) 0)
      ;; 柱を処理中にして、そこから壁を伸ばす
      (set! (~ pflag (pt x1 y1)) #t)
      (set! pnum 1)
      (set! pind 0)
      (set! (~ px pind) x1)
      (set! (~ py pind) y1)
      (let loop2 ((x2 x1) (y2 y1))
        ;; 伸ばす壁の方向を乱数で決定する
        ;; (ただし、処理中の柱がある方向には壁を生成しない(閉ループを作らないため))
        ;; (また、4方向すべてが処理中の柱だった場合には、現在の柱を処理済みにして引き返す)
        (set! rdnum 0)
        (unless (~ pflag (pt x2 (pyadd y2 -1))) (set! (~ rdtbl rdnum) 1) (inc! rdnum))
        (unless (~ pflag (pt (pxadd x2 +1) y2)) (set! (~ rdtbl rdnum) 2) (inc! rdnum))
        (unless (~ pflag (pt x2 (pyadd y2 +1))) (set! (~ rdtbl rdnum) 4) (inc! rdnum))
        (unless (~ pflag (pt (pxadd x2 -1) y2)) (set! (~ rdtbl rdnum) 8) (inc! rdnum))
        (cond
         ((= rdnum 0)
          (set! (~ pdata (pt x2 y2)) (logior (~ pdata (pt x2 y2)) 128))
          (dec! pind)
          (loop2 (~ px pind) (~ py pind)))
         (else
          (set! rd (randint 0 (- rdnum 1)))
          ;; 壁の生成
          (set! (~ pdata (pt x2 y2)) (logior (~ pdata (pt x2 y2)) (~ rdtbl rd)))

          ;; 迷路の生成過程を表示する
          (inc! draw-counter)
          (when (>= draw-counter 5)
            (set! draw-counter 0)
            ;; 柱ごとの壁データを、迷路データに変換する
            (convert-maze pdata)
            ;; 画面表示
            (disp))

          ;; 生成した壁の先の柱に移動し、その柱が未処理であれば、
          ;; その柱を処理中にして、そこからさらに壁を伸ばす
          (let ((x3 x2) (y3 y2))
            (case (~ rdtbl rd)
              ((1) (set! y3 (pyadd y3 -1)))
              ((2) (set! x3 (pxadd x3 +1)))
              ((4) (set! y3 (pyadd y3 +1)))
              ((8) (set! x3 (pxadd x3 -1))))
            (when (= (~ pdata (pt x3 y3)) 0)
              (set! (~ pflag (pt x3 y3)) #t)
              (inc! pnum)
              (set! pind (- pnum 1))
              (set! (~ px pind) x3)
              (set! (~ py pind) y3)
              (loop2 x3 y3))
            ))))
      ;; 柱の処理中フラグをリセット
      (do ((i 0 (+ i 1)))
          ((>= i pnum) #f)
        (set! (~ pflag (pt (~ px i) (~ py i))) #f))
      )
    (cond
     ((< x1 (- *mw* 1)) (loop (+ x1 1) y1))
     ((< y1 (- *mh* 1)) (loop 0 (+ y1 1))))
    )
  ;; 柱ごとの壁データを、迷路データに変換する
  (convert-maze pdata)
  )

;; 柱ごとの壁データを、迷路データに変換する
(define (convert-maze pdata)
  ;; ブロックの数だけループする
  (let loop ((x1 0) (y1 0))
    ;; 柱ごとの壁データを、迷路データに変換
    (let1 d1 0
      (if (logtest (~ pdata (pt (pxadd x1 -1) (pyadd y1 -1))) 2) (set! d1 (logior d1 1)))
      (if (logtest (~ pdata (pt (pxadd x1 -1) (pyadd y1 -1))) 4) (set! d1 (logior d1 8)))
      (if (logtest (~ pdata (pt x1            (pyadd y1 -1))) 8) (set! d1 (logior d1 1)))
      (if (logtest (~ pdata (pt x1            (pyadd y1 -1))) 4) (set! d1 (logior d1 2)))
      (if (logtest (~ pdata (pt (pxadd x1 -1) y1           )) 1) (set! d1 (logior d1 8)))
      (if (logtest (~ pdata (pt (pxadd x1 -1) y1           )) 2) (set! d1 (logior d1 4)))
      (if (logtest (~ pdata (pt x1            y1           )) 1) (set! d1 (logior d1 2)))
      (if (logtest (~ pdata (pt x1            y1           )) 8) (set! d1 (logior d1 4)))
      (set! (~ *mdata* (pt x1 y1)) d1)
      )
    (cond
     ((< x1 (- *mw* 1)) (loop (+ x1 1) y1))
     ((< y1 (- *mh* 1)) (loop 0 (+ y1 1))))
    )
  )

;; 迷路の表示
(define (disp-maze)
  (define ws (win-h *win* *wsize*)) ; ウィンドウ上の壁の長さ(px)
  (define ww (win-h *win* *wwide*)) ; ウィンドウ上の壁の幅(px)
  (define ox (/. (- *width*  (* ws *mw*)) 2)) ; ウィンドウ上の迷路の左上点のX座標(px)
  (define oy (/. (- *height* (* ws *mh*)) 2)) ; ウィンドウ上の迷路の左上点のY座標(px)

  ;; ブロックの数だけループする
  (let loop ((x1 0) (y1 0))
    (let ((bx1 (+ ox (* x1 ws))) ; ウィンドウ上の1ブロックの左上点のX座標(px)
          (by1 (+ oy (* y1 ws))) ; ウィンドウ上の1ブロックの左上点のY座標(px)
          (d1  (~ *mdata* (pt x1 y1)))) ; 1ブロックの迷路データ
      ;; 壁の表示
      (gl-color *wallcolor*)
      (if (logtest d1 1) (draw-win-rect bx1 (+ by1 (/. ww -2))    ws ww *width* *height*))
      (if (logtest d1 2) (draw-win-rect (+ bx1 ws (/. ww -2)) by1 ww ws *width* *height*))
      (if (logtest d1 4) (draw-win-rect bx1 (+ by1 ws (/. ww -2)) ws ww *width* *height*))
      (if (logtest d1 8) (draw-win-rect (+ bx1 (/. ww -2)) by1    ww ws *width* *height*))
      ;; 1ブロックの背景の表示
      (gl-color (cond ((logtest d1 128) *goalcolor*)
                      ((logtest d1  64) *startcolor*)
                      ((logtest d1  32) *routecolor*)
                      (else             *backcolor*)))
      (draw-win-rect bx1 by1 ws ws *width* *height* 'left -0.99999)
      )
    (cond
     ((< x1 (- *mw* 1)) (loop (+ x1 1) y1))
     ((< y1 (- *mh* 1)) (loop 0 (+ y1 1))))
    )
  )

;; 迷路の探索(幅優先探索)
(define (search-maze)
  (define goal   #f) ; ゴールフラグ
  (define sflag  (make-vector *msize* #f)) ; 迷路の探索済みフラグ
  (define snum   0)  ; 探索点の数
  (define sind  -1)  ; 探索点の注目番号
  (define spx    (make-vector *msize* 0)) ; 探索点のX座標
  (define spy    (make-vector *msize* 0)) ; 探索点のY座標
  (define spno   (make-vector *msize* 0)) ; 探索点の親番号
  (define (make-one-spoint x y) ; 探索点1個の生成
    (set! (~ spx  snum) x)
    (set! (~ spy  snum) y)
    (set! (~ spno snum) sind)
    (set! (~ sflag (pt x y)) #t) ; 探索ずみにする
    (inc! snum))

  ;; 最初の探索点を生成
  (make-one-spoint *sx* *sy*)
  (inc! sind)
  ;; 探索点を1個取り出す
  (let loop ((x1 (~ spx sind)) (y1 (~ spy sind)))
    (cond
     ;; ゴールのときは抜ける
     ((and (= x1 *gx*) (= y1 *gy*))
      (set! goal #t))
     ;; ゴールではなかったとき
     (else
      ;; 上下左右を探索して探索点を追加する
      (let1 d1 (~ *mdata* (pt x1 y1))
        (unless (or (logtest d1 1) (~ sflag (pt x1 (pyadd y1 -1))))
          (make-one-spoint x1 (pyadd y1 -1)))
        (unless (or (logtest d1 2) (~ sflag (pt (pxadd x1 +1) y1)))
          (make-one-spoint (pxadd x1 +1) y1))
        (unless (or (logtest d1 4) (~ sflag (pt x1 (pyadd y1 +1))))
          (make-one-spoint x1 (pyadd y1 +1)))
        (unless (or (logtest d1 8) (~ sflag (pt (pxadd x1 -1) y1)))
          (make-one-spoint (pxadd x1 -1) y1))
        )
      ;; 次の探索点へ移動
      (inc! sind)
      (if (< sind snum)
        (loop (~ spx sind) (~ spy sind)))
      )))
  ;; ゴールまでの探索ルートを迷路データに反映
  ;; (ゴールから逆にたどっていく)
  (when goal
    (let loop ((x1 (~ spx sind)) (y1 (~ spy sind)))
      (set! (~ *mdata* (pt x1 y1)) (logior (~ *mdata* (pt x1 y1)) 32))
      (set! sind (~ spno sind))
      (if (>= sind 0)
        (loop (~ spx sind) (~ spy sind)))
      ))
  ;; スタートとゴールを迷路データに反映
  (set! (~ *mdata* (pt *sx* *sy*)) (logior (~ *mdata* (pt *sx* *sy*)) 64))
  (set! (~ *mdata* (pt *gx* *gy*)) (logior (~ *mdata* (pt *gx* *gy*)) 128))
  )


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 迷路の生成
  (make-maze)
  ;; 画面表示
  (disp)
  ;; 迷路の探索
  (search-maze)
  ;; 画面表示
  (disp)
  )

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  ;; 迷路の表示
  (disp-maze)
  (gl-flush))

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  w)
  (set! *height* (min w h))
  (win-update-size *win* *width* *height*)
  ;; 縦横比を変えずにリサイズ
  (gl-viewport 0 (quotient (- h *height*) 2) *width* *height*))

;; キー入力
(define (keyboard key x y)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit 0))
   ;; スペースキーで次を表示
   ((= key (char->integer #\space))
    ;; 迷路の生成
    (make-maze)
    ;; 画面表示
    (disp)
    ;; 迷路の探索
    (search-maze)
    ;; 画面表示
    (disp))
   ;; [g]キーでGC実行(デバッグ用)
   ((or (= key (char->integer #\g)) (= key (char->integer #\G)))
    (gc) (print (gc-stat)))
   ))

;; メイン処理
(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size *width* *height*)
  (glut-init-window-position 100 100)
  (glut-create-window *title*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  ;; コールバック内エラー対策
  (guard (ex (else (report-error ex) (exit 0)))
    (glut-main-loop))
  0)

