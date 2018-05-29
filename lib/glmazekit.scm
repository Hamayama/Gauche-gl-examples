;; -*- coding: utf-8 -*-
;;
;; glmazekit.scm
;; 2018-5-29 v1.05
;;
;; ＜内容＞
;;   迷路の生成と探索を行うためのモジュールです。
;;
;;   迷路の生成については、以下のページを参考にしています。
;;   「古くて新しい自動迷路生成アルゴリズム」
;;   http://d.hatena.ne.jp/yaneurao/20130125
;;
(define-module glmazekit
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use math.const)
  (use glmintool)
  (export
    <maze> maze-init maze-generate
    maze-set-start maze-set-goal
    maze-search
    ))
(select-module glmazekit)

;; 迷路クラス
(define-class <maze> ()
  ((width      :init-value 0)  ; 迷路の幅  (=水平方向のブロック数)
   (height     :init-value 0)  ; 迷路の高さ(=垂直方向のブロック数)
   (data       :init-value #f) ; 迷路データ(ベクタ)
   ;                           ;   (=1:上側に壁あり,=2:右,=4:下,=8:左,
   ;                           ;    =16:移動済み,=32:探索ルート,
   ;                           ;    =64:スタート,=128:ゴール)
   (start-x    :init-value 0)  ; スタートのX座標
   (start-y    :init-value 0)  ; スタートのY座標
   (goal-x     :init-value 0)  ; ゴールのX座標
   (goal-y     :init-value 0)  ; ゴールのY座標
   (goal-state :init-value 0)  ; ゴール状態(=0:未ゴール,=1:ゴール)
   (maze-state :init-value 0)  ; 迷路状態(=0:初期状態,=1:生成済み)
   ))

;; 迷路の初期化
(define-method maze-init ((mz <maze>) (w <integer>) (h <integer>))
  (set! (~ mz 'width)      w)
  (set! (~ mz 'height)     h)
  (set! (~ mz 'data)       (make-vector (* w h) 0))
  (set! (~ mz 'start-x)    0)
  (set! (~ mz 'start-y)    0)
  (set! (~ mz 'goal-x)     0)
  (set! (~ mz 'goal-y)     0)
  (set! (~ mz 'goal-state) 0)
  (set! (~ mz 'maze-state) 0))

;; 迷路の生成
(define-method maze-generate ((mz <maze>) :optional (disp-proc #f))
  (define mw    (~ mz 'width))  ; 迷路の幅
  (define mh    (~ mz 'height)) ; 迷路の高さ
  (define msize (* mw mh))      ; 迷路のサイズ
  (define (pt x y) (+ (* y mw) x)) ; 配列番号への変換
  (define (pxadd x dx) (wrap-range (+ x dx) 0 mw)) ; X座標加算(端を超えたら反対側に移動する)
  (define (pyadd y dy) (wrap-range (+ y dy) 0 mh)) ; Y座標加算(端を超えたら反対側に移動する)

  (define pdata (make-vector msize 0))  ; 柱ごとの壁データ
  ;                                     ;   (=1:上方向に伸びる壁あり,=2:右,=4:下,=8:左,
  ;                                     ;    =128:処理済み)
  (define pflag (make-vector msize #f)) ; 柱の処理中フラグ
  (define pnum  0)                      ; 処理中の柱の数
  (define pind  0)                      ; 処理中の柱の注目番号(ここから壁を伸ばす)
  (define px    (make-vector msize 0))  ; 処理中の柱のX座標
  (define py    (make-vector msize 0))  ; 処理中の柱のY座標
  (define rd    0)                      ; 乱数
  (define rdnum 0)                      ; 乱数の範囲決定用
  (define rdtbl (make-vector 4 0))      ; 乱数から方向への変換テーブル
  (define draw-counter 0)               ; 表示間引き用カウンタ

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

          ;; 迷路の生成過程を表示するとき
          (when disp-proc
            (inc! draw-counter)
            (when (>= draw-counter 5)
              (set! draw-counter 0)
              ;; 柱ごとの壁データを、迷路データに変換する
              (convert-maze mz pdata)
              ;; 画面表示
              (disp-proc)))

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
     ((< x1 (- mw 1)) (loop (+ x1 1) y1))
     ((< y1 (- mh 1)) (loop 0 (+ y1 1))))
    )
  ;; 柱ごとの壁データを、迷路データに変換する
  (convert-maze mz pdata)
  ;; 迷路状態を更新
  (set! (~ mz 'maze-state) 1))

;; 柱ごとの壁データを、迷路データに変換する(内部処理用)
(define (convert-maze mz pdata)
  (define mw    (~ mz 'width))  ; 迷路の幅
  (define mh    (~ mz 'height)) ; 迷路の高さ
  (define mdata (~ mz 'data))   ; 迷路データ
  (define (pt x y) (+ (* y mw) x)) ; 配列番号への変換
  (define (pxadd x dx) (wrap-range (+ x dx) 0 mw)) ; X座標加算(端を超えたら反対側に移動する)
  (define (pyadd y dy) (wrap-range (+ y dy) 0 mh)) ; Y座標加算(端を超えたら反対側に移動する)

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
      (set! (~ mdata (pt x1 y1)) d1)
      )
    (cond
     ((< x1 (- mw 1)) (loop (+ x1 1) y1))
     ((< y1 (- mh 1)) (loop 0 (+ y1 1))))
    ))

;; 迷路のスタート地点の設定
(define-method maze-set-start ((mz <maze>) (sx <integer>) (sy <integer>))
  (define mw    (~ mz 'width))  ; 迷路の幅
  (define mdata (~ mz 'data))   ; 迷路データ
  (define (pt x y) (+ (* y mw) x)) ; 配列番号への変換
  (set! (~ mz 'start-x) sx)
  (set! (~ mz 'start-y) sy)
  (set! (~ mdata (pt sx sy)) (logior (~ mdata (pt sx sy)) 64)))

;; 迷路のゴール地点の設定
(define-method maze-set-goal ((mz <maze>) (gx <integer>) (gy <integer>))
  (define mw    (~ mz 'width))  ; 迷路の幅
  (define mdata (~ mz 'data))   ; 迷路データ
  (define (pt x y) (+ (* y mw) x)) ; 配列番号への変換
  (set! (~ mz 'goal-x) gx)
  (set! (~ mz 'goal-y) gy)
  (set! (~ mdata (pt gx gy)) (logior (~ mdata (pt gx gy)) 128)))

;; 迷路の探索(幅優先探索)
(define-method maze-search ((mz <maze>))
  (define mw    (~ mz 'width))   ; 迷路の幅
  (define mh    (~ mz 'height))  ; 迷路の高さ
  (define msize (* mw mh))       ; 迷路のサイズ
  (define mdata (~ mz 'data))    ; 迷路データ
  (define sx    (~ mz 'start-x)) ; スタートのX座標
  (define sy    (~ mz 'start-y)) ; スタートのY座標
  (define gx    (~ mz 'goal-x))  ; ゴールのX座標
  (define gy    (~ mz 'goal-y))  ; ゴールのY座標
  (define (pt x y) (+ (* y mw) x)) ; 配列番号への変換
  (define (pxadd x dx) (wrap-range (+ x dx) 0 mw)) ; X座標加算(端を超えたら反対側に移動する)
  (define (pyadd y dy) (wrap-range (+ y dy) 0 mh)) ; Y座標加算(端を超えたら反対側に移動する)

  (define goal  #f)                     ; ゴールフラグ
  (define sflag (make-vector msize #f)) ; 迷路の探索済みフラグ
  (define snum  0)                      ; 探索点の数
  (define sind -1)                      ; 探索点の注目番号
  (define spx   (make-vector msize 0))  ; 探索点のX座標
  (define spy   (make-vector msize 0))  ; 探索点のY座標
  (define spno  (make-vector msize 0))  ; 探索点の親番号
  (define (make-one-spoint x y)  ; 探索点1個の生成
    (set! (~ spx  snum) x)
    (set! (~ spy  snum) y)
    (set! (~ spno snum) sind)
    (set! (~ sflag (pt x y)) #t) ; 探索済みにする
    (inc! snum))

  ;; 最初の探索点を生成
  (make-one-spoint sx sy)
  (inc! sind)
  ;; 探索点を1個取り出す
  (let loop ((x1 (~ spx sind)) (y1 (~ spy sind)))
    (cond
     ;; ゴールのときは抜ける
     ((and (= x1 gx) (= y1 gy))
      (set! goal #t))
     ;; ゴールではなかったとき
     (else
      ;; 上下左右を探索して探索点を追加する
      (let1 d1 (~ mdata (pt x1 y1))
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
      (set! (~ mdata (pt x1 y1)) (logior (~ mdata (pt x1 y1)) 32))
      (set! sind (~ spno sind))
      (if (>= sind 0)
        (loop (~ spx sind) (~ spy sind)))
      ))
  ;; ゴール状態を更新
  (set! (~ mz 'goal-state) (if goal 1 0)))

