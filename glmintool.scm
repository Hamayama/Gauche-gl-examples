;; -*- coding: utf-8 -*-
;;
;; glmintool.scm
;; 2016-12-7 v1.21
;;
;; ＜内容＞
;;   Gauche-gl を使うプログラムのための簡単なツール類です。
;;
(define-module glmintool
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use math.mt-random)
  (export
    randint round-n truncate-n recthit? make-fpath make-vector-of-class
    <wininfo> win-init win-update-size win-x win-y win-w win-h win-w-r win-h-r
    <keystateinfo> key-on key-off spkey-on spkey-off key-on? spkey-on? mdkey-on?
    <keywaitinfo>  keywait keywait-timer keywait-clear keywait-waiting? keywait-finished?
    <timewaitinfo> timewait timewait-timer timewait-clear timewait-waiting? timewait-finished?
    <waitcalcinfo> waitcalc waitcalc-set-wait
    <xrand> xrand-init xrand-randint xrand-test
    ))
(select-module glmintool)

;; 乱数
;;   (randint n1 n2)でn1以上n2以下の整数の乱数を取得する(n1,n2は整数であること)
(define randint
  (let1 m (make <mersenne-twister> :seed (sys-time))
    (lambda (n1 n2)
      (if (> n1 n2) (let1 t n1 (set! n1 n2) (set! n2 t)))
      (+ (mt-random-integer m (+ (- n2 n1) 1)) n1))))

;; 数値xを偶数丸めして小数第n位までの数値にする
(define (round-n x n)
  (let1 p (expt 10 n)
    (/. (round (* x p)) p)))

;; 数値xを0方向に切り捨てて小数第n位までの数値にする
(define (truncate-n x n)
  (let1 p (expt 10 n)
    (/. (truncate (* x p)) p)))

;; 長方形の衝突チェック
;;   左上座表が (x1,y1) で 幅 w1 高さ h1 の長方形と、
;;   左上座標が (x2,y2) で 幅 w2 高さ h2 の長方形とが、
;;   衝突していれば #t を返す。
;;   そうでなければ #f を返す。
(define (recthit? x1 y1 w1 h1 x2 y2 w2 h2)
  (and (< x1 (+ x2 w2))
       (< x2 (+ x1 w1))
       (< y1 (+ y2 h2))
       (< y2 (+ y1 h1))))

;; ファイルのパス名生成
;;   ディレクトリのパス名とファイル名から、ファイルのパス名を生成する
(define (make-fpath . paths)
  (fold (lambda (p1 path)
          (cond ((equal? path "") p1)
                ((#/[\/\\]$/ path) (string-append path p1))
                (else (string-append path "/" p1))))
        ""
        paths))

;; クラスcのインスタンスをn個生成してベクタにして返す
(define (make-vector-of-class n c)
  (rlet1 v (make-vector n)
    (do ((i 0 (+ i 1)))
        ((>= i n) #f)
      (set! (~ v i) (make c)))))


;; ウィンドウ情報クラス
;;   ・ウィンドウ上の画面幅 width と画面高さ height の情報を保持して、
;;     ウィンドウ上の座標を計算可能とする
;;     (OpenGLの座標系とはY軸の方向が逆になる)
;;   ・本クラスは、ウィンドウのサイズ変更に追従する必要がある。
;;     このため、glut-reshape-func のコールバックで win-update-size を呼び出して、
;;     ウィンドウのサイズ変更に追従するようにすること
(define-class <wininfo> ()
  ((width   :init-value 0) ; ウィンドウ上の画面幅(px)
   (height  :init-value 0) ; ウィンドウ上の画面高さ(px)
   (wd      :init-value 0) ; OpenGL上の画面幅
   (ht      :init-value 0) ; OpenGL上の画面高さ
   (xoffset :init-value 0) ; OpenGL上の画面左上のX座標
   (yoffset :init-value 0) ; OpenGL上の画面左上のY座標
   ))
;; 初期化
(define-method win-init ((wn <wininfo>)
                         (width <real>) (height <real>) (wd <real>) (ht <real>)
                         :optional (xoffset #f) (yoffset #f))
  (set! (~ wn 'width)  width)
  (set! (~ wn 'height) height)
  (set! (~ wn 'wd)     wd)
  (set! (~ wn 'ht)     ht)
  (if xoffset
    (set! (~ wn 'xoffset) xoffset)
    (set! (~ wn 'xoffset) (- (/. wd 2))))
  (if yoffset
    (set! (~ wn 'yoffset) yoffset)
    (set! (~ wn 'yoffset) (/. ht 2)))
  )
;; ウィンドウのサイズ変更に追従
(define-method win-update-size ((wn <wininfo>) (width <real>) (height <real>))
  (set! (~ wn 'width)  width)
  (set! (~ wn 'height) height))
;; ウィンドウ上のX座標を計算
(define-method win-x ((wn <wininfo>) (x <real>))
  (/. (* (- x (~ wn 'xoffset)) (~ wn 'width)) (~ wn 'wd)))
;; ウィンドウ上のY座標を計算
(define-method win-y ((wn <wininfo>) (y <real>))
  (/. (* (- (~ wn 'yoffset) y) (~ wn 'height)) (~ wn 'ht)))
;; ウィンドウ上の幅を計算
(define-method win-w ((wn <wininfo>) (w <real>))
  (/. (* w (~ wn 'width)) (~ wn 'wd)))
;; ウィンドウ上の高さを計算
(define-method win-h ((wn <wininfo>) (h <real>))
  (/. (* h (~ wn 'height)) (~ wn 'ht)))
;; ウィンドウ上の幅を、比を指定して計算
(define-method win-w-r ((wn <wininfo>) (n1 <real>) :optional (n2 #f))
  (if n2 (/. (* (~ wn 'width) n1) n2)
      (* (~ wn 'width) (exact n1))))
;; ウィンドウ上の高さを、比を指定して計算
(define-method win-h-r ((wn <wininfo>) (n1 <real>) :optional (n2 #f))
  (if n2 (/. (* (~ wn 'height) n1) n2)
      (* (~ wn 'height) (exact n1))))


;; キー入力状態管理クラス
;;   ・キー入力のON/OFF状態を管理する
;;   ・glut-keyboard-func, glut-keyboard-up-func, glut-special-func,
;;     glut-special-up-func と組み合わせて使用する
(define-class <keystateinfo> ()
  ((keystate   :init-form (make-hash-table 'eqv?)) ; キー入力状態(ハッシュテーブル)
   (spkeystate :init-form (make-hash-table 'eqv?)) ; 特殊キー入力状態(ハッシュテーブル)
   (mdkeystate :init-form (make-hash-table 'eqv?)) ; Shift,Ctrl,Altキー入力状態(ハッシュテーブル)
   ))
(define (%check-mdkey k)
  ;; Shift,Ctrl,Altキーの入力チェック
  ;; (glut-get-modifiers は、入力デバイス関連のコールバック内でしか使用できない。
  ;;  また、環境によっては、他のキーと同時押ししないと状態を取得できない)
  (let1 mdkey (glut-get-modifiers)
    (hash-table-put! (~ k 'mdkeystate) GLUT_ACTIVE_SHIFT
                     (not (= (logand mdkey GLUT_ACTIVE_SHIFT) 0)))
    (hash-table-put! (~ k 'mdkeystate) GLUT_ACTIVE_CTRL
                     (not (= (logand mdkey GLUT_ACTIVE_CTRL)  0)))
    (hash-table-put! (~ k 'mdkeystate) GLUT_ACTIVE_ALT
                     (not (= (logand mdkey GLUT_ACTIVE_ALT)   0)))
    ))
(define-method key-on ((k <keystateinfo>) (key <integer>))
  (%check-mdkey k)
  (hash-table-put! (~ k 'keystate) key #t))
(define-method key-off ((k <keystateinfo>) (key <integer>))
  (%check-mdkey k)
  (hash-table-put! (~ k 'keystate) key #f)
  ;; アルファベットの大文字と小文字は両方OFFにする(Shiftキー同時押しの対策)
  (cond
   ((<= #x41 key #x5A) (hash-table-put! (~ k 'keystate) (+ key #x20) #f))
   ((<= #x61 key #x7A) (hash-table-put! (~ k 'keystate) (- key #x20) #f))))
(define-method spkey-on ((k <keystateinfo>) (key <integer>))
  (%check-mdkey k)
  (hash-table-put! (~ k 'spkeystate) key #t))
(define-method spkey-off ((k <keystateinfo>) (key <integer>))
  (%check-mdkey k)
  (hash-table-put! (~ k 'spkeystate) key #f))
(define (%check-keystate keystate key/keylist)
  (cond
   ((list? key/keylist)
    (any (lambda (key) (hash-table-get keystate (x->integer key) #f)) key/keylist))
   (else
    (hash-table-get keystate (x->integer key/keylist) #f))))
(define-method key-on? ((k <keystateinfo>) key/keylist)
  (%check-keystate (~ k 'keystate) key/keylist))
(define-method spkey-on? ((k <keystateinfo>) key/keylist)
  (%check-keystate (~ k 'spkeystate) key/keylist))
(define-method mdkey-on? ((k <keystateinfo>) key/keylist)
  (%check-keystate (~ k 'mdkeystate) key/keylist))
;(define *ksinfo* (make <keystateinfo>)) ; インスタンス生成例


;; キー入力待ちクラス
;;   ・指定したキーが入力されるまで待つ
;;   ・glut-timer-func と組み合わせて使用する
(define-class <keywaitinfo> ()
  ((state    :init-value 0)   ; 待ち状態(=0:初期状態,=1:キー入力待ち開始,=2:キー入力待ち中,=3:キー入力完了)
   (waitkey  :init-value '()) ; 待ち受けキー(文字のリストで指定)
   (keystate :init-keyword :keystate :init-form (make-hash-table 'eqv?)) ; キー入力状態(ハッシュテーブル)
   ))
(define-method keywait ((k <keywaitinfo>) (wk <list>) (finished-func <procedure>))
  (case (~ k 'state)
    ((0) (set! (~ k 'waitkey) wk)
         (set! (~ k 'state) 1))
    ((3) (finished-func))))
(define-method keywait-timer ((k <keywaitinfo>))
  (case (~ k 'state)
    ((1) (for-each (lambda (c) (hash-table-put! (~ k 'keystate) (char->integer c) #f)) (~ k 'waitkey))
         (set! (~ k 'state) 2))
    ((2) (when (any (lambda (c) (hash-table-get (~ k 'keystate) (char->integer c) #f)) (~ k 'waitkey))
           (set! (~ k 'state) 3)))))
(define-method keywait-clear ((k <keywaitinfo>))
  (set! (~ k 'state) 0))
(define-method keywait-waiting? ((k <keywaitinfo>))
  (or (= (~ k 'state) 1) (= (~ k 'state) 2)))
(define-method keywait-finished? ((k <keywaitinfo>))
  (= (~ k 'state) 3))
;(define *kwinfo* (make <keywaitinfo> :keystate (~ *ksinfo* 'keystate))) ; インスタンス生成例


;; 時間待ちクラス
;;   ・指定した時間が経過するまで待つ
;;   ・glut-timer-func と組み合わせて使用する
(define-class <timewaitinfo> ()
  ((state        :init-value 0) ; 待ち状態(=0:初期状態,=1:時間待ち開始,=2:時間待ち中,=3:時間待ち完了)
   (waittime     :init-value 0) ; 待ち時間(msec)
   (waitinterval :init-keyword :waitinterval :init-value 0) ; 待ち時間インターバル(msec)
   (waitcount    :init-value 0) ; 待ち時間カウント用(msec)
   ))
(define-method timewait ((t <timewaitinfo>) (wt <integer>) (finished-func <procedure>))
  (case (~ t 'state)
    ((0) (set! (~ t 'waittime) wt)
         (set! (~ t 'state) 1))
    ((3) (finished-func))))
(define-method timewait-timer ((t <timewaitinfo>))
  (case (~ t 'state)
    ((1) (set! (~ t 'waitcount) 0)
         (set! (~ t 'state) 2))
    ((2) (set! (~ t 'waitcount) (+ (~ t 'waitcount) (~ t 'waitinterval)))
         (when (>= (~ t 'waitcount) (~ t 'waittime))
           (set! (~ t 'state) 3)))))
(define-method timewait-clear ((t <timewaitinfo>))
  (set! (~ t 'state) 0))
(define-method timewait-waiting? ((t <timewaitinfo>))
  (or (= (~ t 'state) 1) (= (~ t 'state) 2)))
(define-method timewait-finished? ((t <timewaitinfo>))
  (= (~ t 'state) 3))
;(define *twinfo* (make <timewaitinfo> :waitinterval *wait*)) ; インスタンス生成例


;; ウェイト時間調整クラス
;;   ・処理時間を測定して、ウェイト時間が一定になるように調整する
;;   ・glut-timer-func と組み合わせて使用する
(define-class <waitcalcinfo> ()
  ((waitdata :init-value 0) ; ウェイト時間調整用(msec)
   (waittime :init-keyword :waittime :init-value 0) ; ウェイト時間指定値(msec)
   ))
(define-method waitcalc ((w <waitcalcinfo>))
  (let* ((tnow      (current-time))
         (tnowmsec  (+ (* (~ tnow 'second) 1000) (quotient (~ tnow 'nanosecond) 1000000)))
         (tdiffmsec (- tnowmsec (~ w 'waitdata)))
         (wt        (~ w 'waittime))
         (waitmsec  wt))
    (cond
     ((and (>= tdiffmsec (- wt)) (< tdiffmsec wt))
      (set! waitmsec (- wt tdiffmsec)))
     ((and (>= tdiffmsec wt) (< tdiffmsec (* wt 50)))
      (set! waitmsec 1)))
    (set! (~ w 'waitdata) (+ tnowmsec waitmsec))
    ;(print tdiffmsec " " waitmsec)
    waitmsec))
(define-method waitcalc-set-wait ((w <waitcalcinfo>) (wt <integer>))
  (set! (~ w 'waittime) wt))
;(define *wcinfo* (make <waitcalcinfo> :waittime *wait*)) ; インスタンス生成例


;; 疑似乱数生成クラス
;;   ・シード値が同じならば同じ乱数列になる
(define-class <xrand> ()
  ((seed  :init-value 1) ; 乱数のシード値(0はNG)
   (value :init-value 1) ; 乱数の値
   ))
;; 疑似乱数の初期化
(define-method xrand-init ((xr <xrand>) (seed <integer>))
  (set! (~ xr 'seed)  seed)
  (set! (~ xr 'value) seed)
  ;; 最初の10個は捨てる
  (do ((i 0 (+ i 1))) ((>= i 10) #f) (%xorshift32 xr)))
;; 疑似乱数の範囲を指定して取得
(define-method xrand-randint ((xr <xrand>) (minx <integer>) (maxx <integer>))
  (if (> minx maxx) (let1 t minx (set! minx maxx) (set! maxx t)))
  ;; 別件との互換性のため、符号を変換
  ;(+ (modulo (%xorshift32 xr) (+ (- maxx minx) 1)) minx)
  (let* ((v  (%xorshift32 xr))
         (v2 (if (= (logand v #x80000000) 0)
               v
               (- (+ (logand (lognot v) #xFFFFFFFF) 1)))))
    (+ (modulo (abs v2) (+ (- maxx minx) 1)) minx)
    ))
;; 疑似乱数の内部計算処理(xorshift 32bit)
(define-method %xorshift32 ((xr <xrand>))
  (rlet1 v (~ xr 'value)
    (set! v (logand v #xFFFFFFFF))
    (set! v (logxor v (logand (ash v  13) #xFFFFFFFF)))
    (set! v (logxor v (logand (ash v -17) #x7FFF))) ; 符号なしの右シフト
    (set! v (logxor v (logand (ash v  15) #xFFFFFFFF)))
    ;(print v)
    (set! (~ xr 'value) v)))
;; 疑似乱数のテスト(デバッグ用)
(define (xrand-test)
  (let ((xr     (make <xrand>))
        (result (make-vector 10 0))
        (count  (make-vector 10 0)))
    (xrand-init xr 1)
    (do ((i 0 (+ i 1))) ((>= i 10) #f)
      (set! (~ result i) (xrand-randint xr 0 9)))
    (print "xrand-randint(0,9)=" result " -> "
           (if (equal? result #(7 2 4 5 0 5 0 9 5 6)) "OK" "NG"))
    (do ((i 0 (+ i 1))) ((>= i 10000) #f)
      (inc! (~ count (xrand-randint xr 0 9))))
    (print "count[0-9]=" count " -> "
           (if (equal? count #(998 981 997 1010 1006 1052 1008 997 995 956)) "OK" "NG"))
    ))


