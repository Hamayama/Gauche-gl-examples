;; -*- coding: utf-8 -*-
;;
;; glmintool.scm
;; 2016-3-31 v1.01
;;
;; ＜内容＞
;;   Gauche-gl のプログラムで使うための簡単なツール類です。
;;
(define-module glmintool
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use math.mt-random)
  (export
    randint recthit? get-one-arg
    <keywaitinfo> keywait keywait-timer keywait-clear keywait-waiting? keywait-finished?
    <timewaitinfo> timewait timewait-timer timewait-clear timewait-waiting? timewait-finished?
    <waitmsecinfo> waitmsec-calc
    ))
(select-module glmintool)

;; 乱数
;;   (randint n1 n2)でn1以上n2以下の整数の乱数を取得する(n1,n2は整数であること)
(define randint
  (let1 m (make <mersenne-twister> :seed (sys-time))
    (lambda (n1 n2)
      (if (> n1 n2) (let1 t n1 (set! n1 n2) (set! n2 t)))
      (+ (mt-random-integer m (+ (- n2 n1) 1)) n1))))

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

;; コマンドライン引数1個の取得
(define (get-one-arg args i)
  (and (integer? i)
       (>= i 0)
       (list? args)
       (> (length args) i)
       (~ args i)))


;; キー入力待ちクラス
;;   ・指定したキーが入力されるまで待つ
;;   ・glut-timer-func と組み合わせて使用する
(define-class <keywaitinfo> ()
  ((state    :init-value 0)   ; 待ち状態(=0:初期状態,=1:キー入力待ち開始,=2:キー入力待ち中,=3:キー入力完了)
   (waitkey  :init-value '()) ; 待ち受けキー(文字のリストで指定)
   (keystate :init-keyword :keystate :init-form (make-hash-table 'eqv?)) ; キー入力状態(ハッシュテーブル)
   ))
(define-method keywait ((k <keywaitinfo>) (wk <pair>) (finished-func <procedure>))
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
;(define *kwinfo* (make <keywaitinfo> :keystate *keystate*)) ; インスタンス生成例


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
(define-class <waitmsecinfo> ()
  ((waitdata :init-value 0) ; ウェイト時間調整用(msec)
   (waittime :init-keyword :waittime :init-value 0) ; ウェイト時間指定値(msec)
   ))
(define-method waitmsec-calc ((w <waitmsecinfo>))
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
;(define *wtinfo* (make <waitmsecinfo> :waittime *wait*)) ; インスタンス生成例


