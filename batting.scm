;; -*- coding: utf-8 -*-
;;
;; batting.scm
;;
;; ＜内容＞
;;   Gauche-gl を使用した、バッティングゲームです。
;;   矢印キーでカーソルを左右に移動し、
;;   スペースキーでボールを打ちます。
;;   引き付けるほど飛びます(最大199m)。ただし見逃しは0mです。
;;   ESCキーを押すと終了します。
;;
;; ＜履歴＞
;;   2015-7-27  v1.00  初版
;;   2015-7-27  v1.01  コメント修正のみ
;;   2015-7-28  v1.02  空の座標修正
;;                     いくつかの処理をクラス化
;;   2015-7-28  v1.03  待ち状態処理修正
;;   2015-7-28  v1.04  見逃し時のパラメータ表示修正等
;;   2015-7-28  v1.05  打撃判定条件を一部修正
;;   2015-7-29  v1.06  コメント修正のみ
;;
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use math.mt-random)
(use srfi-13) ; string-fold,string-for-each用

(define *wait*      15) ; ウェイト(msec)
(define *width*    480) ; 画面幅(px)
(define *height*   480) ; 画面高さ(px)
(define *vangle*   100) ; 視野角(度)
(define *tanvan*     (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)
(define *keystate*   (make-hash-table 'eqv?)) ; キー入力状態(ハッシュテーブル)
(define *spkeystate* (make-hash-table 'eqv?)) ; 特殊キー入力状態(ハッシュテーブル)

(define *wd/2*     120) ; 画面幅/2
(define *ht/2*     120) ; 画面高さ/2
(define *zstart* -1000) ; ボールの初期位置のZ座標
(define *zend*    -100) ; ボールの最終位置のZ座標
(define *r*         12) ; ボールの半径
(define *x*          0) ; ボールのX座標
(define *y*       (- *r*)) ; ボールのY座標
(define *z*          *zstart*) ; ボールのZ座標
(define *vx*         0) ; ボールのX方向速度
(define *vy*         0) ; ボールのY方向速度
(define *vz*         0) ; ボールのZ方向速度
(define *vx2*        0) ; ボールのX方向速度2
(define *vy2*        0) ; ボールのY方向速度2
(define *cx*         0) ; カーソルのX座標
(define *cy*     (/. *ht/2* -2)) ; カーソルのY座標
(define *cz*         *zend*) ; カーソルのZ座標
(define *gdy*     -100) ; 地面のY座標
(define *hit*        0) ; 当たり判定(=0:なし,=1:空振り,=2:当たり,=3:大当たり)
(define *foul*      #f) ; ファールフラグ
(define *sc*         0) ; スコア
(define *hs*         0) ; ハイスコア
(define *scene*      0) ; シーン情報(=0:スタート画面,=1:打撃前,=2:打撃後,=3:結果画面)

;; 乱数
;;   (randint n1 n2)でn1以上n2以下の整数の乱数を取得する(n1,n2は整数でn1<n2であること)
(define randint
  (let1 m (make <mersenne-twister> :seed (sys-time))
    (lambda (n1 n2) (+ (mt-random-integer m (+ (- n2 n1) 1)) n1))))

;; キー入力待ちクラス
(define-class <keywaitinfo> ()
  ((state    :init-value 0)   ; 待ち状態(=0:初期状態,=1:キー入力待ち開始,=2:キー入力待ち中,=3:キー入力完了)
   (waitkey  :init-value '()) ; 待ち受けキー(文字のリストで指定)
   (keystate :init-keyword :keystate :init-value (make-hash-table 'eqv?)) ; キー入力状態(ハッシュテーブル)
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
(define *kwinfo* (make <keywaitinfo> :keystate *keystate*)) ; インスタンス生成

;; 時間待ちクラス
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
(define *twinfo* (make <timewaitinfo> :waitinterval *wait*)) ; インスタンス生成

;; ウェイト時間調整クラス
;;   (処理時間を測定して、ウェイト時間が一定になるように調整する)
(define-class <waitmsecinfo> ()
  ((waitdata :init-value #f) ; ウェイト時間調整用(msec)
   (waittime :init-keyword :waittime :init-value 0) ; ウェイト時間指定値(msec)
   ))
(define-method waitmsec-calc ((w <waitmsecinfo>))
  (let* ((tnow      (current-time))
         (tnowmsec  (+ (* (~ tnow 'second) 1000) (quotient (~ tnow 'nanosecond) 1000000)))
         (tdiffmsec 0)
         (wt        (~ w 'waittime))
         (waitmsec  wt))
    (when (~ w 'waitdata)
      (set! tdiffmsec (- tnowmsec (~ w 'waitdata)))
      (cond
       ((and (>= tdiffmsec (- wt)) (< tdiffmsec wt))
        (set! waitmsec (- wt tdiffmsec)))
       ((and (>= tdiffmsec wt) (< tdiffmsec (* wt 50)))
        (set! waitmsec 1))
       ))
    (set! (~ w 'waitdata) (+ tnowmsec waitmsec))
    ;(print tdiffmsec " " waitmsec)
    waitmsec))
(define *wtinfo* (make <waitmsecinfo> :waittime *wait*)) ; インスタンス生成

;; 文字列表示(ストロークフォント)
;;   ・座標 (x,y) は左下が原点で (0,0)-(*width*,*height*) の範囲で指定する必要がある
;;     (図形表示とは座標系が異なるので注意)
;;   ・日本語表示不可
;;   ・文字のサイズは指定可能
(define (draw-stroke-text str x y :optional (size 24) (hcenter #f) (font GLUT_STROKE_ROMAN))
  (gl-disable GL_LIGHTING)
  (gl-matrix-mode GL_PROJECTION)
  (gl-push-matrix)
  (gl-load-identity)
  ;; (文字幅がpx単位でしかとれないので、座標系も一時的にpx単位にする)
  (gl-ortho 0 *width* 0 *height* -1.0 1.0)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-push-matrix)
  (gl-load-identity)
  (let1 scale (/. size 152.38)
    (if hcenter
      (let1 sw (string-fold (lambda (c n) (+ (glut-stroke-width font (char->integer c)) n)) 0 str)
        (gl-translate (- x (/. (* sw scale) 2)) y 0))
      (gl-translate x y 0))
    (gl-scale scale scale scale)
    (string-for-each (lambda (c) (glut-stroke-character font (char->integer c))) str))
  (gl-pop-matrix)
  (gl-matrix-mode GL_PROJECTION)
  (gl-pop-matrix)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-enable GL_LIGHTING))


;; 直方体(上面に原点あり)
(define (box x y z)
  (define f32 f32vector)
  (define -x  (- x))
  (define -2y (* -2 y))
  (define -z  (- z))
  (let ((vertex (vector (f32  x  0  z) (f32 x -2y  z) (f32 -x -2y  z) (f32 -x  0  z)
                        (f32  x  0 -z) (f32 x -2y -z) (f32 -x -2y -z) (f32 -x  0 -z)))
        (face   #(#(0 1 2 3) #(0 4 5 1) #(1 5 6 2) #(2 6 7 3) #(3 7 4 0) #(4 7 6 5)))
        (normal #(#f32( 0  0  1) #f32( 1  0  0) #f32( 0 -1  0)
                  #f32(-1  0  0) #f32( 0  1  0) #f32( 0  0 -1))))
    (gl-begin GL_QUADS)
    (do ((i 0 (+ i 1)))
        ((>= i 6) #f)
      (gl-normal (~ normal i))
      (do ((j 0 (+ j 1)))
          ((>= j 4) #f)
        (gl-vertex (~ vertex (~ face i j)))))
    (gl-end)))

;; カーソル(中心に原点あり)
(define (cursor r)
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 0.0 0.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 10.0)
  (gl-push-matrix)
  (gl-translate 0 r 0)
  (box 1 r 1)
  (gl-pop-matrix)
  (gl-push-matrix)
  (gl-translate 0 1 0)
  (box r 1 1)
  (gl-pop-matrix)
  )

;; 空(上面に原点あり)
(define (sky)
  (gl-disable GL_LIGHTING)
  (gl-color 0.0 0.0 1.0 1.0)
  (box 40000 40000 1)
  (gl-enable GL_LIGHTING)
  )

;; 地面(上面に原点あり)
(define (ground)
  (gl-material GL_FRONT GL_DIFFUSE #f32(0.0 0.75 0.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 10.0)
  (box 40000 40000 10000)
  )


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
  )

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 文字表示
  (let ((str1 "") (str2 "") (str3 "") (str4 "") (str5 "") (y2 58))
    ;; シーン情報で場合分け
    (case *scene*
      ((0) ; スタート画面
       (set! str2 "HIT [S] KEY")
       (set! y2 56))
      ((1) ; 打撃前
       (set! str2 "MOVE : [<-] [->]  HIT : [SPACE]")
       (set! y2 4))
      ((2) ; 打撃後
       )
      ((3) ; 結果画面
       (set! str1 (format #f "SCORE : ~Dm" *sc*))
       (if (= *hit* 3) (set! str1 (string-append str1 " (MAXIMUM!!)")))
       (if *foul*      (set! str1 (string-append str1 " (FOUL!!)")))
       (if (timewait-finished? *twinfo*) (set! str2 "HIT [D] KEY")))
      )
    (set! str3 (format #f "HI-SCORE : ~Dm" *hs*))
    (set! str4 (format #f "(X=~D Y=~D Z=~D)"
                       (truncate->exact *x*) (truncate->exact *y*) (truncate->exact *z*)))
    (set! str5 (format #f "(VX=~D VY=~D VZ=~D)"
                       (if (= *scene* 0) 0 (/. (truncate->exact (* 100 *vx*)) 100))
                       (/. (truncate->exact (* 100 *vy*)) 100)
                       (/. (truncate->exact (* 100 *vz*)) 100)))
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text str1 (/. *width* 2) (/. (* *height* 70) 100) (/. *height* 15) #t)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str2 (+ (/. *width* 2) (/. *height* 100))
                      (/. (* *height* y2) 100) (/. *height* 20) #t)
    (gl-color 1.0 0.0 1.0 1.0)
    (draw-stroke-text str3 (/. *height* 100) (/. (* *height* 95) 100) (/. *height* 20) #f)
    (gl-color 0.0 1.0 0.0 1.0)
    (draw-stroke-text str4 (/. *height* 100) (/. (* *height* 90) 100) (/. *height* 25) #f)
    (draw-stroke-text str5 (/. *height* 100) (/. (* *height* 85) 100) (/. *height* 25) #f)
    )
  ;; ボールを表示
  (gl-push-matrix)
  (gl-translate *x* *y* *z*)
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 1.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 2.0)
  (glut-solid-sphere *r* 20 20)
  (gl-pop-matrix)
  ;; カーソルを表示
  (gl-push-matrix)
  (gl-translate *cx* *cy* *cz*)
  (cursor *r*)
  (gl-pop-matrix)
  ;; 空を表示
  (gl-push-matrix)
  (gl-translate 0 40000 -10000)
  (sky)
  (gl-pop-matrix)
  ;; 地面を表示
  (gl-push-matrix)
  (gl-translate 0 *gdy* 0)
  (ground)
  (gl-pop-matrix)
  ;(gl-flush)
  (glut-swap-buffers)
  )

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  w)
  (set! *height* h)
  ;; 縦横比を変えずにリサイズ
  (cond
   ((< *width* *height*)
    (gl-viewport 0 (quotient (- *height* *width*) 2) *width* *width*)
    (set! *height* *width*))
   (else
    (gl-viewport 0 0 *width* *height*))
   )
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  ;; 透視射影する範囲を設定
  (glu-perspective *vangle* (/. *width* *height*) 1 10000)
  )

;; キー入力ON
(define (keyboard key x y)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit 0))
   ;; [g]キーでGC実行(デバッグ用)
   ((or (= key (char->integer #\g)) (= key (char->integer #\G)))
    (gc) (print (gc-stat)))
   )
  (hash-table-put! *keystate* key #t))

;; キー入力OFF
(define (keyboardup key x y)
  (hash-table-put! *keystate* key #f))

;; 特殊キー入力ON
(define (specialkey key x y)
  (hash-table-put! *spkeystate* key #t))

;; 特殊キー入力OFF
(define (specialkeyup key x y)
  (hash-table-put! *spkeystate* key #f))

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
       (set! *x*      0)
       (set! *y*     (- *r*))
       (set! *z*      *zstart*)
       (set! *vz*     5)
       (set! *vx*    (* (/. (randint (- (quotient *wd/2* 2)) (quotient *wd/2* 2))
                            (- *zend* *zstart*))
                        *vz*))
       (set! *vy*    (* (/. (- *r* (/. *ht/2* 2))
                            (- *zend* *zstart*))
                        *vz*))
       (set! *vx2*    0)
       (set! *vy2*    0)
       (set! *cx*     0)
       (set! *cy*    (/. *ht/2* -2))
       (set! *cz*     *zend*)
       (set! *hit*    0)
       (set! *foul*  #f)
       (set! *sc*     0)
       ;; キー入力待ち
       (keywait *kwinfo* '(#\s #\S)
                (lambda ()
                  (set! *scene* 1)
                  (keywait-clear *kwinfo*)))
       )
      ((1) ; 打撃前
       (set! *x* (+ *x* *vx*))
       (set! *y* (+ *y* *vy*))
       (set! *z* (+ *z* *vz*))
       ;; カーソル移動
       (if (hash-table-get *spkeystate* GLUT_KEY_LEFT  #f)
         (set! *cx* (clamp (+ *cx* -1) (- *wd/2*) *wd/2*)))
       (if (hash-table-get *spkeystate* GLUT_KEY_RIGHT #f)
         (set! *cx* (clamp (+ *cx*  1) (- *wd/2*) *wd/2*)))
       ;; 打撃判定
       (when (and (> *z* (- *zend* 100))
                  (hash-table-get *keystate* (char->integer #\space) #f)
                  (= *hit* 0))
         (set! *hit* 1)
         (when (<= (abs (- *x* *cx*)) (* *r* 2))
           (set! *hit* (if (= *z* *zend*) 3 2))
           (set! *vx2* (- *x* *cx*))
           (set! *vy2* (* (/. (* *ht/2* 2 3.7) 100)
                          (+ 1 (/. (- *z* *zend*) 30))))
           (if (< *vy2* 1) (set! *vy2* 1))
           ))
       (when (>= *z* *zend*)
         (set! *scene* 2)
         (when (>= *hit* 2)
           (set! *vx* *vx2*)
           (set! *vy* *vy2*)
           (set! *vz* -5))
         )
       )
      ((2) ; 打撃後
       (cond
        ((< *hit* 2)
         (set! *scene* 3))
        (else
         (set! *x*  (+ *x*  *vx*))
         (set! *y*  (+ *y*  *vy*))
         (set! *z*  (+ *z*  *vz*))
         (set! *vy* (- *vy* (/. (* *ht/2* 2 3.7) 20000)))
         (when (and (<= *vy* 0) (<= *y* (- *r*)))
           (set! *scene* 3))
         (when (>= (abs *x*) (abs (* *z* *tanvan*)))
           (set! *scene* 3)
           (set! *hit*   2)
           (set! *foul*  #t))
         )
        )
       )
      ((3) ; 結果画面
       (set! *sc* (truncate->exact (/. (- *zend* *z*) 10)))
       (if (= *hit* 3)    (set! *sc* 199))
       (if *foul*         (set! *sc* 0))
       (if (< *sc*  0)    (set! *sc* 0))
       (if (> *sc*  *hs*) (set! *hs* *sc*))
       ;; 時間待ち
       (timewait *twinfo* 1500
                 (lambda ()
                   ;; キー入力待ち
                   (keywait *kwinfo* '(#\d #\D)
                            (lambda ()
                              (set! *scene* 0)
                              (timewait-clear *twinfo*)
                              (keywait-clear  *kwinfo*)))))
       )
      )
    )
   )
  (glut-post-redisplay)
  ;; ウェイト時間調整
  (glut-timer-func (waitmsec-calc *wtinfo*) timer 0)
  )

;; メイン処理
(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size *width* *height*)
  (glut-init-window-position 100 100)
  (glut-create-window "batting")
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-keyboard-up-func keyboardup)
  (glut-special-func specialkey)
  (glut-special-up-func specialkeyup)
  (glut-timer-func *wait* timer 0)
  ;; コールバック内エラー対策
  (guard (ex (else (report-error ex) (exit 0)))
    (glut-main-loop))
  0)

