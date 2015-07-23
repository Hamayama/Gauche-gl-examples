;; -*- coding: utf-8 -*-
;;
;; 格闘ゲーム (Gauche-glを使用)
;; 2015-7-21  v1.00  初版
;; 2015-7-21  v1.01  キャラクターの向きを修正
;; 2015-7-21  v1.02  ウィンドウのタイトル修正等
;; 2015-7-21  v1.03  デモの起動直後の処理修正等
;; 2015-7-21  v1.04  ウェイト時間調整の処理修正等
;; 2015-7-23  v1.05  コールバック内でエラーが発生すると異常終了する件の対策
;;                   難易度調整等
;;
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use math.mt-random)
(use srfi-13) ; string-fold,string-for-each用

(define *wait*      30) ; ウェイト(msec)
(define *width*    480) ; 画面幅(px)
(define *height*   480) ; 画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*   (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)
(define *keystate*   (make-hash-table 'eqv?)) ; キー入力状態(ハッシュテーブル)
(define *spkeystate* (make-hash-table 'eqv?)) ; 特殊キー入力状態(ハッシュテーブル)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *chw/2*     50) ; キャラクタの幅/2
(define *chh*      100) ; キャラクタの高さ
(define *gdy*     -300) ; 地面のY座標
(define *maxx*    (- *wd/2* *chw/2*))  ; X座標最大値
(define *minx* (- (- *wd/2* *chw/2*))) ; X座標最小値
(define *miny*    (+ *gdy*  *chh*))    ; Y座標最小値
(define *k*          0) ; 自分の行動決定用
(define *x*       (+ *minx* 40)) ; 自分のX座標
(define *y*          *miny*) ; 自分のY座標
(define *vx*         0) ; 自分のX方向速度
(define *vy*         0) ; 自分のY方向速度
(define *act*        0) ; 自分のアクション
(define *dir*        1) ; 自分の向き
(define *ft*         0) ; 自分の硬直時間カウント用
(define *kcount*     0) ; 自分の連続キック防止用
(define *rk*         0) ; 敵の行動決定用
(define *rx*      (- *maxx* 40)) ; 敵のX座標
(define *ry*         *miny*) ; 敵のY座標
(define *rvx*        0) ; 敵のX方向速度
(define *rvy*        0) ; 敵のY方向速度
(define *ract*       0) ; 敵のアクション
(define *rdir*      -1) ; 敵の向き
(define *rft*        0) ; 敵の硬直時間カウント用
(define *endstate*   0) ; 終了状態(=1:自分の勝ち,=2:敵の勝ち,=3:自分の勝ちで終了,=4:敵の勝ちで終了)
(define *waku*      10) ; 当たり判定調整用
(define *fixtime*    8) ; 硬直時間
(define *stephigh*  30) ; ステップ高さ
(define *demoflg*   #f) ; デモフラグ
(define *demotime*   0) ; デモ時間調整用(msec)
(define *starttime*  0) ; スタート後経過時間(msec)
(define *scene*      0) ; シーン情報(=0:スタート画面,=1:戦闘中,=2:戦闘終了)
(define *waitflg*   #f) ; 待ち状態フラグ
(define *waitstepA*  0) ; 待ち状態A(=0:無効,=1:キー入力待ち開始,=2:キー入力待ち中,=3:キー入力完了)
(define *waitstepB*  0) ; 待ち状態B(=0:無効,=1:時間待ち開始,=2:時間待ち中,=3:時間待ち完了)
(define *waittime*   0) ; 待ち時間(msec)
(define *waitcount*  0) ; 待ち時間カウント用(msec)
(define *waitkey*  '()) ; 待ち受けキー(文字のリストで指定)
(define *waitdata*  #f) ; ウェイト時間調整用(msec)

;; 乱数
;;   (randint n1 n2)でn1以上n2以下の整数の乱数を取得する(n1,n2は整数でn1<n2であること)
(define randint
  (let1 m (make <mersenne-twister> :seed (sys-time))
    (lambda (n1 n2) (+ (mt-random-integer m (+ (- n2 n1) 1)) n1))))

;; 文字列表示(ビットマップフォント)
;;   ・座標 (x,y) は左下が原点で (0,0)-(*width*,*height*) の範囲で指定する必要がある
;;     (図形表示とは座標系が異なるので注意)
;;   ・日本語表示不可
;;   ・文字のサイズは固定
(define (draw-bitmap-text str x y :optional (hcenter #f) (font GLUT_BITMAP_HELVETICA_18))
  (gl-disable GL_LIGHTING)
  (gl-matrix-mode GL_PROJECTION)
  (gl-push-matrix)
  (gl-load-identity)
  ;; (文字幅がpx単位でしかとれないので、座標系も一時的にpx単位にする)
  (gl-ortho 0 *width* 0 *height* -1.0 1.0)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-push-matrix)
  (if hcenter
    (let1 sw (string-fold (lambda (c n) (+ (glut-bitmap-width font (char->integer c)) n)) 0 str)
      (gl-raster-pos (- x (/. sw 2)) y))
    (gl-raster-pos x y))
  (string-for-each (lambda (c) (glut-bitmap-character font (char->integer c))) str)
  (gl-pop-matrix)
  (gl-matrix-mode GL_PROJECTION)
  (gl-pop-matrix)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-enable GL_LIGHTING)
  )

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
  (let1 scale (/. size 152.38)
    (if hcenter
      (let1 sw (string-fold (lambda (c n) (+ (glut-stroke-width font (char->integer c)) n)) 0 str)
        (gl-translate (- x (/. (* sw scale) 2)) y 0))
      (gl-translate x y 0))
    (gl-scale scale scale scale)
    (string-for-each (lambda (c) (glut-stroke-character font (char->integer c))) str)
    )
  (gl-pop-matrix)
  (gl-matrix-mode GL_PROJECTION)
  (gl-pop-matrix)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-enable GL_LIGHTING)
  )

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
        (gl-vertex (~ vertex (~ face i j)))
        ))
    (gl-end)
    ))

;; 円柱(上面に原点あり)
(define (cylinder r h s)
  (define step (/. 2pi s))
  (define -2h  (* -2 h))
  ;; 上面
  (gl-begin GL_TRIANGLE_FAN)
  (gl-normal #f32(0 1 0))
  (do ((i 0 (+ i 1))
       (angle 0 (+ angle step)))
      ((>= i s) #f)
    (gl-vertex (* r (cos angle)) 0 (* r (sin angle))))
  (gl-end)
  ;; 底面
  (gl-begin GL_TRIANGLE_FAN)
  (gl-normal #f32(0 -1 0))
  (do ((i 0 (+ i 1))
       (angle 0 (- angle step)))
      ((>= i s) #f)
    (gl-vertex (* r (cos angle)) -2h (* r (sin angle))))
  (gl-end)
  ;; 側面
  (gl-begin GL_QUAD_STRIP)
  (do ((i 0 (+ i 1))
       (angle 0 (+ angle step)))
      ((> i s) #f)
    (let ((x (cos angle))
          (z (sin angle)))
      (gl-normal (f32vector x 0 z))
      (gl-vertex (* r x) 0   (* r z))
      (gl-vertex (* r x) -2h (* r z))
      ))
  (gl-end)
  )

;; 人形モデル(頭に原点あり。大きさ100くらいに固定)
;;   type  タイプ(=0:自分,=1:敵)
;;   pose  ポーズ(=0:通常,=1:前進,=2:後退,=3:やられ,=4:パンチ,=5:キック)
(define (model type pose)
  (gl-push-matrix)
  ;; 色設定
  (case type
    ((0) (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 1.0 1.0)))
    ((1) (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 0.0 1.0)))
    )
  ;; 頭
  (gl-push-matrix)
  (case type
    ((0) (gl-translate 0 -10 0)
         (glut-solid-sphere 10 20 20))
    ((1) (box 8 8 8))
    )
  (gl-pop-matrix)
  ;; 胴体
  (gl-translate 0 -20 0)
  (box 10 20 10)
  ;; 右手
  (gl-push-matrix)
  (gl-translate -15 0 0)
  (case pose
    ((0) (gl-rotate -15  0 0 1))
    ((1) (gl-rotate -25  1 0 1))
    ((2) (gl-rotate -25 -1 0 1))
    ((3) (gl-rotate -25  1 0 1))
    ((4) (gl-translate 0 -5 10)
         (gl-rotate -135 1 0 0.1))
    ((5) (gl-rotate -25 -1 0 1))
    )
  (cylinder 4 20 20)
  (gl-pop-matrix)
  ;; 左手
  (gl-push-matrix)
  (gl-translate  15 0 0)
  (case pose
    ((0) (gl-rotate  15  0 0 1))
    ((1) (gl-rotate  25  1 0 1))
    ((2) (gl-rotate  25 -1 0 1))
    ((3) (gl-rotate  25 -1 0 1))
    ((4) (gl-rotate  25  1 0 1))
    ((5) (gl-rotate  25 -1 0 1))
    )
  (cylinder 4 20 20)
  (gl-pop-matrix)
  ;; 右足
  (gl-push-matrix)
  (gl-translate -5 -40 0)
  (case pose
    ((2) (gl-rotate -20  1 0 0))
    ((3) (gl-rotate -35  1 0 0.1))
    ((5) (gl-translate 0 0 10)
         (gl-rotate -100 1 0 0))
    )
  (cylinder 4 20 20)
  (gl-pop-matrix)
  ;; 左足
  (gl-push-matrix)
  (gl-translate  5 -40 0)
  (case pose
    ((1) (gl-rotate  30  1 0 0))
    ((3) (gl-rotate  35 -1 0 0.1))
    ((4) (gl-rotate  40  1 0 0))
    ((5) (gl-rotate  40  1 0 0))
    )
  (cylinder 4 20 20)
  (gl-pop-matrix)
  (gl-pop-matrix)
  )

;; 地面(上面に原点あり)
(define (ground)
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 0.0 0.0 1.0))
  (box *wd/2* *ht/2* 150)
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
  (let ((str1 "") (str2 "") (y2 34))
    ;; シーン情報で場合分け
    (case *scene*
      ((0) ; スタート画面
       (cond
        ;; デモのとき
        (*demoflg*)
        ;; デモでないとき
        (else
         (set! str1 "Ready?")
         (set! str2 "HIT [S] KEY")
         (set! y2 33))
        ))
      ((1) ; 戦闘中
       (cond
        ;; デモのとき
        (*demoflg*
         (set! str1 "== Demo ==")
         (set! str2 "HIT [D] KEY"))
        ;; デモでないとき
        (else
         (when (<= *starttime* 3000)
           (set! str1 "Fight!!")
           (set! str2 "USE [<-] [->] [Z] [X] KEY")))
        ))
      ((2) ; 戦闘終了
       (set! str1 (if (= *endstate* 3) "You win!!" "You lose!!"))
       (if (= *waitstepB* 3) (set! str2 "HIT [D] KEY")))
      )
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text str1 (/. *width* 2) (/. (* *height* 40) 50) (/. *height* 10) #t)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str2 (/. *width* 2) (/. (* *height* y2) 50) (/. *height* 20) #t))
  ;; 自分を表示
  (gl-push-matrix)
  (gl-translate *x* *y* 0)
  (gl-rotate (* *dir* 90) 0 1 0)
  (model 0 (case *act*
             ((2 12) 4)
             ((3 13) 5)
             ((14)   3)
             (else (if (< (* *dir* *vx*) 0) 2 (if (> (* *dir* *vx*) 0) 1 0)))))
  (gl-pop-matrix)
  ;; 敵を表示
  (gl-push-matrix)
  (gl-translate *rx* *ry* 0)
  (gl-rotate (* *rdir* 90) 0 1 0)
  (model 1 (case *ract*
             ((2 12) 4)
             ((3 13) 5)
             ((14)   3)
             (else (if (< (* *rdir* *rvx*) 0) 2 (if (> (* *rdir* *rvx*) 0) 1 0)))))
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
  (glu-perspective *vangle* (/. *width* *height*) 1 2000)
  ;; 視点の位置と方向を設定
  (glu-look-at 0 0 (/. *wd/2* *tanvan*) 0 0 0 0 1 0)
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
   (*waitflg*
    ;; キー入力待ち状態のとき
    (case *waitstepA*
      ((1) (for-each (lambda (c) (hash-table-put! *keystate* (char->integer c) #f)) *waitkey*)
           (set! *waitstepA* 2))
      ((2) (when (any (lambda (c) (hash-table-get *keystate* (char->integer c) #f)) *waitkey*)
             (set! *waitstepA* 3)
             (set! *waitflg* #f))))
    ;; 時間待ち状態のとき
    (case *waitstepB*
      ((1) (set! *waitcount* 0)
           (set! *waitstepB* 2))
      ((2) (set! *waitcount* (+ *waitcount* *wait*))
           (when (>= *waitcount* *waittime*)
             (set! *waitstepB* 3)
             (set! *waitflg* #f))))
    )
   ;; 待ち状態でないとき
   (else
    ;; シーン情報で場合分け
    (case *scene*
      ((0) ; スタート画面
       ;; 変数初期化
       (set! *x*      (+ *minx* 40))
       (set! *y*         *miny*)
       (set! *vx*        0)
       (set! *vy*        0)
       (set! *act*       0)
       (set! *dir*       1)
       (set! *ft*        0)
       (set! *kcount*    0)
       (set! *rx*     (- *maxx* 40))
       (set! *ry*        *miny*)
       (set! *rvx*       0)
       (set! *rvy*       0)
       (set! *ract*      0)
       (set! *rdir*     -1)
       (set! *rft*       0)
       (set! *endstate*  0)
       (set! *demotime*  0)
       (set! *starttime* 0)
       (cond
        ;; デモのとき
        (*demoflg*
         (set! *scene* 1))
        ;; デモでないとき
        (else
         ;; キー入力待ち
         (case *waitstepA*
           ((0) (set! *waitstepA* 1)
                (set! *waitflg*   #t)
                (set! *waitkey*   '(#\s #\S)))
           ((3) (set! *scene*     1)))
         ;; 時間待ち(タイムアップでデモへ移行)
         (case *waitstepB*
           ((0) (set! *waitstepB* 1)
                (set! *waitflg*   #t)
                (set! *waittime*  5000))
           ((3) (set! *scene*     1)
                (set! *demoflg*   #t)))
         (when (= *scene* 1)
           (set! *waitstepA* 0)
           (set! *waitstepB* 0))
         )
        )
       )
      ((1) ; 戦闘中
       (if (< *starttime* 60000) (set! *starttime* (+ *starttime* *wait*)))
       ;; 自分の移動
       (my-action)
       ;; 敵の移動
       (enemy-action)
       ;; 衝突判定
       (if (recthit? (+ *x*  *waku*)
                     (+ *y*  *waku*)
                     (- (* *chw/2* 2) (* *waku* 2))
                     (- *chh*         (* *waku* 2))
                     (+ *rx* *waku*)
                     (+ *ry* *waku*)
                     (- (* *chw/2* 2) (* *waku* 2))
                     (- *chh*         (* *waku* 2)))
         (cond
          ;; 相打ち
          ((and (or (= *act*  2) (= *act*  3))
                (or (= *ract* 2) (= *ract* 3)))
           (set! *act*  (+ *act*  10))
           (set! *ract* (+ *ract* 10))
           (set! *vx*  (- *vx* ))
           (set! *rvx* (- *rvx*))
           (if (< *vy*  15) (set! *vy*  15))
           (if (< *rvy* 15) (set! *rvy* 15))
           )
          ;; 自分の勝ち
          ((and (or  (= *act*  2) (= *act*  3))
                (not (= *ract* 14)))
           (set! *endstate* 1)
           (set! *ract* 14)
           (set! *rdir* (- *dir*))
           (set! *rvx*  (* *rdir* -10))
           (set! *rvy*  50)
           )
          ;; 敵の勝ち
          ((and (or  (= *ract*  2) (= *ract*  3))
                (not (= *act*  14)))
           (set! *endstate* 2)
           (set! *act* 14)
           (set! *dir* (- *rdir*))
           (set! *vx*  (* *dir* -10))
           (set! *vy*  50)
           )
          )
         )
       ;; 決着したとき
       (if (>= *endstate* 3)
         (cond
          ;; デモのとき
          (*demoflg*
           (set! *demotime* (+ *demotime* *wait*))
           (if (>= *demotime* 1600) (set! *scene* 0)))
          ;; デモでないとき
          (else
           (set! *scene* 2))
          ))
       ;; デモを抜けるチェック
       (when (and *demoflg*
                  (or (hash-table-get *keystate* (char->integer #\d) #f)
                      (hash-table-get *keystate* (char->integer #\D) #f)))
         (set! *scene*   0)
         (set! *demoflg* #f))
       )
      ((2) ; 戦闘終了
       ;; 時間待ち
       (case *waitstepB*
         ((0) (set! *waitstepB* 1)
              (set! *waitflg*   #t)
              (set! *waittime*  1500))
         ((3) 
          ;; キー入力待ち
          (case *waitstepA*
            ((0) (set! *waitstepA* 1)
                 (set! *waitflg*   #t)
                 (set! *waitkey*   '(#\d #\D)))
            ((3) (set! *waitstepA* 0)
                 (set! *waitstepB* 0)
                 (set! *scene*     0)))
          )
         )
       )
      )
    )
   )
  (glut-post-redisplay)
  ;; ウェイト時間調整
  ;; (前回からの経過時間を測定して、ウェイト時間が一定になるように調整する)
  (let* ((tnow      (current-time))
         (tnowmsec  (+ (* (~ tnow 'second) 1000) (quotient (~ tnow 'nanosecond) 1000000)))
         (tdiffmsec 0)
         (waitmsec  *wait*))
    (when *waitdata*
      (set! tdiffmsec (- tnowmsec *waitdata*))
      (cond
       ((and (>= tdiffmsec (- *wait*)) (< tdiffmsec *wait*))
        (set! waitmsec (- *wait* tdiffmsec)))
       ((and (>= tdiffmsec *wait*) (< tdiffmsec (* *wait* 50)))
        (set! waitmsec 1))
       ))
    (set! *waitdata* (+ tnowmsec waitmsec))
    ;(print tdiffmsec " " waitmsec)
    (glut-timer-func waitmsec timer 0))
  )

;; 自分の移動
(define (my-action)
  (define demostart (and *demoflg* (<= *starttime* 300)))
  ;; アクションで場合分け
  (case *act*
    ((0) ; 通常
     (when (not demostart)
       (set! *vy* (- *vy* 5))
       (if (<= *y* *miny*) (set! *vy* *stephigh*))
       (set! *dir* (if (> *x* *rx*) -1 1)))
     (cond
      ;; デモの起動直後のとき(何もしない)
      (demostart)
      ;; デモのとき
      (*demoflg*
       ;; 条件と乱数で行動を決定する
       (when (<= *y* *miny*)
         (set! *vx* 0)
         (set! *k*  (randint -1 1))
         (if (= *k* -1) (set! *vx* (+ -10 (if (> *x* *rx*) -2 0))))
         (if (= *k*  1) (set! *vx* (+  10 (if (< *x* *rx*)  2 0))))
         )
       (when (or (and (or (= *ract* 2) (= *ract* 3)) (< (abs (- *rx* *x*)) 250))
                 (= (randint 0 100) 0)
                 (and (<= (randint 0 100) 30) (< (abs (- *rx* *x*)) 250)))
         (set! *k*  (randint 0 10))
         (if (or (= *ract* 2) (= *ract* 3)) (set! *dir* (- *rdir*)))
         (cond
          ((<= *k* 2)
           (set! *act* 2)
           (set! *vx*  (* *dir* 15))
           (set! *vy*  50))
          ((<= *k* 8)
           (set! *act* 3)
           (set! *vx*  (* *dir* 25))
           (set! *vy*  20))
          )
         )
       )
      ;; デモでないとき
      (else
       ;; キー操作で行動を決定する
       (set! *vx* 0)
       (if (hash-table-get *spkeystate* GLUT_KEY_LEFT  #f)
         (set! *vx* (+ -10 (if (> *x* *rx*) -2 0))))
       (if (hash-table-get *spkeystate* GLUT_KEY_RIGHT #f)
         (set! *vx* (+  10 (if (< *x* *rx*)  2 0))))
       (when (> *kcount* 0) (dec! *kcount*) (set! *vx* 0) (set! *vy* 0))
       (when (or (hash-table-get *keystate* (char->integer #\z) #f)
                 (hash-table-get *keystate* (char->integer #\Z) #f))
         (set! *act* 2)
         (set! *vx*  (* *dir* 15))
         (set! *vy*  50))
       (when (and (<= *kcount* 0)
                  (or (hash-table-get *keystate* (char->integer #\x) #f)
                      (hash-table-get *keystate* (char->integer #\X) #f)))
         (set! *act* 3)
         (set! *vx*  (* *dir* 25))
         (set! *vy*  20)
         (set! *kcount* 5))
       )
      )
     )
    ((2) ; パンチ
     (cond
      ((> *endstate* 0)
       (set! *vx* 0)
       (set! *vy* 0))
      (else
       (set! *vy* (- *vy* 5))
       (if (< *vy* 0) (set! *act* 10))))
     )
    ((3) ; キック
     (cond
      ((> *endstate* 0)
       (set! *vx* 0)
       (set! *vy* 0))
      (else
       (set! *vy* (- *vy* 5))
       (when (and (<= *y* *miny*) (< *vy* 0))
         (set! *act* 11)
         (set! *ft*  *fixtime*))))
     )
    ((10) ; 落下
     (set! *vx* 0)
     (set! *vy* (- *vy* 5))
     (if (<= *y* *miny*) (set! *act* 0))
     )
    ((11) ; 硬直
     (set! *vx* 0)
     (dec! *ft*)
     (if (<= *ft* 0) (set! *act* 0))
     )
    ((12) ; 相打ち(パンチ)
     (set! *vy* (- *vy* 5))
     (if (< *vy* 0) (set! *act* 10))
     )
    ((13) ; 相打ち(キック)
     (set! *vy* (- *vy* 5))
     (when (and (<= *y* *miny*) (< *vy* 0))
       (set! *act* 11)
       (set! *ft*  *fixtime*))
     )
    ((14) ; やられ
     (set! *vy* (- *vy* 5))
     (when (and (<= *y* *miny*) (< *vy* 0))
       (set! *endstate* 4)
       (set! *vx* 0))
     )
    )
  (set! *x* (+ *x* *vx*))
  (if (< *x* *minx*) (set! *x* *minx*))
  (if (> *x* *maxx*) (set! *x* *maxx*))
  (set! *y* (+ *y* *vy*))
  (if (< *y* *miny*) (set! *y* *miny*))
  )

;; 敵の移動
(define (enemy-action)
  (define demostart (and *demoflg* (<= *starttime* 300)))
  ;; アクションで場合分け
  (case *ract*
    ((0) ; 通常
     (when (not demostart)
       (set! *rvy* (- *rvy* 5))
       (if (<= *ry* *miny*) (set! *rvy* *stephigh*))
       (set! *rdir* (if (>= *rx* *x*) -1 1)))
     (cond
      ;; デモの起動直後のとき(何もしない)
      (demostart)
      ;; その他のとき
      (else
       ;; 条件と乱数で行動を決定する
       (when (<= *ry* *miny*)
         (set! *rvx* 0)
         (set! *rk*  (randint -1 1))
         (if (= *rk* -1) (set! *rvx* (+ -10 (if (> *rx* *x*) -2 0))))
         (if (= *rk*  1) (set! *rvx* (+  10 (if (< *rx* *x*)  2 0))))
         )
       (when (or (and (or (= *act* 2) (= *act* 3)) (< (abs (- *rx* *x*)) 250))
                 (= (randint 0 100) 0))
         (set! *rk*  (randint 0 10))
         (if (or (= *act* 2) (= *act* 3)) (set! *rdir* (- *dir*)))
         (cond
          ((<= *rk* 2)
           (set! *ract* 2)
           (set! *rvx*  (* *rdir* 15))
           (set! *rvy*  50))
          ((<= *rk* 8)
           (set! *ract* 3)
           (set! *rvx*  (* *rdir* 25))
           (set! *rvy*  20))
          )
         )
       )
      )
     )
    ((2) ; パンチ
     (cond
      ((> *endstate* 0)
       (set! *rvx* 0)
       (set! *rvy* 0))
      (else
       (set! *rvy* (- *rvy* 5))
       (if (< *rvy* 0) (set! *ract* 10))))
     )
    ((3) ; キック
     (cond
      ((> *endstate* 0)
       (set! *rvx* 0)
       (set! *rvy* 0))
      (else
       (set! *rvy* (- *rvy* 5))
       (when (and (<= *ry* *miny*) (< *rvy* 0))
         (set! *ract* 11)
         (set! *rft*  *fixtime*))))
     )
    ((10) ; 落下
     (set! *rvx* 0)
     (set! *rvy* (- *rvy* 5))
     (if (<= *ry* *miny*) (set! *ract* 0))
     )
    ((11) ; 硬直
     (set! *rvx* 0)
     (dec! *rft*)
     (if (<= *rft* 0) (set! *ract* 0))
     )
    ((12) ; 相打ち(パンチ)
     (set! *rvy* (- *rvy* 5))
     (if (< *rvy* 0) (set! *ract* 10))
     )
    ((13) ; 相打ち(キック)
     (set! *rvy* (- *rvy* 5))
     (when (and (<= *ry* *miny*) (< *rvy* 0))
       (set! *ract* 11)
       (set! *rft*  *fixtime*))
     )
    ((14) ; やられ
     (set! *rvy* (- *rvy* 5))
     (when (and (<= *ry* *miny*) (< *rvy* 0))
       (set! *endstate* 3)
       (set! *rvx* 0))
     )
    )
  (set! *rx* (+ *rx* *rvx*))
  (if (< *rx* *minx*) (set! *rx* *minx*))
  (if (> *rx* *maxx*) (set! *rx* *maxx*))
  (set! *ry* (+ *ry* *rvy*))
  (if (< *ry* *miny*) (set! *ry* *miny*))
  )

;; 長方形の衝突チェック
(define (recthit? x1 y1 w1 h1 x2 y2 w2 h2)
  (if (and (< x1 (+ x2 w2))
           (< x2 (+ x1 w1))
           (< y1 (+ y2 h2))
           (< y2 (+ y1 h1)))
    #t
    #f)
  )

;; メイン処理
(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size *width* *height*)
  (glut-init-window-position 100 100)
  (glut-create-window "fighter")
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-keyboard-up-func keyboardup)
  (glut-special-func specialkey)
  (glut-special-up-func specialkeyup)
  (glut-timer-func *wait* timer 0)
  ;; コールバック内でエラーが発生すると異常終了する件の対策
  (guard (ex (else (report-error ex) (exit 0)))
    (glut-main-loop))
  0)

