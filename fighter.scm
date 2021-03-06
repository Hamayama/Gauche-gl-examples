;; -*- coding: utf-8 -*-
;;
;; fighter.scm
;; 2019-6-19 2.22
;;
;; ＜内容＞
;;   Gauche-gl を使用した、簡単な格闘ゲームです。
;;   左側が自分になります。
;;   矢印キーで左右移動。[z]キーでパンチ。[x]キーでキックです。
;;   防御はありません。相打ちはノーダメージです。
;;   パンチ後は下降中が無防備になります。
;;   キック後は一定時間無防備になります。
;;   Ready?の画面でしばらく待つとデモになります。
;;   ESCキーを押すと終了します。
;;
(add-load-path "lib" :relative)
(add-load-path "model" :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use glmintool)
(use gltextscrn)
(use alaudplay)
(use alauddata)
(use glmodelkit)
(use model0101)

(define *wait*      28) ; ウェイト(msec)
(define *title* "fighter") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (* (/. *vangle* 2) pi/180))) ; 視野角/2のタンジェント(計算用)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *zd/2*     200) ; 画面奥行き/2
(define *chw*      100) ; キャラクタの幅
(define *chh*      100) ; キャラクタの高さ
(define *gdy*     -300) ; 地面のY座標
(define *maxx*       (- *wd/2* (/. *chw* 2))) ; X座標最大値
(define *minx*       (- *maxx*))              ; X座標最小値
(define *miny*       (+ *gdy*  *chh*))        ; Y座標最小値
(define *waku*      10) ; 当たり判定調整用
(define *fixtime*   10) ; 硬直時間
(define *stephigh*  29) ; ステップ高さ
(define *demoflag*   #f) ; デモフラグ
(define *starttime*  0) ; スタート後経過時間(msec)
(define *scene*      0) ; シーン情報(=0:スタート画面,=1:戦闘中,=2:戦闘終了)
(define *playcount*  0) ; プレイ数
(define *wincount*   0) ; 勝利数

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 2) (* *ht/2* 2))

;; キー入力状態管理クラスのインスタンス生成
(define *ksinfo* (make <keystateinfo>))

;; キー入力待ちクラスのインスタンス生成
(define *kwinfo* (make <keywaitinfo> :keystate (~ *ksinfo* 'keystate)))

;; 時間待ちクラスのインスタンス生成
(define *twinfo* (make <timewaitinfo> :waitinterval *wait*))

;; ウェイト時間調整クラスのインスタンス生成
(define *wcinfo* (make <waitcalcinfo> :waittime *wait*))

;; ファイタークラス
;;   ・自分と敵の移動、衝突判定、表示を行う
;;   ・グローバル変数や外部の手続きに依存しているので注意
(define-class <fighter> ()
  ((type     :init-value 0) ; タイプ(=0:自分,=1:敵)
   (x        :init-value 0) ; X座標
   (y        :init-value 0) ; Y座標
   (vx       :init-value 0) ; X方向速度
   (vy       :init-value 0) ; Y方向速度
   (act      :init-value 0) ; アクション(=0:通常,=2:パンチ,=3:キック,=10:落下,=11:硬直,
   ;                                     =12:相打ち(パンチ),=13:相打ち(キック),=14:やられ)
   (dir      :init-value 0) ; 方向(=-1:左向き,=0:不明,=1:右向き)
   (ft       :init-value 0) ; 硬直時間カウント用
   (endstate :init-value 0) ; 終了状態(=0:初期状態,=1:自分の勝ち,=2:敵の勝ち,=3:敵の勝ちで終了)
   ))
(define-method fighter-init ((f1 <fighter>) (type <integer>) (x <real>) (y <real>) (dir <integer>))
  (set! (~ f1 'type)     type)
  (set! (~ f1 'x)        x)
  (set! (~ f1 'y)        y)
  (set! (~ f1 'vx)       0)
  (set! (~ f1 'vy)       0)
  (set! (~ f1 'act)      0)
  (set! (~ f1 'dir)      dir)
  (set! (~ f1 'ft)       0)
  (set! (~ f1 'endstate) 0))
(define-method fighter-finished? ((f1 <fighter>))
  (= (~ f1 'endstate) 3))
(define-method fighter-move ((f1 <fighter>) (f2 <fighter>))
  ;; アクションで場合分け
  (case (~ f1 'act)
    ((0) ; 通常
     (cond
      ;; デモの起動直後のとき(何もしない)
      ((and *demoflag* (<= *starttime* 300)))
      ;; デモの起動直後でないとき
      (else
       (set! (~ f1 'vy) (- (~ f1 'vy) 5))
       (if (<= (~ f1 'y) *miny*) (set! (~ f1 'vy) *stephigh*))
       (set! (~ f1 'dir) (sign-value2 (- (~ f2 'x) (~ f1 'x))))
       (cond
        ;; 「タイプが自分」かつデモでないとき
        ((and (= (~ f1 'type) 0) (not *demoflag*))
         ;; キー操作で行動を決定する
         (cond
          ((and (spkey-on? *ksinfo* GLUT_KEY_LEFT) (not (spkey-on? *ksinfo* GLUT_KEY_RIGHT)))
           (set! (~ f1 'vx) (+ -10 (if (> (~ f1 'x) (~ f2 'x)) -2 0)))) ; 左移動
          ((and (spkey-on? *ksinfo* GLUT_KEY_RIGHT) (not (spkey-on? *ksinfo* GLUT_KEY_LEFT)))
           (set! (~ f1 'vx) (+  10 (if (< (~ f1 'x) (~ f2 'x))  2 0)))) ; 右移動
          (else (set! (~ f1 'vx) 0)))
         (let ((d     (abs (- (~ f2 'x) (~ f1 'x)))) ; 相手との間合い
               (f2atk (or  (= (~ f2 'act) 2) (= (~ f2 'act) 3)))) ; 相手攻撃状態
           (when (key-on? *ksinfo* '(#\z #\Z))
             (set! (~ f1 'act) 2) ; パンチ
             (if (and (< d *chw*) f2atk) (set! (~ f1 'dir) (- (~ f2 'dir))))
             (set! (~ f1 'vx) (* (~ f1 'dir) 15))
             (set! (~ f1 'vy) 50))
           (when (key-on? *ksinfo* '(#\x #\X))
             (set! (~ f1 'act) 3) ; キック
             (if (and (< d *chw*) f2atk) (set! (~ f1 'dir) (- (~ f2 'dir))))
             (set! (~ f1 'vx) (* (~ f1 'dir) 23))
             (set! (~ f1 'vy) 20))
           )
         )
        ;; 「タイプが敵」またはデモのとき
        (else
         ;; 条件と乱数で行動を決定する
         (when (<= (~ f1 'y) *miny*)
           (case (randint -1 1)
             ((-1) (set! (~ f1 'vx) (+ -10 (if (> (~ f1 'x) (~ f2 'x)) -2 0)))) ; 左移動
             ((1)  (set! (~ f1 'vx) (+  10 (if (< (~ f1 'x) (~ f2 'x))  2 0)))) ; 右移動
             (else (set! (~ f1 'vx) 0)))
           )
         (let ((d     (abs (- (~ f2 'x) (~ f1 'x)))) ; 相手との間合い
               (f2atk (or  (= (~ f2 'act) 2) (= (~ f2 'act) 3)))) ; 相手攻撃状態
           (when (or (and f2atk
                          (< d 250))
                     (and (<= (randint 0 100) 1)
                          (or (< d 250) (> d 550) (<= (randint 0 100) 20)))
                     (and (= (~ f1 'type) 0)
                          (<= (randint 0 100) 30)
                          (< d 250)))
             (let ((k  (randint 0 100))
                   (k1 (case (~ f2 'act) ((2) 50) ((3) 5) (else 30)))
                   (k2 (if (< d *chw*) 95 80)))
               (cond
                ((<= k k1)
                 (set! (~ f1 'act) 2) ; パンチ
                 (if (and (< d *chw*) f2atk) (set! (~ f1 'dir) (- (~ f2 'dir))))
                 (set! (~ f1 'vx) (* (~ f1 'dir) 15))
                 (set! (~ f1 'vy) 50))
                ((<= k k2)
                 (set! (~ f1 'act) 3) ; キック
                 (if (and (< d *chw*) f2atk) (set! (~ f1 'dir) (- (~ f2 'dir))))
                 (set! (~ f1 'vx) (* (~ f1 'dir) 23))
                 (set! (~ f1 'vy) 20))
                ))
             ))
         ))
       ))
     )
    ((2) ; パンチ
     (cond
      ((> (~ f1 'endstate) 0)
       (set! (~ f1 'vx) 0)
       (set! (~ f1 'vy) 0))
      (else
       (set! (~ f1 'vy) (- (~ f1 'vy) 5))
       (if (< (~ f1 'vy) 0) (set! (~ f1 'act) 10))))
     )
    ((3) ; キック
     (cond
      ((> (~ f1 'endstate) 0)
       (set! (~ f1 'vx) 0)
       (set! (~ f1 'vy) 0))
      (else
       (set! (~ f1 'vy) (- (~ f1 'vy) 5))
       (when (and (<= (~ f1 'y) *miny*) (< (~ f1 'vy) 0))
         (set! (~ f1 'act) 11)
         (set! (~ f1 'ft)  *fixtime*))))
     )
    ((10) ; 落下
     (set! (~ f1 'vx) 0)
     (set! (~ f1 'vy) (- (~ f1 'vy) 5))
     (if (<= (~ f1 'y) *miny*) (set! (~ f1 'act) 0))
     )
    ((11) ; 硬直
     (set! (~ f1 'vx) 0)
     (dec! (~ f1 'ft))
     (if (<= (~ f1 'ft) 0) (set! (~ f1 'act) 0))
     )
    ((12) ; 相打ち(パンチ)
     (set! (~ f1 'vy) (- (~ f1 'vy) 5))
     (if (< (~ f1 'vy) 0) (set! (~ f1 'act) 10))
     )
    ((13) ; 相打ち(キック)
     (set! (~ f1 'vy) (- (~ f1 'vy) 5))
     (when (and (<= (~ f1 'y) *miny*) (< (~ f1 'vy) 0))
       (set! (~ f1 'act) 11)
       (set! (~ f1 'ft)  *fixtime*))
     )
    ((14) ; やられ
     (set! (~ f1 'vy) (- (~ f1 'vy) 5))
     (when (and (<= (~ f1 'y) *miny*) (< (~ f1 'vy) 0))
       (set! (~ f1 'endstate) 3)
       (set! (~ f1 'vx) 0))
     )
    )
  (set! (~ f1 'x) (clamp (+ (~ f1 'x) (~ f1 'vx)) *minx* *maxx*))
  (set! (~ f1 'y) (clamp (+ (~ f1 'y) (~ f1 'vy)) *miny*))
  )
(define-method fighter-check ((f1 <fighter>) (f2 <fighter>))
  ;; 衝突判定
  (if (recthit? (+ (~ f1 'x) *waku*)
                (+ (~ f1 'y) *waku*)
                (- *chw*     (* *waku* 2))
                (- *chh*     (* *waku* 2))
                (+ (~ f2 'x) *waku*)
                (+ (~ f2 'y) *waku*)
                (- *chw*     (* *waku* 2))
                (- *chh*     (* *waku* 2)))
    (cond
     ;; 相打ち
     ((and (or (= (~ f1 'act) 2) (= (~ f1 'act) 3))
           (or (= (~ f2 'act) 2) (= (~ f2 'act) 3)))
      (set! (~ f1 'act) (+ (~ f1 'act) 10))
      (set! (~ f2 'act) (+ (~ f2 'act) 10))
      (set! (~ f1 'vx)  (- (~ f1 'vx)))
      (set! (~ f2 'vx)  (- (~ f2 'vx)))
      (if (< (~ f1 'vy) 15) (set! (~ f1 'vy) 15))
      (if (< (~ f2 'vy) 15) (set! (~ f2 'vy) 15))
      (if (not *demoflag*) (auddata-play *adata-hit1*)))
     ;; 自分の勝ち
     ((and (or  (= (~ f1 'act) 2) (= (~ f1 'act) 3))
           (not (= (~ f2 'act) 14)))
      (set! (~ f1 'endstate) 1)
      (set! (~ f2 'endstate) 2)
      (set! (~ f2 'act) 14)
      (set! (~ f2 'dir) (- (~ f1 'dir)))
      (set! (~ f2 'vx)  (* (~ f2 'dir) -10))
      (set! (~ f2 'vy)  50)
      (if (not *demoflag*) (auddata-play *adata-hit1*)))
     ;; 敵の勝ち
     ((and (or  (= (~ f2 'act) 2) (= (~ f2 'act) 3))
           (not (= (~ f1 'act) 14)))
      (set! (~ f1 'endstate) 2)
      (set! (~ f2 'endstate) 1)
      (set! (~ f1 'act) 14)
      (set! (~ f1 'dir) (- (~ f2 'dir)))
      (set! (~ f1 'vx)  (* (~ f1 'dir) -10))
      (set! (~ f1 'vy)  50)
      (if (not *demoflag*) (auddata-play *adata-hit1*)))
     )))
(define-method fighter-disp ((f1 <fighter>))
  (gl-push-matrix)
  (gl-translate (~ f1 'x) (~ f1 'y) 0)
  (gl-rotate (* (~ f1 'dir) 90) 0 1 0)
  (model0101 (~ f1 'type)
             (case (~ f1 'act)
               ((2 12) 4)
               ((3 13) 5)
               ((14)   3)
               (else (cond
                      ((< (* (~ f1 'dir) (~ f1 'vx)) 0) 2)
                      ((> (* (~ f1 'dir) (~ f1 'vx)) 0) 1)
                      (else 0)))))
  (gl-pop-matrix))
(define *f1* (make <fighter>)) ; インスタンス生成(自分)
(define *f2* (make <fighter>)) ; インスタンス生成(敵)
(fighter-init *f1* 0 (+ *minx* 40) *miny*  1)
(fighter-init *f2* 1 (- *maxx* 40) *miny* -1)


;; モデル0101(人形)(頭に原点あり)(高さ100)
;;   type  タイプ(=0:自分,=1:敵)
;;   pose  ポーズ(=0:通常,=1:前進,=2:後退,=3:やられ,=4:パンチ,=5:キック)
;; (define (model0101 type pose) ...)

;; 地面(上面に原点あり)
(define (ground)
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 0.0 0.0 1.0))
  (box-model *wd/2* *ht/2* 150))


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 光源設定
  (gl-light  GL_LIGHT0 GL_POSITION #f32(1.0 1.0 1.0 0.0))
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_NORMALIZE)
  ;; 材質設定
  (gl-material GL_FRONT GL_SPECULAR #f32(1.0 1.0 1.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 10.0)
  ;; 音楽データの初期化
  (init-auddata *app-dpath*))

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 文字表示
  (let ((str1 "") (str2 "") (str3 "") (y2 31))
    (cond
     ;; デモのとき
     (*demoflag*
      (set! str1 "== Demo ==")
      (set! str2 "HIT [D] KEY"))
     ;; デモでないとき
     (else
      ;; シーン情報で場合分け
      (case *scene*
        ((0) ; スタート画面
         (set! str1 "Ready?")
         (set! str2 "HIT [S] KEY")
         (set! y2 32))
        ((1) ; 戦闘中
         (when (<= *starttime* 3000)
           (set! str1 "Fight!!")
           (set! str2 "USE [<-] [->] [Z] [X] KEY")))
        ((2) ; 戦闘終了
         (set! str1 (if (fighter-finished? *f2*) "You win!!" "You lose!!"))
         (if (timewait-finished? *twinfo*) (set! str2 "HIT [D] KEY")))
        ))
     )
    (set! str3 (format "(W=~D L=~D R=~D)"
                       *wincount*
                       (- *playcount* *wincount*)
                       (if (= *playcount* 0)
                         0.0
                         (round-n (/. *wincount* *playcount*) 2))))
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text str1 (win-w-r *win* 1/2) (win-h-r *win* 14/100) *width* *height* (win-h-r *win* 1/9) 'center)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str2 (win-w-r *win* 1/2) (win-h-r *win* y2 100) *width* *height* (win-h-r *win* 1/18) 'center)
    (gl-color 0.0 1.0 0.0 1.0)
    (draw-stroke-text str3 (win-h-r *win* 1/100) (win-h-r *win* 1/100) *width* *height* (win-h-r *win* 1/24))
    )
  ;; 自分の表示
  (fighter-disp *f1*)
  ;; 敵の表示
  (fighter-disp *f2*)
  ;; 地面の表示
  (gl-push-matrix)
  (gl-translate 0 *gdy* 0)
  (ground)
  (gl-pop-matrix)
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  w)
  (set! *height* (min w h))
  (win-update-size *win* *width* *height*)
  ;; 縦横比を変えずにリサイズ
  (gl-viewport 0 (quotient (- h *height*) 2) *width* *height*)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (let1 z1 (/. *ht/2* *tanvan*)
    ;; 透視射影する範囲を設定
    (glu-perspective *vangle* (/. *width* *height*) (- z1 *zd/2*) (+ z1 *zd/2*))
    ;; 視点の位置と方向を設定
    (glu-look-at 0 0 z1 0 0 0 0 1 0)))

;; キー入力ON
(define (keyboard key x y)
  (key-on *ksinfo* key)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit-main-loop 0))
   ;; [g]キーでGC実行(デバッグ用)
   ((or (= key (char->integer #\g)) (= key (char->integer #\G)))
    (gc) (print (gc-stat)))
   ))

;; キー入力OFF
(define (keyboardup key x y)
  (key-off *ksinfo* key))

;; 特殊キー入力ON
(define (specialkey key x y)
  (spkey-on *ksinfo* key))

;; 特殊キー入力OFF
(define (specialkeyup key x y)
  (spkey-off *ksinfo* key))

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
       (fighter-init *f1* 0 (+ *minx* 40) *miny*  1)
       (fighter-init *f2* 1 (- *maxx* 40) *miny* -1)
       (set! *starttime* 0)
       (cond
        ;; デモのとき
        (*demoflag*
         (set! *scene* 1))
        ;; デモでないとき
        (else
         ;; キー入力待ち
         (keywait  *kwinfo* '(#\s #\S)
                   (lambda ()
                     (set! *scene*    1)
                     (auddata-play *adata-start1*)
                     (keywait-clear  *kwinfo*)
                     (timewait-clear *twinfo*)))
         ;; 時間待ち(タイムアップでデモへ移行)
         (timewait *twinfo* 5000
                   (lambda ()
                     (set! *scene*    1)
                     (set! *demoflag* #t)
                     (keywait-clear  *kwinfo*)
                     (timewait-clear *twinfo*))))
        )
       )
      ((1) ; 戦闘中
       (if (< *starttime* 60000) (set! *starttime* (+ *starttime* *wait*)))
       ;; 自分の移動
       (fighter-move *f1* *f2*)
       ;; 敵の移動
       (fighter-move *f2* *f1*)
       ;; 衝突判定
       (fighter-check *f1* *f2*)
       ;; 終了判定
       (when (or (fighter-finished? *f1*) (fighter-finished? *f2*))
         (set! *scene* 2)
         (when (not *demoflag*)
           (auddata-play *adata-end1*)
           (inc! *playcount*)
           (if (fighter-finished? *f2*) (inc! *wincount*))
           ))
       ;; デモを抜けるチェック
       (when (and *demoflag* (key-on? *ksinfo* '(#\d #\D)))
         (set! *scene*    0)
         (set! *demoflag* #f))
       )
      ((2) ; 戦闘終了
       (cond
        ;; デモのとき
        (*demoflag*
         ;; キー入力待ち
         (keywait  *kwinfo* '(#\d #\D)
                   (lambda ()
                     (set! *scene*    0)
                     (set! *demoflag* #f)
                     (keywait-clear  *kwinfo*)
                     (timewait-clear *twinfo*)))
         ;; 時間待ち(タイムアウトでデモを続行)
         (timewait *twinfo* 2000
                   (lambda ()
                     (set! *scene* 0)
                     (keywait-clear  *kwinfo*)
                     (timewait-clear *twinfo*))))
        ;; デモでないとき
        (else
         ;; 時間待ち
         (timewait *twinfo* 1500
                   (lambda ()
                     ;; キー入力待ち
                     (keywait *kwinfo* '(#\d #\D)
                              (lambda ()
                                (set! *scene* 0)
                                (keywait-clear  *kwinfo*)
                                (timewait-clear *twinfo*))))))
        )
       )
      )
    )
   )
  ;; 画面表示
  (glut-post-redisplay)
  ;; ウェイト時間調整
  (glut-timer-func (waitcalc *wcinfo*) timer 0))

;; 終了
(define (exit-main-loop code)
  (aud-end)
  (exit code))

;; メイン処理
(define (main args)
  (aud-init (> (x->integer (list-ref args 1 0)) 0))
  (glut-init '())
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size *width* *height*)
  (glut-init-window-position 100 100)
  (glut-create-window *title*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-keyboard-up-func keyboardup)
  (glut-special-func specialkey)
  (glut-special-up-func specialkeyup)
  (glut-timer-func *wait* timer 0)
  (glut-show-window)
  ;; コールバック内エラー対策
  (guard (ex (else (report-error ex) (exit-main-loop 1)))
    (glut-main-loop))
  0)

