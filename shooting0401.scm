;; -*- coding: utf-8 -*-
;;
;; shooting0401.scm
;; 2022-5-22 v1.05
;;
;; ＜内容＞
;;   Gauche-gl を使用した、簡単なシューティングゲームです。
;;   (本プログラムは、shooting0301 の視点を変更して、疑似3D風にしたものです)
;;   矢印キーで上下左右移動。
;;   [Ctrl]/[Space]/[a]/[z]キーのいずれかでビーム発射です(押し続けると発射し続けます)。
;;   敵(ワーム)は、頭の部分にのみダメージを与えられます。
;;   (かなり固いため、しばらくビームを当て続ける必要があります)
;;   画面右上のレベル表示は、出現する敵の数の目安になります。
;;   また、スタート画面でしばらく待つとデモになります。
;;   ESCキーを押すと終了します。
;;
(add-load-path "lib" :relative)
(add-load-path "model" :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use gauche.sequence)
(use math.const)
(use glmintool)
(use gltextscrn)
(use alaudplay)
(use alauddata)
(use glwormkit)
(use glmodelkit)
;(use model0501)
(use model0601)

(define *wait*      20) ; ウェイト(msec)
(define *title* "shooting0401") ; ウィンドウのタイトル
(define *width*    960) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (* (/. *vangle* 2) pi/180))) ; 視野角/2のタンジェント(計算用)
(define *aratio*     (/. *width* *height*)) ; アスペクト比(計算用)
(define *vcamera*   39) ; カメラの視線とスクリーン(XY平面)との角度(度)(1-179まで対応)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     405) ; 画面高さ/2
(define *zd/2*     100) ; 画面奥行き/2
(define *chw*       16) ; 文字の幅
(define *chh*       32) ; 文字の高さ
(define *maxx*       (- *wd/2* (* *chw* 1.5)))   ; 自機のX座標最大値
(define *minx*       (- *maxx*))                 ; 自機のX座標最小値
(define *maxy*       (- *ht/2* *chh*))           ; 自機のY座標最大値
(define *miny*       (+ (- *ht/2*) (* *chh* 2))) ; 自機のY座標最小値
(define/init *x*     0) ; 自機のX座標
(define/init *y*     (* *ht/2* -0.6)) ; 自機のY座標
(define *v*         10) ; 自機の速度
(define/init *bc*    0) ; 自機ビームカウンタ
(define *waku*       5) ; 当たり判定調整用
(define/init *mr*    1) ; 敵の数
(define/init *mmr*   1) ; 敵の最大数
(define *mmmr*       7) ; 敵の最大数の最大数
(define/init *life* 50) ; 敵の耐久力
(define *maxlife*  100) ; 敵の耐久力の最大値
(define *wlen*       8) ; ワームの長さ
(define *bossflag*   #f) ; ボス出現フラグ
(define *bosssize* 1.5) ; ボスの大きさ(倍率)
(define *sc*         0) ; スコア
(define *hs*         0) ; ハイスコア
(define *ssc*        0) ; 制御カウンタ
(define *scene*      0) ; シーン情報(=0:スタート画面,=1:プレイ中,=2:プレイ終了)
(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色

(define *demomode*   0) ; デモモード(=0:通常デモ,=1:高速デモ)
(define *demoflag*   #f) ; デモフラグ
(define *demotime1*  0) ; デモ時間調整用1(msec)
(define *demotime2*  0) ; デモ時間調整用2(msec)
(define *democount*  0) ; デモ回数
(define *demosscsum* 0) ; デモ生存カウンタ累積
(define *demotmax* 0.0) ; デモ生存時間最大値
(define *demotmin* 0.0) ; デモ生存時間最小値
(define *demotavg* 0.0) ; デモ生存時間平均値

(define *flatdisp*   0) ; フラット表示(=0:OFF,=1:ON)

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

;; テキスト画面クラスのインスタンス生成
;; (自機)
(define *tscrn-mychr1* (make <textscrn>))
(textscrn-init *tscrn-mychr1* 3 2)
(textscrn-pset *tscrn-mychr1* 0 0 " A ")
(textscrn-pset *tscrn-mychr1* 0 1 "HOH")
(define *tscrn-mychr2* (make <textscrn>))
(textscrn-init *tscrn-mychr2* 3 2)
(textscrn-pset *tscrn-mychr2* 0 0 " @ ")
(textscrn-pset *tscrn-mychr2* 0 1 "@@@")
;; (自機ビーム)
(define *tscrn-beam1* (make <textscrn>))
(textscrn-init *tscrn-beam1* 1 (floor->exact (/. (* *ht/2* 2) *chh*)))

;; 敵クラス
(define-class <enemy> ()
  ((useflag :init-value #f) ; 使用フラグ
   (kind    :init-value 0)  ; 種別(=0:ワーム0101,=1:ワーム0201)
   (state   :init-value 0)  ; 状態(=0:通常,=1:被弾,=10-50:やられ)
   (life    :init-value 0)  ; 耐久力
   (minx    :init-value 0)  ; X座標の最小値
   (maxx    :init-value 0)  ; X座標の最大値
   (miny    :init-value 0)  ; Y座標の最小値
   (maxy    :init-value 0)  ; Y座標の最大値
   (worm    :init-value #f) ; 現在使用中のインスタンス
   (worm1   :init-value #f) ; ワーム0101のインスタンス
   (worm2   :init-value #f) ; ワーム0201のインスタンス
   (count1  :init-value 0)  ; 動作カウンタ1
   (count2  :init-value 0)  ; 動作カウンタ2
   (boss    :init-value #f) ; ボスフラグ
   ))
;; 敵クラスのインスタンス生成
(define *enemies* (make-vector-of-class *mmmr* <enemy>))

;; デモ用パラメータクラス
(define-class <demoparam> ()
  ((p1 :init-value 6.0) ; デモ用パラメータ1(敵との距離のしきい値)
   (p2 :init-value  50) ; デモ用パラメータ2(中央に戻る確率)
   ))
(define-method demoparam-copy ((d1 <demoparam>) (d2 <demoparam>))
  (set! (~ d2 'p1) (~ d1 'p1))
  (set! (~ d2 'p2) (~ d1 'p2)))
;; デモ用パラメータクラスのインスタンス生成
(define *dparam*     (make <demoparam>))
(define *dparam-old* (make <demoparam>))


;; 自機の表示
(define (disp-mychr pattern)
  ;(gl-color (if (= pattern 0) #f32(1.0 1.0 0.0 1.0) #f32(1.0 0.0 0.0 1.0)))
  ;(textscrn-disp (if (= pattern 0) *tscrn-mychr1* *tscrn-mychr2*)
  ;               (win-x *win* *x*) (win-y *win* *y*) *width* *height*
  ;               (win-w *win* *chw*) (win-h *win* *chh*) 'center)
  (gl-push-matrix)
  (gl-translate *x* *y* 0)
  (model0601 pattern *chw* *chh*)
  (gl-pop-matrix)
  )

;; 自機の移動
(define (move-mychr)
  ;; デモ用の自機移動処理
  (define (demo-move-mychr x y)
    (let* ((ne1   (get-nearest-enemy x y *enemies*))
           (rr1   (~ ne1 0))
           (e1    (~ ne1 1))
           (index (~ ne1 2))
           (ne2   (get-nearest-enemy x y *enemies* #t))
           (e2    (~ ne2 1))
           (vx    0)
           (x1    (if e1 (~ e1 'worm 'axvec index) 0))
           (x2    (if e2 (~ e2 'worm 'axvec 0) 0))
           (y2    (if e2 (~ e2 'worm 'ayvec 0) 0))
           (d1    (* *chw* (~ *dparam* 'p1)))
           (pri   #f))
      (cond
       ;; 一番近い敵を避ける(優先行動)
       ((and e1 (< rr1 (* d1 d1)))
        (set! vx (* (sign-value2 (- x1 x)) (- *v*)))
        (set! pri #t))
       ;; 一番近い敵に近づく
       ((and e2 (<= (- *wd/2*) x2 *wd/2*) (<= (- *ht/2*) y2 *ht/2*))
        (set! vx (* (sign-value  (- x2 x))    *v*)))
       ;; 中央に戻る
       ((and (<= (randint 1 100) (~ *dparam* 'p2))
             (>= (abs (- 0 x)) *v*))
        (set! vx (* (sign-value  (-  0 x))    *v*))))
      (cons vx pri)))
  (cond
   ;; デモのとき
   (*demoflag*
    ;; 自機の移動
    ;; (優先行動でないときは、自機の振動を抑制する)
    (let* ((ret1 (demo-move-mychr *x* *y*))
           (vx1  (car ret1))
           (pri  (cdr ret1))
           (ret2 (demo-move-mychr (+ *x* vx1) *y*))
           (vx2  (car ret2)))
      (if (and (not pri) (not (= vx1 0)) (= vx1 (- vx2)))
        (set! vx1 0))
      (set! *x* (clamp (+ *x* vx1) *minx* *maxx*)))
    ;; 自機ビーム発射
    (cond
     ((attack-enemies?)
      (set! *bc* 1)
      (set! *demotime2* 0))
     ((> *bc* 0)
      (set! *demotime2* (+ *demotime2* *wait*))
      (if (>= *demotime2* 200) (set! *bc* 0))))
    )
   ;; デモでないとき
   (else
    ;; 自機の移動
    (let ((vx 0) (vy 0))
      (if (spkey-on? *ksinfo* GLUT_KEY_LEFT)  (set! vx (+ vx (- *v*))))
      (if (spkey-on? *ksinfo* GLUT_KEY_RIGHT) (set! vx (+ vx    *v*)))
      (if (spkey-on? *ksinfo* GLUT_KEY_UP)    (set! vy (+ vy    *v*)))
      (if (spkey-on? *ksinfo* GLUT_KEY_DOWN)  (set! vy (+ vy (- *v*))))
      (set! *x* (clamp (+ *x* vx) *minx* *maxx*))
      (set! *y* (clamp (+ *y* vy) *miny* *maxy*)))
    ;; 自機ビーム発射
    (if (or (mdkey-on? *ksinfo* GLUT_ACTIVE_CTRL)
            (key-on?   *ksinfo* '(#\space #\a #\A #\z #\Z)))
      (set! *bc* 1)
      (set! *bc* 0))
    )
   ))

;; 敵の初期化
(define (init-enemies enemies)
  (for-each (lambda (e1) (set! (~ e1 'useflag) #f)) enemies))

;; 敵の生成
(define (make-enemies enemies)
  (let1 i (find-index (lambda (e1) (not (~ e1 'useflag))) enemies)
    (if (and i (< i *mr*))
      (let ((e1   (~ enemies i))
            (minx (- *wd/2*))
            (maxx    *wd/2*)
            (miny (- (* *ht/2* 2)))
            (maxy    (* *ht/2* 2))
            (w1   (make <worm0101>))
            (w2   (make <worm0201>)))
        (set! (~ e1 'useflag) #t)
        (set! (~ e1 'kind)    1)
        (set! (~ e1 'state)   0)
        (set! (~ e1 'life)    *life*)
        (set! (~ e1 'minx)    minx)
        (set! (~ e1 'maxx)    maxx)
        (set! (~ e1 'miny)    miny)
        (set! (~ e1 'maxy)    maxy)
        (set! (~ e1 'worm)    w2)
        (set! (~ e1 'worm1)   w1)
        (set! (~ e1 'worm2)   w2)
        (set! (~ e1 'count1)  0)
        (set! (~ e1 'count2)  0)
        (set! (~ e1 'boss)    *bossflag*)
        ;; ボス出現
        (when *bossflag*
          (set! *bossflag* #f)
          (set! (~ e1 'life) (* (~ e1 'life) *bosssize*))
          (set! (~ e1 'miny) (* (~ e1 'miny) *bosssize*))
          (set! (~ e1 'maxy) (* (~ e1 'maxy) *bosssize*))
          (set! (~ w1 'fr)   (* (~ w1 'fr)   *bosssize*))
          (set! (~ w1 'ar)   (* (~ w1 'ar)   *bosssize*))
          (set! (~ w1 'al)   (* (~ w1 'al)   *bosssize*))
          (set! (~ w1 'rr)   (* (~ w1 'rr)   *bosssize*))
          (set! (~ w2 'fr)   (* (~ w2 'fr)   *bosssize*))
          (set! (~ w2 'ar)   (* (~ w2 'ar)   *bosssize*))
          (set! (~ w2 'al)   (* (~ w2 'al)   *bosssize*))
          (set! (~ w2 'rr)   (* (~ w2 'rr)   *bosssize*)))
        (worm-init w1 *wlen* (randint minx maxx) maxy 0)
        (worm-init w2 *wlen* (randint minx maxx) maxy 0)
        (worm-set-goal w1 (randint minx maxx) (randint (- *ht/2*) *ht/2*))
        (worm-set-goal w2 (randint minx maxx) (randint (- *ht/2*) *ht/2*))
        ))))

;; 敵の表示
(define (disp-enemies enemies)
  ;(gl-push-attrib GL_LIGHTING_BIT)
  (for-each
   (lambda (e1)
     (when (~ e1 'useflag)
       (let ((w1    (~ e1 'worm))
             (color (case (~ e1 'state)
                      ((0)  #f32(1.0 1.0 1.0 1.0))
                      ((1)  (if (= *flatdisp* 0)
                              #f32(0.5 0.0 0.0 1.0)
                              #f32(0.9 0.5 0.2 1.0)))
                      (else #f32(1.0 1.0 0.0 1.0))))
             (wedge (case (~ e1 'state)
                      ((0)  (if (= (~ e1 'kind) 0) 70 90))
                      ((1)  110)
                      (else 130))))
         (if (= *flatdisp* 0)
           (worm-disp w1 color wedge 0)
           (worm-disp-flat w1 *win* color wedge)))))
   enemies)
  ;(gl-pop-attrib)
  )

;; 敵の移動
(define (move-enemies enemies)
  (for-each
   (lambda (e1)
     (if (~ e1 'useflag)
       (case (~ e1 'state)
         ((0 1)
          ;; 被弾の解除
          (if (= (~ e1 'state) 1) (set! (~ e1 'state) 0))
          ;; 敵の移動
          (let ((w1   (~ e1 'worm))
                (minx (~ e1 'minx))
                (maxx (~ e1 'maxx))
                (miny (~ e1 'miny))
                (maxy (~ e1 'maxy)))
            (worm-move w1)
            (set! (~ e1 'count1) (+ (~ e1 'count1) *wait*))
            ;; 目標に到達したか、または、10秒経過したとき
            (when (or (worm-goal? w1 *waku*)
                      (>= (~ e1 'count1) 10000))
              (set! (~ e1 'count1) 0)
              (inc! (~ e1 'count2))
              ;; ワームの種別を切り換える
              (case (~ e1 'kind)
                ((0)
                 (set! (~ e1 'kind) 1)
                 (set! (~ e1 'worm) (~ e1 'worm2))
                 (worm-convert (~ e1 'worm1) (~ e1 'worm2)))
                ((1)
                 (set! (~ e1 'kind) 0)
                 (set! (~ e1 'worm) (~ e1 'worm1))
                 (worm-convert (~ e1 'worm2) (~ e1 'worm1))))
              (set! w1 (~ e1 'worm))
              (cond
               ((<= (~ e1 'count2) 5)
                ;; 新しい目標を設定
                (worm-set-goal w1 (randint minx maxx) (randint (- *ht/2*) *ht/2*)))
               (else
                ;; 画面下に逃げる
                (worm-set-goal w1 (randint minx maxx) (- miny 10)))))
            ;; 座標の範囲チェック
            (if (not (<= (~ e1 'miny) (~ w1 'ayvec 0) (~ e1 'maxy)))
              ;; 範囲外なら未使用にする
              (set! (~ e1 'useflag) #f))))
         (else
          ;; やられ処理
          (inc! (~ e1 'state))
          (if (>= (~ e1 'state) (if (~ e1 'boss) 50 30))
            ;; やられ終了なら未使用にする
            (set! (~ e1 'useflag) #f))))))
   enemies))

;; 敵の当たり判定
(define (hit-enemies? enemies)
  (let ((ret #f)
        (x1  (+ *x* (* *chw* -1.3) *waku*))
        (y1  (+ *y* (* *chh* -2.0) *waku*))
        (wd1 (- (* *chw* 2.6) (* *waku* 2)))
        (ht1 (- (* *chh* 1.0) (* *waku* 2))))
    (for-each
     (lambda (e1)
       (if (and (~ e1 'useflag) (< (~ e1 'state) 10))
         ;; ワームの全関節をチェックする
         (let1 w1 (~ e1 'worm)
           (for-each
            (lambda (ax ay ar)
              ;; (円に内接する正方形のサイズで判定)
              (let1 ar1 (* ar 0.7)
                (if (recthit? x1 y1 wd1 ht1 (- ax ar1) (- ay ar1) (* ar1 2) (* ar1 2))
                  (set! ret #t))))
            (~ w1 'axvec) (~ w1 'ayvec) (~ w1 'arvec)))))
     enemies)
    (if (and ret (not *demoflag*)) (auddata-play *adata-end1*))
    ret))

;; 自機ビームの表示
(define (disp-beam)
  ;(gl-color 1.0 1.0 0.0 1.0)
  ;(textscrn-cls  *tscrn-beam1*)
  ;(textscrn-line *tscrn-beam1* 0 0 0 (- *bc* 1) "c ")
  ;(textscrn-disp *tscrn-beam1* (win-x *win* *x*) (win-y *win* (+ *y* (* *chh* *bc*)))
  ;               *width* *height* (win-w *win* *chw*) (win-h *win* *chh*) 'center)
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 0.0 1.0))
  (gl-push-matrix)
  (gl-translate *x* (+ *y* (* *chh* *bc*)) 0)
  (cylinder (* *chw* 0.4) (* *chh* (- *bc* 0.2) 0.5) 20)
  (gl-pop-matrix)
  )

;; 自機ビームの当たり判定
(define (hit-beam?)
  (let ((ret   #f)
        (x1    (- *x* (* *chw* 0.5)))
        (y1    *y*)
        (wd1   (* *chw* 1.0))
        (ht1   (- *ht/2* *y*))
        (minby *ht/2*)
        (e2    #f)
        (index 0))
    (for-each
     (lambda (e1)
       (if (and (~ e1 'useflag) (< (~ e1 'state) 10))
         ;; ワームの全関節をチェックする
         (let1 w1 (~ e1 'worm)
           (for-each-with-index
            (lambda (i ax ay ar)
              (let ((ax1 (- ax ar))
                    (ay1 (- ay ar)))
                (when (and (< ay1 minby)
                           (recthit? x1 y1 wd1 ht1 ax1 ay1 (* ar 2) (* ar 2)))
                  (set! e2 e1)
                  (set! index i)
                  (set! minby ay1))))
            (~ w1 'axvec) (~ w1 'ayvec) (~ w1 'arvec)))))
     *enemies*)
    ;; ワームの先端のみダメージを与えられる
    (when (and e2 (= index 0))
      (set! ret #t)
      (set! (~ e2 'state) 1)
      (dec! (~ e2 'life))
      (when (<= (~ e2 'life) 0)
        (set! (~ e2 'state) 10)
        (when (not *demoflag*)
          (set! *sc* (+ *sc* (if (~ e2 'boss) 1000 500)))
          (auddata-play *adata-hit1*))))
    (set! *bc* (max (floor->exact (/. (- minby *y*) *chh*)) 1))
    ret))

;; 自機に最も近い敵の情報を取得する(デモ用)
;;   ・戻り値は (距離の2乗 敵 index) というリストを返す
;;     (ここで index は、ワームの関節の番号)
;;   ・有効な敵が存在しないときは (1000000 #f 0) というリストを返す
;;   ・front-only が #t のときは、ワームの先端のみチェックする
(define (get-nearest-enemy x y enemies :optional (front-only #f))
  (let1 ret '(1000000 #f 0)
    (for-each
     (lambda (e1)
       (if (and (~ e1 'useflag) (< (~ e1 'state) 10))
         ;; ワームの全関節をチェックする
         ;; (ただし、front-only が #t のときは、ワームの先端のみチェックする)
         (let1 w1 (~ e1 'worm)
           (for-each-with-index
            (lambda (i ax ay)
              (unless (and front-only (> i 0))
                (let* ((xdiff (- ax x))
                       (ydiff (- ay (- y (* *chh* 1.5))))
                       (rr    (+ (* xdiff xdiff) (* ydiff ydiff))))
                  (if (< rr (car ret))
                    (set! ret (list rr e1 i))))))
            (~ w1 'axvec) (~ w1 'ayvec)))))
     enemies)
    ret))

;; 自機の敵への攻撃チェック(デモ用)
(define (attack-enemies?)
  (let1 ret #f
    (for-each
     (lambda (e1)
       (if (and (~ e1 'useflag) (< (~ e1 'state) 10))
         ;; ワームの先端のみチェックする
         (let* ((w1 (~ e1 'worm))
                (x1 (- (~ w1 'axvec 0) (~ w1 'arvec 0) *chw*))
                (x2 (+ (~ w1 'axvec 0) (~ w1 'arvec 0) *chw*))
                (y1 (~ w1 'ayvec 0)))
           (if (and (<= x1 *x* x2) (<= (- *ht/2*) y1 *ht/2*))
             (set! ret #t)))))
     *enemies*)
    ret))

;; 枠線表示
(define (draw-border)
  (let ((r1 10))
    ;; 色
    (gl-material GL_FRONT GL_DIFFUSE #f32(0.0 0.5 0.5 1.0))
    ;; 左辺
    (gl-push-matrix)
    (gl-translate (- *wd/2*) *ht/2*     0)
    (cylinder r1 *ht/2* r1)
    (gl-pop-matrix)
    ;; 右辺
    (gl-push-matrix)
    (gl-translate    *wd/2*  *ht/2*     0)
    (cylinder r1 *ht/2* r1)
    (gl-pop-matrix)
    ;; 上辺
    (gl-push-matrix)
    (gl-translate (- *wd/2*) *ht/2*     0)
    (gl-rotate  90  0 0 1)
    (cylinder r1 *wd/2* r1)
    (gl-pop-matrix)
    ;; 下辺
    (gl-push-matrix)
    (gl-translate (- *wd/2*) (- *ht/2*) 0)
    (gl-rotate  90  0 0 1)
    (cylinder r1 *wd/2* r1)
    (gl-pop-matrix)
    ))


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 光源設定
  ;(gl-light  GL_LIGHT0 GL_POSITION #f32(1.0 1.0 1.0 0.0))
  (gl-light  GL_LIGHT0 GL_POSITION #f32(-1.0 1.0 1.0 0.0))
  (gl-light  GL_LIGHT0 GL_AMBIENT  #f32( 0.5 0.5 0.5 1.0)) ; 環境光
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_NORMALIZE)
  ;; 材質設定
  (gl-material GL_FRONT GL_SPECULAR #f32(1.0 1.0 1.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 30.0)
  (gl-material GL_FRONT GL_AMBIENT #f32(0.5 0.5 0.5 1.0))
  ;; 透過設定
  (gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (gl-enable GL_BLEND)
  ;; 音楽データの初期化
  (init-auddata *app-dpath*))

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 文字表示
  (let ((str1 "") (str2 "") (str3 "") (str4 "") (str5 "") (str6 "") (str7 "")
        (y2 49) (z1 0.51))
    (cond
     ;; デモのとき
     (*demoflag*
      (set! str1 "== Demo ==")
      (set! str2 "HIT [D] KEY")
      (set! str6 (format "TIME=(MAX=~D, MIN=~D, AVG=~D, NOW=~D)"
                         *demotmax*  *demotmin*  *demotavg*
                         (round-n (* *ssc* *wait* 0.001) 1)))
      (set! str7 (format "PARAM=(~D, ~D) COUNT=~D"
                         (~ *dparam* 'p1) (~ *dparam* 'p2) *democount*)))
     ;; デモでないとき
     (else
      ;; シーン情報で場合分け
      (case *scene*
        ((0) ; スタート画面
         (set! str1 "== SHOOTING ==")
         (set! str2 "HIT [S] KEY")
         (set! y2 50))
        ((1) ; プレイ中
         )
        ((2) ; プレイ終了
         (set! str1 "GAME OVER")
         (if (timewait-finished? *twinfo*) (set! str2 "HIT [D] KEY")))
        ))
     )
    (set! str3 (format "SCORE : ~D"    *sc*))
    (set! str4 (format "HI-SCORE : ~D" *hs*))
    (set! str5 (format "LEVEL : ~D/~D" *mr* *mmr*))
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text-over str1 (win-w-r *win* 1/2) (win-h-r *win* 36 100) *width* *height*
                           (win-h-r *win* 1/13) 'center z1 #f *backcolor*)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text-over str2 (+ (win-w-r *win* 1/2) (win-h-r *win* 1/100)) (win-h-r *win* y2 100) *width* *height*
                           (win-h-r *win* 1/18) 'center z1 #f *backcolor*)
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text str3 0 0 *width* *height* (win-h-r *win* 1/22) 'left z1)
    (gl-color 1.0 0.0 1.0 1.0)
    (draw-stroke-text str4 (win-w-r *win* 1/2) 0 *width* *height* (win-h-r *win* 1/22) 'center z1)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str5 *width* 0 *width* *height* (win-h-r *win* 1/22) 'right z1)
    (gl-color 0.0 1.0 0.0 1.0)
    (draw-stroke-text str6 0 (win-h-r *win*  5/100) *width* *height* (win-h-r *win* 1/22) 'left z1)
    (draw-stroke-text str7 0 (win-h-r *win* 10/100) *width* *height* (win-h-r *win* 1/22) 'left z1)
    )
  ;; 背景の表示
  (gl-color *backcolor*)
  (draw-win-rect 0 0 *width* *height* *width* *height* 'left -0.99999)
  ;; 画面上部(スコア表示領域)のマスク
  ;(gl-color *backcolor*)
  ;(draw-win-rect 0 0 *width* (win-h *win* *chh*) *width* *height* 'left 0.5)
  ;; 枠線表示
  (draw-border)
  ;; 敵の表示
  (disp-enemies *enemies*)
  ;; 自機ビームの表示
  (if (> *bc* 0) (disp-beam))
  ;; 自機の表示
  (if (= *scene* 2)
    (disp-mychr 1)
    (disp-mychr 0))
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  ;; ＜考え方＞
  ;;   カメラの位置は、3点(0,0,z1)(0,-y1,0)(0,y1,0)を通る円周上とする。
  ;;   こうすることで、円周角の定理により、vangle を一定にすることができる。
  ;;
  ;; ＜変数説明＞
  ;;   y1       スクリーン(XY平面上)の上端のY座標(下端のY座標は-y1になる)
  ;;   z1       スクリーン(XY平面上)を真上(Z軸上)から見る場合のZ座標(視野角によって決まる)
  ;;   vangle/2 視野角/2(rad)
  ;;   vcamera  カメラの視線とスクリーン(XY平面上)との角度(rad)
  ;;   b2       2次方程式の解の計算用
  ;;   r2       カメラと原点の距離
  ;;   y2       カメラのY座標
  ;;   z2       カメラのZ座標
  ;;   v3       カメラと点(0,-y1,0)を結ぶ直線と、スクリーン(XY平面上)の法線との角度(rad)
  ;;   y3       カメラの視線とスクリーン(XY平面上)との交点のY座標
  ;;   r-near   カメラから「点(0,-y1,0)と点(0,y1,0)のうちカメラと近い方」までの距離
  ;;   r-far    カメラから「点(0,-y1,0)と点(0,y1,0)のうちカメラと遠い方」までの距離
  ;;   z-near   カメラの視線上における、カメラから「表示領域の近い側の端」までの距離
  ;;   z-far    カメラの視線上における、カメラから「表示領域の遠い側の端」までの距離
  ;;   d1/d2    スクリーン(XY平面上)を真上(Z軸上)から見る場合と、カメラ位置から見る場合の、
  ;;            視点から「点(0,-y1,0)と点(0,y1,0)のうちカメラと近い方」までの距離の比
  ;;
  (let* ((y1       *ht/2*)
         (z1       (/. *ht/2* *tanvan*))
         (vangle/2 (/. (* *vangle*  pi/180) 2))
         (vcamera  (* *vcamera* pi/180))
         (b2       (/. (* (- (* z1 z1) (* y1 y1)) (sin vcamera))
                       (* 2 z1)))
         (r2       (+ b2 (sqrt (+ (* b2 b2) (* y1 y1)))))
         (y2       (- (* r2 (cos vcamera))))
         (z2       (* r2 (sin vcamera)))
         (v3       (atan (/. (- (+ y1 y2)) z2)))
         (y3       (+ y2 (* (tan (+ v3 vangle/2)) z2)))
         (r-near   (sqrt (+ (* (- y1 (abs y2)) (- y1 (abs y2))) (* z2 z2))))
         (r-far    (sqrt (+ (* (+ y1 (abs y2)) (+ y1 (abs y2))) (* z2 z2))))
         (z-near   (max 1 (- (* r-near (cos vangle/2)) *zd/2*)))
         (z-far           (+ (* r-far  (cos vangle/2)) *zd/2*))
         (d1/d2    (/. (sqrt (+ (* y1 y1) (* z1 z1))) r-near))
         )
    ;; 表示サイズ取得
    (set! *aratio* (* (/. *wd/2* *ht/2*) d1/d2))
    (set! *width*  (min w (truncate->exact (*  h *aratio*))))
    (set! *height* (min h (truncate->exact (/. w *aratio*))))
    (win-update-size *win* *width* *height*)
    ;; 縦横比を変えずにリサイズ
    (gl-viewport (quotient (- w *width*) 2) (quotient (- h *height*) 2) *width* *height*)
    (gl-matrix-mode GL_PROJECTION)
    (gl-load-identity)
    ;; 透視射影する範囲を設定
    ;(glu-perspective *vangle* (/. *width* *height*) (- z1 *zd/2*) (+ z1 *zd/2*))
    (glu-perspective *vangle* (/. *width* *height*) z-near z-far)
    ;; 視点の位置と方向を設定
    ;(glu-look-at 0 0 z1 0 0 0 0 1 0)
    (glu-look-at 0 y2 z2 0 y3 0 0 z2 (- y3 y2))
    ))

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
       (reset/init! *x*)
       (reset/init! *y*)
       (reset/init! *bc*)
       (reset/init! *mr*)
       (reset/init! *mmr*)
       (reset/init! *life*)
       (set! *bossflag*  #f)
       ;(set! *sc*        0)
       (set! *ssc*       0)
       (init-enemies *enemies*)
       (set! *demotime1* 0)
       (set! *demotime2* 0)
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
                     (set! *sc*       0)
                     (auddata-play *adata-start1*)
                     (keywait-clear  *kwinfo*)
                     (timewait-clear *twinfo*)))
         ;; 時間待ち(タイムアップでデモへ移行)
         (timewait *twinfo* 5000
                   (lambda ()
                     (set! *scene*    1)
                     (set! *demoflag* #t)
                     (waitcalc-set-wait *wcinfo* (if (> *demomode* 0) 1 *wait*))
                     (keywait-clear  *kwinfo*)
                     (timewait-clear *twinfo*))))
        )
       )
      ((1) ; プレイ中
       ;; スコアと制御カウンタの処理等
       (if (not *demoflag*) (inc! *sc*))
       (if (> *sc* 1000000) (set! *sc* 1000000))
       (if (> *sc* *hs*)    (set! *hs* *sc*))
       (inc! *ssc*)
       (if (> *ssc* 1000000) (set! *ssc* 1))
       (when (= (modulo *ssc* 200) 0)
         (inc! *mr*)
         (when (> *mr* *mmr*)
           (set! *mr* 0)
           (set! *mmr* (min (+ *mmr* 2) *mmmr*))
           (set! *life* (min (+ *life* 10) *maxlife*))))
       ;; 敵の生成
       (if (= (modulo *ssc* 2000) 0) (set! *bossflag* #t))
       (if (= (modulo *ssc* 50) 0) (make-enemies *enemies*))
       ;; 敵の移動
       (move-enemies *enemies*)
       ;; 自機の移動
       (move-mychr)
       ;; 自機ビーム処理
       (if (> *bc* 0) (hit-beam?))
       ;; 敵の当たり判定
       (if (hit-enemies? *enemies*) (set! *scene* 2))
       ;; デモを抜けるチェック
       (when (and *demoflag* (key-on? *ksinfo* '(#\d #\D)))
         (set! *scene*    0)
         (set! *demoflag* #f)
         (waitcalc-set-wait *wcinfo* *wait*))
       )
      ((2) ; プレイ終了
       (cond
        ;; デモのとき
        (*demoflag*
         (when (= *demotime1* 0)
           ;; デモの各種データを更新
           (inc! *democount*)
           (set! *demosscsum* (+ *demosscsum* *ssc*))
           (let ((t    (round-n (* *ssc* *wait* 0.001) 1))
                 (tavg (round-n (* (/. *demosscsum* *democount*) *wait* 0.001) 1)))
             (set! *demotmax* (max *demotmax* t))
             (set! *demotmin* (if (= *democount* 1) t (min *demotmin* t)))
             (set! *demotavg* tavg)
             ;; デモ用パラメータの自動調整機能
             ;; (パラメータを乱数で少しだけ変化させる。結果が悪ければ元に戻す)
             (if (>= t tavg)
               (demoparam-copy *dparam* *dparam-old*)
               (demoparam-copy *dparam-old* *dparam*))
             (set! (~ *dparam* 'p1)
                   (clamp (round-n (+ (~ *dparam* 'p1) (* (randint -1 1) 0.1)) 1)
                          3 50))
             (set! (~ *dparam* 'p2)
                   (clamp (+ (~ *dparam* 'p2) (randint -1 1))
                          1 100))
             ))
         (set! *demotime1* (+ *demotime1* *wait*))
         (if (>= *demotime1* 1600) (set! *scene* 0))
         ;; デモを抜けるチェック
         (when (key-on? *ksinfo* '(#\d #\D))
           (set! *scene*    0)
           (set! *demoflag* #f)
           (waitcalc-set-wait *wcinfo* *wait*)))
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
  (set! *demomode* (x->integer (list-ref args 2 0)))
  ;(set! *flatdisp* (x->integer (list-ref args 3 0)))
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

