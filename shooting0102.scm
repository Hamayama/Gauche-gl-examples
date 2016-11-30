;; -*- coding: utf-8 -*-
;;
;; shooting0102.scm
;; 2016-12-1 v1.06
;;
;; ＜内容＞
;;   Gauche-gl を使用した、簡単なシューティングゲームです。
;;   矢印キーで上下左右移動。
;;   [Ctrl]/[Space]/[a]/[z]キーのいずれかでビーム発射です(押し続けると発射し続けます)。
;;   敵を破壊するには、コアの部分にビームを当てる必要があります。
;;   また、敵を破壊すると一定範囲が誘爆します。
;;   画面右上のレベル表示は、出現する敵の数と速度の目安になります。
;;   また、スタート画面でしばらく待つとデモになります。
;;   ESCキーを押すと終了します。
;;
(add-load-path "." :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use gauche.sequence)
(use math.const)
(use glmintool)
(use gltextscrn)
(use alaudplay)

(define *wait*      20) ; ウェイト(msec)
(define *title* "shooting0102") ; ウィンドウのタイトル
(define *width*    600) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)
(define *aratio*     (/. *width* *height*)) ; アスペクト比(計算用)

(define *wd/2*     500) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *chw*       16) ; 文字の幅
(define *chh*       16) ; 文字の高さ
(define *x*          (- (/. (* *wd/2* 3) 4))) ; 自機のX座標
(define *y*          0) ; 自機のY座標
(define *v*         10) ; 自機の速度
(define *maxx*       (- *wd/2* *chw*))       ; 自機のX座標最大値
(define *minx*       (- *maxx*))             ; 自機のX座標最小値
(define *maxy*       (- *ht/2* (* *chh* 2))) ; 自機のY座標最大値
(define *miny*       (- *maxy*))             ; 自機のY座標最小値
(define *bc*         0) ; 自機ビームカウンタ
(define *bsize*    300) ; 爆風のサイズ
(define *waku*       5) ; 当たり判定調整用
(define *mr*         1) ; 敵の数
(define *mmr*       10) ; 敵の最大数
(define *mmmr*      30) ; 敵の最大数の最大数
(define *rsize*     17) ; 敵の大きさ(単位:文字)
(define *sc*         0) ; スコア
(define *hs*         0) ; ハイスコア
(define *ssc*        0) ; 制御カウンタ
(define *scene*      0) ; シーン情報(=0:スタート画面,=1:プレイ中,=2:プレイ終了)
(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色

(define *demomode*   0) ; デモモード(=0:通常デモ,=1:高速デモ)
(define *demoflg*    #f) ; デモフラグ
(define *demotime1*  0) ; デモ時間調整用1(msec)
(define *demotime2*  0) ; デモ時間調整用2(msec)
(define *democount*  0) ; デモ回数
(define *demosscsum* 0) ; デモ生存カウンタ累積
(define *demotmax* 0.0) ; デモ生存時間最大値
(define *demotmin* 0.0) ; デモ生存時間最小値
(define *demotavg* 0.0) ; デモ生存時間平均値

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

;; 音楽データクラスのインスタンス生成
(define *adata-start* (make <auddata>))
(define *adata-hit*   (make <auddata>))
(define *adata-end*   (make <auddata>))

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
(textscrn-init *tscrn-mychr1* 4 2)
(textscrn-pset *tscrn-mychr1* 0 0 " LL ")
(textscrn-pset *tscrn-mychr1* 0 1 " OO>")
(define *tscrn-mychr2* (make <textscrn>))
(textscrn-init *tscrn-mychr2* 4 2)
(textscrn-pset *tscrn-mychr2* 0 0 " @@ ")
(textscrn-pset *tscrn-mychr2* 0 1 " @@@")
;; (自機ビーム)
(define *tscrn-beam1* (make <textscrn>))
(textscrn-init *tscrn-beam1* (floor->exact (/. (* *wd/2* 2) *chw*)) 1)
;; (敵)
;(define *tscrn-enemy1* (make <textscrn>))
;(textscrn-init *tscrn-enemy1* *rsize* *rsize*)
;; (敵ミサイル)
(define *tscrn-missile1* (make <textscrn>))
(textscrn-init *tscrn-missile1* 1 1)
(textscrn-pset *tscrn-missile1* 0 0 "O")

;; 敵クラス
(define-class <enemy> ()
  ((useflag :init-value #f) ; 使用フラグ
   (kind    :init-value 0)  ; 種別(=0:敵,=1000:敵ミサイル)
   (state   :init-value 0)  ; 状態(=0:通常,=1-10:爆発)
   (life    :init-value 0)  ; 耐久力
   (x       :init-value 0)  ; X座標
   (y       :init-value 0)  ; Y座標
   (degree  :init-value 0)  ; 角度(度)
   (speed   :init-value 0)  ; 速度
   (tscrn   :init-value #f) ; 表示キャラクター(<textscrn>)
   (hitstr  :init-value "") ; 当たり判定文字列
   (minx    :init-value 0)  ; X座標の最小値
   (maxx    :init-value 0)  ; X座標の最大値
   (miny    :init-value 0)  ; Y座標の最小値
   (maxy    :init-value 0)  ; Y座標の最大値
   ))
;; 敵クラスのインスタンス生成
(define *enemies*  (make-vector-of-class *mmmr* <enemy>))
(define *missiles* (make-vector-of-class *mmmr* <enemy>))
(for-each
 (lambda (e1)
   (set! (~ e1 'tscrn) (make <textscrn>))
   (textscrn-init (~ e1 'tscrn) *rsize* *rsize*))
 *enemies*)

;; デモ用パラメータクラス
(define-class <demoparam> ()
  ((p1 :init-value 8.0) ; デモ用パラメータ1(敵との距離のしきい値)
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
  (let1 tscrn #f
    (case pattern
      ((0)  (gl-color 1.0 1.0 0.0 1.0)
            (set! tscrn *tscrn-mychr1*))
      (else (gl-color 1.0 0.0 0.0 1.0)
            (set! tscrn *tscrn-mychr2*)))
    (textscrn-disp tscrn (win-x *win* *x*) (win-y *win* *y*) *width* *height*
                   (win-w *win* *chw*) (win-h *win* *chh*) 'center)
    ))

;; 自機の移動
(define (move-mychr)
  (cond
   ;; デモのとき
   (*demoflg*
    ;; 自機の移動
    (let* ((nes (get-near-enemies 1))
           (rr1 (car (~ nes 0)))
           (e1  (cdr (~ nes 0)))
           (vy  0))
      (cond
       ;; 一番近い敵/敵ミサイルを避ける
       ((and e1 (< rr1 (* *chh* *chh* (~ *dparam* 'p1) (~ *dparam* 'p1))))
        (set! vy (if (< *y* (~ e1 'y)) (- *v*) *v*)))
       ;; 中央に戻る
       ((<= (randint 1 100) (~ *dparam* 'p2))
        (if (<= *y* (- *v*)) (set! vy    *v*))
        (if (>= *y*    *v*)  (set! vy (- *v*)))))
      (set! *y* (clamp (+ *y* vy) *miny* *maxy*)))
    ;; 自機ビーム発射
    (cond
     ((attack-enemies?)
      (set! *bc* 1)
      (set! *demotime2* 0))
     ((> *bc* 0)
      (set! *bc* 1)
      (set! *demotime2* (+ *demotime2* *wait*))
      (if (>= *demotime2* 200) (set! *bc* 0))))
    )
   ;; デモでないとき
   (else
    (let ((vx 0) (vy 0))
      ;; 自機の移動
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

;; 敵/敵ミサイルの初期化
(define (init-enemies enemies)
  (for-each (lambda (e1) (set! (~ e1 'useflag) #f)) enemies))

;; 敵/敵ミサイルの生成
(define (make-enemies enemies :optional (missiles #f))
  (cond
   ((not missiles)
    ;; 敵の生成
    (let1 i (find-index (lambda (e1) (not (~ e1 'useflag))) enemies)
      (if (and i (< i *mr*))
        (let1 e1 (~ enemies i)
          (set! (~ e1 'useflag) #t)
          (set! (~ e1 'kind)    0)
          (set! (~ e1 'state)   0)
          (set! (~ e1 'life)    6)
          (set! (~ e1 'x)       (+ *wd/2* (* *chw* *rsize* 0.5)))
          (set! (~ e1 'y)       (+ (randint (- *ht/2*) *ht/2*) (* *chh* *rsize* 0.5)))
          (set! (~ e1 'degree)  (randint 150 210))
          (set! (~ e1 'speed)   (* (min (+ 4 (- *mmr* 10)) 10) 1.5 0.65))
          ;(set! (~ e1 'tscrn)   *tscrn-enemy1*)
          (set! (~ e1 'hitstr)  "#+")
          (set! (~ e1 'minx)    (- (+ *wd/2* (* *chw* *rsize* 0.5))))
          (set! (~ e1 'maxx)       (+ *wd/2* (* *chw* *rsize* 0.5)))
          (set! (~ e1 'miny)    (- *ht/2*))
          (set! (~ e1 'maxy)    (+ *ht/2* (* *chh* *rsize*)))
          (textscrn-fcircle (~ e1 'tscrn) 8 8 9 1 1 "+")
          (textscrn-fcircle (~ e1 'tscrn) 8 8 5 1 1 "#")
          ))))
   (else
    ;; 敵ミサイルの生成
    (let1 e1 (~ enemies (randint 0 (max (- *mr* 1) 0)))
      (if (and (~ e1 'useflag)
               (= (~ e1 'state) 0)
               (> (~ e1 'x) 0)
               (>= (* *ssc* *wait*) 2000))
        (let1 i (find-index (lambda (m1) (not (~ m1 'useflag))) missiles)
          (if (and i (< i (min (* *mr* 2) *mmmr*)))
            (let1 m1 (~ missiles i)
              (set! (~ m1 'useflag) #t)
              (set! (~ m1 'kind)    1000)
              (set! (~ m1 'state)   0)
              (set! (~ m1 'life)    0)
              (set! (~ m1 'x)       (+ (~ e1 'x) (* *chw* (randint -3 3))))
              (set! (~ m1 'y)       (+ (~ e1 'y) (* *chh* (randint -3 3)) (* *chh* *rsize* -0.5)))
              (set! (~ m1 'degree)  (* (atan (- (- *y* *chh*) (~ m1 'y)) (- *x* (~ m1 'x))) 180/pi))
              (set! (~ m1 'speed)   (* (min (+ 6 (- *mmr* 10)) 14) 1.5 0.7))
              (set! (~ m1 'tscrn)   *tscrn-missile1*)
              (set! (~ m1 'hitstr)  "O")
              (set! (~ m1 'minx)    (- (+ *wd/2* (* *chw* *rsize* 0.5))))
              (set! (~ m1 'maxx)       (+ *wd/2* (* *chw* *rsize* 0.5)))
              (set! (~ m1 'miny)    (- *ht/2*))
              (set! (~ m1 'maxy)    (+ *ht/2* *chh*))
              ))))))
   ))

;; 敵/敵ミサイルの表示
(define (disp-enemies enemies)
  (for-each
   (lambda (e1)
     (when (~ e1 'useflag)
       (cond
        ((< (~ e1 'kind) 1000)
         ;; 敵の表示
         ;; (削れた分)
         (gl-color 0.0 0.0 0.0 0.0)
         (textscrn-disp-drawer (~ e1 'tscrn) (win-x *win* (~ e1 'x)) (win-y *win* (~ e1 'y))
                               *width* *height* (win-w *win* *chw*) (win-h *win* *chh*) 'center)
         ;; (外周)
         (gl-color 0.7 0.7 0.7 1.0)
         (draw-win-circle (win-x *win* (~ e1 'x))
                          (win-y *win* (- (~ e1 'y) (/. (* (~ e1 'tscrn 'height) *chh*) 2)))
                          (win-w *win* (/. (* 9 *chh*) 2)) *width* *height*)
         ;; (コア)
         (gl-color 0.5 0.5 0.5 1.0)
         (draw-win-circle (win-x *win* (~ e1 'x))
                          (win-y *win* (- (~ e1 'y) (/. (* (~ e1 'tscrn 'height) *chh*) 2)))
                          (win-w *win* (/. (* 17 *chh*) 2)) *width* *height*)
         )
        (else
         ;; ミサイルの表示
         (gl-color 0.6 0.6 0.6 1.0)
         (draw-win-circle (win-x *win* (~ e1 'x))
                          (win-y *win* (- (~ e1 'y) (/. (* (~ e1 'tscrn 'height) *chh*) 2)))
                          (win-w *win* (/. (* 1 *chh*) 4)) *width* *height*)
         (gl-color 0.9 0.9 0.9 1.0)
         (draw-win-circle (win-x *win* (~ e1 'x))
                          (win-y *win* (- (~ e1 'y) (/. (* (~ e1 'tscrn 'height) *chh*) 2)))
                          (win-w *win* (/. (* 1 *chh*) 2)) *width* *height*)
         ))
       ))
   enemies))

;; 敵/敵ミサイルの移動
(define (move-enemies enemies)
  (for-each
   (lambda (e1)
     (if (~ e1 'useflag)
       (cond
        ((= (~ e1 'state) 0)
         ;; 次の座標を計算
         (set! (~ e1 'x) (+ (~ e1 'x) (* (~ e1 'speed) (%cos (* (~ e1 'degree) pi/180)))))
         (set! (~ e1 'y) (+ (~ e1 'y) (* (~ e1 'speed) (%sin (* (~ e1 'degree) pi/180)))))
         ;; 座標の範囲チェック
         (if (not (and (<= (~ e1 'minx) (~ e1 'x) (~ e1 'maxx))
                       (<= (~ e1 'miny) (~ e1 'y) (~ e1 'maxy))))
           ;; 範囲外なら未使用にする
           (set! (~ e1 'useflag) #f)))
        (else
         ;; 爆発の処理
         (inc! (~ e1 'state))
         (if (>= (~ e1 'state) 10)
           ;; 爆発終了なら未使用にする
           (set! (~ e1 'useflag) #f))))
       ))
   enemies))

;; 敵/敵ミサイルの当たり判定
(define (hit-enemies? enemies)
  (let ((ret #f)
        (x1  (win-x *win* (+ *x* (* *chw* -1.0)    *waku*)))
        (y1  (win-y *win* (+ *y*                (- *waku*))))
        (x2  (win-x *win* (+ *x* (* *chw*  1.0) (- *waku*))))
        (y2  (win-y *win* (+ *y* (* *chh* -2.0)    *waku*))))
    (for-each
     (lambda (e1)
       (if (and (~ e1 'useflag) (= (~ e1 'state) 0))
         (when (textscrn-disp-check-str
                (~ e1 'tscrn) (~ e1 'hitstr) x1 y1 x2 y2
                (win-w *win* *chw*) (win-h *win* *chh*)
                (win-x *win* (~ e1 'x)) (win-y *win* (~ e1 'y)) 'center)
           (set! ret #t))))
     enemies)
    (if (and ret (not *demoflg*)) (auddata-play *adata-end*))
    ret))

;; 自機ビームの表示
(define (disp-beam)
  (gl-color 1.0 1.0 0.0 1.0)
  (textscrn-cls  *tscrn-beam1*)
  (textscrn-line *tscrn-beam1* 0 0 (- *bc* 1) 0 "> ")
  (textscrn-disp *tscrn-beam1*
                 (win-x *win* (+ *x* *chw*))
                 (win-y *win* (+ *y* (* *chh* -1.0)))
                 *width* *height* (win-w *win* *chw*) (win-h *win* *chh*) 'left))

;; 自機ビームの当たり判定
(define (hit-beam?)
  (let ((ret      #f)
        (hit-list '())
        (x1       (win-x *win* (+ *x* *chw*             *waku*)))
        (y1       (win-y *win* (+ *y* (* *chh* -1.0) (- *waku*))))
        (x2       (win-x *win* (+ *wd/2*             (- *waku*))))
        (y2       (win-y *win* (+ *y* (* *chh* -2.0)    *waku*)))
        (minbx    (+ *wd/2* (* *chw* *rsize* 0.5)))
        (e2       #f))
    (for-each
     (lambda (e1)
       (if (and (~ e1 'useflag) (= (~ e1 'state) 0))
         (when (and (< (~ e1 'x) minbx)
                    (textscrn-disp-check-str
                     (~ e1 'tscrn) (~ e1 'hitstr) x1 y1 x2 y2
                     (win-w *win* *chw*) (win-h *win* *chh*)
                     (win-x *win* (~ e1 'x)) (win-y *win* (~ e1 'y)) 'center))
           (set! e2 e1)
           (set! minbx (~ e1 'x)))
         ))
     *enemies*)
    (when e2
      (set! ret #t)
      ;; 敵の外周かコアかをチェックする
      (set! hit-list (textscrn-disp-check-str2
                      (~ e2 'tscrn) (~ e2 'hitstr) x1 y1 x2 y2
                      (win-w *win* *chw*) (win-h *win* *chh*)
                      (win-x *win* (~ e2 'x)) (win-y *win* (~ e2 'y)) 'center 1))
      (unless (null? hit-list)
        (cond
         ;; コアのとき(破壊)
         ((eqv? (~ hit-list 0 2) (~ (~ e2 'hitstr) 0))
          (set! (~ e2 'life) 0)
          (when (<= (~ e2 'life) 0)
            (set! (~ e2 'state) 1)
            (when (not *demoflg*)
              (set! *sc* (+ *sc* 100))
              (auddata-play *adata-hit*))))
         ;; 外周のとき(削る)
         (else
          (textscrn-pset (~ e2 'tscrn) (~ hit-list 0 0) (~ hit-list 0 1) "x")
          (set! minbx (+ minbx (* *chw* (- (~ hit-list 0 0) (* *rsize* 0.5))))))
         )))
    (set! *bc* (max (- (floor->exact (/. (- minbx *x*) *chw*)) 1) 1))
    ret))

;; 爆風の表示
(define (disp-blast)
  (gl-color 1.0 1.0 0.0 0.5)
  (for-each
   (lambda (e1)
     (when (and (~ e1 'useflag) (> (~ e1 'state) 0))
       ;; (表示は半分のサイズにする)
       (draw-win-circle (win-x *win* (~ e1 'x))
                        (win-y *win* (- (~ e1 'y) (/. (* (~ e1 'tscrn 'height) *chh*) 2)))
                        (win-w *win* (/. *bsize* 2)) *width* *height* 1 1 'center 0.1)
       ))
   *enemies*))

;; 爆風の当たり判定
(define (hit-blast?)
  (let ((ret #f)
        (rr  (* *bsize* *bsize*)))
    (for-each
     (lambda (e1)
       (if (and (~ e1 'useflag) (> (~ e1 'state) 0))
         (for-each
          (lambda (e2)
            (if (and (~ e2 'useflag) (= (~ e2 'state) 0))
              (let ((xdiff (- (~ e2 'x) (~ e1 'x)))
                    (ydiff (- (~ e2 'y) (~ e1 'y))))
                (when (< (+ (* xdiff xdiff) (* ydiff ydiff)) rr)
                  (set! ret #t)
                  (dec! (~ e2 'life))
                  (when (<= (~ e2 'life) 0)
                    (set! (~ e2 'state) 1)
                    (when (not *demoflg*)
                      (set! *sc* (+ *sc* 200))
                      (auddata-play *adata-hit*)))))
              ))
          *enemies*)
         ))
     *enemies*)
    ret))

;; 自機に近い敵/敵ミサイルを、近い順にn個だけ取得する(デモ用)
;;   ・戻り値は #((距離の2乗 . 敵) ...) というベクタを返す
;;   ・有効な敵がいなければ #((1000000 . #f) ...) というベクタを返す
(define (get-near-enemies n)
  (let1 ret (make-vector n '(1000000 . #f))
    (define (%search-near-enemies enemies)
      (for-each
       (lambda (e1)
         (if (and (~ e1 'useflag) (= (~ e1 'state) 0))
           (let* ((xdiff (- (~ e1 'x) *x*))
                  (ydiff (- (~ e1 'y) *y*))
                  (rr    (+ (* xdiff xdiff) (* ydiff ydiff))))
             (when (< rr (car (~ ret (- n 1))))
               (set! (~ ret (- n 1)) (cons rr e1))
               (set! ret (sort! ret < car))))
           ))
       enemies))
    (%search-near-enemies *enemies*)
    (%search-near-enemies *missiles*)
    ;(print ret)
    ret))

;; 自機の敵への攻撃チェック(デモ用)
(define (attack-enemies?)
  (let ((ret #f)
        (y1  (- *y* (* *chh* 10)))
        (y2  (+ *y* (* *chh* 10))))
    (for-each
     (lambda (e1)
       (if (and (~ e1 'useflag) (= (~ e1 'state) 0) (<= y1 (~ e1 'y) y2))
         (set! ret #t)))
     *enemies*)
    ret))


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
  ;; 透過設定
  (gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (gl-enable GL_BLEND)
  ;; 文字-描画手続きの割り付け設定
  (set-char-drawer #\x (lambda (x y width height chw chh z)
                         (draw-win-rect x y chw chh width height 'left z)))
  ;; 音楽データの初期化
  (auddata-load-wav-file *adata-start* (make-fpath *app-dpath* "sound/appear1.wav"))
  (auddata-set-prop *adata-start* AL_GAIN  0.07)
  (auddata-set-prop *adata-start* AL_PITCH 3.0)
  (auddata-load-wav-file *adata-hit*   (make-fpath *app-dpath* "sound/decide2.wav"))
  (auddata-set-prop *adata-hit*   AL_GAIN  0.4)
  (auddata-set-prop *adata-hit*   AL_PITCH 1.1)
  (auddata-load-wav-file *adata-end*   (make-fpath *app-dpath* "sound/pattern05.wav"))
  (auddata-set-prop *adata-end*   AL_GAIN  0.2)
  (auddata-set-prop *adata-end*   AL_PITCH 1.3)
  )

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 文字表示
  (let ((str1 "") (str2 "") (str3 "") (str4 "") (str5 "") (str6 "") (str7 "") (y2 49))
    (cond
     ;; デモのとき
     (*demoflg*
      (set! str1 "== Demo ==")
      (set! str2 "HIT [D] KEY")
      (set! str6 (format "TIME=(MAX=~D, MIN=~D, AVG=~D, NOW=~D)"
                         *demotmax*  *demotmin*  *demotavg*
                         (round-n (* *ssc* *wait* 0.001) 1)))
      (set! str7 (format "PARAM=(~D, ~D) COUNT=~D"
                         (~ *dparam* 'p1) (~ *dparam* 'p2) *democount*))
      )
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
        )
      )
     )
    (set! str3 (format "SCORE : ~D"    *sc*))
    (set! str4 (format "HI-SCORE : ~D" *hs*))
    (set! str5 (format "LEVEL : ~D/~D" *mr* *mmr*))
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text-over str1 (win-w-r *win* 1/2) (win-h-r *win* 36 100) *width* *height*
                           (win-h-r *win* 1/13) 'center 0 #f *backcolor* 1.1 1.0)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text-over str2 (+ (win-w-r *win* 1/2) (win-h-r *win* 1/100))
                           (win-h-r *win* y2 100) *width* *height*
                           (win-h-r *win* 1/18) 'center 0 #f *backcolor*)
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text str3 0 0 *width* *height* (win-h-r *win* 1/22))
    (gl-color 1.0 0.0 1.0 1.0)
    (draw-stroke-text str4 (win-w-r *win* 1/2) 0 *width* *height* (win-h-r *win* 1/22) 'center)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str5 *width* 0 *width* *height* (win-h-r *win* 1/22) 'right)
    (gl-color 0.0 1.0 0.0 1.0)
    (draw-stroke-text str6 0 (win-h-r *win*  5/100) *width* *height* (win-h-r *win* 1/22))
    (draw-stroke-text str7 0 (win-h-r *win* 10/100) *width* *height* (win-h-r *win* 1/22))
    )
  ;; 背景の表示
  (gl-color *backcolor*)
  (draw-win-rect 0 0 *width* *height* *width* *height* 'left -0.1)
  ;; 画面上部(スコア表示領域)のマスク
  (gl-color *backcolor*)
  (draw-win-rect 0 0 *width* (win-h *win* (* *chh* 2)) *width* *height*)
  ;; 敵ミサイルの表示
  (disp-enemies *missiles*)
  ;; 自機ビームの表示
  (if (> *bc* 0) (disp-beam))
  ;; 自機の表示
  (if (= *scene* 2) (disp-mychr 1))
  (disp-mychr 0)
  ;; 敵の表示
  (disp-enemies *enemies*)
  ;; 爆風の表示
  (disp-blast)
  ;(gl-flush)
  (glut-swap-buffers)
  )

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  (min w (truncate->exact (*  h *aratio*))))
  (set! *height* (min h (truncate->exact (/. w *aratio*))))
  (win-update-size *win* *width* *height*)
  ;; 縦横比を変えずにリサイズ
  (gl-viewport (quotient (- w *width*) 2) (quotient (- h *height*) 2) *width* *height*)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  ;; 透視射影する範囲を設定
  (glu-perspective *vangle* (/. *width* *height*) 1 2000)
  ;; 視点の位置と方向を設定
  (glu-look-at 0 0 (/. *wd/2* *tanvan*) 0 0 0 0 1 0)
  )

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
    (when (= *scene* 0)
      (if (keywait-finished?  *kwinfo*) (timewait-clear *twinfo*))
      (if (timewait-finished? *twinfo*) (keywait-clear  *kwinfo*)))
    )
   ;; 待ち状態でないとき
   (else
    ;; シーン情報で場合分け
    (case *scene*
      ((0) ; スタート画面
       ;; 初期化
       (set! *x*     (- (/. (* *wd/2* 3) 4)))
       (set! *y*     0)
       (set! *bc*    0)
       (set! *mr*    1)
       (set! *mmr*  10)
       ;(set! *sc*    0)
       (set! *ssc*   0)
       (init-enemies *enemies*)
       (init-enemies *missiles*)
       (set! *demotime1* 0)
       (set! *demotime2* 0)
       (cond
        ;; デモのとき
        (*demoflg*
         (set! *scene* 1))
        ;; デモでないとき
        (else
         ;; キー入力待ち
         (keywait  *kwinfo* '(#\s #\S)
                   (lambda ()
                     (set! *scene*   1)
                     (set! *sc*      0)
                     (auddata-play *adata-start*)
                     ))
         ;; 時間待ち(タイムアップでデモへ移行)
         (timewait *twinfo* 5000
                   (lambda ()
                     (set! *scene*   1)
                     (set! *demoflg* #t)
                     (waitcalc-set-wait *wcinfo* (if (> *demomode* 0) 1 *wait*))
                     ))
         (when (= *scene* 1)
           (keywait-clear  *kwinfo*)
           (timewait-clear *twinfo*))
         )
        )
       )
      ((1) ; プレイ中
       ;; スコアと制御カウンタの処理等
       (if (not *demoflg*) (inc! *sc*))
       (if (> *sc* 1000000) (set! *sc* 1000000))
       (if (> *sc* *hs*)    (set! *hs* *sc*))
       (inc! *ssc*)
       (if (> *ssc* 1000000) (set! *ssc* 1))
       (when (= (modulo *ssc* 50) 0)
         (inc! *mr*)
         (when (> *mr* *mmr*)
           (set! *mr* 0)
           (set! *mmr* (min (+ *mmr* 2) *mmmr*))
           ))
       ;; 敵の生成
       (if (= (modulo *ssc* 12) 0) (make-enemies *enemies*))
       ;; 敵の移動
       (move-enemies *enemies*)
       ;; 敵ミサイルの生成
       (if (and (= (modulo *ssc* 3) 0) (< (randint 0 100) 50))
         (make-enemies *enemies* *missiles*))
       ;; 敵ミサイルの移動
       (move-enemies *missiles*)
       ;; 自機の移動
       (move-mychr)
       ;; 自機ビーム処理
       (if (= *bc* 1) (hit-beam?))
       ;; 爆風の当たり判定
       (hit-blast?)
       ;; 敵の当たり判定
       (if (hit-enemies? *enemies*) (set! *scene* 2))
       ;; 敵ミサイルの当たり判定
       (if (hit-enemies? *missiles*) (set! *scene* 2))
       ;; デモを抜けるチェック
       (when (and *demoflg* (key-on? *ksinfo* '(#\d #\D)))
         (set! *scene*   0)
         (set! *demoflg* #f)
         (waitcalc-set-wait *wcinfo* *wait*))
       )
      ((2) ; プレイ終了
       (cond
        ;; デモのとき
        (*demoflg*
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
                          5 50))
             (set! (~ *dparam* 'p2)
                   (clamp (+ (~ *dparam* 'p2) (randint -1 1))
                          1 100))
             ))
         (set! *demotime1* (+ *demotime1* *wait*))
         (if (>= *demotime1* 1600) (set! *scene* 0))
         ;; デモを抜けるチェック
         (when (key-on? *ksinfo* '(#\d #\D))
           (set! *scene*   0)
           (set! *demoflg* #f)
           (waitcalc-set-wait *wcinfo* *wait*))
         )
        ;; デモでないとき
        (else
         ;; 時間待ち
         (timewait *twinfo* 1500
                   (lambda ()
                     ;; キー入力待ち
                     (keywait *kwinfo* '(#\d #\D)
                              (lambda ()
                                (set! *scene* 0)
                                (timewait-clear *twinfo*)
                                (keywait-clear  *kwinfo*)))))
         ))
       )
      )
    )
   )
  ;; 画面表示
  (glut-post-redisplay)
  ;; ウェイト時間調整
  (glut-timer-func (waitcalc *wcinfo*) timer 0)
  )

;; 終了
(define (exit-main-loop code)
  (aud-end)
  (exit code))

;; メイン処理
(define (main args)
  (aud-init (> (x->integer (list-ref args 1 0)) 0))
  (set! *demomode* (x->integer (list-ref args 2 0)))
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
  ;; コールバック内エラー対策
  (guard (ex (else (report-error ex) (exit-main-loop 1)))
    (glut-main-loop))
  0)

