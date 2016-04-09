;; -*- coding: utf-8 -*-
;;
;; shooting.scm
;; 2016-4-9 v1.16
;;
;; ＜内容＞
;;   Gauche-gl を使用した、簡単なシューティングゲームです。
;;   矢印キーで上下左右移動。
;;   [Ctrl]/[Space]/[a]/[z]キーのいずれかでビーム発射です(押し続けると発射し続けます)。
;;   敵は若干固いので、しばらくビームを当て続ける必要があります。
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
(use data.heap)
(use glmintool)
(use gltextscrn)
(use alaudplay)

(define *wait*      20) ; ウェイト(msec)
(define *title* "shooting") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)
(define *keystate*   (make-hash-table 'eqv?)) ; キー入力状態(ハッシュテーブル)
(define *spkeystate* (make-hash-table 'eqv?)) ; 特殊キー入力状態(ハッシュテーブル)
(define *mdkeystate* (make-hash-table 'eqv?)) ; Shift,Ctrl,Altキー入力状態(ハッシュテーブル)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *chw*       16) ; 文字の幅
(define *chh*       32) ; 文字の高さ
(define *x*          0) ; 自機のX座標
(define *y*       -240) ; 自機のY座標
(define *maxx*       (- *wd/2* (* *chw* 1.5)))   ; 自機のX座標最大値
(define *minx*       (- *maxx*))                 ; 自機のX座標最小値
(define *maxy*       (- *ht/2* *chh*))           ; 自機のY座標最大値
(define *miny*       (+ (- *ht/2*) (* *chh* 2))) ; 自機のY座標最小値
(define *bc*         0) ; 自機ビームカウンタ
(define *bs*       240) ; 爆風のサイズ
(define *waku*       4) ; 当たり判定調整用
(define *mr*         1) ; 敵の数
(define *mmr*       10) ; 敵の最大数
(define *mmmr*      30) ; 敵の最大数の最大数
(define *sc*         0) ; スコア
(define *hs*         0) ; ハイスコア
(define *ssc*        0) ; 制御カウンタ
(define *scene*      0) ; シーン情報(=0:スタート画面,=1:プレイ中,=2:プレイ終了)
(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色

(define *demoflg*    #f) ; デモフラグ
(define *demotime1*  0) ; デモ時間調整用1(msec)
(define *demotime2*  0) ; デモ時間調整用2(msec)
(define *democount*  0) ; デモ回数
(define *demosscsum* 0) ; デモ生存カウンタ累積
(define *demotmax* 0.0) ; デモ生存時間最大値
(define *demotmin* 0.0) ; デモ生存時間最小値
(define *demotavg* 0.0) ; デモ生存時間平均値


;; 音楽データクラスのインスタンス生成
(define *adata-start* (make <auddata>))
(define *adata-hit*   (make <auddata>))
(define *adata-end*   (make <auddata>))

;; キー入力待ちクラスのインスタンス生成
(define *kwinfo* (make <keywaitinfo> :keystate *keystate*))

;; 時間待ちクラスのインスタンス生成
(define *twinfo* (make <timewaitinfo> :waitinterval *wait*))

;; ウェイト時間調整クラスのインスタンス生成
(define *wtinfo* (make <waitmsecinfo> :waittime *wait*))

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
(textscrn-init *tscrn-beam1* 1 25)
;; (敵)
(define *tscrn-enemy1* (make <textscrn>))
(textscrn-init *tscrn-enemy1* 5 3)
(textscrn-pset *tscrn-enemy1* 0 0 " === ")
(textscrn-pset *tscrn-enemy1* 0 1 "=RRR=")
(textscrn-pset *tscrn-enemy1* 0 2 " === ")
;; (敵ミサイル)
(define *tscrn-missile1* (make <textscrn>))
(textscrn-init *tscrn-missile1* 1 2)
(textscrn-pset *tscrn-missile1* 0 0 "|")
(textscrn-pset *tscrn-missile1* 0 1 "V")

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
(define *enemies*  (make-vector *mmmr* #f))
(do ((i 0 (+ i 1))) ((>= i (vector-length *enemies* )) #f)
  (set! (~ *enemies*  i) (make <enemy>)))
(define *missiles* (make-vector *mmmr* #f))
(do ((i 0 (+ i 1))) ((>= i (vector-length *missiles*)) #f)
  (set! (~ *missiles* i) (make <enemy>)))

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


;; ウィンドウ上のX座標を取得
(define (get-win-x x)
  (+ (/. (* x *width*) (* *wd/2* 2))
     (/. *width* 2)))

;; ウィンドウ上のY座標を取得
(define (get-win-y y)
  (+ (/. (* (- y) *height*) (* *ht/2* 2))
     (/. *height* 2)))

;; ウィンドウ上の幅を取得
(define (get-win-w w)
  (/. (* w *width*) (* *wd/2* 2)))

;; ウィンドウ上の高さを取得
(define (get-win-h h)
  (/. (* h *height*) (* *ht/2* 2)))

;; 自機の表示
(define (disp-mychr pattern)
  (let1 tscrn #f
    (case pattern
      ((0)  (gl-color 1.0 1.0 0.0 1.0)
            (set! tscrn *tscrn-mychr1*))
      (else (gl-color 1.0 0.0 0.0 1.0)
            (set! tscrn *tscrn-mychr2*)))
    (textscrn-disp tscrn (get-win-x *x*) (get-win-y *y*)
                   *width* *height* (get-win-w *chw*) (get-win-h *chh*) 'center)
    ))

;; 自機ビームの表示
(define (disp-beam)
  (gl-color 1.0 1.0 0.0 1.0)
  (textscrn-cls  *tscrn-beam1*)
  (textscrn-line *tscrn-beam1* 0 0 0 (- *bc* 1) "c ")
  (textscrn-disp *tscrn-beam1* (get-win-x *x*) (get-win-y (+ *y* (* *chh* *bc*)))
                 *width* *height* (get-win-w *chw*) (get-win-h *chh*) 'center))

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
          (set! (~ e1 'x)       (randint (- *wd/2*) *wd/2*))
          (set! (~ e1 'y)       *ht/2*)
          (set! (~ e1 'degree)  (randint -60 -120))
          (set! (~ e1 'speed)   (min (+ 6 (- *mmr* 10)) 14))
          (set! (~ e1 'tscrn)   *tscrn-enemy1*)
          (set! (~ e1 'hitstr)  "R")
          (set! (~ e1 'minx)    (- *wd/2*))
          (set! (~ e1 'maxx)    *wd/2*)
          (set! (~ e1 'miny)    (- *ht/2*))
          (set! (~ e1 'maxy)    *ht/2*)
          ))))
   (else
    ;; 敵ミサイルの生成
    (let1 e1 (~ enemies (randint 0 (max (- *mr* 1) 0)))
      (if (and (~ e1 'useflag)
               (= (~ e1 'state) 0)
               (> (~ e1 'y) 150))
        (let1 i (find-index (lambda (m1) (not (~ m1 'useflag))) missiles)
          (if (and i (< i *mr*))
            (let1 m1 (~ missiles i)
              (set! (~ m1 'useflag) #t)
              (set! (~ m1 'kind)    1000)
              (set! (~ m1 'state)   0)
              (set! (~ m1 'life)    0)
              (set! (~ m1 'x)       (~ e1 'x))
              (set! (~ m1 'y)       (~ e1 'y))
              (set! (~ m1 'degree)  (* (atan (- *y* (~ e1 'y)) (- *x* (~ e1 'x))) 180/pi))
              (set! (~ m1 'speed)   (min (+ 8 (- *mmr* 10)) 18))
              (set! (~ m1 'tscrn)   *tscrn-missile1*)
              (set! (~ m1 'hitstr)  "V")
              (set! (~ m1 'minx)    (- *wd/2*))
              (set! (~ m1 'maxx)    *wd/2*)
              (set! (~ m1 'miny)    (- *ht/2*))
              (set! (~ m1 'maxy)    *ht/2*)
              ))))))
   ))

;; 敵/敵ミサイルの表示
(define (disp-enemies enemies)
  (for-each
   (lambda (e1)
     (when (~ e1 'useflag)
       (if (= (~ e1 'state) 0)
         (gl-color 1.0 1.0 1.0 1.0)
         (gl-color 1.0 1.0 1.0 0.9))
       (textscrn-disp (~ e1 'tscrn) (get-win-x (~ e1 'x)) (get-win-y (~ e1 'y))
                      *width* *height* (get-win-w *chw*) (get-win-h *chh*) 'center)
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
         (if (or (< (~ e1 'x) (~ e1 'minx))
                 (> (~ e1 'x) (~ e1 'maxx))
                 (< (~ e1 'y) (~ e1 'miny))
                 (> (~ e1 'y) (~ e1 'maxy)))
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
        (x1  (get-win-x (+ *x* (* *chw* -1.3)    *waku* )))
        (y1  (get-win-y (+ *y* (* *chh* -1.0) (- *waku*))))
        (x2  (get-win-x (+ *x* (* *chw*  1.3) (- *waku*))))
        (y2  (get-win-y (+ *y* (* *chh* -2.0)    *waku* ))))
    (for-each
     (lambda (e1)
       (if (and (~ e1 'useflag) (= (~ e1 'state) 0))
         (let ((ex (get-win-x (~ e1 'x)))
               (ey (get-win-y (~ e1 'y))))
           (when (textscrn-disp-check-str (~ e1 'tscrn) (~ e1 'hitstr) x1 y1 x2 y2
                                          (get-win-w *chw*) (get-win-h *chh*)
                                          ex ey 'center)
             (set! ret #t)
             (if (not *demoflg*) (auddata-play *adata-end*))
             ))
         ))
     enemies)
    ret))

;; 自機ビームの当たり判定
(define (hit-beam?)
  (let ((ret #f)
        (x1  (get-win-x (+ *x* (* *chw* -0.5)       *waku* )))
        (y1  (get-win-y (+ *y* (* *chh* *bc*)    (- *waku*))))
        (x2  (get-win-x (+ *x* (* *chw*  0.5)    (- *waku*))))
        (y2  (get-win-y (+ *y* (* *chh* (- *bc* 1)) *waku* ))))
    (for-each
     (lambda (e1)
       (if (and (~ e1 'useflag) (= (~ e1 'state) 0))
         (let ((ex (get-win-x (~ e1 'x)))
               (ey (get-win-y (~ e1 'y))))
           (when (textscrn-disp-check-str (~ e1 'tscrn) (~ e1 'hitstr) x1 y1 x2 y2
                                          (get-win-w *chw*) (get-win-h *chh*)
                                          ex ey 'center)
             (set! ret #t)
             (dec! (~ e1 'life))
             (when (<= (~ e1 'life) 0)
               (set! (~ e1 'state) 1)
               (when (not *demoflg*)
                 (set! *sc* (+ *sc* 100))
                 (auddata-play *adata-hit*)
                 ))
             ))
         ))
     *enemies*)
    ret))

;; 爆風の表示
(define (disp-blast)
  (gl-color 1.0 1.0 0.0 0.5)
  (for-each
   (lambda (e1)
     (when (and (~ e1 'useflag) (> (~ e1 'state) 0))
       (fill-win-circle (get-win-x (~ e1 'x))
                        (get-win-y (- (~ e1 'y) (/. (* (~ e1 'tscrn 'height) *chh*) 2)))
                        (get-win-w *bs*) 1 1 *width* *height* 'center)
       ))
   *enemies*))

;; 爆風の当たり判定
(define (hit-blast?)
  (let ((ret #f)
        (rr  (* *bs* *bs*)))
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
                      (auddata-play *adata-hit*)
                      ))
                  ))
              ))
          *enemies*)
         ))
     *enemies*)
    ret))

;; 自機に近い敵/敵ミサイルを、近い順にn個だけ取得する(デモ用)
;;   ・戻り値は ((距離の2乗 . 敵) ...) というリストを返す
;;   ・有効な敵がいなければ ((#f . #f) ...) というリストを返す
(define (get-near-enemies n)
  (let* ((ret '())
         (bh  (make-binary-heap :storage (make-vector n #f) :key car)))
    (define (%search-near-enemies enemies)
      (for-each
       (lambda (e1)
         (if (and (~ e1 'useflag) (= (~ e1 'state) 0))
           (let* ((xdiff (- (~ e1 'x) *x*))
                  (ydiff (- (~ e1 'y) *y*))
                  (rr    (+ (* xdiff xdiff) (* ydiff ydiff))))
             (if (< (binary-heap-num-entries bh) n)
               (binary-heap-push! bh (cons rr e1))
               (if (< rr (car (binary-heap-find-max bh)))
                 (binary-heap-swap-max! bh (cons rr e1))))
             )))
       enemies))
    (%search-near-enemies *enemies*)
    (%search-near-enemies *missiles*)
    (do ((i 0 (+ i 1)))
        ((>= i n) #f)
      (push! ret (if (binary-heap-empty? bh)
                   '(#f . #f)
                   (binary-heap-pop-min! bh))))
    (reverse ret)))

;; 自機の敵への攻撃チェック(デモ用)
(define (attack-enemies?)
  (let ((ret #f)
        (x1  (- *x* (* *chw* 2)))
        (x2  (+ *x* (* *chw* 2))))
    (for-each
     (lambda (e1)
       (if (and (~ e1 'useflag) (= (~ e1 'state) 0) (<= x1 (~ e1 'x) x2))
         (set! ret #t)))
     *enemies*)
    ret))

;; 文字列の上書き表示(背景を塗りつぶしてから、その上に表示する)
(define (draw-stroke-text-over str x y size :optional (align 'left))
  (when (not (equal? str ""))
    (draw-stroke-text str x y *width* *height* size align)
    (gl-color *backcolor*)
    (fill-win-rect x (- y (* size 0.1)) (* size (string-length str) 0.67) (* size 1.2)
                   *width* *height* 'center)
    ))


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
  ;; 音楽データの初期化
  (auddata-load-wav-file *adata-start* "sound/appear1.wav")
  (auddata-set-prop *adata-start* AL_GAIN  0.05)
  (auddata-set-prop *adata-start* AL_PITCH 3.0)
  (auddata-load-wav-file *adata-hit*   "sound/decide2.wav")
  (auddata-set-prop *adata-hit*   AL_GAIN  0.4)
  (auddata-set-prop *adata-hit*   AL_PITCH 1.1)
  (auddata-load-wav-file *adata-end*   "sound/pattern05.wav")
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
      (set! str6 (format #f "TIME=(MAX=~D, MIN=~D, AVG=~D, NOW=~D)"
                         *demotmax*  *demotmin*  *demotavg*
                         (round-n (* *ssc* *wait* 0.001) 1)))
      (set! str7 (format #f "PARAM=(~D, ~D) COUNT=~D"
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
    (set! str3 (format #f "SCORE : ~D"    *sc*))
    (set! str4 (format #f "HI-SCORE : ~D" *hs*))
    (set! str5 (format #f "LEVEL : ~D/~D" *mr* *mmr*))
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text-over str1 (/. *width* 2) (/. (* *height* 36) 100) (/. *height* 13) 'center)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text-over str2 (+ (/. *width* 2) (/. *width* 130))
                           (/. (* *height* y2) 100) (/. *height* 18) 'center)
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-stroke-text str3 0 0 *width* *height* (/. *height* 22))
    (gl-color 1.0 0.0 1.0 1.0)
    (draw-stroke-text str4 (/. *width* 2) 0 *width* *height* (/. *height* 22) 'center)
    (gl-color 1.0 1.0 0.0 1.0)
    (draw-stroke-text str5 *width* 0 *width* *height* (/. *height* 22) 'right)
    (gl-color 0.0 1.0 0.0 1.0)
    (draw-stroke-text str6 0 (/. (* *height*  5) 100) *width* *height* (/. *height* 22))
    (draw-stroke-text str7 0 (/. (* *height* 10) 100) *width* *height* (/. *height* 22))
    )
  ;; 画面上部(スコア表示領域)のマスク
  (gl-color *backcolor*)
  (fill-win-rect (/. *width* 2) 0 *width* (get-win-h *chh*) *width* *height* 'center)
  ;; 敵ミサイルの表示
  (disp-enemies *missiles*)
  ;; 敵の表示
  (disp-enemies *enemies*)
  ;; 自機ビームの表示
  (if (> *bc* 0) (disp-beam))
  ;; 自機の表示
  (if (= *scene* 2) (disp-mychr 1))
  (disp-mychr 0)
  ;; 爆風の表示
  (disp-blast)
  ;; 背景の表示
  (gl-color *backcolor*)
  (fill-win-rect (/. *width* 2) 0 *width* *height* *width* *height* 'center)
  ;(gl-flush)
  (glut-swap-buffers)
  )

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  (min w h))
  (set! *height* (min w h))
  ;; 縦横比を変えずにリサイズ
  (if (< w h)
    (gl-viewport 0 (quotient (- h w) 2) *width* *height*)
    (gl-viewport (quotient (- w h) 2) 0 *width* *height*))
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  ;; 透視射影する範囲を設定
  (glu-perspective *vangle* (/. *width* *height*) 1 2000)
  ;; 視点の位置と方向を設定
  (glu-look-at 0 0 (/. *wd/2* *tanvan*) 0 0 0 0 1 0)
  )

;; キー入力ON
(define (keyboard key x y)
  (check-modifier-keys)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit-main-loop 0))
   ;; [g]キーでGC実行(デバッグ用)
   ((or (= key (char->integer #\g)) (= key (char->integer #\G)))
    (gc) (print (gc-stat)))
   )
  (hash-table-put! *keystate* key #t))

;; キー入力OFF
(define (keyboardup key x y)
  (check-modifier-keys)
  (hash-table-put! *keystate* key #f))

;; 特殊キー入力ON
(define (specialkey key x y)
  (check-modifier-keys)
  (hash-table-put! *spkeystate* key #t))

;; 特殊キー入力OFF
(define (specialkeyup key x y)
  (check-modifier-keys)
  (hash-table-put! *spkeystate* key #f))

;; Shift,Ctrl,Altキーの入力チェック
;;   glut-get-modifiers は、入力デバイス関連のコールバック内でしか使用できない。
;;   また、環境によっては、他のキーと同時押ししないと状態を取得できない
(define (check-modifier-keys)
  (let1 mdkey (glut-get-modifiers)
    (hash-table-put! *mdkeystate* GLUT_ACTIVE_SHIFT
                     (not (= (logand mdkey GLUT_ACTIVE_SHIFT) 0)))
    (hash-table-put! *mdkeystate* GLUT_ACTIVE_CTRL
                     (not (= (logand mdkey GLUT_ACTIVE_CTRL)  0)))
    (hash-table-put! *mdkeystate* GLUT_ACTIVE_ALT
                     (not (= (logand mdkey GLUT_ACTIVE_ALT)   0)))
    ))

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
       (set! *x*     0)
       (set! *y*  -240)
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
                     (set! *demoflg* #t)))
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
       (if (= (modulo *ssc* 6) 0) (make-enemies *enemies*))
       ;; 敵の移動
       (move-enemies *enemies*)
       ;; 敵ミサイルの生成
       (if (and (= (modulo *ssc* 6) 0) (< (randint 0 100) 50))
         (make-enemies *enemies* *missiles*))
       ;; 敵ミサイルの移動
       (move-enemies *missiles*)
       ;; 自機の操作
       (cond
        ;; デモのとき
        (*demoflg*
         ;; 自機の移動
         (let* ((nes (get-near-enemies 1))
                (rr1 (car (~ nes 0)))
                (e1  (cdr (~ nes 0)))
                (vx  0))
           (cond
            ;; 一番近い敵/敵ミサイルを避ける
            ((and rr1 (< rr1 (* *chw* *chw* (~ *dparam* 'p1) (~ *dparam* 'p1))))
             (set! vx (if (< *x* (~ e1 'x)) -8 +8)))
            ;; 中央に戻る
            (else
             (when (< (randint 0 100) (~ *dparam* 'p2))
               (if (< *x* -8) (set! vx +8))
               (if (> *x* +8) (set! vx -8))))
            )
           (set! *x* (clamp (+ *x* vx) *minx* *maxx*))
           )
         ;; 自機ビーム発射
         (cond
          ((attack-enemies?)
           (set! *bc* 1)
           (set! *demotime2* 0))
          ((> *bc* 0)
           (set! *demotime2* (+ *demotime2* *wait*))
           (if (>= *demotime2* 200) (set! *bc* 0))))
         ;; デモを抜けるチェック
         (when (or (hash-table-get *keystate* (char->integer #\d) #f)
                   (hash-table-get *keystate* (char->integer #\D) #f))
           (set! *scene*   0)
           (set! *demoflg* #f))
         )
        ;; デモでないとき
        (else
         ;; 自機の移動
         (if (hash-table-get *spkeystate* GLUT_KEY_LEFT  #f)
           (set! *x* (clamp (+ *x* -8) *minx* *maxx*)))
         (if (hash-table-get *spkeystate* GLUT_KEY_RIGHT #f)
           (set! *x* (clamp (+ *x*  8) *minx* *maxx*)))
         (if (hash-table-get *spkeystate* GLUT_KEY_UP    #f)
           (set! *y* (clamp (+ *y*  8) *miny* *maxy*)))
         (if (hash-table-get *spkeystate* GLUT_KEY_DOWN  #f)
           (set! *y* (clamp (+ *y* -8) *miny* *maxy*)))
         ;; 自機ビーム発射
         (if (or (hash-table-get *mdkeystate* GLUT_ACTIVE_CTRL #f)
                 (hash-table-get *keystate* (char->integer #\space) #f)
                 (hash-table-get *keystate* (char->integer #\a) #f)
                 (hash-table-get *keystate* (char->integer #\A) #f)
                 (hash-table-get *keystate* (char->integer #\z) #f)
                 (hash-table-get *keystate* (char->integer #\Z) #f))
           (set! *bc* 1)
           (set! *bc* 0))
         ))
       ;; 自機ビーム処理
       (if (> *bc* 0)
         (let loop ((i 1))
           (set! *bc* i)
           (if (and (<= i 25) (not (hit-beam?)))
             (loop (+ i 1)))
           ))
       ;; 爆風の当たり判定
       (hit-blast?)
       ;; 敵の当たり判定
       (if (hit-enemies? *enemies*) (set! *scene* 2))
       ;; 敵ミサイルの当たり判定
       (if (hit-enemies? *missiles*) (set! *scene* 2))
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
             ;; デモ用パラメータの自動調整
             (if (< t tavg)
               (demoparam-copy *dparam-old* *dparam*)) ; 結果が悪いときは戻す
             (demoparam-copy *dparam* *dparam-old*)
             (set! (~ *dparam* 'p1) (round-n (+ (~ *dparam* 'p1) (* (randint -1 1) 0.1)) 1))
             (set! (~ *dparam* 'p2) (+ (~ *dparam* 'p2) (randint -1 1)))
             ))
         (set! *demotime1* (+ *demotime1* *wait*))
         (if (>= *demotime1* 1600) (set! *scene* 0))
         ;; デモを抜けるチェック
         (when (or (hash-table-get *keystate* (char->integer #\d) #f)
                   (hash-table-get *keystate* (char->integer #\D) #f))
           (set! *scene*   0)
           (set! *demoflg* #f))
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
  (glut-timer-func (waitmsec-calc *wtinfo*) timer 0)
  )

;; 終了
(define (exit-main-loop code)
  (aud-end)
  (exit code))

;; メイン処理
(define (main args)
  (aud-init (> (x->integer (get-one-arg args 1)) 0))
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

