;; -*- coding: utf-8 -*-
;;
;; glwormkit.scm
;; 2018-5-31 v1.03
;;
;; ＜内容＞
;;   ワームシミュレータ用のモジュールです。
;;
(define-module glwormkit
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use gauche.version)
  (use math.const)
  (use glmintool)
  (use gltextscrn)
  (use glmodelkit)
  (use model0501)
  (export
    <worm0101> <worm0201>
    worm-init worm-set-goal worm-goal? worm-move worm-disp worm-disp-flat
    %worm-calc-angle %worm-move-tail %worm-calc-point
    %worm-move-front
    worm-convert
    ))
(select-module glwormkit)

;; Gauche v0.9.4 で、vector-copy! を使用可能にする
(if (version<=? (gauche-version) "0.9.4")
  (eval '(use srfi-43) (current-module)))

;; ワーム0101クラス
;; (末尾から移動していくタイプ)
(define-class <worm0101> ()
  (;; 多関節情報(公開用)
   (anum  :init-value   8)  ; 関節の数
   (axvec :init-value   #f) ; 関節のX座標(ベクタ)
   ;                        ;   (ベクタ番号=0:先頭,=1～anum:関節,=anum+1:末尾)
   (ayvec :init-value   #f) ; 関節のY座標(ベクタ)
   ;                        ;   (ベクタ番号=0:先頭,=1～anum:関節,=anum+1:末尾)
   (arvec :init-value   #f) ; 関節の半径(ベクタ)
   ;                        ;   (ベクタ番号=0:先頭,=1～anum:関節,=anum+1:末尾)
   (gx    :init-value   0)  ; 目標のX座標
   (gy    :init-value   0)  ; 目標のY座標
   ;; 多関節情報(内部用)
   (rx    :init-value   0)  ; 末尾のX座標
   (ry    :init-value   0)  ; 末尾のY座標
   (rr    :init-value  20)  ; 末尾の半径
   (rv    :init-value   1)  ; 末尾の速度
   (rcv   :init-value 1.0)  ; 末尾の角速度(度)
   (ax    :init-value   #f) ; 関節のX座標(ベクタ)
   ;                        ;   (ベクタ番号=0:末尾,=1～anum:関節,=anum+1:先端)
   (ay    :init-value   #f) ; 関節のY座標(ベクタ)
   ;                        ;   (ベクタ番号=0:末尾,=1～anum:関節,=anum+1:先端)
   (ar    :init-value  10)  ; 関節の半径
   (al    :init-value  40)  ; 関節の距離
   (ac    :init-value   #f) ; 関節の角度(度)(ベクタ)
   ;                        ;   (ベクタ番号=0:末尾,=1～anum:関節)
   (acv   :init-value 0.2)  ; 関節の角速度(度)
   (maxac :init-value  45)  ; 関節の角度の最大値(度)
   (fx    :init-value   0)  ; 先端のX座標
   (fy    :init-value   0)  ; 先端のY座標
   (fr    :init-value  25)  ; 先端の半径
   (fc    :init-value   0)  ; 先端の角度(度)
   (vvec  :init-value   #f) ; フラット表示用(ベクタ)
   ))
;; ワーム0101の初期化
;;   anum  関節の数
;;   x     (末尾の)X座標
;;   y     (末尾の)Y座標
;;   c     (末尾の)角度(度)
(define-method worm-init ((w1 <worm0101>) (anum <integer>) (x <real>) (y <real>) (c <real>))
  (set! (~ w1 'anum)  anum)
  (set! (~ w1 'rx)    x)
  (set! (~ w1 'ry)    y)
  (set! (~ w1 'ax)    (make-vector (+ anum 2) 0))
  (set! (~ w1 'ay)    (make-vector (+ anum 2) 0))
  (set! (~ w1 'ac)    (make-vector (+ anum 1) 0))
  (set! (~ w1 'ac 0)  c)
  (set! (~ w1 'axvec) (make-vector (+ anum 2) 0))
  (set! (~ w1 'ayvec) (make-vector (+ anum 2) 0))
  (set! (~ w1 'arvec) (make-vector (+ anum 2) (~ w1 'ar)))
  (set! (~ w1 'arvec  0)           (~ w1 'fr))
  (set! (~ w1 'arvec  (+ anum 1))  (~ w1 'rr))
  (let1 slice 100
    (set! (~ w1 'vvec) (make-vector (+ slice 1) #f))
    (do ((i 0 (+ i 1))) ((> i slice) #f) (set! (~ w1 'vvec i) (f32vector 0 0))))
  (%worm-calc-point w1))
;; ワーム0101の目標設定
;;   gx  目標のX座標
;;   gy  目標のY座標
(define-method worm-set-goal ((w1 <worm0101>) (gx <real>) (gy <real>))
  (set! (~ w1 'gx)    gx)
  (set! (~ w1 'gy)    gy))
;; ワーム0101の移動
(define-method worm-move ((w1 <worm0101>))
  (%worm-calc-angle w1)
  (%worm-move-tail  w1)
  (%worm-calc-point w1))
;; ワーム0101の角度計算(内部処理用)
(define-method %worm-calc-angle ((w1 <worm0101>))
  (define anum  (~ w1 'anum))
  (define rcv   (~ w1 'rcv))
  (define acv   (~ w1 'acv))
  (define maxac (~ w1 'maxac))
  (define gx    (~ w1 'gx))
  (define gy    (~ w1 'gy))
  (define fx    (~ w1 'fx))
  (define fy    (~ w1 'fy))
  ;; 先端から順番に目標に近づけていく
  (set! (~ w1 'ax 0) (~ w1 'rx))
  (set! (~ w1 'ay 0) (~ w1 'ry))
  (do ((i anum (- i 1)))
      ((< i 0) #f)
    (let* ((ax    (~ w1 'ax i))
           (ay    (~ w1 'ay i))
           (gx1   (- gx ax))
           (gy1   (- gy ay))
           (fx1   (- fx ax))
           (fy1   (- fy ay))
           (c1    (* (atan gy1 gx1) 180/pi))
           (c2    (* (atan fy1 fx1) 180/pi))
           (diffc (wrap-range (- c1 c2) -180 180))
           (ac1   0))
      (cond
       ((= i 0)
        ;; 末尾の角度を計算
        (set! diffc (clamp diffc (- rcv) rcv))
        (set! (~ w1 'ac 0) (wrap-range (+ (~ w1 'ac 0) diffc) -180 180)))
       (else
        ;; 関節の角度を計算
        (set! diffc (clamp diffc (- acv) acv))
        (set! ac1 (+ (~ w1 'ac i) diffc))
        (when (> (abs ac1) maxac)
          (set! ac1 (clamp ac1 (- maxac) maxac))
          (set! diffc (- ac1 (~ w1 'ac i))))
        (set! (~ w1 'ac i) ac1)))
      ;; 先端の座標を補正して繰り返す
      (set! fx (+ ax (- (* fx1 (cos (* diffc pi/180)))
                        (* fy1 (sin (* diffc pi/180))))))
      (set! fy (+ ay (+ (* fx1 (sin (* diffc pi/180)))
                        (* fy1 (cos (* diffc pi/180))))))
      )))
;; ワーム0101の目標到達チェック
(define-method worm-goal? ((w1 <worm0101>) :optional (waku 4))
  (define gx    (~ w1 'gx))
  (define gy    (~ w1 'gy))
  (define fx    (~ w1 'fx))
  (define fy    (~ w1 'fy))
  (recthit? (- gx waku) (- gy waku) (* waku 2) (* waku 2)
            (- fx waku) (- fy waku) (* waku 2) (* waku 2)))
;; ワーム0101の末尾移動(内部処理用)
(define-method %worm-move-tail ((w1 <worm0101>))
  (define rv    (~ w1 'rv))
  (set! (~ w1 'rx) (+ (~ w1 'rx) (* rv (cos (* (- (~ w1 'ac 0) 90) pi/180)))))
  (set! (~ w1 'ry) (+ (~ w1 'ry) (* rv (sin (* (- (~ w1 'ac 0) 90) pi/180))))))
;; ワーム0101の座標計算(内部処理用)
(define-method %worm-calc-point ((w1 <worm0101>))
  (define acsum -90)
  (define anum  (~ w1 'anum))
  (define al    (~ w1 'al))
  ;; 末尾の方から順番に座標を計算していく
  (set! (~ w1 'ax 0) (~ w1 'rx))
  (set! (~ w1 'ay 0) (~ w1 'ry))
  (do ((i 0 (+ i 1)))
      ((> i anum) #f)
    (set! acsum (+ acsum (~ w1 'ac i)))
    (set! (~ w1 'ax (+ i 1)) (+ (~ w1 'ax i) (* al (cos (* acsum pi/180)))))
    (set! (~ w1 'ay (+ i 1)) (+ (~ w1 'ay i) (* al (sin (* acsum pi/180)))))
    )
  (set! (~ w1 'fx) (~ w1 'ax (+ anum 1)))
  (set! (~ w1 'fy) (~ w1 'ay (+ anum 1)))
  (set! (~ w1 'fc) acsum)
  ;; 公開用情報を更新
  ;; (内部情報とは逆順になる)
  (do ((i 0 (+ i 1)))
      ((> i (+ anum 1)) #f)
    (set! (~ w1 'axvec i) (~ w1 'ax (- (+ anum 1) i)))
    (set! (~ w1 'ayvec i) (~ w1 'ay (- (+ anum 1) i)))))
;; ワーム0101の表示
(define-method worm-disp ((w1 <worm0101>) :optional (color #f32(1.0 1.0 1.0 1.0)) (wedge 70))
  (%worm-disp-sub w1 color wedge))
(define (%worm-disp-sub w1 color wedge)
  (define anum  (~ w1 'anum))
  (define ar    (~ w1 'ar))
  (define rr    (~ w1 'rr))
  ;; 色
  (gl-material GL_FRONT GL_DIFFUSE color)
  (gl-material GL_FRONT GL_AMBIENT #f32(0.5 0.5 0.5 1.0))
  ;; 先端
  (gl-push-matrix)
  (gl-translate (~ w1 'fx) (~ w1 'fy) 0)
  (gl-rotate (~ w1 'fc) 0 0 1)
  (gl-rotate -90 1 0 0)
  (model0501 (~ w1 'fr) 15 10 wedge)
  (gl-pop-matrix)
  ;; 関節と末尾
  (do ((i 1 (+ i 1)))
      ((> i (+ anum 1)) #f)
    (gl-push-matrix)
    (gl-translate (~ w1 'axvec i) (~ w1 'ayvec i) 0)
    (glut-solid-sphere (if (< i (+ anum 1)) ar rr) 20 20)
    (gl-pop-matrix)))
;; ワーム0101のフラット表示
(define-method worm-disp-flat ((w1 <worm0101>)
                               (win <wininfo>)
                               :optional (color #f32(1.0 1.0 1.0 1.0)) (wedge 70) (z 0))
  (%worm-disp-flat-sub w1 win color wedge z))
(define (%worm-disp-flat-sub w1 win color wedge z)
  (define width  (~ win 'width))
  (define height (~ win 'height))
  (define anum   (~ w1  'anum))
  (define frwin  (win-w win (~ w1 'fr)))
  (define arwin  (win-w win (~ w1 'ar)))
  (define rrwin  (win-w win (~ w1 'rr)))
  (define slice  100)
  (define wedge1 (/. (* wedge pi/180) 2))
  (define step   (/. (* (- pi wedge1) 2) 100))
  (define fc1    (* (~ w1 'fc) pi/180))
  ;; 色
  (gl-color color)
  ;; 先端
  (%win-ortho-on width height)
  (set! (~ w1 'vvec 0 0) 0)
  (set! (~ w1 'vvec 0 1) 0)
  (do ((i   1 (+ i 1))
       (rad (- wedge1 fc1) (if (< (+ i 1) slice) (+ rad step) (- 2pi wedge1 fc1))))
      ((> i slice) #f)
    (set! (~ w1 'vvec i 0) (* frwin (cos rad)))
    (set! (~ w1 'vvec i 1) (* frwin (sin rad))))
  (%draw-win-poly (win-x win (~ w1 'fx))
                  (win-y win (~ w1 'fy))
                  (~ w1 'vvec)
                  width height z)
  ;; 関節と末尾
  (do ((i 1 (+ i 1)))
      ((> i (+ anum 1)) #f)
    (%draw-win-line   (win-x win (~ w1 'axvec (- i 1)))
                      (win-y win (~ w1 'ayvec (- i 1)))
                      (win-x win (~ w1 'axvec i))
                      (win-y win (~ w1 'ayvec i))
                      width height z)
    (%draw-win-circle (win-x win (~ w1 'axvec i))
                      (win-y win (~ w1 'ayvec i))
                      (if (< i (+ anum 1)) arwin rrwin)
                      width height 1 1 'center z))
  (%win-ortho-off))


;; ワーム0201クラス
;; (先端から移動していくタイプ)
(define-class <worm0201> ()
  (;; 多関節情報(公開用)
   (anum  :init-value   8)  ; 関節の数
   (axvec :init-value   #f) ; 関節のX座標(ベクタ)
   ;                        ;   (ベクタ番号=0:先頭,=1～anum:関節,=anum+1:末尾)
   (ayvec :init-value   #f) ; 関節のY座標(ベクタ)
   ;                        ;   (ベクタ番号=0:先頭,=1～anum:関節,=anum+1:末尾)
   (arvec :init-value   #f) ; 関節の半径(ベクタ)
   ;                        ;   (ベクタ番号=0:先頭,=1～anum:関節,=anum+1:末尾)
   (gx    :init-value   0)  ; 目標のX座標
   (gy    :init-value   0)  ; 目標のY座標
   ;; 多関節情報(内部用)
   (fx    :init-value   0)  ; 先端のX座標
   (fy    :init-value   0)  ; 先端のY座標
   (fr    :init-value  25)  ; 先端の半径
   (fv    :init-value   7)  ; 先端の速度
   (fc    :init-value   0)  ; 先端の角度(度)
   (fcv   :init-value   5)  ; 先端の角速度(度)
   (ar    :init-value  10)  ; 関節の半径
   (al    :init-value  40)  ; 関節の距離
   (adf   :init-value   #f) ; 関節の遅延フレーム係数
   (axque :init-value   #f) ; 関節のX座標の遅延キュー
   (ayque :init-value   #f) ; 関節のY座標の遅延キュー
   (rr    :init-value  20)  ; 末尾の半径
   (vvec  :init-value   #f) ; フラット表示用(ベクタ)
   ))
;; ワーム0201の初期化
;;   anum  関節の数
;;   x     (先端の)X座標
;;   y     (先端の)Y座標
;;   c     (先端の)角度(度)
(define-method worm-init ((w1 <worm0201>) (anum <integer>) (x <real>) (y <real>) (c <real>))
  (set! (~ w1 'anum)  anum)
  (set! (~ w1 'fx)    x)
  (set! (~ w1 'fy)    y)
  (set! (~ w1 'fc)    c)
  (set! (~ w1 'adf)   (round->exact (/. (~ w1 'al) (~ w1 'fv))))
  (set! (~ w1 'axque) (make <quedata>))
  (set! (~ w1 'ayque) (make <quedata>))
  (quedata-init (~ w1 'axque) (+ (* (+ anum 1) (~ w1 'adf)) 1) x)
  (quedata-init (~ w1 'ayque) (+ (* (+ anum 1) (~ w1 'adf)) 1) y)
  (set! (~ w1 'axvec) (make-vector (+ anum 2) 0))
  (set! (~ w1 'ayvec) (make-vector (+ anum 2) 0))
  (set! (~ w1 'arvec) (make-vector (+ anum 2) (~ w1 'ar)))
  (set! (~ w1 'arvec  0)           (~ w1 'fr))
  (set! (~ w1 'arvec  (+ anum 1))  (~ w1 'rr))
  (let1 slice 100
    (set! (~ w1 'vvec) (make-vector (+ slice 1) #f))
    (do ((i 0 (+ i 1))) ((> i slice) #f) (set! (~ w1 'vvec i) (f32vector 0 0))))
  )
;; ワーム0201の目標設定
;;   gx  目標のX座標
;;   gy  目標のY座標
(define-method worm-set-goal ((w1 <worm0201>) (gx <real>) (gy <real>))
  (set! (~ w1 'gx)    gx)
  (set! (~ w1 'gy)    gy))
;; ワーム0201の目標到達チェック
(define-method worm-goal? ((w1 <worm0201>) :optional (waku 4))
  (define gx    (~ w1 'gx))
  (define gy    (~ w1 'gy))
  (define fx    (~ w1 'fx))
  (define fy    (~ w1 'fy))
  (recthit? (- gx waku) (- gy waku) (* waku 2) (* waku 2)
            (- fx waku) (- fy waku) (* waku 2) (* waku 2)))
;; ワーム0201の移動
(define-method worm-move ((w1 <worm0201>))
  (%worm-move-front w1)
  (%worm-calc-point w1))
;; ワーム0201の先端移動(内部処理用)
(define-method %worm-move-front ((w1 <worm0201>) :optional (add-rand #f))
  (define gx    (~ w1 'gx))
  (define gy    (~ w1 'gy))
  (define fx    (~ w1 'fx))
  (define fy    (~ w1 'fy))
  (define fv    (~ w1 'fv))
  (define fcv   (~ w1 'fcv))
  ;; 先端の角度を目標に近づける
  (let* ((c1    (* (atan (- gy fy) (- gx fx)) 180/pi))
         (diffc (wrap-range (- c1 (~ w1 'fc)) -180 180)))
    (when (> (abs diffc) fcv)
      (set! diffc (clamp diffc (- fcv) fcv))
      ;; 指定があれば乱数を加算
      (when add-rand
        (set! diffc (+ diffc (randint (- fcv) fcv)))))
    (set! (~ w1 'fc) (wrap-range (+ (~ w1 'fc) diffc) -180 180))
    ;; 先端の座標を計算
    (set! (~ w1 'fx) (+ fx (* fv (cos (* (~ w1 'fc) pi/180)))))
    (set! (~ w1 'fy) (+ fy (* fv (sin (* (~ w1 'fc) pi/180)))))
    ;; 遅延キューに座標を追加
    (quedata-push (~ w1 'axque) (~ w1 'fx))
    (quedata-push (~ w1 'ayque) (~ w1 'fy))
    ))
;; ワーム0201の座標計算(内部処理用)
(define-method %worm-calc-point ((w1 <worm0201>))
  (define anum  (~ w1 'anum))
  ;; 公開用情報を更新
  ;; (遅延キューの座標を参照して設定)
  (do ((i 0 (+ i 1)))
      ((> i (+ anum 1)) #f)
    (set! (~ w1 'axvec i) (quedata-ref (~ w1 'axque) (* i (~ w1 'adf))))
    (set! (~ w1 'ayvec i) (quedata-ref (~ w1 'ayque) (* i (~ w1 'adf))))))
;; ワーム0201の表示
(define-method worm-disp ((w1 <worm0201>) :optional (color #f32(1.0 1.0 1.0 1.0)) (wedge 70))
  (%worm-disp-sub w1 color wedge))
;; ワーム0201のフラット表示
(define-method worm-disp-flat ((w1 <worm0201>)
                               (win <wininfo>)
                               :optional (color #f32(1.0 1.0 1.0 1.0)) (wedge 70) (z 0))
  (%worm-disp-flat-sub w1 win color wedge z))


;; ワーム0101からワーム0201へのコンバート
;; (座標と角度の情報のみ変換する)
(define-method worm-convert ((w1 <worm0101>) (w2 <worm0201>))
  (define anum  (~ w1 'anum))
  (define adf   (~ w2 'adf))
  (set! (~ w2 'anum)  anum)
  (vector-copy! (~ w2 'axvec) 0 (~ w1 'axvec))
  (vector-copy! (~ w2 'ayvec) 0 (~ w1 'ayvec))
  ;(vector-copy! (~ w2 'arvec) 0 (~ w1 'arvec))
  (set! (~ w2 'gx)    (~ w1 'gx))
  (set! (~ w2 'gy)    (~ w1 'gy))
  (set! (~ w2 'fx)    (~ w1 'axvec 0))
  (set! (~ w2 'fy)    (~ w1 'ayvec 0))
  (set! (~ w2 'fc)    (~ w1 'fc))
  ;; 遅延キューを線形補間により生成
  (do ((i anum (- i 1)))
      ((< i 0) #f)
    (let* ((ax1 (~ w1 'axvec (+ i 1)))
           (ay1 (~ w1 'ayvec (+ i 1)))
           (ax2 (~ w1 'axvec i))
           (ay2 (~ w1 'ayvec i))
           (dx1 (/. (- ax2 ax1) adf))
           (dy1 (/. (- ay2 ay1) adf)))
      (do ((j 0 (+ j 1)))
          ((>= j adf) #f)
        (quedata-push (~ w2 'axque) ax1)
        (quedata-push (~ w2 'ayque) ay1)
        (set! ax1 (+ ax1 dx1))
        (set! ay1 (+ ay1 dy1)))))
  (quedata-push (~ w2 'axque) (~ w1 'axvec 0))
  (quedata-push (~ w2 'ayque) (~ w1 'ayvec 0)))


;; ワーム0201からワーム0101へのコンバート
;; (座標と角度の情報のみ変換する)
(define-method worm-convert ((w1 <worm0201>) (w2 <worm0101>))
  (define anum  (~ w1 'anum))
  (define acsum -90)
  (set! (~ w2 'anum)  anum)
  (vector-copy! (~ w2 'axvec) 0 (~ w1 'axvec))
  (vector-copy! (~ w2 'ayvec) 0 (~ w1 'ayvec))
  ;(vector-copy! (~ w2 'arvec) 0 (~ w1 'arvec))
  (set! (~ w2 'gx)    (~ w1 'gx))
  (set! (~ w2 'gy)    (~ w1 'gy))
  (set! (~ w2 'rx)    (~ w1 'axvec (+ anum 1)))
  (set! (~ w2 'ry)    (~ w1 'ayvec (+ anum 1)))
  (do ((i 0 (+ i 1)))
      ((> i (+ anum 1)) #f)
    (set! (~ w2 'ax i) (~ w1 'axvec (- (+ anum 1) i)))
    (set! (~ w2 'ay i) (~ w1 'ayvec (- (+ anum 1) i))))
  ;; 関節の角度を計算
  (do ((i 0 (+ i 1)))
      ((> i anum) #f)
    (let* ((x1 (~ w2 'ax i))
           (y1 (~ w2 'ay i))
           (x2 (~ w2 'ax (+ i 1)))
           (y2 (~ w2 'ay (+ i 1)))
           (c1 (* (atan (- y2 y1) (- x2 x1)) 180/pi)))
      (set! (~ w2 'ac i) (wrap-range (- c1 acsum) -180 180))
      (set! acsum c1)))
  (set! (~ w2 'fx)    (~ w1 'axvec 0))
  (set! (~ w2 'fy)    (~ w1 'ayvec 0))
  (set! (~ w2 'fc)    acsum))


;; モデル0501(欠けた球)(中心に原点あり)
;;   r      半径
;;   slice  y軸のまわりの分割数
;;   stack  y軸に垂直な分割数
;;   wedge  欠けの大きさ(角度)
;; (define (model0501 r slice stack wedge) ...)


