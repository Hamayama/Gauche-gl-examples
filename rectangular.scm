;; -*- coding: utf-8 -*-
;;
;; rectangular.scm
;; 2024-7-21 v1.00
;;
;; ＜内容＞
;;   Gauche-gl を使用した、直方体オブジェを表示するプログラムです。
;;   矢印キーと[z]/[x]/[Ctrl]キーで少し操作できます。
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
(use glmodelkit)
(use model0701)

(define *wait*      20) ; ウェイト(msec)
(define *title* "rectangular") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    90) ; 視野角(度)
(define *tanvan*     (tan (* (/. *vangle* 2) pi/180))) ; 視野角/2のタンジェント(計算用)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *zd/2*     400) ; 画面奥行き/2

(define *wx/2*      25) ; 表示物の幅/2
(define *wh/2*     250) ; 表示物の高さ/2
(define *wz/2*      25) ; 表示物の奥行き/2
(define *startz*     (* (- *zd/2*) 11)) ; 表示物の初期位置のZ座標(奥側が負になる)
(define *endz*       (* (- *zd/2*) 1))  ; 表示物の最終位置のZ座標(奥側が負になる)
(define *rot-spd*   10) ; 表示物の回転速度
(define *speed*     25) ; 表示物の速度

(define *mr*         1) ; 表示数
(define *mmr*       10) ; 表示数の最大数
(define *mmmr*      20) ; 表示数の最大数の最大数
(define *ssc*        0) ; 制御カウンタ

(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色

;; キー入力状態管理クラスのインスタンス生成
(define *ksinfo* (make <keystateinfo>))

;; ウェイト時間調整クラスのインスタンス生成
(define *wcinfo* (make <waitcalcinfo> :waittime *wait*))

;; 表示物クラス
(define-class <disp-object> ()
  ((useflag :init-value #f) ; 使用フラグ
   (x       :init-value 0)  ; X座標
   (y       :init-value 0)  ; Y座標
   (z       :init-value 0)  ; Z座標
   (rot     :init-value 0)  ; 回転角度(度)
   ))
;; 表示物クラスのインスタンス生成
(define *disp-objects* (make-vector-of-class *mmmr* <disp-object>))


;; 表示物の初期化
(define (init-disp-objects disp-objects)
  (for-each (lambda (e1) (set! (~ e1 'useflag) #f)) disp-objects))

;; 表示物の生成
(define (make-disp-objects disp-objects)
  (let1 i (find-index (lambda (e1) (not (~ e1 'useflag))) disp-objects)
    (when (and i (< i *mr*))
      (let1 e1 (~ disp-objects i)
        (set! (~ e1 'useflag) #t)
        (let loop ((count 0))
          (set! (~ e1 'x) (randint (- *wd/2*) *wd/2*))
          (set! (~ e1 'y) (randint (- *ht/2*) *ht/2*))
          ;; 中央付近には生成しないようにする
          (when (and (<= (/. (- *wd/2*) 2) (~ e1 'x) (/. *wd/2* 2))
                     (<= (/. (- *ht/2*) 2) (~ e1 'y) (/. *ht/2* 2)))
            (if (< count 10)
              (loop (+ count 1))
              (set! (~ e1 'useflag) #f))
            ))
        (set! (~ e1 'z)   *startz*)
        (set! (~ e1 'rot) 0)
        ))))

;; 表示物の表示
(define (disp-disp-objects disp-objects)
  (for-each
   (lambda (e1)
     (when (~ e1 'useflag)
       (gl-push-matrix)
       (gl-translate (~ e1 'x) (~ e1 'y) (~ e1 'z))
       (gl-rotate 90 1 0 0)
       (model0701 *wx/2* *wh/2* *wz/2* (~ e1 'rot))
       (gl-pop-matrix)
       ))
   disp-objects))

;; 表示物の移動
(define (move-disp-objects disp-objects)
  (for-each
   (lambda (e1)
     (when (~ e1 'useflag)
       (let ((vx 0) (vy 0) (vz *speed*) (vrot *rot-spd*))
         (when (spkey-on? *ksinfo* GLUT_KEY_LEFT)    (set! vx (+ vx    (- *speed*))))
         (when (spkey-on? *ksinfo* GLUT_KEY_RIGHT)   (set! vx (+ vx       *speed*)))
         (when (spkey-on? *ksinfo* GLUT_KEY_UP)      (set! vy (+ vy       *speed*)))
         (when (spkey-on? *ksinfo* GLUT_KEY_DOWN)    (set! vy (+ vy    (- *speed*))))
         (when (key-on?   *ksinfo* '(#\z #\Z))       (set! vz (+ vz    (* *speed* 2))))
         (when (key-on?   *ksinfo* '(#\x #\X))       (set! vz (+ vz (- (* *speed* 2)))))
         (when (mdkey-on? *ksinfo* GLUT_ACTIVE_CTRL) (set! vz 0))
         (set! (~ e1 'x)   (+ (~ e1 'x)   vx))
         (set! (~ e1 'y)   (+ (~ e1 'y)   vy))
         (set! (~ e1 'z)   (+ (~ e1 'z)   vz))
         (set! (~ e1 'rot) (+ (~ e1 'rot) vrot))
         (when (> (~ e1 'z) *endz*) (set! (~ e1 'useflag) #f))
         (set! (~ e1 'x)   (clamp (~ e1 'x) (- (* *wd/2* 100)) (* *wd/2* 100)))
         (set! (~ e1 'y)   (clamp (~ e1 'y) (- (* *ht/2* 100)) (* *ht/2* 100)))
         (set! (~ e1 'z)   (clamp (~ e1 'z) *startz* 0))
         (set! (~ e1 'rot) (wrap-range (~ e1 'rot) 0 360))
         )))
   disp-objects))

;; 3D投影の確認用の枠を表示
(define (disp-3d-corner-mark)
  (gl-disable GL_LIGHTING)
  (gl-color 1.0 1.0 1.0 1.0)
  (gl-begin GL_LINE_LOOP)
  ;(gl-normal #f32(0 0 1))
  (gl-vertex (f32vector  (- *wd/2*)  *ht/2*    (- *zd/2*)))
  (gl-vertex (f32vector  100         100    (* (- *zd/2*) 2)))
  (gl-vertex (f32vector  100        -100    (* (- *zd/2*) 2)))
  (gl-vertex (f32vector -100        -100    (* (- *zd/2*) 2)))
  (gl-end)
  (gl-enable GL_LIGHTING))


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
  )

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 表示物の表示
  (disp-disp-objects *disp-objects*)
  ;; 3D投影の確認用の枠を表示
  ;(disp-3d-corner-mark)
  ;; 背景の表示
  (gl-color *backcolor*)
  (draw-win-rect 0 0 *width* *height* *width* *height* 'left -0.999999)
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  w)
  (set! *height* h)
  ;; 画面幅の変化に追従
  (set! *wd/2* (truncate->exact (* (/. *width* (max *height* 1)) *ht/2*)))
  ;; 画面のリサイズ
  (gl-viewport 0 0 *width* *height*)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  ;; 透視射影する範囲を設定
  (glu-perspective *vangle* (/. *width* *height*) (- *endz*) (+ (- *startz*) *zd/2*)))

;; キー入力ON
(define (keyboard key x y)
  (key-on *ksinfo* key)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit 0))
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
  ;; 制御カウンタの処理等
  (inc! *ssc*)
  (when (> *ssc* 1000000) (set! *ssc* 1))
  (when (= (modulo *ssc* 100) 0)
    (inc! *mr*)
    (when (> *mr* *mmr*)
      (set! *mr* 0)
      (set! *mmr* (min (+ *mmr* 2) *mmmr*))))
  ;; 表示物の生成
  (when (= (modulo *ssc* 20) 0)
    (make-disp-objects *disp-objects*))
  ;; 表示物の移動
  (move-disp-objects *disp-objects*)
  ;; 画面表示
  (glut-post-redisplay)
  ;; ウェイト時間調整
  (glut-timer-func (waitcalc *wcinfo*) timer 0))

;; メイン処理
(define (main args)
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
  (guard (ex (else (report-error ex) (exit 1)))
    (glut-main-loop))
  0)

