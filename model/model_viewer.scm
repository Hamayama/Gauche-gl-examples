;; -*- coding: utf-8 -*-
;;
;; モデルビューワー
;; 2017-3-24
;;
;; ＜使い方＞
;;   gosh  model_viewer.scm  [modelXXXX.scm]
;;
;; ＜注意事項＞
;; ・モデルのサイズは一辺が100程度であることを想定しています
;;
(add-load-path ".." :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use gauche.collection)
(use math.const)
(use glmintool)
(use gltextscrn)
(use glmodelkit)

(define *wait*      20) ; ウェイト(msec)
(define *title* "model-viewer") ; ウィンドウのタイトル
(define *width*    960) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)
(define *aratio*     (/. *width* *height*)) ; アスペクト比(計算用)

(define *wd/2*      80) ; 画面幅/2
(define *ht/2*      80) ; 画面高さ/2
(define *zd/2*     110) ; 奥行き/2
(define *xrot*       0) ; X軸を軸とする回転角(度)
(define *yrot*       0) ; Y軸を軸とする回転角(度)
(define *drot*       2) ; 回転角の増分(度)
(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 4) (* *ht/2* 2))

;; キー入力状態管理クラスのインスタンス生成
(define *ksinfo* (make <keystateinfo>))

;; ウェイト時間調整クラスのインスタンス生成
(define *wcinfo* (make <waitcalcinfo> :waittime *wait*))

;; カスタマイズ用
(define *model-name*          "")
(define *model-text-vec-A*    (make-vector  5 ""))
(define *model-text-vec-B*    (make-vector  5 ""))
(define *model-para-vec*      (make-vector 10 0))
(define (viewer-init)         #f)
(define (viewer-disp)         #f)
(define (viewer-reshape)      #f)
(define (viewer-keyboard)     #f)
(define (viewer-keyboardup)   #f)
(define (viewer-specialkey)   #f)
(define (viewer-specialkeyup) #f)
(define (viewer-timer)        #f)


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
  ;; カスタマイズ用
  (viewer-init))

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 文字表示
  (let ((x1 (+ (win-w-r *win* 1/2) 8))
        (y1 0))
    (gl-color 1.0 1.0 1.0 1.0)
    (draw-bitmap-text *model-name* x1 y1 *width* *height* 24)
    (set! y1 (+ y1 30))
    (for-each (lambda (str)
                (draw-bitmap-text str x1 y1 *width* *height* 24)
                (set! y1 (+ y1 30)))
              *model-text-vec-A*)
    (set! y1 (+ y1 30))
    (draw-bitmap-text "[esc] : exit" x1 y1 *width* *height* 24)
    (set! y1 (+ y1 30))
    (draw-bitmap-text "[arrow keys] : rotate" x1 y1 *width* *height* 24)
    (set! y1 (+ y1 30))
    (for-each (lambda (str)
                (draw-bitmap-text str x1 y1 *width* *height* 24)
                (set! y1 (+ y1 30)))
              *model-text-vec-B*)
    )
  ;; モデルの表示
  (gl-push-matrix)
  (gl-rotate *yrot* 0 1 0) ; Y軸を軸とする回転
  (gl-rotate *xrot* 1 0 0) ; X軸を軸とする回転
  (viewer-disp)
  (gl-pop-matrix)
  ;; 背景の表示
  (gl-color 0.5 0.5 0.5 1.0)
  (draw-win-line (win-w-r *win* 1/2) 0 (win-w-r *win* 1/2) *height* *width* *height*)
  (gl-color *backcolor*)
  (draw-win-rect 0 0 (win-w-r *win* 1/2) *height* *width* *height* 'left -0.99999)
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  (min w (truncate->exact (*  h *aratio*))))
  (set! *height* (min h (truncate->exact (/. w *aratio*))))
  (win-update-size *win* *width* *height*)
  ;; 縦横比を変えずにリサイズ
  (gl-viewport (quotient (- w *width*) 2) (quotient (- h *height*) 2) *width* *height*)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (let1 z1 (/. *ht/2* *tanvan*)
    ;; 透視射影する範囲を設定
    ;(glu-perspective *vangle* (/. *width* *height*) (- z1 *zd/2*) (+ z1 *zd/2*))
    (gl-frustum (- *wd/2*) (* *wd/2* 3) (- *ht/2*) *ht/2* z1 (+ z1 (* *zd/2* 2)))
    ;; 視点の位置と方向を設定
    ;(glu-look-at 0 0 z1 0 0 0 0 1 0)
    (glu-look-at 0 0 (+ z1 *zd/2*) 0 0 0 0 1 0)
    )
  ;; カスタマイズ用
  (viewer-reshape))

;; キー入力ON
(define (keyboard key x y)
  (key-on *ksinfo* key)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit 0))
   ;; [g]キーでGC実行(デバッグ用)
   ((or (= key (char->integer #\g)) (= key (char->integer #\G)))
    (gc) (print (gc-stat)))
   )
  ;; カスタマイズ用
  (viewer-keyboard))

;; キー入力OFF
(define (keyboardup key x y)
  (key-off *ksinfo* key)
  ;; カスタマイズ用
  (viewer-keyboardup))

;; 特殊キー入力ON
(define (specialkey key x y)
  (spkey-on *ksinfo* key)
  ;; カスタマイズ用
  (viewer-specialkey))

;; 特殊キー入力OFF
(define (specialkeyup key x y)
  (spkey-off *ksinfo* key)
  ;; カスタマイズ用
  (viewer-specialkeyup))

;; タイマー
(define (timer val)
  ;; モデルの回転操作
  (if (spkey-on? *ksinfo* GLUT_KEY_LEFT)
    (set! *yrot* (- *yrot* *drot*)))
  (if (spkey-on? *ksinfo* GLUT_KEY_RIGHT)
    (set! *yrot* (+ *yrot* *drot*)))
  (if (spkey-on? *ksinfo* GLUT_KEY_UP)
    (set! *xrot* (- *xrot* *drot*)))
  (if (spkey-on? *ksinfo* GLUT_KEY_DOWN)
    (set! *xrot* (+ *xrot* *drot*)))
  ;; カスタマイズ用
  (viewer-timer)
  ;; 画面表示
  (glut-post-redisplay)
  ;; ウェイト時間調整
  (glut-timer-func (waitcalc *wcinfo*) timer 0))

;; メイン処理
(define (main args)

  ;; モデルのロード
  (load (make-fpath
         *app-dpath*
         (x->string (list-ref args 1 "model0101.scm"))))

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
  (guard (ex (else (report-error ex) (exit 0)))
    (glut-main-loop))
  0)

