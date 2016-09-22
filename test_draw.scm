;; -*- coding: utf-8 -*-
;;
;; 2D描画のテスト
;; 2016-9-22
;;
(add-load-path "." :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use gltextscrn)

(define *title* "test-draw") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色


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
  ;; 文字表示(ビットマップフォント)(拡大縮小不可)
  (gl-color 1.0 1.0 1.0 1.0)
  (draw-bitmap-text "AIJMQabcdefghijklmnopqrstuvwxyz[]"
                    0 0 *width* *height* 24)
  (gl-color 1.0 1.0 1.0 1.0)
  (draw-bitmap-text-over "AIJMQabcdefghijklmnopqrstuvwxyz[]"
                         (/. *width* 2) (/. (* *height* 8) 100)
                         *width* *height* 18 'center
                         #f #f32(0.0 0.0 1.0 1.0) 1.1 1.2 0
                         GLUT_BITMAP_HELVETICA_18)
  ;; 文字表示(ストロークフォント)(拡大縮小可能)
  (gl-color 1.0 1.0 1.0 1.0)
  (draw-stroke-text "AIJMQabcdefghijklmnopqrstuvwxyz[]"
                    0 (/. (* *height* 16) 100) *width* *height* (/. *height* 18))
  (gl-color 1.0 1.0 1.0 1.0)
  (draw-stroke-text-over "AIJMQabcdefghijklmnopqrstuvwxyz[]"
                         (/. *width* 2) (/. (* *height* 24) 100)
                         *width* *height* (/. *height* 19) 'center
                         #f #f32(0.0 0.0 1.0 1.0) 1.1 1.2 0
                         GLUT_STROKE_ROMAN)
  ;; 線の表示
  (gl-color 0.0 1.0 1.0 1.0)
  (draw-win-line 0 0 *width* *height* *width* *height*)
  ;; 長方形の表示
  (gl-color 1.0 1.0 0.0 1.0)
  (draw-win-rect (/. (* *width* 10) 100) (/. (* *height* 40) 100)
                 (/. (* *width* 30) 100) (/. (* *height* 20) 100) *width* *height*)
  ;; 円の表示
  (gl-color 0.0 0.8 0.0 1.0)
  (draw-win-circle (/. (* *width* 70) 100) (/. (* *height* 50) 100)
                   (/. (* *width* 15) 100) 1 1 *width* *height*)
  ;; 多角形の表示
  (gl-color 1.0 0.0 1.0 1.0)
  (draw-win-poly (/. (* *width* 10) 100) (/. (* *height* 70) 100)
                 (vector (f32vector (/. (* *width* 10) 100) 0)
                         (f32vector 0 (/. (* *height* 20) 100))
                         (f32vector (/. (* *width* 30) 100) (/. (* *height* 20) 100)))
                 *width* *height*)
  ;; 背景の表示
  (gl-color *backcolor*)
  (draw-win-rect (/. *width* 2) 0 *width* *height* *width* *height* 'center)
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
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit-main-loop 0))
   ;; [g]キーでGC実行(デバッグ用)
   ((or (= key (char->integer #\g)) (= key (char->integer #\G)))
    (gc) (print (gc-stat)))
   ))

;; 終了
(define (exit-main-loop code)
  (exit code))

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
  ;(glut-keyboard-up-func keyboardup)
  ;(glut-special-func specialkey)
  ;(glut-special-up-func specialkeyup)
  ;(glut-timer-func *wait* timer 0)
  ;; コールバック内エラー対策
  (guard (ex (else (report-error ex) (exit-main-loop 1)))
    (glut-main-loop))
  0)
