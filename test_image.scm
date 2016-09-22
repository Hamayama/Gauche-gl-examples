;; -*- coding: utf-8 -*-
;;
;; 画像表示のテスト
;; 2016-9-23
;;
(add-load-path "." :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use glmintool)
(use gltextscrn)

(define *title* "test-image") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)
(define *vangle*    45) ; 視野角(度)
(define *tanvan*     (tan (/. (* *vangle* pi) 180 2))) ; 視野角/2のタンジェント(計算用)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

;; テクスチャの配列(u32vector)
(define *tex* #f)

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 2) (* *ht/2* 2))

;; テキスト画面クラスのインスタンス生成
(define *tscrn1* (make <textscrn>))
(textscrn-init *tscrn1* 3 2)
(textscrn-pset *tscrn1* 0 0 "AAA")
(textscrn-pset *tscrn1* 0 1 "BBB")


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
  ;(gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;(gl-enable GL_BLEND)
  (gl-alpha-func GL_GREATER 0.5)
  (gl-enable GL_ALPHA_TEST)
  ;; テクスチャ設定
  ;(gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_REPLACE)
  ;(gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
  (set! *tex* (gl-gen-textures 2))
  (load-texture-bitmap-file (~ *tex* 0) (make-fpath *app-dpath* "image/char0001.bmp") '(0 0 0))
  (load-texture-bitmap-file (~ *tex* 1) (make-fpath *app-dpath* "image/char0002.bmp") '(0 0 0))
  (set-char-texture #\A (~ *tex* 0))
  (set-char-texture #\B (~ *tex* 1))
  )

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  ;; 文字に割り付けたテクスチャの一括表示
  (textscrn-disp-texture *tscrn1* *win* 0 0 (win-w *win* 50) (win-h *win* 50))
  ;; テクスチャ付き長方形の表示
  (draw-texture-rect *win* (~ *tex* 0) (win-x *win* -50) (win-y *win*  50)
                     (win-w *win* 200) (win-h *win* 200) 'center)
  (draw-texture-rect *win* (~ *tex* 1) (win-x *win*  50) (win-y *win* 150)
                     (win-w *win* 200) (win-h *win* 200) 'center)
  ;; 背景の表示
  (gl-color *backcolor*)
  (draw-win-rect *win* (win-w-r *win* 1/2) 0 *width* *height* 'center)
  ;(gl-flush)
  (glut-swap-buffers)
  )

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  (min w h))
  (set! *height* (min w h))
  (win-update-size *win* *width* *height*)
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

