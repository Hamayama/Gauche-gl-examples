;; -*- coding: utf-8 -*-
;;
;; 画像表示のテスト
;; 2017-8-10
;;
(add-load-path "../lib" :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use glmintool)
(use gltextscrn)

(define *title* "test-image") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色

;; アプリのディレクトリのパス名
(define *app-dpath* (if-let1 path (current-load-path) (sys-dirname path) ""))

;; テクスチャデータクラスのインスタンス生成
(define *tex* (make-vector-of-class 3 <texdata>))

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 2) (* *ht/2* 2))

;; テキスト画面クラスのインスタンス生成
(define *tscrn1* (make <textscrn>))
(textscrn-init *tscrn1* 5 4)
(textscrn-pset *tscrn1* 0 0 "AAAAA")
(textscrn-pset *tscrn1* 0 1 "BBBBB")
(textscrn-pset *tscrn1* 0 2 "CCCCC")
(textscrn-pset *tscrn1* 0 3 "DDDDD")


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 透過設定
  ;(gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;(gl-enable GL_BLEND)
  (gl-alpha-func GL_GREATER 0.1)
  (gl-enable GL_ALPHA_TEST)
  ;; テクスチャ設定
  ;(gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_REPLACE)
  ;(gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
  (load-texture-bitmap-file (~ *tex* 0) (make-fpath *app-dpath* "../image/char0001.bmp") '(0 0 0))
  (load-texture-bitmap-file (~ *tex* 1) (make-fpath *app-dpath* "../image/char0002.bmp") '(0 0 0))
  (load-texture-bitmap-file (~ *tex* 2) (make-fpath *app-dpath* "../image/char0003.bmp") '(0 0 0))
  (set-char-texture #\A (~ *tex* 0))
  (set-char-texture #\B (~ *tex* 1))
  (set-char-texture #\C (~ *tex* 2) 1.0 1.0  0.5 1.0 0.25 0.0)
  (set-char-texture #\D (~ *tex* 2) 1.0 1.0 -0.5 1.0 0.75 0.0))

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  ;; 文字に割り付けたテクスチャの一括表示
  (textscrn-disp-texture *tscrn1* 0 0 *width* *height* (win-w *win* 50) (win-h *win* 50))
  ;; テクスチャ付き長方形の表示
  (draw-texture-rect (~ *tex* 0) (win-x *win* -50) (win-y *win*  50)
                     (win-w *win* 200) (win-h *win* 200) *width* *height* 'center)
  (draw-texture-rect (~ *tex* 1) (win-x *win*  50) (win-y *win* 150)
                     (win-w *win* 200) (win-h *win* 200) *width* *height* 'center)
  (draw-texture-rect (~ *tex* 2) (win-x *win* 150) (win-y *win* 250)
                     (win-w *win* 100) (win-h *win* 200) *width* *height* 'center)
  ;; 背景の表示
  (gl-color *backcolor*)
  (draw-win-rect 0 0 *width* *height* *width* *height*)
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  (min w h))
  (set! *height* (min w h))
  (win-update-size *win* *width* *height*)
  ;; 縦横比を変えずにリサイズ
  (gl-viewport (quotient (- w *width*) 2) (quotient (- h *height*) 2) *width* *height*))

;; キー入力ON
(define (keyboard key x y)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit 0))
   ;; [g]キーでGC実行(デバッグ用)
   ((or (= key (char->integer #\g)) (= key (char->integer #\G)))
    (gc) (print (gc-stat)))
   ))

;; メイン処理
(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size *width* *height*)
  (glut-init-window-position 100 100)
  (glut-create-window *title*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  ;; コールバック内エラー対策
  (guard (ex (else (report-error ex) (exit 1)))
    (glut-main-loop))
  0)

