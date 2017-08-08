;; -*- coding: utf-8 -*-
;;
;; テキスト画面クラスのテスト
;; 2017-3-25
;;
(add-load-path ".." :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use glmintool)
(use gltextscrn)

(define *title* "test-tscrn") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *chw*       16) ; 文字の幅
(define *chh*       32) ; 文字の高さ
(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 2) (* *ht/2* 2))

;; テキスト画面クラスのインスタンス生成
(define *tscrn1* (make <textscrn>))
(textscrn-init    *tscrn1* 50 25)
(textscrn-cls     *tscrn1*)
(textscrn-pset    *tscrn1*  0  0 (list->string (map integer->char (iota 32 32))))
(textscrn-pset    *tscrn1*  0  1 (list->string (map integer->char (iota 32 64))))
(textscrn-pset    *tscrn1*  0  2 (list->string (map integer->char (iota 32 96))))
(textscrn-pset    *tscrn1* 44  0 (textscrn-pget *tscrn1* 1 1 5))
(textscrn-line    *tscrn1* 12  6 49 24 "*=")
(textscrn-box     *tscrn1*  0  3  4  7 "012")
(textscrn-fbox    *tscrn1*  6  3 10  7 "012")
(textscrn-circle  *tscrn1* 26  6  7  1  2 "o")
(textscrn-fcircle *tscrn1* 41  6  7  1  2 "o")
(textscrn-poly    *tscrn1* '(( 3 9) (0 13) ( 6 13)) "xyz")
(textscrn-fpoly   *tscrn1* '((11 9) (8 13) (14 13)) "xyz")
(textscrn-fpoly   *tscrn1* '((10 15) (4 24) (19 18) (1 18) (16 24)) "#=")
(define *tscrn2* (make <textscrn>))
(textscrn-init    *tscrn2*  5  5)
(textscrn-pset    *tscrn2*  0  0 "ABC")
(textscrn-pset    *tscrn2* -1  1 "ABC")
(textscrn-pset    *tscrn2*  3  2 "ABC")
(textscrn-pset    *tscrn2* -3  3 "ABC")
(textscrn-pset    *tscrn2*  5  3 "ABC")
(textscrn-pset    *tscrn2* -1  4 "1234567")
(textscrn-box     *tscrn2* -1 -1  5  5 "x")


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST))

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  ;; テキスト画面の表示
  (gl-color 1.0 1.0 1.0 1.0)
  (textscrn-disp *tscrn1* 0 0 *width* *height*
                 (win-w *win* *chw*) (win-h *win* *chh*))
  (gl-color 0.0 1.0 1.0 1.0)
  (textscrn-disp *tscrn2* (win-x *win* 0) (win-y *win* 0) *width* *height*
                 (win-w *win* *chw*) (win-h *win* *chh*) 'right)
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

