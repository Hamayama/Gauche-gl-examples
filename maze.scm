;; -*- coding: utf-8 -*-
;;
;; maze.scm
;; 2017-8-10 v1.21
;;
;; ＜内容＞
;;   Gauche-gl を使用した、迷路を自動生成して表示するサンプルです。
;;   スタート(水色)からゴール(赤色)までのルートも探索して表示します。
;;   生成される迷路の上下左右はつながっています。
;;   スペースキーを押すと、次の迷路を表示します。
;;   ESCキーを押すと終了します。
;;
;;   参考「古くて新しい自動迷路生成アルゴリズム」
;;   http://d.hatena.ne.jp/yaneurao/20130125
;;
(add-load-path "lib" :relative)
(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)
(use glmintool)
(use gltextscrn)
(use glmazekit)

(define *title* "maze") ; ウィンドウのタイトル
(define *width*    480) ; ウィンドウ上の画面幅(px)
(define *height*   480) ; ウィンドウ上の画面高さ(px)

(define *mw*        30) ; 迷路の幅  (=水平方向のブロック数)
(define *mh*        30) ; 迷路の高さ(=垂直方向のブロック数)
(define *sx*         1) ; スタートのX座標
(define *sy*         1) ; スタートのY座標
(define *gx*         (quotient (+ *mw* 1) 2)) ; ゴールのX座標
(define *gy*         (quotient (+ *mh* 1) 2)) ; ゴールのY座標

(define *wd/2*     400) ; 画面幅/2
(define *ht/2*     400) ; 画面高さ/2
(define *wsize*     20) ; 壁の長さ
(define *wwide*      3) ; 壁の幅
(define *backcolor*  #f32(0.0 0.0 0.3 1.0)) ; 背景色
(define *wallcolor*  #f32(1.0 1.0 1.0 1.0)) ; 壁の色
(define *startcolor* #f32(0.0 0.7 1.0 1.0)) ; スタートの色
(define *goalcolor*  #f32(1.0 0.0 0.0 1.0)) ; ゴールの色
(define *routecolor* #f32(1.0 1.0 0.0 1.0)) ; 探索ルートの色

;; ウィンドウ情報クラスのインスタンス生成
(define *win* (make <wininfo>))
(win-init *win* *width* *height* (* *wd/2* 2) (* *ht/2* 2))

;; 迷路クラスのインスタンス生成
(define *maze* (make <maze>))


;; 迷路の表示
(define (disp-maze mz)
  (define mw    (~ mz 'width))  ; 迷路の幅
  (define mh    (~ mz 'height)) ; 迷路の高さ
  (define mdata (~ mz 'data))   ; 迷路データ
  (define (pt x y) (+ (* y mw) x)) ; 配列番号への変換

  (define ws (win-h *win* *wsize*)) ; ウィンドウ上の壁の長さ(px)
  (define ww (win-h *win* *wwide*)) ; ウィンドウ上の壁の幅(px)
  (define ox (/. (- *width*  (* ws mw)) 2)) ; ウィンドウ上の迷路の左上点のX座標(px)
  (define oy (/. (- *height* (* ws mh)) 2)) ; ウィンドウ上の迷路の左上点のY座標(px)

  ;; ブロックの数だけループする
  (let loop ((x1 0) (y1 0))
    (let ((bx1 (+ ox (* x1 ws))) ; ウィンドウ上の1ブロックの左上点のX座標(px)
          (by1 (+ oy (* y1 ws))) ; ウィンドウ上の1ブロックの左上点のY座標(px)
          (d1  (~ mdata (pt x1 y1)))) ; 1ブロックの迷路データ
      ;; 壁の表示
      (gl-color *wallcolor*)
      (if (logtest d1 1) (draw-win-rect bx1 (+ by1 (/. ww -2))    ws ww *width* *height*))
      (if (logtest d1 2) (draw-win-rect (+ bx1 ws (/. ww -2)) by1 ww ws *width* *height*))
      (if (logtest d1 4) (draw-win-rect bx1 (+ by1 ws (/. ww -2)) ws ww *width* *height*))
      (if (logtest d1 8) (draw-win-rect (+ bx1 (/. ww -2)) by1    ww ws *width* *height*))
      ;; 1ブロックの背景の表示
      (gl-color (cond ((logtest d1 128) *goalcolor*)
                      ((logtest d1  64) *startcolor*)
                      ((logtest d1  32) *routecolor*)
                      (else             *backcolor*)))
      (draw-win-rect bx1 by1 ws ws *width* *height* 'left -0.99999)
      )
    (cond
     ((< x1 (- mw 1)) (loop (+ x1 1) y1))
     ((< y1 (- mh 1)) (loop 0 (+ y1 1))))
    ))


;; 初期化
(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-enable GL_DEPTH_TEST)
  ;; 迷路の初期化
  (maze-init      *maze* *mw* *mh*)
  ;; 迷路の生成
  (maze-generate  *maze* disp)
  ;; 画面表示
  (disp)
  ;; 迷路の探索
  (maze-set-start *maze* *sx* *sy*)
  (maze-set-goal  *maze* *gx* *gy*)
  (maze-search    *maze*)
  ;; 画面表示
  (disp))

;; 画面表示
(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  ;; 迷路の表示
  (disp-maze *maze*)
  ;(gl-flush)
  (glut-swap-buffers))

;; 画面のリサイズ
(define (reshape w h)
  (set! *width*  w)
  (set! *height* (min w h))
  (win-update-size *win* *width* *height*)
  ;; 縦横比を変えずにリサイズ
  (gl-viewport 0 (quotient (- h *height*) 2) *width* *height*))

;; キー入力
(define (keyboard key x y)
  (cond
   ;; ESCキーで終了
   ((= key (char->integer #\escape)) (exit 0))
   ;; スペースキーで次を表示
   ((= key (char->integer #\space))
    ;; 迷路の初期化
    (maze-init      *maze* *mw* *mh*)
    ;; 迷路の生成
    (maze-generate  *maze* disp)
    ;; 画面表示
    (disp)
    ;; 迷路の探索
    (maze-set-start *maze* *sx* *sy*)
    (maze-set-goal  *maze* *gx* *gy*)
    (maze-search    *maze*)
    ;; 画面表示
    (disp))
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

