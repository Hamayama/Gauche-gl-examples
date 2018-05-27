;; -*- coding: utf-8 -*-
;;
;; モデル0101(人形)
;; 2018-5-25
;;
(define-module model0101
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use math.const)
  (use glmintool)
  (use glmodelkit)
  (export
    model0101
    model0101-viewer-init
    ))
(select-module model0101)

;; モデル0101(人形)(頭に原点あり)(高さ100)
;;   type  タイプ(=0:自分,=1:敵)
;;   pose  ポーズ(=0:通常,=1:前進,=2:後退,=3:やられ,=4:パンチ,=5:キック)
(define (model0101 type pose)
  ;; 色設定
  (case type
    ((0) (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 1.0 1.0)))
    ((1) (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 0.0 1.0)))
    )
  ;; 頭
  (gl-push-matrix)
  (case type
    ((0) (gl-translate 0 -10 0) (glut-solid-sphere 10 20 20))
    ((1) (box-model 8 8 8))
    )
  (gl-pop-matrix)
  ;; 胴体
  (gl-translate 0 -20 0)
  (box-model 10 20 10)
  ;; 右手
  (gl-push-matrix)
  (gl-translate -15 0 0)
  (case pose
    ((0) (gl-rotate -15  0 0 1))
    ((1) (gl-rotate -25  1 0 1))
    ((2) (gl-rotate -25 -1 0 1))
    ((3) (gl-rotate -25  1 0 1))
    ((4) (gl-translate 0 -5 10) (gl-rotate -135 1 0 0.1))
    ((5) (gl-rotate -25 -1 0 1))
    )
  (cylinder 4 20 20)
  (gl-pop-matrix)
  ;; 左手
  (gl-push-matrix)
  (gl-translate  15 0 0)
  (case pose
    ((0) (gl-rotate  15  0 0 1))
    ((1) (gl-rotate  25  1 0 1))
    ((2) (gl-rotate  25 -1 0 1))
    ((3) (gl-rotate  25 -1 0 1))
    ((4) (gl-rotate  25  1 0 1))
    ((5) (gl-rotate  25 -1 0 1))
    )
  (cylinder 4 20 20)
  (gl-pop-matrix)
  ;; 右足
  (gl-push-matrix)
  (gl-translate -5 -40 0)
  (case pose
    ((2) (gl-rotate -20  1 0 0))
    ((3) (gl-rotate -35  1 0 0.1))
    ((5) (gl-translate 0 0 10) (gl-rotate -100 1 0 0))
    )
  (cylinder 4 20 20)
  (gl-pop-matrix)
  ;; 左足
  (gl-push-matrix)
  (gl-translate  5 -40 0)
  (case pose
    ((1) (gl-rotate  30  1 0 0))
    ((3) (gl-rotate  35 -1 0 0.1))
    ((4) (gl-rotate  40  1 0 0))
    ((5) (gl-rotate  40  1 0 0))
    )
  (cylinder 4 20 20)
  (gl-pop-matrix)
  )

;;
;; 以下はモデルビューワー用
;;

(define *type* 0)
(define *pose* 0)

;; モデルビューワー情報の初期化
(define (model0101-viewer-init vwinfo)
  (set! (~ vwinfo 'model-name)      "model0101")
  (set! (~ vwinfo 'text-vec-A 0)    "  type : 0")
  (set! (~ vwinfo 'text-vec-A 1)    "  pose : 0 (normal)")
  (set! (~ vwinfo 'text-vec-B 0)    "[z] : change type")
  (set! (~ vwinfo 'text-vec-B 1)    "[space] : change pose")
  (set! (~ vwinfo 'viewer-disp)     viewer-disp)
  (set! (~ vwinfo 'viewer-keyboard) viewer-keyboard)
  )

;; モデルの表示
(define (viewer-disp vwinfo)
  (model0101 *type* *pose*))

;; キー入力ON
(define (viewer-keyboard vwinfo)
  (when (key-on? (~ vwinfo 'ksinfo) '(#\z #\Z))
    (inc! *type*)
    (if (> *type* 1) (set! *type* 0))
    (set! (~ vwinfo 'text-vec-A 0) (format "  type : ~d" *type*)))
  (when (key-on? (~ vwinfo 'ksinfo) #\space)
    (inc! *pose*)
    (if (> *pose* 5) (set! *pose* 0))
    (set! (~ vwinfo 'text-vec-A 1)
          (format "  pose : ~a" (case *pose*
                                  ((0) "0 (normal)")
                                  ((1) "1 (foreward)")
                                  ((2) "2 (backword)")
                                  ((3) "3 (down)")
                                  ((4) "4 (punch)")
                                  ((5) "5 (kick)")))))
  )

