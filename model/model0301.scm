;; -*- coding: utf-8 -*-
;;
;; モデル0301(簡易飛行機)
;; 2017-3-26
;;
;(add-load-path ".." :relative)
;(use glmodelkit) ; box-model用

;; モデル0301(簡易飛行機)(胴体と羽の交点に原点あり)(胴体の長さ80)
;;   smoke  煙状態(=0:OFF,=1:ON)
(define (model0301 smoke)
  ;; 色設定
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 0.0 1.0))
  ;; 胴体
  (gl-push-matrix)
  (gl-translate 0 30 0)
  (box-model 10 40 10)
  (gl-pop-matrix)
  ;; 羽
  (gl-push-matrix)
  (gl-translate 0 10 0)
  (box-model 36 10 10)
  (gl-pop-matrix)
  ;; 煙
  (when (> smoke 0)
    (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 1.0 1.0))
    (gl-push-matrix)
    (gl-translate  0 -60 0)
    (glut-solid-sphere 7 20 20)
    (gl-translate -8 -15 0)
    (glut-solid-sphere 8 20 20)
    (gl-translate 16   0 0)
    (glut-solid-sphere 8 20 20)
    (gl-pop-matrix)
    )
  )

;;
;; 以下はモデルビューワー用
;;

(define *model-name* "model0301")
(define *model-text-vec-A*     (make-vector  5 ""))
(define *model-text-vec-B*     (make-vector  5 ""))
(define *model-para-vec*       (make-vector 10 0))
(set! (~ *model-text-vec-A* 0) "  smoke : 0")
(set! (~ *model-text-vec-B* 0) "[space] : change smoke")
(set! (~ *model-para-vec*   0) 0) ; smoke

;; モデルの表示
(define (viewer-disp)
  (model0301 (~ *model-para-vec* 0)))

;; キー入力ON
(define (viewer-keyboard)
  (when (key-on? *ksinfo* '#\space)
    (inc! (~ *model-para-vec* 0))
    (if (> (~ *model-para-vec* 0) 1) (set! (~ *model-para-vec* 0) 0))
    (set! (~ *model-text-vec-A* 0)
          (format "  smoke : ~d" (~ *model-para-vec* 0)))
    )
  )

