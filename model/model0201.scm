;; -*- coding: utf-8 -*-
;;
;; モデル0201(人形)
;; 2017-2-22
;;
;(add-load-path ".." :relative)
;(use glmodelkit) ; box-model,cylinder用

;; モデル0201(人形)(頭に原点あり)(高さ100)
;;   pose  ポーズ(=0-7:はばたき)
(define (model0201 pose)
  ;; 色設定
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 0.0 1.0))
  ;; 頭
  (gl-push-matrix)
  (gl-translate   0 -10 0)
  (glut-solid-sphere 10 20 20)
  (gl-pop-matrix)
  ;; 胴体
  (gl-translate   0 -20 0)
  (box-model 10 20 7)
  ;; 右手
  (gl-push-matrix)
  (gl-translate -14 -13 0)
  (case pose
    ((0)  (gl-rotate   0 0 1 0))
    ((1)  (gl-rotate  30 0 1 0))
    ((2)  (gl-rotate  60 0 1 0))
    ((3)  (gl-rotate  30 0 1 0))
    ((4)  (gl-rotate   0 0 1 0))
    ((5)  (gl-rotate -30 0 1 0))
    ((6)  (gl-rotate -60 0 1 0))
    ((7)  (gl-rotate -30 0 1 0))
    )
  (gl-rotate -90 0 0 1)
  (box-model 12 22 2)
  (gl-pop-matrix)
  ;; 左手
  (gl-push-matrix)
  (gl-translate  14 -13 0)
  (case pose
    ((0)  (gl-rotate   0 0 1 0))
    ((1)  (gl-rotate -30 0 1 0))
    ((2)  (gl-rotate -60 0 1 0))
    ((3)  (gl-rotate -30 0 1 0))
    ((4)  (gl-rotate   0 0 1 0))
    ((5)  (gl-rotate  30 0 1 0))
    ((6)  (gl-rotate  60 0 1 0))
    ((7)  (gl-rotate  30 0 1 0))
    )
  (gl-rotate  90 0 0 1)
  (box-model 12 22 2)
  (gl-pop-matrix)
  ;; 右足
  (gl-push-matrix)
  (gl-translate  -5 -40 0)
  (cylinder 4 20 20)
  (gl-pop-matrix)
  ;; 左足
  (gl-push-matrix)
  (gl-translate   5 -40 0)
  (cylinder 4 20 20)
  (gl-pop-matrix)
  )

;;
;; 以下はモデルビューワー用
;;

(define *model-name*           "model0201")
(define *model-text-vec-A*     (make-vector  5 ""))
(define *model-text-vec-B*     (make-vector  5 ""))
(define *model-para-vec*       (make-vector 10 0))
(set! (~ *model-text-vec-A* 0) "  pose : 0")
(set! (~ *model-text-vec-B* 0) "[space] : change pose")
(set! (~ *model-para-vec*   0) 0) ; pose

;; モデルの表示
(define (viewer-disp)
  (model0201 (~ *model-para-vec* 0)))

;; キー入力ON
(define (viewer-keyboard)
  (when (key-on? *ksinfo* '#\space)
    (inc! (~ *model-para-vec* 0))
    (if (> (~ *model-para-vec* 0) 7) (set! (~ *model-para-vec* 0) 0))
    (set! (~ *model-text-vec-A* 0)
          (format "  pose : ~d" (~ *model-para-vec* 0)))
    )
  )

