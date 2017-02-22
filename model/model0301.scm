;; -*- coding: utf-8 -*-
;;
;; モデル0301(簡易飛行機)
;; 2017-2-22
;;
;(add-load-path ".." :relative)
;(use glmodelkit) ; box-model用

;; モデル0301(簡易飛行機)(胴体と羽の交点に原点あり)(胴体の長さ80)
(define (model0301)
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
  )

;;
;; 以下はモデルビューワー用
;;

(define *model-name* "model0301")

;; モデルの表示
(define (viewer-disp)
  (model0301))

