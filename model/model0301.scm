;; -*- coding: utf-8 -*-
;;
;; モデル0301(簡易飛行機)
;; 2018-5-25
;;
(define-module model0301
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use math.const)
  (use glmintool)
  (use glmodelkit)
  (export
    model0301
    model0301-viewer-init
    ))
(select-module model0301)

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

(define *smoke* 0)

;; モデルビューワー情報の初期化
(define (model0301-viewer-init vwinfo)
  (set! (~ vwinfo 'model-name)      "model0301")
  (set! (~ vwinfo 'text-vec-A 0)    "  smoke : 0")
  (set! (~ vwinfo 'text-vec-B 0)    "[space] : change smoke")
  (set! (~ vwinfo 'viewer-disp)     viewer-disp)
  (set! (~ vwinfo 'viewer-keyboard) viewer-keyboard)
  )

;; モデルの表示
(define (viewer-disp vwinfo)
  (model0301 *smoke*))

;; キー入力ON
(define (viewer-keyboard vwinfo)
  (when (key-on? (~ vwinfo 'ksinfo) #\space)
    (inc! *smoke*)
    (if (> *smoke* 1) (set! *smoke* 0))
    (set! (~ vwinfo 'text-vec-A 0) (format "  smoke : ~d" *smoke*)))
  )

