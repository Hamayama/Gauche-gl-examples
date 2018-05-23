;; -*- coding: utf-8 -*-
;;
;; モデル0401(標準のティーポット)
;; 2018-5-24
;;
(define-module model0401
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use gauche.collection)
  (use math.const)
  (use glmintool)
  ;(use glmodelkit)
  (export
    model0401
    model0401-viewer-init
    ))
(select-module model0401)

;; モデル0401(標準のティーポット)(中心付近に原点あり)(半径50程度)
;;   color  色(=0:白,=1:赤,=2:緑,=3:青,=4:黄色,=5:ピンク,=6:水色)
(define (model0401 color)
  ;; 色設定
  (case color
    ((0)  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 1.0 1.0)))
    ((1)  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 0.0 0.0 1.0)))
    ((2)  (gl-material GL_FRONT GL_DIFFUSE #f32(0.0 1.0 0.0 1.0)))
    ((3)  (gl-material GL_FRONT GL_DIFFUSE #f32(0.0 0.0 1.0 1.0)))
    ((4)  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 0.0 1.0)))
    ((5)  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 0.0 1.0 1.0)))
    ((6)  (gl-material GL_FRONT GL_DIFFUSE #f32(0.0 1.0 1.0 1.0)))
    (else (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 1.0 1.0))))
  ;; モデル表示
  (glut-solid-teapot 50)
  )

;;
;; 以下はモデルビューワー用
;;

(define *color* 0)

;; モデルビューワー情報の初期化
(define (model0401-viewer-init vwinfo)
  (set! (~ vwinfo 'model-name)      "model0401")
  (set! (~ vwinfo 'text-vec-A 0)    "  color : 0")
  (set! (~ vwinfo 'text-vec-B 0)    "[space] : change color")
  (set! (~ vwinfo 'viewer-disp)     viewer-disp)
  (set! (~ vwinfo 'viewer-keyboard) viewer-keyboard)
  )

;; モデルの表示
(define (viewer-disp vwinfo)
  (model0401 *color*))

;; キー入力ON
(define (viewer-keyboard vwinfo)
  (when (key-on? (~ vwinfo 'ksinfo) #\space)
    (inc! *color*)
    (if (> *color* 6) (set! *color* 0))
    (set! (~ vwinfo 'text-vec-A 0) (format "  color : ~d" *color*)))
  )

