;; -*- coding: utf-8 -*-
;;
;; モデル0401(標準のティーポット)
;; 2018-3-3
;;

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

(define *model-name* "model0401")
(define *model-text-vec-A*     (make-vector  5 ""))
(define *model-text-vec-B*     (make-vector  5 ""))
(define *model-para-vec*       (make-vector 10 0))
(set! (~ *model-text-vec-A* 0) "  color : 0")
(set! (~ *model-text-vec-B* 0) "[space] : change color")
(set! (~ *model-para-vec*   0) 0) ; color

;; モデルの表示
(define (viewer-disp)
  (model0401 (~ *model-para-vec* 0)))

;; キー入力ON
(define (viewer-keyboard)
  (when (key-on? *ksinfo* '#\space)
    (inc! (~ *model-para-vec* 0))
    (if (> (~ *model-para-vec* 0) 6) (set! (~ *model-para-vec* 0) 0))
    (set! (~ *model-text-vec-A* 0)
          (format "  color : ~d" (~ *model-para-vec* 0)))
    )
  )

