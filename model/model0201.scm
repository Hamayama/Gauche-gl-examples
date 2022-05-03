;; -*- coding: utf-8 -*-
;;
;; モデル0201(人形)
;; 2022-5-3
;;
(define-module model0201
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use math.const)
  (use glmintool)
  (use glmodelkit)
  (export
    model0201
    model0201-viewer-init
    ))
(select-module model0201)

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
  (gl-push-matrix)
  (gl-translate   0 -20 0)
  (box-model 10 20 7)
  (gl-pop-matrix)
  ;; 右手
  (gl-push-matrix)
  (gl-translate -14 -33 0)
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
  (gl-translate  14 -33 0)
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
  (gl-translate  -5 -60 0)
  (cylinder 4 20 20)
  (gl-pop-matrix)
  ;; 左足
  (gl-push-matrix)
  (gl-translate   5 -60 0)
  (cylinder 4 20 20)
  (gl-pop-matrix)
  )

;;
;; 以下はモデルビューワー用
;;

(define *pose* 0)

;; モデルビューワー情報の初期化
(define (model0201-viewer-init vwinfo)
  (set! (~ vwinfo 'model-name)      "model0201")
  (set! (~ vwinfo 'text-vec-A 0)    "  pose : 0")
  (set! (~ vwinfo 'text-vec-B 0)    "[space] : change pose")
  (set! (~ vwinfo 'viewer-disp)     viewer-disp)
  (set! (~ vwinfo 'viewer-keyboard) viewer-keyboard)
  )

;; モデルの表示
(define (viewer-disp vwinfo)
  (model0201 *pose*))

;; キー入力ON
(define (viewer-keyboard vwinfo)
  (when (key-on? (~ vwinfo 'ksinfo) #\space)
    (inc! *pose*)
    (if (> *pose* 7) (set! *pose* 0))
    (set! (~ vwinfo 'text-vec-A 0) (format "  pose : ~d" *pose*)))
  )

