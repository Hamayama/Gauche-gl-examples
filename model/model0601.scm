;; -*- coding: utf-8 -*-
;;
;; モデル0601(自機)
;; 2022-4-23
;;
(define-module model0601
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use math.const)
  (use glmintool)
  (use glmodelkit)
  (export
    model0601
    model0601-viewer-init
    ))
(select-module model0601)

;; モデル0601(自機)(先端に原点あり)(幅chw*3)(高さchh*2)
;;   type  タイプ(=0:通常,=1:破壊)
(define (model0601 type chw chh)
  (define r     (/. chw 2))
  (define h     (/. chh 2))
  (define slice 20)
  ;; 色設定
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 0.0 1.0))
  ;; 先端
  (gl-push-matrix)
  (gl-translate 0 (* h -1.9) 0)
  (gl-rotate -90  1 0 0)
  (glut-solid-cone (* r 1.1) (* h 1.9) slice slice)
  (gl-pop-matrix)
  ;; 胴体
  (gl-push-matrix)
  (gl-translate 0        (* h -2) 0)
  (gl-translate 0 (* h -0.1) 0)
  (cylinder  (* r 0.5) (* h 0.9) slice)
  (gl-pop-matrix)
  ;; 左エンジン
  (gl-push-matrix)
  (gl-translate (* r -2) (* h -2) 0)
  (gl-translate 0 (* h -0.1) 0)
  (gl-rotate  45  0 1 0)
  (box-model (* r 0.8) (* h 0.9) (* r 0.8))
  (gl-pop-matrix)
  ;; 右エンジン
  (gl-push-matrix)
  (gl-translate (* r  2) (* h -2) 0)
  (gl-translate 0 (* h -0.1) 0)
  (gl-rotate  45  0 1 0)
  (box-model (* r 0.8) (* h 0.9) (* r 0.8))
  (gl-pop-matrix)
  ;; 爆発(別途、透過設定が必要)
  (when (= type 1)
    (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 0.0 0.0 0.5))
    (gl-push-matrix)
    (gl-translate 0 (* h -2) 0)
    (glut-solid-sphere (* r 5) 20 20) 
    (gl-pop-matrix))
  )

;;
;; 以下はモデルビューワー用
;;

(define *type* 0)

;; モデルビューワー情報の初期化
(define (model0601-viewer-init vwinfo)
  (set! (~ vwinfo 'model-name)      "model0101")
  (set! (~ vwinfo 'text-vec-A 0)    "  type : 0")
  (set! (~ vwinfo 'text-vec-B 0)    "[z] : change type")
  (set! (~ vwinfo 'viewer-init)     viewer-init)
  (set! (~ vwinfo 'viewer-disp)     viewer-disp)
  (set! (~ vwinfo 'viewer-keyboard) viewer-keyboard)
  )

;; 初期化
(define (viewer-init vwinfo)
  ;; 透過設定
  (gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (gl-enable GL_BLEND)
  )

;; モデルの表示
(define (viewer-disp vwinfo)
  (model0601 *type* 20 40))

;; キー入力ON
(define (viewer-keyboard vwinfo)
  (when (key-on? (~ vwinfo 'ksinfo) '(#\z #\Z))
    (inc! *type*)
    (if (> *type* 1) (set! *type* 0))
    (set! (~ vwinfo 'text-vec-A 0) (format "  type : ~d" *type*)))
  )

