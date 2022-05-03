;; -*- coding: utf-8 -*-
;;
;; モデル0501(欠けた球)
;; 2022-5-3
;;
(define-module model0501
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use math.const)
  (use glmintool)
  (export
    model0501
    model0501-viewer-init
    ))
(select-module model0501)

;; モデル0501(欠けた球)(中心に原点あり)
;;   r      半径
;;   slice  y軸のまわりの分割数
;;   stack  y軸に垂直な分割数
;;   wedge  欠けの大きさ(角度)
(define (model0501 r slice stack wedge)
  (define f32 f32vector)
  (define s1 stack)
  (define s2 slice)
  (define w1 (/. (* wedge pi/180) 2))
  (define step1 (/.  pi s1))
  ;(define step2 (/. 2pi s2))
  (define step2 (* (/. (- pi w1) s2) 2))
  (define vertex (make-vector (* (+ s1 1) (+ s2 1))))
  (define normal (make-vector (* (+ s1 1) (+ s2 1))))
  ;; 座標計算
  (let ((idx 0) (r1 0) (x1 0) (y1 0) (z1 0))
    (do ((i 0 (+ i 1))
         (angle1 0 (if (< (+ i 1) s1) (+ angle1 step1) pi)))
        ((> i s1) #f)
      (set! y1 (cos angle1))
      (set! r1 (sin angle1))
      (do ((j 0 (+ j 1))
           ;(angle2 0 (if (< (+ j 1) s2) (+ angle2 step2) 0)))
           (angle2 w1 (if (< (+ j 1) s2) (+ angle2 step2) (- 2pi w1))))
          ((> j s2) #f)
        (set! x1 (* r1 (cos angle2)))
        (set! z1 (* r1 (sin angle2)))
        (set! (~ vertex idx) (f32 (* r x1) (* r y1) (* r z1)))
        (set! (~ normal idx) (f32 x1 y1 z1))
        (inc! idx))))
  ;; 球面
  (let ((idx 0) (idx2 0) (idx3 0) (idx4 0))
    (do ((i 0 (+ i 1)))
        ((>= i s1) #f)
      (gl-begin GL_TRIANGLE_STRIP)
      (do ((j 0 (+ j 1)))
          ((>= j s2) #f)
        (set! idx2 (+ idx s2 1))
        (set! idx3 (+ idx 1))
        (set! idx4 (+ idx s2 2))
        (gl-normal (~ normal idx))
        (gl-vertex (~ vertex idx))
        (gl-normal (~ normal idx2))
        (gl-vertex (~ vertex idx2))
        (gl-normal (~ normal idx3))
        (gl-vertex (~ vertex idx3))
        (gl-normal (~ normal idx4))
        (gl-vertex (~ vertex idx4))
        (inc! idx))
      (inc! idx)
      (gl-end)))
  ;; 断面1
  (let ((idx 0))
    (gl-begin GL_TRIANGLE_FAN)
    (gl-normal (f32 (sin w1) 0 (- (cos w1))))
    (gl-vertex #f32(0 0 0))
    (do ((i 0 (+ i 1)))
        ((> i s1) #f)
      (gl-vertex (~ vertex idx))
      (set! idx (+ idx s2 1)))
    (gl-end))
  ;; 断面2
  (let ((idx s2))
    (gl-begin GL_TRIANGLE_FAN)
    (gl-normal (f32 (sin w1) 0 (cos w1)))
    (gl-vertex #f32(0 0 0))
    (do ((i 0 (+ i 1)))
        ((> i s1) #f)
      (gl-vertex (~ vertex idx))
      (set! idx (+ idx s2 1)))
    (gl-end))
  )

;;
;; 以下はモデルビューワー用
;;

(define *wedge-index* 0)
(define *wedge-data*  '(90 60 30 0 30 60))

;; モデルビューワー情報の初期化
(define (model0501-viewer-init vwinfo)
  (set! (~ vwinfo 'model-name)      "model0501")
  (set! (~ vwinfo 'text-vec-A 0)    "  wedge : 90")
  (set! (~ vwinfo 'text-vec-B 0)    "[space] : change wedge")
  (set! (~ vwinfo 'viewer-disp)     viewer-disp)
  (set! (~ vwinfo 'viewer-keyboard) viewer-keyboard)
  )

;; モデルの表示
(define (viewer-disp vwinfo)
  ;(glut-wire-sphere 50 5 10)
  ;(glut-solid-sphere 50 5 10)
  ;(model0501 50 20 20 90)
  (model0501 50 15 10 (list-ref *wedge-data* *wedge-index*))
  )

;; キー入力ON
(define (viewer-keyboard vwinfo)
  (when (key-on? (~ vwinfo 'ksinfo) #\space)
    (inc! *wedge-index*)
    (if (>= *wedge-index* (length *wedge-data*)) (set! *wedge-index* 0))
    (set! (~ vwinfo 'text-vec-A 0) (format "  wedge : ~d" (list-ref *wedge-data* *wedge-index*))))
  )

