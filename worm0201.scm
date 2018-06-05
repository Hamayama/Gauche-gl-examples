;; -*- coding: utf-8 -*-
;;
;; worm0201.scm
;; 2018-6-5 v1.30
;;
;; ＜内容＞
;;   Gauche-gl を使用した、ワームシミュレータです。
;;   (先端から移動していくタイプ)
;;   矢印キーかマウスボタン1でカーソルを移動します。
;;   空腹状態のワームはカーソルを追跡します。
;;   ESCキーを押すと終了します。
;;

;; 実際の処理は worm0101.scm に統合
(add-load-path "." :relative)
(load "worm0101.scm")

;; ワームの種別の設定
(set! *wormkind* 1)

;; ワームクラスの再定義
;; (ワーム0201クラスを継承)
(define-class <worm> (<worm0201>)
  ((state  :init-value      0) ; 状態(=0:追跡中,=1:食事中,=2:ランダム動作中)
   (count1 :init-value      0) ; 動作カウンタ1
   (count2 :init-value      0) ; 動作カウンタ2
   (wtime1 :init-value   1000) ; 食事時間(msec)
   (wtime2 :init-value   8000) ; ランダム動作時間最小値(msec)
   (wtime3 :init-value  15000) ; ランダム動作時間最大値(msec)
   (wtime4 :init-value   2000) ; ランダム動作切換時間最小値(msec)
   (wtime5 :init-value   8000) ; ランダム動作切換時間最大値(msec)
   ))

;; メイン処理を実行
(main (command-line))

