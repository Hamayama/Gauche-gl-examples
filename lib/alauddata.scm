;; -*- coding: utf-8 -*-
;;
;; alauddata.scm
;; 2017-8-6 v1.05
;;
;; ＜内容＞
;;   alaudplay 用の音楽データをいくつか設定するためのモジュールです。
;;
(define-module alauddata
  (use alaudplay)
  (use glmintool)
  (export
    init-auddata
    *adata-start1*
    *adata-start2*
    *adata-brake1*
    *adata-hit1*
    *adata-hit2*
    *adata-hit3*
    *adata-end1*
    *adata-end2*
    *adata-end3*
    ))
(select-module alauddata)

;; 音楽データクラスのインスタンス生成
(define *adata-start1* (make <auddata>)) ; スタート(汎用)
(define *adata-start2* (make <auddata>)) ; スタート(バッティング用)
(define *adata-brake1* (make <auddata>)) ; ブレーキ(ドライブ用)
(define *adata-hit1*   (make <auddata>)) ; ヒット(汎用)
(define *adata-hit2*   (make <auddata>)) ; ヒット(ジャンプ用)
(define *adata-hit3*   (make <auddata>)) ; ヒット(バッティング用)
(define *adata-end1*   (make <auddata>)) ; エンド(汎用)
(define *adata-end2*   (make <auddata>)) ; エンド(バッティング,ドライブ用)
(define *adata-end3*   (make <auddata>)) ; エンド(フライト用)

;; 音楽データの初期化
(define (init-auddata app-dpath)
  (auddata-load-wav-file *adata-start1* (make-fpath app-dpath "sound/appear1B.wav"))
  (auddata-set-prop *adata-start1* AL_GAIN  0.25)
  ;(auddata-set-prop *adata-start1* AL_PITCH 2.75)
  (auddata-load-wav-file *adata-start2* (make-fpath app-dpath "sound/warp1B.wav"))
  (auddata-set-prop *adata-start2* AL_GAIN  0.30)
  ;(auddata-set-prop *adata-start2* AL_PITCH 1.2)
  (auddata-load-wav-file *adata-brake1* (make-fpath app-dpath "sound/cursor4.wav"))
  (auddata-set-prop *adata-brake1* AL_GAIN  0.30)
  (auddata-load-wav-file *adata-hit1*   (make-fpath app-dpath "sound/decide2B.wav"))
  (auddata-set-prop *adata-hit1*   AL_GAIN  0.50)
  ;(auddata-set-prop *adata-hit1*   AL_PITCH 1.1)
  (auddata-load-wav-file *adata-hit2*   (make-fpath app-dpath "sound/decide2C.wav"))
  (auddata-set-prop *adata-hit2*   AL_GAIN  0.50)
  ;(auddata-set-prop *adata-hit2*   AL_PITCH 1.05)
  (auddata-load-wav-file *adata-hit3*   (make-fpath app-dpath "sound/pattern05B.wav"))
  (auddata-set-prop *adata-hit3*   AL_GAIN  0.50)
  ;(auddata-set-prop *adata-hit3*   AL_PITCH 2.0)
  (auddata-load-wav-file *adata-end1*   (make-fpath app-dpath "sound/pattern05C.wav"))
  (auddata-set-prop *adata-end1*   AL_GAIN  0.65)
  ;(auddata-set-prop *adata-end1*   AL_PITCH 1.3)
  (auddata-load-wav-file *adata-end2*   (make-fpath app-dpath "sound/pattern03.wav"))
  (auddata-set-prop *adata-end2*   AL_GAIN  0.60)
  (auddata-load-wav-file *adata-end3*   (make-fpath app-dpath "sound/decide10.wav"))
  (auddata-set-prop *adata-end3*   AL_GAIN  0.65)
  )

