# Gauche-gl-examples

![image](image0101.png)
![image](image0201.png)
![image](image0301.png)
![image](image0401.png)
![image](image0501.png)

## 概要
- Gauche-gl を使用したサンプルプログラムです。


## 内容
1. 内トロコイド曲線 ( trochoid.scm )  
   Gauche-gl を使って、内トロコイド曲線を描くサンプルです。  
   スペースキーを押すと、パラメータを乱数で生成して次を表示します。  
   ESCキーを押すと終了します。

2. 星の表示 ( planet.scm )  
   Gauche-gl を使って、星を表示するサンプルです。  
   ESCキーを押すと終了します。

3. 格闘ゲーム ( fighter.scm )  
   Gauche-gl を使用した、簡単な格闘ゲームです。  
   左側が自分になります。  
   矢印キーで左右移動。[z]キーでパンチ。[x]キーでキックです。  
   防御はありません。相打ちはノーダメージです。  
   パンチ後は下降中が無防備になります。  
   キック後は一定時間無防備になります。  
   Ready?の画面でしばらく待つとデモになります。  
   ESCキーを押すと終了します。

4. バッティングゲーム ( batting.scm )  
   Gauche-gl を使用した、バッティングゲームです。  
   矢印キーでカーソルを左右に移動し、  
   スペースキーでボールを打ちます。  
   引き付けるほど飛びます(最大199m)。ただし見逃しは0mです。  
   ESCキーを押すと終了します。

5. シューティングゲーム ( shooting.scm )  
   Gauche-gl を使用した、簡単なシューティングゲームです。  
   矢印キーで上下左右移動。  
   Ctrlキーかスペースキーでビーム発射です(押し続けると発射し続けます)。  
   敵は若干固いので、しばらくビームを当て続ける必要があります。  
   また、敵を破壊すると一定範囲が誘爆します。  
   画面右上のレベル表示は、出現する敵の数と速度の目安になります。  
   ESCキーを押すと終了します。


## 注意事項
1. 一部のサンプルプログラムは、glmintool.scm と gltextscrn.scm に依存しています。
   同一フォルダに配置してください。


## 環境等
- OS
  - Windows 8.1 (64bit)
- 言語
  - Gauche v0.9.4
  - Gauche v0.9.5_pre1

## 履歴
- 1001HISTORY.txt を参照ください。


(2016-2-8)
