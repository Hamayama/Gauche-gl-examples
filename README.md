# Gauche-gl-examples

|   |   |   |   |
|---|---|---|---|
|![image](image/image0101.png)|![image](image/image0201.png)|![image](image/image0301.png)|![image](image/image0401.png)|
|![image](image/image0501.png)|![image](image/image0601.png)|![image](image/image0701.png)|![image](image/image0801.png)|
|![image](image/image0901.png)|![image](image/image1001.png)|![image](image/image1101.png)|![image](image/image1201.png)|
|![image](image/image1301.png)|![image](image/image1401.png)|![image](image/image1501.png)|![image](image/image1601.png)|
|![image](image/image1701.png)|![image](image/image1801.png)|![image](image/image1901.png)||

## 概要
- Gauche-gl を使用したサンプルプログラム等です。  
  Gauche-gl は、Gauche から OpenGL を使用するための拡張ライブラリです。


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

5. シューティングゲーム ( shooting0101.scm )  
   Gauche-gl を使用した、簡単なシューティングゲームです。  
   矢印キーで上下左右移動。  
   [Ctrl]/[Space]/[a]/[z]キーのいずれかでビーム発射です(押し続けると発射し続けます)。  
   敵は若干固いので、しばらくビームを当て続ける必要があります。  
   また、敵を破壊すると一定範囲が誘爆します。  
   画面右上のレベル表示は、出現する敵の数と速度の目安になります。  
   また、スタート画面でしばらく待つとデモになります。  
   ESCキーを押すと終了します。

6. ドライブゲーム ( drive.scm )  
   Gauche-gl を使用した、簡単なドライブゲームです。  
   矢印キーで左右移動。  
   スペースキーでブレーキをかけます。  
   アクセルは自動です。スピードが上がると曲がりにくくなります。  
   ゴールするかコースアウトするとゲーム終了です。  
   (道路の端が画面の中心に来ると、コースアウトと判定されます)  
   ESCキーを押すと終了します。

7. ジャンプアクション ( jump.scm )  
   Gauche-gl を使用した、簡単なジャンプアクションゲームです。  
   矢印キーで左右移動。  
   スペースキーでジャンプします(長押しすると高く飛びます)。  
   雲には乗ることができます。  
   一定時間で出たり消えたりするゴールに触れるとステージクリアです。  
   飛んでいる敵に触れるとゲームオーバーです。  
   (ただし、敵の頭には乗ることができ、乗ると色が変わって無力化します)  
   ESCキーを押すと終了します。

8. シューティングゲーム2 ( shooting0201.scm )  
   Gauche-gl を使用した、簡単なシューティングゲームです。  
   矢印キーで上下左右移動。  
   [Ctrl]/[Space]/[a]/[z]キーのいずれかでビーム発射です(押し続けると発射し続けます)。  
   敵を破壊するには、コアの部分にビームを当てる必要があります。  
   また、敵を破壊すると一定範囲が誘爆します。  
   画面右上のレベル表示は、出現する敵の数と速度の目安になります。  
   また、スタート画面でしばらく待つとデモになります。  
   ESCキーを押すと終了します。

9. フライトゲーム ( flight.scm )  
   Gauche-gl を使用した、簡単なフライトゲームです。  
   画面左下の小さい黄色が自機です。(画面の左上には自機の姿勢が表示されています)  
   スペースキーを押し続けると加速します。(上昇中はほとんど加速できません)  
   矢印キーの上下で方向を変えます。  
   (矢印キーの上で反時計回り、下で時計回りに角度を変えます)  
   画面の左右はつながっています。  
   画面右上のチェックポイント(オレンジ色の円)に触れてから、地面に着陸するとゴールです。  
   着陸の際は、地面への進入角が30度以下である必要があります。  
   (進入角が30度より大きいとバウンドしてしまいます)  
   [r]キーを押すとゲームをリセットします。  
   ESCキーを押すと終了します。

10. 迷路生成 ( maze.scm )  
    Gauche-gl を使用した、迷路を自動生成して表示するサンプルです。  
    スタート(水色)からゴール(赤色)までのルートも探索して表示します。  
    生成される迷路の上下左右はつながっています。  
    スペースキーを押すと、次の迷路を表示します。  
    ESCキーを押すと終了します。  
    参考「古くて新しい自動迷路生成アルゴリズム」  
    http://d.hatena.ne.jp/yaneurao/20130125

11. 時計 ( clock.scm )  
    Gauche-gl を使用した、アナログ時計を表示するサンプルです。  
    ESCキーを押すと終了します。

12. 探索ゲーム ( walker.scm )  
    Gauche-gl を使用した、簡単な探索ゲームです。  
    矢印キーで上下左右移動。  
    迷路の出口に到達すればクリアです。  
    迷路のサイズは、5 x 5 = 25 部屋分となっています。  
    また、迷路の上下左右はつながっており、外周は存在しません。  
    スペースキーを押すと、壁にマーク(アルファベット)を付加できます。  
    マークは最大5つまでで、それをこえると古いものから消えていきます。  
    また、スタート画面でしばらく待つとデモになります。  
    [r]キーを押すとゲームをリセットします。  
    ESCキーを押すと終了します。

13. ワームシミュレータ ( worm0101.scm, worm0201.scm )  
    Gauche-gl を使用した、ワームシミュレータです。  
    矢印キーかマウスボタン1でカーソルを移動します。  
    空腹状態のワームはカーソルを追跡します。  
    ESCキーを押すと終了します。  
    worm0101.scm は末尾から移動するタイプで、  
    worm0201.scm は先端から移動するタイプです。

14. シューティングゲーム3 ( shooting0301.scm )  
    Gauche-gl を使用した、簡単なシューティングゲームです。  
    矢印キーで上下左右移動。  
    [Ctrl]/[Space]/[a]/[z]キーのいずれかでビーム発射です(押し続けると発射し続けます)。  
    敵(ワーム)は、頭の部分にのみダメージを与えられます。  
    (かなり固いため、しばらくビームを当て続ける必要があります)  
    画面右上のレベル表示は、出現する敵の数の目安になります。  
    また、スタート画面でしばらく待つとデモになります。  
    ESCキーを押すと終了します。

15. 振り子シミュレータ ( pendulum.scm )  
    Gauche-gl を使用した、振り子シミュレータです。  
    ラグランジュの運動方程式とルンゲクッタ法により  
    近似解を求めています。  
    ESCキーを押すと終了します。

16. 物体(球)の衝突 ( collision.scm )  
    Gauche-gl を使用した、物体(球)の衝突をシミュレートするプログラムです。  
    (一部計算が正しくないところがあります)  
    ESCキーを押すと終了します。

17. 影のある星 (shadow.scm)  
    Gauche-gl を使用した、影のある星を表示するプログラムです。  
    スペースキーを押すと、2D表示と3D表示を切り換えます。  
    ESCキーを押すと終了します。

18. シューティングゲーム4 ( shooting0401.scm )  
    シューティングゲーム3 ( shooting0301.scm ) の視点を変更して、  
    疑似3D風にしたものです。  
    ゲームの内容は、シューティングゲーム3 と同じです。

19. 直方体オブジェ ( rectangular.scm )  
    Gauche-gl を使用した、直方体オブジェを表示するプログラムです。  
    矢印キーと[z]/[x]/[Ctrl]キーで少し操作できます。  
    ESCキーを押すと終了します。


## その他 注意事項等
1. 一部のサンプルは、以下のモジュールを使用しています。これらは、lib フォルダに格納しています。
   
   |<div align="center">ファイル名</div>|<div align="center">内容</div>|
   |---|---|
   |glmintool.scm |Gauche-gl を使うプログラムのための簡単なツール類です。            |
   |gltextscrn.scm|Gauche-gl を使って文字列の表示等を行うためのモジュールです。      |
   |alaudplay.scm |Gauche-al を使って音楽を演奏するためのモジュールです。            |
   |alauddata.scm |alaudplay 用の音楽データをいくつか設定するためのモジュールです。  |
   |glmodelkit.scm|Gauche-gl を使って基本的なモデルの生成を行うためのモジュールです。|
   |glmazekit.scm |迷路の生成と探索を行うためのモジュールです。                      |
   |glwormkit.scm |ワームシミュレータ用のモジュールです。                            |

2. シューティングゲームについて、キーボードによっては キーの同時押しの制限が存在し、  
   斜め移動とスペースキーの同時押しが効かない場合があります。  
   キーの割り当てがたくさんあるのはその回避のためです。  
   (Ctrlキー単独の状態取得は、freeglut の v2.8.0 以後で対応されたもよう)

3. 一部のサンプルに効果音を追加しました。  
   効果音ファイルは、sound フォルダに格納しています。  
   これらは、TAM Music Factory 様 ( http://www.tam-music.com/ ) の素材を使用しています。  
   効果音の再生には、OpenAL, freealut, Gauche-al のインストールが必要です。  
   以下のページを参考に、インストールを実施ください。  
   https://github.com/Hamayama/Gauche-al-mg  
   上記インストール後に、xxx_snd.bat (xxxはサンプル名) を実行するか、または、  
   `gosh xxx.scm 1` のように引数を付けて実行すると、効果音付きになります。  
   (Gauche-al が存在しない場合には、効果音なしになります)

4. 一部のサンプルは、画像ファイルを読み込んで使用しています。  
   画像ファイルは、image フォルダに格納しています。

5. 一部のサンプルは、モデル表示用ファイルを読み込んで使用しています。  
   モデル表示用ファイルは、model フォルダに格納しています。

6. 各サンプルについての説明をもう少し、以下のページに載せています。  
   http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3AGauche-gl%E3%82%B5%E3%83%B3%E3%83%97%E3%83%AB

7. Linux 上での利用について (参考)  
   以下は、Windows の VirtualBox 内の Linux Mint 19.3 (Cinnamon) 上で  
   動作させたときのメモです(2020-4-12)。
   ```
   # Gauche のインストール
   #  ( get-gauche.sh 内の make -j を make に置換しないと、(多分PCが非力なため) 固まった )
   sudo apt install automake
   sudo apt install libtool
   curl -f -o get-gauche.sh https://raw.githubusercontent.com/shirok/get-gauche/master/get-gauche.sh
   sed -i -e 's/make -j/make/' get-gauche.sh
   chmod +x get-gauche.sh
   ./get-gauche.sh

   # Gauche-gl のインストール
   #  ( https://github.com/shirok/Gauche-gl からソースを取得してインストール )
   sudo apt install libglu1-mesa-dev mesa-common-dev # already installed
   sudo apt install freeglut3-dev
   sudo apt install libxmu-dev
   sudo apt install libxi-dev
   ./DIST gen
   ./configure
   make
   make check
   sudo make install

   # Gauche-al のインストール
   #  ( https://github.com/Hamayama/Gauche-al-mg からソースを取得してインストール )
   sudo apt install libopenal-dev
   sudo apt install libalut-dev
   sudo apt install texinfo
   ./DIST gen
   ./configure
   make
   make check
   sudo make install

   # あとは、本サイト ( https://github.com/Hamayama/Gauche-gl-examples ) のソースを取得して、
   # gosh xxx.scm のように実行する。
   # (一部のサンプルは、gosh xxx.scm 1 のように引数を付けて実行すると、効果音付きになる)
   gosh trochoid.scm
   gosh shooting0101.scm 1
   ```


## 環境等
- OS
  - Windows 10 (version 22H2) (64bit)
  - Windows 8.1 (64bit)
- 言語
  - Gauche v0.9.15
  - Gauche v0.9.11
  - Gauche v0.9.10
  - Gauche v0.9.9
  - Gauche v0.9.8
  - Gauche v0.9.7
  - Gauche v0.9.6
  - Gauche v0.9.5
  - Gauche v0.9.4
- 拡張ライブラリ
  - Gauche-gl v0.7_pre3
  - Gauche-gl v0.6
  - Gauche-al v1.0
- 効果音
  - TAM Music Factory 様 ( http://www.tam-music.com/ )

## 履歴
- 1001HISTORY.txt を参照ください。


(2024-7-21)
