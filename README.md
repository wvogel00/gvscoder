 # gvscoder

## 動作確認環境
MacOS Catalina 10.15.5  
(Windows10ではリクエスト作成に問題があり400が返ってくる．修正の予定はない)  
(ただし，軽微な問題で，ByteString周りの処理を見直せば修正できる見込み)

## ACが獲れないなら，AC電流を流せば良いじゃない？
___

競技プログラミングの国内サービス：AtCoderには，提出したソースコードの挙動により，次のような判定がある．  
AC, WA, TLE, MLE, RE, CE, QLE, IE, WJ, WR

このうち，WJは判定中，WRは再ジャッジ待ち，IEはジャッジシステムのエラーで，提出者の提出コードが誤っているわけでは有りません．しかし，次のメッセージが出た時にはソースコード中に誤りがあります．  
- CE (Compilation Error)...コンパイルエラー  
- MLE (Memory Limit Exceeded)...メモリ制限超過  
- TLE (Time Limit Exceeded)...実行時間超過  
- RE (Runtime Error)...実行時エラー  
- OLE (Output Limit Exceeded)..出力制限オーバー

AtCoderではACが唯一の正解となり，参加者はACを獲るために奮闘するわけです．

しかし，ACがどうしても獲れない時もあるでしょう．
ただただWAやREが返ってくる状況に，苦しめられることになります．
そんなプログラマ達に救済をもたらすのが本プロジェクト， __GVS-Coder__ です．
ACが獲れなかった場合には，代わりに頭にAC電流を流してくれるのです！

まさに，__何があってもACを獲り続けることができる画期的なシステム__ といえるでしょう！

## What is GVS?
___
__GVS__ (galvanic vestibular stimulation : 前庭電気刺激)は，
古くは医療の分野で使われていた電気刺激手法．電流を流すことで平衡感覚を操作することが出来ます．シンプルな構成だと，左右の乳様突起に電極を貼付して電気を流すだけで済みます．  
正しく刺激を印加すれば痛みはありません．そのためには，
- 皮膚表面の汚れを落とす
- 電極をしっかり貼付する
- 矩形波ではなく正弦波を印加する
などが挙げられます．

近年ではVR酔いの軽減や，VRへの没入感の向上などに利用・研究が勧められています．

## フロー
___
1. GVSをPCに接続する．GVSのシリアルポートを確認し，portnameファイルにシリアルポートの名前を入力する
  1. Windows ... COMx (非対応)
  2. Mac ... /dev/tty~~~
1. 自分のアカウント情報と参加するコンテスト名を入力してログインする，
1. atcoderのサイトからファイルを提出する．
1. ACを取得すればAccepted!が，そうでなければGVSが流れる

## その他
- 過去にMaker Faire 2018に出展した際のソースコードは
https://github.com/wvogel00/GVS
にあります．
Maker Faire 2017出展時の開発はmbedで行っており非公開ですが，
処理としては本プロジェクトとほぼ同一です．
- 当初，cursesを使ったUIとしていましたが，シリアル通信と相性が悪く削除しました．どなたか知見があればご教示ください．
