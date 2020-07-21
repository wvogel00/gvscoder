# gvscoder

## ACが獲れないなら，AC電流を流せば良いじゃない？
___

競技プログラミングの国内サービス：AtCoderには，提出したソースコードの挙動により，次のような判定がある．  
AC, WA, TLE, MLE, RE, CE, QLE, IE, WJ, WR

このうち，WJは判定中，WRは再ジャッジ待ち，IEはジャッジシステムのエラーなので結果を待機している状態です．  
- CE (Compilation Error)...コンパイルエラー  
- MLE (Memory Limit Exceeded)...メモリ制限超過  
- TLE (Time Limit Exceeded)...実行時間超過  
- RE (Runtime Error)...実行時エラー  
- OLE (Output Limit Exceeded)..出力制限オーバー

そのためACが唯一の正解となり，参加者はACを獲るために奮闘するわけです．

しかし，ACがどうしても獲れない時もあるでしょう．
ただただWAやREが返ってくる状況に，苦しめられることになります．
そんなプログラマ達に救済をもたらすのが本プロジェクト， __GVS-Coder__ です．
ACが(Waiting for Re-judging) 再ジャッジを待っている状態です。

## What is GVS?
___
__GVS__ (galvanic vestibular stimulation : 前庭電気刺激)は，
古くは医療の分野で使われていた電気刺激手法．電流を流すことで平衡感覚を操作することが出来ます．シンプルな構成だと，左右の乳様突起に電極を貼付して電気を流すだけで済みます．

近年ではVR酔いの軽減や，VRへの没入感の向上などに利用・研究が勧められています．
参考URL :
- https://sites.google.com/site/gvslover/
- https://www.moguravr.com/osaka-univ-gvs/

## フロー
___
1. GVSをPCに接続する．GVSのシリアルポートを確認し，portnameファイルにシリアルポートの名前を入力する
  1. Windows ... COMx
  2. Mac ... /dev/tty~~~
1. 自分のアカウント情報と参加するコンテスト名を入力してログインする，
1. 正常にログイン出来れば，Cursesモードに突入し，No Changeの文字が流れる
1. atcoderのサイトからファイルを提出する．
1. ACを取得すればAccepted!が，そうでなければGVSが流れる
