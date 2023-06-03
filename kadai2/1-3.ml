let rec wake n lst =
  match lst with 
    |[] -> ([],[])
    |first :: rest ->
      let (a,b) = (wake n rest)
      in if first > n then (a,first :: b) else (first :: a,b)
let rec quick lst =
  match lst with 
    |[] -> []
    |first :: rest ->
      let (a,b) = (wake first rest)
      in (quick a)@(first :: (quick b))
    ;;

(*lst = [2;4;1;3]とする
まず[]でないので2 :: [4;1;3]とわけられる(10行目)
これを11行目のwake first lestに代入、wake 2 [4;1;3]を計算し([1],[4;3])が出力される(1~6行目)
12行目により(quick [1])@(2 :: quick [4;3])を計算すると、quick [1] = [1],quick[4;3] = [3;4](下記参照)より、
[1;2;3;4]が出力される。
quick [4;3] はwake 4 [3] = ([3],[])より(quick [3])@(4 :: quick[]) = [3;4]となる。*)