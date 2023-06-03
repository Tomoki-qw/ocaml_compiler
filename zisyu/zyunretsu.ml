let rec select trace a = match a with
  | [] -> [] 
  | x :: xs -> (List.rev trace, x, xs) :: select (x :: trace) xs
(* xsの各要素xと，xより左にある要素のリスト，xより右にある要素のリストを列挙する *)
let select xs = select [] xs

(* 与えられたリストからn要素を選ぶ順列 *)
let rec perm n xs =
  match n, xs with
  | 0, _ -> [[]]
  | _, [] -> []
  | n, xs ->
      List.concat (
      List.map (fun (ys, x, zs) ->
        List.map (List.cons x) (
        (* リストxsからxだけを取り除いたリストは，ys @ zs *)
        perm (n - 1) (List.concat[ys;zs]))) (
      select xs ))
      ;;



