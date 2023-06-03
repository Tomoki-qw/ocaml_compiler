let rec checkl x y =
	match y with
	[] -> false
	| h :: rest -> 
	if x == h then true
	else checkl x rest ;;
