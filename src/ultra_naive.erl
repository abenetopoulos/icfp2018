-module(ultra_naive).

-export([print_model/2]).

-import(lists, [nth/2]).

print_model(R, Model) ->
    Flip = {flip, []},
    Moves = print_voxel(R, 1, 1, 2, Model, []),
    Halt = {halt, []},
    [Flip] ++ [{smove, [{0,0,1}]}] ++ Moves ++ [Flip] ++ [Halt].

print_voxel(R, X, Y, Z, Model, Acc) when Z =:= R ->
    [{smove, [{1, 0, 0}]}] ++ return(z, R, Z) ++ 
	print_voxel(R, X + 1, Y, 2, Model);
print_voxel(R, X, Y, Z, Model) when X =:= R ->
    [{smove, [{0, 1, 0}]}] ++ return(x, R, X) ++ 
	print_voxel(R, 1, Y + 1, Z, Model);
print_voxel(R, X, Y, Z, Model) when Y =:= R ->
    return(y, R, Y) ++ [{smove, [{0,0,-1}]}];
    %% return(x, R, X) ++ print_voxel(R, 1, Y, Z, Model, Flag);
print_voxel(R, X, Y, Z, Model) ->
    %% erlang:display({X,Y,Z}),
    Fill =
	case nth(Z-1, nth(Y, nth(X, Model))) of
	    0 -> 
		[];
	    1 ->
		[{fill, [{0,0,-1}]}]
	end,
    Move = [{smove, [{0,0,1}]}],
    Fill ++ Move ++ print_voxel(R, X, Y, Z+1, Model).

return(z, R, 2) ->
    [];
return(Dir, R, 1) ->
    [];
return(Dir, R, Curr) ->
    Dist = 
	case Curr > 16 of
	    true -> -15;
	    false -> - Curr + 
			 case Dir of
			     z -> 2;
			     _ -> 1
			 end
	end,
    DC =
	case Dir of
	    x -> {Dist, 0, 0};
	    y -> {0, Dist, 0};
	    z -> {0, 0, Dist}
	end,
    [{smove, [DC]}|return(Dir, R, Curr + Dist)].
    
    
