-module(ultra_naive).

-export([print_model/2]).

-import(lists, [nth/2]).

print_model(R, Model) ->
    Flip = {flip, []},
    Moves = print_voxel(R, 1, 1, 1, Model, init),
    Halt = {halt, []},
    [Flip] ++ Moves ++ [Flip] ++ [Halt].

print_voxel(_R, 1, 1, 1, _Model, fill) ->
    [];
print_voxel(R, X, Y, Z, Model, Flag) when Z =:= (R-1) ->
    return(z, R, Z) ++ [{smove, [{0, 1, 0}]}] ++ 
	print_voxel(R, X, Y + 1, 1, Model, Flag);
print_voxel(R, X, Y, Z, Model, Flag) when Y =:= R ->
    return(y, R, Y) ++ [{smove, [{1, 0, 0}]}] ++ 
	print_voxel(R, X + 1, 1, Z, Model, Flag);
print_voxel(R, X, Y, Z, Model, Flag) when X =:= R ->
    return(x, R, X);
    %% return(x, R, X) ++ print_voxel(R, 1, Y, Z, Model, Flag);
print_voxel(R, X, Y, Z, Model, _Flag) ->
    %% erlang:display({X,Y,Z}),
    Fill =
	case nth(Z+1, nth(Y, nth(X, Model))) of
	    0 -> 
		[];
	    1 ->
		[{fill, [{0,0,1}]}]
	end,
    Move = [{smove, [{0,0,1}]}],
    Fill ++ Move ++ print_voxel(R, X, Y, Z+1, Model, fill).

return(Dir, R, 1) ->
    [];
return(Dir, R, Curr) ->
    Dist = 
	case Curr > 16 of
	    true -> -15;
	    false -> - Curr + 1
	end,
    DC =
	case Dir of
	    x -> {Dist, 0, 0};
	    y -> {0, Dist, 0};
	    z -> {0, 0, Dist}
	end,
    [{smove, [DC]}|return(Dir, R, Curr + Dist)].
    
    
