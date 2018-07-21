-module(ultra_naive).

-export([print_model/2]).

-import(lists, [nth/2]).

print_model(R, Model) ->
    Flip = {flip, []},
    ModelMap = topological:height_map(R, Model),
    Moves = print_levels(1, R, ModelMap),
    Halt = {halt, []},
    [Flip] ++ Moves ++ [Flip] ++ [Halt].

print_levels_parallel(R, R, Model, _Bids, Commands, CurrCoords) ->
    Commands;
print_levels_parallel(Y, R, Model, [Bid|Bids], Commands, CurrCoords) ->
    CurrBidPos = maps:get(Bid, CurrCoords),
    case points_in_level(Y, R, Model) of
	[] ->
	    MoveBack = bounding_box:move_robot(CurrBidPos, {1,Bid,Bid}),
	    NewCommands = 
		maps:update_with(Bid, fun(BidComms) -> BidComms ++ MoveBack end, Commands),
	    NewCurrCoords =
		maps:update_with(Bid, fun(_) -> {1,Bid,Bid} end, CurrCoords),
	    print_levels_parallel(Y+1, R, Model, Bids ++ [Bid], NewCommands, NewCurrCoords);
	LevelPoints ->
	    {BoxMin, BoxMax} = bounding_box:find_box(LevelPoints, {R+1,Y,R+1}, {0,Y,0}),
	    %% io:format("Level: ~p Box: {~p, ~p}~n", [Y, BoxMin, BoxMax]),
	    {Moves, NewPos} = bounding_box:print_box(BoxMin, BoxMax, CurrBidPos, Model),
	    %% io:format("Moves:~n~p~n", [Moves]),
	    NewCommands = 
		maps:update_with(Bid, fun(BidComms) -> BidComms ++ Moves end, Commands),
	    NewCurrCoords =
		maps:update_with(Bid, fun(_) -> NewPos end, CurrCoords),
	    print_levels_parallel(Y+1, R, Model, Bids ++ [Bid], NewCommands, NewCurrCoords)
    end.

print_levels(Y, R, Model) ->
    case points_in_level(Y, R, Model) of
	[] ->
	    bounding_box:move_robot({1,Y,1}, {1,1,1});
	LevelPoints ->
	    {BoxMin, BoxMax} = bounding_box:find_box(LevelPoints, {R+1,R+1,R+1}, {0,0,0}),
	    %% io:format("Level: ~p Box: {~p, ~p}~n", [Y, BoxMin, BoxMax]),
	    Moves = bounding_box:print_box(BoxMin, BoxMax, {1,Y,1}, Model),
	    OptimizedMoves = bounding_box:optimize_one_bot_moves(Moves),
	    %% io:format("Moves:~n~p~n", [Moves]),
	    OptimizedMoves ++ print_levels(Y+1, R, Model)
    end.


points_in_level(Y, R, Model) ->
    AllPointsInLevel = lists:flatten([[{X, Y, Z} || X <- lists:seq(1,R)] 
				      || Z <- lists:seq(1,R)]),
    [{X, Y, Z} || {X, Y, Z} <- AllPointsInLevel, bounding_box:model_get({X,Y,Z}, Model) =:= 1].


%% print_model(R, Model) ->
%%     Flip = {flip, []},
%%     Moves = print_voxel(R, 1, 1, 2, Model),
%%     Halt = {halt, []},
%%     [Flip] ++ [{smove, [{0,0,1}]}] ++ Moves ++ [Flip] ++ [Halt].

print_voxel(R, X, Y, Z, Model) when Z =:= R ->
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
