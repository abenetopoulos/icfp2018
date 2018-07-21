-module(ultra_naive).

-export([print_model/2,
	 print_model_parallel/3,
	 points_in_level/3]).

-import(lists, [nth/2]).

print_model(R, Model) ->
    Flip = {flip, []},
    ModelMap = topological:height_map(R, Model),
    Moves = print_levels(1, R, ModelMap),
    Halt = {halt, []},
    [Flip] ++ Moves ++ [Flip] ++ [Halt].

print_model_parallel(R, Model, ParallelBots) ->
    Flip = {flip, []},
    ModelMap = topological:height_map(R, Model),
    SpawnMoves = parallel:spawn_bots(ParallelBots-1),
    %% io:format("SpawnMoves: ~p~n", [SpawnMoves]),
    Bids = lists:seq(1,ParallelBots),
    EmptyCommands = maps:from_list([{B, []} || B <- Bids]),
    NewCurrs = maps:from_list([{B, {1,B,B}} || B <- Bids]),
    BidsWork = lists:zip(Bids, [0 || _ <- Bids]), 
    ParallelMoves = print_levels_parallel(1, R, ModelMap, BidsWork, EmptyCommands, NewCurrs),
    %% io:format("Parallel Moves: ~n~p~n", [maps:keys(ParallelMoves)]),
    StalledParallelMoves = parallel:create_stalls(Bids, ParallelMoves),
    Moves = serialize_parallel_moves(Bids, StalledParallelMoves, []),
    %% io:format("Moves: ~n~p~n", [Moves]),
    GatherMoves = parallel:gather_bots(ParallelBots-1),
    %% io:format("GatherMoves: ~p~n", [GatherMoves]),
    Halt = {halt, []},
    [Flip] ++ SpawnMoves ++ Moves ++ GatherMoves ++ [Flip] ++ [Halt].

print_levels_parallel(R, R, Model, BidsWork, Commands, CurrCoords) ->
    {Bids, _} = lists:unzip(BidsWork),
    {NewCommands, NewCoords} = move_robots_back(Bids, Commands, CurrCoords),
    NewCommands;
print_levels_parallel(Y, R, Model, [{Bid, Work}|BidsWork], Commands, CurrCoords) ->
    CurrBidPos = maps:get(Bid, CurrCoords),
    case points_in_level(Y, R, Model) of
	[] ->
	    print_levels_parallel(R, R, Model, insert_bids_work({Bid, Work}, BidsWork), Commands, CurrCoords);
	LevelPoints ->
	    {BoxMin, BoxMax} = bounding_box:find_box(LevelPoints, {R+1,Y,R+1}, {0,Y,0}),
	    %% io:format("Level: ~p Box: {~p, ~p}~n", [Y, BoxMin, BoxMax]),
	    {Moves, NewPos} = bounding_box:print_box(BoxMin, BoxMax, CurrBidPos, Model),
	    FinalPos = {1,Y,Bid},
	    FinalMoves = bounding_box:move_robot(NewPos,FinalPos),
	    OptimizedMoves = bounding_box:optimize_one_bot_moves(Moves++FinalMoves),
	    %% OptimizedMoves = Moves ++ FinalMoves,
	    %% io:format("Moves:~n~p~n", [Moves]),
	    NewCommands = 
		maps:update_with(Bid, fun(BidComms) -> BidComms ++ OptimizedMoves end, Commands),
	    NewCurrCoords =
		maps:update_with(Bid, fun(_) -> FinalPos end, CurrCoords),
	    print_levels_parallel(Y+1, R, Model, 
				  insert_bids_work({Bid, Work + length(OptimizedMoves)}, BidsWork), 
				  NewCommands, NewCurrCoords)
    end.

insert_bids_work({Bid, Work}, BidsWork) ->
    insert_bids_work({Bid, Work}, BidsWork, []).

insert_bids_work({Bid, Work}, [], Acc) ->
    lists:reverse([{Bid, Work}|Acc]);
insert_bids_work({Bid, Work}, Rest = [{Bid2, Work2}|BidsWork], Acc) ->
    case Work =< Work2 of
	true ->
	    lists:reverse([{Bid, Work}|Acc]) ++ Rest;
	false ->
	    insert_bids_work({Bid, Work}, BidsWork, [{Bid2, Work2}|Acc])
    end.

move_robots_back([], Commands, CurrCoords) ->
    {Commands, CurrCoords};
move_robots_back([Bid|Bids], Commands, CurrCoords) ->
    CurrBidPos = maps:get(Bid, CurrCoords),
    MoveBack = bounding_box:move_robot(CurrBidPos, {1,Bid,Bid}),
    NewCommands = 
	maps:update_with(Bid, fun(BidComms) -> BidComms ++ MoveBack end, Commands),
    NewCurrCoords =
	maps:update_with(Bid, fun(_) -> {1,Bid,Bid} end, CurrCoords),
    move_robots_back(Bids, NewCommands, NewCurrCoords).


%% We need to end when we have dealt k times with each Bid
serialize_parallel_moves([1|Bids], Commands, Acc) when map_size(Commands) =:= 0 ->
    lists:reverse(Acc);
serialize_parallel_moves([Bid|Bids], Commands, Acc) ->
    %% io:format("Left Commands: ~n~p~n", [lists:map(fun(X) -> length(X) end, maps:values(Commands))]),
    case maps:find(Bid, Commands) of
	{ok, []} ->
	    serialize_parallel_moves(Bids ++ [Bid], maps:remove(Bid, Commands), [{wait, []}|Acc]);
	{ok, [Move|Moves]} ->
	    serialize_parallel_moves(Bids ++ [Bid], maps:put(Bid, Moves, Commands), [Move|Acc]);
	error ->
	    serialize_parallel_moves(Bids ++ [Bid], Commands, [{wait, []}|Acc])
    end.



print_levels(Y, R, Model) ->
    case points_in_level(Y, R, Model) of
	[] ->
	    bounding_box:move_robot({1,Y,1}, {1,1,1});
	LevelPoints ->
	    {BoxMin, BoxMax} = bounding_box:find_box(LevelPoints, {R+1,R+1,R+1}, {0,0,0}),
	    %% io:format("Level: ~p Box: {~p, ~p}~n", [Y, BoxMin, BoxMax]),
	    {Moves,CurrPos} = bounding_box:print_box(BoxMin, BoxMax, {1,Y,1}, Model),
	    FinalMoves = bounding_box:move_robot(CurrPos, {1, Y+1,1}),
	    OptimizedMoves = bounding_box:optimize_one_bot_moves(Moves++FinalMoves),
	    %% OptimizedMoves = Moves,
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
