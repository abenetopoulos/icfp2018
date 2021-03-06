-module(bounding_box).

-export([find_box/1,
	 print_box/4,
	 move_robot/2,
	 move_robot_zxy/2,
	 model_get/2,
	 optimize_one_bot_moves/1,
	 optimize_seq_trace/1,
	 add_coords/2,
	 scale_coords/2,
	 filter_points_in_segment/3]).

-import(lists, [nth/2]).

find_box([Point|Points]) ->
    find_box(Points, Point, Point).

find_box([], Min, Max) ->
    {Min, Max};
find_box([{X, Y, Z}|Points], {MinX, MinY, MinZ}, {MaxX, MaxY, MaxZ}) ->
    NewMin = {min(X, MinX), min(Y, MinY), min(Z, MinZ)},
    NewMax = {max(X, MaxX), max(Y, MaxY), max(Z, MaxZ)},
    find_box(Points, NewMin, NewMax).
    
%% TODO: Optimize with parallelism
print_box(Min, Max, Curr, Model) -> 
    Pre = move_robot(Curr, Min),
    % We always cover bounding boxes from low to high x and z, so initial direction is plus
    {Moves, NewCurr} = print_voxel(Min, Max, Min, plus, Model, []),
    {Pre ++ Moves, NewCurr}.	
    
%% We paint the voxel behind us that is why Z > MaxZ + 1
print_voxel(Min={_,_,MinZ}, Max={_,_,MaxZ}, Curr={X,Y,Z}, plus, Model, Acc) when Z > MaxZ ->
    NewAcc = [{smove, [{1, 0, 0}]}|Acc],
    print_voxel(Min, Max, {X + 1, Y, Z}, minus, Model, NewAcc);
print_voxel(Min={_,_,MinZ}, Max={_,_,MaxZ}, Curr={X,Y,Z}, minus, Model, Acc) when Z < MinZ ->
    NewAcc = [{smove, [{1, 0, 0}]}|Acc],
    print_voxel(Min, Max, {X + 1, Y, Z}, plus, Model, NewAcc);
print_voxel(Min={MinX,_,MinZ}, Max={MaxX,_,MaxZ}, Curr={X,Y,Z}, Direction, Model, Acc) when X > MaxX ->
    %% {_, ReturnMoves} = linear_move(x, Curr, Min),
    MoveToStart = lists:flatten([move_robot({X, Y, Z}, {X, Y, MinZ - 1}), move_robot({X, Y, MinZ - 1}, {MinX - 1, Y, MinZ - 1})]),
    {lists:reverse(Acc) ++ MoveToStart, {MinX-1,Y,MinZ-1}};
    %% NewAcc = lists:reverse(ReturnMoves) ++ [{smove, [{0, 1, 0}]}|Acc],
    %% print_voxel(Min, Max, {MinX, Y + 1, Z}, Model, NewAcc);
%% print_voxel(Min={_,MinY,_}, Max={_,MaxY,_}, Curr={X,Y,Z}, Model, NewAcc) when Y > MaxY ->
%%     ReturnMoves = move_robot(Curr, {1, Y, 1}),
%%     {lists:reverse(NewAcc) ++ ReturnMoves, {1, Y, 1}};
print_voxel(Min, Max, {X, Y, Z}, Direction, Model, Acc) ->
    %% erlang:display({X,Y,Z}),
    {Move, NewZ} = case Direction of
               plus -> {[{smove, [{0,0,1}]}], Z + 1};
               minus -> {[{smove, [{0,0,-1}]}], Z - 1}
           end,
    {Lookup, ToFill} = case Direction of
                 plus -> {{X,Y,NewZ - 1}, {0, 0, -1}};
                 minus -> {{X,Y,NewZ + 1}, {0, 0, 1}}
             end,
    Fill =
	case model_get(Lookup, Model) of
	    0 -> 
		[];
	    1 ->
		[{fill, [ToFill]}]
	end,	    
    NewAcc = Fill ++ Move ++ Acc,
    print_voxel(Min, Max, {X, Y, NewZ}, Direction, Model, NewAcc).
    

model_get({X,Y,Z}, Model) ->
    maps:get(Z, maps:get(Y, maps:get(X, Model))).

%% TODO: Optimize
list_model_get({X,Y,Z}, Model) ->
    nth(Z, nth(Y, nth(X, Model))).

optimize_seq_trace(Commands) ->
    optimize_seq_trace(Commands, {0,0,0}, []).

optimize_seq_trace([], Buffer, Acc) ->
    FinalMoves = instantiate_moves(Buffer),
    lists:reverse(Acc) ++ FinalMoves;
optimize_seq_trace([Com|Commands], Buffer, Acc) ->
    case Com of
	{smove, [Cd]} ->
	    optimize_seq_trace(Commands, add_coords(Cd, Buffer), Acc);
	{lmove, [Cd1, Cd2]} ->
	    optimize_seq_trace(Commands, add_coords(Cd2, add_coords(Cd1, Buffer)), Acc);
	_ ->
	    BufferMoves = instantiate_moves(Buffer),
	    NewAcc = [Com|lists:reverse(BufferMoves)] ++ Acc,
	    optimize_seq_trace(Commands, {0,0,0}, NewAcc)
    end.

instantiate_moves(Buffer) ->
    instantiate_moves(Buffer, []).

instantiate_moves({0, 0, 0}, Acc) ->
    lists:reverse(Acc);

instantiate_moves({0, 0, Z}, Acc) ->
    Dz = max(min(Z,15),-15),
    instantiate_moves({0,0,Z-Dz}, [{smove, [{0,0,Dz}]}|Acc]);

instantiate_moves({0, Y, Z}, Acc) when abs(Y) =< 5 andalso abs(Z) > 0 ->
    Dy = Y,
    Dz = max(min(Z,5),-5),
    instantiate_moves({0,Y-Dy,Z-Dz}, [{lmove, [{0,Dy,0}, {0,0,Dz}]}|Acc]);
instantiate_moves({0, Y, Z}, Acc) ->
    Dy = max(min(Y,15),-15),
    instantiate_moves({0,Y-Dy,Z}, [{smove, [{0,Dy,0}]}|Acc]);

instantiate_moves({X, Y, Z}, Acc) when abs(X) =< 5 andalso abs(Y) > 0 ->
    Dx = X,
    Dy = max(min(Y,5),-5),
    instantiate_moves({X-Dx,Y-Dy,Z}, [{lmove, [{Dx,0,0}, {0,Dy,0}]}|Acc]);
instantiate_moves({X, Y, Z}, Acc) when abs(X) =< 5 andalso abs(Z) > 0 ->
    Dx = X,
    Dz = max(min(Z,5),-5),
    instantiate_moves({X-Dx,Y,Z-Dz}, [{lmove, [{Dx,0,0}, {0,0,Dz}]}|Acc]);
instantiate_moves({X, Y, Z}, Acc)->
    Dx = max(min(X,15),-15),
    instantiate_moves({X-Dx,Y,Z}, [{smove, [{Dx,0,0}]}|Acc]).
    

optimize_one_bot_moves([]) ->
    [];
optimize_one_bot_moves([Move|Moves]) ->
    optimize_one_bot_moves(Moves, Move, []).

optimize_one_bot_moves([], Last, Acc) ->
    lists:reverse([Last|Acc]);
optimize_one_bot_moves([Move|Moves], Last, Acc) ->
    case mergeable(Move, Last) of
	{ok, NewLast} ->
	    optimize_one_bot_moves(Moves, NewLast, Acc);
	{ok, NewAcc, NewLast} ->
	    optimize_one_bot_moves(Moves, NewLast, [NewAcc|Acc]);
	{ok, []} -> 
	    optimize_one_bot_moves(Moves, hd(Acc), tl(Acc));
	no ->
	    optimize_one_bot_moves(Moves, Move, [Last|Acc])
    end.

mergeable({smove,[Cd1]}, {smove,[Cd2]}) ->
    case same_dir_and_short(Cd1, Cd2) of
	{ok, NewCd} ->
	    {ok, {smove, [NewCd]}};
	{ok, NewCd, FullCd} ->
	    {ok, {smove, [FullCd]}, {smove, [NewCd]}};
	no ->
	    case add_coords(Cd1, Cd2) of
		0 -> {ok, []};
		_ -> no
	    end
    end;
mergeable(_, _) -> no.


same_dir_and_short({X1,0,0}, {X2,0,0}) when abs(X1 + X2) =< 15 andalso X1 + X2 =/= 0 ->
    {ok, {X1 + X2, 0, 0}};
same_dir_and_short({0,Y1,0}, {0,Y2,0}) when abs(Y1 + Y2) =< 15 andalso Y1 + Y2 =/= 0 ->
    {ok, {0, Y1 + Y2, 0}};
same_dir_and_short({0,0,Z1}, {0,0,Z2}) when abs(Z1 + Z2) =< 15 andalso Z1 + Z2 =/= 0 ->
    {ok, {0, 0, Z1 + Z2}};
same_dir_and_short({X1,0,0}, {X2,0,0}) when abs(X1 + X2) > 15 ->
    Full = 
	case X1 + X2 > 15 of
	    true -> 15;
	    false -> - 15
	end,
    {ok, {X1 + X2 - Full , 0, 0}, {Full, 0, 0}};
same_dir_and_short({0,Y1,0}, {0,Y2,0}) when abs(Y1 + Y2) > 15 ->
    Full = 
	case Y1 + Y2 > 15 of
	    true -> 15;
	    false -> - 15
	end,
    {ok, {0, Y1 + Y2 - Full, 0}, {0, Full, 0}};
same_dir_and_short({0,0,Z1}, {0,0,Z2}) when abs(Z1 + Z2) > 15 ->
    Full = 
	case Z1 + Z2 > 15 of
	    true -> 15;
	    false -> - 15
	end,
    {ok, {0, 0, Z1 + Z2 - Full}, {0, 0, Full}};
same_dir_and_short(_, _) ->
    no.
    

%% TODO: Optimize
%% Moves first on y then on x then on z so that the robot does not crash on any filled block
move_robot(From, To) -> 
    {TempY, MovesY} = linear_move(y, From, To),
    {TempX, MovesX} = linear_move(x, TempY, To),
    {To, MovesZ} = linear_move(z, TempX, To),
    MovesY ++ MovesX ++ MovesZ.

move_robot_zxy(From, To) -> 
    {TempZ, MovesZ} = linear_move(z, From, To),
    {TempX, MovesX} = linear_move(x, TempZ, To),
    {To, MovesY} = linear_move(y, TempX, To),
    MovesZ ++ MovesX ++ MovesY.

linear_move(Dir, From, To) ->
    linear_move(Dir, From, To, []).

linear_move(x, F = {Fx, Fy, Fz}, {Tx, Ty, Tz}, Acc) when Fx =:= Tx ->
    {F, lists:reverse(Acc)};
linear_move(y, F = {Fx, Fy, Fz}, {Tx, Ty, Tz}, Acc) when Fy =:= Ty ->
    {F, lists:reverse(Acc)};
linear_move(z, F = {Fx, Fy, Fz}, {Tx, Ty, Tz}, Acc) when Fz =:= Tz ->
    {F, lists:reverse(Acc)};
linear_move(Dir, F = {Fx, Fy, Fz}, T = {Tx, Ty, Tz}, Acc) ->
    Dist = 
	case Dir of
	    x -> Tx - Fx;
	    y -> Ty - Fy;
	    z -> Tz - Fz
	end,
    LMove = max(min(Dist, 15), -15),
    DC =
	case Dir of
	    x -> {LMove, 0, 0};
	    y -> {0, LMove, 0};
	    z -> {0, 0, LMove}
	end,
    linear_move(Dir, add_coords(F, DC), T, [{smove, [DC]}|Acc]).
    
add_coords({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {X1 + X2, Y1 + Y2, Z1 + Z2}.

scale_coords(S, {X, Y, Z}) ->
    {S * X, S * Y, S* Z}.

filter_points_in_segment({X1, Y1, Z1}, {X2, Y2, Z2}, FilterFun) ->
    AllPointsInSegment = lists:flatten([[[{X, Y, Z} || X <- lists:seq(X1,X2)] 
				       || Y <- lists:seq(Y1,Y2)]
				      || Z <- lists:seq(Z1,Z2)]),
    [{X, Y, Z} || {X, Y, Z} <- AllPointsInSegment, FilterFun({X,Y,Z})].
