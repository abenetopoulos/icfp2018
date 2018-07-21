-module(bounding_box).

-export([find_box/3,
	 print_box/4,
	 move_robot/2,
	 model_get/2]).

-import(lists, [nth/2]).

find_box([], Min, Max) ->
    {Min, Max};
find_box([{X, Y, Z}|Points], {MinX, MinY, MinZ}, {MaxX, MaxY, MaxZ}) ->
    NewMin = {min(X, MinX), min(Y, MinY), min(Z, MinZ)},
    NewMax = {max(X, MaxX), max(Y, MaxY), max(Z, MaxZ)},
    find_box(Points, NewMin, NewMax).
    
%% TODO: Optimize with parallelism
print_box(Min, Max, Curr, Model) -> 
    Pre = move_robot(Curr, Min),
    Moves = print_voxel(Min, Max, add_coords(Min, {0,0,1}), Model),
    Pre ++ [{smove, [{0,0,1}]}] ++ Moves.	
    
%% We paint the voxel behind us that is why Z > MaxZ + 1
print_voxel(Min={_,_,MinZ}, Max={_,_,MaxZ}, Curr={X,Y,Z}, Model) when Z > (MaxZ+1) ->
    {_, ReturnMoves} = linear_move(z, Curr, Min),
    [{smove, [{1, 0, 0}]}] ++ ReturnMoves ++ 
	print_voxel(Min, Max, {X + 1, Y, MinZ}, Model);
print_voxel(Min={MinX,_,_}, Max={MaxX,_,_}, Curr={X,Y,Z}, Model) when X > MaxX ->
    {_, ReturnMoves} = linear_move(x, Curr, Min),
    [{smove, [{0, 1, 0}]}] ++ ReturnMoves ++ 
	print_voxel(Min, Max, {MinX, Y + 1, Z}, Model);
print_voxel(Min={_,MinY,_}, Max={_,MaxY,_}, Curr={X,Y,Z}, Model) when Y > MaxY ->
    ReturnMoves = move_robot(Curr, {1, Y, 1}),
    ReturnMoves;
print_voxel(Min, Max, {X, Y, Z}, Model) ->
    %% erlang:display({X,Y,Z}),
    Fill =
	case model_get({X,Y,Z-1}, Model) of
	    0 -> 
		[];
	    1 ->
		[{fill, [{0,0,-1}]}]
	end,
    Move = [{smove, [{0,0,1}]}],
    Fill ++ Move ++ print_voxel(Min, Max, {X, Y, Z+1}, Model).
    

%% TODO: Optimize
model_get({X,Y,Z}, Model) ->
    nth(Z, nth(Y, nth(X, Model))).


%% TODO: Optimize
%% Moves first on y then on x then on z so that the robot does not crash on any filled block
move_robot(From, To) -> 
    {TempY, MovesY} = linear_move(y, From, To),
    {TempX, MovesX} = linear_move(x, TempY, To),
    {To, MovesZ} = linear_move(z, TempX, To),
    MovesY ++ MovesX ++ MovesZ.

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
