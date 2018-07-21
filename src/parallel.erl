-module(parallel).

-export([spawn_bots/1,
	 gather_bots/1,
	 create_stalls/2]).

spawn_bots(N) ->
    spawn_bots(N, N).

spawn_bots(0, N) ->
    [];
spawn_bots(M, N) ->
    Waits = [{wait, []} ||  _ <- lists:seq(1, N-M)],
    Waits ++ [{fission, [{0,1,1},M]} | spawn_bots(M-1, N)].

gather_bots(N) ->
    gather_bots(N, N).

gather_bots(0, N) ->
    [];
gather_bots(M, N) ->
    Waits = [{wait, []} ||  _ <- lists:seq(1, M-1)],
    Waits ++ [{fusionp, [{0,1,1}]}, {fusions, [{0,-1,-1}]} | gather_bots(M-1, N)].

create_stalls(Bids, Moves) ->
    Coords = maps:from_list([{B, {1,B,B}} || B <- Bids]),
    OptMoves = maps:from_list([{B, []} || B <- Bids]),
    create_stalls(Bids, Moves, Coords, OptMoves).


create_stalls(_Bids, Moves, _Coords, OptMoves) when map_size(Moves) =:= 0 ->
    maps:map(fun(_K,Mvs) -> lists:reverse(Mvs) end, OptMoves);
create_stalls([], Moves, Coords, OptMoves) ->
    create_stalls(lists:sort(maps:keys(Coords)), Moves, Coords, OptMoves);
create_stalls([Bid|Bids], Moves, Coords, OptMoves) -> 
    case maps:find(Bid, Moves) of
	{ok, []} ->
	    %% Maybe Change this to somehow show that this Bid is never needed again
	    create_stalls(Bids, maps:remove(Bid, Moves), Coords, OptMoves);
	{ok, [Move|Rest]} ->
	    Curr = maps:get(Bid, Coords),
	    case is_there_any_collision(Curr, Move, Bids, Moves, Coords) of
		collision ->
		    NewOptMoves = maps:update_with(Bid, fun(Mvs) -> [{wait,[]}|Mvs] end, OptMoves),
		    create_stalls(Bids, Moves, Coords, NewOptMoves); 
		no_collision ->
		    NewCoords = simulate_command(Bid, Move, Curr, Coords),
		    NewMoves = Moves#{Bid => Rest},
		    NewOptMoves = maps:update_with(Bid, fun(Mvs) -> [Move|Mvs] end, OptMoves),
		    create_stalls(Bids, NewMoves, NewCoords, NewOptMoves)
	    end;
	error ->
	    %% For now this should be never called
	    create_stalls(Bids, Moves, Coords, OptMoves)
    end.
					  
is_there_any_collision(_Curr, _Move, [], _Moves, _Coords) ->
    no_collision;
is_there_any_collision(Curr, Move, [Bid|Bids], Moves, Coords) ->
    case maps:find(Bid, Moves) of
	error ->
	    is_there_any_collision(Curr, Move, Bids, Moves, Coords);
	{ok, []} ->
	    is_there_any_collision(Curr, Move, Bids, Moves, Coords);
	{ok, [Move2|_]} ->
	    Curr2 = maps:get(Bid, Coords),
	    case is_collision(Curr, Move, Curr2, Move2) of
		true ->
		    collision;
		false -> 
		    is_there_any_collision(Curr, Move, Bids, Moves, Coords)
	    end
    end.

is_collision(Curr1, {_, [Cd1 = {_,_,_}]}, Curr2, {_, [Cd2 = {_,_,_}]}) ->
    is_collision0(Curr1, Cd1, Curr2, Cd2).

is_collision0({Ax, Ay, Az}, {Cx, Cy, Cz}, {Bx, By, Bz}, {Dx, Dy, Dz}) ->
    MinAx = min(Ax, Ax + Cx),
    MaxAx = max(Ax, Ax + Cx),
    MinAy = min(Ay, Ay + Cy),
    MaxAy = max(Ay, Ay + Cy),
    MinAz = min(Az, Az + Cz),
    MaxAz = max(Az, Az + Cz),
    MinBx = min(Bx, Bx + Dx),
    MaxBx = max(Bx, Bx + Dx),
    MinBy = min(By, By + Dy),
    MaxBy = max(By, By + Dy),
    MinBz = min(Bz, Bz + Dz),
    MaxBz = max(Bz, Bz + Dz),
    MaxAx >= MinBx andalso MinAx =< MaxBx andalso
	MaxAy >= MinBy andalso MinAy =< MaxBy andalso
	MaxAz >= MinBz andalso MinAz =< MaxBz.

simulate_command(Bid, {smove, [Cd]}, Curr, Coords) ->
    NewCurr = bounding_box:add_coords(Curr, Cd),
    Coords#{Bid => NewCurr};
simulate_command(Bid, {lmove, [Cd]}, Curr, Coords) ->
    NewCurr = bounding_box:add_coords(Curr, Cd),
    Coords#{Bid => NewCurr};
simulate_command(Bid, {fission, [Cd, _]}, Curr, Coords) ->
    NewBidCoords = bounding_box:add_coords(Curr, Cd),
    NewBid = Bid+1,
    Coords#{NewBid => NewBidCoords};
simulate_command(Bid, {fusionp, [Cd]}, Curr, Coords) ->
    Coords;
simulate_command(Bid, {fusions, [Cd]}, Curr, Coords) ->
    maps:remove(Bid, Coords);
simulate_command(Bid, {halt, []}, Curr, Coords) ->
    #{};
simulate_command(Bid, _, Curr, Coords) ->
    Coords.



%% To trexw me map twn moves kai me map twn coordinates anti gia etsi opws to kanw twra 
%% wste na mporw eukola na prosthetw waits xwris na gamaw thn alhlouxia.

%% Algorithmos crash test. Ola me ola. An exoun ta bounding boxes twn grammwn 




