-module(parallel).

-export([spawn_bots/1,
	 gather_bots/1,
	 create_stalls/1]).

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

create_stalls(Moves) ->
    stub.
    %% create_stalls(Moves, [{1,1,1}]).

%% simulate_command(Bids, Commands, Coords) ->
    


simulate_commands(Moves, Coords) ->
    lists:flatten([simulate_command(Move, Curr) ||
		      {Move, Curr} <- lists:zip(Moves, Coords)]).


%% To trexw me map twn moves kai me map twn coordinates anti gia etsi opws to kanw twra 
%% wste na mporw eukola na prosthetw waits xwris na gamaw thn alhlouxia.

%% Algorithmos crash test. Ola me ola. An exoun ta bounding boxes twn grammwn 


simulate_command({smove, [Cd]}, Curr) ->
    [bounding_box:add_coords(Curr, Cd)];
simulate_command({lmove, [Cd]}, Curr) ->
    [bounding_box:add_coords(Curr, Cd)];
simulate_command({fission, [Cd, _]}, Curr) ->
    [Curr,bounding_box:add_coords(Curr, Cd)];
simulate_command({fusionp, [Cd]}, Curr) ->
    [Curr];
simulate_command({fusions, [Cd]}, Curr) ->
    [];
simulate_command(_, Curr) ->
    [Curr].




