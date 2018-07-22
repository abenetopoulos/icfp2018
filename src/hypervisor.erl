-module(hypervisor).

-export([execute/2]).

-record(bot, {coords,   %% x,y,z coordinates
	      dir1,     %% x,y,z coordinates with ||dir|| = 1
	      dir2,     %% x,y,z coordinates with ||dir|| = 1
	      commands, %% The list of the commands the bot has until now
	      n_comms,  %% The number of commands or the time after the commands have finished
	      mode}).   %% Could either be building or destroying


execute_sequential(R, Model) ->
    ModelMap = topological:height_map(R, Model),
    InitialBotState = init_bot_state(),
    FinalBotState = 
	lists:foldl(
	  fun(Y, BotState) ->
		  bot_process_segment(BotState, {{1,Y,1},{R,Y,R}}, ModelMap)
	  end, InitialBotState, lists:seq(1, R-1)),
    MoveBack = bounding_box:move_robot_zxy(FinalBotState#bot.coords, {1,1,1}),
    OptimizedCommands = bounding_box:optimize_seq_trace(FinalBotState#bot.commands),
    complete_commands(OptimizedCommands ++ MoveBack).

complete_commands(Commands) ->
    Flip = {flip, []},
    Halt = {halt, []},    
    [Flip] ++ Commands ++ [Flip] ++ [Halt].

init_bot_state() ->
    #bot{coords = {1,1,1},
	 dir1 = {0,0,1},
	 dir2 = {1,0,0},
	 commands = [],
	 n_comms = 0,
	 mode = build}.

make_bots() ->
    {bots_data_structure,
     commands_to_create_bots}.

%% Segments are given to bots to fill/destroy
divide_in_segments(N, R) ->
    segments.

bot_process_segment(BotState, Segment, Model) ->
    {SegmentMin, SegmentMax} = Segment,
    %% There is no need for a bounding box as 
    %% we are going to optimize away moves without any fill in between them
    {NewBotState, NewCommands} = bot_move_loop(BotState, to_fill_fun(Model), Segment, []),
    %% io:format("NewCommands:~n~p~n", [NewCommands]),
    OptimizedCommands = bounding_box:optimize_seq_trace(NewCommands),
    %% io:format("OptCommands:~n~p~n", [OptimizedCommands]),
    FinalBotState = 
	NewBotState#bot{commands = NewBotState#bot.commands ++ OptimizedCommands,
			n_comms = NewBotState#bot.n_comms + length(OptimizedCommands)},
    FinalBotState.
    

bot_move_loop(BotState, ActionFun, Borders, AccCommands) ->
    #bot{coords = Coords, dir1 = Dir1, dir2 = Dir2} = BotState,
    NextPos = bounding_box:add_coords(Coords, Dir1),
    case in_region(NextPos, Borders) of
	false ->
	    NewDir1 = bounding_box:scale_coords(-1, Dir1),
	    NextPos2 = bounding_box:add_coords(Coords, Dir2),
	    case in_region(NextPos2, Borders) of
		false ->
		    %% TODO:I am not handling the last coordinate
		    NewDir2 = bounding_box:scale_coords(-1, Dir2),
		    NextPos3 = bounding_box:add_coords(Coords, {0,1,0}),
		    MoveCommand = {unoptimizable, {smove, [{0,1,0}]}},
		    ActionCommands = ActionFun(Coords, {0,1,0}),
		    NewAccCommands = lists:reverse(ActionCommands) ++ [MoveCommand|AccCommands],
		    NewBotState = BotState#bot{coords = NextPos3, dir1 = NewDir1, dir2 = NewDir2},
		    {NewBotState, lists:reverse(NewAccCommands)};
		true ->
		    MoveCommand = {smove, [Dir2]},
		    ActionCommands = ActionFun(Coords, Dir2),
		    NewAccCommands = lists:reverse(ActionCommands) ++ [MoveCommand|AccCommands],
		    NewBotState = BotState#bot{coords = NextPos2, dir1 = NewDir1},
		    bot_move_loop(NewBotState, ActionFun, Borders, NewAccCommands)
	    end;
	true ->
	    MoveCommand = {smove, [Dir1]},
	    ActionCommands = ActionFun(Coords, Dir1),
	    NewAccCommands = lists:reverse(ActionCommands) ++ [MoveCommand|AccCommands],
	    NewBotState = BotState#bot{coords = NextPos},
	    bot_move_loop(NewBotState, ActionFun, Borders, NewAccCommands)
    end.
		    
to_fill_fun(Model) ->	
    fun(Coords, MoveDir) ->
	    case bounding_box:model_get(Coords, Model) of
		0 -> 
		    [];
		1 ->
		    [{fill, [bounding_box:scale_coords(-1, MoveDir)]}]
	    end
    end.


in_region({X,Y,Z}, {{MinX, MinY, MinZ}, {MaxX, MaxY, MaxZ}}) ->
    X >= MinX andalso X =< MaxX andalso
    Y >= MinY andalso Y =< MaxY andalso
    Z >= MinZ andalso Z =< MaxZ.

point_to_fill_fun(Model) ->
    fun({X,Y,Z}) ->
	    bounding_box:model_get({X,Y,Z}, Model) =:= 1
    end.
