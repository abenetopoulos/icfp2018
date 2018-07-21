-module(app).

-export([main/0,
	 run_all/0]).

run_all() ->
    Testcases = lists:seq(1, 186),
    lists:foreach(
      fun(Testcase) ->
	      InputFilename = io_lib:format("problemsL/LA~3..0B_tgt.mdl", [Testcase]),
	      OutputFilename = io_lib:format("output/LA~3..0B.nbt", [Testcase]),
	      {R, Model} = parse_mdl:parse(InputFilename),
	      Traces = ultra_naive:print_model(R, Model),
	      %% io:format("Model:~n~p~n", [Model]),
	      %% io:format("Traces:~n~p~n", [Traces]),
	      output:write_trace_file(Traces, OutputFilename),
	      io:format("Testcase: ~p done!~n", [Testcase])
      end, Testcases).


main() ->
    {R, Model} = parse_mdl:parse("problemsL/LA076_tgt.mdl"),
    Traces = ultra_naive:print_model(R, Model),
    %% Traces = ultra_naive:print_model_parallel(R, Model),
    %% SpawnCommands = parallel:spawn_bots(4),
    %% io:format("Spawns: ~p~n", [SpawnCommands]),
    %% io:format("Model:~n~p~n", [Model]),
    %% io:format("Traces:~n~p~n", [Traces]),
    %% SortedModel = topological:sort(R, Model),
    %% io:format("Sorted model:~n~p~n~p~n", [SortedModel, lists:foldl(fun(S, Acc) -> Acc + sets:size(S) end, 0, SortedModel)]),
    output:write_trace_file(Traces, "out_test.nbt"),
    io:format("Wrote trace file~n").

out_test() ->
    output:write_trace_file([{halt, []}, {wait, []}, {flip, []}, {smove, [{12, 0, 0}]}, {lmove, [{0, 2, 0}, {-1, 0, 0}]},
                             {fusionp, [{1,2,3}]}, {fusions, [{1,2,3}]}, {fission, [{1,2,3}, 42]}, {fill, [{1,2,3}]}]),
    io:format("Trace written~n").

