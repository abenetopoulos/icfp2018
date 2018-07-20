-module(app).

-export([main/0]).

main() ->
    Model = parse_mdl:parse("problemsL/LA001_tgt.mdl"),
    io:format("Model:~n~p~n", [Model]),
    output:write_trace_file([{halt, []}, {wait, []}, {flip, []}, {smove, [{12, 0, 0}]}, {lmove, [{0, 2, 0}, {-1, 0, 0}]},
                             {fusionp, [{1,2,3}]}, {fusions, [{1,2,3}]}, {fission, [{1,2,3}, 42]}, {fill, [{1,2,3}]}]),
    io:format("Trace written~n"),
    {R, Model} = parse_mdl:parse("problemsL/LA001_tgt.mdl"),
    Traces = ultra_naive:print_model(R, Model),
    io:format("Model:~n~p~n", [Model]),
    io:format("Traces:~n~p~n", [Traces]).