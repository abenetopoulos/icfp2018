-module(app).

-export([main/0]).

main() ->
    {R, Model} = parse_mdl:parse("problemsL/LA001_tgt.mdl"),
    Traces = ultra_naive:print_model(R, Model),
    io:format("Model:~n~p~n", [Model]),
    io:format("Traces:~n~p~n", [Traces]).
