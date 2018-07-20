-module(app).

-export([main/0]).

main() ->
    Model = parse_mdl:parse("problemsL/LA001_tgt.mdl"),
    io:format("Model:~n~p~n", [Model]).
