-module(parse_mdl).

-export([parse/1]).

parse(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    {R, Model} = parse_binary(Binary),
    %% io:format("Binary:~n~p~n", [{R, Binary}]),
    %% io:format("R: ~p~nDims: ~p, ~p, ~p~n", [R, length(Model), length(hd(Model)), length(hd(hd(Model)))]),
    {R, Model}.

parse_binary(Binary) ->
    <<R:8, ModelBinary/bitstring>> = Binary,
    io:format("Binary:~n~p~n", [ModelBinary]),
    XLines = parse_x_lines(R, ModelBinary, []),
    {R, XLines}.

parse_x_lines(_R, <<>>, Planes) ->
    lists:reverse(Planes);
parse_x_lines(R, BinPlanes, Planes) ->
    R2 = R*R,
    %% io:format("BinPlanes:~n~p~n", [BinPlanes]),
    %% io:format("BinPlanes Size: ~p~n", [erlang:bit_size(BinPlanes)]),
    %% io:format("R2: ~p~n", [R2]),
    %% Plane = binary:part(BinPlanes, {0, R2}),
    %% Rest = binary:part(BinPlanes, {R2, byte_size(BinPlanes) - R2}),
    <<Plane:R2/bitstring, Rest/bitstring>> = BinPlanes,
    YLines = parse_y_lines(R, Plane, []),
    %% io:format("BinPlane:~n~p~n", [Plane]),
    %% io:format("Plane:~n~p~n", [YLines]),
    parse_x_lines(R, Rest, [YLines|Planes]).

parse_y_lines(_R, <<>>, Lines) ->
    lists:reverse(Lines);
parse_y_lines(R, BinLines, Lines) ->
    %% io:format("BinLines:~n~p~n", [BinLines]),
    <<Line:R/bitstring, Rest/bitstring>> = BinLines,
    %% Line = binary:part(BinLines, {0, R}),
    %% Rest = binary:part(BinLines, {R, byte_size(BinLines) - R}),
    ZLine = parse_z_lines(Line, []),
    %% io:format("BinLine:~n~p~nLine:~n~p~n", [Line,ZLine]),
    parse_y_lines(R, Rest, [ZLine|Lines]).

parse_z_lines(<<>>, Line) ->
    lists:reverse(Line);
parse_z_lines(BinVoxels, Line) ->
    %% io:format("BinVoxels: ~p~n", [BinVoxels]),
    <<Voxel:1, Rest/bitstring>> = BinVoxels,
    parse_z_lines(Rest, [Voxel|Line]).
    

