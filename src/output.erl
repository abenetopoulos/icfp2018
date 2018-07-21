-module(output).
-export([write_trace_file/2]).

-record(enc_lng_dist, {a :: string(),
                       i :: integer()
                      }).

-record(enc_sht_dist, {a :: string(),
                       i :: integer()
                      }).

write_trace_file(Trace, OutputFile) ->
    EncodedTrace = lists:map(fun(Command) -> encode_command(Command) end, Trace),
    BinOfList = fun(L) ->
                        lists:foldl(fun(V, Acc) ->
                                            <<Acc/binary, V/binary>>
                                    end, <<>>, L)
                end,
    case file:open(OutputFile, [write, raw]) of
        {ok, FileD} ->
            WriteableTrace = lists:foldl(fun(Val, Acc) ->
                                                 <<Acc/binary, (BinOfList(Val))/binary>>
                                         end, <<>>, EncodedTrace),
            file:write(FileD, WriteableTrace),
            file:close(FileD);
        {error, Reason} -> throw(Reason)
    end.

encode_long_linear_distance({Dx, Dy, Dz}) ->
    case Dx of
        0 ->
            case Dy of
                0 ->
                    case Dz of
                        0 -> throw('Bad long linear distance value');
                        _ -> #enc_lng_dist{a = <<3:2>>, i = <<(Dz + 15):5>>}
                    end;
                _ -> #enc_lng_dist{a = <<2:2>>, i = <<(Dy + 15):5>>}
            end;
        _ -> #enc_lng_dist{a = <<1:2>>, i = <<(Dx + 15):5>>}
    end.

encode_short_linear_distance({Dx, Dy, Dz}) ->
    case Dx of
        0 ->
            case Dy of
                0 ->
                    case Dz of
                        0 -> throw('Bad long linear distance value');
                        _ -> #enc_sht_dist{a = <<3:2>>, i = <<(Dz + 5):4>>}
                    end;
                _ -> #enc_sht_dist{a = <<2:2>>, i = <<(Dy + 5):4>>}
            end;
        _ -> #enc_sht_dist{a = <<1:2>>, i = <<(Dx + 5):4>>}
    end.

encode_near_coordinate_difference({Dx, Dy, Dz}) ->
    <<((Dx + 1) * 9 + (Dy + 1) * 3 + (Dz + 1)):5>>.

encode_command({Command, Params}) ->
    case Command of
        halt -> [<<255:8>>];
        wait -> [<<254:8>>];
        flip -> [<<253:8>>];
        smove ->
            EncodedDistance = encode_long_linear_distance(hd(Params)),
            A = EncodedDistance#enc_lng_dist.a,
            I = EncodedDistance#enc_lng_dist.i,
            FirstByte = <<0:2, A/bitstring, 4:4>>,
            SecondByte = <<0:3, I/bitstring>>,
            [FirstByte, SecondByte];
        lmove ->
            EncodedDistance1 = encode_short_linear_distance(hd(Params)),
            EncodedDistance2 = encode_short_linear_distance(hd(tl(Params))),
            A1 = EncodedDistance1#enc_sht_dist.a,
            I1 = EncodedDistance1#enc_sht_dist.i,
            A2 = EncodedDistance2#enc_sht_dist.a,
            I2 = EncodedDistance2#enc_sht_dist.i,
            FirstByte = <<A2/bitstring, A1/bitstring, 12:4>>,
            SecondByte = <<I2/bitstring, I1/bitstring>>,
            [FirstByte, SecondByte];
        fusionp ->
            EncodedDistance = encode_near_coordinate_difference(hd(Params)),
            [<<EncodedDistance/bitstring, 7:3>>];
        fusions ->
            EncodedDistance = encode_near_coordinate_difference(hd(Params)),
            [<<EncodedDistance/bitstring, 6:3>>];
        fission ->
            EncodedDistance = encode_near_coordinate_difference(hd(Params)),
            [<<EncodedDistance/bitstring, 5:3>>, <<(hd(tl(Params))):8>>];
        fill ->
            EncodedDistance = encode_near_coordinate_difference(hd(Params)),
            [<<EncodedDistance/bitstring, 3:3>>]
    end.
