-module(topological).
-export([sort/2,
	 height_map/2]).


height_map(R, Model) ->
    ListOfIndices = lists:seq(1, R),
    IndexedModel = lists:zip(ListOfIndices, 
			     lists:map(
			       fun (X) ->
				       lists:zip(ListOfIndices,
						 lists:map(
						   fun (Y) ->
							   lists:zip(ListOfIndices, Y)
						   end, X))
			       end, Model)),
    HeightMap = lists:foldl(fun({IndexX, X}, MapX) ->
                                    YMap = lists:foldl(fun ({IndexY, Y}, MapY) ->
                                                               ZMap = lists:foldl(fun ({IndexZ, Z}, MapZ) ->
                                                                                          maps:put(IndexZ, Z, MapZ)
                                                                                  end, maps:new(), Y),
                                                               maps:put(IndexY, ZMap, MapY)
                                                       end, maps:new(), X),
                                    maps:put(IndexX, YMap, MapX)
                            end, maps:new(), IndexedModel),
    HeightMap.

sort(R, Model) ->
    ListOfIndices = lists:seq(1, R),
    HeightMap = height_map(R, Model),
    Grounded = get_grounded(ListOfIndices, HeightMap),
    get_sort(HeightMap, sets:from_list(Grounded)).

get_grounded(ListOfIndices, HeightMap) -> get_grounded(ListOfIndices, HeightMap, []).

get_grounded([], _, Acc) -> Acc;
get_grounded([I | Is], HeightMap, Acc) ->
    Row = maps:get(0, maps:get(I, HeightMap)),
    get_grounded(Is, HeightMap, maps:fold(fun(ZIndex, Z, Filled) ->
                                                  case Z of
                                                      0 -> Filled;
                                                      1 -> [{I, 1, ZIndex} | Filled]
                                                  end
                                          end, [], Row) ++ Acc).

get_sort(HeightMap, AtLevel) ->
    get_sort(HeightMap, AtLevel, []).

get_sort(HeightMap, AtLevel, Previous) ->
    Neighbours = sets:fold(fun (Filled, Acc) ->
                                   FilledNeighbours = get_filled_neighbours(HeightMap, Filled),
                                   sets:union(Acc, FilledNeighbours)
                           end, sets:new(), AtLevel),
    %io:format("At level ~p~n", [AtLevel]),
    %io:format("Neighbours is ~p~n", [Neighbours]),
    FilteredNeighbours = lists:foldl(fun(P, Acc) -> sets:subtract(Acc, P) end, Neighbours, [AtLevel | Previous]),
    %io:format("FilteredNeighbours is ~p~n", [FilteredNeighbours]),
    case sets:size(FilteredNeighbours) of
        0 -> [AtLevel | Previous];
        _ -> get_sort(HeightMap, FilteredNeighbours, [AtLevel | Previous])
    end.

get_filled_neighbours(HeightMap, {X, Y, Z}) ->
    lists:foldl(fun({Xn, Yn, Zn}, Acc) ->
                        case maps:find(Xn, HeightMap) of
                            error -> Acc;
                            {ok, HeightMapY} ->
                                case maps:find(Yn, HeightMapY) of
                                    error -> Acc;
                                    {ok, HeightMapZ} ->
                                        case maps:find(Zn, HeightMapZ) of
                                            error -> Acc;
                                            {ok, 0} -> Acc;
                                            {ok, 1} -> sets:add_element({Xn, Yn, Zn}, Acc)
                                        end
                                end
                        end
                end, sets:new(), [{X - 1, Y, Z - 1}, {X - 1, Y, Z}, {X - 1, Y, Z + 1}, {X - 1, Y - 1, Z}, {X - 1, Y - 1, Z + 1}, {X - 1, Y + 1, Z}, {X - 1, Y - 1, Z - 1}, {X - 1, Y + 1, Z - 1}, {X - 1, Y + 1, Z + 1},
                                  {X + 1, Y, Z - 1}, {X + 1, Y, Z}, {X + 1, Y, Z + 1}, {X + 1, Y - 1, Z}, {X + 1, Y - 1, Z + 1}, {X + 1, Y + 1, Z}, {X + 1, Y - 1, Z - 1}, {X + 1, Y + 1, Z - 1}, {X + 1, Y + 1, Z + 1},
                                  {X, Y, Z - 1}, {X, Y, Z + 1}, {X, Y - 1, Z}, {X, Y + 1, Z}, {X, Y - 1, Z + 1}, {X, Y + 1, Z - 1}, {X, Y - 1, Z - 1}, {X, Y + 1, Z + 1}]).
