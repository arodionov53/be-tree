% 
% Convers files with raw events from betree_events_raw to proper format betree_events
% For example
% convert_events:convert_files("betree_events_raw", "../build/tests/data/betree_events").
% 
% make shure that thre are no \n
% in editor change:
% "\r" -> [13]
% "\t" -> [9]
% 
-module(convert_events).


-export([convert_files/0, convert_files/1, convert_files/2]).
-export([flight_request_variables/1, flight_impression_variables/1, flight_vpaid_variables/1]).
% -export[to_file/0, to_file/1, to_file/2].
-export([t_to_l/1, convert/1, beautify/1]).
-export([test/0, test/1, tbeautify/1]).


-record(flight_request_variables, {
    bidder_ids,
    exchange,
    member_id,
    member_alphanumeric_id,
    country,
    region,
    city,
    pc,
    dma,
    lg,
    doubleclick_verticals,
    iab_categories,
    device_type_id,
    device_id,
    operating_system_id,
    operating_system_variant_id,
    browser_id,
    browser_variant_id,
    matched_user,
    matched_tv,
    matched_ip,
    matched_idl_pid,
    matched_idl_hhid,
    latitude,
    longitude,
    device_located,
    frequency_caps,
    sitelist_ids,
    adgear_segments,
    segments_with_timestamp,
    ip,
    now,
    iplist_ids,
    tvidlist_ids,
    ias_segments,
    exchange_seller_app_id,
    exchange_seller_site_id,
    geo_radius_list_ids,
    lotame_segments,
    liveramp_1p_segments,
    liveramp_3p_segments,
    adobe_aam_segments,
    nielsen_segments,
    bluekai_segments,
    bluekai_3p_segments,
    bluekai_custom_segments,
    neustar_segments,
    krux_1p_segments,
    unified_segments,
    applist_ids,
    impression_type,
    publisher_id,
    ssl,
    isp,
    org_name,
    uid_type,
    genre,
    netspeed,
    rand,
    stv_model,
    merkle_segments,
    ads_txt_relationship,
    samsung_device_country,
    marketing_name,
    % genre_array
    genre_array,
    consent_vendor_ids,
    tcf_optout
}).

% -type flight_request_variables() :: #flight_request_variables{}.

-record(flight_impression_variables, {
    width,
    height,
    types,
    position,
    deal_ids,
    exchange_seat_ids,
    private,
    video_start_delay,
    min_duration :: undefined | non_neg_integer(),
    max_duration :: undefined | pos_integer(),
    player_w,
    player_h,
    player_premium :: undefined | boolean(),
    hdmi_vendor :: undefined | binary(),
    hdmi_product :: undefined | binary(),
    contentgrouplists :: undefined | list(pos_integer())
}).

% -type flight_impression_variables() :: #flight_impression_variables{}.

-record(flight_vpaid_variables, {
    vpaid
}).

% -type flight_vpaid_variables() :: #flight_vpaid_variables{}.

-define(RECORD_FR_TUPLE(RECORD_NAME),
    RECORD_NAME(Tpl) -> 
        [RECORD_NAME | Tail] = t_to_l(Tpl),
        lists:zip(record_info(fields, RECORD_NAME), Tail)
    ).

% converts tuple to list
-spec t_to_l(tuple()) -> list().
t_to_l(Tpl) when is_tuple(Tpl) -> [element(Pos,Tpl) || Pos <- lists:seq(1,tuple_size(Tpl))].

?RECORD_FR_TUPLE(flight_request_variables).
?RECORD_FR_TUPLE(flight_impression_variables).
?RECORD_FR_TUPLE(flight_vpaid_variables).

-spec convert([tuple()]) -> [tuple()]. 
convert([]) -> [];
convert([Tpl | Tail]) ->
    case element(1, Tpl)  of
        flight_request_variables -> flight_request_variables(Tpl);
        flight_impression_variables -> flight_impression_variables(Tpl);
        flight_vpaid_variables -> flight_vpaid_variables(Tpl);
        _ -> []
    end ++ convert(Tail).

tbeautify({segments_with_timestamp, Vals}) ->
    [atom_to_list(segments_with_timestamp), sbeutify(Vals)];
tbeautify({frequency_caps, Vals}) ->
    [atom_to_list(frequency_caps), fbeutify(Vals)];
tbeautify({Name, Value}) ->
    [atom_to_list(Name), vbeautify(Value)].

vbeautify(Value) when is_binary(Value) -> binary_to_list(Value);
vbeautify(Value) when is_list(Value) -> [vbeautify(V) || V <- Value];
vbeautify(Value) -> Value.

-spec beautify([tuple()]) -> [list()]. 
beautify(Tpl) -> [tbeautify(T) || T = {_, V} <- Tpl, V /= undefined].

-spec fbeutify([tuple()]) -> [tuple()].
fbeutify(TList) -> 
    [ftbeutify({{S1,K,S2}, M, N}) || {{S1,K,S2}, M, N} <- TList].

ftbeutify({{S1,K,S2}, M, N}) ->
    [[binary_to_list(S1), K,  binary_to_list(S2)], M, N].

sbeutify(Vals) ->
    [[A,B] || {A,B} <- Vals].

-spec fwrt(list([_|_])) -> list().
fwrt(Lst) -> fwrt(Lst, []).

io_print(X) -> io_lib:print(X, 1, 1000000, -1).

fwrt([[A, B] | Tail], []) when is_list(B) -> 
    fwrt(Tail, [io_lib:fwrite("{~p:", [A]) ++ io_print(B)]);
fwrt([[A, B] | Tail], []) -> 
    fwrt(Tail, [io_lib:fwrite("{~p:~p", [A, B])]);
fwrt([[A, B] | Tail], Acc) when is_list(B) -> 
    fwrt(Tail, [io_lib:fwrite(",~p:",[A]) ++ io_print(B) | Acc]); 
fwrt([[A, B] | Tail], Acc) -> 
    fwrt(Tail, [io_lib:fwrite(",~p:~p",[A, B]) | Acc]); 
fwrt([], []) ->
    []; 
fwrt([], Acc) -> 
    lists:reverse([io_lib:fwrite("}\n", []) | Acc]).

to_file(Fname, TpList) -> to_file(Fname, TpList, [append]).

to_file(Fname, TpList, Options) ->
    Txt = fwrt(beautify(convert(TpList))),
    S = lists:flatten(Txt),
    file:write_file(Fname, S, Options).


convert_files() -> convert_files("betree_events_raw").

convert_files(FileFrom) -> convert_files(FileFrom, "../build/tests/data/betree_events").

convert_files(FileFrom, FileTo) ->
    {ok, [E | Events]} = file:consult(FileFrom),
    to_file(FileTo, E, [write]),
    lists:foreach(fun(X) -> to_file(FileTo, X) end, Events).


test() -> test(1).
test(1) ->
    [{flight_vpaid_variables,[]}, 
    {flight_request_variables,[1,4],
                        11,0,<<"0">>,<<"US">>,<<"NC">>,<<"durham">>,
                        <<"27707">>,560,undefined,undefined,
                        [190000],
                        5,6,20,0,0,0,true,false,true,false,false,35.980598,
                        -78.842598,true,

                        % [],
                        [{{<<"advertiser:ip">>,8093,<<"3607239">>},
                            2,1691381791000000},
                        {{<<"campaign:ip">>,133067,<<"3626330">>},
                            8,1690092479000000},
                        {{<<"campaign:ip">>,133592,<<"3627253">>},
                            1,1690669148000000},
                        {{<<"campaign:ip">>,137992,<<"3634682">>},
                            1,1691255548000000},
                        {{<<"campaign:ip">>,137992,<<"3634684">>},
                            1,1691255548000000}],

                        [],[],
                        [{52665,1691105826267143},{52662,1691105826267634}],
                        <<"75.189.203.2">>,
                        1691430589,[],[],undefined,undefined,undefined,
                        [24089],
                        [],[],[],[],[],[],[],[],[],[],
                        [187622,108684,10079,9177],
                        [111, 828,1085,1271,1310,1984,2614,2932,2954,2980,3006,3152,3308,3309],
                        <<"app">>,undefined,true,<<"Spectrum">>,
                        <<"Spectrum">>,2,undefined,<<"Cable/DSL">>,858181,
                        <<>>,[],2,undefined,<<"UNKNOWN">>,undefined
                        ,undefined,undefined
                    },
    {flight_impression_variables,1,1,
                            [3],
                            undefined,
                            [<<"PM-CMFV-3433">>,<<"PM-PZXL-5134">>],
                            undefined,false,0,5,120,1920,1080,true,
                            undefined,undefined,[]}];
test(2) ->
    [{product_request_variables,6,<<"US">>,<<"GA">>,<<"atlanta">>,33.7485,
                            -84.3871,<<"30302">>,524,true,<<"en">>,5,12,19,0,
                            0,0,[],103,undefined,1694790415},
    {product_impression_variables,2,2}].
                            
% convert_events:beautify(convert_events:convert(convert_events:test())).
% lists:foreach(fun(X) -> io:format("~p:~p,~n", X) end, convert_events:beautify(convert_events:convert(convert_events:test()))).
% 