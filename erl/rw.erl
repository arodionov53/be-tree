-module(rw).


-export([ast_variables/0, to_l/1, mk_f/1, to_c/0, to_c/1]).
-export([tp_f/0, tp_f/1, to_file/0, to_file/1, to_file/2]).

% @doc Predefined ast variables.
% From  rtb_gateway/src/boolean_languages/flight_compiler.er'
ast_variables() ->
    [A, B] = variables(),
    A ++ B.

variables() ->
    [
        [
            {bidder_ids, int_list, disallow_undefined},
            {exchange, int, disallow_undefined},
            {member_id, int, disallow_undefined},
            {member_alphanumeric_id, bin, allow_undefined},
            {country, bin, allow_undefined},
            {region, bin, allow_undefined},
            {city, bin, allow_undefined},
            {pc, bin, allow_undefined},
            {dma, int, allow_undefined},
            {lg, bin, allow_undefined},
            {doubleclick_verticals, int_list, allow_undefined},
            {iab_categories, int_list, allow_undefined},
            {device_type_id, int, disallow_undefined},
            {device_id, int, disallow_undefined},
            {operating_system_id, int, disallow_undefined},
            {operating_system_variant_id, int, disallow_undefined},
            {browser_id, int, disallow_undefined},
            {browser_variant_id, int, disallow_undefined},
            {matched_user, bool, disallow_undefined},
            {matched_tv, bool, disallow_undefined},
            {matched_ip, bool, disallow_undefined},
            {matched_idl_pid, bool, disallow_undefined},
            {matched_idl_hhid, bool, disallow_undefined},
            {latitude, float, allow_undefined},
            {longitude, float, allow_undefined},
            {device_located, bool, disallow_undefined},
            {frequency_caps, frequency_caps, allow_undefined},
            {sitelist_ids, int_list, disallow_undefined},
            {adgear_segments, int_list, allow_undefined},
            {segments_with_timestamp, segments, allow_undefined},
            {ip, bin, allow_undefined},
            {now, int64, disallow_undefined},
            {iplist_ids, int_list, allow_undefined},
            {tvidlist_ids, int_list, allow_undefined},
            {ias_segments, int_list, allow_undefined},
            {exchange_seller_app_id, int, allow_undefined},
            {exchange_seller_site_id, int, allow_undefined},
            {geo_radius_list_ids, int_list, disallow_undefined},
            {lotame_segments, int_list, allow_undefined},
            {liveramp_1p_segments, int_list, allow_undefined},
            {liveramp_3p_segments, int_list, allow_undefined},
            {adobe_aam_segments, int_list, allow_undefined},
            {nielsen_segments, int_list, allow_undefined},
            {bluekai_segments, int_list, allow_undefined},
            {bluekai_3p_segments, int_list, allow_undefined},
            {bluekai_custom_segments, int_list, allow_undefined},
            {neustar_segments, int_list, allow_undefined},
            {krux_1p_segments, int_list, allow_undefined},
            {unified_segments, int_list, allow_undefined},
            {applist_ids, int_list, disallow_undefined},
            {impression_type, bin, allow_undefined},
            {publisher_id, int, allow_undefined},
            {ssl, bool, disallow_undefined},
            {isp, bin, allow_undefined},
            {org_name, bin, allow_undefined},
            {uid_type, int, allow_undefined},
            {genre, bin, allow_undefined},
            {netspeed, bin, allow_undefined},
            {rand, int, disallow_undefined},
            {stv_model, bin, allow_undefined},
            {merkle_segments, int_list, allow_undefined},
            {ads_txt_relationship, int, allow_undefined},
            {samsung_device_country, bin, allow_undefined},
            {marketing_name, bin, allow_undefined},
            {genre_array, bin_list, allow_undefined}
        ],
        [
            {width, int, disallow_undefined},
            {height, int, disallow_undefined},
            {types, int_list, disallow_undefined},
            {position, bin, allow_undefined},
            %% should not be undefined
            {deal_ids, bin_list, allow_undefined},
            {exchange_seat_ids, int_list, allow_undefined},
            {private, bool, disallow_undefined},
            {video_start_delay, int, allow_undefined},
            {min_duration, int, allow_undefined},
            {max_duration, int, allow_undefined},
            {player_w, int, allow_undefined},
            {player_h, int, allow_undefined},
            {player_premium, bool, allow_undefined},
            {hdmi_vendor, bin, allow_undefined},
            {hdmi_product, bin, allow_undefined},
            {contentgrouplists, int_list, allow_undefined},
            {vpaid, int_list, allow_undefined}
        ]
    ].


% @doc write c code. example:
% betree_add_boolean_variable(tree, "ssl", true);
to_c() -> to_c(ast_variables()).

to_c(Vars) ->
    lists:foreach(fun(A) -> io:format(mk_f(A), A) end, to_l(Vars)).

to_l([]) -> [];
to_l([{Name, Type, Allow} | Tail]) -> 
    [[fun_name(Type), tree, atom_to_list(Name), allow(Allow) | limits(Type)] | to_l(Tail)].


fun_name(bin) -> 'betree_add_string_variable';
fun_name(bin_list) -> 'betree_add_string_list_variable';
fun_name(bool) -> 'betree_add_boolean_variable';
fun_name(int) -> 'betree_add_integer_variable';
fun_name(int64) -> 'betree_add_integer_variable';
fun_name(int_list) -> 'betree_add_integer_list_variable';
fun_name(float) -> 'betree_add_float_variable';
fun_name(segments) -> 'betree_add_segments_variable';
fun_name(frequency_caps) -> 'betree_add_frequency_caps_variable'.

allow(allow_undefined) -> true;
allow(disallow_undefined) -> false.

limits(int) -> ['INT64_MIN', 'INT64_MAX'];
limits(int64) -> ['INT64_MIN', 'INT64_MAX'];
limits(int_list) -> ['INT64_MIN', 'INT64_MAX'];
limits(float) -> ['FLT_MIN', 'FLT_MAX'];
limits(bin) -> ['SIZE_MAX'];
limits(bin_list) -> ['SIZE_MAX'];
limits(segmens) -> ['SIZE_MAX'];
limits(_) -> [].

mk_f([_ | Tail]) -> "~p(" ++ mk_fl(Tail) ++ ");~n".

mk_fl([_]) -> "~p";
mk_fl([_ | Tail]) -> "~p, " ++ mk_fl(Tail).  
% rw:to_l(rw:ast_variables()).
% rw:to_c(rw:ast_variables()).

-spec tp_l({atom(), atom(), atom()}) -> {atom(), atom(), atom()}.
tp_l({Name, Type, Allow}) -> {Name, mk_type(Type), allow(Allow)}.

% @doc write data for betree_def to console. Example
% liveramp_3p_segments|integer list|false
% adobe_aam_segments|integer list|false
% nielsen_segments|integer list|false
tp_f() -> tp_f(ast_variables()).

tp_f([]) -> io:format("~n");
tp_f([F | Tail]) ->
    {N, T, A} = tp_l(F),
    io:format("~s|~s|~s~n", [N, T, A]),
    tp_f(Tail).

mk_type(bin) -> 'string';
mk_type(bin_list) -> 'string list';
mk_type(bool) -> 'boolean';
mk_type(int) -> 'integer';
mk_type(int64) -> 'integer';
mk_type(int_list) -> 'integer list';
mk_type(float) -> 'float';
mk_type(segments) -> 'segments';
mk_type(frequency_caps) -> 'integer enum'.


% @doc write data for betree_def to file. Example
% liveramp_3p_segments|integer list|false
% adobe_aam_segments|integer list|false
% nielsen_segments|integer list|false
-spec to_file() -> ok.
to_file() -> to_file("betree_defs").

-spec to_file(string()) -> ok.
to_file(Fname) -> to_file(Fname, ast_variables()).

to_file(Fname, Varlist) ->
    S = lists:flatten([io_lib:fwrite("~s|~s|~s~n", tuple_to_list(tp_l(V))) || V <- Varlist]),
    file:write_file(Fname, S).


% 310KQ04883    





% {ok, File} = file:open("/tmp/id-const-expr.txt", [write]).
% recon_trace:calls({erl_betree, betree_make_sub, '_'}, 10000, [{formatter, fun ({trace,_,call,{_,_,[_,ExpressionId, [{campaign_group_id, CGID},{campaign_id, CID},{advertiser_id, AID},{flight_id, FID}], Expression]}}) 
% -> io_lib:format("~p\t\t~p\t\t~p\t\t~p\t\t~p\t\t~s~n", [ExpressionId,CGID,CID,AID,FID, Expression]) end}, {io_server, File}]).
% recon_trace:clear().
% file:close(File).

% awk -F'\t\t' '{print $3","$4","$5}' id-const-expr.txt  > betree_constants
% awk -F'\t\t' '{print $6}' id-const-expr.txt  > betree_exprs 
% 

% rw:tp_f().
% 
% 
