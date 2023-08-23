F = fun G() -> receive M -> io:format("~p got: ~p\n", [self(), M]) end, G() end.

PI = spawn(fun() -> F() end).

is_process_alive(PI). 

{P, Args} = erlang:system_monitor(PI, [{long_schedule, 100}]).

erlang:system_monitor(P, Args).

exit(PI, kill).

% whereis(mon).



% (rtb-gateway-0@lga-gateway-67.sys.adgear.com)2> erlang:system_monitor(whereis(mon), [{long_schedule, 1000}]).
% {<0.450.0>,
%  [busy_dist_port,busy_port,
%   {large_heap,10485760},
%   {long_gc,50}]}

% erlang:system_monitor(whereis(mon), [{long_schedule, 100}]).


% <8183.25868.1003> got: {monitor,<8183.1294.0>,long_schedule,
%                                 [{timeout,187},
%                                  {in,{lists,foreach,2}},
%                                  {out,{lists,foreach,2}}]}
% <8183.25868.1003> got: {monitor,<8183.1294.0>,long_schedule,
%                                 [{timeout,279},
%                                  {in,{lists,foreach,2}},
%                                  {out,{lists,foreach,2}}]}
% <8183.25868.1003> got: {monitor,<8183.1294.0>,long_schedule,
%                                 [{timeout,148},
%                                  {in,{flights,generate_flight_fcap_mapping,2}},
%                                  {out,undefined}]}
% <8183.25868.1003> got: {monitor,<8183.1294.0>,long_schedule,
%                                 [{timeout,177},
%                                  {in,{jsx_parser,count,3}},
%                                  {out,undefined}]}
% {<0.25868.1003>,[{long_schedule,100}]}
% 
% 
% (rtb-gateway-0@lga-gateway-67.sys.adgear.com)7> erlang:system_monitor(whereis(mon), [{long_schedule, 50}]). 
% {<0.25868.1003>,[{long_schedule,100}]}
% <8183.25868.1003> got: {monitor,<8183.1487.0>,long_schedule,
%                                 [{timeout,115},
%                                  {in,{gen_server,loop,7}},
%                                  {out,{rig_utils,match_all,1}}]}
% <8183.25868.1003> got: {monitor,<8183.1487.0>,long_schedule,
%                                 [{timeout,295},
%                                  {in,{rig_utils,match_all,1}},
%                                  {out,{rig_utils,match_all,1}}]}
% <8183.25868.1003> got: {monitor,<8183.1487.0>,long_schedule,
%                                 [{timeout,148},
%                                  {in,{rig_utils,match_all,1}},
%                                  {out,{rig_utils,match_all,1}}]}
% <8183.25868.1003> got: {monitor,<8183.1487.0>,long_schedule,
%                                 [{timeout,214},
%                                  {in,{rig_utils,match_all,1}},
%                                  {out,{rig_utils,match_all,1}}]}
% <8183.25868.1003> got: {monitor,<8183.1487.0>,long_schedule,
%                                 [{timeout,149},
%                                  {in,{rig_utils,match_all,1}},
%                                  {out,{rig_utils,match_all,1}}]}
% <8183.25868.1003> got: {monitor,<8183.1487.0>,long_schedule,
%                                 [{timeout,173},
%                                  {in,{rig_utils,match_all,1}},
%                                  {out,{rig_utils,match_all,1}}]}
% <8183.25868.1003> got: {monitor,<8183.1487.0>,long_schedule,
%                                 [{timeout,193},
%                                  {in,{rig_utils,match_all,1}},
%                                  {out,{rig_utils,match_all,1}}]}
% <8183.25868.1003> got: {monitor,<8183.1487.0>,long_schedule,
%                                 [{timeout,200},
%                                  {in,{rig_utils,match_all,1}},
%                                  {out,{rig_utils,match_all,1}}]}
% <8183.25868.1003> got: {monitor,<8183.1487.0>,long_schedule,
%                                 [{timeout,162},
%                                  {in,{rig_utils,match_all,1}},
%                                  {out,{rig_utils,match_all,1}}]}
% <8183.25868.1003> got: {monitor,<8183.1487.0>,long_schedule,
%                                 [{timeout,153},
%                                  {in,{rig_utils,match_all,1}},
%                                  {out,{rig_utils,match_all,1}}]}
% <8183.25868.1003> got: {monitor,<8183.1487.0>,long_schedule,
%                                 [{timeout,155},
%                                  {in,{rig_utils,match_all,1}},
%                                  {out,{rig_utils,match_all,1}}]}


% <8183.25868.1003> got: {monitor,<8183.1294.0>,long_schedule,
%                                 [{timeout,166},
%                                  {in,{flights,generate_flight_fcap_mapping,2}},
%                                  {out,undefined}]}
% 
% <8183.25868.1003> got: {monitor,<8183.1294.0>,long_schedule,
% {in,{lists,foreach,2}},
% {out,
%     {yabee_symbols_table,
%         '-build_symbols_table/1-fun-0-',2}}]}
% <8183.25868.1003> got: {monitor,<8183.1294.0>,long_schedule,
%     [{timeout,222},
%      {in,{segments,'-compile_/3-fun-0-',2}},
%      {out,{lists,foreach,2}}]}


% (rtb-gateway@lga-exchange-27.sys.adgear.com)5> {P, Args} = Q.
% {<0.778.0>,
%  [busy_dist_port,busy_port,
%   {large_heap,10485760},
%   {long_gc,50}]}
% <8183.19408.125> got: {monitor,<8183.12870.127>,long_schedule,
%                           [{timeout,101},
%                            {in,{rtb_gateway_frequency_caps,filter_flights,5}},
%                            {out,
%                                {rtb_gateway_frequency_caps,filter_flights,
%                                    5}}]}
% <8183.19408.125> got: {monitor,<8183.2050.0>,long_schedule,
%                                [{timeout,166},
%                                 {in,{lists,foreach,2}},
%                                 {out,{lists,foreach,2}}]}
% <8183.19408.125> got: {monitor,<8183.2050.0>,long_schedule,
%                                [{timeout,256},
%                                 {in,{lists,foreach,2}},
%                                 {out,{lists,foreach,2}}]}
% <8183.19408.125> got: {monitor,<8183.2050.0>,long_schedule,
%                                [{timeout,133},
%                                 {in,{lists,foreach,2}},
%                                 {out,{lists,foreach,2}}]}
% <8183.19408.125> got: {monitor,<8183.2050.0>,long_schedule,
%                                [{timeout,227},
%                                 {in,{flights,generate_flight_fcap_mapping,2}},
%                                 {out,undefined}]}
% <8183.19408.125> got: {monitor,<8183.2050.0>,long_schedule,
%                                [{timeout,101},{in,undefined},{out,undefined}]}
% <8183.19408.125> got: {monitor,<8183.2050.0>,long_schedule,
%                                [{timeout,429},{in,undefined},{out,undefined}]}
% (rtb-gateway@lga-exchange-27.sys.adgear.com)6> 


% 8183.19408.125> got: {monitor,<8183.3261.0>,long_schedule,
%                                [{timeout,122},
%                                 {in,{rig_utils,match_all,1}},
%                                 {out,{rig_utils,match_all,1}}]}
% <8183.19408.125> got: {monitor,<8183.3261.0>,long_schedule,
%                                [{timeout,305},
%                                 {in,{rig_utils,match_all,1}},
%                                 {out,{rig_utils,match_all,1}}]}
% <8183.19408.125> got: {monitor,<8183.31012.130>,long_schedule,
%                           [{timeout,118},
%                            {in,{rtb_gateway_external_service,wait_identifyd,
%                                    4}},
%                            {out,
%                                {pipe_products,
%                                    '-update_imp_deals/3-lc$^3/1-0-',1}}]}
% <8183.19408.125> got: {monitor,<8183.13124.141>,long_schedule,
%                           [{timeout,120},
%                            {in,undefined},
%                            {out,
%                                {yabee_cmp,'-get_list_var_fun_/2-fun-22-',3}}]}
% <8183.19408.125> got: {monitor,<8183.13284.141>,long_schedule,
%                                [{timeout,115},
%                                 {in,{zlib,append_iolist,2}},
%                                 {out,{zlib,append_iolist,2}}]}
% <8183.19408.125> got: {monitor,<8183.3261.0>,long_schedule,
%                                [{timeout,315},
%                                 {in,{rig_utils,match_all,1}},
%                                 {out,{rig_utils,match_all,1}}]}
% <8183.19408.125> got: {monitor,<8183.3261.0>,long_schedule,


% <8183.19408.125> got: {monitor,<8183.2252.0>,long_schedule,
%                                [{timeout,143},
%                                 {in,{gen_statem,loop_receive,3}},
%                                 {out,{gen_statem,loop_receive,3}}]}
% <8183.19408.125> got: {monitor,<8183.3559.141>,long_schedule,
%                           [{timeout,131},
%                            {in,{cowboy_router,'-split_path/2-lc$^1/1-1-',1}},
%                            {out,{rtb_gateway_exchange,wait_request_,2}}]}
% <8183.19408.125> got: {monitor,<8183.8872.141>,long_schedule,
%                           [{timeout,139},
%                            {in,{rtb_gateway_external_service,wait_identifyd,
%                                    4}},
%                            {out,
%                                {rtb_gateway_external_service,receive_all,4}}]}
% <8183.19408.125> got: {monitor,<8183.3546.141>,long_schedule,
%                           [{timeout,146},
%                            {in,{cowboy_router,'-split_path/2-lc$^1/1-1-',1}},
%                            {out,{rtb_gateway_exchange,wait_request_,2}}]}
% <8183.19408.125> got: {monitor,<8183.8792.141>,long_schedule,
%                           [{timeout,114},
%                            {in,{rtb_gateway_external_service,wait_identifyd,
%                                    4}},
%                            {out,
%                                {rtb_gateway_external_service,receive_all,4}}]}
% <8183.19408.125> got: {monitor,<8183.8713.141>,long_schedule,
%                                [{timeout,267},
%                                 {in,{prometheus_counter,inc,4}},
%                                 {out,{rtb_gateway_pacing,explode_pacings,2}}]}


% (rtb-gateway-0@lga-gateway-67.sys.adgear.com)4> Q = erlang:system_monitor(PI, [{long_schedule, 100}]).
% {<0.432.0>,
%  [busy_dist_port,busy_port,
%   {large_heap,10485760},
%   {long_gc,50}]}
% <8183.10648.13> got: {monitor,<8183.1668.0>,long_schedule,
%                               [{timeout,305},
%                                {in,{rig_utils,match_all,1}},
%                                {out,{rig_utils,match_all,1}}]}
% <8183.10648.13> got: {monitor,<8183.1668.0>,long_schedule,
%                               [{timeout,313},
%                                {in,{rig_utils,match_all,1}},
%                                {out,{rig_utils,match_all,1}}]}
% <8183.10648.13> got: {monitor,<8183.1668.0>,long_schedule,
%                               [{timeout,440},
%                                {in,{rig_utils,match_all,1}},
%                                {out,{rig_utils,match_all,1}}]}
% <8183.10648.13> got: {monitor,<8183.1668.0>,long_schedule,
%                               [{timeout,374},
%                                {in,{rig_utils,match_all,1}},
%                                {out,{rig_utils,match_all,1}}]}
% <8183.10648.13> got: {monitor,<8183.1668.0>,long_schedule,
%                               [{timeout,710},
%                                {in,{rig_utils,match_all,1}},
%                                {out,{gen_server,loop,7}}]}
% 


