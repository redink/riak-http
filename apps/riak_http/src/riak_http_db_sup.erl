%%%-------------------------------------------------------------------
%%% @author tao1 <>
%%% @copyright (C) 2013, tao1
%%% @doc
%%%
%%% @end
%%% Created :  2 Jun 2013 by tao1 <>
%%%-------------------------------------------------------------------
-module(riak_http_db_sup).

-behaviour(supervisor).

-export([start_link/3]).
-export([cast/2, call/2]).

-export([init/1]).

-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].
behaviour_info(callbacks) ->                        
    [{get_name,1}];
behaviour_info(_Other) ->
    undefined.

start_link(CterName, CbMod, Args) ->
    Dp_num = 
	case application:get_env(riak_http, db_num) of 
	    undefined ->
		120;
	    {ok, Val} ->
		Val
	end,
    Ret = {ok, _Pid} = supervisor:start_link({local, CterName}, ?MODULE, [{callback, CbMod}, {args, Args}]),
    N = erlang:system_info(schedulers),
    [{ok, _} =  supervisor:start_child(CterName, [{db_master, I}]) || I <- lists:seq(1, N*Dp_num)],
    Ret. 

cast(CbMod, Req) ->
    Handler = select_handler(CbMod),
    gen_server:cast(Handler, Req).

call(CbMod, Req) ->
    Handler = select_handler(CbMod),
    gen_server:call(Handler, Req).


init([{callback, CbMod}, {args, Args} | _]) -> 
    Strategy = {simple_one_for_one, 10, 10},
    Mod = {undefined, {CbMod, start_link, Args},
		  permanent, 3000, worker, [CbMod]},
    {ok, {Strategy, [Mod]}}.

select_handler(CbMod) ->
    I = erlang:system_info(scheduler_id),
    CbMod:get_name(I).
