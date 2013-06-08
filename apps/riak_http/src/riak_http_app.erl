%%%-------------------------------------------------------------------
%%% @author redink <>
%%% @copyright (C) 2013, redink
%%% @doc
%%%
%%% @end
%%% Created :  6 Jan 2013 by redink <>
%%%-------------------------------------------------------------------

-module(riak_http_app).
-behaviour(application).

-export([start/2]).

-export([stop/1]).

-include("logger.hrl").
-include("records.hrl").

start(_Type, _Args) ->
    lager:start(),
    pg2:start_link(),
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),

    ensure_start_db_server(),
    ensure_ping_other_node(),
    
    riak_http_sup:start_link().


stop(_State) ->
    ok.

ensure_start_db_server() ->
    Dispatch = cowboy_router:compile([
				      {'_', [
					     {"/dbop/cb", riak_http_db_server_op_cb, []},
					     {"/dbop/lb", riak_http_db_server_op_lb, []},
					     {"/dbop/lk", riak_http_db_server_op_lk, []},
					     {"/dbop/iv", riak_http_db_server_op_iv, []},
					     {"/dbop/dv", riak_http_db_server_op_dv, []},
					     {"/dbop/uv", riak_http_db_server_op_uv, []},
					     {"/dbop/sv", riak_http_db_server_op_sv, []},
					     {"/dbop/db", riak_http_db_server_op_db, []}
					    ]}
				     ]),
    Port = 
	case application:get_env(riak_http, web_port) of
	    undefined ->
		8081;
	    {ok, Val} ->
		Val
	end,
    case cowboy:start_http(dbserver, 
			   200, 
			   [{port, Port}], 
			   [{env, [{dispatch, Dispatch}]}]) of
	{ok, _} ->
	    ok;
	_Any ->
	    ?LOG(["start db server error:", _Any])
    end.

ensure_ping_other_node() ->
    case net_adm:host_file() of
	{error,enoent} ->
	    ingore;
	Node_List ->
	    [net_adm:ping(A) || A <- Node_List -- [node()]]
    end.
