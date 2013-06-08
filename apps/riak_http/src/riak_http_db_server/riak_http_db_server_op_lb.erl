%%%-------------------------------------------------------------------
%%% @author tao <>
%%% @copyright (C) 2013, tao
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2013 by tao <>
%%%-------------------------------------------------------------------
-module(riak_http_db_server_op_lb).

-export([init/3]).
-export([content_types_provided/2]).
-export([riak_http_handle/2]).

-include("logger.hrl").
-include("records.hrl").

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>,        riak_http_handle},
      {<<"application/json">>, riak_http_handle},
      {<<"text/plain">>,       riak_http_handle}
     ], Req, State}.

riak_http_handle(Req, State) ->
    {Qs, _} = cowboy_req:qs(Req),
    Body    = handle_db_lb(get_params(Qs)),
    {Body, Req, State}.

get_params(_Qs) ->
    {}.

handle_db_lb({}) ->
    case catch verify_params() of
	{Error_Code, Error_Msg} ->
	    riak_http_util:build_body(Error_Code, Error_Msg);
	ok ->
	    handle_db_lb_do()
    end.

handle_db_lb_do() ->
    {ok, Result} = riak_http_db:get_all_buckets(),
    Return_Result = [erlang:binary_to_list(A) 
		     || A <- Result],
    riak_http_util:build_body("0", Return_Result).
	
verify_params() ->
    ok.		    
