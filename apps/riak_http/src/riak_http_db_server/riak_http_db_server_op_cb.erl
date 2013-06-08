%%%-------------------------------------------------------------------
%%% @author tao <>
%%% @copyright (C) 2013, tao
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2013 by tao <>
%%%-------------------------------------------------------------------
-module(riak_http_db_server_op_cb).

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
    Body    = handle_db_cb(get_params(Qs)),
    {Body, Req, State}.

get_params(Qs) ->
    Struct     = mochijson2:decode(base64:decode_to_string(Qs)),
    BucketName = struct:get_value(<<"bucketname">>, Struct),
    {BucketName}.

handle_db_cb({BucketName}) ->
    case catch verify_params(BucketName) of
	{Error_Code, Error_Msg} ->
	    riak_http_util:build_body(Error_Code, Error_Msg);
	ok ->
	    handle_db_cb_do(BucketName)
    end.

verify_params(BucketName) ->
    case BucketName of
	"" ->
	    erlang:throw({"-1", "bucketname can not empty"});
	undefined ->
	    erlang:throw({"-20", "missing params"});
	_ ->
	    ok
    end,
    case riak_http_db:get_all_buckets() of
	{ok, []} ->
	    ok;
	{ok, Bucket_List} ->
	    case lists:member(list_to_binary(BucketName), Bucket_List) of
		false ->
		    ok;
		_ ->
		    erlang:throw({"-2", "this bucket exist"})
	    end
    end.
	    
handle_db_cb_do(BucketName) -> 
    %% after create bucket, initialise bucket 
    %%=======================================
    %% key   : <<"sys_initialization">>
    %% value : list_to_binary(Username)
    %%=======================================
    riak_http_db:write_db(list_to_binary(BucketName),
		     <<"sys_initialization">>, 
		     <<"sys_initialization">>),
    riak_http_util:build_body("0", "ok").

