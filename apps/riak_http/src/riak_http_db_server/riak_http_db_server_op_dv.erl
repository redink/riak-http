%%%-------------------------------------------------------------------
%%% @author tao <>
%%% @copyright (C) 2013, tao
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2013 by tao <>
%%%-------------------------------------------------------------------
-module(riak_http_db_server_op_dv).

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
    Body    = handle_db_dv(get_params(Qs)),
    {Body, Req, State}.


get_params(Qs) ->
    Struct     = mochijson2:decode(base64:decode_to_string(Qs)),
    BucketName = struct:get_value(<<"bucketname">>, Struct),
    KeyName    = struct:get_value(<<"keyname">>, Struct),
    {BucketName, KeyName}.

handle_db_dv({BucketName, KeyName}) ->
    case catch verify_params(BucketName, KeyName) of
	{Error_Code, Error_Msg} ->
	    riak_http_util:build_body(Error_Code, Error_Msg);
	ok ->
	    handle_db_dv_do(BucketName, KeyName)
    end.

verify_params(BucketName, KeyName) ->
    case KeyName of
	"" ->
	    erlang:throw({"-8", "key can not empty"});
	undefined ->
	    erlang:throw({"-20", "missing params"});
	_ ->
	    ok
    end,
    case BucketName of
	"" ->
	    erlang:throw({"-6", "bucketname can not empty"});
	undefined ->
	    erlang:throw({"-20", "missing params"});
	_ ->
	    ok
    end,
    case riak_http_db:get_all_buckets() of
	{ok, []} ->
	    erlang:throw({"-7", "can not find the bucket"});
	{ok, Bucket_List} ->
	    case lists:member(list_to_binary(BucketName), Bucket_List) of
		false ->
		    erlang:throw({"-7", "can not find the bucket"});
		_ ->
		    ok
	    end
    end.

handle_db_dv_do(BucketName, KeyName) ->
    riak_http_db:delete_from_db(list_to_binary(BucketName),
				list_to_binary(KeyName)),
    riak_http_util:build_body("0", "ok").
