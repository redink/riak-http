%%%-------------------------------------------------------------------
%%% @author redink <>
%%% @copyright (C) 2013, redink
%%% @doc
%%%
%%% @end
%%% Created :  7 Jan 2013 by redink <>
%%%-------------------------------------------------------------------
-module(riak_http_db).
-include("logger.hrl").
-compile(export_all).

write_db(Bucket, Key, Value) ->
    Pid = riak_http_pg_master:get_best_pid(<<"msg_len">>, db_group),
    erlang:send(Pid, {put_value, Bucket, Key, Value}).

get_from_db(Bucket, Key) ->
    Pid = riak_http_pg_master:get_best_pid(<<"mem">>, db_group),
    case gen_server:call(Pid, {get_value, Bucket, Key}, infinity) of
	{ok, O1} ->
	    {ok, riakc_obj:get_value(O1)};
	{error, notfound} ->
	    {error, notfound};
	_Any ->
	    ?LOG(["op db error:", _Any])
    end.

delete_from_db(Bucket, Key) ->
    Pid = riak_http_pg_master:get_best_pid(<<"msg_len">>, db_group),
    erlang:send(Pid, {delete_value, Bucket, Key}).

get_all_buckets() ->
    gen_server:call(riak_http_pg_master:get_best_pid(<<"mem">>, db_group),
		    {get_buckets}, infinity).

get_keys(Bucket) ->
    gen_server:call(riak_http_pg_master:get_best_pid(<<"mem">>, db_group),
		    {get_keys, Bucket}, infinity).
