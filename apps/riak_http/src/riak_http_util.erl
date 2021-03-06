%%%-------------------------------------------------------------------
%%% @author tao1 <>
%%% @copyright (C) 2013, tao1
%%% @doc
%%%
%%% @end
%%% Created : 11 Apr 2013 by tao1 <>
%%%-------------------------------------------------------------------
-module(riak_http_util).

%% API
-export([build_body/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
build_body(Code, Msg) ->
    erlang:list_to_binary(mochijson2:encode({struct, [{code, Code},
						      {msg, Msg}]})).
%%%===================================================================
%%% Internal functions
%%%===================================================================
