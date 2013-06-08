%%%-------------------------------------------------------------------
%%% @author tao1 <>
%%% @copyright (C) 2013, tao1
%%% @doc
%%%
%%% @end
%%% Created :  9 Apr 2013 by tao1 <>
%%%-------------------------------------------------------------------
-module(riak_http_db_master).

-include("logger.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {connection}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    {A, B} = Args,
    gen_server:start_link(
      {local, list_to_atom(lists:concat([A, "_", B]))},
      ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    io:format("swa"),
    case riak_http_pg_master:get_members(db_group) of
	{error, {no_such_group, _}} ->
	    riak_http_pg_master:enter_pg(db_group, self());
	Pg_List ->
	    case lists:member(self(), Pg_List) of
		false ->
		    riak_http_pg_master:enter_pg(db_group, self());
		_ ->
		    ok
	    end
    end,
    Riak_Host = 
	case application:get_env(riak_http, riak_host) of
	    undefined ->
		"127.0.0.1";
	    {ok, Val} ->
		Val
	end,
    Riak_Port = 
	case application:get_env(riak_http, riak_port) of
	    undefined ->
		8087;
	    {ok, Val1} ->
		Val1
	end,
    {ok, Connection} = riakc_pb_socket:start_link(Riak_Host, Riak_Port),
    State = #state{connection = Connection},
    {ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_keys, Bucket}, _, State) ->
    Connection = State#state.connection,
    Reply = riakc_pb_socket:list_keys(Connection, Bucket),
    {reply, Reply, State};

handle_call({get_buckets}, _, State) ->
    Connection = State#state.connection,
    Reply = riakc_pb_socket:list_buckets(Connection),
    {reply, Reply, State};

handle_call({get_value, Bucket, Key}, _, State) ->
    Connection = State#state.connection,
    Reply = riakc_pb_socket:get(Connection, Bucket, Key),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% input data
%%handle_info({}, State) ->
%%    Connection = State#state.connection,
handle_info({put_value, Bucket, Key, Value}, State) ->
    Connection = State#state.connection,
    riakc_pb_socket:put(Connection, riakc_obj:new(Bucket, Key, Value)),
    {noreply, State};

handle_info({delete_value, Bucket, Key}, State) ->
    Connection = State#state.connection,
    riakc_pb_socket:delete(Connection, Bucket, Key),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
