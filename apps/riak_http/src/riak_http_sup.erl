%%%-------------------------------------------------------------------
%%% @author redink <cnredink@gmail.com>
%%% @copyright (C) 2013, redink
%%% @doc
%%%
%%% @end
%%% Created :  6 Jan 2013 by redink <cnredink@gmail.com>
%%%-------------------------------------------------------------------
-module(riak_http_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    start_child(db_master_sup, Pid),
    erlang:send_after(10*1000, erlang:whereis(riak_http_pg_master), {collect_process_info}),
    {ok, Pid}.

%%%===================================================================
%%% Supervisor callbacks 
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    Pg_Master_MFA    = {riak_http_pg_master, start_link, []},
    Pg_Master_Worker = {riak_http_pg_master, Pg_Master_MFA, transient, 5000, worker, [riak_http_pg_master]},

    {ok, {SupFlags, [Pg_Master_Worker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_child(db_master_sup, Pid) ->
    io:format("12"),
    DB_Master_Sup_MFA = {riak_http_db_sup, start_link, [riak_http_db_sup, riak_http_db_master, []]},
    DB_Master_Sup     = {riak_http_db_sup,
			 DB_Master_Sup_MFA, 
			 transient,
			 5000,
			 supervisor,
			 [riak_http_db_sup]},
    supervisor:start_child(Pid, DB_Master_Sup).

