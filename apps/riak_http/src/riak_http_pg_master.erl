-module(riak_http_pg_master).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, 
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2, 
	 terminate/2,
	 code_change/3]).
-export([enter_pg/2, 
	 leave_pg/2, 
	 delete_pg/1,
	 get_members/1, 
	 get_local_members/1,
	 broadcast_in_group/2,
	 get_closest_pid/1,
	 collect_process_info/1,
	 get_best_pid/2]).
-include("logger.hrl").
-record(state,{dp_collect_num}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enter_pg(PG_Name, UserPid) ->
    gen_server:cast(?MODULE, {enter_pg, PG_Name, UserPid}).

leave_pg(PG_Name, UserPid)->
    gen_server:cast(?MODULE, {leave_pg, PG_Name, UserPid}).

delete_pg(PG_Name) ->
    gen_server:cast(?MODULE, {delete_pg, PG_Name}).

get_members(PG_Name) ->
    gen_server:call(?MODULE, {get_members, PG_Name}).

get_local_members(PG_Name) ->
    gen_server:call(?MODULE, {get_local_members, PG_Name}).

broadcast_in_group(PG_Name, Message) ->
    gen_server:cast(?MODULE, {broadcast_in_group, PG_Name, Message}).

get_closest_pid(PG_Name) ->
    gen_server:call(?MODULE, {get_closest_pid, PG_Name}).

get_best_pid(<<"mem">>, PG_Name) ->
    gen_server:call(?MODULE, {<<"mem">>, PG_Name});

get_best_pid(<<"msg_len">>, PG_Name) ->
    gen_server:call(?MODULE, {<<"msg_len">>, PG_Name}).

init([])->
    pg2:start_link(),
    Collect_Num = 
	case application:get_env(riak_http, dp_collect_num) of
	    undefined ->
		12;
	    {ok, Val} ->
		Val
	end,
    State = #state{dp_collect_num = Collect_Num},
    {ok, State}.

handle_call({get_closest_pid, PG_Name}, _, State) ->
    {reply, get_closest_pid_do(PG_Name), State};

handle_call({get_members, PG_Name},_From, State) ->
    {reply, get_members_do(PG_Name), State};

handle_call({get_local_members, PG_Name}, _From, State) ->
    {reply, get_local_members_do(PG_Name), State};

handle_call({<<"msg_len">>, PG_Name}, _, State) ->
    Reply1 = 
	case PG_Name of
	    db_group ->
		erlang:get(db_pid);
	    _ ->
		ingore
	end,
    [Reply, _] = Reply1,
    {reply, get_random_pid(Reply), State};

handle_call({<<"mem">>, PG_Name}, _, State) ->
    Reply1 = 
	case PG_Name of
	    db_group ->
		erlang:get(db_pid);
	    _ ->
		ingore
	end,
    [_, Reply] = Reply1,
    {reply, get_random_pid(Reply), State};
    
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({delete_pg, PG_Name}, State) ->
    delete_pg_do(PG_Name),
    {noreply, State};

handle_cast({leave_pg, PG_Name, UserPid}, State) ->
    leave_pg_do(PG_Name, UserPid),
    {noreply, State};

handle_cast({enter_pg, PG_Name, UserPid}, State) ->
    case Pg_List = pg2:which_groups() of
	[] ->
	    register_pg(PG_Name),
	    enter_pg_do(PG_Name, UserPid);
	_ ->
	    case lists:member(PG_Name, Pg_List) of
		false ->
		    register_pg(PG_Name),
		    enter_pg_do(PG_Name, UserPid);
		_ ->
		    enter_pg_do(PG_Name, UserPid)
	    end
    end,
    {noreply, State};

handle_cast({broadcast_in_group, PG_Name, Message}, State) ->
    broadcast_in_group_do(PG_Name, Message),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({collect_process_info}, State) ->
    {DB_Pids} = collect_process_info(State#state.dp_collect_num),
    erlang:put(db_pid, DB_Pids),
    erlang:send_after(1*100, self(), {collect_process_info}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
register_pg(PG_Name) ->
    pg2:create(PG_Name).

enter_pg_do(PG_Name, UserPid) ->
    pg2:join(PG_Name, UserPid).

leave_pg_do(PG_Name, UserPid) ->
    pg2:leave(PG_Name, UserPid).

get_members_do(PG_Name) ->
    pg2:get_members(PG_Name).

delete_pg_do(PG_Name) ->
    pg2:delete(PG_Name).

get_local_members_do(PG_Name) ->
    pg2:get_local_members(PG_Name).

broadcast_in_group_do(PG_Name, Message) ->
    F = fun() ->
		[erlang:send(Pid, Message) || 
		    Pid <- riak_http_pg_master:get_members(PG_Name)]
	end,
    spawn(F).

get_closest_pid_do(PG_Name) ->
    pg2:get_closest_pid(PG_Name).

collect_process_info(Collect_Num) ->
    DB_Pid = collect_process_info_db(Collect_Num),
    {DB_Pid}.
    
collect_process_info_db(Collect_Num) ->
    Pid_List = 
	[list_to_tuple((tuple_to_list(process_info(Pid, message_queue_len)) ++ [Pid]))
	 || Pid <- pg2:get_local_members(db_group)],
    {Pid_Result_Len, _} = lists:split(Collect_Num, lists:keysort(2, Pid_List)),
    Pid_List1 = 
	[list_to_tuple((tuple_to_list(process_info(Pid1, memory)) ++ [Pid1]))
	 || Pid1 <- pg2:get_local_members(db_group)],
    {Pid_Result_Mem, _} = lists:split(Collect_Num, lists:keysort(2, Pid_List1)),
    [{message_len, Pid_Result_Len}, {memory, Pid_Result_Mem}].
    
get_random_pid(List) ->
    {_, List_1} = List,
    {_, _, Pid} = lists:nth(random:uniform(erlang:length(List_1)), List_1),
    Pid.
