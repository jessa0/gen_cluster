-module(gen_cluster_server).
-behaviour(gen_server).

-callback init(Arg :: any(), Group :: gen_cluster_client:group_name()) -> {ok, NewState :: any()}.

-callback dispatch_call(Request :: any(), From :: any(), Group :: gen_cluster_client:group_name(), State :: any()) ->
    {noreply, NewState :: any()} |
    {reply, Reply :: any(), NewState :: any()}.

-callback dispatch_cast(Msg :: any(), Group :: gen_cluster_client:group_name(), State :: any()) -> {noreply, NewState :: any()}.

-callback terminate(Group :: gen_cluster_client:group_name(), State :: any()) -> _Ignore.

-optional_callbacks([terminate/2]).

-include_lib("kernel/include/logger.hrl").

%% public api
-export([start_link/4, child_spec/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type opts() :: #{island => pos_integer(),
                  role => gen_cluster_client:role()}.
-record(state,
        {group :: gen_cluster_client:group_name(),
         mod :: atom(),
         mod_state :: any()}).

%%
%% public api
%%

-spec start_link(Name :: gen_cluster_client:service_name(),
                 Mod :: atom(),
                 Arg :: any(),
                 Opts :: [{atom(), any()}] | opts()) -> Res when
      Res :: {ok, Pid :: pid()} |
             {error, {already_started, Pid :: pid()}} |
             {error, Reason :: any()}.
start_link(Name, Mod, Arg, Opts) when is_list(Opts) ->
    start_link(Name, Mod, Arg, maps:from_list(Opts));
start_link(Name, Mod, Arg, Opts) ->
    Group = {Name, maps:get(island, Opts, 1), maps:get(role, Opts, primary)},
    gen_server:start_link({via, gen_cluster_client, Group}, ?MODULE, {Group, Mod, Arg}, []).

child_spec(Name, Mod, Arg, Opts) ->
    #{id => ?MODULE,
      start => {?MODULE, start_link, [Name, Mod, Arg, Opts]},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [?MODULE]}.

%%
%% gen_server callbacks
%%

init({Group, Mod, Arg}) ->
    ?LOG_INFO(?MODULE_STRING" ~p starting on node ~p", [Group, node()]),
    ModState = Mod:init(Arg, Group),
    State = #state{group = Group, mod = Mod, mod_state = ModState},
    {ok, State}.

handle_call(Request, From, State) ->
    try (State#state.mod):dispatch_call(Request, From, State#state.group, State#state.mod_state) of
        {noreply, NewModState}      -> {noreply, State#state{mod_state = NewModState}};
        {reply, Reply, NewModState} -> {reply, Reply, State#state{mod_state = NewModState}}
    catch
        throw:Err -> exit(Err)
    end.

handle_cast(Msg, State) ->
    try (State#state.mod):dispatch_cast(Msg, State#state.group) of
        {noreply, NewModState} -> {noreply, State#state{mod_state = NewModState}}
    catch
        throw:Err -> exit(Err)
    end.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ?LOG_INFO(?MODULE_STRING" ~p stopping on node ~p: ~p", [State#state.group, node(), Reason]),
    case erlang:function_exported(State#state.mod, terminate, 2) of
        true ->
            (State#state.mod):terminate(State#state.mod, State#state.mod_state);
        false ->
            ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
