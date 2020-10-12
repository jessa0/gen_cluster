-module(gen_cluster_alarm_handler).
-behaviour(gen_event).

%% api
-export([start/0]).

%% gen_event
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%
%% api
%%

start() ->
    case lists:member(?MODULE, gen_event:which_handlers(alarm_handler)) of
        false ->
            gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {?MODULE, undefined});
        true ->
            ok
    end.

%%
%% gen_event
%%

init(_Args) ->
    net_kernel:monitor_nodes(true, [nodedown_reason]),
    erlang:system_monitor(self(), [{long_gc, 50},
                                   {long_schedule, 50},
                                   busy_port, busy_dist_port]),
    {ok, nostate}.

handle_call(_Request, State) ->
    {ok, {error, unhandled}, State}.

%% memsup

handle_event({set_alarm, {system_memory_high_watermark, []}}, State) ->
    {Total, Alloc, _Worst} = memsup:get_memory_data(),
    TotalMB = Total div (1024*1024),
    AllocMB = Alloc div (1024*1024),
    AllocPct = round((Alloc / Total) * 100),
    ?LOG_WARNING("system high memory usage: ~BMB/~BMB (~B%)", [TotalMB, AllocMB, AllocPct]),
    {ok, State};
handle_event({set_alarm, {process_memory_high_watermark, Pid}}, State) ->
    {Total, _Alloc, Worst} = memsup:get_memory_data(),
    TotalMB = Total div (1024*1024),
    WorstMB = Worst div (1024*1024),
    WorstPct = round((Worst / Total) * 100),
    ?LOG_WARNING("process high memory usage: ~p: ~BMB/~BMB (~B%)", [Pid, TotalMB, WorstMB, WorstPct]),
    {ok, State};

%%
%% disksup
%%

handle_event({set_alarm, {{disk_almost_full, MountedOn}, []}}, State) ->
    case lists:keyfind(MountedOn, 1, disksup:get_disk_data()) of
        {MountedOn, TotalKB, UsedPct} ->
            TotalMB = TotalKB div 1024,
            UsedMB = TotalKB * UsedPct div (100 * 1024),
            ?LOG_WARNING("disk almost full: ~s: ~BMB/~BMB (~B%)", [MountedOn, TotalMB, UsedMB, UsedPct]);
        false ->
            ?LOG_WARNING("disk almost full: ~s", [MountedOn])
    end,
    {ok, State};

%%
%% mnesia
%%

handle_event({mnesia_system_event, {mnesia_up, Node}}, State) ->
    ?LOG_INFO("mnesia_up: ~p", [Node]),
    {ok, State};
handle_event({mnesia_system_event, {mnesia_down, Node}}, State) ->
    ?LOG_INFO("mnesia_down: ~p", [Node]),
    {ok, State};
handle_event({mnesia_system_event, {mnesia_checkpoint_activated, _Checkpoint}}, State) ->
    {ok, State};
handle_event({mnesia_system_event, {mnesia_checkpoint_deactivated, _Checkpoint}}, State) ->
    {ok, State};
handle_event({mnesia_system_event, {mnesia_overload, Details}}, State) ->
    ?LOG_INFO("mnesia_overload: ~p", [Details]),
    {ok, State};
handle_event({mnesia_system_event, {inconsistent_database, Reason, Node}}, State) ->
    ?LOG_INFO("mnesia inconsistent_database on ~p: ~p", [Node, Reason]),
    {ok, State};
handle_event({mnesia_system_event, {mnesia_fatal, Format, Args, _BinCore}}, State) ->
    ?LOG_ERROR("mnesia_fatal: "++Format, Args),
    {ok, State};
handle_event({mnesia_system_event, {mnesia_error, Format, Args}}, State) ->
    ?LOG_ERROR("mnesia_error: "++Format, Args),
    {ok, State};
handle_event({mnesia_system_event, {mnesia_info, _Format, _Args}}, State) ->
    {ok, State};
handle_event({mnesia_system_event, _MnesiaEvent}=Event, State) ->
    ?LOG_WARNING("mnesia_system_event: ~p", [Event]),
    {ok, State};
handle_event({mnesia_table_event, _}, State) ->
    {ok, State};
handle_event(Event, State) ->
    ?LOG_WARNING("~p", [Event]),
    {ok, State}.

%%
%% net_kernel:monitor_nodes
%%

handle_info({nodeup, _Node, _InfoList}, State) ->
    {ok, State};
handle_info({nodedown, Node, InfoList}, State) ->
    case lists:keyfind(nodedown_reason, 1, InfoList) of
        {nodedown_reason, Reason} ->
            ?LOG_WARNING("nodedown ~p: ~p", [Node, Reason]);
        false ->
            ok
    end,
    {ok, State};

%%
%% erlang:system_monitor
%%

handle_info({monitor, GcPid, long_gc, Info}, State) ->
    ?LOG_WARNING("long_gc on pid ~p: ~p", [GcPid, Info]),
    {ok, State};
handle_info({monitor, PidOrPort, long_schedule, Info}, State) ->
    ?LOG_WARNING("long_schedule on pid ~p: ~p", [PidOrPort, Info]),
    {ok, State};
handle_info({monitor, SusPid, busy_port, Port}, State) ->
    ?LOG_WARNING("busy_port ~p on pid ~p", [Port, SusPid]),
    {ok, State};
handle_info({monitor, _SusPid, busy_dist_port, Port}, State) ->
    ?LOG_WARNING("busy_dist_port: ~p", [Port]),
    {ok, State};

handle_info(_Msg, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
