-module(gen_cluster_client).

-include_lib("kernel/include/logger.hrl").

%% OTP gen_* process registry callbacks
-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).

-type role() :: primary | fallback.
-type service_name() :: atom().
-type island_name() :: {Service :: service_name(), IslandNo :: pos_integer()}.
-type group_name() :: {Service :: service_name(), IslandNo :: pos_integer(), Role :: role()}.
-export_type([role/0, service_name/0, island_name/0, group_name/0]).

%%
%% OTP gen_* process registry callbacks
%%

-spec register_name(Group :: group_name(), Pid :: pid()) -> yes | no.
register_name({Service, IslandNo, Role}=Group, Pid) ->
    try register(Service, Pid) of
        true ->
            try
                ok = gen_cluster_pg:join(Group, Pid),
                ?LOG_INFO("node ~s pid ~p joined service ~s island ~B as ~s", [node(), Pid, Service, IslandNo, Role]),
                yes
            catch
                _:PgErr ->
                    unregister(Service),
                    exit(PgErr)
            end
    catch
        error:_ ->
            no
    end.

-spec unregister_name(Group :: group_name()) -> ok.
unregister_name({Service, IslandNo, Role}=Group) ->
    %% try our best to leave, but pg will auto-remove this process when it exits anyway
    Pid = whereis(Service),
    gen_cluster_pg:leave(Group, Pid),
    catch unregister(Service),
    ?LOG_INFO("node ~s pid ~p left service ~s island ~B as ~s", [node(), Pid, Service, IslandNo, Role]),
    ok.

-spec whereis_name(Island :: island_name()) -> pid() | undefined;
                  (Group :: group_name()) -> pid() | undefined.
whereis_name({Service, IslandNo}) ->
    case gen_cluster_pg:get_members({Service, IslandNo, primary}) of
        [_|_]=Pids ->
            get_closest_pid(Pids);
        _ ->
            case gen_cluster_pg:get_members({Service, IslandNo, fallback}) of
                [_|_]=Pids ->
                    get_closest_pid(Pids);
                _ ->
                    undefined
            end
    end;
whereis_name({_, _, _}=Group) ->
    case gen_cluster_pg:get_members(Group) of
        [_|_]=Pids ->
            get_closest_pid(Pids);
        _ ->
            undefined
    end.

-spec send(Group :: group_name(), Msg :: any()) -> Msg :: any().
send(Group, Msg) ->
    case whereis_name(Group) of
        undefined -> Msg;
        Pid -> Pid ! Msg
    end.

%%
%% private
%%

get_closest_pid([Pid | _]) when node(Pid) =:= node() ->
    Pid;
get_closest_pid([Pid | []]) ->
    Pid;
get_closest_pid([_ | Rest]) ->
    get_closest_pid(Rest).
