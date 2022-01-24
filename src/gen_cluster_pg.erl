-module(gen_cluster_pg).

%% API
-export([child_spec/0, join/2, leave/2, get_local_members/1, get_members/1, which_groups/0]).

-define(SCOPE, gen_cluster).

%%
%% API
%%

child_spec() ->
    #{id => ?MODULE,
      start => {pg, start_link, [?SCOPE]}}.

-spec join(Group :: pg:group(), PidOrPids :: pid() | [pid()]) -> ok.
join(Group, PidOrPids) ->
    pg:join(?SCOPE, Group, PidOrPids).

-spec leave(Group :: pg:group(), PidOrPids :: pid() | [pid()]) -> ok.
leave(Group, PidOrPids) ->
    pg:leave(?SCOPE, Group, PidOrPids).

-spec get_local_members(Group :: pg:group()) -> [pid()].
get_local_members(Group) ->
    pg:get_local_members(?SCOPE, Group).

-spec get_members(Group :: pg:group()) -> [pid()].
get_members(Group) ->
    pg:get_members(?SCOPE, Group).

-spec which_groups() -> [pg:group()].
which_groups() ->
    pg:which_groups(?SCOPE).
