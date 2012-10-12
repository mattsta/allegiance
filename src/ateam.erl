-module(ateam).

-export([create_team/2, create_team/3, create_team/4]).
-export([teams/0, team_properties/1, team_property/2, team_property/3]).
-export([members_of_team/1, is_team_member/2]).
-export([teams_for/1]).
-export([team_size/1]).

-export([create_invite_token/2, create_invite_token/3]).
-export([redeem_invite_token/2]).

-export([join/2, leave/2]).

%%%--------------------------------------------------------------------
%%% Teaming Reading
%%%--------------------------------------------------------------------
teams() ->
  allegiance:all(team).

members_of_team(TeamId) ->
  allegiance:members_of(team, TeamId).

teams_for(Uid) ->
  allegiance:member_has(team, Uid).

team_size(TeamId) ->
  allegiance:member_count(team, TeamId).

team_properties(TeamId) ->
  allegiance:bottle_props(team, TeamId).

team_property(TeamId, Property) ->
  allegiance:bottle_prop(team, TeamId, Property).

team_property(Uid, Property, Value) ->
  allegiance:bottle_prop(team, Uid, Property, Value).

is_team_member(TeamId, Uid) ->
  allegiance:is_member(team, TeamId, Uid).

%%%--------------------------------------------------------------------
%%% Concrete Teaming Creation
%%%--------------------------------------------------------------------
create_team(Name, CreatedByUid) ->
  create_team(Name, CreatedByUid, 40).

create_team(Name, CreatedByUid, MaxSz) ->
  allegiance:create_bottle(team, Name, CreatedByUid, MaxSz).

create_team(TeamId, Name, CreatedByUid, MaxSz) ->
  allegiance:create_bottle(team, TeamId, Name, CreatedByUid, MaxSz).

%%%--------------------------------------------------------------------
%%% Concrete Team Joining / Parting
%%%--------------------------------------------------------------------
join(TeamId, NewMemberId) ->
  allegiance:add_member(team, TeamId, NewMemberId).

leave(TeamId, OldMemberId) ->
  allegiance:remove_member(team, TeamId, OldMemberId).

create_invite_token(TeamId, InviterUid) ->
  allegiance:create_invite_token(team, TeamId, InviterUid).
create_invite_token(TeamId, InviterUid, ForUid) ->
  allegiance:create_invite_token(team, TeamId, InviterUid, ForUid).

redeem_invite_token(TokenId, UserId) ->
  allegiance:redeem_invite_token(TokenId, UserId).
