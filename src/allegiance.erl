-module(allegiance).

-export([start/0]).
-export([cohorts_for_user/1, users_who_have_this_uid_as_a_cohort/1]).
-export([add_cohort_to_user/2, add_cohort_to_user/3]).
-export([remove_cohort_from_user/2]).

-export([teams/0, team_properties/1, team_property/2]).
-export([members_of_team/1]).
-export([teams_for_member/1]).
-export([team_size/1]).

-export([create_team/2]).
-export([add_member/2]).
-export([create_invite_token/2, create_invite_token/3, redeem_invite_token/2]).

-import(proplists, [get_value/2]).

%%%--------------------------------------------------------------------
%%% starting
%%%--------------------------------------------------------------------
start() ->
  application:start(allegiance).

%%%--------------------------------------------------------------------
%%% Cohorting Reading
%%%--------------------------------------------------------------------
cohorts_for_user(Uid) ->
  zmembers(cohorts, Uid).

users_who_have_this_uid_as_a_cohort(Uid) ->
  zmembers(cohorts_belong, Uid).

%%%--------------------------------------------------------------------
%%% Cohorting Updating
%%%--------------------------------------------------------------------
add_cohort_to_user(UserUid, NewCohortAdditionUid) ->
  add_cohort_to_user(UserUid, NewCohortAdditionUid, 5).

add_cohort_to_user(UserUid, NewCohortAdditionUid, MaxSz) ->
  with_cohort_lock(UserUid,
    fun(Size) when Size < MaxSz ->
      Epoch = epoch(),
      zadd(cohorts, UserUid, Epoch, NewCohortAdditionUid),
      zadd(cohorts_belong, NewCohortAdditionUid, Epoch, UserUid),
      Size + 1;
    (_) -> full
  end).

remove_cohort_from_user(UserUid, RemoveCohortUid) ->
  zrem(cohorts, UserUid, RemoveCohortUid).

with_cohort_lock(Uid, WorkFun) ->
  with_lock(cohort, Uid, fun() -> WorkFun(cohort_size(Uid)) end).

with_lock(Namespace, Id, WorkFun) ->
  lock(Namespace, Id),
  try
    WorkFun()
  after
    unlock(Namespace, Id)
  end.

cohort_size(Uid) ->
  case zcard(cohorts, Uid) of
    nil -> 0;
      S -> S
  end.

%%%--------------------------------------------------------------------
%%% Teaming Reading
%%%--------------------------------------------------------------------
teams() ->
  zmembers(teams).

members_of_team(TeamId) ->
  zmembers(members, TeamId).

teams_for_member(Uid) ->
  zmembers(teams, Uid).

team_size(TeamId) ->
  zcard(members, TeamId).

team_properties(TeamId) ->
  hgetall(team, TeamId).

team_property(TeamId, Property) ->
  hget(team, TeamId, Property).

%%%--------------------------------------------------------------------
%%% Teaming Updating
%%%--------------------------------------------------------------------
create_team(Name, CreatedByUid) ->
  create_team(Name, CreatedByUid, 40).

create_team(Name, CreatedByUid, MaxSz) ->
  TeamId = incr(genkey(team_counter)),
  Epoch = epoch(),
  hmset(team, TeamId, [name, Name,
                       created, Epoch,
                       maxSz, MaxSz,
                       createdBy, CreatedByUid]),
  add_member(TeamId, CreatedByUid), % creator is auto-joined to team
  zadd(teams, epoch(), TeamId).

epoch() ->
  {Mega, Sec, _} = now(),
  Mega * 1000000 + Sec.

add_member(TeamId, NewMemberUid) ->
  MaxSz = list_to_integer(binary_to_list(hget(team, TeamId, maxSz))),
  with_team_lock(TeamId,
    fun(Size) when Size < MaxSz ->
      zadd(members, TeamId, epoch(), NewMemberUid),
      zadd(teams, NewMemberUid, epoch(), TeamId);
    (_) -> full
  end).

gen_token() ->
  list_to_binary(
   string:to_lower(
    binary_to_list(
     iolist_to_binary(
      re:replace(
       base64:encode(
        crypto:rand_bytes(12)), "[+=/]", "", [global]))))).

create_invite_token(TeamId, InviterUid) ->
  create_invite_token(TeamId, InviterUid, anybody).

create_invite_token(TeamId, InviterUid, SpecificallyFor) ->
  TokenUUid = gen_token(),
  TokenDetails = [creator, InviterUid,
                  created, epoch(),
                  team, TeamId,
                  forUid, SpecificallyFor],
  hmset(token, TokenUUid, TokenDetails),
  TokenUUid.

redeem_invite_token(TokenId, UserId) when is_atom(UserId) ->
  redeem_invite_token(TokenId, list_to_binary(atom_to_list(UserId)));
redeem_invite_token(TokenId, UserId) when is_binary(UserId) ->
  Details = hgetall(token, TokenId),
  For = get_value(forUid, Details),
  case For of
    <<"anybody">> -> redeem(TokenId, Details, UserId);
           UserId -> redeem(TokenId, Details, UserId);
                 _ -> baduser
  end.

redeem(TokenId, TokenDetails, UserId) ->
  TeamId = get_value(team, TokenDetails),
  Epoch = epoch(),
  case add_member(TeamId, UserId) of
    full -> full;
       _ -> hmset(token, TokenId, [redeemed_by, UserId,
                                   redeemed_at, Epoch]),
            zrem(unredeemed_tokens, TokenId),
            zadd(redeemed_tokens, Epoch, TokenId),
            welcome
  end.

with_team_lock(TeamId, WorkFun) ->
  with_lock(team, TeamId, fun() -> WorkFun(team_size(TeamId)) end).

% create a max of 5 sub8 here exclusive of all other sub8s

%%%--------------------------------------------------------------------
%%% Keys
%%%--------------------------------------------------------------------
% Key model is:
%  cohorts -- set of uids max size 5 assigned per UID.
%  cohorts_paid -- set of uids with max size 5 and the relation is recip.
%  team -- hash of team info (name, creation time, creation uid, ...)
%  members -- sorted set of team members.  default, sorted by addition time.
%  tokens:TeamId -- set of outstanding unredeemed token
%  token:TokenId -- hash of Token information (creation date, created by, created for, ...)

genkey(Type) ->
  eru:er_key(alleg, Type).

genkey(Type, Id) ->
  eru:er_key(alleg, Type, Id).

genkey(Typer, Type, Id) ->
  eru:er_key(alleg, Typer, Type, Id).

incr(Type) ->
  er:incr(redis_allegiance, genkey(Type)).

hmset(Type, Id, Combined) ->
  er:hmset(redis_allegiance, genkey(Type, Id), Combined).

hget(Type, Id, What) ->
  er:hget(redis_allegiance, genkey(Type, Id), What).

hgetall(Type, Id) ->
  er:hgetall_p(redis_allegiance, genkey(Type, Id)).

zmembers(Type) ->
  er:zrange(redis_allegiance, genkey(Type), 0, -1).
zmembers(Type, Id) ->
  er:zrange(redis_allegiance, genkey(Type, Id), 0, -1).

zcard(Type, Id) ->
  er:zcard(redis_allegiance, genkey(Type, Id)).

zadd(Type, Score, NewThing) ->
  er:zadd(redis_allegiance, genkey(Type), Score, NewThing).

zadd(Type, Id, Score, NewThing) ->
  er:zadd(redis_allegiance, genkey(Type, Id), Score, NewThing).

zrem(Type, OldThing) ->
  er:zrem(redis_allegiance, genkey(Type), OldThing).
zrem(Type, Id, OldThing) ->
  er:zrem(redis_allegiance, genkey(Type, Id), OldThing).

lock(Type, Id) ->
  er:setnx(redis_allegiance, key_locker(Type, Id), term_to_binary(now())),
  er:expire(redis_allegiance, key_locker(Type, Id), 20).
unlock(Type, Id) ->
  er:del(redis_allegiance, key_locker(Type, Id)).

key_locker(Type, Id) ->
  genkey(templock, Type, Id).
