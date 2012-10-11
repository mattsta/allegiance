-module(allegiance).

-export([start/0]).

-export([all/1, members_of/2, member_has/2, bottle_props/2, bottle_prop/3]).
-export([is_member/3]).
-export([member_count/2]).
-export([create_bottle/4, create_bottle/5, create_bottle/6]).
-export([add_member/3, remove_member/3]).
-export([create_invite_token/3, create_invite_token/4, redeem_invite_token/2]).

-import(proplists, [get_value/2]).

%%%--------------------------------------------------------------------
%%% starting
%%%--------------------------------------------------------------------
start() ->
  application:start(allegiance).

%%%--------------------------------------------------------------------
%%% Bottle Creation
%%%--------------------------------------------------------------------
create_bottle(Type, Name, CreatedByUid, MaxSz) ->
  Id = incr(genkey(counter, Type)),
  create_bottle(Type, Id, Name, CreatedByUid, MaxSz).

create_bottle(Type, Id, Name, CreatedByUid, MaxSz) ->
  create_bottle(Type, Id, Name, CreatedByUid, MaxSz, addSelf).

create_bottle(Type, Id, Name, CreatedByUid, MaxSz, AddSelf) ->
  Epoch = epoch(),
  hmset(Type, Id, [name, Name,
                   created, Epoch,
                   maxSz, MaxSz,
                   createdBy, CreatedByUid]),
  case AddSelf of
    addSelf -> add_member(Type, Id, CreatedByUid); % creator auto-joined to team
          _ -> nope
  end,
  zadd(Type, epoch(), Id).

epoch() ->
  {Mega, Sec, _} = now(),
  Mega * 1000000 + Sec.

%%%--------------------------------------------------------------------
%%% Reading
%%%--------------------------------------------------------------------
member_count(Type, Id) ->
  zcard(Type, members, Id).

% e.g. all teams available = all(team)
all(Type) ->
  zmembers(Type).

members_of(Type, Id) ->
  zmembers(Type, members, Id).

member_has(Type, Uid) ->
  zmembers(memberHas, Type, Uid).

bottle_props(Type, Id) ->
  hgetall(Type, Id).

bottle_prop(Type, Id, Prop) ->
  hget(Type, Id, Prop).

is_member(Type, Id, Uid) ->
  zmember(Type, members, Id, Uid).

%%%--------------------------------------------------------------------
%%% Abstract Bottle Manipulation
%%%--------------------------------------------------------------------
add_member(Type, Id, NewMemberId) ->
  MaxSz = list_to_integer(binary_to_list(hget(Type, Id, maxSz))),
  with_lock(Type, Id,
    fun(Size) when Size < MaxSz ->
      % e.g. course:members:CourseId -> {members}:
      zadd(Type, members, Id, epoch(), NewMemberId),
      % e.g. memberHas:course:UID -> {courses}:
      zadd(memberHas, Type, NewMemberId, epoch(), Id),
      Size + 1;
    (_) -> full
  end).

remove_member(Type, Id, OldMemberId) ->
  with_lock(Type, Id,
    fun(_Size) ->
      % remove from memberHas:course:MemberId -> {joined courses}
      zrem(memberHas, Type, OldMemberId, Id),
      % remove from course:members:CourseId -> {members}
      zrem(Type, members, Id, OldMemberId)
  end).

% Hate me later.
gen_token() ->
  list_to_binary(
   string:to_lower(
    binary_to_list(
     iolist_to_binary(
      re:replace(
       base64:encode(
        crypto:rand_bytes(12)), "[+=/]", "", [global]))))).

create_invite_token(Type, Id, InviterUid) ->
  create_invite_token(Type, Id, InviterUid, anybody).

create_invite_token(Type, Id, InviterUid, SpecificallyFor) ->
  TokenUUid = gen_token(),
  TokenDetails = [creator, InviterUid,
                  created, epoch(),
                  type, Type,
                  id, Id,
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
  Type = get_value(type, TokenDetails),
  Id = get_value(id, TokenDetails),
  Epoch = epoch(),
  case add_member(Type, Id, UserId) of
    full -> full;
       _ -> hmset(token, TokenId, [redeemed_by, UserId,
                                   redeemed_at, Epoch]),
            zrem(unredeemed_tokens, TokenId),
            zadd(redeemed_tokens, Epoch, TokenId),
            welcome
  end.

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

zmember(Type, Section, Id, Member) ->
  case er:zrank(redis_allegiance, genkey(Type, Section, Id), Member) of
    nil -> false;
      _ -> true
  end.

zmembers(Type) ->
  er:zrange(redis_allegiance, genkey(Type), 0, -1).
zmembers(Type, Section, Id) ->
  er:zrange(redis_allegiance, genkey(Type, Section, Id), 0, -1).

zcard(What, Type, Id) ->
  er:zcard(redis_allegiance, genkey(What, Type, Id)).

zadd(Type, Score, NewThing) ->
  er:zadd(redis_allegiance, genkey(Type), Score, NewThing).

zadd(Type, Section, Id, Score, NewThing) ->
  er:zadd(redis_allegiance, genkey(Type, Section, Id), Score, NewThing).

zrem(Type, OldThing) ->
  er:zrem(redis_allegiance, genkey(Type), OldThing).
zrem(Type, Section, Id, OldThing) ->
  er:zrem(redis_allegiance, genkey(Type, Section, Id), OldThing).

lock(Type, Id) ->
  er:setnx(redis_allegiance, key_locker(Type, Id), term_to_binary(now())),
  er:expire(redis_allegiance, key_locker(Type, Id), 20).
unlock(Type, Id) ->
  er:del(redis_allegiance, key_locker(Type, Id)).

key_locker(Type, Id) ->
  genkey(templock, Type, Id).

%%%--------------------------------------------------------------------
%%% helping
%%%--------------------------------------------------------------------
with_lock(Type, Id, WorkFun) ->
  lock(Type, Id),
  try
    WorkFun(member_count(Type, Id))
  after
    unlock(Type, Id)
  end.
