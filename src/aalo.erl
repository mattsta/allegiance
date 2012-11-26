-module(aalo).

-export([create_alo/2, create_alo/3, create_alo/4]).
-export([alos/0, alo_properties/1, alo_property/2]).
-export([members_of_alo/1, is_alo_member/2]).
-export([alos_for/1, count_alos_for/1]).
-export([alo_size/1]).

-export([join/2, leave/2]).

%%%--------------------------------------------------------------------
%%% Aloing Reading
%%%--------------------------------------------------------------------
alos() ->
  allegiance:all(alo).

members_of_alo(AloId) ->
  allegiance:members_of(alo, AloId).

alos_for(Clo) ->
  allegiance:member_has(alo, Clo).

count_alos_for(Clo) ->
  allegiance:count_member_has(alo, Clo).

alo_size(AloId) ->
  allegiance:member_count(alo, AloId).

alo_properties(AloId) ->
  allegiance:bottle_props(alo, AloId).

alo_property(AloId, Property) ->
  allegiance:bottle_prop(alo, AloId, Property).

is_alo_member(AloId, Clo) ->
  allegiance:is_member(alo, AloId, Clo).

%%%--------------------------------------------------------------------
%%% Concrete Aloing Creation
%%%--------------------------------------------------------------------
create_alo(Name, CreatedByClo) ->
  % it has to have a max size, so why not 40 million?
  create_alo(Name, CreatedByClo, 40000000).

create_alo(Name, CreatedByClo, MaxSz) ->
  allegiance:create_bottle(alo, Name, CreatedByClo, MaxSz).

create_alo(AloId, Name, CreatedByUid, MaxSz) ->
  allegiance:create_bottle(alo, AloId, Name, CreatedByUid, MaxSz).

%%%--------------------------------------------------------------------
%%% Concrete Alo Joining / Parting
%%%--------------------------------------------------------------------
join(AloId, NewMemberId) ->
  allegiance:add_member(alo, AloId, NewMemberId).

leave(AloId, OldMemberId) ->
  allegiance:remove_member(alo, AloId, OldMemberId).
