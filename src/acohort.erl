-module(acohort).

-export([create_cohort/2, create_cohort/3]).
-export([cohorts/0, cohort_properties/1, cohort_property/2, cohort_property/3]).
-export([cohorts_for/1, is_cohort_member/2]).
-export([cohorts_uid_belongs_to/1]).
-export([cohort_size/1]).

-export([join/2, leave/2]).

%%%--------------------------------------------------------------------
%%% Cohorting Reading
%%%--------------------------------------------------------------------
cohorts() ->
  allegiance:all(cohort).

% cohorts for this Uid
cohorts_for(Uid) ->
  allegiance:members_of(cohort, Uid).

% other cohorts this Uid belongs to
cohorts_uid_belongs_to(Uid) ->
  allegiance:member_has(cohort, Uid).

cohort_size(Uid) ->
  allegiance:member_count(cohort, Uid).

cohort_properties(Uid) ->
  allegiance:bottle_props(cohort, Uid).

cohort_property(Uid, Property) ->
  allegiance:bottle_prop(cohort, Uid, Property).

cohort_property(Uid, Property, Value) ->
  allegiance:bottle_prop(cohort, Uid, Property, Value).

is_cohort_member(Uid, MemberQueryUid) ->
  allegiance:is_member(cohort, Uid, MemberQueryUid).

%%%--------------------------------------------------------------------
%%% Concrete Cohorting Creation
%%%--------------------------------------------------------------------
create_cohort(Name, CreatedByUid) ->
  create_cohort(Name, CreatedByUid, 5).

create_cohort(Name, CreatedByUid, MaxSz) ->
  allegiance:create_bottle(cohort, CreatedByUid, Name, CreatedByUid, MaxSz, no).

%%%--------------------------------------------------------------------
%%% Concrete Cohort Joining / Parting
%%%--------------------------------------------------------------------
join(Uid, NewMemberId) ->
  allegiance:add_member(cohort, Uid, NewMemberId).

leave(Uid, OldMemberId) ->
  allegiance:remove_member(cohort, Uid, OldMemberId).
