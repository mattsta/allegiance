-module(acourse).

-export([create_course/2, create_course/3, create_course/4]).
-export([courses/0, course_properties/1, course_property/2]).
-export([members_of_course/1, is_course_member/2]).
-export([courses_for_member/1]).
-export([course_size/1]).

-export([create_invite_token/2, create_invite_token/3]).
-export([redeem_invite_token/2]).

-export([join/2, leave/2]).

%%%--------------------------------------------------------------------
%%% Courseing Reading
%%%--------------------------------------------------------------------
courses() ->
  allegiance:all(course).

members_of_course(CourseId) ->
  allegiance:members_of(course, CourseId).

courses_for_member(Uid) ->
  allegiance:member_has(course, Uid).

course_size(CourseId) ->
  allegiance:member_count(course, CourseId).

course_properties(CourseId) ->
  allegiance:bottle_props(course, CourseId).

course_property(CourseId, Property) ->
  allegiance:bottle_prop(course, CourseId, Property).

is_course_member(CourseId, Uid) ->
  allegiance:is_member(course, CourseId, Uid).

%%%--------------------------------------------------------------------
%%% Concrete Courseing Creation
%%%--------------------------------------------------------------------
create_course(Name, CreatedByUid) ->
  % it has to have a max size, so why not 40 million?
  create_course(Name, CreatedByUid, 40000000).

create_course(Name, CreatedByUid, MaxSz) ->
  allegiance:create_bottle(course, Name, CreatedByUid, MaxSz).

create_course(CourseId, Name, CreatedByUid, MaxSz) ->
  allegiance:create_bottle(course, CourseId, Name, CreatedByUid, MaxSz).

%%%--------------------------------------------------------------------
%%% Concrete Course Joining / Parting
%%%--------------------------------------------------------------------
join(CourseId, NewMemberId) ->
  allegiance:add_member(course, CourseId, NewMemberId).

leave(CourseId, OldMemberId) ->
  allegiance:remove_member(course, CourseId, OldMemberId).

create_invite_token(CourseId, InviterUid) ->
  allegiance:create_invite_token(course, CourseId, InviterUid).
create_invite_token(CourseId, InviterUid, ForUid) ->
  allegiance:create_invite_token(course, CourseId, InviterUid, ForUid).

redeem_invite_token(TokenId, UserId) ->
  allegiance:redeem_invite_token(TokenId, UserId).
