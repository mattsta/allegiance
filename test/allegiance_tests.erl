-module(allegiance_tests).

-include_lib("eunit/include/eunit.hrl").

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
allegiance_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
     {"Add thee things to cohort",
       fun cohort_3/0},
     {"Add four more things to cohort",
       fun cohort_7/0},
     {"Check cohort reverse mapping",
       fun cohort_reversal/0},
     {"Remove existing and non-existing cohorts",
       fun cohort_removal/0},

     {"Create team",
       fun create_team/0},

     {"Token creation and redemption",
       fun token_doing/0}
    ]
  }.

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------
cohort_3() ->
  Result1 = allegiance:add_cohort_to_user(user1, cohort1),
  Result2 = allegiance:add_cohort_to_user(user1, cohort2),
  Result3 = allegiance:add_cohort_to_user(user1, cohort3),
  All = allegiance:cohorts_for_user(user1),
  ?assertEqual(1, Result1),
  ?assertEqual(2, Result2),
  ?assertEqual(3, Result3),
  ?assertEqual([<<"cohort1">>, <<"cohort2">>, <<"cohort3">>], All).

cohort_7() ->
  Result4 = allegiance:add_cohort_to_user(user1, cohort4),
  Result5 = allegiance:add_cohort_to_user(user1, cohort5),
  Result6 = allegiance:add_cohort_to_user(user1, cohort6),
  Result7 = allegiance:add_cohort_to_user(user1, cohort7),
  All = allegiance:cohorts_for_user(user1),
  ?assertEqual(4, Result4),
  ?assertEqual(5, Result5),
  ?assertEqual(full, Result6),
  ?assertEqual(full, Result7),
  ?assertEqual([<<"cohort1">>, <<"cohort2">>,
                <<"cohort3">>, <<"cohort4">>, <<"cohort5">>], All).

cohort_reversal() ->
  ReversedYes = allegiance:users_who_have_this_uid_as_a_cohort(cohort4),
  ReversedNo = allegiance:users_who_have_this_uid_as_a_cohort(cohort7),
  ?assertEqual([<<"user1">>], ReversedYes),
  ?assertEqual([], ReversedNo).

cohort_removal() ->
  Six = allegiance:remove_cohort_from_user(user1, cohort6),
  Five = allegiance:remove_cohort_from_user(user1, cohort5),
  Seven = allegiance:remove_cohort_from_user(user1, cohort7),
  ?assertEqual(0, Six),
  ?assertEqual(1, Five),
  ?assertEqual(0, Seven).

create_team() ->
  Added = allegiance:create_team("Team Broooooooohaha", mineUid),
  Teams = allegiance:teams(),
  Members = allegiance:members_of_team(1),
  TeamsOfMember = allegiance:teams_for_member(mineUid),
  Sz = allegiance:team_size(1),
  ?assertEqual([<<"1">>], Teams),
  ?assertEqual(1, Added),
  ?assertEqual([<<"mineUid">>], Members),
  ?assertEqual([<<"1">>], TeamsOfMember),
  ?assertEqual(1, Sz).

token_doing() ->
  TokenAnybody = allegiance:create_invite_token(1, mineUid),
  TokenBob = allegiance:create_invite_token(1, mineUid, bob),
  GoodFine = allegiance:redeem_invite_token(TokenAnybody, anybodyJones),
  Bad = allegiance:redeem_invite_token(TokenBob, notBob),
  GoodBob = allegiance:redeem_invite_token(TokenBob, bob),

  Members = allegiance:members_of_team(1),
  Sz = allegiance:team_size(1),
  TeamsOfMemberNotBob = allegiance:teams_for_member(notBob),
  TeamsOfMemberBob = allegiance:teams_for_member(bob),
  TeamsOfMemberAJ = allegiance:teams_for_member(anybodyJones),

  ?assertEqual(welcome, GoodFine),
  ?assertEqual(baduser, Bad),
  ?assertEqual(welcome, GoodBob),
  ?assertEqual([<<"anybodyJones">>, <<"bob">>, <<"mineUid">>], Members),
  ?assertEqual([], TeamsOfMemberNotBob),
  ?assertEqual([<<"1">>], TeamsOfMemberBob),
  ?assertEqual([<<"1">>], TeamsOfMemberAJ),
  ?assertEqual(3, Sz).

%%%----------------------------------------------------------------------
%%% Set it up, tear it down
%%%----------------------------------------------------------------------
setup() ->
  application:start(er),
  er_pool:start_link(redis_allegiance, "127.0.0.1", 6389),
  er:flushall(redis_allegiance).

teardown(_) ->
  application:stop(er).
