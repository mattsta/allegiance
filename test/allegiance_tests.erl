-module(allegiance_tests).

-include_lib("eunit/include/eunit.hrl").

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
allegance_test_() ->
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

     {"Add Member",
       fun add_member/0},
     {"Remove Member",
       fun remove_member/0},

     {"Token creation and redemption",
       fun token_doing/0}
    ]
  }.

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------
cohort_3() ->
  CreatedId = acohort:create_cohort("awesome cohort of awesomeness", user1),
  Result1 = acohort:join(user1, cohort1),
  Result2 = acohort:join(user1, cohort2),
  Result3 = acohort:join(user1, cohort3),
  All = acohort:cohorts_for(user1),
  Yes = acohort:is_cohort_member(user1, cohort3),
  No = acohort:is_cohort_member(user1, cohort64),
  ?assertEqual(user1, CreatedId),
  ?assertEqual(1, Result1),
  ?assertEqual(2, Result2),
  ?assertEqual(3, Result3),
  ?assertEqual([<<"cohort1">>, <<"cohort2">>, <<"cohort3">>], All),
  ?assertEqual(true, Yes),
  ?assertEqual(false, No).

cohort_7() ->
  Result4 = acohort:join(user1, cohort4),
  Result5 = acohort:join(user1, cohort5),
  Result6 = acohort:join(user1, cohort6),
  Result7 = acohort:join(user1, cohort7),
  All = acohort:cohorts_for(user1),
  ?assertEqual(4, Result4),
  ?assertEqual(5, Result5),
  ?assertEqual(full, Result6),
  ?assertEqual(full, Result7),
  ?assertEqual([<<"cohort1">>, <<"cohort2">>,
                <<"cohort3">>, <<"cohort4">>, <<"cohort5">>], All).

cohort_reversal() ->
  ReversedYes = acohort:cohorts_uid_belongs_to(cohort4),
  ReversedNo = acohort:cohorts_uid_belongs_to(cohort7),
  ?assertEqual([<<"user1">>], ReversedYes),
  ?assertEqual([], ReversedNo).

cohort_removal() ->
  Six = acohort:leave(user1, cohort6),
  Five = acohort:leave(user1, cohort5),
  Seven = acohort:leave(user1, cohort7),
  ?assertEqual(0, Six),
  ?assertEqual(1, Five),
  ?assertEqual(0, Seven).

create_team() ->
  CreatedId = ateam:create_team("Team Broooooooohaha", mineUid),
  Name = ateam:team_property(1, name),
  Teams = ateam:teams(),
  Members = ateam:members_of_team(1),
  TeamsOfMember = ateam:teams_for(mineUid),
  Sz = ateam:team_size(1),
  Yes = ateam:is_team_member(1, mineUid),
  No = ateam:is_team_member(1, froofroo),
  ateam:team_property(CreatedId, name, rambo2),
  NewName = ateam:team_property(CreatedId, name),
  ?assertEqual([<<"1">>], Teams),
  ?assertEqual(1, CreatedId),
  ?assertEqual(<<"Team Broooooooohaha">>, Name),
  ?assertEqual([<<"mineUid">>], Members),
  ?assertEqual([<<"1">>], TeamsOfMember),
  ?assertEqual(1, Sz),
  ?assertEqual(true, Yes),
  ?assertEqual(false, No),
  ?assertEqual(<<"rambo2">>, NewName).

add_member() ->
  ateam:join(1, bobAdded),
  Members = ateam:members_of_team(1),
  ?assertEqual([<<"bobAdded">>, <<"mineUid">>], Members).

remove_member() ->
  ateam:leave(1, bobAdded),
  Members = ateam:members_of_team(1),
  ?assertEqual([<<"mineUid">>], Members).

token_doing() ->
  TokenAnybody = ateam:create_invite_token(1, mineUid),
  TokenBob = ateam:create_invite_token(1, mineUid, bob),
  GoodFine = ateam:redeem_invite_token(TokenAnybody, anybodyJones),
  Bad = ateam:redeem_invite_token(TokenBob, notBob),
  GoodBob = ateam:redeem_invite_token(TokenBob, bob),

  Members = ateam:members_of_team(1),
  Sz = ateam:team_size(1),
  TeamsOfMemberNotBob = ateam:teams_for(notBob),
  TeamsOfMemberBob = ateam:teams_for(bob),
  TeamsOfMemberAJ = ateam:teams_for(anybodyJones),

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
