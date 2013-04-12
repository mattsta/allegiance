allegiance: group membership management
=======================================

Status
------
`allegiance` places things in stuff.  We call the stuff bottles.
Things go in bottles.  Examples of bottles are teams (with members
being users), one user (with members being directional friends
of the one user or items the user posesses or courses the user
belongs to), a course (with members being students taking
the course), or a course container (with members being lessons
so we can quickly check which courses a lesson belongs to).

You can:
  - create bottles
  - add things to bottles
  - remove things from bottles
  - get members of a bottle
  - get a count of members of a bottle
  - get a count of how many bottles a member belongs to
  - get a list of which same-type bottles a member belongs to
  - set arbitrary properties on a bottle
  - create invite tokens per bottle
  - only allow something to join a bottle if they have a
pre-created bottle access token
  - query a user for which bottles they belong to (as long
as they are of the same type)
  - set a maximum number of things the bottle can hold (e.g.
a course could have a maximum of 40 students, or a user may
have a maximum of 30 friends, or a team may have a maximum of
100 members at any given time)

The main source is `allegiance.erl`, but `allegiance` ships
with some wrapers around `allegiance.erl`:
  - `ateam.erl` - team-based bottles
  - `acourse.erl` - course-based bottles for students to join
  - `acohort.erl` - cohort/friends-of-user based bottles
  - `aalo.erl` - course based bottles for *lessons* to join

Each wrapper has some per-bottle-type specific funtionality
exposed from the main `allegiance.erl` module.  It's a cheap
way of providing a public interface to the widly varying 'private'
`allegiance.erl` functions.

Usage
-----
See the tests for some example usage.  It's all pretty simple.

Building
--------
        rebar get-deps
        rebar compile

Testing
-------
        rebar eunit skip_deps=true suite=allegiance

Next Steps
----------
A better reporting/querying/status/realtime notification interface
could be nice.  Maybe.
