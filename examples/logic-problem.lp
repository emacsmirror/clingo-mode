%%%% Boys in the Hood
%%%% https://llaisdy.co.uk/2020/08/26/solving-a-logic-puzzle-with-clingo/

%%%% The world.
episode(1..5).
victim(friar_tuck; little_john; maid_marian; much; will_scarlet).
location(inn; dungeon; lodge; gaol; stocks).
disguise(abbot; shepherd; phantom; prince_john; washerwoman).

%% For each Episode, there is one (V,L,D) combination,
%% and the four variables together make a story.
{ story(E, V, L, D) :
  victim(V),
  location(L),
  disguise(D) } = 1 :-
    episode(E).

%% Each victim, location and disguise appears in one episode only.
E1 = E2 :- story(E1, V, _, _), story(E2, V, _, _).
E1 = E2 :- story(E1, _, L, _), story(E2, _, L, _).
E1 = E2 :- story(E1, _, _, D), story(E2, _, _, D).

%%%% Clue 1
%% "Prince John" rescued a victim from the dungeon, ...
D = prince_john :-
    story(_, _, dungeon, D).

%% ... this was in the next episode after Little John's appearance.
E2 = E1 + 1 :-
    story(E1, little_john, _, _),
    story(E2, _, dungeon, _).

%%%% Clue 2
%% Friar Tuck was rescued from the inn.
L = inn :-
    story(_, friar_tuck, L, _).

%%%% Clue 3
%% The "washerwoman" appeared in episode 2.
D = washerwoman :-
    story(2, _, _, D).

%%%% Clue 4
%% Much the miller's son appeared in a later episode than the "abbot", ...
E1 < E2 :-
    story(E1, _, _, abbot),
    story(E2, much, _, _).

%% ... but an earlier episode than the rescue from the lodge.
E1 < E2 :-
    story(E1, much, _, _),
    story(E2, _, lodge, _).

%%%% Clue 5
%% The "phantom horseman" rescued Will Scarlet, ...
V = will_scarlet :-
    story(_, V, _, phantom).

%% ... in a later episode than the rescue from the gaol.
E1 < E2 :-
    story(E1, _, gaol, _),
    story(E2, will_scarlet, _, _).

%%%% Clue 6
%% The rescue from the stocks was in episode 3, ...
L = stocks :-
    story(3, _, L, _).

%% ... and did not feature Will Scarlet.
:- story(3, will_scarlet, _, _).

%%%% Clue 7
%% The "shepherd" was the rescuer in the episode before Maid Marian's.
E2 = E1 + 1 :-
    story(E1, _, _, shepherd),
    story(E2, maid_marian, _, _).

%% Maid Marian was not the victim in episode 2.
:- story(2, maid_marian, _, _).

#show story/4.
