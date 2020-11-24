% Program: agent.pl
% Source: Prolog
% Assignment Name: assignment3

% Member 1:
% Full Name: Yuchen Yan
% Student Number: z5146418

% Member 2:
% Full Name: Yang Jiao
% Student Number: z5173922



/*
% Question1
% Binds Intentions to intents(L,[]) with L in the form [[goal(X1,Y1),[]], ... , [goal(Xn,Yn),[]]]
%
initial_intentions(intents(L,[])):-
    agent_at(X0,Y0),
    assert(goal(goal(X0,Y0))),
    monster(XM,YM),
    solve(goal(XM,YM),Path,_,_),
    findall(X,(member(goal(G1,G2),Path),not(land(G1,G2)),X = [goal(G1,G2),[]]),L),
    retract(goal(goal(X0,Y0))),
    retractall(s(_,_,1000)).
*/


s(goal(X1,Y1),goal(X2,Y2),1):-
    land_or_dropped(X2,Y2),
    distance((X1,Y1),(X2,Y2),1).

s(goal(X1,Y1),goal(X2,Y2),1000):-
    UX1 is X1 + 1,
    DX1 is X1 -1,
    UY1 is Y1 +1,
    DY1 is Y1 -1,
    between(DY1,UY1,Y2),
    between(DX1,UX1,X2),
    distance((X1,Y1),(X2,Y2),1).


% Question2
% Takes a list of percepts, each of the form stone(X,Y), and converts it into a corresponding list of goals, each of the form goal(X,Y)
%
trigger([],[]).
trigger([stone(X,Y)|Tail1], [goal(X,Y)|Tail2]):-
    trigger(Tail1, Tail2).



/*
% Question3
% 1. a set of Goals in the form of a list [goal(X1,Y1), ... , goal(Xn,Yn)]
% 2. the current Intentions of the agent, in the form intents(Int_drop,Int_pick) where Int_drop, Int_pick are lists of intentions in the form [goal(X,Y), Plan]
% Your procedure should return the updated Intentions of the agent after inserting the new goals into Int_pick.
%
%Decide whether there is a path from the agent to the goal
incorporate_goals([],Intentions,Intentions):- !.

incorporate_goals([Goals|Goals],Intentions,Intentions1):-
    agent_at(X0,Y0),
    assert(goal(goal(X0,Y0))),
    insert_intent(Goal,Intentions,IntentionsAcc),
    retract(goal(goal(X0,Y0))),
    incorporate_goals(Goals,IntentionsAcc,Intentions1),!.

insert_intent(Goal,intents(Int_Drop,Int_Pick),intents(Int_Drop,Int_Pick)):-
    not(solve(Goal,_,_,_)),!.

insert_intent(Goal,intents(Int_Drop,Int_Pick),intents(Int_Drop,Int_Pick1)):-
    solve(Goal,_,G,_).
    insert_intent_hlpr(Goal,G,Int_Pick,Int_Pick1),!.

insert_intent_hlpr(Goal,_,[],[[Goal,[]]]).

insert_intent_hlpr(Goal,_,[[Goal,Plan]|TL],[[Goal,Plan]|TL]).

insert_intent_hlpr(Goal,G,[[HLGoal,HLPlan]|TL],[[HLGoal,HLPlan]|TL1]):-
    solve(HLGoal,_,GHL,_),
    G >= GHL,
    insert_intent_hlpr(Goal,G,TL,TL1).

insert_intent_hlpr(Goal,G,[[HLGoal,HLPlan]|TL],[[Goal,[]],[HLGoal,HLPlan]|TL]):-
    solve(HLGoal,_,GHL,_),
    G < GHL,!.

*/







% Question4
%
%
%get_action(Intentions, Intentions1, Action):-














% Question5
%
%
%update_intentions(Observation, Intentions, Intentions1):-
update_intentions(at(_,_), Intentions, Intentions).

update_intentions(picked(_,_), intents(_,[]), intents(_,[])).
update_intentions(picked(X,Y), intents(L, [[goal(X,Y), []]]), intents(L, [])):-
    update_intentions(picked(X,Y), intents(L, []), intents(L, [])).
update_intentions(picked(X,Y), intents(L, [[goal(X,Y), []]|Tail1]), intents(L, Tail2)):-
    update_intentions(picked(X,Y), intents(L, Tail1), intents(L, Tail2)).

update_intentions(picked(X,Y), intents(L, [[goal(X1,Y1), []]|Tail1]), intents(L, [[goal(X1,Y1), []]|Tail2])):-
    X =\= X1,
    update_intentions(picked(X,Y), intents(L, Tail1), intents(L, Tail2)).
update_intentions(picked(X,Y), intents(L, [[goal(X1,Y1), []]|Tail1]), intents(L, [[goal(X1,Y1), []]|Tail2])):-
    Y =\= Y1,
    update_intentions(picked(X,Y), intents(L, Tail1), intents(L, Tail2)).

update_intentions(dropped(_, _), intents([], _), intents([], _)).
update_intentions(dropped(X,Y), intents([[goal(X,Y), []]], L), intents([], L)):-
    update_intentions(dropped(X,Y), intents([], L), intents([],L)).
update_intentions(dropped(X, Y), intents([[goal(X, Y), []]|Tail1], L), intents(Tail2, L)):-
    update_intentions(dropped(X, Y), intents(Tail1, L), intents(Tail2, L)).

update_intentions(dropped(X,Y), intents([[goal(X1,Y1), []]|Tail1], L), intents([[goal(X1,Y1), []]|Tail2], L)):-
    X =\= X1,
    update_intentions(dropped(X,Y), intents(Tail1, L), intents(Tail2, L)).
update_intentions(dropped(X,Y), intents([[goal(X1,Y1), []]|Tail1], L), intents([[goal(X1,Y1), []]|Tail2], L)):-
    Y =\= Y1,
    update_intentions(dropped(X,Y), intents(Tail1, L), intents(Tail2, L)).









% Uniform Cost Search, using Dijkstras Algorithm

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% solve(Start, Solution, G, N)
% Solution is a path (in reverse order) from start node to a goal state.
% G is the length of the path, N is the number of nodes expanded.

solve(Start, Solution, G, N)  :-
% insert_legs(), head_member(), build_path()
ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N).

% ucsdijkstra(Generated, Expanded, Solution, L, N)
%
% The algorithm builds a list of generated "legs" in the form
% Generated = [[Node1,Prev1,G1],[Node2,Prev2,G2],...,[Start,Start,0]]
% The path length G from the start node is stored with each leg,
% and the legs are listed in increasing order of G.
% The expanded nodes are moved to another list (G is discarded)
%  Expanded = [[Node1,Prev1],[Node2,Prev2],...,[Start,Start]]

% If the next leg to be expanded reaches a goal node,
% stop searching, build the path and return it.
ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N)  :-
goal(Node),
build_path([[Node,Pred]|Expanded], Path).


% Extend the leg at the head of the queue by generating the
% successors of its destination node.
% Insert these newly created legs into the list of generated nodes,
% keeping it sorted in increasing order of G; and continue searching.
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N) :-
extend(Node, G, Expanded, NewLegs),
M is L + 1,
insert_legs(Generated, NewLegs, Generated1),
ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N).


% Find all successor nodes to this node, and check in each case
% that the new node has not previously been expanded.
extend(Node, G, Expanded, NewLegs) :-
% write(Node),nl,   % print nodes as they are expanded
findall([NewNode, Node, G1], (s(Node, NewNode, C)
, not(head_member(NewNode, Expanded))
, G1 is G + C
), NewLegs).


% base case: insert leg into an empty list.
insert_one_leg([], Leg, [Leg]).


% If we already knew a shorter path to the same node, discard the new one.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
Leg  = [Node,_Pred, G ],
Leg1 = [Node,_Pred1,G1],
G >= G1, ! .

% Insert the new leg in its correct place in the list (ordered by G).
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
Leg  = [_Node, _Pred, G ],
Leg1 = [_Node1,_Pred1,G1],
G < G1, ! .


% Search recursively for the correct place to insert.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
insert_one_leg(Generated, Leg, Generated1).


% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% This file provides code for insert_legs(), head_member() and build_path()
% used by bfsdijkstra(), ucsdijkstra(), greedy() and astar().

% insert_legs(Generated, Legs, Generated1).
% insert new legs into list of generated legs,
% by repeatedly calling insert_one_leg()

% base case: no legs to be inserted
insert_legs(Generated, [], Generated).

% Insert the first leg using insert_one_leg(); and continue.
insert_legs(Generated, [Leg|Legs], Generated2) :-
insert_one_leg(Generated, Leg, Generated1),
insert_legs(Generated1, Legs, Generated2).

% head_member(Node, List)
% check whether Node is the head of a member of List.

% base case: node is the head of first item in list.
head_member(Node,[[Node,_]|_]).

% otherwise, keep searching for node in the tail.
head_member(Node,[_|Tail]) :-
head_member(Node,Tail).

% build_path(Expanded, [[Node,Pred]], Path).

% build_path(Legs, Path)
% Construct a path from a list of legs, by joining the ones that match.

% base case: join the last two legs to form a path of one step.
build_path([[Next,Start],[Start,Start]], [Next,Start]).

% If the first two legs match, add to the front of the path.
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
build_path([[B,A]|Expanded],[B,A|Path]), ! .

% If the above rule fails, we skip the next leg in the list.
build_path([Leg,_SkipLeg|Expanded],Path) :-
build_path([Leg|Expanded],Path).
