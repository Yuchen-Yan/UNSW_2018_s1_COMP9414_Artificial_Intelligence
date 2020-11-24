
% land.pl

% 9 xx   xxx
% 8  xx   xxx
% 7   xx   xx
% 6 x  xxx  x
% 5 xx  xx
% 4 xxx xxx
% 3 xxxx  xx
% 2 xxxxx  xx
% 1 xxxxxx  x
%   123456789

monster(9,9).


land(1,1).
land(2,1).
land(3,1).
land(4,1).
land(5,1).
land(6,1).
land(1,2).
land(2,2).
land(3,2).
land(4,2).
land(5,2).
land(1,3).
land(2,3).
land(3,3).
land(4,3).
land(1,4).
land(2,4).
land(3,4).
land(1,5).
land(2,5).
land(1,6).
land(9,1).
land(8,2).
land(9,2).
land(7,3).
land(8,3).
land(5,4).
land(6,4).
land(7,4).
land(5,5).
land(6,5).
land(4,6).
land(5,6).
land(6,6).
land(3,7).
land(4,7).
land(2,8).
land(3,8).
land(1,9).
land(2,9).
land(9,6).
land(8,7).
land(9,7).
land(7,8).
land(8,8).
land(9,8).
land(6,9).
land(7,9).
land(8,9).

agent_at(1,1).

/*
initial_intentions(intents(L,[])):-
    agent_at(X0,Y0),
    assert(goal(goal(X0,Y0))),
    monster(XM,YM),
    solve(goal(XM,YM),Path,_,_),
    findall(X,(member(goal(G1,G2),Path),not(land(G1,G2)),X = [goal(G1,G2),[]]),L),
    retract(goal(goal(X0,Y0))),
    retractall(a(_,_,1000)).











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
