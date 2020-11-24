% Program: assignment_1.pl
% Source: Prolog
% Full Name: Yuchen Yan
% Student Number: z5146418
% Assignment Name: assignment1

% Question 1
% sumsq_neg(Numbers, Sum) - sums the squares of only the negative numbers in a list of numbers
% Example of use:
% ?- sumsq_neg([1,-3,-5,2,6,8,-2], Sum).
% Sum = 38;
% false
sumsq_neg([], 0).
sumsq_neg([Head | Tail], Sum):-
    Head >= 0,
    sumsq_neg(Tail, Sum).
sumsq_neg([Head | Tail], Sum):-
    Head =< 0,
    sumsq_neg(Tail, RestSum),
    Sum is RestSum + Head * Head.



% Question 2
% all_like_all(Who_List, What_List) - takes a list of people Who_List and a list of items What_List and succeeds if every person in Who_List likes every item in What_List
% Example:
% ?- all_like_all([jane,tim],[apple,mango]).
% true ;
% false.
like_all(_, []).
like_all(Item, [Head | Tail]):-
    likes(Item, Head),
    like_all(Item, Tail).
all_like_all([], _):-
    true.
all_like_all(_, []):-
    ture.
all_like_all([Head | Tail], What_List):-
    like_all(Head, What_List),
    all_like_all(Tail, What_List).





% Question 3
% sqrt_table(N, M, Result) - binds Result to the list of pairs consisting of a number and its square root, from N down to M, where N and M are non-negative integers, and N >= M.
% Example:
% sqrt_table(7, 4, Result).
% Result = [[7, 2.6457513110645907], [6, 2.449489742783178], [5, 2.23606797749979], [4, 2.0]] ;
% false.
% ?- sqrt_table(7, 8, Result).
% false.
sqrt_table(N, M, [Head|[]]):-
    N = M,
    S is sqrt(N),
    Head = [N, S].
sqrt_table(N, M, [Head|Tail]):-
    N > M,
    S is sqrt(N),
    N1 is N - 1,
    sqrt_table(N1, M, Tail),
    Head = [N, S].





% Question 4
% chop_up(List, NewList) - takes List and binds NewList to List with all sequences of successive increasing whole numbers replaced by a two-item list containing only the first and last number in the sequence.
% Example:
% ?- chop_up([9,10,5,6,7,3,1], Result).
% Result = [[9, 10], [5, 7], 3, 1] ;
% false.
chop_up([], []).
chop_up([Head|[]], [Head|[]]).
chop_up([Head|[Head1|Tail1]], [Head|[Head2|Tail2]]):-
    not(Head1 is Head + 1),
    chop_up([Head1|Tail1],[Head2|Tail2]).
chop_up([Head|[Head1|Tail1]], [[Head|Tail2]|Tail3]):-
    Head1 is Head + 1,
    chop_up([Head1|Tail1],[[_|Tail2]|Tail3]).
chop_up([Head|[Head1|Tail1]], [[Head|[Head2|[]]]|Tail3]):-
    Head1 is Head + 1,
    chop_up([Head1|Tail1],[Head2|Tail3]),
    Head2 \= [], not(Head2 = [_|_]).






% Question 5
% tree_eval(Value, Tree, Eval) - binds Eval to the result of evaluating the expression-tree Tree, with the variable z set equal to the specified Value.
% Example:
% ?- tree_eval(2, tree(tree(empty,z,empty),
% '+',tree(tree(empty,1,empty),
% '/',tree(empty,z,empty))), Eval).
% Eval = 2.5 ;
% false.
tree_eval(_,tree(empty, V, empty), Eval):-
    not(V = z),
    Eval = V.
tree_eval(Value,tree(empty, V, empty), Value):-
    V = z.
tree_eval(Value,tree(Left, Op, Right), Eval):-
    Op = '+',
    tree_eval(Value, Left, EvalL),
    tree_eval(Value, Right, EvalR),
    Eval is EvalL + EvalR.
tree_eval(Value,tree(Left, Op, Right), Eval):-
    Op = '-',
    tree_eval(Value, Left, EvalL),
    tree_eval(Value, Right, EvalR),
    Eval is EvalL - EvalR.
tree_eval(Value,tree(Left, Op, Right), Eval):-
    Op = '*',
    tree_eval(Value, Left, EvalL),
    tree_eval(Value, Right, EvalR),
    Eval is EvalL * EvalR.
tree_eval(Value,tree(Left, Op, Right), Eval):-
    Op = '/',
    tree_eval(Value, Left, EvalL),
    tree_eval(Value, Right, EvalR),
    Eval is EvalL / EvalR.
