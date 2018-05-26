%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 1 - kakuroVerify(Instance+, Solution+)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this is a modification of our solution to task 3 - kakuroVerify(Solution+) from previous task

% kakuroVerify(Instance+, Solution+)
 kakuroVerify(Instance, Solution) :-
    validate_similarity(Instance, Solution),
    kakuroVerify(Solution).

kakuroVerify([], []).

% validate_similarity(Instance+, Solution+)
% satisfies if and only if the solution is of the same structure as the instnace
% this predicate can fail if solution hold more than one value for a variable in Instance
validate_similarity([Sum = Vals | RestInstance], [Sum = Vals | RestSolution]) :-
    validate_similarity(RestInstance, RestSolution).

validate_similarity([], []).

% kakuroVerify(Solution+) - verifies the solution
kakuroVerify([H | Rest]) :-
    verify_assignment(H),
    kakuroVerify(Rest).

kakuroVerify([]).

% verify_assignment(Sum+ = Assignments+) - satisfies if and only if the anumbers assigned fit kakuro rules
verify_assignment(Sum = Assignments) :-
    is_sum(Sum, Assignments),
    all_numbers_diff(Assignments).

% is_sum(Sum+, Numbers+) - satisfies if and only if the sum of Numbers is Sum
is_sum(Sum, [H | T]) :-
    Sum1 is Sum - H,
    is_sum(Sum1, T).

% all numbers were substracted from Sum and 0 is remaining -> sum of all given numbers is Sum
is_sum(0, []).

% all_numbers_diff(Numbers+) - satisfies if and only if all numbers are distinct
all_numbers_diff([X1, X2 | Rest]) :-
    X1 \== X2,                      % X1 is different than X2
    all_numbers_diff([X1 | Rest]),  % X1 is different than the rest of the numbers
    all_numbers_diff([X2 | Rest]).  % X2 is different than the rest of the numbers

all_numbers_diff([_]).
all_numbers_diff([]).

