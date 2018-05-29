:- module('ex4', [ kakuroSolve/2 ]).% TODO fix these stuff
:- use_module('./bee/bApplications/auxs/auxRunExpr',[runExpr/5, decodeIntMatrix/2, decodeInt/2]).
:- use_module('./bee/bApplications/auxs/auxMatrix',[matrixCreate/3, matrixTranspose/2, matrix2vector/2]).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 2 - kakuroEncode(Instance+,Map-,Constraints-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% map_solution_variables(Instance+, VarsMap-, Cs-)
% Map is built in the form of [Instance, VarsMap] and this predicate receives the VarsMap part of the Map
% VarsMap maps each variable into another variables declared with BEE's new_int with the form InstanceVar = BeeVar
% when decoding the BEE var we will assign the resulted value for the Instance's Var as well
% the Var assignemnt will reflect in instance and therefor in the solution

map_solution_variables(Instance, VarsMap, Cs) :-
    accumulateVars(Instance, Vars),
    sort(Vars, VarsNoDuplicates),   % handle duplicated appearances of Vars
    declareNums(VarsNoDuplicates, VarsMap, 1, 9, Cs-[]). % taken from the NDK problem example
    


% accumulateVars(Instance+, AllVars+)
% appends all variables mentioned in the Instance (may contain duplicates)
accumulateVars([_ = Vars | Rest], AllVars) :-
    accumulateVars(Rest, AccumulatedVars),
    append(Vars, AccumulatedVars, AllVars).

accumulateVars([], []).

% taken from the NDK problem example in BEE library with slight modifications
declareNums([],[],_,_,Cs-Cs).
declareNums([Var|Vars],[Var = Num|RestMap],LB,UB,[new_int(Num,LB,UB)|CsH]-CsT):-
    declareNums(Vars,RestMap,LB,UB,CsH-CsT).

kakuroEncode(Instance, map([Instance, VarsMap]),Constraints) :-
    % map_solution_variables(Instance, VarsMap, Constraints),  % map instance and declare variables
    map_solution_variables(Instance, VarsMap, VarsCs),  % map instance and declare variables
    encodeKakuroConstraints(Instance, VarsMap, CorrectnessCs-[]),
    append(VarsCs, CorrectnessCs, Constraints).


encodeKakuroConstraints([], _, Tail-Tail).
encodeKakuroConstraints([Sum = Vars | Rest], Map, Cs-Tail) :-
    % sum_equals(Sum, BitVectors, Cs-Cs1),                  % CNF1 satisfies when Vars sum into Sum
    var_list_to_bee_nums(Vars, Nums, Map),
    Cs = [int_array_sum_eq(Nums, Sum) | Cs1],
    % all_diff(BitVectors, Cs1-Cs2),                         % CNF2 satisfies when Vars are distinct from each other
    Cs1 = [int_array_allDiff(Nums) | Cs2],
    encodeKakuroConstraints(Rest, Map, Cs2-Tail).

var_list_to_bee_nums([], [], _).
var_list_to_bee_nums([HVars | RestVars], [HNums | RestNums], VarsMap) :-
    var_to_bee_num(HVars, HNums, VarsMap),
    var_list_to_bee_nums(RestVars, RestNums, VarsMap).

var_to_bee_num(Var, Num, [H = Num| _]) :-
    Var == H.

var_to_bee_num(Var, Num, [H = _| RestMap]) :-
    Var \== H,
    var_to_bee_num(Var, Num, RestMap).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 3 - kakuroDecode(Map+,Solution-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


kakuroDecode(map([Solution, VarsMap]),Solution) :-
    decodeVars(VarsMap).

decodeVars([]).
decodeVars([Var = Num | Rest]) :-
    decodeInt(Num, Var),
    decodeVars(Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 4 - kakuroSolve(Instance+,Solution-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
kakuroSolve(Instance,Solution) :-
    runExpr(Instance,Solution,
            ex4:kakuroEncode,
            ex4:kakuroDecode,
            ex4:kakuroVerify).
