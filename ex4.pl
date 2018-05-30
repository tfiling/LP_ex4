:- module('ex4', [ kakuroSolve/2, schedulingSolve/2]).
:- use_module('./bee/bApplications/auxs/auxRunExpr',[runExpr/5, runExprMax/5, decodeInt/2]).
:- use_module('./bee/bApplications/auxs/auxMatrix',[matrixCreate/3, matrixGetCell/4, matrixGetRow/3, matrixTranspose/2]).

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
% please note this implementation is a modification of ex3's kakuro part

kakuroEncode(Instance, map([Instance, VarsMap]),Constraints) :-
    map_solution_variables(Instance, VarsMap, VarsCs),  % map instance and declare BEE variables
    encodeKakuroConstraints(Instance, VarsMap, CorrectnessCs-[]), % declare the solution correctness constraints
    append(VarsCs, CorrectnessCs, Constraints).

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
% declareNums(Vars+, BeeVars+, LowerBound+, UpperBound+ ,Cs-Cs-) - declares BEE variables equivalent to those in the list Vars
% returns the constraints resulted from the declaration
declareNums([],[],_,_,Cs-Cs).
declareNums([Var|Vars],[Var = Num|RestMap],LB,UB,[new_int(Num,LB,UB)|CsH]-CsT):-
    declareNums(Vars,RestMap,LB,UB,CsH-CsT).

% encodeKakuroConstraints(Instance+, Map+, Cs-Tail-) - for each instance component in the form of 
% Sum = Vars appends with a difference list the constraints for the correctness of the solution
encodeKakuroConstraints([], _, Tail-Tail).
encodeKakuroConstraints([Sum = Vars | Rest], Map, Cs-Tail) :-
    var_list_to_bee_nums(Vars, Nums, Map),                  % fetch the relevant BEE variables
    Cs = [int_array_sum_eq(Nums, Sum) | Cs1],               % Cs1 satisfies when Vars sum into Sum
    Cs1 = [int_array_allDiff(Nums) | Cs2],                  % Cs2 satisfies when Vars are distinct from each other
    encodeKakuroConstraints(Rest, Map, Cs2-Tail).

% var_list_to_bee_nums(Vars+, BeeVars-, VarsMap+) - returns a list of BEE variables mapped to the variables in Vars
var_list_to_bee_nums([], [], _).
var_list_to_bee_nums([HVars | RestVars], [HNums | RestNums], VarsMap) :-
    var_to_bee_num(HVars, HNums, VarsMap),
    var_list_to_bee_nums(RestVars, RestNums, VarsMap).

% var_to_bee_num(Var+, Num-, VarsMap+) - given the map of variables returns the BEE variable mapped to the given instance's variables
% case 1 - the next variable in the map is the one that should be returned.
var_to_bee_num(Var, Num, [H = Num| _]) :-
    Var == H.

% case 2 - the next variable in the map is NOT the one that should be returned, keep looking
var_to_bee_num(Var, Num, [H = _| RestMap]) :-
    Var \== H,
    var_to_bee_num(Var, Num, RestMap).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 3 - kakuroDecode(Map+,Solution-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% kakuroDecode(Map+,Solution-) iterates over all of the variables in the instance and applied on them the values BEE solver found appropriate
% the solution is the original instance with the assigned values for its variables
kakuroDecode(map([Solution, VarsMap]),Solution) :-
    decodeVars(VarsMap).

% decodeVars(VarsMap+) - apply the decode (and assignment) for each variable
decodeVars([]).
decodeVars([Var = Num | Rest]) :-
    decodeInt(Num, Var),    % decodes the possible value for Num and assignes it to Var
    decodeVars(Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 4 - kakuroSolve(Instance+,Solution-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
% kakuroSolve(Instance+,Solution-) - connects all parts together and invokes runExpr
kakuroSolve(Instance,Solution) :-
    runExpr(Instance,Solution,
            ex4:kakuroEncode,
            ex4:kakuroDecode,
            ex4:kakuroVerify).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 5 - schedulingVerify(Instance+, Solution+)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schedulingVerify(schedule(NExams, Conflicts), Solution) :-
    length(Solution, NExams),
    validate_solution_conflicts(Solution, Conflicts).


validate_solution_conflicts(_, []).
validate_solution_conflicts(Solution, [c(I, J) | RestConflicts]) :-
    nth1(I, Solution, TI),
    nth1(J, Solution, TJ),
    TI \== TJ,
    validate_solution_conflicts(Solution, RestConflicts).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 6 - schedulingEncode(Instance+,Map+,Constraints-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schedulingEncode(schedule(NExams, Conflicts), Map, M, [new_int(M, 1, NExams), bool_array_sum_eq(Vector, M) | Constraints]) :-
    matrixCreate(NExams, NExams, Matrix),
    matrixTranspose(Matrix, Matrix),
    set_matrix_contents(Matrix, Constraints-Cs2),
    append(Matrix, Vector),
    Map = map(Matrix),
    apply_zero_diagonal_constraints(Matrix, 1, NExams, Cs2-Cs3),
    apply_conflict_constraints(Matrix, Conflicts, Cs3-Cs4),
    apply_clique_only_edges_constraints(Matrix, NExams, 1, Cs4-[]).

set_matrix_contents([], Tail-Tail).
set_matrix_contents([[] | RestRows], Cs-Tail) :-
    set_matrix_contents(RestRows, Cs-Tail).
set_matrix_contents([[H | T] | RestRows], [new_bool(H) | Cs] - Tail) :-
    set_matrix_contents([T | RestRows], Cs-Tail).

apply_zero_diagonal_constraints(_, CurrentIndex, N, Tail-Tail) :-
    CurrentIndex > N.

apply_zero_diagonal_constraints(Matrix, CurrentIndex, N, [ bool_eq(XII, -1) | RestConstraints ]-Tail) :-
    CurrentIndex =< N,
    matrixGetCell(Matrix, CurrentIndex, CurrentIndex, XII),
    I1 is CurrentIndex + 1,
    apply_zero_diagonal_constraints(Matrix, I1, N, RestConstraints-Tail).

apply_symmetry_constraints(Matrix, NExams, Constraints-Tail) :-
    findall((I, J), (between(1, NExams, I), I1 is I+1, between(I1, NExams, J)), AllIndexPairs),
    apply_symmetry_constraints(Matrix, NExams, AllIndexPairs, Constraints-Tail).


apply_symmetry_constraints(_, _, [], Tail-Tail).
apply_symmetry_constraints(Matrix, _, [(I, J) | RestIndexPairs], [ bool_eq(XIJ, XJI) | RestConstraints]-Tail) :-
    matrixGetCell(Matrix, I, J, XIJ),
    matrixGetCell(Matrix, J, I, XJI),
    apply_symmetry_constraints(Matrix, _, RestIndexPairs, RestConstraints-Tail).


apply_conflict_constraints(_, [], Tail-Tail).
apply_conflict_constraints(Matrix, [c(I, J) | RestConflicts], [ bool_or_reif(XIJ, XJI, -1) | RestConstraints]-Tail) :-
    matrixGetCell(Matrix, I, J, XIJ),
    matrixGetCell(Matrix, J, I, XJI),
    apply_conflict_constraints(Matrix, RestConflicts, RestConstraints-Tail).


apply_clique_only_edges_constraints(_, NExams, CurrentRowIndex, Tail-Tail) :-
    CurrentRowIndex > NExams.

apply_clique_only_edges_constraints(Matrix, NExams, CurrentRowIndex, Constraints-Tail) :-
    CurrentRowIndex =< NExams,
    matrixGetRow(Matrix,CurrentRowIndex, Row),
    findall((I, J), (between(1, NExams, I), I1 is I+1, between(I1, NExams, J)), Pairs),
    row_apply_clique_only_edges_constraints(Pairs, Row, Matrix, Constraints-Cs2),
    NextRowIndex is CurrentRowIndex + 1,
    apply_clique_only_edges_constraints(Matrix, NExams, NextRowIndex, Cs2-Tail).


row_apply_clique_only_edges_constraints([], _, _, Tail-Tail).
row_apply_clique_only_edges_constraints([(I, J) | RestPairs], Row, Matrix, [bool_array_or([-X, -Y, Z]) | RestRowConstraints]-Tail) :-
    nth1(I, Row, X),
    nth1(J, Row, Y),
    matrixGetCell(Matrix, I, J, Z),
    row_apply_clique_only_edges_constraints(RestPairs, Row, Matrix, RestRowConstraints-Tail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 7 - schedulingDecode(Map+,Solution-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schedulingDecode(map(Matrix),Solution) :-
    length(Matrix, NExams),
    length(Solution, NExams),
    numlist(1, NExams, AllIndexes),
    extract_distribution(Matrix, AllIndexes, ExamDistribution),
    populate_solution(ExamDistribution, 1, Solution).


extract_distribution(_, [], []).
extract_distribution(Matrix, [CurrentIndex | RestIndexes], [ [CurrentIndex | AdjacentExamsIndexes] | RestExamDistribution]) :-
    matrixGetRow(Matrix, CurrentIndex, Row),
    extract_adjacent_exams(Matrix, Row, AdjacentExamsIndexes),
    delete_all(RestIndexes, AdjacentExamsIndexes, RemainingIndexes),
    extract_distribution(Matrix, RemainingIndexes, RestExamDistribution).

extract_adjacent_exams(_, Row, []) :-
    \+member(1, Row).
extract_adjacent_exams(Matrix, Row, [ExamIndex | RestAdjacentExamsIndexes]) :-
    nth1(ExamIndex, Row, 1),
    select(1, Row, -1, NewRow),
    extract_adjacent_exams(Matrix, NewRow, RestAdjacentExamsIndexes).


delete_all(RemainingList, [], RemainingList).
delete_all(List, [H | T], RemainingList) :-
    delete(List, H, CurrentRemainingList),
    delete_all(CurrentRemainingList, T, RemainingList).

populate_solution([], _, _).
populate_solution([DayDistribution | RestDistribution], CurrentDayPopulated, Solution) :-
    populate_day(DayDistribution, CurrentDayPopulated, Solution),
    NextDay is CurrentDayPopulated + 1,
    populate_solution(RestDistribution, NextDay, Solution).

populate_day([], _, _).
populate_day([ExamIndex | RestExams], CurrentDayPopulated, Solution) :-
    nth1(ExamIndex, Solution, CurrentDayPopulated),
    populate_day(RestExams, CurrentDayPopulated, Solution).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 8 - schedulingSolve(Instance+,Solution-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


schedulingSolve(Instance,Solution) :-
    runExprMax(Instance,Solution,
        ex4:schedulingEncode,
        ex4:schedulingDecode,
        ex4:schedulingVerify).
