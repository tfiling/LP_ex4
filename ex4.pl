:- module('ex4', [ kakuroSolve/2, schedulingSolve/2, findMaxClique/2]).
:- use_module('./bee/bApplications/auxs/auxRunExpr',[runExpr/5, runExprMax/5, runExprMin/5, decodeInt/2, decodeIntArray/2]).
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
 
schedulingEncode(schedule(NExams, Conflicts), map(ExamDays), M, [new_int(M, CliqueSize, NExams) | Constraints]) :-
    createAsymmetricMatrix(1, NExams, Matrix),
    length(ExamDays, NExams),
    optimize_with_greatest_clique(ExamDays, Conflicts, CliqueSize),
    set_matrix_contents(Matrix, Constraints-Cs2),
    apply_M_binding_constraints(Matrix, M, 1, ExamDays, Cs2-Cs3),
    apply_conflict_constraints(ExamDays, Conflicts, Cs3-[]).


createAsymmetricMatrix(NExams1, NExams, []) :-
    NExams1 is NExams + 1.
createAsymmetricMatrix(CurrentExpectedLength, NExams, [CurrentRow | RestRows]) :-
    CurrentExpectedLength =< NExams,
    length(CurrentRow, CurrentExpectedLength),
    NextExpectedLength is CurrentExpectedLength + 1,
    createAsymmetricMatrix(NextExpectedLength, NExams, RestRows).


set_matrix_contents([], Tail-Tail).
set_matrix_contents([[] | RestRows], Cs-Tail) :-
    set_matrix_contents(RestRows, Cs-Tail).
set_matrix_contents([[H | T] | RestRows], [new_bool(H) | Cs] - Tail) :-
    set_matrix_contents([T | RestRows], Cs-Tail).

apply_conflict_constraints(_, [], Tail-Tail).
% one of the constraints forces a single true value 
% instead of comparing each of the row cells we simply require the whole arrays to be different
apply_conflict_constraints(ExamDays, [c(I, J) | RestConflicts], [ direct_neq(XI, XJ) | RestConstraints]-Tail) :-
    nth1(I, ExamDays, XI),
    nth1(J, ExamDays, XJ),
    apply_conflict_constraints(ExamDays, RestConflicts, RestConstraints-Tail).


apply_single_day_constraints([], Tail-Tail).
apply_single_day_constraints([HeadRow | RestMatrixRows], [bool_array_sum_eq(HeadRow, 1) | RestConstraints]-Tail) :-
    apply_single_day_constraints(RestMatrixRows, RestConstraints-Tail).


apply_M_binding_constraints([], _, _, [], Tail-Tail).
apply_M_binding_constraints([XI | RestExams], M, CurrentPossibleMax, [ YI | RestExamDays], [ new_int(YI, 1, CurrentPossibleMax) | [int_leq(YI, M) | [int_direct2bool_array(YI, XI, 1) | RestConstraints]]]-Tail) :-
    var(YI),
    CurrentPossibleMax1 is CurrentPossibleMax + 1,
    apply_M_binding_constraints(RestExams, M, CurrentPossibleMax1, RestExamDays, RestConstraints-Tail).


apply_M_binding_constraints([XI | RestExams], M, CurrentPossibleMax, [ YI | RestExamDays], [int_leq(YI, M) | [int_direct2bool_array(YI, XI, 1) | RestConstraints]]-Tail) :-
    \+var(YI),
    CurrentPossibleMax1 is CurrentPossibleMax + 1,
    apply_M_binding_constraints(RestExams, M, CurrentPossibleMax1, RestExamDays, RestConstraints-Tail).


optimize_with_greatest_clique(ExamDays, Conflicts, CliqueSize) :-
    length(ExamDays, N),
    matrixCreate(N, N, Matrix),
    set_conflict_edges(Matrix, Conflicts),
    zero_matrix(Matrix),
    findMaxClique(Matrix, MaxCliqueVertexList),
    length(MaxCliqueVertexList, CliqueSize),
    set_clique_exam_days(ExamDays, MaxCliqueVertexList, 1).

set_clique_exam_days(_, [], _).
set_clique_exam_days(ExamDays, [CurrentCliqueVertex | RestVertices], CurrentAvailableDay) :-
    NextAvailableDay is CurrentAvailableDay + 1,
    nth1(CurrentCliqueVertex, ExamDays, CurrentAvailableDay),
    set_clique_exam_days(ExamDays, RestVertices, NextAvailableDay).

set_conflict_edges(_, []).
set_conflict_edges(Matrix, [c(I, J) | RestConflicts]) :-
    matrixGetCell(Matrix, I, J, 1),
    matrixGetCell(Matrix, J, I, 1),
    set_conflict_edges(Matrix, RestConflicts).

zero_matrix([]).
zero_matrix([[] | RestRows]) :-
    zero_matrix(RestRows).
zero_matrix([[-1 | T] | RestRows]) :-
    zero_matrix([T | RestRows]).
zero_matrix([[H | T] | RestRows]) :-
    H == 1, % the edge was set
    zero_matrix([T | RestRows]).


%%%%%%%%%%%%%%%%%% LEGACY %%%%%%%%%%%%%%%%%%%%%%%%%%
    % length(ExamsSchedule, NExams),
    % populateExams(ExamsSchedule, 1, Constraints-Cs2),
    % apply_conflict_constraints(ExamsSchedule, Conflicts, Cs2-Cs3),
    % apply_M_constraints(ExamsSchedule, M, Cs3-[]).

% populateExams([], _, Tail-Tail).
% populateExams([Xi | RestExams], CurrentPossibleMax, [new_int(Xi, 1, CurrentPossibleMax) | RestConstraints]-Tail) :-
%     NextPossibleMax is CurrentPossibleMax + 1,
%     populateExams(RestExams, NextPossibleMax, RestConstraints-Tail).


% apply_conflict_constraints(_, [], Tail-Tail).
% apply_conflict_constraints(ExamsSchedule, [c(I, J) | RestConflicts], [ int_neq(XI, XJ) | RestConstraints]-Tail) :-
%     nth1(I, ExamsSchedule, XI),
%     nth1(J, ExamsSchedule, XJ),
%     apply_conflict_constraints(ExamsSchedule, RestConflicts, RestConstraints-Tail).

% apply_M_constraints([], _, Tail-Tail).
% apply_M_constraints([Xi | RestExams], M, [int_leq(Xi, M) | RestConstraints]-Tail) :-
%     apply_M_constraints(RestExams, M, RestConstraints-Tail).

%%%%%%%%%%%%%%%%%% LEGACY %%%%%%%%%%%%%%%%%%%%%%%%%%
% schedulingEncode(schedule(NExams, Conflicts), Map, M, [new_int(M, 1, MaxM), MaxMConstraint | Constraints]) :-
%     calc_MaxM(NExams, MaxM),
%     matrixCreate(NExams, NExams, Matrix),
%     set_MaxM_constraint(Matrix, M, MaxMConstraint),
%     writeln(MaxMConstraint),
%     matrixTranspose(Matrix, Matrix),
%     set_matrix_contents(Matrix, Constraints-Cs2),
%     Map = map(Matrix),
%     apply_zero_diagonal_constraints(Matrix, 1, NExams),
%     apply_conflict_constraints(Matrix, Conflicts, Cs2-Cs3),
%     apply_clique_only_edges_constraints(Matrix, NExams, 1, Cs3-[]).

% % MaxM = (NExams^2 - NExams) / 2
% calc_MaxM(NExams, MaxM) :-
%     NExams2 is NExams * NExams,
%     NExams2_SubNExams is NExams2 - NExams,
%     MaxM is NExams2_SubNExams / 2.

% set_MaxM_constraint(Matrix, M, bool_array_sum_eq(Vector, M)) :-
%     extract_top_right_side(Matrix, 1, PartialMatrixVector),
%     append(PartialMatrixVector, Vector).


% extract_top_right_side(Matrix, CurrentRowIndex, []) :-
%     length(Matrix, Len),
%     CurrentRowIndex > Len.

% extract_top_right_side(Matrix, CurrentRowIndex, [CurrentExtracted | RestExtracted]) :-
%     length(Matrix, Len),
%     CurrentRowIndex =< Len,
%     matrixGetRow(Matrix, CurrentRowIndex, Row),
%     CurrentRowIndex1 is CurrentRowIndex + 1,
%     length(Head, CurrentRowIndex),
%     append(Head, CurrentExtracted, Row),
%     extract_top_right_side(Matrix, CurrentRowIndex1, RestExtracted).

% set_matrix_contents([], Tail-Tail).
% set_matrix_contents([[] | RestRows], Cs-Tail) :-
%     set_matrix_contents(RestRows, Cs-Tail).
% set_matrix_contents([[H | T] | RestRows], [new_bool(H) | Cs] - Tail) :-
%     set_matrix_contents([T | RestRows], Cs-Tail).

% apply_zero_diagonal_constraints(_, CurrentIndex, N) :-
%     CurrentIndex > N.

% apply_zero_diagonal_constraints(Matrix, CurrentIndex, N) :-
%     CurrentIndex =< N,
%     matrixGetCell(Matrix, CurrentIndex, CurrentIndex, -1),
%     I1 is CurrentIndex + 1,
%     apply_zero_diagonal_constraints(Matrix, I1, N).

% apply_symmetry_constraints(Matrix, NExams, Constraints-Tail) :-
%     findall((I, J), (between(1, NExams, I), I1 is I+1, between(I1, NExams, J)), AllIndexPairs),
%     apply_symmetry_constraints(Matrix, NExams, AllIndexPairs, Constraints-Tail).


% apply_symmetry_constraints(_, _, [], Tail-Tail).
% apply_symmetry_constraints(Matrix, _, [(I, J) | RestIndexPairs], [ bool_eq(XIJ, XJI) | RestConstraints]-Tail) :-
%     matrixGetCell(Matrix, I, J, XIJ),
%     matrixGetCell(Matrix, J, I, XJI),
%     apply_symmetry_constraints(Matrix, _, RestIndexPairs, RestConstraints-Tail).


% apply_conflict_constraints(_, [], Tail-Tail).
% apply_conflict_constraints(Matrix, [c(I, J) | RestConflicts], [ bool_or_reif(XIJ, XJI, -1) | RestConstraints]-Tail) :-
%     matrixGetCell(Matrix, I, J, XIJ),
%     matrixGetCell(Matrix, J, I, XJI),
%     apply_conflict_constraints(Matrix, RestConflicts, RestConstraints-Tail).


% apply_clique_only_edges_constraints(_, NExams, CurrentRowIndex, Tail-Tail) :-
%     CurrentRowIndex > NExams.

% apply_clique_only_edges_constraints(Matrix, NExams, CurrentRowIndex, Constraints-Tail) :-
%     CurrentRowIndex =< NExams,
%     matrixGetRow(Matrix,CurrentRowIndex, Row),
%     findall((I, J), (between(1, NExams, I), I1 is I+1, between(I1, NExams, J)), Pairs),
%     row_apply_clique_only_edges_constraints(Pairs, Row, Matrix, Constraints-Cs2),
%     NextRowIndex is CurrentRowIndex + 1,
%     apply_clique_only_edges_constraints(Matrix, NExams, NextRowIndex, Cs2-Tail).


% row_apply_clique_only_edges_constraints([], _, _, Tail-Tail).
% row_apply_clique_only_edges_constraints([(I, J) | RestPairs], Row, Matrix, [bool_array_or([-X, -Y, Z]) | RestRowConstraints]-Tail) :-
%     nth1(I, Row, X),
%     nth1(J, Row, Y),
%     matrixGetCell(Matrix, I, J, Z),
%     row_apply_clique_only_edges_constraints(RestPairs, Row, Matrix, RestRowConstraints-Tail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 7 - schedulingDecode(Map+,Solution-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schedulingDecode(map(ExamDays),Solution) :-
    decodeDays(ExamDays, Solution).

decodeDays([], []).
decodeDays([H | T], [HS | TS]) :-
    decodeInt(H, HS),
    decodeDays(T, TS).

%%%%%%%%%%%%%%%%%% LEGACY %%%%%%%%%%%%%%%%%%%%%%%%%%

% schedulingDecode(map(ExamsSchedule),Solution) :-
%     decodeIntArray(ExamsSchedule, Solution).

%%%%%%%%%%%%%%%%%% LEGACY %%%%%%%%%%%%%%%%%%%%%%%%%%

% schedulingDecode(map(Matrix),Solution) :-
%     length(Matrix, NExams),
%     length(Solution, NExams),
%     numlist(1, NExams, AllIndexes),
%     extract_distribution(Matrix, AllIndexes, ExamDistribution),
%     populate_solution(ExamDistribution, 1, Solution).


% extract_distribution(_, [], []).
% extract_distribution(Matrix, [CurrentIndex | RestIndexes], [ [CurrentIndex | AdjacentExamsIndexes] | RestExamDistribution]) :-
%     matrixGetRow(Matrix, CurrentIndex, Row),
%     extract_adjacent_exams(Matrix, Row, AdjacentExamsIndexes),
%     delete_all(RestIndexes, AdjacentExamsIndexes, RemainingIndexes),
%     extract_distribution(Matrix, RemainingIndexes, RestExamDistribution).

% extract_adjacent_exams(_, Row, []) :-
%     \+member(1, Row).
% extract_adjacent_exams(Matrix, Row, [ExamIndex | RestAdjacentExamsIndexes]) :-
%     nth1(ExamIndex, Row, 1),
%     select(1, Row, -1, NewRow),
%     extract_adjacent_exams(Matrix, NewRow, RestAdjacentExamsIndexes).


% delete_all(RemainingList, [], RemainingList).
% delete_all(List, [H | T], RemainingList) :-
%     delete(List, H, CurrentRemainingList),
%     delete_all(CurrentRemainingList, T, RemainingList).

% populate_solution([], _, _).
% populate_solution([DayDistribution | RestDistribution], CurrentDayPopulated, Solution) :-
%     populate_day(DayDistribution, CurrentDayPopulated, Solution),
%     NextDay is CurrentDayPopulated + 1,
%     populate_solution(RestDistribution, NextDay, Solution).

% populate_day([], _, _).
% populate_day([ExamIndex | RestExams], CurrentDayPopulated, Solution) :-
%     nth1(ExamIndex, Solution, CurrentDayPopulated),
%     populate_day(RestExams, CurrentDayPopulated, Solution).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 8 - schedulingSolve(Instance+,Solution-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


schedulingSolve(Instance,Solution) :-
    runExprMin(Instance,Solution,
        ex4:schedulingEncode,
        ex4:schedulingDecode,
        ex4:schedulingVerify).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utility - findMaxClique(Instance+, Solution-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Solution is a list of vertex ID that belong to the max clique

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% verify

% Solution is a list of numbers which are the vertex indexes of the vertexes in the clique of size K
cliqueVerify(Matrix, Solution) :-
    length(Solution, K),
    findall((I, J), (between(1, K, I), I1 is I+1, between(I1, K, J)), AllIndexPairs),
    verify_all_solution_vertexes_in_clique(AllIndexPairs, Matrix, Solution).


verify_all_solution_vertexes_in_clique([], _, _).
verify_all_solution_vertexes_in_clique([(I, J) | RestIndexPairs], Matrix, Solution) :-
    nth1(I, List, X),
    nth1(J, List, Y),
    matrixGetCell(Matrix, X, Y, 1),
    verify_all_solution_vertexes_in_clique(RestIndexPairs, Matrix, Solution).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode
cliqueEncode(Matrix, map(VertexList), K, [new_int(K, 1, N) | Constraints]) :-
    length(Matrix, N),
    length(VertexList, N),
    NMinus1 is N - 1,
    Constraints = [bool_array_sum_eq(VertexList, K) | Cs],
    findall((I, J), (between(1, NMinus1, I), I1 is I+1, between(I1, N, J)), AllIndexPairs),% TODO the N-1 might should be used in the other findalls
    setCliqueConstraints(AllIndexPairs, VertexList, Matrix, Cs-[]).


% VertexList is a list of booleans where True in cell i means that vertex i is part of the clique
setCliqueConstraints([], _, _, Tail-Tail).
setCliqueConstraints([(I, J) | RestPairs], VertexList, Matrix, [bool_array_or([-X, -Y, Z]) | RestRowConstraints]-Tail) :-
    nth1(I, VertexList, X),
    nth1(J, VertexList, Y),
    matrixGetCell(Matrix, I, J, Z),
    setCliqueConstraints(RestPairs, VertexList, Matrix, RestRowConstraints-Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode

cliqueDecode(map(VertexList), Solution) :-
    accumulateCliqueVertexList(VertexList, 1, Solution).

accumulateCliqueVertexList([], _, []).
accumulateCliqueVertexList([1 | RestVertexes], CurrentVertexID, [CurrentVertexID | RestSolution]) :-
    NextVertexID is CurrentVertexID + 1,
    accumulateCliqueVertexList(RestVertexes, NextVertexID, RestSolution).

accumulateCliqueVertexList([-1 | RestVertexes], CurrentVertexID, RestSolution) :-
    NextVertexID is CurrentVertexID + 1,
    accumulateCliqueVertexList(RestVertexes, NextVertexID, RestSolution).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% runExprMax - find max clique

findMaxClique(Instance,Solution) :-
    runExprMax(Instance,Solution,
        ex4:cliqueEncode,
        ex4:cliqueDecode,
        ex4:cliqueVerify).
