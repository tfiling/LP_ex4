:- module('ex4', [ kakuroSolve/2, schedulingSolve/2, findMaxClique/2]).
:- use_module('./bee/bApplications/auxs/auxRunExpr',[runExpr/5, runExprMax/5, runExprMin/5, decodeInt/2, decodeIntArray/2]).
:- use_module('./bee/bApplications/auxs/auxMatrix',[matrixCreate/3, matrixGetCell/4]).

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
 
% M is the variable used for comparing solutions. 
% M's minimal possible value is the size of the largest exam group where every 2 members conflict with each other(see optimize_with_greatest_clique notes for more details)
schedulingEncode(schedule(NExams, Conflicts), map(ExamsSchedule), M, [new_int(M, CliqueSize, NExams) | Constraints]) :-
    length(ExamsSchedule, NExams),
    optimize_with_greatest_clique(ExamsSchedule, Conflicts, CliqueSize),
    populateExams(ExamsSchedule, NExams, Constraints-Cs2),
    apply_conflict_constraints(ExamsSchedule, Conflicts, Cs2-Cs3),
    apply_M_constraints(ExamsSchedule, M, Cs3-[]).

% optimize_with_greatest_clique(ExamsSchedule+, Conflicts+, CliqueSize-)
% finds the greatest clique of exams that conflict with each other. in this group of exams every pair of exams has a conflict.
% the predicate return the size of the clique - CliqueSize and sets a different day for each exam in the clique within ExamsSchedule variable
% the assigned days are between 1 and CliqueSize
optimize_with_greatest_clique(ExamsSchedule, Conflicts, CliqueSize) :-
    length(ExamsSchedule, N),
    matrixCreate(N, N, Matrix),
    set_conflict_edges(Matrix, Conflicts),% represents the conflicts as a matrix where every vertex is an exam and every conflict is an edge
    zero_matrix(Matrix),% set -1 where there are no edges
    findMaxClique(Matrix, MaxCliqueVertexList),% invokes a BEE based predicate that finds the greatest clique in the graph
    length(MaxCliqueVertexList, CliqueSize),
    set_clique_exam_days(ExamsSchedule, MaxCliqueVertexList, 1).% assign different exam date for the members of the greatest clique

% set_clique_exam_days(ExamDays+, CliqueVertexList+, CurrentAvailableDay+)
% assign different exam date for the members of the greatest clique
set_clique_exam_days(_, [], _).
set_clique_exam_days(ExamDays, [CurrentCliqueVertex | RestVertices], CurrentAvailableDay) :-
    NextAvailableDay is CurrentAvailableDay + 1,
    nth1(CurrentCliqueVertex, ExamDays, CurrentAvailableDay),
    set_clique_exam_days(ExamDays, RestVertices, NextAvailableDay).

% set_conflict_edges(Matrix+, Conflicts+)
% set 1, representing an edge in the adjacency matrix, representing the conflicts graph
% for each conflict c(i, j) Matrix[i, j] will be 1
set_conflict_edges(_, []).
set_conflict_edges(Matrix, [c(I, J) | RestConflicts]) :-
    matrixGetCell(Matrix, I, J, 1),
    matrixGetCell(Matrix, J, I, 1),
    set_conflict_edges(Matrix, RestConflicts).

% zero_matrix(Matrix) - sets -1 in the adjacency matrix where an edge was not set
zero_matrix([]).
zero_matrix([[] | RestRows]) :-
    zero_matrix(RestRows).
zero_matrix([[-1 | T] | RestRows]) :-
    zero_matrix([T | RestRows]).
zero_matrix([[H | T] | RestRows]) :-
    H == 1, % the edge was set
    zero_matrix([T | RestRows]).


% populateExams(ExamsSchedule+, NExams+, Constraints-Tail-)
% for each exam i which its day is still unknown creates a BEE int which will be resolved by the BEE's solver to be the exam's i day
populateExams([], _, Tail-Tail).
populateExams([Xi | RestExams], NExams, [new_int(Xi, 1, NExams) | RestConstraints]-Tail) :-
    var(Xi),
    populateExams(RestExams, NExams, RestConstraints-Tail).

populateExams([Xi | RestExams], NExams, Constraints-Tail) :-
    \+var(Xi),
    populateExams(RestExams, NExams, Constraints-Tail).

% apply_conflict_constraints(ExamsSchedule+, Conflicts+, Constraints-).
% adds constraints that will force the conflicts
apply_conflict_constraints(_, [], Tail-Tail).
apply_conflict_constraints(ExamsSchedule, [c(I, J) | RestConflicts], [ int_neq(XI, XJ) | RestConstraints]-Tail) :-
    nth1(I, ExamsSchedule, XI),
    nth1(J, ExamsSchedule, XJ),
    apply_conflict_constraints(ExamsSchedule, RestConflicts, RestConstraints-Tail).

% apply_M_constraints(ExamsSchedule+, M, Constraints-)
% this predicate adds constraints that bind the resulted exam days with M - representing how many days were used for distributing the exams
% M is the variable that is used for comparing solutions, the smaller M the better the solution
apply_M_constraints([], _, Tail-Tail).
apply_M_constraints([Xi | RestExams], M, [int_leq(Xi, M) | RestConstraints]-Tail) :-
    apply_M_constraints(RestExams, M, RestConstraints-Tail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 7 - schedulingDecode(Map+,Solution-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the map contains the list of days where the ith element is the day the ith exam should take place
% all we need is to decode that list in to the solution
schedulingDecode(map(ExamsSchedule),Solution) :-
    decodeIntArray(ExamsSchedule, Solution).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 8 - schedulingSolve(Instance+,Solution-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for comfort will print the amount of days the solution need to distribute the exams while holding the conflicts
schedulingSolve(Instance,Solution) :-
    runExprMin(Instance,Solution,
        ex4:schedulingEncode,
        ex4:schedulingDecode,
        ex4:schedulingVerify),
        max_list(Solution, Max),
        write('Distributed exams in '),
        write(Max),
        writeln(' days').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utility - findMaxClique(Instance+, Solution-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this utility, BEE based, predicate gets an adjacency matrix representing a graph and find the greatest clique in that graph
% Solution is a list of vertex ID that belong to the max clique

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% verify

% Solution is a list of numbers which are the vertex indexes of the vertexes in the clique of size K
% the instance is an adjacency matrix representing the graph
cliqueVerify(Matrix, Solution) :-
    length(Solution, N),
    NMinus1 is N - 1,
    findall((I, J), (between(1, NMinus1, I), I1 is I+1, between(I1, N, J)), AllIndexPairs),% create all possible pairs from the clique vertex list
    verify_all_solution_vertexes_in_clique(AllIndexPairs, Matrix).% make sure there is an edge between any pair of vertices in the clique

% verify_all_solution_vertexes_in_clique(IndexPairs+, Matrix+)
verify_all_solution_vertexes_in_clique([], _).
verify_all_solution_vertexes_in_clique([(I, J) | RestIndexPairs], Matrix) :-
    nth1(I, List, X),
    nth1(J, List, Y),
    matrixGetCell(Matrix, X, Y, 1),% there must be an edge, represented by 1, between the current pair of vertices from the clique
    verify_all_solution_vertexes_in_clique(RestIndexPairs, Matrix).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode

% the general idea of the encoding is that the map holds an array of boolean variables. 
% true value in the ith cell indicates that the ith vertex is part of the clique
cliqueEncode(Matrix, map(VertexList), K, [new_int(K, 1, N) | Constraints]) :-
    length(Matrix, N),
    length(VertexList, N),
    NMinus1 is N - 1,
    Constraints = [bool_array_sum_eq(VertexList, K) | Cs],
    findall((I, J), (between(1, NMinus1, I), I1 is I+1, between(I1, N, J)), AllIndexPairs),% generate all possible index pairs
    setCliqueConstraints(AllIndexPairs, VertexList, Matrix, Cs-[]).


% VertexList is a list of booleans where True in cell i means that vertex i is part of the clique
% the constraint is that if X and Y are in the clique then they must have an edge between them:
% X AND Y => Matrix[X, Y] = 1 <==> !X OR !Y OR Matrix[X, Y]
setCliqueConstraints([], _, _, Tail-Tail).
setCliqueConstraints([(I, J) | RestPairs], VertexList, Matrix, [bool_array_or([-X, -Y, Z]) | RestRowConstraints]-Tail) :-
    nth1(I, VertexList, X),
    nth1(J, VertexList, Y),
    matrixGetCell(Matrix, I, J, Z),
    setCliqueConstraints(RestPairs, VertexList, Matrix, RestRowConstraints-Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode
% converts the boolean list into an integer list holding the IDs of the vertices belonged to the clique
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
