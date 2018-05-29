:- include('ex4.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test kakuroVerify

test1(1,
        [14 = [I11, I12, I13, I14], 
        17 = [I3, I4, I5, I6], 
        3 = [I7, I8], 
        4 = [I9, I10], 
        11 = [I3, I7],
        6 = [I1, I2], 
        8 = [I4, I8, I11], 
        3 = [I1, I5], 
        18 = [I2, I6, I9, I13], 
        3 = [I10, I14]], % instance 

        [14 = [5, 1, 6, 2], 
        17 = [9, 2, 1, 5], 
        3 = [2, 1], 
        4 = [3, 1], 
        11 = [9, 2],
        6 = [2, 4], 
        8 = [2, 1, 5], 
        3 = [2, 1], 
        18 = [4, 5, 3, 6], 
        3 = [1, 2]], % board

        1). % should pass

test1(2,    
        [14 = [I11, I12, I13, I14], 
        17 = [I3, I4, I5, I6], 
        3 = [I7, I8], 
        4 = [I9, I10], 
        11 = [I3, I7],
        6 = [I1, I2], 
        8 = [I4, I8, I11], 
        3 = [I1, I5], 
        18 = [I2, I6, I9, I13], 
        3 = [I10, I14]], % instance 
 
        % [14 = [5, 1, 6, 2], 
        [14 = [5, 1, 6, 3],     % not 14
        17 = [9, 2, 1, 5], 
        3 = [2, 1], 
        4 = [3, 1], 
        11 = [9, 2],
        6 = [2, 4], 
        8 = [2, 1, 5], 
        3 = [2, 1], 
        18 = [4, 5, 3, 6], 
        3 = [1, 2]], % board

        0). % should pass

test1(3,     
        [14 = [I11, I12, I13, I14], 
        17 = [I3, I4, I5, I6], 
        3 = [I7, I8], 
        4 = [I9, I10], 
        11 = [I3, I7],
        6 = [I1, I2], 
        8 = [I4, I8, I11], 
        3 = [I1, I5], 
        18 = [I2, I6, I9, I13], 
        3 = [I10, I14]], % instance 

        % [14 = [5, 1, 6, 2], 
        [14 = [5, 1, 2, 2],     % 2 apears twice and not 14
        17 = [9, 2, 1, 5], 
        3 = [2, 1], 
        4 = [3, 1], 
        11 = [9, 2],
        6 = [2, 4], 
        8 = [2, 1, 5], 
        3 = [2, 1], 
        18 = [4, 5, 3, 6], 
        3 = [1, 2]], % board

        0). % should pass

test1(4,     
        [14 = [I11, I12, I13, I14], 
        17 = [I3, I4, I5, I6], 
        3 = [I7, I8], 
        4 = [I9, I10], 
        11 = [I3, I7],
        6 = [I1, I2], 
        8 = [I4, I8, I11], 
        3 = [I1, I5], 
        18 = [I2, I6, I9, I13], 
        3 = [I10, I14]], % instance 

        [14 = [5, 1, 6, 2], 
        17 = [9, 2, 1, 5], 
        3 = [2, 1], 
        4 = [2, 2],         % 4 but 2 appears twice
        11 = [9, 2],
        6 = [2, 4], 
        8 = [2, 1, 5], 
        3 = [2, 1], 
        18 = [4, 5, 3, 6], 
        3 = [1, 2]], % board

        0). % should pass

test1(5,     
        [14 = [I11, I12, I13, I14], 
        17 = [I3, I4, I5, I6], 
        3 = [I7, I8], 
        4 = [I9, I10], 
        11 = [I3, I7],
        6 = [I1, I2], 
        8 = [I4, I8, I11], 
        3 = [I1, I5], 
        18 = [I2, I6, I9, I13], 
        3 = [I10, I14]], % instance 

        [14 = [5, 1, 6, 2], 
        17 = [9, 2, 2, 4], % 17 but 2 appears twice
        3 = [2, 1], 
        4 = [3, 1], 
        11 = [9, 2],
        6 = [2, 4], 
        8 = [2, 1, 5], 
        3 = [2, 1], 
        18 = [4, 5, 3, 6], 
        3 = [1, 2]], % board

        0). % should pass

test1(6,
        [14 = [I11, I12, I13, I14], 
        17 = [I3, I4, I5, I6], 
        3 = [I7, I8], 
        4 = [I9, I10], 
        11 = [I3, I7],
        6 = [I1, I11], 
        8 = [I4, I8, I11], 
        3 = [I1, I5], 
        18 = [I2, I6, I9, I13], 
        3 = [I10, I14]], % instance 

        [14 = [5, 1, 6, 2], 
        17 = [9, 2, 1, 5], 
        3 = [2, 1], 
        4 = [3, 1], 
        11 = [9, 2],
        6 = [2, 4], 
        8 = [2, 1, 5], 
        3 = [2, 1], 
        18 = [4, 5, 3, 6], 
        3 = [1, 2]], % board

        0). % should pass

% test1(1,     
%             [14 = [I11, I12, I13, I14], 
%             17 = [I3, I4, I5, I6], 
%             3 = [I7, I8], 
%             4 = [I9, I10], 
%             11 = [I3, I7],
%             6 = [I1, I2], 
%             8 = [I4, I8, I11], 
%             3 = [I1, I5], 
%             18 = [I2, I6, I9, I13], 
%             3 = [I10, I14]], % board

%             1). % should pass            


testKakuroVerify :-
    test1(I, Instacne, Sol, ShouldPass),
    (ShouldPass is 1 -> 
        (kakuroVerify(Instacne, Sol) -> writeln(I: ok); writeln(I: failed)) ;
        (kakuroVerify(Instacne, Sol) -> writeln(I: failed); writeln(I: ok))
    ),
    fail.
testKakuroVerify.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test kakuroSolve

test2(1,
        [14 = [I11, I12, I13, I14],
        17 = [I3, I4, I5, I6], 
        3 = [I7, I8], 
        4 = [I9, I10], 
        11 = [I3, I7],
        6 = [I1, I2], 
        8 = [I4, I8, I11], 
        3 = [I1, I5], 
        18 = [I2, I6, I9, I13], 
        3 = [I10, I14]
        ],        % instance

        [14 = [5, 1, 6, 2],
        17 = [9, 2, 1, 5], 
        3 = [2, 1], 
        4 = [3, 1], 
        11 = [9, 2],
        6 = [2, 4], 
        8 = [2, 1, 5], 
        3 = [2, 1], 
        18 = [4, 5, 3, 6], 
        3 = [1, 2]
        ],            % solution

        1). % are equal

test2(2,
        [
                14 = [X1, X2],
                3 = [X3, X4],
                11 = [X1, X3],
                6 = [X2, X4]
        ],

        [
                14 = [9, 5],
                3 = [2, 1],
                11 = [9, 2],
                6 = [5, 1]
        ],
        1
).

test2(3,
        [
                6 = [X1, X2],
                4 = [X3, X4],
                6 = [X1, X3],
                4 = [X2, X4]
        ],

        [
                6 = [5, 1],
                4 = [1, 3],
                6 = [5, 1],
                4 = [1, 3]
        ],
        1
).

test2(4,
        [
                16 = [X1, X2, X3],
                9 = [X4, X5, X6],
                17 = [X7, X8, X9],
                18 = [X10, X11, X12],
                3 = [X3, X6],
                30 = [X2, X5, X9, X12],
                11 = [X1, X4, X8, X11],
                16 = [X7, X10]
        ],
        [
                16 = [5, 9, 2],
                9 = [2, 6, 1],
                17 = [9, 1, 7],
                18 = [7, 3, 8],
                3 = [2, 1],
                30 = [9, 6, 7, 8],
                11 = [5, 2, 1, 3],
                16 = [9, 7]
        ],
        1
).

testKakuroSolve :-
    test2(I, Instance, Sol, AreEqual),
    (AreEqual is 1 -> 
        (kakuroSolve(Instance, Result), Sol = Result -> writeln(I: ok);writeln(I: failed)) ;
        (kakuroSolve(Instance, Result), Sol \= Result -> writeln(I: failed); writeln(I: ok))
    ),
    fail.
testKakuroSolve.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test schedulingVerify

test3(1,
        schedule(2, []),        % instance
        [1, 1],                 % solution
        1                       % should pass
).


test3(2,
        schedule(2, []),        % instance
        [1, 2],                 % solution
        1                       % should pass
).

test3(3,
        schedule(2, [c(1,2)]),  % instance
        [1, 1],                 % solution
        0                       % should pass
).

test3(4,
        schedule(2, [c(1,2)]),  % instance
        [1, 2],                 % solution
        1                       % should pass
).

test3(5,
        schedule(3, [c(1, 3), c(1, 2)]),  % instance
        [1, 1, 2],                 % solution
        0                       % should pass
).

test3(6,
        schedule(3, [c(1, 3), c(1, 2)]),  % instance
        [1, 3, 2],                 % solution
        1                      % should pass
).


test3(7,
        schedule(2, [c(1,2)]),  % instance
        [1],                 % solution
        0                       % should pass
).

test3(8,
        schedule(2, [c(1,2)]),  % instance
        [1],                 % solution
        0                       % should pass
).

test3(9,
        schedule(3, [c(1, 3), c(1, 2)]),  % instance
        [1, 1],                 % solution
        0                       % should pass
).

test3(10,
        schedule(3, [c(1, 3), c(1, 2)]),  % instance
        [1, 3],                 % solution
        0                      % should pass
).


testSchedulingVerify :-
    test3(I, Instance, Sol, ShouldPass),
    (ShouldPass is 1 -> 
        (schedulingVerify(Instance, Sol) -> writeln(I: ok);writeln(I: failed)) ;
        (schedulingVerify(Instance, Sol) -> writeln(I: failed); writeln(I: ok))
    ),
    fail.
testSchedulingVerify.


:- 
    writeln('testKakuroVerify'),
    testKakuroVerify,
    writeln('testKakuroSolve'),
    testKakuroSolve,
    writeln('testSchedulingVerify'),
    testSchedulingVerify.