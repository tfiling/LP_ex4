:- include('ex4.pl'). 
:- use_module(library(statistics)). 
 
 
measure(FileName) :- 
        statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]), 
        writeln(FileName), 
        nl, 
        consult(FileName), 
        schedule(NExams,Constraints),  
        schedulingSolve(schedule(NExams,Constraints), Sol), 
        statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]), 
        write('Execution took '), write(ExecutionTime), write(' ms.'),  
        nl. 
 
measure('instances/sched-hec-s-92', 1) :- measure('instances/sched-hec-s-92'). 
measure('instances/sched-sta-f-83', 2) :- measure('instances/sched-sta-f-83').% works 1831 ms 
measure('instances/sched-ute-s-92', 3) :- measure('instances/sched-ute-s-92').% works 794 ms 
measure('instances/sched-lse-f-91', 4) :- measure('instances/sched-lse-f-91'). 
measure('instances/sched-yor-f-83', 4) :- measure('instances/sched-yor-f-83'). 
measure('instances/sched-ear-f-83', 5) :- measure('instances/sched-ear-f-83'). 
measure('instances/sched-kfu-s-93', 6) :- measure('instances/sched-kfu-s-93'). 
measure('instances/sched-tre-s-92', 7) :- measure('instances/sched-tre-s-92'). 
measure('instances/sched-rye-s-93', 8) :- measure('instances/sched-rye-s-93').% out of global stack 
measure('instances/sched-car-f-92', 9) :- measure('instances/sched-car-f-92').% out of global stack 
measure('instances/sched-uta-s-92', 10) :- measure('instances/sched-uta-s-92').% out of global stack 
measure('instances/sched-car-s-91', 11) :- measure('instances/sched-car-s-91').% out of global stack