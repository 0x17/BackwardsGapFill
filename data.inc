sets j Arbeitsgänge / j1*j7 /
     t Perioden / t0*t10 /
     r Ressourcen / r1 /;

parameters
         zmax(r)         Maximale ZK             / r1 0 /
         kappa(r)        Kosten pro Einheit ZK   / r1 1 /
         capacities(r)   Kapazitäten             / r1 2 /

         durations(j)    Dauern                  / j1 0, j2 1, j3 2, j4 2, j5 3, j6 1, j7 0 /
         costs(j)        Kosten                  / j1 0, j2 1, j3 1, j4 1, j5 1, j6 1, j7 0 /
         ustar(t)        Erlös bei Makespan t    / t1 10, t2 9, t3 8, t4 7, t5 6, t6 5, t7 4, t8 3, t9 2, t10 1 /;

table demands(j,r) Bedarf
                 r1
         j1      0
         j2      2
         j3      2
         j4      1
         j5      1
         j6      1
         j7      0;

parameters efts(j) Früheste Endzeitpunkte   / j1 0, j2 0, j3 0, j4 1, j5 2, j6 0, j7 5 /
           lfts(j) Späteste Endzeitpunkte   / j1 4, j2 7, j3 6, j4 9, j5 9, j6 9, j7 9 /;

set pred(i,j) yes gdw. i Vorgänger von j ist;
pred('j1', 'j2') = yes;
pred('j1', 'j3') = yes;
pred('j1', 'j6') = yes;
pred('j2', 'j4') = yes;
pred('j3', 'j5') = yes;
pred('j4', 'j7') = yes;
pred('j5', 'j7') = yes;
pred('j6', 'j7') = yes;
