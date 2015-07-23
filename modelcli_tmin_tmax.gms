$eolcom §

*$set instname ProjectStructureData

options OPTCR = 0
        MIP = GUROBI
        RESLIM = 604800
        THREADS = 0;

sets j Arbeitsgänge
     t Perioden
     r Ressourcen;

alias(j,i);
alias(t,tau);

parameters
         solvetime       CPU-Zeit
         slvstat         Termination status
         zmax(r)         Maximale ZK
         kappa(r)        Kosten pro Einheit ZK
         capacities(r)   Kapazitäten
         durations(j)    Dauern
         u3(t)           Erlös (Parabel) bei Makespan t
         efts(j)         Früheste Startzeitpunkte
         lfts(j)         Späteste Endzeitpunkte
         demands(j,r)    Bedarf;

set pred(i,j) yes gdw. i Vorgänger von j ist;

$GDXIN %instname%.gdx
$load j t r zmax kappa capacities durations efts lfts demands pred
$GDXIN

set tw(j, t) yes gdw. t im Zeitfenster von j liegt;
tw(j, t)$(efts(j) <= ord(t) and ord(t) <= lfts(j)) = yes;

set actual(j) yes gdw. Job kein Dummy;
actual(j)$(1 < ord(j) and ord(j) < card(j)) = yes;

set lastJob(j) yes gdw. Job letzter AG;
lastJob(j)$(ord(j) = card(j)) = yes;

set fw(j, t, tau) yes gdw. AG j in tau beendet werden kann wenn er in t lief;
fw(j, t, tau)$(ord(tau)>=ord(t) and ord(tau)<=ord(t)+durations(j)-1) = yes;

binary variable  x(j,t) 1 gdw. AG j in Periode t endet d.h. FTj=t;

integer variable z(r,t) Einheiten ZK von r in Periode t gebucht;

variable         ms         Makespan;

equations
                mseq        Makespan ist Zielfunktion
                precedence  Vorrangbeziehung durchsetzen
                resusage    Ressourcenverbrauchsrestriktion
                once        Jeden AG genau 1x einplanen
                oclimits    Beschränke buchbare ZK
                zerozmax    Erzwinge zmax auf Null;

mseq                      .. ms =e= sum(j$lastJob(j), sum(t$tw(j,t), ord(t)*x(j,t)));
precedence(i,j)$pred(i,j) .. sum(t$tw(i,t), ord(t)*x(i,t)) =l= sum(t$tw(j,t), ord(t)*x(j,t)) - durations(j);
resusage(r,t)             .. sum(j$actual(j), demands(j,r)*sum(tau$fw(j,t,tau), x(j,tau))) =l= capacities(r) + z(r,t);
once(j)                   .. sum(t$tw(j,t), x(j,t)) =e= 1;
oclimits(r,t)             .. z(r,t) =l= zmax(r);
zerozmax(r,t)             .. z(r,t) =e= 0;

model rcpspmc  /mseq, precedence, resusage, once, zerozmax/;
model rcpspmms /mseq, precedence, resusage, once, oclimits/;

solve rcpspmc using mip minimizing ms;
solvetime = rcpspmc.resusd;
slvstat = rcpspmc.solvestat;
execute_unload "%instname%_tmax_results.gdx" x.l x.m z.l z.m ms.l ms.m solvetime slvstat;

solve rcpspmms using mip minimizing ms;
solvetime = rcpspmms.resusd;
slvstat = rcpspmms.solvestat;
execute_unload "%instname%_tmin_results.gdx" x.l x.m z.l z.m ms.l ms.m solvetime slvstat;


display z.l;
