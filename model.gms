sets j Arbeitsgänge
     t Perioden
     r Ressourcen;

alias(j,i);
alias(t,tau);

parameters
         zmax(r)         Maximale ZK
         kappa(r)        Kosten pro Einheit ZK
         capacities(r)   Kapazitäten
         durations(j)    Dauern
         costs(j)        Kosten
         ustar(t)        Erlös bei Makespan t
         efts(j)         Früheste Startzeitpunkte
         lfts(j)         Späteste Endzeitpunkte
         demands(j,r)    Bedarf;

set pred(i,j) yes gdw. i Vorgänger von j ist;

*$include "data.inc"
$GDXIN ProjectStructureData.gdx
$load j t r zmax kappa capacities durations costs ustar efts lfts demands pred
$GDXIN

*$gdxout exampleResults
*$unload j t r zmax kappa capacities durations costs ustar efts lfts demands pred
*$gdxout

set tw(j, t) yes gdw. t im Zeitfenster von j liegt;
tw(j, t)$(efts(j) <= ord(t) and ord(t) <= lfts(j)) = yes;

set actual(j) yes gdw. Job kein Dummy;
actual(j)$(1 < ord(j) and ord(j) < card(j)) = yes;

set lastJob(j) yes gdw. Job letzter AG;
lastJob(j)$(ord(j) = card(j)) = yes;

binary variable  x(j,t) 1 gdw. AG j in Periode t endet d.h. FTj=t;
integer variable z(r,t) Einheiten ZK von r in Periode t gebucht;
variable         profit Gewinn;

equations        objective   Zielfunktion
                 precedence  Vorrangbeziehung durchsetzen
                 resusage    Ressourcenverbrauchsrestriktion
                 once        Jeden AG genau 1x einplanen
                 oclimits    Beschränke buchbare ZK;

$eolcom §

objective                 .. profit =e= sum(j$lastJob(j), sum(t$tw(j,t), x(j,t)*ustar(t)))-sum(j$actual(j), costs(j))-sum(r, sum(t, z(r,t)*kappa(r)));
precedence(i,j)$pred(i,j) .. sum(t$tw(i,t), ord(t)*x(i,t)) =l= sum(t$tw(j,t), ord(t)*x(j,t)) - durations(j);
resusage(r,t)             .. sum(j$actual(j), sum(tau$(ord(tau)>=ord(t)+1 and ord(tau)<=ord(t)+durations(j)), demands(j,r)*x(j,tau))) =l= capacities(r) + z(r,t);
once(j)                   .. sum(t$tw(j,t), x(j,t)) =e= 1;
oclimits(r,t)             .. z(r,t) =l= zmax(r);

model rcpsp /all/;
solve rcpsp using mip maximizing profit;

display z.l;
