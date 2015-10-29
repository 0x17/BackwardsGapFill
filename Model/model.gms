$ontext
STj=tk bzw. FTj=tk bedeutet, dass AG j am Ende von Periode k anfängt bzw. beginnt.
tk als Zeitpunkt interpretiert bedeutet also Ende der Periode k.
$offtext

$eolcom §

sets j Arbeitsgänge
     t Perioden
     r Ressourcen;

alias(j,i);
alias(t,tau);

parameters
         solvetime       CPU-Zeit
         slvstat         Termination status
         modelstat       Optimality status
         zmax(r)         Maximale ZK
         kappa(r)        Kosten pro Einheit ZK
         capacities(r)   Kapazitäten
         durations(j)    Dauern
         u(t)            Erlös bei Makespan t
         efts(j)         Früheste Startzeitpunkte
         lfts(j)         Späteste Endzeitpunkte
         demands(j,r)    Bedarf
         deadline        Maximal zulässige Projektdauer;

set pred(i,j) yes gdw. i Vorgänger von j ist;

*$include "data.inc"

*$ontext
$GDXIN ProjectStructureData.gdx
$load j t r zmax kappa capacities durations u efts lfts demands pred deadline
$GDXIN
*$offtext

$ontext
 $gdxout ExampleData
 $unload j t r zmax kappa capacities durations u efts lfts demands pred deadline
 $gdxout
$offtext

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

variable         profit  Gewinn (Andre)
                 ms      Makespan;

equations
                objective   Zielfunktion
                mseq        Makespan in ms setzen
                deq         Deadline erzwingen
                precedence  Vorrangbeziehung durchsetzen
                resusage    Ressourcenverbrauchsrestriktion
                once        Jeden AG genau 1x einplanen
                oclimits    Beschränke buchbare ZK
                zerozmax    Erzwinge zmax auf Null;

objective                 .. profit =e= sum(j$lastJob(j), sum(t$tw(j,t), x(j,t)*u(t)))-sum(r, sum(t, z(r,t)*kappa(r)));
precedence(i,j)$pred(i,j) .. sum(t$tw(i,t), ord(t)*x(i,t)) =l= sum(t$tw(j,t), ord(t)*x(j,t)) - durations(j);
resusage(r,t)             .. sum(j$actual(j), demands(j,r)*sum(tau$fw(j,t,tau), x(j,tau))) =l= capacities(r) + z(r,t);
once(j)                   .. sum(t$tw(j,t), x(j,t)) =e= 1;
oclimits(r,t)             .. z(r,t) =l= zmax(r);

mseq                      .. ms =e= sum(j$lastJob(j), sum(t$tw(j,t), ord(t)*x(j,t)));
deq                       .. deadline+1 =e= sum(j$lastJob(j), sum(t$tw(j,t), ord(t)*x(j,t)));

zerozmax(r,t)             .. z(r,t) =e= 0;

model rcpspoc  /objective, precedence, resusage, once, oclimits/;
model rcpspmc  /mseq, precedence, resusage, once, zerozmax/;
model rcpspmms /mseq, precedence, resusage, once, oclimits/;
model rcpspdl  /objective, precedence, resusage, once, oclimits, deq/;

$include %IncFile%
*$include rcpspoc.inc

display z.l;
