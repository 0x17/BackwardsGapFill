solve rcpspoc using mip maximizing profit;
solvetime = rcpspoc.resusd;
slvstat = rcpspoc.solvestat;
execute_unload "results.gdx" x.l x.m z.l z.m solvetime slvstat;

solve rcpspoc2 using mip maximizing profit2;
solvetime = rcpspoc2.resusd;
slvstat = rcpspoc2.solvestat;
execute_unload "results2.gdx" x.l x.m z.l z.m solvetime slvstat;

solve rcpspoc3 using mip maximizing profit3;
solvetime = rcpspoc3.resusd;
slvstat = rcpspoc3.solvestat;
execute_unload "results3.gdx" x.l x.m z.l z.m solvetime slvstat;

solve rcpspmms using mip minimizing ms;
solvetime = rcpspmms.resusd;
slvstat = rcpspmms.solvestat;
execute_unload "resultsminms.gdx" x.l x.m z.l z.m solvetime slvstat;

solve rcpspmc using mip minimizing ms;
solvetime = rcpspmc.resusd;
slvstat = rcpspmc.solvestat;
execute_unload "resultsmincost.gdx" x.l x.m z.l z.m solvetime slvstat;
