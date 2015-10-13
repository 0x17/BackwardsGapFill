solve rcpspmms using mip minimizing ms;
solvetime = rcpspmms.resusd;
slvstat = rcpspmms.solvestat;
execute_unload "Results/resultsminms.gdx" x.l x.m z.l z.m solvetime slvstat;

solve rcpspmc using mip minimizing ms;
solvetime = rcpspmc.resusd;
slvstat = rcpspmc.solvestat;
execute_unload "Results/resultsmincost.gdx" x.l x.m z.l z.m solvetime slvstat;
