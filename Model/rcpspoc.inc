solve rcpspoc using mip maximizing profit;
solvetime = rcpspoc.resusd;
slvstat = rcpspoc.solvestat;
modelstat = rcpspoc.modelstat;
execute_unload "Results/results.gdx" x.l x.m z.l z.m solvetime slvstat modelstat;
