solve rcpspoc using mip maximizing profit3;
solvetime = rcpspoc.resusd;
slvstat = rcpspoc.solvestat;
execute_unload "results.gdx" x.l x.m z.l z.m solvetime slvstat;
