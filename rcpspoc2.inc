solve rcpspoc2 using mip maximizing profit2;
solvetime = rcpspoc2.resusd;
slvstat = rcpspoc2.solvestat;
execute_unload "results2.gdx" x.l x.m z.l z.m solvetime slvstat;