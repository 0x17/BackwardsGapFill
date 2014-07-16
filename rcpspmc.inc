solve rcpspmc using mip minimizing ms;
solvetime = rcpspmc.resusd;
slvstat = rcpspmc.solvestat;
execute_unload "resultsmincost.gdx" x.l x.m z.l z.m solvetime slvstat;