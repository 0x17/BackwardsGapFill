solve rcpspdl using mip maximizing profit;
solvetime = rcpspdl.resusd;
slvstat = rcpspdl.solvestat;
execute_unload "Results/results.gdx" x.l x.m z.l z.m solvetime slvstat;
