// rpcspoc.lsp

use io;

function model() {
	// Primary decision variables
	S[j in 1..njobs] <- int(0, nperiods-1);
	
	// Derived cumulated demand expressions
	cumulatedDemand[r in 1..nres][t in 0..nperiods-1] <- sum[j in 1..njobs](demands[j][r] * (t > S[j] && t <= S[j] + durations[j]));
		
	// Objective
	obj <- revenue[S[njobs]] + sum[r in 1..nres][t in 0..nperiods-1](-kappa[r]*max(0,cumulatedDemand[r][t]-capacities[r]));
	
	// Precedence restrictions
	for[j in 1..njobs]
		constraint S[j] >= max[i in 1..njobs](adjMx[i][j]*(S[i]+durations[i]));
	
	// Resource capacity restrictions
	for[r in 1..nres][t in 0..nperiods-1]
		constraint cumulatedDemand[r][t] <= capacities[r] + zmax[r];
	
	maximize obj;
}

function input() {
	if(inFileName == nil) inFileName = "LSInstance.txt";
	local f = io.openRead(inFileName);
	
	f.readln();
	njobs = f.readInt(); // J
	nperiods = f.readInt(); // T
	nres = f.readInt(); // R
	
	f.readln();
	durations[j in 1..njobs] = f.readInt(); // d_j
	f.readln();
	demands[j in 1..njobs][r in 1..nres] = f.readInt(); // k_{jr}
	f.readln();
	adjMx[i in 1..njobs][j in 1..njobs] = f.readInt(); // i \in P_j
	f.readln();
	capacities[r in 1..nres] = f.readInt(); // K_r
	
	f.readln();
	revenue[t in 0..nperiods] = f.readDouble(); // u_t
	f.readln();
	zmax[r in 1..nres] = f.readInt(); // \overline{z}
	f.readln();
	kappa[r in 1..nres] = f.readDouble(); // \kappa_r
}

function output() {
	if(outFileName == nil) outFileName = "ResultSchedule.txt";
	local f = io.openWrite(outFileName);
	for[j in 1..njobs] {
		local outStr = j+"->"+S[j].value;		
		f.print(outStr);
		print(outStr);
		if(j < njobs) {
			print("\n");
			f.print("\n");
		}
	}
}

function param() {
	lsTimeLimit = 2;
	lsNbThreads = 8;
	lsVerbosity = 2;
}