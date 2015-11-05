// rpcspoc3.lsp

use io;

function model() {
	// Decision variables
	x[j in 1..njobs][t in 0..nperiods] <- bool(); // primary
	
	// Derived cumulated demands
	cumulatedDemand[r in 1..nres][t in 0..nperiods] <- sum[j in 1..njobs][tau in t..min(nperiods,t+durations[j]-1)](demands[j][r]*x[j][tau]);
	
	// Objective
	obj <- sum[t in efts[njobs]..lfts[njobs]](x[njobs][t] * revenue[t]) - sum[r in 1..nres][t in 0..nperiods](kappa[r]*max(0, cumulatedDemand[r][t]-capacities[r]));
	
	// Each once
	for[j in 1..njobs]
		constraint sum[t in 0..nperiods](x[j][t]) == 1;
	
	// Precedence constraint
	for[j in 1..njobs] {
		latestPredFinished <- max[i in 1..njobs](adjMx[i][j] * sum[t in efts[i]..lfts[i]](t*x[i][t]));
		startingTime <- sum[t in efts[j]..lfts[j]](t*x[j][t])-durations[j];
		constraint latestPredFinished <= startingTime;
	}
	
	// Resource capacity constraint
	for[r in 1..nres][t in 0..nperiods]
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
	efts[j in 1..njobs] = f.readInt(); // EFTS_j
	f.readln();
	lfts[j in 1..njobs] = f.readInt(); // LFTS_j
	
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
		for[t in 0..nperiods]
			if(x[j][t].value == 1)
				S[j] = t - durations[j];
		
		local outStr = j+"->"+S[j];
		f.print(outStr);
		print(outStr);
		if(j < njobs) {
			print("\n");
			f.print("\n");
		}
	}
}

function param() {
	lsTimeLimit = 120;
	lsNbThreads = 8;
	lsVerbosity = 2;
}