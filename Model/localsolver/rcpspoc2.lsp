// rpcspoc2.lsp

use io;

function model() {
	// Decision variables
	x[j in 1..njobs][t in 0..nperiods] <- bool(); // primary
	z[r in 1..nres][t in 0..nperiods] <- int(0, zmax[r]); // derived
	
	// Objective
	obj <- sum[t in efts[njobs]..lfts[njobs]](x[njobs][t] * revenue[t]) - sum[r in 1..nres][t in 0..nperiods](kappa[r]*z[r][t]);
	
	// Precedence constraint
	for[j in 1..njobs] {
		latestPredFinished <- sum[i in 1..njobs](adjMx[i][j] * sum[t in efts[i]..lfts[i]](t*x[i][t]));
		startingTime <- sum[t in efts[j]..lfts[j]](t*x[j][t]);
		constraint latestPredFinished <= startingTime;
	}
	
	// Resource capacity constraint
	for[r in 1..nres][t in 0..nperiods-1] {
		cumulatedDemand <- sum[j in 1..njobs][tau in t..min(nperiods,t+durations[j]-1)](demands[j][r]*x[j][tau]);
		constraint cumulatedDemand <= capacities[r] + z[r][t];
	}
	
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
	lsTimeLimit = 120;
	lsNbThreads = 8;
	lsVerbosity = 2;
}