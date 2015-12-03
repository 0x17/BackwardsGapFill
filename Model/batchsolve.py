import os
import sys

if len(sys.argv) == 3:
    dirname = sys.argv[1]
    timelimit = float(sys.argv[2])
else:
    dirname = "j30"
    timelimit = 10

def solveWithMethod(method, instancePath):
	cmd = "Solver.exe " + method + " " + str(timelimit) + " " + instancePath
	os.system(cmd)

def forceDeleteFile(fn):
	while True:
		try:
			os.remove(fn)
		except OSError:
			print "Deleting " + fn + " failed. Retry!"
		else:
			break
	
def convertSmToGdx(fn):
	os.system("Convert.exe " + fn)
	for f in ["_gams_net_gdb0.gdx", "_gams_net_gjo0.gms","_gams_net_gjo0.lst"]:
		forceDeleteFile(f)

def solveWithGams(solver, instname):
	gams_prefix = "gams modelcli.gms --timelimit="+str(timelimit)+" --solver="+solver+" --instname=" + instname
	convertSmToGdx(instname)
	os.system(gams_prefix)
	forceDeleteFile(instname + ".gdx")
		
for fn in os.listdir(dirname):
	pfn = dirname + "/" + fn
	
	solveWithGams("Gurobi", pfn)
	solveWithGams("LocalSolver", pfn)

	solveWithMethod("BranchAndBound", pfn)
	solveWithMethod("LocalSolver", pfn)
	
	for i in range(5):
		solveWithMethod("GA" + str(i), pfn)
