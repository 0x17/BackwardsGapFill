namespace RCPSP

open Utils

module LambdaBeta =
    let solveWithGA (ps:ProjectStructure) popSize numGens pmutate =
        LambdaTau.solveWithGAShared (fun ix -> rand 0 1) (fun b -> 1-b) ps.SerialSGSBeta ps popSize numGens pmutate
