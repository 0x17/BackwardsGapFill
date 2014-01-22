namespace RCPSP

open PSPLibParser

module Runners =
    let testProjectStructure () =
        //let testFilename = @"Projekte/12Jobs/Modellendogen002.DAT"
        //let testFilename = @"Projekte/12Jobs/EXPL2.DAT"
        //let testFilename = @"Projekte/32JobsB/EXPL1.DAT"
        let testFilename = @"Projekte/32Jobs/Modellendogen0027.DAT"
        //let testFilename = @"Projekte/32Jobs/Modellendogen0003.DAT"
        //let testFilename = @"Projekte/16Jobs/EXPL41.DAT"
        PSPLibParser.parse testFilename
