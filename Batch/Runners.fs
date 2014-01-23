namespace RCPSP

open PSPLibParser

module Runners =
    let testProjectStructure () =
        let testFilename =
        //  @"Projekte/12Jobs/Modellendogen002.DAT"
        //  @"Projekte/12Jobs/EXPL2.DAT"
        //  @"Projekte/32JobsB/EXPL1.DAT"
            @"Projekte/32Jobs/Modellendogen0027.DAT"
        //  @"Projekte/32Jobs/Modellendogen0003.DAT"
        //  @"Projekte/16Jobs/EXPL41.DAT"
        //    @"Projekte/24Jobs/EXPL62.DAT"
        //  @"Projekte/28Jobs/EXPL71.DAT"
        //  @"Projekte/26Jobs/EXPL81.DAT"
        PSPLibParser.parse testFilename
