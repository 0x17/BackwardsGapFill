namespace RCPSP

open PSPLibParser

module Runners =
    let testFilename =
          @"Projekte/32Jobs/Modellendogen0001.DAT"
        //  @"Projekte/12Jobs/EXPL2.DAT"
        //  @"Projekte/32JobsB/EXPL1.DAT"
        //  @"Projekte/32Jobs/Modellendogen0027.DAT"
        //  @"Projekte/32Jobs/Modellendogen0007.DAT"
        //  @"Projekte/16Jobs/EXPL41.DAT"
        //  @"Projekte/24Jobs/EXPL62.DAT"
        //  @"Projekte/28Jobs/EXPL71.DAT"
        //  @"Projekte/26Jobs/EXPL81.DAT"
        //  @"Projekte/32Jobs/Modellendogen0049.DAT"
        //    @"Projekte/32Jobs/Modellendogen0004.DAT"
        //    @"Projekte/j30/j301_1.sm"
        //    @"Projekte/j30/j3010_10.sm"

    let testProjectStructure () =
        PSPLibParser.parse testFilename
