best <- function(state, outcome) {
        ## Read outcome data
        outcomes <- read.csv("outcome-of-care-measures.csv")
        ## Check that state and outcome are valid
        if (!(state %in% outcomes[,7])) stop("invalid state")
        ill <- switch(outcome, "heart attack"=11, "heart failure"=17, "pneumonia"=23, 0)
        if (ill == 0) stop("invalid outcome")
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        range <- outcomes[outcomes[['State']]==state,]
        nnarange <- range[range[,ill]!="Not Available",]
        bests <- nnarange[nnarange[,ill]==min(as.numeric(as.character(nnarange[,ill]))),]
        as.character(if(length(best)==46) bests[['Hospital.Name']]
                     else bests[bests[,2]==min(as.character(bests[,2])),][[2]])
}

