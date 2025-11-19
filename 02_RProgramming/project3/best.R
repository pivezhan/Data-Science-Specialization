best <- function(state, Output) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if (sum(Output==c("pneumonia","heart attack","heart failure"))==0){
                stop(gettextf("invalid outcome"))
        }
        if (sum(as.character(state)==outcome$State)==0){
                stop(gettextf("invalid state"))
        }  
        if (Output=="heart attack"){
        outp<-sort(outcome[(outcome[,11]==min(as.numeric(outcome[(outcome$State==state),11]),na.rm=T))&(outcome$State==state),2],decreasing= F)
        } else if (Output=="pneumonia"){
        outp<-sort(outcome[(outcome[,23]==min(as.numeric(outcome[(outcome$State==state),23]),na.rm=T))&(outcome$State==state),2],decreasing= F)
        } else if (Output=="heart failure"){
        outp<-sort(outcome[(outcome[,17]==min(as.numeric(outcome[(outcome$State==state),17]),na.rm=T))&(outcome$State==state),2],decreasing= F)
        }
        outp
        }