
best1 <- function(state, detect) {
                 
if (sum(as.character(detect)==c("pneumonia","heart attack","heart failure"))==0) stop(gettextf("Error in best(%s, %s) : invalid outcome", 
                      state, outcome))
}
#