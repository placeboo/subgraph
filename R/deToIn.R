#' Increasing order
#' function change the decreasing pair to increasing, eg. 2-1 to 1-2

#' @param a One node
#' @param b Another node

#' @return A string represending a edge, eg. 1-2

#' @examples
#' deToIn(2,1)
deToIn = function(a, b){ # length(vec) = 2
        if(a < b){
                return(paste(a,"-", b, sep = ""))
        }else{
                return(paste(b,"-", a, sep = ""))
        }

}
