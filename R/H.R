#' H graphs
#' List all possible H graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of H graphs

#' @examples
#' H(c(1:6))

H = function(x){
        if(length(x) != 6){
                stop("Then number of nodes should be SIX!")
        }
        x = sort(x)
        twoP3.mat = twoP3(x)
        temp.mat = apply(twoP3.mat, 1, function(y)  as.numeric(unlist(strsplit(y, '-'))))
        # the number appear twice is the hub
        # connect the hub
        hub.mat = apply(temp.mat, 2, function(y) x[table(y) == 2])
        H.mat = cbind(twoP3.mat, paste(hub.mat[1,], "-", hub.mat[2,], sep = ""))
        return(H.mat)
}


