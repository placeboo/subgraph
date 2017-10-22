#' Check duplication
#' Among the isomorphisms of a certain subgraphs, are there any duplications?
#' @param mat The matrix with colnames representing as edges, e.g 1-2, 2-3, and its entries are 1, 0 to represents there are edges or not.

#' @return Returns TRUE if there is no duplications among these isomorphisms of the certain subgraphs. Retures the index of duplication if there are duplications.

#' @examples
#' checkUnique(fillprob(1:6, c6(1:6)))


checkUnique = function(mat){
        col.names = colnames(mat) # edges
        edgeToNum.vec = 2^(0:(length(col.names)-1))
        binToTen.vec = apply(mat, 1, function(x)sum(edgeToNum.vec[x == 1]))

        # if F, there is duplication; T, no duplication
        FT = length(binToTen.vec) == length(unique(binToTen.vec))
        # test whether there are repliacation
        if(FT == FALSE){ # find out where duplicated
                return(which(duplicated(binToTen.vec)))
        }else{
                return(FT)
        }
}
