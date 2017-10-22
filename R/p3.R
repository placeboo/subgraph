#' P3 edges
#' List all possible p3 graphs based on given nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of p3 graphs

#' @examples
#' p3(c(1, 2, 4, 5))
#' p3(c(1, 4, 3, 5))

p3 = function(x){
        temp.list = list()
        for(i in 1: length(x)){ # go through the middle piont
                temp.list[[i]] = as.matrix(combn(x[-i], 2))
        }
        mtx = matrix(,,ncol = nrow(temp.list[[1]]))
        leng = length(temp.list)
        for(ll in 1: leng){
                ll.mtx = temp.list[[ll]]
                temp.mtx = matrix(0, nrow = ncol(ll.mtx), ncol = nrow(ll.mtx))
                for(k in 1: ncol(ll.mtx)){
                        for(i in 1: nrow(ll.mtx)){
                                temp = sort(c(x[ll], ll.mtx[i, k]))
                                temp.mtx[k, i] = paste(temp[1], "-", temp[2], sep = "")
                        }
                }
                mtx = rbind(mtx, temp.mtx)
        }
        return(mtx[-1, ])
}
