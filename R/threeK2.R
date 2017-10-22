#' 3k2 graphs
#' List all possible 3k2 graphs based on given six nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of 3k2 graphs

#' @examples
#' threek2(c(1:6))

threeK2 = function(x){
        x = sort(x)

        smallest = x[1]
        rest = x[-1]
        temp = paste(smallest, '-', rest[1], sep = "")
        twoK2.mat = twoK2(rest[-1])
        threeK2.mat = cbind(rep(temp, nrow(twoK2.mat)), twoK2.mat)
        for(i in 2: 5){
                temp = paste(smallest, '-', rest[i], sep = "")
                twoK2.mat = twoK2(rest[-i])
                threeK2.mat = rbind(threeK2.mat, cbind(rep(temp, nrow(twoK2.mat)), twoK2.mat))
        }
        return(threeK2.mat)
}
