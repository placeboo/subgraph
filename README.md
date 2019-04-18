subgraph
================
Jackie Jiaqi Yin
4/17/2019

Overview
--------

<<<<<<< HEAD
**subgraph** is a system for displaying graphs by showing the connected edges. You prvide the name of nodes, and the name of graph which is based on [the graph list](http://graphclasses.org/smallgraphs.html). Then **subgraph** will provide you all homogenous graphs by showing the connected edges. The maximum number of nodes is 6.
=======
**subgraph** is a system for displaying graphs by showing the connected edges. Base on [the graph list](http://graphclasses.org/smallgraphs.html)
>>>>>>> b4ffae73821f545501986e35eafc53829fee9bf9

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("placeboo/subgraph")
<<<<<<< HEAD
install.packages("combinat", repos = "http://cran.us.r-project.org")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/8m/ddcyb_xx6hx0nx6yr5msflfm0000gn/T//RtmpgZmci0/downloaded_packages

=======
```

>>>>>>> b4ffae73821f545501986e35eafc53829fee9bf9
Useage
------

``` r
library(subgraph)
<<<<<<< HEAD
library(combinat)
```

If we want to display all the possible homogenous graphs for [claw](http://graphclasses.org/images/g_claw.gif), whose number of nodes is 4,

``` r
claw(c(1:4))
```

    ##      [,1]  [,2]  [,3] 
    ## [1,] "1-2" "1-3" "1-4"
    ## [2,] "1-2" "2-3" "2-4"
    ## [3,] "1-3" "2-3" "3-4"
    ## [4,] "1-4" "2-4" "3-4"

More
----

Check the help page. The package is incorporated with the working paper of mine.
=======
```
>>>>>>> b4ffae73821f545501986e35eafc53829fee9bf9
