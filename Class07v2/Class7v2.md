Class 7 V2
================
Nathaniel Bloom
1/28/2020

``` r
source("http://tinyurl.com/rescale-R")

#simplify to single vectors 
x <- df1$IDs
y <- df2$IDs

intersect(x, y) #where x and y have the same value 
```

    ## [1] "gene2" "gene3"

``` r
# "gene2" "gene3"
x %in% y # values of x in y 
```

    ## [1] FALSE  TRUE  TRUE

``` r
# FALSE  TRUE  TRUE
x[x %in% y] # values of x for which x in y 
```

    ## [1] "gene2" "gene3"

``` r
#"gene2" "gene3"
y %in% x # values of y in x 
```

    ## [1]  TRUE FALSE  TRUE FALSE

``` r
#   TRUE FALSE  TRUE FALSE
y[y %in% x] # values of y for which y in x 
```

    ## [1] "gene2" "gene3"

``` r
# "gene2" "gene3"

# combine the values retreived for the following 
cbind( x[ x %in% y ], y[ y %in% x ] )
```

    ##      [,1]    [,2]   
    ## [1,] "gene2" "gene2"
    ## [2,] "gene3" "gene3"

``` r
#      [,1]    [,2]   
#[1,] "gene2" "gene2"
#[2,] "gene3" "gene3"

# use extract to turn the previous line into a function using extract 
gene_intersect <- function(x, y) {
  cbind( x[ x %in% y ], y[ y %in% x ] )
  # returns same answer 
  #   [,1]    [,2]   
  #[1,] "gene2" "gene2"
  #[2,] "gene3" "gene3"
}
gene_intersect(x, y) # use the function 
```

    ##      [,1]    [,2]   
    ## [1,] "gene2" "gene2"
    ## [2,] "gene3" "gene3"

``` r
    # [,1]    [,2]   
#[1,] "gene2" "gene2"
#[2,] "gene3" "gene3"

# chnage function to be able to take data frame as input 
Gene_intersect2 <- function (df1, df2){
  cbind(df1[ df1$IDs %in% df2$IDs, ], #df1 value of df1 IDs in df2 IDs 
        df2[ df2$IDs %in% df1$IDs, "exp"] ) # df2 value of df2 IDs in df1 IDs
}
#call the function 
Gene_intersect2 (df1, df2)
```

    ##     IDs exp df2[df2$IDs %in% df1$IDs, "exp"]
    ## 2 gene2   1                               -2
    ## 3 gene3   1                                1

``` r
#> IDs         exp               df2[df2$IDs %in% df1$IDs, "exp"]
#> 2 gene2      1                             -2
#> 3 gene3      1                              1

# N.B. Our input $IDs column name may change:
#So lets add flexibility by allowing the user to specify the
#gene containing column name

# Experiment first to make sure things are as we expect
gene.colname="IDs"
df1[,gene.colname]
```

    ## [1] "gene1" "gene2" "gene3"

``` r
# "gene1" "gene2" "gene3"

Gene_intersect_3 <- function(df1, df2, gene.colname="IDs"){
   cbind( df1[ df1[,gene.colname] %in%
          df2[,gene.colname], ],
          exp2=df2[ df2[,gene.colname] %in%
          df1[,gene.colname], "exp"] )
}
Gene_intersect_3 (df1, df2)
```

    ##     IDs exp exp2
    ## 2 gene2   1   -2
    ## 3 gene3   1    1

``` r
#   IDs   exp exp2
#2 gene2   1   -2
#3 gene3   1    1
# function works but it is not easy on the reader 

#simplify the function 

Gene_intersect4 <- function(df1, df2, gene.colname="IDs") {

 df1.name <- df1[,gene.colname]
 df2.name <- df2[,gene.colname]

 df1.inds <- df1.name %in% df2.name
 df2.inds <- df2.name %in% df1.name

 cbind( df1[ df1.inds, ],
 exp2=df2[ df2.inds, "exp"] )
}
Gene_intersect4(df1, df2)
```

    ##     IDs exp exp2
    ## 2 gene2   1   -2
    ## 3 gene3   1    1
