##
## Coursera RPG07 course - jaimolto@gmail.com - Peer Assessments/Programming Assignment 2: Lexical Scoping
##
## Given a matrix "x", makeCacheMatrix() function will create a metastructure-of-the-matrix/metamatrix,
## as a list, with four elements, each of them a function
##
## function set - Function to set up the matrix value within the metamatrix structure
## function get - Function to retrieve the matrix value stored into the metamatrix structure
## function setinv - Function to calculate the inverse matrix in a variable "cachedinv", once the inverse has been calculated, for further use
## function getinv - Function to return the variable "cachedinv" (cached inverse matrix) if available
## 
## The metastructure will be used later on by function "cacheSolve()" to minimize the effort to calculate inverse matrix
## by looking if the inverse has been already calculated abd is available as cachedinv, before calculate the inverse using "solve"
##
makeCacheMatrix <- function(thematrix = matrix()) {
  
  cachedinv <- NULL
  
  ## In case original matrix is changed using "set function", we will need to:
  ## 1. Assign the new matrix y to the internal variable thematrix, so when function get is invoked the right matrix is returned
  ## 2. Assign NULL cachedinv, so next time getinv() is invoked, it will return NULL and instructs to calculate inverse again
        
        set <- function(y) {
          thematrix <<- y
          cachedinv <<- NULL
        }
  
  ## get function returns the original matrix
        
        get <- function() thematrix
  
  ## setinv function is used to store the inverse matrix value within metamatrix 
        
        setinv <- function(inv) cachedinv <<- inv
  
  ## getinv function is used to retrieve the inverse matrix value within metamatrix, cached, if available 
  
        getinv <- function() cachedinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
#
#
## CacheSolve function: Given a metamatrix "x" created with previous function "makeCacheMatrix",
## function will return de inverse of original matrix
## by looking first in the cached inverse (inverse calculation were already done, thus fast proccessing)
## or by calculating the inverse with "solve", if it is the first time calculation
##
## if new matrix is assigned to metamatrix "x" using x$set function, a new inverse calculation will be required

cacheSolve <- function(x, ...) {

  ## Function CacheSolve Returns a matrix that is the inverse of 'x' using cached info if available
  
  ## we capture system start to be able to calculate proccessing time with and without cache.
  
        start <- Sys.time()
        
  ## We first try to get the already cached inverse matrix calculated
  ## we will compute also the time for this specific operation using system.time function
        
        invtime <- system.time(inv <- x$getinv())
  
  ## The first time the inverse of matrix x is calculated, it will need to be done using "solve" --> inv == NULL
  ## Subsequent calculations will just get the the value from x$getinv --> inv != NULL

        if(!is.null(inv)) {
                
                message("getting cached -solved inverse matrix- data")
                
                ## Calculating & showing proccesing time, using "Sys.time()" and "system.time()" for comparison
        
                proccess_time <- Sys.time() - start
                message("Proccessing time - CACHED: ", proccess_time)
                message("Proccessing time - CACHED (system.time): ")
                print(invtime)
                return(inv)
        }
  
  ## Is the first time the inverse matrix is calculated, so we need to use solve() standard function
  ## we will compute also the time for this specific operation using system.time function
        
        OriMatrix <- x$get() 
        invtime <- system.time(inv <- solve(OriMatrix, ...))
  
        ## Calculating proccesing time
  
        proccess_time <- Sys.time() - start
        message("Proccessing time - NON CACHED: ", proccess_time)
        message("Proccessing time - NON CACHED (system.time): ")
        print(invtime)
  
        ## Before returning the inv, we set up the cached value in cachedinv, so it can be retrieved on later
        ## inverse calculation of the matrix using the function x$getinv
  
        x$setinv(inv)
        inv
}

## Example output of functions follows below
##
## set.seed(1)

## ## Matrix Sample of 10.000 elements, randomly generated for a matrix 100x100
## mat <- matrix(rnorm(1000000, 10,4), 1000, 1000)
## str(mat)
##      num [1:1000, 1:1000] 7.49 10.73 6.66 16.38 11.32 ...

## ## Building the metastructure
## matcached <- makeCacheMatrix(mat)
## matcached
        ##  $set
        ## function (y) 
        ## {
        ##   thematrix <<- y
        ##   cachedinv <<- NULL
        ## }
        ## <environment: 0x0000000008fc29c8>
        ##   
        ##   $get
        ## function () 
        ##   thematrix
        ## <environment: 0x0000000008fc29c8>
        ##   
        ##   $setinv
        ## function (inv) 
        ##   cachedinv <<- inv
        ## <environment: 0x0000000008fc29c8>
        ##   
        ##   $getinv
        ## function () 
        ##   cachedinv
        ## <environment: 0x0000000008fc29c8>
##   
## Calculating the inverse the first time - As first time inverse matrix not yet in cache
## matinverse <- cacheSolve(matcached)
##      Proccessing time - NON CACHED: 1.85681414604187
##      Proccessing time - NON CACHED (system.time): 
##      user  system elapsed 
##      1.73    0.01    1.77 
##
## If we calculate the inverse again - now we will use the cached one - (as shown, compute time is much lower)
## matinverse <- cacheSolve(matcached)
##      getting cached -solved inverse matrix- data
##      Proccessing time - CACHED: 0.0429999828338623
##      Proccessing time - CACHED (system.time): 
##      user  system elapsed 
##        0       0       0 
##
## Same situation if we calculate again
## matinverse <- cacheSolve(matcached)
##      getting cached -solved inverse matrix- data
##      Proccessing time - CACHED: 0.0450000762939453
##      Proccessing time - CACHED (system.time): 
##      user  system elapsed 
##        0       0       0 
## We change original matrix with $set() and calculate again inverse. As new matrix, is not yet cached
## matcached$set(matinverse)
## matinverse <- cacheSolve(matcached)
##      Proccessing time - NON CACHED: 1.86199998855591
##      Proccessing time - NON CACHED (system.time): 
##      user  system elapsed 
##      1.74    0.00    1.78 
##
## If we calculate the inverse again - now we will use the cached one - (as shown, compute time is much lower)
## matinverse <- cacheSolve(matcached)
##      getting cached -solved inverse matrix- data
##      Proccessing time - CACHED: 0.0470001697540283
##      Proccessing time - CACHED (system.time): 
##      user  system elapsed 
##        0       0       0 
##
## we check inverse matrix is properly calculated, with small matrix 3x3
## > set.seed(1)
## > mat <- matrix(rnorm(9, 10,4), 3, 3)
## > matcached <- makeCacheMatrix(mat)
## > matinverse <- cacheSolve(matcached)
##      Proccessing time - NON CACHED: 0.0880000591278076
##      Proccessing time - NON CACHED (system.time): 
##      user  system elapsed 
##       0       0       0 
## > mat %*% matinverse
##                  [,1]         [,2]     [,3]
##      [1,]  1.000000e+00 0.000000e+00    0
##      [2,]  6.938894e-17 1.000000e+00    0
##      [3,] -8.326673e-17 4.440892e-16    1
## > matinverse <- cacheSolve(matcached)
##      getting cached -solved inverse matrix- data
##      Proccessing time - CACHED: 0.0439999103546143
##      Proccessing time - CACHED (system.time): 
##      user  system elapsed 
##      0       0       0 
## > mat %*% matinverse
##                  [,1]         [,2]     [,3]
##      [1,]  1.000000e+00 0.000000e+00    0
##      [2,]  6.938894e-17 1.000000e+00    0
##      [3,] -8.326673e-17 4.440892e-16    1
## 
