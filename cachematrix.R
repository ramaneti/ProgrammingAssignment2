## This R package has four functions:
## 1. makeCacheMatrix(), 2. makeVector(), 3. cachemean() and 4. cacheSolve()
## The following activities use the above functions.
## (I)To create a vector and return a cached value of it's mean it exists else return a calculated value of mean
##    the following steps can be used
##    1.vec <- makeVector(1:100)  
##    2.cachemean(vec)    
##    3.cachemean(Vec)  ## repated for returning a cached value                 
## (II)To generate a matrix and it's inverse and return a cached one if it exists, the following steps
##     can be used:
##     1.matdefn <- matrix(data = 100:103, nrow = 2, ncol = 2, byrow = FALSE, dimnames = NULL)
##     2.mat <- makeCacheMatrix(matdefn)
##     3.cacheSolve(mat)
##     4.cacheSolve(mat) ## repated for returning a cached value

## The makeCacheMatrix function creates a matrix that can cached
## The input to the matrix matdefn <- matrix(data = 100:103, nrow = 2, ncol = 2, byrow = FALSE, dimnames = NULL)
## The inverse matrix is also initialized here and will be used when calling the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  mat <- x
  invx <<- matrix() ## initialize the inverse matrix so that it can be used in the cacheSolve function
  return(mat)
}

## The makeVector function creates a vector, which is actually a list functions 1 to 4 below to create a vector
## and calculate the mean of the values in the vector
## 1. set the value of the vector
## 2.  get the value of the vector
## 3.	set the value of the mean
## 4.	get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL    ## initializes m (mean) to a NULL
  set <- function(y) {   ## make a vector whose mean is to be found as argument
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## This function computes the inverse of the special "matrix" returned by function makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(!is.na(invx[1])) {            ## check if inverse matrix is not empty (conatining NAs)
    message("getting cached data")
    return(invx)
  }
  else {
    
    invx <<- solve(x) ## inverse of matrix x
    return(invx)
  }
} 

## The following function calculates the mean of a vector created by the makeVector function. 
## It first checks to see if the mean has already been calculated. If so, it gets the mean from the cache 
## and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the 
## cache via the setmean function.
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {   ## check if mean is already calculated and cached
    message("getting cached data")
    return(m)
  }
  data <- x$get()  ## call the get function in makeVector to get the vector values
  m <- mean(data, ...) ## calculate the mean of the vector
  x$setmean(m)  ## call the setmean function in makeVector to get the set the value m to mean
  m ## return mean
}