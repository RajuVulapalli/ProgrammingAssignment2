## Matrix inversion is usually a costly computation.  
## Catching the inverse may have some benfit rather than computing repetedly
## The following two functions are used to cache the inverse of a matrix

## Assumption is that the matrix supplied is always invertible
## makeCacheMatrix creates a list containing a function to
## set the values of the matix
## get the values of the matix
## set the values of inverse of the matrix
## get the values of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize the inverse property
  inv <- NULL
  set <- function(y) {
        x <<- y
        inv <<- NULL
  }
  ##method to get matrix
  get <- function() x
  ## setting the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  ##getting the inverse of matrix
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The following function first checks if the inverse has already been
## computed and returns the inverse of matrix.  If inverse does not
## exists the function computes the inverse, sets the value in the cache
## using setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  ## checking to see if inverse exists
  if(!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  matrixdata <- x$get()
  inv <- solve(matrixdata)
  x$setinverse(inv)
  inv
        
}

##Test run
##> x = rbind(c(3, 1/6), c(1/6, 3))
##> m = makeCacheMatrix(x)
##> m$get()
## Matrix
##[,1]      [,2]
##[1,] 3.0000000 0.1666667
##[2,] 0.1666667 3.0000000

##  No cache in the first run
##> cacheSolve(m)
##[,1]        [,2]
##[1,]  0.33436533 -0.01857585
##[2,] -0.01857585  0.33436533
## Retrieving from the cache in the second run
##> cacheSolve(m)
##Getting Cached Data
##[,1]        [,2]
##[1,]  0.33436533 -0.01857585
##[2,] -0.01857585  0.33436533
##> 