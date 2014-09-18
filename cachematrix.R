## This pair of functions perform the following:
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## cacheSolve: Computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the
## cacheSolve will return the inverse from the cache.

## Function "makeCacheMatrix" creates a special "matrix", which is really a list containing a functions to:
## set the value of the Matrix
## get the value of the Matrix
## set the value of the Inverse of matrix
## get the value of the Inverse of matrix

## example useage : 
## amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## amatrix$get()         # Returns original matrix
## cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
## amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will return the inverse from the cache.
##Example useage: See header, must first create matrix with makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Get Matrix "m" from Cache
        m <- x$getinverse()
        ## Check if not null, read from Cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
  }
  ## if "m" is NULL, get original Matrix
  data <- x$get()
  ## Calculate inverse using Solve
  m <- solve(data, ...)
  x$setinverse(m)
  ## Return inverse
  m
}