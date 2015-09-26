## cache matrix inverse to avoid re-compute 
## USAGE:
## >source("cachematrix.R")
## >m1<-matrix(rnorm(16, mean=10, sd=1), 4, 4)
## >mc<-makeCacheMatrix()
## >mc$set(m1)
## >mc$get()
## >cacheSolve(mc)
## >cacheSolve(mc)

## makeCacheMatrix is a list, which include: 
## mc<-makeCacheMatrix()
##	1. set -- mc$set(matrix), set the input matrix y to x, clean inverse cache 
##  2. get -- mc$get(), get the cached matrix x, which is set before
##  Notice: cacheSolve(mc), solve x and cache it
##  3. getinverse -- mc$getinverse
##  4. setinverse -- mc$setinverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		if (!(class(y) == "matrix")) {
			message("Please input square matrix")
			return(NULL)
		}

		if (dim(y)[1] != dim(y)[2]) {
			message("Please input square matrix")
			return(NULL)
		}

		x <<- y
		inverse <<- NULL
	}
            
	get <- function() x
	setinverse <- function(inverse_com) inverse <<- inverse_com
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)	
}

## try get inverse; if not, compute Matrix inversion, then cache;
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}
