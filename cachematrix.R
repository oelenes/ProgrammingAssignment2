## The following two functions demonstrates how to cache the inverse 
## of a matrix in R language. The caching is beneficial when calculating 
## the invers of the same matrix is required repetitively.

## The makeCacheMatrix function creates a matrix object that caches its 
## invers. This function set/get the matrix and set/get the inverse of the
## matrix using solve function.
makeCacheMatrix <- function(x = matrix()) 
{
  	m <- NULL

  	set <- function(y)
	{
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

## The cacheSolve function calculates the invers of a matrix created with 
## makeCacheMatrix function. It first checks to see if the invers has 
## previously been calculated. In this case gets the inverse from the cache 
## and skips the computation. If not, calculates the inverse and  sets the 
## value of the inverse.
cacheSolve <- function(x = matrix(), ...) 
{
	m <- x$getinverse()

    	if(!is.null(m))
	{
      	message("getting cached data")
      	return(m)
    	}

    	matrix <- x$get()
    	m <- solve(matrix, ...)
    	x$setinverse(m)
    	m
}