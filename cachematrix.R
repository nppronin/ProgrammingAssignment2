## The following pair of functions can be used to cache (and retrieve)
## an inverse of a given matrix to save time on future calculations

## This following function creates a special "matrix" object 
## that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) 
{
	inverse <- NULL	##at the start there is no inverted value
	set <- function(m) ##set the primary matrix
	{
		x <<- m
		inverted <<- NULL ## it is possible to store the matrix without its inverse, 
					## also when the matrix changes - the inverted value is reinitialized
	}
	get <- function() x
	
	cacheInverse <- function() inverse <<- solve(x) ##cache the inverted matrix
	getInverse <- function() inverse
	 list(set = set ,
       get = get,
       cacheInverse = cacheInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	if(is.null(inverse))	##if there is no cached value the new one is calculated and stored
	{
		x$cacheInverse()
		return(x$getInverse())
	}
	else ##if there was cached value it is returned
	{
		return(inverse)
	}
}
