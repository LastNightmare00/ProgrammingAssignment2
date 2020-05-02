##Programming assignment week 3

##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
##rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
##Here we write a pair of functions that cache the inverse of a matrix.

#1)makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix())
{

         inv <- NULL
         set_matrix <- function(y)
        	{
	        x <<- y
	        inv <<- NULL
	        }
         get <- function()x
         setInverse <- function(inverse) inv <<- inverse
         getInverse <- function() inv
         list(set_matrix = set_matrix,get = get,setInverse = setInverse,getInverse = getInverse)
}

#2)cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...)
{
         a <- x$getInverse()
         if(!is.null(a))
	 {
	  message("taking from cached data")
	  return(a)
	 }
         #if not cached ,then 
         mat <- x$get()
         i <- solve(mat,...)
         x$setInverse(i)
         i   
         ## Return a matrix that is the inverse of 'x'
}

#after putting a new matrix,intially calling getInverse() will return NULL
#you need to call cacheSolve to set it, after if you call again ,
#then cached data is shown

