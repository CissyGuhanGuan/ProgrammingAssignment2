## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, makeVector creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	xi <- NULL
    set <- function(y) {
            x <<- matrix(y)
            xi <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) xi <<- inverse
    getinv <- function() xi
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

}


## Write a short comment describing this function

## The following function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the mean in the cache via the setmean function.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xi <- x$getinv()
        if( !is.null(xi) ){
        	message("getting cached data")
        	return(xi)
        }
        data <- x$get()
        xi <- solve(data, ...)
        x$setinv(xi)
        xi
        
}
