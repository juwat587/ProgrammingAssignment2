## My functions are created for Programming Assaignment 2. 

## makeCacheMatrix creates a matrix object that is a cache of the inverse

makeCacheMatrix <- function(x = matrix()) {
        f <- NULL
        set <- function(j) {
                x <<- j
                f <<- NULL
        }
        get <- function() x
        settheinverse <- function(inverse) f <<- inverse
        gettheinverse <- function() f
        list(set = set, get = get,
             settheinverse = settheinverse,gettheinverse = gettheinverse)
}


## cacheSolve creates the inverse of the makeCacheMatrix function. If the inverse is calculate, than it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  f <- x$gettheinverse()
      if(!is.null(f)) {
            message("The Cached data is being retrieved")
            return(f)
      }
      getdata <- x$get()
      f <- solve(getdata, ...)
      x$settheinverse(f)
      f
}
