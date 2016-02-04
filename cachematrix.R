### The function assign the matrix to x
### x can be accessed from the outside

makeCacheMatrix <- function(x = matrix()) {
				m<-NULL
   	     set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsol <- function(sol) m <<- sol 
        getsol <- function() m
        list(set = set, get = get,
             setsol = setsol,
             getsol = getsol)
}


## The function checks if there is an inverse for the matrix;
## If there is none, it computes the inverse and stores it to cache;
## If there is an inverse in cache, it takes the result from there.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsol()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsol(m)
        m
}
