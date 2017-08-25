##Make cache matrix gives a list of 4 functions that can set or get the value of a matrix, or set or get the inverse of 
##a matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,  ##Lists the four function options
             setInverse = setInverse,
             getInverse = getInverse)
}
##cacheSolve first searches for a cached version of the inverse of the matrix if it is available.
##If no cached version is available, it calculates the inverse of the matrix
cacheSolve <- function(x, ...) {
        m <- x$getInverse()  ##This functions gets the cached inverse if availablle
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get() ##obtains value of matrix
        m <- solve(data, ...) ##Computes the inverse
        x$setInverse(m) ##Caches the calculated inverse
        m
}
