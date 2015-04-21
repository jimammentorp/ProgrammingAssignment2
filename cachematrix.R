## Lexical Scoping

# This first function, makeCache creates a special "matrix" that can cache its inverse

# Function Arguments:
#
#  x: matrix - the matrix that should be cached
#
# Returned Values: is a list containing functions
# 
#  set: sets the value of the matrix
#  get: get the value of the matrix
#  setcache: set the value of the cache
#  getcache: get the value of the cache
#

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setcache <- function(cache) m <<- cache
    getcache <- function() m
    list(set = set, get = get,
         setcache = setcache,
         getcach = getcache)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated # (and the matrix has not changed)
# then the cachesolve should retrieve the inverse from the cache.
#
# Function Arguments:
#  cacheMatrix, ...
#
# Returned Values:
#  the inverse the matrix 
#

cacheSolve <- function(x, ...) {
    m <- x$getcache()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m    
}