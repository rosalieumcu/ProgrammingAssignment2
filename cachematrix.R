## These functions calculate the inverse of a given matrix x, but first check if the
## inverse is already 'living' in the cache memory. If the inverse of matrix x has 
## already been calculated, the function returns the inverse matrix from the cache memory
## rather than calculating it again.

## makeCacheMatrix returns a list of functions. It does not calculate anything, but 
## rather 'stores' what is assigned to functions in it's environment. Here, 'm' is the 
## inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve uses the functions inside makeCacheMatrix. In the first line, m is assigned
## the getinv function. If m is NOT null, that means inside makeCacheMatrix a value for 
## m is stored. The message "getting cached data" is displayed and m is returned. If 
## m is NULL, this means there is no cached value for m. The inverse matrix of x has to 
## be calculated and is then assigned to m, after which m is returned.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
