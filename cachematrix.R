## Computes the inverse of a square matrix. If matrix already
## exists the cached version is returned

## Set the matrix
## Get the matrix
## Set the matrix inverse
## Get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
        
}


## Gets a matrix (from makeCacheMatrix) and if matrix exists 
## it gets the cached version of the matrix and then the 
## inverse of the matrix is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
