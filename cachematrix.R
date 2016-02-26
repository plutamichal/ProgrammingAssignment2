## Function: makeCacheMatrix  create matrix
## additional functions: 
##      setinverse - create inverse of the matrix and store inverse in m
##      getinverse - return inverse matrix m
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # set values of matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get matrix
        get <- function() x
        # set inverse of a matrix
        setinverse <- function(solve) m <<- solve
        # get inverse of a matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
            getinverse = getinverse)
}

## Function: cacheSolve - function return inverse of given matrix x
## if inverse of given matrix already exists,  previously calculated and saved inverse is returned
## if inverse does not exist, inverse is calculated and stored using x$setinverse function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting inverse of a matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


