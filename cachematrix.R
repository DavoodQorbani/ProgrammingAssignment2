## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse.
## (We assume that the matrix supplied is always invertible.)


## The first function creates a special "matrix" object that can cache its inverse.
## Example:
## For the sake of fun, run the following in your R console:
## > rpeng <- makeCacheMatrix(matrix(1:4, 2, 2))
## (This is an example. You can change "rpeng" or the content of the square matrix as you wish!)
## Till now, the inverse has not been calculated. To make sure, you can run the following:
## > rpeng$getSolve()
## As you see, you get a matrix with NA values.

makeCacheMatrix <- function(x = matrix()) {
        s <- matrix(NA, nrow(x), nrow(x))
        set <- function(y) {
                x <<- y
                s <<- matrix(NA, nrow(x), nrow(x))
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

## Example (Continued):
## Run the following:
## > cacheSolve(rpeng)
## You will see the inverse.
## If you run the above command again (it means that you have not changed the matrix),
## you will see this message: "getting cached data" and the cached inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!anyNA(s)) {
                message("getting cached data")
                return(s)
                invisible(x)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}