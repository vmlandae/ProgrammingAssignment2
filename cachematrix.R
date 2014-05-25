

makeCacheMatrix <- function(x = matrix()) {
    ##this function sets a matrix into a list of subfunctions in the same way 
    ##that the makevector function
    m <- NULL  ##cache matrix
    set <- function(y) { ##set the matrix into this new object
        x <<- y
        m <<- NULL
    }
    get <- function() x ##returns the new object setted from the original matrix
    setinv <- function(solve) m <<- solve ##to calculate the inverse of the matrix
    getinv <- function() m ##get the inverse matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
cacheSolve <- function(x, ...) {
    
    m <- x$getinv()
    if(!is.null(m)) { ##if inverse matrix is in cache, returns inverse from there
        message("getting cached data") ##useful to know if the program take this path
        return(m)
    }
    data <- x$get() ##asign the original matrix
    m <- solve(data) ##calculate inverse matrix
    message("calculating new data") ##to know we are calculating 
    x$setinv(m) ##setting the inverse matrix into the m cache
    m
    
    
}
