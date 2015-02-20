## This set of functions calculate inverse matrix for given matrix
## the inverse matrix is stored in cache, so if we want to get inverse
## matrix again, function returns already calculated inverse matrix.
## 

## First function is the one we use to initialize new matrix "object"
## in this "object" we have several "methods":
##
## set - assigns new matrix and resets inverse matrix to NULL
## get - returns matrix
## setinversematrix - calculates inverse matrix
## getinversematrix - returns inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversematrix <- function(solve) m <<- solve
    getinversematrix <- function() m
    list (set = set,
          get = get,
          setinversematrix = setinversematrix,
          getinversematrix = getinversematrix)
    
}


## Second function is the one we use to actually execute "methods" from
## matrix "object" initialized by first function. It tries to get inverted
## matrix by calling "get" method; if it is stored in cache it returns it
## with proper announcement
## if it returns NULL this function calculates inverse matrix
## and executes## "setinversematrix" "method" 
## to store it in cache; then returns just calculated inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinversematrix()
    if(!is.null(m)){
        message("getting cashed data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversematrix(m)
    m   
    
}

## test code:

a = matrix(rnorm(4),2,2)
c = makeCacheMatrix(a)
cacheSolve(c)
cacheSolve(c)
b = matrix(rnorm(9),3,3)
c = makeCacheMatrix(b)
cacheSolve(c)
c = makeCacheMatrix(a)
cacheSolve(c)