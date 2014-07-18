## Bill Wolf 
## rprog-005 Assignment #2
## 7/15/14

## answer is based on course example provided at 
##https://class.coursera.org/rprog-005/human_grading/
##view/courses/972576/assessments/3/submissions

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                # variable to store cache
    set <- function(y) {     # define set
        x <<- y
        m <<- NULL
    }
    get <- function() x      # define  get
    setinv <- function(solve) m <<- solve   #define setinv
    getinv <- function() m                  #define getinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSmolve retrieves the inverse 
## from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()   #first check if cache is set
    if(!is.null(m)) {  #if so return it
        message("getting cached data")
        return(m)
    }
    data <- x$get()    # if not calculate it 
    m <- solve(data, ...)
    x$setinv(m)        # and set cahce for next time
    m
}




