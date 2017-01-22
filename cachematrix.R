## Put comments here that give an overall description of what your
## functions do

## This function accepts an original matrix x and then creates a makeCacheMatrix object that includes data x and m and provides the set, get,
## getinverse, and setinverse functions. The data is stored in the parent environment so that it can be accessed by other functions, i.e. cached.
## See additional notes below.

makeCacheMatrix <- function(x = matrix()) {
        ## clears the m value
        m <- NULL
        ## defines the set function that puts the value y into the parent environment as x and nulls out the value m in the parent environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## defines the get function that returns x
        get <- function() x
        ## defines the set inverse function that will put the inverse matrix out to the parent environment as the cached value
        setinverse <- function(cacheMatrix) m <<- cacheMatrix
        ## defines the get inverse function that will return the value m which is the cached value
        getinverse <- function() m
        ## defines the list that is returned by the makeCacheMatrix function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function will check to see if cached data is available. If so it returns it. If not it calculates the inverse using the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ## calls the get inverse function from the makeCacheMatrix object
        m <- x$getinverse()
        ## if the value of m is not null, it means there is cached data and the function returns that data, i.e. m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## otherwise, we need to get the original matrix data contained in the makeCacheMatrix object and put that in the data variable
        data <- x$get()
        ## we now solve for the inverse of the matrix and store the inverse as object m
        m <- solve(data, ...)
        ## we now cache the matrix inverse using the set inverse function in the makeCacheMatrix object
        x$setinverse(m)
        ## we return m which is the calculated inverse matrix
        m
        
}
