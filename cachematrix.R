## The overall functions take a matrix input and stores it then looks to build a inverse
## the next time you want the inverse of that same matrix you can call the cached value
## instead of wasting time running the inverse again


## This function created stores a list of functions

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  ##initalize to NULL so the object exists
    get <- function() x  ##returns matrix stored in main function
    setInverse <- function(inv) i <<- inv ##stores value of inverse"
    getInverse <- function() i ## gives back the "inverse" value
    
    ##  stores the above 3 functions so we can assign the function
    ## an object and give it all 3 functions
    list(get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function verifies / calculates the matrix inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ##verify inverse exists
    i <- x$getInverse() ##get inverse from makeCacehMatrix function
    
    if (!is.null(i)) {   ## test the existance
        message("getting cached data")
        return(i) #return the existing "inverse"
    } else {
        ## when "inverse" doesn't exist
        data <- x$get()  ##make 'data' the original matrix
        i <- solve(data,...)  ## calculate the inverse using solve()
        x$setInverse(i)  ## give the setInverse function the inverse
        i  ##auto print the inverse
    }
}
