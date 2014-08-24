## Comments are provided in respective lines describing their purpose in brief

## makeCacheMatrix() creates a "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {             # x will be a matrix
    mi <- NULL             # will store the inverse of the matrix and reset to 
                           # NULL whenever makeCacheMatrix() function is called

    get <- function() x                     # gets the original matrix value
    
    setminverse <- function(solve) mi <<- solve  # called by cacheSolve()
                                             # will super assign to store value

    getminverse <- function() mi         # On subsequent access of same matrix,
    # the cached inverse matrix is returned
    
    list(get = get,                     # Lists all the functions that are part
         setminverse = setminverse,     # of the object. This returns along
         getminverse = getminverse)     # with the newly created object
}

## cacheSolve() computes the inverse of the "matrix" returned by
## makeCacheMatrix(). For a pre-calculated and unchanged "matrix" obect,
## function cacheSolve() retrieves the inverse from the cache

cacheSolve <- function(x, ...) {    # object created by 1st function serves as
                                    # input for this function
    mi <- x$getminverse()            # accesses the object by the list method
    
    if(!is.null(mi)) {              # check if inverse is already available
        message("getting cached value")  # text confirming availability
        return(mi)                  # returns the matrix inverse and exits
    }

    data <- x$get()                 # if inverse is not available, then goes
                                    # back to 1st function to get data
    mi <- solve(data, ...)          # calculates the inverse here and
    x$setminverse(mi)               # stores the calculated inverse in x
    mi                              # and returns the inverse of the matrix
}
