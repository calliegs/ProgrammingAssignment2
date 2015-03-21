## makeCacheMatrix sets up the initial matrix and the functions set and get for
## makeCacheMatrix and cacheSolve are intended to cache the value
## of a matrix and its inverse to save lengthy computation

## makeCacheMatrix sets up the initial matrix and the functions set and get for
## setting and getting the values of the cached matrices and setinv and getinv for 
## setting and getting the inverse matrices
makeCacheMatrix <- function(x = matrix()) {
    stored_inv <- NULL      ## store the inverse within the makeCacheMatrix function
    ## set function defined within makeCacheMatrix
    set <- function(y) {
      x <<- y  ## ensures the value of x is available 
      ## outside of set function in the makeCacheMatrix function
      ## sets x to the required value y from the set function call
      stored_inv <<- NULL ## sets stored_inv as null for use outside set function in parent environment
    }
    get <- function() return(x) ## returns current value of x
    setinv <- function(chosen_inv) stored_inv <<- chosen_inv
    ## sets value of stored_inv as inverse of matrix as passed in function call
    getinv <- function() return(stored_inv) 
    ## returns value of stored_inv - the stored inverse matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
 ## returns hierarchical list of functions and variables defined in makeCacheMatrix space
}


## cacheSolve is for calculating the inverse of a matrix that is returned
## from makeCacheMatrix

cacheSolve <- function(cache, ...) {
        ## Return a matrix that is the inverse of 'x'
  cached_inv <- cache$getinv() ## from the get function above - gets the cached inverse
  ## and stores it as cached_inv
  message("Checking cache...")
  ## if there is a cached inverse already then prints message to say so
  if(!is.null(cached_inv)) { ## if there's a matrix in the cache that is not null
    message("Getting previously cached value.")
    return(cached_inv) ## returns inverse matrix stored in cache
  } else { ## otherwise sets inverse and prints it
    cached_matrix<-cache$get() ## get the matrix to be cached from the function call and store it
    cached_inv<-solve(cached_matrix) ## calculates the inverse of the matrix and stores it as cached_inv
    return(cached_inv) ## returns the inverse matrix
  }
message("Finished.")
}
