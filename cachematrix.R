## Based on Professor Robert Peng's example "getmean" code, the following code 
## functions to create, cache and retrieve the inverse of an invertible matrix. 

## The first function "makeCacheMatrix" processes the initial matrix input "x", inverts it 
## using "solve", then caches it.

makeCacheMatrix <- function(x = matrix()) {      ## setting up the function for matrix "x"
  inv <- NULL                                    ## creates NULL "inv" object, locally
  set <- function(y) {                           ## nested "function(y)" assigned to "set", locally
    x <<- y                                      ## in a separate environment, assigned 
    inv <<- NULL                                 ## "y" to "x" and "inv" to NULL
  }
  get <- function() x                            ## assigns "function() x" to "get" (includes matrix(x))
  setsolve <- function(solve) inv <<- solve      ## solves for inverse "inv" and caches 
  getsolve <- function() inv                     ## function to retrieve inverse "inv" from cache
  list(set = set, get = get,                     ## returns values for "set", "get", 
       setsolve = setsolve,                      ##"setsolve" and "getsolve"
       getsolve = getsolve)
}

a <- makeCacheMatrix(x)     ## Prior to running the second function, it is necessary to 
## assign makeCacheMatrix(x) to "a", which makes the output a 
## list, to avoid the error "$ operator is invalid for atomic 
## vectors". Many thanks for Kelvin Ting for this idea. Attempt 
## to use [] rather than $ resulted in "cannot find 'getsolve'"

## The second function "cacheSolve" is fed "a". If the inverse is 
## already cached, it will retrieve it, otherwise it will calculate it and return it.

cacheSolve <- function(a, ...) {                 ## setting up function for "a" from makeCacheMatrix
  inv <- a$getsolve()                            ## locate inverse and assign to "inv",locally
  if(!is.null(inv)) {                            ## if "inv", is not NULL locally (i.e. has been  
    message("getting cached data")               ## evaluated already) locates and returns  
    return(inv)
  }
  data <- a$get()                                ## if NULL, data from "a" (originally, 
  inv <- solve(data, ...)                        ## matrix(x)) is  pulled (get) and evaluated 
  a$setsolve(inv)                                ## for the inverse and cached
  inv                                            ## Returns inverse matrix of 'x'
}      
