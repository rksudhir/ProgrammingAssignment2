###########################################################################################################################
## These two functions are helpful for any program in saving processing time required for computing inverse of a matrix, ##
## when the data of the matrix has not changed.                                                                          ##
###########################################################################################################################


############################################################################################################################
## This function takes an invertible numeric square matrix as its parameter. It creates a special structure at the current 
## environment's parent level, which is a list of four functions:
##    1) set(x) to set the parent env. variable to the value of the matrix parameter (x)
##    2) get() to return the parent env. variable
##    3) setInverse(I) to set the inverse (I) of the matrix in the parent environment
##    4) getInverse() to return the saved inverse matrix from the parent environment.
## Each time this function is called, the inverse matrix in the parent environment is set to null for that input matrix.
## If this function is called mutiple times with separate input matrices, it creates separate structures for each of the 
## input matrices in the parent environment, so that their inverse martices can be cached separately.
##--------------------------------------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){ x;}
  setInverse <- function(solve){ m <<- solve;}
  getInverse <- function() {m;}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

############################################################################################################################
## This function is used to cache the inverse of a numeric square matrix. When called to fetch the inverse back, it checks   
## if the inverse for the same matrix data is cached previously, and if so, returnes the cached inverse. If there is no  
## inverse matrix already cached, it recomputes the inverse and caches the inverse afresh and returns it.   
## If this function is called for different matrix data, it computes and caches the inverse separately for each martices.                                                                                                             ##
##--------------------------------------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
