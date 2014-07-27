#makeCacheMatrix:
#Input: either a matrix or empty argument to just create the object
#Description:
#   initializes cached.inverse to NULL
#   defines function 'set' to change (matrix) values and reset cached.inverse to NULL
#   defines function 'get' to return values
#   defines function 'setinverse' to cache the inverse 
#       (at some point found using base 'solve' function)
#   defines function 'getinverse' to return current cached inverse


makeCacheMatrix <- function(data = matrix()) {
    cached.inverse <- NULL

    set <- function(new.values) {
        data            <<- new.values
        cached.inverse  <<- NULL
    }

    get <- function() data

    setinverse <- function(inverse) cached.inverse <<- inverse

    getinverse <- function() cached.inverse

    list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

########################################################################
#cacheSolve:

#Input:
#   cached.matrix: an object created by 'makeCacheMatrix'

#Description:
#   fetches the cached inverse
#   if this is not NULL, it has already been calculated and cached
#       and so the cached value is returned
#   if this is NULL (not previously calculated),
#       the matrix values are fetched and passed to the base R
#       function 'solve' to calculate the inverse,
#       which is then cached using setinverse().
#   The cached inverse (whether previously cached or newly
#       calculated) is then returned

cacheSolve <- function(cached.matrix, ...) {
        ## Return a matrix that is the inverse of 'cached.matrix'
        cached.inverse <- cached.matrix$getinverse()

        if (!is.null(cached.inverse)) {
            message("getting cached inverse")
            return(cached.inverse)
        }

        data            <- cached.matrix$get()      #get data
        cached.inverse  <- solve(data, ...)         #calculate inverse
        cached.matrix$setinverse(cached.inverse)    #update cached inverse
        return(cached.inverse)                      #return inverse
}
