#function makeCacheMatrix takes a matrix and creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL             #inv will be used to store cached matrix
        set <- function(y) {    #create four functions to set and get matrix and its inverse
                x <<- y         # <<- accesses the object in the parent environment
                inv <<- NULL    # <<- forces the assignment to an object in the parent environment
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv) #return list of 4 functions
}
#function cacheSolve looks for the matrix inverse in cache
#if inverse is found in cache, the inverse is returned
#if not found in cache, calculate inverse, store it in cache and return it
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {  #check to see if cached matrix exists
                message("getting cached matrix")
                return(inv)  #return the cached matrix
        }
        data <- x$get()      #no cached matrix found, so run solve to get the inverse
        inv <- solve(data)   #run solve function to calculate the matrix inverse
        x$setinv(inv)        #put matrix in cache so it can be used next time
        inv                  #return the inverse found by function solve
}
