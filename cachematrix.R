## makeCacheMatrix  creates a special "matrix" object that can cache its inverse.
## For this assignment, assume that the matrix supplied is always invertible.
## this means we do not have to determine if the determinant is 0.
# use the following as sample template:
# makeVector <- function(x = numeric()) {
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
# }

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse #change sample from MEAN to 
                                                   #INVERSE
        getinv <- function() m 
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

# cachemean <- function(x, ...) {
#         m <- x$getmean()
#         if(!is.null(m)) {
#                 message("getting cached data")
#                 return(m)
#         }
#         data <- x$get()
#         m <- mean(data, ...)
#         x$setmean(m)
#         m
# }

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
                 m <- x$getinv()
                 
                 #here, check to see if it exists
                 if(!is.null(m)) {
                         # get it and just return
                         message("found data")
                         message("getting cached data")
                         return(m)
                 }
                 
                 message("did NOT find data, calculating...")
                 #go calculate instead
                 data <- x$get()
                 m <- solve(data, ...)
                 
                 #use our set to set it too
                 x$setinv(m)
                 m
}
