#################################################
#                                               #
# MakeCacheMatrix does the following:           #
# sets the value of the matrix                  #
# gets the value of the matrix                  #
# sets the value of the matrix inverse          #
# gets the value of the matrix inverse          #
#                                               #
#################################################

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#################################################
#                                               #
# cacheSolve does the following:                #
# If the inverse of a matrix has been calcula-  #
# ted, it returns the cached value. Otherwise   #
# it calculates and returns the inverse of the  #
# matrix                                        # 
#                                               #
#################################################






cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data.")
                return(inver)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inver)
        inver
}









