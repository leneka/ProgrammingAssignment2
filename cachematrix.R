## Put comments here that give an overall description of what your
## functions do

## the function 'makeCacheMatrix' is to create a matrix

makeCacheMatrix <- function(x = matrix()) {
                i<-NULL
                set<-function(y){
                x<<-y
                i<<-NULL
        }
                get<-function() x
                setinverse<-function(inverse) i<<-inverse
                getinverse<-function() i
                list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## the function 'cacheSolve' is to return the inverse matrix of the matrix created before.

cacheSolve <- function(x, ...) {
                i<-x$getinverse()
                if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
                data<-x$get()
                i<-solve(data,...) ## Return a matrix that is the inverse of 'x'
                x$setinverse(i)
                i
}
