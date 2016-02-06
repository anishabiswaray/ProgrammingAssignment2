## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function.
##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL 
}
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function.
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
#Example:1

#> m <- makeCacheMatrix(matrix(5:8,2,2))

#> m$get()

#[,1] [,2]
#[1,]    5    7
#[2,]    6    8

#> m$getinverse()

#NULL

#> cacheSolve(m)

#[,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5

#> cacheSolve(m)

#getting cached data.

#[,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5

#> m$getinverse()

#[,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.53

#Example :2

#> m$set(matrix(c(15,36,41,5),2,2))

#> m$get()

#[,1] [,2]
#[1,]   15   41
#[2,]   36    5

#> m$getinverse()

#NULL

#> cacheSolve(m)

#[,1]        [,2]
#[1,] -0.003568879  0.02926481
#[2,]  0.025695931 -0.01070664

#> cacheSolve(m)

#getting cached data.

#[,1]        [,2]
#[1,] -0.003568879  0.02926481
#[2,]  0.025695931 -0.01070664

#> m$getinverse()

#[,1]        [,2]
#[1,] -0.003568879  0.02926481
#[2,]  0.025695931 -0.01070664
