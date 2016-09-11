# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.
>   makeCacheMatrix <- function(x = matrix()) {
+     inv <- NULL
+      set <- function(y) {
+          x <<- y
+          inv <<- NULL
+      }
+      get <- function() x
+      setinverse <- function(inverse) inv <<- inverse
+      getinverse <- function() inv
+      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
+  }
>  
# This function assumes that the matrix is always invertible.
>  cacheSolve <- function(x, ...) {
+      inv <- x$getinverse()
+      if(!is.null(inv)) {
+          message("getting cached data.")
+          return(inv)
+      }
+      data <- x$get()
+      inv <- solve(data)
+      x$setinverse(inv)
+      inv
+  }

# Sample Run
> x = rbind(c(4,3), c(3, 2))
>  m = makeCacheMatrix(x)
>  m$get()
     [,1] [,2]
[1,]    4    3
[2,]    3    2
> cacheSolve(m)
     [,1] [,2]
[1,]   -2    3
[2,]    3   -4
> cacheSolve(m)
getting cached data.
     [,1] [,2]
[1,]   -2    3
[2,]    3   -4
> 