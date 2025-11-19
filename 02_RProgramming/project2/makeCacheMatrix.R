makeCacheMatrix <- function(x = numeric()) {
        #     setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\r programming\\project2")
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(ginv) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}