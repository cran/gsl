"gsl_sf_sin" <- function(x, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  val <- x.vec
  err <- x.vec
  jj <- .C("sin_e",
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(val),
           err=as.double(err),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(x)
  err <- jj$err
  status <- jj$status
  attributes(err) <- attributes(x)
  attributes(status) <- attributes(x)

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
      return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
}  

"gsl_sf_cos" <- function(x, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  val <- x.vec
  err <- x.vec
  jj <- .C("cos_e",
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(val),
           err=as.double(err),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(x)
  err <- jj$err
  status <- jj$status
  attributes(err) <- attributes(x)
  attributes(status) <- attributes(x)

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
      return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
}  

