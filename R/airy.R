"airy_Ai" <- function(x, mode=0, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  val <- x.vec
  err <- x.vec
  jj <- .C("airy_Ai_e",
           as.double(x.vec),
           as.integer(length(x.vec)),
           as.integer(mode),
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

"airy_Bi" <- function(x, mode=0, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  val <- x.vec
  err <- x.vec
  jj <- .C("airy_Bi_e",
           as.double(x.vec),
           as.integer(length(x.vec)),
           as.integer(mode),
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

"airy_Ai_scaled" <- function(x, mode=0, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  val <- x.vec
  err <- x.vec
  jj <- .C("airy_Ai_scaled_e",
           as.double(x.vec),
           as.integer(length(x.vec)),
           as.integer(mode),
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

"airy_Bi_scaled" <- function(x, mode=0, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  val <- x.vec
  err <- x.vec
  jj <- .C("airy_Bi_scaled_e",
           as.double(x.vec),
           as.integer(length(x.vec)),
           as.integer(mode),
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

"airy_Ai_deriv" <- function(x, mode=0, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  val <- x.vec
  err <- x.vec
  jj <- .C("airy_Ai_deriv_e",
           as.double(x.vec),
           as.integer(length(x.vec)),
           as.integer(mode),
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

"airy_Bi_deriv" <- function(x, mode=0, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  val <- x.vec
  err <- x.vec
  jj <- .C("airy_Bi_deriv_e",
           as.double(x.vec),
           as.integer(length(x.vec)),
           as.integer(mode),
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

"airy_Ai_deriv_scaled" <- function(x, mode=0, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  val <- x.vec
  err <- x.vec
  jj <- .C("airy_Ai_deriv_scaled_e",
           as.double(x.vec),
           as.integer(length(x.vec)),
           as.integer(mode),
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

"airy_Bi_deriv_scaled" <- function(x, mode=0, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  val <- x.vec
  err <- x.vec
  jj <- .C("airy_Bi_deriv_scaled_e",
           as.double(x.vec),
           as.integer(length(x.vec)),
           as.integer(mode),
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


"airy_zero_Ai" <- function(n,give=FALSE, strict=TRUE){
  n.vec <- as.vector(n)
  jj <- .C("airy_zero_Ai_e",
           as.integer(n.vec),
           as.integer(length(n.vec)),
           val=as.double(n.vec),
           err=as.double(n.vec),
           status=seq(along=n.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(n)
  status <- jj$status
  attributes(status) <- attributes(n)
  
  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
    return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
} 


"airy_zero_Bi" <- function(n,give=FALSE, strict=TRUE){
  n.vec <- as.vector(n)
  jj <- .C("airy_zero_Bi_e",
           as.integer(n.vec),
           as.integer(length(n.vec)),
           val=as.double(n.vec),
           err=as.double(n.vec),
           status=seq(along=n.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(n)
  status <- jj$status
  attributes(status) <- attributes(n)
  
  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
    return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
} 


"airy_zero_Ai_deriv" <- function(n,give=FALSE, strict=TRUE){
  n.vec <- as.vector(n)
  jj <- .C("airy_zero_Ai_deriv_e",
           as.integer(n.vec),
           as.integer(length(n.vec)),
           val=as.double(n.vec),
           err=as.double(n.vec),
           status=seq(along=n.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(n)
  status <- jj$status
  attributes(status) <- attributes(n)
  
  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
    return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
} 


"airy_zero_Bi_deriv" <- function(n,give=FALSE, strict=TRUE){
  n.vec <- as.vector(n)
  jj <- .C("airy_zero_Bi_deriv_e",
           as.integer(n.vec),
           as.integer(length(n.vec)),
           val=as.double(n.vec),
           err=as.double(n.vec),
           status=seq(along=n.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(n)
  status <- jj$status
  attributes(status) <- attributes(n)
  
  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
    return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
} 


