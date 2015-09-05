rm(list=ls())
name = "Thomas Zhang"
liuid = "thozh912"

sheldon_game<-function(player1, player2){
  a=c("spock","rock","scissors","lizard","paper")
  i <- 0
  j <- 0
  for (x in a){
    if(x == player1){
      leftval<-i
    }
    if(x == player2){
      rightval<-j
    }
    i <-i+1
    j<-j+1
  }
  diff <- (leftval - rightval) %% 5
  if(diff == 0){
    return("Draw!")
  }
  else if(diff >= 3){
    return("Player 1 wins!")
  }
  else{
    return("Player 2 wins!")
  }
}

sheldon_game("rock","paper")

my_moving_median<-function(x,n,...){
  stopifnot(is.numeric(x) & is.numeric(n) & is.vector(x) & length(n) == 1)
  resvect <- rep(0,length(x)-n)
  
  for(i in 1:(length(x)-n)){
    truncvect <- x[i:(i+n)]
    res <- median(truncvect,...)
    resvect[i] <-res
  }
  return(resvect) 
}

my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2,na.rm=TRUE)

for_mult_table<-function(from,to){
  stopifnot(is.numeric(from) & is.numeric(to) & length(from) == 1 & length(to) == 1 )
  multi <- matrix(0,to-from + 1,to-from + 1)
  for( i in from:to){
    for(j in from:to){
      multi[i + 1 - from,j + 1 - from]<- i*j
    }
  }
  rownames(multi)<-from:to
  colnames(multi)<-from:to
  return(multi)
}
for_mult_table(1,5)

cor_matrix<-function(X){
  stopifnot(is.data.frame(X) )
  matr<-matrix(0,length(X),length(X))
  for(i in 1:length(X)){
    for(j in 1:length(X)){
      covariance<-mean((X[,i]-mean(X[,i])) * (X[,j]-mean(X[,j])))
      isd<-sqrt(mean(X[,i]^2) - mean(X[,i])^2)
      jsd<-sqrt(mean(X[,j]^2) - mean(X[,j])^2)
      matr[i,j]<-covariance/(isd * jsd)
    }
  }
  return(matr)
}
data(iris)
data(faithful)
cor_matrix(iris[,1:4])


find_cumsum<-function(x,find_sum){
  stopifnot(is.numeric(x) & is.numeric(find_sum))
  cumsum<-0;
  i<-1;
  while(cumsum <= find_sum & i <= length(x)){
    cumsum <- cumsum + x[i]
    i<- i + 1
  }
  return(cumsum)
}
find_cumsum(c(1:10),1000)

while_mult_table<-function(from,to){
  stopifnot(is.numeric(from) & is.numeric(to) & length(from) == 1 & length(to) == 1 )
  multi <- matrix(0,to-from + 1,to-from + 1)
  fromto <- from:to
  i<-1
  j<-1
  while(i <= to - from + 1){
    while(j <= to - from + 1){
      multi[i,j]<- fromto[i]*fromto[j]
      j <-j + 1  
    }
    i <-i + 1
    j <- 1
  }
  rownames(multi)<-from:to
  colnames(multi)<-from:to
  return(multi)
}

while_mult_table(0,10)

trial_division_factorization<-function(x){
  factvec <-c()
  factlist<-rep(TRUE,floor(sqrt(x)))
  i<-2;
  j<-2;
  while(i <= floor(sqrt(x))){
    while(j <= floor(sqrt(x))/i){
      factlist[i*j]<- FALSE
      j <- j + 1
    }
    i<-i + 1
    j<-i
  }
  numberlist <-1:floor(sqrt(x))
  numberlist <-numberlist[factlist]
  
  #print(factlist)
  #print(numberlist)
  
  if(x == 1 | x ==2 | x==3){
    factvec <-length(numberlist)
    return(as.numeric(factvec))
  }
  
  pointer <-2;
  while(numberlist[pointer] <= sqrt(x)){
    
    if( numberlist[pointer] == sqrt(x)){
      factvec <- c(factvec,numberlist[pointer],numberlist[pointer])
      return(as.numeric(factvec))
    }
    
    if(x %% numberlist[pointer] == 0){
      factvec <- c(factvec,numberlist[pointer])
      x <- x/numberlist[pointer]
      pointer <- 2
      next
    }
    
    
    pointer <- pointer + 1
  }
  factvec<-c(factvec,x)
  return(as.numeric(factvec))
}

trial_division_factorization(323)





repeat_find_cumsum<-function(x,find_sum){
  stopifnot(is.numeric(x) & is.numeric(find_sum))
  cumsum<-0;
  i<-1;
  repeat{
    cumsum <- cumsum + x[i]
    i<- i + 1
    if(i>length(x)){
      break
    }
    if(cumsum > find_sum){
      break
    }
  }
  return(cumsum)  
}
repeat_find_cumsum(c(1:100),500)

repeat_my_moving_median<-function(x,n,...){
  stopifnot(is.numeric(x) & is.numeric(n) & is.vector(x) & length(n) == 1)
  resvect <- rep(0,length(x)-n)
  i<-1
  repeat{
    if(i>(length(x)-n)){
      break
    }
    truncvect <- x[i:(i+n)]
    res <- median(truncvect,...)
    resvect[i] <-res
    i <-i + 1
  }
  
  return(resvect)
}

repeat_my_moving_median(5:15,4)

in_environment<-function(env){
  environ<-as.environment(env)
  text<-ls(environ)
  return(text)
}

funs<-in_environment("package:graphics")
head(funs)

where<-function(fun){
  stopifnot(is.character(fun) & length(fun) == 1)
  searchpath<-search()
  for(i in 1:length(searchpath)){
    env<-searchpath[i]
    for(j in in_environment(env)){
      if(fun == j ){
        return(env)
      }
    }
  }
  nofound <- paste(fun,"not found!")
  return(nofound)
  
}

where("madeup")

cov<-function(X){
  stopifnot(is.data.frame(X))
  vectlist<-lapply(X,function(y) sd(y)/mean(y))
  vect<-unlist(vectlist)
  return(vect)  
}

cov(iris[3:4])

moment<-function(i){
  stopifnot(is.numeric(i) & length(i) == 1)
  function(x){
    res <- mean((x - mean(x))^i)
    return(res)
  }
  
}

h<-1:100
m1<-moment(1)
m2<-moment(2)

m1(h)
m2(h)

mcmc_counter_factory<-function(burnin,thin){
  stopifnot( burnin >= 0 & thin > 0)
  iteration <- 0
  samples <- 0
  
  function(){  
  
  store_sample <- FALSE
  
  iteration <<- iteration  + 1
  if( (iteration - burnin) > 0 & (iteration - burnin) %% thin == 0){
    store_sample <- TRUE
    samples <<- samples + 1
  }
  
  iterlist <-list(iteration,store_sample,samples)
  return(iterlist)
  }
}

mcmccnt<-mcmc_counter_factory(3,2)

mcmccnt()
mcmccnt()
mcmccnt()
mcmccnt()
mcmccnt()
mcmccnt()
mcmccnt()
mcmccnt()


