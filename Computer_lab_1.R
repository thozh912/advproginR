rm(list=ls())
name<-"Thomas Zhang"
liuid<-"thozh912"
data(iris)
data(faithful)

my_num_vector<-function(){
  a<-log10(11);
  b<-cos(pi/5);
  c<-exp(pi/3);
  d<-(1173 %% 7)/19;
  vec<-c(a,b,c,d)
  return(vec)
  
}
my_num_vector()

filter_my_vector<-function(x,leq){
  logi<- x<leq;
  x[!logi]<-NA;
  return(x)
}
filter_my_vector(x=c(1,2,3,4,5),leq=2)

dot_prod<-function(a,b){
  prod<-sum(a*b);
  return(prod)
}
dot_prod(a=c(1,2,3),b=c(0,0,34,9))

approx_e<-function(N){
  sum<-0;
  i<-0;
  while(i<N+1){
    sum<-sum+1/factorial(i);
    i<-i+1;
  }
  return(sum)
}
#e to 5 decimals is 2.71828
#N=9 is required to get the fifth decimal place right
approx_e(N=9)

my_magic_matrix<-function(){
  vect<-c(4,3,8,9,5,1,2,7,6)
  matr<-matrix(vect,ncol=3)
  return(matr)
}
my_magic_matrix()

calculate_elements<-function(A){
  attr<-attributes(A);
  elem<-as.double(attr$dim[1]*attr$dim[2]);
  return(elem)
}
calculate_elements(cbind(diag(3),my_magic_matrix()))

row_to_zero<-function(A,i){
  A[i,]<-0;
  return(A)
}
row_to_zero(my_magic_matrix(),2)

add_elements_to_matrix<-function(A,x,i,j){
  A[i,j]<-A[i,j] + x;
  return(A)
}
add_elements_to_matrix(diag(3),7,2,2)

my_magic_list<-function(){
  thelist<-list(info="my own list",my_num_vector(),my_magic_matrix())
  return(thelist)
}
my_magic_list()

change_info<-function(x,text){
  x$info<-text;
  return(x)
}
change_info(my_magic_list(),text = "Some new info")

add_note<-function(x,note){
  x$note <- note;
  return(x)
}
add_note(my_magic_list(),note = "This is a magic list!")

sum_numeric_parts<-function(x){
  sum<-0;
  i<-1;
  while(i < length(x)+1){
    if(is.numeric(x[[i]])){
      sum<-sum + sum(x[[i]]);
    }
    i<-i + 1;
  }
  return(sum)
}
sum_numeric_parts(my_magic_list())

my_data.frame<-function(){
  df<-data.frame(id=c(1,2,3),name=c("John","Lisa","Azra"),income=c(7.3,0,15.21),rich=c(FALSE,FALSE,TRUE));
  return(df)
}
my_data.frame()

sort_head<-function(df,var.name,n){
  i<-1;
  while(names(df[i]) != var.name){
    i<-i+1;
  }
  df<-df[order(df[,i],decreasing=TRUE),];
  return(head(df,n))
}

sort_head(iris,"Petal.Length",5)


add_median_variable<-function(df,j){
  i<-1;
  medi<-median(df[,j]);
  newvar<-df[,j]< medi;
  invvar<-df[,j]> medi;
  newvar[newvar == TRUE]<-"Smaller";
  newvar[invvar == TRUE]<-"Greater";
  newvar[newvar == TRUE | newvar == FALSE]<-"Median";
  res<-cbind(df,compared_to_median=newvar);
  return(res)
}

head(add_median_variable(df=faithful,1))

analyze_columns<-function(df,j){
  a<-df[,j[1]]
  b<-df[,j[2]]
  vect<-c(mean(a),median(a),sd(a))
  vecto<-c(mean(b),median(b),sd(b))
  names(vect)<-c("mean","median","sd")
  names(vecto)<-c("mean","median","sd")
  matr<-matrix(c(cor(a,a),cor(b,a),cor(a,b),cor(b,b)),ncol=2)
  rownames(matr)<-c(names(df[j[1]]),names(df[j[2]]))
  colnames(matr)<-c(names(df[j[1]]),names(df[j[2]]))
  final<-list(lel=vect,lol=vecto,correlation_matrix=matr)
  names(final)[1]<-names(df[j[1]])
  names(final)[2]<-names(df[j[2]])
  return(final)
}

analyze_columns(df =faithful,1:2)