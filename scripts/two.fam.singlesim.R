#' Title
#'
#' @param lambda1 nas
#' @param lambda2 nas
#' @param l1 nas
#' @param l2 nas
#' @param mu1 nas
#' @param mu2 nas
#' @param kappa1 nas
#' @param kappa2 nas
#' @param experiment nas
#'
#' @return
#' @export
#'
#' @examples
lambda1 <- 4; lambda2 <- 9
l1 <- 2; l2 <- 3
mu1 <- pi/2; mu2 <- 0
kappa1 <- 10; kappa2 <- 10
two.fam.singlesim<-function(lambda1,lambda2,l1,l2,mu1,mu2,kappa1,kappa2,experiment=TRUE){

  n1<-rpois(1,lambda1) # number of fractures
  n2<-rpois(1,lambda2)

  locs1<-matrix(c(runif(n1,0,1),runif(n1,0,1)),ncol=2) # midpoint locations
  locs2<-matrix(c(runif(n2,0,1),runif(n2,0,1)),ncol=2) # midpoint locations

  lengths1<-rep(l1,n1) # constant length vector
  lengths2<-rep(l2,n2) # constant length vector

  angles1<-rvonmises(n1,circular(mu1),kappa1) # angle vector
  angles2<-rvonmises(n2,circular(mu2),kappa2) # angle vector

  # Use the previous data to calculate the endpoints of every fracture:
  endpts1<-suppressWarnings(polar.to.rect(n1,locs1,lengths1,angles1))
  endpts2<-suppressWarnings(polar.to.rect(n2,locs2,lengths2,angles2))
  endpts.all<-suppressWarnings(polar.to.rect((n1+n2),rbind(locs1,locs2),cbind(lengths1,lengths2),c(angles1,angles2)))

  # Identify the fractures that intersect the boundary.
  # Edges for vertical percolation.
  lower.edge1<-which(endpts1[,2]<0|endpts1[,4]<0)
  upper.edge1<-which(endpts1[,2]>1|endpts1[,4]>1)

  lower.edge2<-which(endpts2[,2]<0|endpts2[,4]<0)
  upper.edge2<-which(endpts2[,2]>1|endpts2[,4]>1)

  lower.edge.all<-which(endpts.all[,2]<0|endpts.all[,4]<0)
  upper.edge.all<-which(endpts.all[,2]>1|endpts.all[,4]>1)


  # Edges for horizontal percolation.
  left.edge1<-which(endpts1[,1]<0|endpts1[,3]<0)
  right.edge1<-which(endpts1[,1]>1|endpts1[,3]>1)

  left.edge2<-which(endpts2[,1]<0|endpts2[,3]<0)
  right.edge2<-which(endpts2[,1]>1|endpts2[,3]>1)

  left.edge.all<-which(endpts.all[,1]<0|endpts.all[,3]<0)
  right.edge.all<-which(endpts.all[,1]>1|endpts.all[,3]>1)

  # Build the incidence matrix:
  in.mat1<-incidence.matrix.nopb(endpts1)
  in.mat2<-incidence.matrix.nopb(endpts2)
  in.mat.all<-incidence.matrix.nopb(endpts.all)

  # Feed the incidence matrix to igraph:
  G1<-graph.adjacency(in.mat1)
  G2<-graph.adjacency(in.mat2)
  G.all<-graph.adjacency(in.mat.all)

  # Use igraph's "cluster" function to find and store clusters:
  C1<-clusters(G1)
  clust1<-as.vector(clusters(G1)[[1]])
  clust.num1<-C1[[3]]

  C2<-clusters(G2)
  clust2<<-as.vector(clusters(G2)[[1]])
  clust.num2<<-C2[[3]]

  C.all<-clusters(G.all)
  clust.all<<-as.vector(clusters(G.all)[[1]])
  clust.num.all<<-C.all[[3]]

  # Create a matrix to store percolation information and cluster sizes.
  percolation.info1<-cbind(mat.or.vec(clust.num1,2),C1[[2]])
  colnames(percolation.info1)<-c("V","H","Size")
  rownames(percolation.info1)<-c(1:clust.num1)

  percolation.info2<-cbind(mat.or.vec(clust.num2,2),C2[[2]])
  colnames(percolation.info2)<-c("V","H","Size")
  rownames(percolation.info2)<-c(1:clust.num2)

  percolation.info.all<-cbind(mat.or.vec(clust.num.all,2),C.all[[2]])
  colnames(percolation.info.all)<-c("V","H","Size")
  rownames(percolation.info.all)<-c(1:clust.num.all)
  ##############################################################
  # Check for vertical percolation (FIRST FAMILY):
  if(length(left.edge1)>0||length(right.edge1)>0)
  {  for(i in 1:clust.num1)
  {   current.cluster=which(clust1==i)
  if(length(intersect(current.cluster,left.edge1))>0&&length(intersect(current.cluster,right.edge1))>0)percolation.info1[i,1]<-1
  }
  }

  #Check for horizontal percolation:
  if(length(lower.edge1)>0||length(upper.edge1)>0)
  {  for(i in 1:clust.num1)
  {   current.cluster=which(clust1==i)
  if(length(intersect(current.cluster,lower.edge1))>0&&length(intersect(current.cluster,upper.edge1))>0)percolation.info1[i,2]<-1
  }
  }
  ##############################################################
  # Check for vertical percolation (SECOND FAMILY):
  if(length(left.edge2)>0||length(right.edge2)>0)
  {  for(i in 1:clust.num2)
  {   current.cluster=which(clust2==i)
  if(length(intersect(current.cluster,left.edge2))>0&&length(intersect(current.cluster,right.edge2))>0)percolation.info2[i,1]<-1
  }
  }

  #Check for horizontal percolation:
  if(length(lower.edge2)>0||length(upper.edge2)>0)
  {  for(i in 1:clust.num2)
  {   current.cluster=which(clust2==i)
  if(length(intersect(current.cluster,lower.edge2))>0&&length(intersect(current.cluster,upper.edge2))>0)percolation.info2[i,2]<-1
  }
  }

  ##############################################################
  # Check for vertical percolation (SUPERIMPOSED FAMILY):
  if(length(left.edge.all)>0||length(right.edge.all)>0)
  {  for(i in 1:clust.num.all)
  {   current.cluster=which(clust.all==i)
  if(length(intersect(current.cluster,left.edge.all))>0&&length(intersect(current.cluster,right.edge.all))>0)percolation.info.all[i,1]<-1
  }
  }

  #Check for horizontal percolation:
  if(length(lower.edge.all)>0||length(upper.edge.all)>0)
  {  for(i in 1:clust.num.all)
  {   current.cluster=which(clust.all==i)
  if(length(intersect(current.cluster,lower.edge.all))>0&&length(intersect(current.cluster,upper.edge.all))>0)percolation.info.all[i,2]<-1
  }
  }

  ##############################################################
  # Add connectivity information:

  connectivity.info1<-c(length(articulation.points(G1)),graph.cohesion(G1))
  percolation.info1<-percolation.info1

  connectivity.info2<-c(length(articulation.points(G2)),graph.cohesion(G2))
  percolation.info2<-percolation.info2

  connectivity.info.all<-c(length(articulation.points(G.all)),graph.cohesion(G.all))
  percolation.info.all<-percolation.info.all

  ##############################################################
  # Output for FIRST FAMILY

  fam1.info<-c(0,0)
  vertical.perco1<-sum(percolation.info1[,1])
  horizontal.perco1<-sum(percolation.info1[,2])
  if(vertical.perco1>0)fam1.info[1]=1
  if(horizontal.perco1>0)fam1.info[2]=1
  fam1.info=append(fam1.info,max(percolation.info1[,3]))
  if(vertical.perco1==1||horizontal.perco1==1)fam1.info[3]=unique(c(percolation.info1[which(percolation.info1[,1]==1),3],percolation.info1[which(percolation.info1[,2]==1),3]))
  fam1.info=append(fam1.info,connectivity.info1)

  ##############################################################
  # Output for SECOND FAMILY

  fam2.info<-c(0,0)
  vertical.perco2<-sum(percolation.info2[,1])
  horizontal.perco2<-sum(percolation.info2[,2])
  if(vertical.perco2>0)fam2.info[1]=1
  if(horizontal.perco2>0)fam2.info[2]=1
  fam2.info=append(fam2.info,max(percolation.info2[,3]))
  if(vertical.perco2==1||horizontal.perco2==1)fam2.info[3]=unique(c(percolation.info2[which(percolation.info2[,1]==1),3],percolation.info2[which(percolation.info2[,2]==1),3]))
  fam2.info=append(fam2.info,connectivity.info2)

  ##############################################################
  # Output for SUPERIMPOSED FAMILY

  fam.all.info<-c(0,0)
  vertical.perco.all<-sum(percolation.info.all[,1])
  horizontal.perco.all<-sum(percolation.info.all[,2])
  if(vertical.perco.all>0)fam.all.info[1]=1
  if(horizontal.perco.all>0)fam.all.info[2]=1
  fam.all.info=append(fam.all.info,max(percolation.info.all[,3]))
  if(vertical.perco.all==1||horizontal.perco.all==1)fam.all.info[3]=unique(c(percolation.info.all[which(percolation.info.all[,1]==1),3],percolation.info.all[which(percolation.info.all[,2]==1),3] ))
  fam.all.info=append(fam.all.info,connectivity.info.all)

  lambda.all=lambda1+lambda2

  if(!experiment){
    output<-matrix(c(lambda1,l1,mu1,kappa1,fam1.info,lambda2,l2,mu2,kappa2,fam2.info,lambda.all,(l1+l2)/2,(mu1+mu2)/2,0,fam.all.info),byrow=TRUE,ncol=9)
    colnames(output)=c(" Lambda"," Length"," Mu"," Kappa"," Vertical Perco"," Horizontal Perco", " Cluster Size", " Articulation Points", " Vertex Connectivity")
    rownames(output)=c("1st Family", "2nd Family", "Superimposed Family")
  }

  else {
    output<-matrix(c(lambda1,l1,mu1,kappa1,fam1.info,lambda2,l2,mu2,kappa2,fam2.info,lambda.all,(l1+l2)/2,(mu1+mu2)/2,0,fam.all.info),byrow=TRUE,ncol=9)
  }
  return(output)
}
