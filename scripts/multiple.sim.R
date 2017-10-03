#' Title
#'
#' @details
#' This code implements the previous function (single.sim.nopb) multiple times in
#' order to estimate the percolation function. It also organizes the information
#' for an easily readable output.
#'
#' @param N nas
#' @param lambda nas
#' @param l nas
#' @param mu nas
#' @param kappa nas
#' @param progBar nas
#' @param experiment nas
#'
#' @return
#' @export
#'
#' @examples
multiple.sim<-function(N, lambda,l,mu, kappa, progBar=FALSE, experiment=TRUE){

  t<-proc.time()

  vertical.perco<-0

  horizontal.perco<-0

  multiple.vertical.clusters<-0

  multiple.horizontal.clusters<-0

  vertical.cluster.sizes<-vector()

  rel.vertical.cluster.sizes<-vector()

  horizontal.cluster.sizes<-vector()

  rel.horizontal.cluster.sizes<-vector()

  art.points<-vector()

  ver.connect<-vector()

  if(progBar) progress.bar <- txtProgressBar(min = 0, max = N, style = 3)

  for (i in 1:N){

    simulation=single.sim.nopb(lambda,l,mu,kappa)

    m=simulation[[1]]

    n=sum(m[,3])

    connect=simulation[[2]]

    rownames(m)=NULL
    colnames(m)=NULL

    vert.clust.num=sum(m[,1])
    hor.clust.num=sum(m[,2])

    if (vert.clust.num>0)   vertical.perco=vertical.perco+1

    if (hor.clust.num>0)    horizontal.perco=horizontal.perco+1

    if (vert.clust.num>1)   multiple.vertical.clusters=multiple.vertical.clusters+1

    if (hor.clust.num>1)    multiple.horizontal.clusters=multiple.horizontal.clusters+1

    vertical.cluster.sizes=append(vertical.cluster.sizes,m[which(m[,1]==1),3])

    rel.vertical.cluster.sizes=append(rel.vertical.cluster.sizes,m[which(m[,1]==1),3]/n)

    horizontal.cluster.sizes=append(horizontal.cluster.sizes,m[which(m[,2]==1),3])

    rel.horizontal.cluster.sizes=append(rel.horizontal.cluster.sizes,m[which(m[,2]==1),3]/n)

    art.points=append(art.points,connect[1])

    ver.connect=append(ver.connect,connect[2])

    if(progBar)setTxtProgressBar(progress.bar, i)


  }

  # To avoid NaNs:

  if(length(vertical.cluster.sizes)==0)vertical.cluster.sizes=0
  if(length(horizontal.cluster.sizes)==0)horizontal.cluster.sizes=0
  if(length(rel.vertical.cluster.sizes)==0)rel.vertical.cluster.sizes=0
  if(length(rel.horizontal.cluster.sizes)==0)rel.horizontal.cluster.sizes=0

  # Percolation Data

  perco.data<-matrix(c(
    vertical.perco/N, mean(vertical.cluster.sizes), var(vertical.cluster.sizes), mean(rel.vertical.cluster.sizes),
    horizontal.perco/N, mean(horizontal.cluster.sizes), var(horizontal.cluster.sizes), mean(rel.horizontal.cluster.sizes)),

    byrow=TRUE,ncol=4)

  if(!experiment){

    rownames(perco.data)=c("Vertical Percolation","Horizontal Percolation")
    colnames(perco.data)=c("    Estimated probability","   Mean size","   Size variance","   Relative size")}


  # Multiple Percolating Clusters Data:
  multiple.data<matrix(c(multiple.vertical.clusters/N,multiple.horizontal.clusters/N),byrow=TRUE,ncol=2)
  if(!experiment){
    colnames(multiple.data)=c("   Vertical","   Horizontal")
    rownames(multiple.data)=c("Estimated Probability")}


  # Connectivity Data
  connect.data<-matrix(c(round(mean(art.points)),var(art.points),mean(ver.connect),var(ver.connect)),ncol=4)

  if(!experiment){
    colnames(connect.data)=c("   Articulation Points","    Variance","    Vertex Connectivity", "   Variance")
    rownames(connect.data)=c("Means")}


  #Parameters Data

  parameters<-matrix(c(lambda,l,as.pi.fraction(mu),kappa),byrow=TRUE,ncol=4)
  if(!experiment){
    colnames(parameters)=c("  Intensity","  Length","  Mu","  Kappa")
    rownames(parameters)=" "}

  output<-list(parameters,proc.time()-t,perco.data,multiple.data,connect.data)

  if(!experiment){
    names(output)=c(" PARAMETERS","SIMULATION TIME","PERCOLATING CLUSTERS DATA","MULTIPLE PERCOLATING CLUSTERS","CONNECTIVITY DATA")}

  return(output)
}
