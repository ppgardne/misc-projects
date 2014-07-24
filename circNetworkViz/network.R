#function:
# given some layered (partate) network data - draw a network diagram, illustrating the network as a series of concentric circles.
#R CMD BATCH --no-save  network.R

#File Formats (tab-delimited):
#file 1 (networkLayers.tsv):      node->layer-mapping    (weights) [the ordering in this file determines the node order on the circles]
#file 2 (networkConnections.tsv): node->node-connections (weights) []

# layer 1 inner circle, layer 2, next, ...
# order of points determined by file 1. Points are equidistant, on the circle. 
# The rotation of the outer circles is determined by minimising the total edge lengths.

##TOP100
## cat          ../top100.network | awk '{print $3}' | sort -d | uniq -c | sort -nr | perl -lane '$F[1]=~s/Terminator\d/Terminator/; print "$F[1]\t1"' | uniq > networkLayers.dat
## cat          ../top100.network | awk '{print $2}' | sort -d | uniq -c | sort -r  | awk '{print $2"\t2"}' >> networkLayers.dat
## cat          ../top100.network | cut -f 2,3,5 | perl -lane '$_=~s/Terminator\d/Terminator/; print "$_"'  > networkConnections.dat

##ALL
## cat          ../miscellany/rfam/*network | awk '{print $3}' | sort -d | uniq -c | sort -nr | perl -lane '$F[1]=~s/Terminator\d/Terminator/; print "$F[1]\t1"' | uniq > networkLayers.dat
## cat          ../miscellany/rfam/*network | awk '{print $2}' | sort -d | uniq -c | sort -r  | awk '{print $2"\t2"}' >> networkLayers.dat
## cat          ../miscellany/rfam/*network | perl -lane '$F[2]=~s/Terminator\d/Terminator/; print "$F[1]\t$F[2]\t$F[4]"' > networkConnections.dat




######################################################################
#DEFINE FUNCTIONS:

#computeXY: function returning (x,y) coordinates given a layer, number of nodes and angle
computeXY <- function(layer, nodes, angle){
  XY<-mat.or.vec(nodes, 3)
  colnames(XY)<-c('X','Y', 'T')
  for (i in seq(0,nodes-1)){
    XY[i+1,'X'] <- layer*cos( ((i*2*pi)/nodes + angle)%%(2*pi))
    XY[i+1,'Y'] <- layer*sin( ((i*2*pi)/nodes + angle)%%(2*pi))
    XY[i+1,'T'] <-            ((i*2*pi)/nodes + angle)%%(2*pi)
  }  
  return(XY)
}

#optimiseRotation: return circle coordinates such that the total network distance is approximately minimal
optimiseRotation<-function(layer, numNodes, oldCircle, networkConnections){
  minDistance<-numNodes*1000000
  optimalAngle<-0
  for(i in seq(0,2*pi, length = 60)){
    newCircle<-computeXY(layer,numNodes,i)
    distance<-computeTotalEdgeLength(newCircle, oldCircle, networkConnections)
    if(minDistance>distance){
      optimalAngle<-i
      minDistance<-distance
      optimalCircle<-newCircle
    }
  }
  print(optimalAngle);
  return(optimalCircle);
}

#computeTotalEdgeLength: does what it says on the tin.
computeTotalEdgeLength <- function(newCircle, oldCircle, networkConnections){

  totalDist<-0
  for(i in seq(1,length(networkConnections$V1))){
    node1<-networkConnections$V1[i]
    node2<-networkConnections$V2[i]
    if(      length(newCircle[rownames(newCircle)==node1,])>0  &&  length(oldCircle[rownames(oldCircle)==node2,])>0){
      x<-c(newCircle[rownames(newCircle)==node1,'X'], oldCircle[rownames(oldCircle)==node2,'X'])
      y<-c(newCircle[rownames(newCircle)==node1,'Y'], oldCircle[rownames(oldCircle)==node2,'Y'])
      totalDist<-totalDist+distance(x,y)
    }
    else if (length(newCircle[rownames(newCircle)==node2,])>0  &&  length(oldCircle[rownames(oldCircle)==node1,])>0){
      x<-c(  newCircle[rownames(newCircle)==node2,'X'], oldCircle[rownames(oldCircle)==node1,'X'])
      y<-c(  newCircle[rownames(newCircle)==node2,'Y'], oldCircle[rownames(oldCircle)==node1,'Y'])
      totalDist<-totalDist+distance(x,y)
    }
  }      
  return(totalDist);
}

#distance: given two 2D vectors, return the euclidean distance (dist() really should do this):
distance<-function(x,y){  
  return(sqrt( (x[1]-x[2])^2 + (y[1]-y[2])^2 ) )  
}

#plotConnections: given coordinates, plot lines between networked points
plotConnections <- function(innerCircle, outerCircle, networkConnections){
  
  for(i in seq(1,length(networkConnections$V1))){
    node1<-networkConnections$V1[i]
    node2<-networkConnections$V2[i]
    if(      length(innerCircle[rownames(innerCircle)==node1,])>0  &&  length(outerCircle[rownames(outerCircle)==node2,])>0){
      x<-c(innerCircle[rownames(innerCircle)==node1,'X'], outerCircle[rownames(outerCircle)==node2,'X'])
      y<-c(innerCircle[rownames(innerCircle)==node1,'Y'], outerCircle[rownames(outerCircle)==node2,'Y'])
      lines(x,y,col="black",lwd=2)
    }
    else if (length(innerCircle[rownames(innerCircle)==node2,])>0  &&  length(outerCircle[rownames(outerCircle)==node1,])>0){
      x<-c(  innerCircle[rownames(innerCircle)==node2,'X'], outerCircle[rownames(outerCircle)==node1,'X'])
      y<-c(  innerCircle[rownames(innerCircle)==node2,'Y'], outerCircle[rownames(outerCircle)==node1,'Y'])
      lines(x,y,col="black",lwd=2)
    }
  }
}


#annotateNodes: label nodes
annotateNodes <- function(radius,angle,name,offset,inner){
  deg<-rad2deg(angle)
  adj<-c(0,0);
  if(inner == 1){
    if( deg<=90){
      adj<-c(1,0);
      deg<-deg; 
    } else if( deg<=180 ){
      adj<-c(0,1);
      deg<-(deg+180)%%360; 
    } else if( deg<=270 ){
      adj<-c(0,1);
      deg<-deg %% 180; 
    } else if( deg<=360 ){
      adj<-c(1,0);
    }
  }
  else {
    if( deg<=90){
      adj<-c(0,1);
      deg<-deg; 
    } else if( deg<=180 ){
      adj<-c(1,0);
      deg<-(deg+180)%%360; 
    } else if( deg<=270 ){
      adj<-c(1,0);
      deg<-deg %% 180; 
    } else if( deg<=360 ){
      adj<-c(0,1);
    }
  }
  text((radius+offset)*cos(angle),(radius+offset)*sin(angle),cex=1.25,name, srt=(deg), adj=adj, offset=1.0, font=2)  
}

rad2deg <- function(angle){
  angle <- angle %% (2*pi);
  angle <- 360*angle/(2*pi)
  return(angle)
}

deg2rad <- function(angle){
  angle <- angle %% (360);
  angle <- 2*pi*angle/(360)
  return(angle)
}

#FUNCTION DEFINITIONS END!
######################################################################

networkLayers      <-read.table("networkLayers.tsv",              header = F, sep = "\t")
networkConnections <-read.table("networkConnections.tsv",         header = F, sep = "\t")

#initialise:
numLayers<-max(networkLayers$V2)
numInnerNodes<-length(networkLayers$V2[networkLayers$V2==1])
innerNames<-as.vector(networkLayers$V1[networkLayers$V2==1])
innerCircle<-computeXY(1*15,numInnerNodes,0)
rownames(innerCircle)<-innerNames
border<-(numLayers+1)*15+5

pdf(file="network.pdf",  width=11, height=11)
plot(0,0,ylim=c(-1*border,border),xlim=c(-1*border,border),type='p',pch='.', cex=3, xaxt="n", yaxt="n",xlab="",ylab="",bty="n")

#Plot the innermost circle:
for(i in seq(1,numInnerNodes)){
  annotateNodes(1*15,innerCircle[innerNames[i],'T'],innerNames[i],-1,1)
}

for (j in seq(2,numLayers)){
  #compute coordinates and plot the outer circles:
  numNodes<-length(networkLayers$V2[networkLayers$V2==j])
  names<-as.vector(networkLayers$V1[networkLayers$V2==j])
  circle<-optimiseRotation(j*15,numNodes, innerCircle, networkConnections)
  rownames(circle)<-names  
  inner  <-  1
  offset <- -1
  if (j==numLayers){
    inner  <-  0
    offset <-  1
  }

  plotConnections(innerCircle,circle,networkConnections)
  points(innerCircle[,'X'],innerCircle[,'Y'], bg="red",pch=21,col="black",lwd=2,cex=1.5)
  for(i in seq(1,numNodes)){
    annotateNodes(j*15,circle[names[i],'T'],names[i],offset,inner)
  }
  innerCircle<-circle
}
points(innerCircle[,'X'],innerCircle[,'Y'], bg="red",pch=21,col="black",lwd=2,cex=1.5)
dev.off()

