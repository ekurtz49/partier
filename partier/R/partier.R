#' City Block Calculation
#'
#' This helper function calculates the city block (i.e. Manhattan or L1) distance between two points.
#' This distance is simply the sum of all distances of the individual dimensions.
#' @param x n-dimensional vector representing first point, or coordinate, in n-space
#' @param y n-dimensional vector representing second point, or coordinate, in n-space
#' @keywords city block, manhattan, L1, taxicab
#' @export
#' @examples
#' ## Find the L1 distance between (1,2) and (4,6) in 2d space.
#' plot(c(1,4),c(2,6)) # visually, L1 distance should be 6
#' cityblock(c(1,2),c(4,6))
#'
#' ## Find the L1 distance between two points in 4d space
#' cityblock(c(1,1,1,1),c(3,3,3,3))
#'
cityblock<-function(x,y){
  if(length(x)!=length(y)) stop('x and y must have same length')
  return(sum(abs(x-y))) #difference between the two vectors, elementwise, then elements all summed
}












#' Duplicated Minimum Detection
#'
#' This helper function detects whether a given vector has a unique minimum value.
#' Returns a 0 (FALSE) if the minimum value in the vector is unique and a 1 (TRUE) if minimum is not unique.
#' @param x vector
#' @keywords duplicate
#' @export
#' @examples
#' ## Detect whether a vector contains a unique minimum
#' duplicate_minima(c(1,2,3)) # visually does (1 is unique)
#' duplicate_minima(c(1,1,3)) # visually does not (1 is not unique, appears twice)
#'
duplicate_minima<-function(x){
  return(sum(x==min(x))>1) #check whether each element equals the minimum of the vector,
  #sum up all Ts (or 1s), and if greater than 0, return 1 (or T) because there is a duplicated minimum
}













#' All possible party distances
#'
#' This function generates distances between all possible n-dimensioned tuples and all inputed centers and determines "closest" centers.
#' For example, for two dimensions on a scale from 1 to 10, there are 100 possible 2d tuples. We could have, say, two centers,
#' one at (2,4) and the other at (6,8) (see example). A possible tuple could be (1,1). This function calculates and records the distance from
#' (1,1) to both (2,4) and (6,8) and also determines that (2,4) is closest and records that.
#' @param policies matrix representing parties' policy positions, with positions being rows and parties being columns
#' @param scale.lower number representing lower bound for scale that all dimensions are measured on
#' @param scale.upper number representing upper bound for scale that all dimensions are measured on
#' @keywords distances
#' @export
#' @examples
#' party1<-c(2,4)
#' party2<-c(6,8)
#' partypolicies<-matrix(c(party1,party2),nrow=2)
#' coordinate_distances(partypolicies)
coordinate_distances<-function(policies,scale.lower=1,scale.upper=10){
  numpolicies=nrow(policies) #since rows of policies matrix are policy positions, nrow of matrix is number of policy positions
  numparties=ncol(policies) #columns are parties, so ncol is number of parties

  if(sum(policies>scale.upper)>0) stop('policies must not exceed scale.upper')
  if(sum(policies<scale.lower)>0) stop('policies cannot be lower than scale.lower')

  # policies<-round(policies) # i suppose no harm in removing this row
  # I am going to initialize a matrix called partyData that will ultimately be returned by this function.
  # This matrix will initially have c columns, where c is the sum of the number of parties and of policy dimensions being considered.
  # This matrix will have r rows, where r is the number of discrete points on the scale used, raised to the power of the number of policy dimensions.
  # In the first numpolicies columns of this matrix, all possible coordinates for policy positions will be enumerated.
  # In the last numparties columns of this matrix, the distances between the proposed policy positions and the given parties will be partyDataed.
  # Ultimately, one more column will be added; this last column will summarize which party is "closest" to the proposed positions, for each row.
  partyData<-matrix(-1,ncol=numpolicies+numparties,nrow=(scale.upper-scale.lower+1)^numpolicies)
  partyData.rows<-nrow(partyData)

  repfactor<-1 # ease of enumeration of possible positions in first numpolicies columns
  for(i in 1:numpolicies){ # this loop's only purpose is to populate the possible positions in the first numpolicies columns
    partyData[,i]<-rep(scale.lower:scale.upper,each=repfactor,length=partyData.rows) #repeat literal integers "scale.lower" to "scale.upper"
    # do it to fill the whole column (of length partyData.rows), and repeat an integer repfactor times before moving onto next integer
    repfactor<-repfactor*scale.upper #update repfactor for next column to fill in all possible coordinates
  }

  for(j in 1:numparties){ #this loop's purpose is to fill in distances from the row's given potential coordinates to each party in order
    partyData[,numpolicies+j]<-apply(abs(t(partyData[,1:numpolicies]) - policies[,j]),2,sum) # sum up elementwise the distances between potential coordinates and party's position
  }
  partyData<-cbind(partyData,rep(0,nrow(partyData))) #create one new column at the end of partyData matrix to partyData "closest" party

  for(i in 1:nrow(partyData)){ #this loop's purpose is to determine the closest party for each row in partyData (i.e each possible coordinate)
    if(duplicate_minima(as.vector(partyData[i,(numpolicies+1):(numpolicies+numparties)]))==1){ #if there is no unique minimum
      partyData[i,numparties+numpolicies+1]<-0 # 0 will denote a "tie" among two or more parties
    } else {
      partyData[i,numparties+numpolicies+1]<-which.min(partyData[i,(numpolicies+1):(numpolicies+numparties)]) #subset matrix into vector, give place of closest element, which denotes closest party
    }
  }

  partyData<-list("matrix"=partyData,"originalMatrix"=policies) #enable more than just the ultimate partyData matrix to be viewed
  class(partyData)<-append("PartyMatrix",class(partyData)) #set a "partymatrix" class to enable creation of generic versions of summary, plot, etc. functions
  colnames(partyData$matrix)<-c(paste("Dimension",c(1:numpolicies)),paste("Distance from Party",c(1:numparties)),"Closest Party")
  return(partyData)
}














#' Plotting Party Territories
#'
#' This function plots parties' "territories" based on inputted party platforms
#' @param partyData object created by coordinate_distances
#' @param col an array of colors, one for each party, to be used to show parties' territories. Defaults to "auto" which automatically creates color scheme
#' @param dimensions an array of integers specifying which dimensions a plot is desired for (corresponding to the desired rows of the matrix inputted into the coordinate_distances function). Defaults to "all"
#' @param scale.lower number representing lower bound for scale that all dimensions are measured on
#' @param scale.upper number representing upper bound for scale that all dimensions are measured on
#' @param ... Arguments to be passed to methods. See ?plot for more information. (NEEDS TO BE FIXED)
#' @keywords cityblock
#' @export
#' @examples
#' party1<-c(2,4)
#' party2<-c(6,8)
#' partypolicies<-matrix(c(party1,party2),nrow=2)
#' partyData<-coordinate_distances(partypolicies)
#' plot(partyData)
plot.PartyMatrix<-function(partyData,col="auto",dimensions="all",scale.lower=1,scale.upper=10,...){
  if(class(partyData)[1]!="PartyMatrix") stop("Should only use this function to plot PartyMatrix xs")
  if(any(dimensions=="all")){
    policies<-partyData$originalMatrix
  }else{
    policies<-partyData$originalMatrix[dimensions,] #parties' original policies, but only using the ones user specifies
  }
  ndim<-nrow(policies) #if one dimensional, is just array, so need length
  #message(ndim)
  #message(dimensions)
  #scale.lower<-min(partyData$matrix[,1])
  #scale.upper<-max(partyData$matrix[,2])
  #if col argument is auto, just fill in a default here
  if(any(col=="auto")){
    col<-grDevices::rainbow(ncol(policies))
  } #otherwise use the user-specified colors
  if(length(ndim)==0){
    plot1d(partyPos=policies,col,scale.lower,scale.upper) #WHAT IS PARTYPOS
  }else if(ndim==2){
    plot2d(partyPos=policies,col,dimensions=dimensions,scale.lower=scale.lower,scale.upper=scale.upper) #WHAT IS PARTYDATA
  } else{
    #message(scale.upper)
    plot_matrix(partyPos=policies,col,scale.lower,scale.upper,...) #if ==3, do plot3d? give option between this and plot3d maybe
  }
}










#' Plotting Party Territories on One Dimension
#'
#' This helper function plots parties' "territories" on one dimension based on inputted party platforms
#' @param partyPos vector of party positions on one dimension
#' @param colorList an array of colors, one for each party, to be used to show parties' territories. Defaults to "auto" which automatically creates color scheme
#' @param scale.lower number representing lower bound for scale that all dimensions are measured on
#' @param scale.upper number representing upper bound for scale that all dimensions are measured on
#' @param dimensions character string describing the name of the dimension
#' @param ... Arguments to be passed to methods. See ?plot for more information. (FIXED LATER)
#' @keywords cityblock
#' @export
#' @examples
#' plot1d(c(1,5,3,8,6),colorList=c("red","green","blue","purple","orange"))
plot1d<-function(partyPos,colorList="auto",scale.lower=1,scale.upper=10,dimensions="Dimension 1",...){
  dataForBarPlot=c(0)
  colorsForBarPlot=c("#888888")
  prevMin=0
  temppartyPos=partyPos
  if(any(colorList=="auto")){
    colorList<-grDevices::rainbow(length(partyPos))
  }
  for (zone in 1:length(temppartyPos)) {
    currMin=min(temppartyPos, na.rm=TRUE)
    smallestIndex=which(temppartyPos==currMin)
    if(length(smallestIndex)>1) {
      currWinner=0
      currColor="#888888"
    }
    else {
      currWinner=smallestIndex
      currColor=colorList[currWinner]
    }
    if (prevMin==0) {
      dataForBarPlot[1]=currMin
      colorsForBarPlot[1]=currColor
    }
    else{
      changeInMin=currMin-prevMin
      dataForBarPlot[zone-1]=dataForBarPlot[zone-1] + (changeInMin/2)

      dataForBarPlot[zone] = (changeInMin/2)
      colorsForBarPlot[zone] = currColor
    }
    prevMin = currMin
    temppartyPos[temppartyPos==currMin] = NA

    if (sum(is.na(temppartyPos)) == length(temppartyPos)) {
      dataForBarPlot[zone] = dataForBarPlot[zone] + (scale.upper-sum(dataForBarPlot))
      break
    }
  }
  dataForBarPlot = t(dataForBarPlot)
  dataForBarPlot = t(dataForBarPlot)
  barplot(dataForBarPlot, horiz=TRUE, col = colorsForBarPlot,xlim=c(scale.lower,scale.upper),xpd=FALSE,xlab=dimensions)
  axis(side=1,at=c(scale.lower:scale.upper))
  #stripchart(partyPos, pch = 15,add=TRUE)
  points(partyPos,rep(.75,length(partyPos)),pch=15)
}














#' Plotting Party Territories on Two Dimensions
#'
#' This function creates a 2d plot of party territories, with one dimension represented on the x axis and the other on the y axis
#' @param partyPos matrix of party positions, with dimensions represented by rows and parties represented by columns
#' @param colorList an array of colors, one for each party, to be used to show parties' territories. Defaults to "auto" which automatically creates color scheme
#' @param scale.lower number representing lower bound for scale that all dimensions are measured on
#' @param scale.upper number representing upper bound for scale that all dimensions are measured on
#' @param dimensions character string array describing the names of the dimensions
#' @param ... Arguments to be passed to methods. See ?plot for more information.
#' @keywords cityblock
#' @export
#' @examples
#' party1<-c(2,4)
#' party2<-c(6,8)
#' partypolicies<-matrix(c(party1,party2),nrow=2)
#' plot2d(partypolicies)
plot2d<-function(partyPos,colorList="auto",scale.lower=1,scale.upper=10,dimensions=c("Dimension 1","Dimension 2")){
  partyData2d<-coordinate_distances(partyPos,scale.lower=scale.lower,scale.upper=scale.upper)
  #scale.lower<-min(partyData2d$matrix[,1])
  #scale.upper<-max(partyData2d$matrix[,1])
  if(any(colorList=="auto")){
    colorList<-grDevices::rainbow(ncol(partyPos))
  }
  if(0 %in% partyData2d$matrix[,ncol(partyData2d$matrix)]){
    colorList<-c("black",colorList)
  } #else keep colorList as is
  image(x=scale.lower:scale.upper,y=scale.lower:scale.upper,z=matrix(partyData2d$matrix[,ncol(partyData2d$matrix)],nrow=scale.upper-scale.lower+1,byrow=TRUE),
        col=colorList,xaxt="n",yaxt="n",xlab=dimensions[2],ylab=dimensions[1])
  axis(1,at=(scale.lower:scale.upper),labels=scale.lower:scale.upper)
  axis(2,at=(scale.lower:scale.upper),labels=scale.lower:scale.upper)
  #then make dots of parties' positions on top
  #points(x=partyData2d$originalMatrix[1,]/max(partyData2d$matrix[,1]),y=partyData2d$originalMatrix[2,]/max(partyData2d$matrix[,1]))
  points(x=partyData2d$originalMatrix[2,],y=partyData2d$originalMatrix[1,],pch=15)
}









#' Plotting Territory Matrix
#'
#' This function creates a matrix of party territory plots based on the desired dimensions. Main diagonal plots are created using plot1d, off diagonal plots are created by plot2d
#' @param partyPos matrix of party positions, with dimensions represented by rows and parties represented by columns
#' @param col an array of colors, one for each party, to be used to show parties' territories. Defaults to "auto" which automatically creates color scheme
#' @param scale.lower number representing lower bound for scale that all dimensions are measured on
#' @param scale.upper number representing upper bound for scale that all dimensions are measured on
#' @param ... Arguments to be passed to methods. See ?plot for more information.
#' @param ... Arguments to be passed to methods. See ?plot for more information.
#' @keywords cityblock
#' @export
#' @examples
#' party1<-c(1,2,4)
#' party2<-c(6,5,9)
#' partypolicies<-matrix(c(party1,party2),nrow=3)
#' plot_matrix(partypolicies)
plot_matrix<-function(partyPos,col="auto",scale.lower=1,scale.upper=10,...){
  #message(scale.upper)
  graphics::par(mfrow=c(nrow(partyPos),nrow(partyPos)))
  if(any(col=="auto")){
    col<-grDevices::rainbow(ncol(partyPos))
  } #otherwise use the user-specified colors
  for(i in 1:nrow(partyPos)){
    for(j in 1:nrow(partyPos)){
      if(i==j){
        plot1d(partyPos=partyPos[i,],colorList=col,scale.lower=scale.lower,scale.upper=scale.upper,dimensions=paste("Dimension",i))
      } else{
        plot2d(partyPos[c(i,j),],colorList=col,dimensions=c(paste("Dimension",c(i,j))),scale.lower=scale.lower,scale.upper=scale.upper)
      }
    }
  }
  par(mfrow=c(1,1))
}











#' Summarizing parties' territories
#'
#' This function gives a well organized summary of parties' territories, including total territory counts for each party and descriptive statistics of the distances to each territory box for each party
#' @param object matrix output from coordinate_distances
#' @param ... Arguments to be passed to methods. See ?summary for more information.
#' @keywords summary
#' @export
#' @examples
#' party1<-c(2,4)
#' party2<-c(6,8)
#' partypolicies<-matrix(c(party1,party2),nrow=2)
#' object<-coordinate_distances(partypolicies)
#' summary(object)
summary.PartyMatrix<-function(object,...){
  winner<-summary(as.factor(object$matrix[,ncol(object$matrix)]))
  numpolicies<-nrow(object$originalMatrix)
  distances<-summary(object$matrix[,(numpolicies+1):(ncol(object$matrix)-1)],...)
  my_list<-list("distances"=distances,"territory_totals"=winner)
  my_list
}











#' Generating total vote outcome summary
#'
#' This function adds a probabilistic component and generates voting outcomes for randomly generated (by sim_voter_positions) voters.
#' @param matrix output from sim_voter_positions, or another matrix with voters being rows and policy positions columns
#' @param loss either "absolute" or "squared" used for probability generation based on distances
#' @param partypos matrix of party positions with positions as rows, parties as columns
#' @keywords generate, simulate, election, outcomes
#' @export
#' @examples
#'
sim_vote<-function(matrix,loss="absolute",partypos){
  ifelse(loss=="absolute",power<-1,power<-2)
  distances<-matrix(rep(-1,nrow(matrix)*ncol(partypos)),nrow=nrow(matrix))
  outcome<-rep(-1,nrow(matrix))
  for(i in 1:nrow(matrix)){
    for(j in 1:ncol(partypos)){
      distances[i,j]<-sum(abs(matrix[i,]-partypos[,j]))
    }
  }
  probs<-(1/distances)^power
  for(i in 1:nrow(matrix)){
    outcome[i]<-which(rmultinom(1,1,probs[i,])==1)
  }
  return(outcome)
}










#' Generating voters
#'
#' This function generates voters based on desired policy dimensions, scales, and underlying distributions of opinions.
#' @param num number of voters generated
#' @param range array of size 2 containing upper and lower bound of policy positions
#' @param ndim number of policy dimensions
#' @param shape1 array corresponding to alpha parameters for beta distributions of policies
#' @param shape2 array corresponding to beta parameters for beta distributions of policies
#' @param corrmat correlation matrix signifying how correlated dimensions should be. Main diagonal should be 1s
#' @param rounded if true, rounds voters' positions to nearest integer
#' @keywords generate, simulate, voters
#' @export
#' @examples
#' sim_voter_positions(10,c(1,20),3)
#'
sim_voter_positions<-function(num,range=c(0,1),ndim,shape1,shape2,corrmat,rounded=FALSE){
  #check if range and distributions vectors are equal to ndim
  #could also just force everything to be beta, alpha=beta=5 is good normal approx
  if(length(range)!=2)stop('range array requires two numbers, an upper and lower bound')
  if(ndim!=length(shape1)|ndim!=length(shape2))stop('should specify shape1 and shape2 values for each dimension')

  #generate multivariate standard normals with corrmat correlations between variables
  mu<-rep(0,ndim)
  rawpositions<-MASS::mvrnorm(n=num, mu=mu, Sigma=corrmat)
  pvars <- pnorm(rawpositions)
  betavars<-matrix(rep(-1,num*ndim),nrow=num,ncol=ndim)
  for(i in 1:ndim){
    betavars[,i]<-qbeta(pvars[,i],shape1[i],shape2[i])
  }
  betavars<-betavars*(range[2]-range[1])+range[1]
  return(betavars)
}

