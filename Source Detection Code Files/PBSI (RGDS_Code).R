#Give paths to input underlying graphs and infection graphs
#Set the working directory

library(igraph)
library(expm)

#Code for Label Propagation based Source Identification (pbsi) - Single Source

#Calculating S, the normalized weighted matrix of a graph, takes time if the graph size is large.
#We have already calculated S for each graph and saved it. Therefore S can be directly loaded.

#CODE TO CALCULATE S
#Code starts
"%^%" <- function(S, power) 
   with(eigen(S), vectors %*% (values^power * t(vectors))) 

# Load rumor underlying network files (Comment/uncomment for Beta=3 and Beta=4)
# (uncomment line below to execute for Beta=3)
# file_list_UG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks (Beta=3)/")
# Beta = 4 (comment out line below to execute for Beta=3)
file_list_UG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks (Beta=4)/")

# Load rumor infection network files (Common for both Beta=3 and Beta=4)
file_list_LIG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/")

alpha=0.5 #Constant throughout the network
pbsi_FB_Ht_2=list() 
pbsi_sum=0
shift_list_estimated=c()
k=1

while(k<=length(file_list_UG)){
# Load rumor underlying network (Comment/uncomment for Beta=3 and Beta=4)
# (uncomment line below to execute for Beta=3)
# load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks (Beta=3)/",file_list_UG[i],sep=''))
# Beta = 4 (comment out line below to execute for Beta=3)
load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks (Beta=4)/",file_list_UG[i],sep=''))

# Load rumor infection network (Common for both Beta=3 and Beta=4)
load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/",file_list_LIG[i],sep=''))

graph=ug
sg=g
W=as.matrix(get.adjacency(graph)) 
D=diag(rowSums(W))
S=(D%^%-1/2)%*%(W)%*%(D%^%-1/2)
#Code ends
A=as.matrix(get.adjacency(graph))


source=V(g)[1]$label

Y=c(rep(-1,length(V(graph))))
Y[c(V(g)$label)]=c(rep(1,length(V(g)))) 

df=NULL
df=as.data.frame(rbind(Y))

iter=2
while(iter<=10){ 
	G=c(rep(0,length(V(graph))))
	
	for(i in sample(1:length(V(graph)))) {
		summation=0
		nbr=as.numeric(which(A[i,]==1))

		for(j in nbr){
			summation=summation+unlist(S[i,j])*unlist(df[iter-1,j]) 
		}

		G[[i]]=alpha*summation+(1-alpha)*unlist(Y[i])
	}

	df=as.data.frame(rbind(df,unlist(G)))
	iter=iter+1
}

estimated=which(max(df[5,])==df[5,])
d_pbsi=distances(graph, v = estimated, to = source, mode = c("all"), weights=NULL, algorithm = c("unweighted"))

samp=sample(length(d_pbsi),1)

pbsi_sum=pbsi_sum+as.numeric(d_pbsi[samp])
pbsi_FB_Ht_2[[k]]=as.numeric(d_pbsi[samp])



k=k+1

centers=names(which(min(sort((eccentricity(sg, vids = V(sg), mode = c("all")))))==sort((eccentricity(sg, vids = V(sg), mode = c("all"))))))
	est_source=dimnames(d_pbsi)[[1]][samp]
	shift=distances(sg, v = as.character(centers), to = as.character(est_source) , mode = c("all"), weights=NULL, algorithm = c("unweighted"))

	shift_list_estimated=append(shift_list_estimated,ceiling(min(shift)))
}


df=as.data.frame(cbind(file_list_UG,unlist(pbsi_FB_Ht_2),shift_list_estimated))
colnames(df)=c('graph','PBSI','shift_list_estimated')

# Save results for Beta=3
# write.csv(df,"./Covid 19 Results PBSI (Beta=3).csv")

# Save results for Beta=4
write.csv(df,"./Covid 19 Results PBSI (Beta=4).csv")

