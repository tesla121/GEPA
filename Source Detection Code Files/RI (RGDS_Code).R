#Give paths to input underlying graphs and infection graphs
#Set the working directory

file_list_LIG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/")

library(igraph)

#Code for Reverse Infection (RI)

RI_FB_Ht_2=list()
JC_sum=0
i=1
shift_list_estimated=c()
while(i<=length(file_list_LIG)){
	load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/",file_list_LIG[i],sep=''))

	source=V(g)[1]$label

	sg=g

id=list()
id2=list()

k=1
while(k<=length(V(sg))){

	id[[k]]=V(sg)[k]
	k=k+1
}

id2=list()
id2=id
stop=0

while(stop==0){
k=1
	while(k<=length(V(sg))) {
	neighbors=list()
	neighbors=unlist(adjacent_vertices(sg,k))
	for(n in neighbors){

		id2[[n]]=unique(append(id2[[n]],id[[k]]))

	}
	k=k+1

}
id=id2


id=unique(id2)

j=1
while(j<=length(V(sg))) {
if(length(id[[j]])==length(V(sg))){
	stop=1
}
j=j+1
}

}

JCS=which(max(lengths(id))==lengths(id))
clo=closeness(sg, vids = JCS, mode = c( "all"),weights = NULL, normalized = TRUE)
index=which(max(clo)==clo)

	estimated_JC=V(sg)[as.numeric(JCS[index[1]])]$label

	d_JC=distances(sg, v = (estimated_JC), to = (source), mode = c("all"), weights=NULL, algorithm = c("unweighted"))

	samp=sample(length(d_JC),1)

	JC_sum=JC_sum+as.numeric(d_JC[samp])
	RI_FB_Ht_2[[i]]=as.numeric(d_JC[samp])

	i=i+1

	centers=names(which(min(sort((eccentricity(sg, vids = V(sg), mode = c("all")))))==sort((eccentricity(sg, vids = V(sg), mode = c("all"))))))
	est_source=dimnames(d_JC)[[1]][samp]
	shift=distances(sg, v = as.character(centers), to = as.character(est_source) , mode = c("all"), weights=NULL, algorithm = c("unweighted"))

	shift_list_estimated=append(shift_list_estimated,ceiling(min(shift)))

}


df=as.data.frame(cbind(file_list_LIG,unlist(RI_FB_Ht_2),shift_list_estimated))
colnames(df)=c('graph','RI','shift_list_estimated')


write.csv(df,"./Covid 19 Results RI.csv")
