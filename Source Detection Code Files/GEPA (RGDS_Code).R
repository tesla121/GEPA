
l2=list()

temp <<- list()
level=0

score<-function(Du1,Di,l,level,temp,alpha){
	
	if(level>(radius(sg)-alpha)){
			return(0)
	}

	for(item in l){
		l2=append(l2,attributes(which(Ai[item,]==1))$names)

	}
	l2=unlist(l2)
	
	l3=setdiff(l2,l)
	l3=setdiff(l3,intersect(l3,temp))
	
	l3=(l3)
	scr=0
	if(length(l3)==0){
		return (0)
	}
	for(item in l){
		
		scr=scr+(Di[item,item]/Du1[item,item])/(1/(1+log(Du1[item,item]))) 
		
	}

	temp=unique(unlist(append(temp,l)))
	
	if(length(l3)==0){
		return (0)
	}
	else{
		return (scr+score(Du1,Di,l3,level+1,temp,alpha))
	}
}


# Load rumor underlying network files (Comment/uncomment for Beta=3 and Beta=4)
# (uncomment line below to execute for Beta=3)
# file_list_UG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks (Beta=3)/")
# Beta = 4 (comment out line below to execute for Beta=3)
file_list_UG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks (Beta=4)/")

# Load rumor infection network files (Common for both Beta=3 and Beta=4)
file_list_LIG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/")

library(igraph)

sys=0
est_sum=0
EPA_Random_Ht_Single=list()
i=1

shift_list_estimated=c()
shift_list_actual=c()
while(i<=length(file_list_UG)){

# Load rumor underlying network (Comment/uncomment for Beta=3 and Beta=4)
# (uncomment line below to execute for Beta=3)
# load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks (Beta=3)/",file_list_UG[i],sep=''))
# Beta = 4 (comment out line below to execute for Beta=3)
load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks (Beta=4)/",file_list_UG[i],sep=''))

# Load rumor infection network (Common for both Beta=3 and Beta=4)
load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/",file_list_LIG[i],sep=''))


graph=ug
Au=as.matrix(get.adjacency(graph))
Du=diag(rowSums(Au))
rownames(Du)=(V(graph)$label)
colnames(Du)=(V(graph)$label)
	
source=V(g)[1]$label
sg=g
radius=radius(sg)
nnodes<<-length(V(sg))
Ai<<-as.matrix(get.adjacency(sg))
rownames(Ai)=V(sg)$label
colnames(Ai)=V(sg)$label
Di=diag(rowSums(Ai))
colnames(Di)=(V(sg)$label)
rownames(Di)=(V(sg)$label)
Du1=Du[rownames(Di),colnames(Di)]
scr_list=list()
alpha=radius(sg)
alpha_iter=1
while(alpha>=0){

k=1

for(node in (V(sg)$label)){
	scr=score(Du1,Di,(node),level,temp,alpha)
	penalty=as.numeric(eccentricity(sg, vids = node, mode = c("all")))
	scr_list[k]=scr
	k=k+1
} 
scr_list=unlist(scr_list)
index=which(max(scr_list)==scr_list)

d=distances(sg, v = (V(sg)[index]$label), to = (source), mode = c("all"), weights=NULL, algorithm = c("unweighted"))

old_alpha=alpha
if(length(d)>1){
	alpha=alpha-1
}

if(alpha_iter>1){
	if(old_alpha==alpha){
		break
	}
}

alpha_iter=alpha_iter+1
}
#set.seed(1)
samp=sample(length(d),1)

est_sum=est_sum+as.numeric(d[samp])
EPA_Random_Ht_Single[[i]]=as.numeric(d[samp])

	i=i+1

	centers=names(which(min(sort((eccentricity(sg, vids = V(sg), mode = c("all")))))==sort((eccentricity(sg, vids = V(sg), mode = c("all"))))))
	est_source=dimnames(d)[[1]][samp]
	shift=distances(sg, v = as.character(centers), to = as.character(est_source) , mode = c("all"), weights=NULL, algorithm = c("unweighted"))
	shift_list_estimated=append(shift_list_estimated,ceiling(min(shift)))
	shift=distances(sg, v = as.character(centers), to = source , mode = c("all"), weights=NULL, algorithm = c("unweighted"))
	shift_list_actual=append(shift_list_actual,ceiling(min(shift)))

}
df=as.data.frame(cbind(file_list_UG,unlist(EPA_Random_Ht_Single),shift_list_actual,shift_list_estimated))
colnames(df)=c('graph','GEPA','shift_list_actual','shift_list_estimated')

# Save results for Beta=3
# write.csv(df,"./Covid 19 Results GEPA (Beta=3).csv")

# Save results for Beta=4
write.csv(df,"./Covid 19 Results GEPA (Beta=4).csv")
