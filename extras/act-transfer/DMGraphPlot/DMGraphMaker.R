# This R-script makes graphs of instructions in declarative memory
# In Lisp, run the function (print-net), this will create two files, vertices.txt and edges.txt


library(igraph)
library(stringr)
nodes <- read.table("vertices.txt",header=T)
edges <- read.table("edges.txt",header=T)

edges$weight <- ifelse(str_detect(str_c(edges$from,edges$to),"INSTR"),1,5)

g <- graph.data.frame(edges,directed=TRUE,vertices=unique(nodes))

tasks <- unique(c("CONDITION","ACTION",V(g)$task))
cl <- colors()[400:600]
randomcolors <- cl[trunc(runif(100,min=1,max=length(cl)))]
colors <- c("white","lightgray","green","red","yellow","orange","lightblue","salmon","cyan","blue","lightgreen","magenta","plum","hotpink2","seagreen","sienna","purple","lavender","yellowgreen",randomcolors)[1:length(tasks)]
df <-data.frame(tasks,colors)
rownames(df) <- df$tasks
V(g)$color <- colors[df[V(g)$task,]$color]
V(g)$color <- levels(df[V(g)$task,]$color)[df[V(g)$task,]$color]

# The exact plot depends on the number of vertices. Adjust the size of the document
# (width and height in the pdf command)
# Also adjustable is the vertex.size and the edge.width


# This is for lots of vertices
pdf(file="transferTask.pdf",width=50,height=50)
plot(g,layout=layout.fruchterman.reingold,vertex.size=4,edge.width=4)
legend("topright",legend=tasks,pch=15,col=colors,cex=2)
dev.off()

# Medium sized
pdf(file="transferTask.pdf",width=25,height=25)
plot(g,layout=layout.fruchterman.reingold,vertex.size=7,edge.width=4)
legend("topright",legend=tasks,pch=15,col=colors,cex=2)
dev.off()

# Small graph with big vertices
pdf(file="transferTask.pdf",width=10,height=10)
plot(g,layout=layout.fruchterman.reingold,vertex.size=18,edge.width=4)
legend("topright",legend=tasks,pch=15,col=colors,cex=1)
dev.off()

# For the really large graphs, it is better to removed the labels except the task names.
# Also, the weights are modified to make action and condition lists stick more together.
# This one was used for the final figure in the paper.

V(g)$label <- ifelse(V(g)$label %in% c("ED","EMACS","EDT","INDEX2-MIN-MINERAL","GREATER-ALGAE","TASK-SWITCHING-AB","SOLID-LIME-DIFF","PART-PLUS-MINERAL","COUNT-SPAN" ,"SINGLE-TASK-A","VERBAL-CWM","STROOP" ,"PROCEDURE-A","INDEX1-DIV-MARINE","MEAN-TOXIN"),V(g)$label,"")
V(g)$size <- ifelse(V(g)$label == "",3,10)

lay <- layout.fruchterman.reingold(g,weightsA=edges$weight,niterNumeric=5000, coolexpNumeric = 1.2)
pdf(file="transferTask.pdf",width=20,height=20)
plot(g,layout=lay,vertex.size=V(g)$size,edge.width=1,edge.color="black",vertex.label.cex=1.5)
#legend("topright",legend=tasks,pch=15,col=colors,cex=1.5)
dev.off()


