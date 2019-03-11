options(scipen = 999)
library(data.table)
library(ggplot2)
library(stringr)
library(ggraph)
library(igraph)
library(reshape2)
library(tidyr)
#install.packages("jsonlite")
library(plotly)
library(dplyr)
library(ggimage)

setwd("C:\\PSDB\\Rede45")

filenames <- dir()

files_dt_all_comments <- filenames[str_detect(filenames, "dt_all_comments")]
files_dt_all_comments <- files_dt_all_comments[order(files_dt_all_comments)]
files_dt_all_likes <- filenames[str_detect(filenames, "dt_all_likes")]
files_dt_all_likes <- files_dt_all_likes[order(files_dt_all_comments)]
files_dt_all_reactions <- filenames[str_detect(filenames, "dt_all_reactions")]
files_dt_all_reactions <- files_dt_all_reactions[order(files_dt_all_comments)]

dt_comments <- rbindlist(lapply(files_dt_all_comments, function(f) fread(f, sep=",", encoding = "Latin-1")))
dt_likes <- rbindlist(lapply(files_dt_all_likes, function(f) fread(f, sep=",", encoding = "Latin-1")))
dt_reactions <- rbindlist(lapply(files_dt_all_reactions, function(f) fread(f, sep=",", encoding = "Latin-1")))

all_comments <- dt_comments
all_likes <- dt_likes
all_reactions <- dt_reactions

all_comments <- fread("dt_all_comments_0921.1021.csv")
all_likes <- fread("dt_all_likes_0921.1021.csv")
all_reactions <- fread("dt_all_reactions_0921.1021.csv")
View(all_comments)
View(all_likes)
View(all_reactions)
all_reactions[,.N,by = .(from_type, to_name)][order(-N)]
all_reactions

View(highschool)

dt_highschool <- as.data.table(highschool)

dt_highschool[,.N,by = .(from,to)][N > 1][order(-N)]


graph <- graph_from_data_frame(highschool)
V(graph)$Popularity <- degree(graph, mode = 'in')
ggraph(graph, layout = 'kk') + 
    geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) + 
    geom_node_point(aes(size = Popularity)) + 
    facet_edges(~year) + 
    theme_graph(foreground = 'steelblue')

# Top likers
likers <- dt_likes[,.N,by = from_name][order(-N)]
likers_outliers <- boxplot.stats(likers$N)$out
likers <- likers[N %in% likers_outliers,,]

top_likers <- likers[1:20]
ggplot(top_likers, aes(x = from_name, y = N, fill = from_name ) ) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("") + labs(fill="Usuário Facebook") +
  theme(axis.text.x = element_text(color="#993333", angle = 90))

# Top likeds
likeds <- dt_likes[,.N,by = to_name][order(-N)]
likeds_outliers <- boxplot.stats(likeds$N)$out
likeds <- likeds[N %in% likeds_outliers,,]

top_likeds <- likeds[to_name!="PSDB",,][1:20]
cvalues <- colorRampPalette(brewer.pal(8, "Accent"))(20)
sfm <- scale_fill_manual(values = cvalues,guide = guide_legend(nrow=2))
ggplot(top_likeds, aes(x = to_name, y = N, fill = to_name) ) +
  geom_bar(stat = "identity") + sfm +
  geom_text(hjust=-0.1, aes(x = to_name, angle = 90, label = to_name, colour="red", fontface = "bold", size=18)) +
  scale_y_continuous(limits = c(0, 430)) +
  xlab("") + ylab("") + labs(fill="Usuário Facebook") + 
  theme(axis.text.x = element_blank(), legend.position="none") # element_text(color="#993333", angle = 90)
  
# Top reactions given
from_types <- dt_reactions[,unique(from_type),]
reactions_long <- dt_reactions[,.N,by = .(from_name, from_type)][order(-N)]
reactions_wide <- as.data.table(dcast(reactions_long, from_name ~ from_type))
reactions_wide[is.na(reactions_wide)] <- 0
reactions_values <- reactions_wide[,2:9,with = FALSE,]
reactions_wide$rank <- reactions_values[,do.call(pmax, .SD),]
reactions_wide <- reactions_wide[order(-rank)]

# Top posts Hahaha
unique(dt_reactions$id_parent)

top_reactions_long <- rbindlist(lapply(from_types, function(x) reactions_long[from_type == x][1]))[order(-N)]
plot_ly(top_reactions_long, x = ~from_name, y = ~N, color = ~from_type)
plot_ly(reactions_long[from_type == "LIKE"][order(-N)][1:20], x = ~from_name, y = ~N, color = ~from_name)
plot_ly(reactions_long[from_type == "ANGRY"][order(-N)][1:20], x = ~from_name, y = ~N, color = ~from_name)
plot_ly(reactions_long[from_type == "LOVE"][order(-N)][1:20], x = ~from_name, y = ~N, color = ~from_name)
plot_ly(reactions_long[from_type == "HAHA"][order(-N)][1:20], x = ~from_name, y = ~N, color = ~from_name)
plot_ly(reactions_long[from_type == "WOW"][order(-N)][1:20], x = ~from_name, y = ~N, color = ~from_name)
plot_ly(reactions_long[from_type == "SAD"][order(-N)][1:20], x = ~from_name, y = ~N, color = ~from_name)
plot_ly(reactions_long[from_type == "THANKFUL"][order(-N)][1:20], x = ~from_name, y = ~N, color = ~from_name)
plot_ly(reactions_long[from_type == "PRIDE"][order(-N)][1:20], x = ~from_name, y = ~N, color = ~from_name)

reactions_type <- dt_reactions[,.(count = .N),by = .(from_type)][order(-count)]
reactions_type <- reactions_type[from_type!="LIKE"]
reactions_type %>% plot_ly(labels = ~from_type, values = ~count) %>% add_pie(hole = 0.6) %>%
  layout(title = "",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

index_top <- c(
  order(reactions_wide$ANGRY)[1:10], 
  order(reactions_wide$HAHA)[1:10],
  order(reactions_wide$LIKE)[1:10],
  order(reactions_wide$LOVE)[1:10],
  order(reactions_wide$PRIDE)[1:10],
  order(reactions_wide$SAD)[1:10],
  order(reactions_wide$THANKFUL)[1:10],
  order(reactions_wide$WOW)[1:10]
  )
index_top <- unique(index_top)
top_reactions <- reactions_wide[index_top,,][order(-rank)]

# Mostrando o que um usuário que deu muito HAHA rí ide que? Quais posts?
#View(dt_comments[str_detect(str_to_lower(message), "bolsonaro"),,])
#dt_comments[str_detect(str_to_lower(message), "bolsonaro|bolsomito"),.N,by = from_name][order(-N)]
dt_comments[from_name == "Toim Neto",,]
names(dt_comments)
setkey(dt_comments, id)
id_parestes_toim <- dt_reactions[from_id=="10202880285183106" & from_type=="HAHA",id_parent,]
View(dt_comments[id_parestes_toim,,])

unique(dt_comments$comment_type)
View(dt_comments)

id_posta_mais_comentados <- dt_comments[comment_type=="reply",.N,by = id_parent][order(-N)]
# qplot(id_posta_mais_comentados[N > 5,N,], geom="histogram") 
id_posta_mais_comentados <- id_posta_mais_comentados[N >= 20,,]
setkey(id_posta_mais_comentados, id_parent)
dt_comments$rank <- id_posta_mais_comentados[dt_comments$id,N,]
View(dt_comments[order(-rank),,])
View(dt_comments[,.N,by = from_name][N > 25][order(-N)])
View(dt_comments[!is.na(rank)][order(-rank),,])

reactions_long_top <- reactions_long[from_name %in% top_reactions$from_name]
ggplot(reactions_long_top, aes(x = from_name, y = from_type)) +
  geom_point(aes(size = N, colour = from_type), alpha=.5) +
  geom_text(aes(label=N)) + 
  scale_size(range = c(10, 20)) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_text(color="#993333", angle = 90))


ids <- dt_reactions[from_name %in% reactions_long_top$from_name,unique(from_id),]
users <- list()
for(id in ids) {
  try({users[[length(users) + 1]] <- getUsers(id, APIKey)})
}
dt_users <- rbindlist(users, use.names=TRUE, fill=TRUE)
setkey(reactions_long, from_name)
dt_users$N <- reactions_long[dt_users$name,N,mult = "first"]
Nlt100 <- dt_users[N < 100,.(min = min(N), max = max(N)),]
Ngt100 <- dt_users[N >= 100,.(min = min(N), max = max(N)),]
dt_users$categoty <- ifelse(dt_users$N < 100, sprintf("Entre %d e %d reações",Nlt100$min,Nlt100$max), sprintf("Entre %d e %d reações",Ngt100$min,Ngt100$max))

ggplot(dt_users, aes(y = name, x = N)) +
  geom_image(aes(image=picture), by = "height") + xlab("") + ylab("")
ggplot(dt_users, aes(y = name, x = categoty)) +
  geom_image(aes(image=picture), by = "height") + xlab("") + ylab("")
ggsave("top reactions.png")  

dt_comments[is.na(to_name),to_name := "PSDB",]
dt_comments[,.N,by = from_name][order(-N)]
dt_comments[,.N,by = to_name][order(-N)]
View(dt_comments[from_name != "PSDB" & to_name != "PSDB" & from_name != to_name,.N,by = .(from_name, to_name)][order(-N)])

qplot(dt_likes[,.N,by = from_name][order(-N)][,N,])
hist(dt_likes[,.N,by = from_name][order(-N)][,N,])
x[!x %in% boxplot.stats(x)$out]
