library(ggplot2)
library(ggimage)

# Data loading
data(movies, package="ggplot2movies")
movies <- as.data.frame(movies)

# Data preprocessing
movietype <- colnames(movies)[18:24]
mv <- movies[rowSums(movies[, movietype]) == 1,]
alt <- c(movietype[length(movietype)], movietype[-length(movietype)]) 
macierz <- as.matrix(mv[, alt])
mv$Type <- factor(macierz %*% (1:length(alt)), labels=alt)
mv2<-mv[mv$Type!= "Short", ]
mv3 <- mv2[!is.na(mv2$budget),]

### Scatter-line plot
ggplot(mv, aes(budget/1000000, Type)) +
  theme_classic()+
  geom_line(aes(group = Type), col="snow4") +
  geom_point(aes(color = (votes>20000), size=(mv$votes)), alpha=.1)+
  labs(title="Bud¿et w zale¿noœci od rodzaju filmu i liczby g³osów", x="Bud¿et [mln $]", 
       y="", col="liczba g³osów")+
  guides(size=F, color = guide_legend(override.aes = list(alpha=1)))+
  scale_colour_manual(values=c("firebrick1", "steelblue4"), labels = c("<=20 000", ">20 000"))+
  theme(legend.position ="top", plot.title = element_text(hjust=1))

#### Horizontal violin plot
ggplot(mv, aes(Type, rating)) +
  theme_bw()+
  coord_flip()+
  geom_violin(aes(group = Type), fill="steelblue4", col="slategray2", alpha=.5, trim=F, scale="width", adjust=.07)+
  scale_colour_gradient(low = "red", high = "gray40") +
  geom_boxplot(width=.1, notch = TRUE, alpha=0.4, coef = 6, fill="turquoise4", col="gray50")+
  labs(title="Ocena filmu, kwartyle ocen", y="Ocena", x="")

#### Horizontal violin plots
ggplot(mv2, aes(votes, rating)) +
  theme_bw()+
  coord_flip()+
  geom_violin( fill="steelblue4", col="slategray2", alpha=.5, trim=F, scale="width", adjust=.07)+
  scale_colour_gradient(low = "red", high = "gray40") +
  labs(title="Ocena filmu, kwartyle ocen", y="Ocena", x="")+
  facet_wrap(Type ~ ., ncol=3)


# Scatter plots, logarythmic scale
ggplot(mv2, aes(rating, votes)) + geom_point(aes(size=(mv2$votes),col=mv2$year), alpha=0.2) +
  labs(title="Wykres zale¿noœci oceny filmu od liczby g³osów\nz podzia³em na gatunki", x="Ocena", 
  y="Liczba g³osów", col="Rok")+
  coord_trans(y="log2")+
  guides(size=F)+
  theme_bw()+
  scale_colour_gradient(low = "darkorange", high = "steelblue4") +
  facet_wrap(Type ~ ., ncol=3)+
  theme(strip.background = element_rect(colour="black", fill="transparent"))


# Scatter plots
ggplot(mv3, aes(year, rating)) + 
  geom_point(aes(size=(mv3$votes),col=mv3$budget/1000000), alpha=0.2) +
  labs(title="Wykres zale¿noœci oceny filmu od roku powstania\nz podzia³em na gatunki", x="Rok", 
       y="Ocena", col="Bud¿et\n[mln $]")+
  guides(size=F)+
  theme_bw()+
  scale_colour_gradient(high = "turquoise1", low = "steelblue4") +
  facet_wrap(Type ~ ., ncol=2)+
  theme(strip.background = element_rect(colour="black", fill="aliceblue"))

# Iso density plot
ggplot(mv2, aes(rating, length)) +
  geom_density_2d(aes(colour = ..level..),bins=20)+
  facet_wrap(Type ~ ., ncol=3)+
  theme_bw()+
  scale_colour_gradient(high = "firebrick1", low = "steelblue4")+
  ylim(40,173)+
  labs(title="Wykres zale¿noœci oceny filmu od jego d³ugoœci", x="Ocena", 
       y="d³ugoœæ [min]")
  
citation()
