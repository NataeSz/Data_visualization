rm(list=ls())
### Economics Data Visualization - ggplot2

library(ggplot2)
ec <- economics


ggplot(ec, aes(pce)) + 
  geom_histogram(bins=7, fill="deepskyblue4", col="deepskyblue3", alpha=0.7)+
  labs(title="Histogram", x="Osobiste wydatki konsumpcyjne", y="liczebnosc")+
  theme_dark()

ggplot(ec, aes(psavert, unemploy)) + geom_point(aes(size=ec$uempmed,col=ec$uempmed), alpha=0.33) +
  labs(title="Wykres b¹belkowy", x="WskaŸnik oszczêdnoœci osobistych", 
       y="Liczba bezrobotnych w tys.", col="Mediana\nliczby\ntygodni\nbez pracy")+
  guides(size=F)+
  scale_colour_gradient(low = "steelblue3", high = "limegreen") 

ggplot(ec, aes(psavert, uempmed)) + geom_point(aes(size=(ec$unemploy),col=ec$unemploy), alpha=0.4) +
  labs(title="Wykres b¹belkowy", x="WskaŸnik oszczêdnoœci osobistych", 
       y="Mediana liczby tygodni bez pracy", col="Liczba\nbezrobotnych\nw tys.")+
  guides(size=F)+
  theme_bw()+
  scale_colour_gradient(low = "gray50", high = "skyblue1") 

ggplot(ec, aes(uempmed, unemploy))+
  theme_bw()+
  geom_point(alpha=.5, size=0.00027*ec$pce, aes(col=ec$pce))+
  scale_colour_gradient(low = "red", high = "gray40") +
  geom_violin(aes(group = cut_width(uempmed, 2.5)), scale = "width", adjust=.3, alpha=.4, fill="darkorange", col="gray40")+
  labs(title="Wykres wiolinowy", x="Mediana liczby tygodni bez pracy", 
       y="Liczba bezrobotnych w tys.", col="wydatki\nkonsumpcyjne\n[mld $]")
