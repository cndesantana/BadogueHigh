res = 300, units = "in")
}
ggsave(file, plot = plotDetratoresCurtidos(), device = device)
}
)
output$detratoresassiduos = downloadHandler(
filename = function() {
paste("detratoresassiduos.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotDetratoresAssiduos(), device = device)
}
)
output$apoiadoresassiduos = downloadHandler(
filename = function() {
paste("apoiadoresassiduos.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotApoiadoresAssiduos(), device = device)
}
)
output$palavrasapoiadores = downloadHandler(
filename = function() {
paste("palavrasapoiadores.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotPalavrasApoiadores(), device = device)
}
)
output$palavrasdetratores = downloadHandler(
filename = function() {
paste("palavrasdetratores.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotPalavrasDetratores(), device = device)
}
)
output$genero = downloadHandler(
filename = function() {
paste("genero.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotGenero(), device = device)
}
)
output$preditores = downloadHandler(
filename = function() {
paste("preditores.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotPreditores(), height=9,width=16,dpi=300)
}
)
output$difpreditores = downloadHandler(
filename = function() {
paste("difpreditores.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotDifPreditores(), height=9,width=16,dpi=300)
}
)
output$serietemporal = downloadHandler(
filename = function() {
paste("serietemporal.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = 16, height = 9,
res = 300, units = "in")
}
ggsave(file, plot = plotSerieTemporal(), device = device)
}
)
output$palavras = downloadHandler(
filename = function() {
paste("palavras.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotPalavras(), device = device)
}
)
output$palavrasnegativas = downloadHandler(
filename = function() {
paste("listadepalavrasnegativas.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotPalavrasNegativas(), device = device)
}
)
output$palavraspositivas = downloadHandler(
filename = function() {
paste("listadepalavraspositivas.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotPalavrasPositivas(), device = device)
}
)
output$palavrasneutras = downloadHandler(
filename = function() {
paste("listadepalavrasneutras.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotPalavrasNeutras(), device = device)
}
)
output$wordcloud = downloadHandler(
filename = function() {
paste("wordcloud.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotWordcloud(), device = device)
}
)
### Treemap
output$treemap = downloadHandler(
filename = function() {
paste("treemap.png", sep = "")
},
content = function(file) {
ggsave(file, plot = plotTreemap(),
width=10.67, height=6, dpi=300,
device = "png", units="in")
}
)
output$treemapnegativo = downloadHandler(
filename = function() {
paste("treemapnegativo.png", sep = "")
},
content = function(file) {
ggsave(file, plot = plotTreemapNegativo(), width=10.67, height=6, dpi=300, device = "png", units="in")
}
)
output$treemappositivo = downloadHandler(
filename = function() {
paste("treemappositivo.png", sep = "")
},
content = function(file) {
ggsave(file, plot = plotTreemapPositivo(), width=10.67, height=6, dpi=300, device = "png", units="in")
}
)
output$treemapneutro = downloadHandler(
filename = function() {
paste("treemapneutro.png", sep = "")
},
content = function(file) {
ggsave(file, plot = plotTreemapNeutro(), width=10.67, height=6, dpi=300, device = "png", units="in")
}
)
####
output$wordcloudnegativo = downloadHandler(
filename = function() {
paste("wordcloudnegativo.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotWordcloudNegativo(), device = device)
}
)
output$wordcloudpositivo = downloadHandler(
filename = function() {
paste("wordcloudpositivo.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotWordcloudPositivo(), device = device)
}
)
output$wordcloudneutro = downloadHandler(
filename = function() {
paste("wordcloudneutro.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotWordcloudNeutro(), device = device)
}
)
output$plotpostsporsentimento = downloadHandler(
filename = function() {
paste("postsporsentimento.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotPostsPorSentimento(), device = device)
}
)
output$curtidascomentarios = downloadHandler(
filename = function() {
paste("curtidascomentarios.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotCurtidasComentarios(), device = device)
}
)
output$melhorpiorposts = downloadHandler(
filename = function() {
paste("melhorpiorposts.png", sep = "")
},
content = function(file) {
device <- function(..., width, height) {
grDevices::png(..., width = width, height = height,
res = 300, units = "in")
}
ggsave(file, plot = plotMelhorPiorPosts(), device = device)
}
)
}
filepath
fromCommentsToUnigram <- function(allmessages){
spSamp<- unlist(strsplit(allmessages, split=", "))
#   nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))
#   ascVec<- spSamp[ - nonAscIDX]
ascVec<- spSamp;
ascSamp<- paste(ascVec, collapse = ", ")
clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)
SampCrps<- Corpus(VectorSource(clnSamp))
SampCrps<- tm_map(SampCrps, tolower)
SampCrps<- tm_map(SampCrps, removePunctuation)
SampCrps<- tm_map(SampCrps, removeNumbers)
urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
SampCrps<-tm_map(SampCrps, urlPat)
emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
SampCrps<- tm_map(SampCrps, emlPat)
tt<-function(x) gsub("RT |via", "", x)
SampCrps<- tm_map(SampCrps, tt)
tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
SampCrps<- tm_map(SampCrps, tun)
SampCrps<-tm_map(SampCrps, removeWords, stopwords("portuguese"))
myCrps<- txt.to.words(SampCrps)
tblUniGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 1)))
stblUnigrm<-tblUniGrm[order(tblUniGrm$Freq, decreasing = TRUE),]
return(stblUnigrm)
}
file
head(file)
p
text <- toupper(file$text[which(toupper(file$Sentimento) == "POSITIVO")])
text
unigram <- getUnigram(text)
unigram <- unigram %>%
filter(!(words %in% badwords))%>%
filter(!is.na(words)) %>%
select(words) %>% group_by(words) %>%
summarise(palavras = n()) %>%
arrange(palavras) %>% tail(50)
numerodereferencia <- max(unigram$palavras) %/% 5
unigram <- unigram %>%
mutate(classe = case_when(palavras < numerodereferencia ~ "de 1 a 5",
palavras < 2*numerodereferencia ~ "de 5 a 10",
palavras < 3*numerodereferencia ~ "de 10 a 50",
palavras < 4*numerodereferencia ~ "de 50 a 100",
palavras >= 4*numerodereferencia ~ "mais que 100")) %>%
mutate(classe = factor(classe, levels = c("de 1 a 5", "de 5 a 10", "de 10 a 50", "de 50 a 100", "mais que 100")))
colfunc <- colorRampPalette(c(corpositivo))
ggplot(unigram, aes(area = palavras,
fill = colfunc(5)[as.numeric(unigram$classe)],
label = words,
subgroup=palavras)) +
geom_treemap(fill = "black") +
geom_treemap(aes(alpha=palavras)) +
geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
grow = F, reflow=TRUE) +
geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 1,
col="white", cex=10) +
ggtitle("Palavras mais comentadas")+
scale_fill_identity() +
scale_alpha_continuous(range = c(0.4, 1),guide = 'none')
p1 <- ggplot(unigram, aes(area = palavras,
fill = colfunc(5)[as.numeric(unigram$classe)],
label = words,
subgroup=palavras)) +
geom_treemap(fill = "black") +
geom_treemap(aes(alpha=palavras)) +
geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
grow = F, reflow=TRUE) +
geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 1,
col="white", cex=10) +
ggtitle("Palavras mais comentadas")+
scale_fill_identity() +
scale_alpha_continuous(range = c(0.4, 1),guide = 'none')
png("treemap_positivo.png",width = 3200,height = 1800,res = 300)
print(p1)
dev.off()
file <- read.csv2("filepath")
filepath
file <- read.csv2(filepath)
text <- toupper(file$text[which(toupper(file$polarization) == "NEGATIVO")])
unigram <- getUnigram(text)
unigram <- unigram %>%
filter(!(words %in% badwords))%>%
filter(!is.na(words)) %>%
select(words) %>% group_by(words) %>%
summarise(palavras = n()) %>%
arrange(palavras) %>% tail(50)
numerodereferencia <- max(unigram$palavras) %/% 5
unigram <- unigram %>%
mutate(classe = case_when(palavras < numerodereferencia ~ "de 1 a 5",
palavras < 2*numerodereferencia ~ "de 5 a 10",
palavras < 3*numerodereferencia ~ "de 10 a 50",
palavras < 4*numerodereferencia ~ "de 50 a 100",
palavras >= 4*numerodereferencia ~ "mais que 100")) %>%
mutate(classe = factor(classe, levels = c("de 1 a 5", "de 5 a 10", "de 10 a 50", "de 50 a 100", "mais que 100")))
colfunc <- colorRampPalette(c(cornegativo))
ggplot(unigram, aes(area = palavras,
fill = colfunc(5)[as.numeric(unigram$classe)],
label = words,
subgroup=palavras)) +
geom_treemap(fill = "black") +
geom_treemap(aes(alpha=palavras)) +
geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
grow = F, reflow=TRUE) +
geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 1,
col="white", cex=10) +
ggtitle("Palavras mais comentadas")+
scale_fill_identity() +
scale_alpha_continuous(range = c(0.4, 1),guide = 'none')
file
text
file$text <- file$Post
text <- toupper(file$text[which(toupper(file$polarization) == "NEGATIVO")])
unigram <- getUnigram(text)
unigram <- unigram %>%
filter(!(words %in% badwords))%>%
filter(!is.na(words)) %>%
select(words) %>% group_by(words) %>%
summarise(palavras = n()) %>%
arrange(palavras) %>% tail(50)
numerodereferencia <- max(unigram$palavras) %/% 5
unigram <- unigram %>%
mutate(classe = case_when(palavras < numerodereferencia ~ "de 1 a 5",
palavras < 2*numerodereferencia ~ "de 5 a 10",
palavras < 3*numerodereferencia ~ "de 10 a 50",
palavras < 4*numerodereferencia ~ "de 50 a 100",
palavras >= 4*numerodereferencia ~ "mais que 100")) %>%
mutate(classe = factor(classe, levels = c("de 1 a 5", "de 5 a 10", "de 10 a 50", "de 50 a 100", "mais que 100")))
colfunc <- colorRampPalette(c(cornegativo))
ggplot(unigram, aes(area = palavras,
fill = colfunc(5)[as.numeric(unigram$classe)],
label = words,
subgroup=palavras)) +
geom_treemap(fill = "black") +
geom_treemap(aes(alpha=palavras)) +
geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
grow = F, reflow=TRUE) +
geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 1,
col="white", cex=10) +
ggtitle("Palavras mais comentadas")+
scale_fill_identity() +
scale_alpha_continuous(range = c(0.4, 1),guide = 'none')
file
file$text <- file$Post
file$text
text <- toupper(file$text[which(toupper(file$Sentimento) == "NEGATIVO")])
unigram <- getUnigram(text)
unigram <- unigram %>%
filter(!(words %in% badwords))%>%
filter(!is.na(words)) %>%
select(words) %>% group_by(words) %>%
summarise(palavras = n()) %>%
arrange(palavras) %>% tail(50)
numerodereferencia <- max(unigram$palavras) %/% 5
unigram <- unigram %>%
mutate(classe = case_when(palavras < numerodereferencia ~ "de 1 a 5",
palavras < 2*numerodereferencia ~ "de 5 a 10",
palavras < 3*numerodereferencia ~ "de 10 a 50",
palavras < 4*numerodereferencia ~ "de 50 a 100",
palavras >= 4*numerodereferencia ~ "mais que 100")) %>%
mutate(classe = factor(classe, levels = c("de 1 a 5", "de 5 a 10", "de 10 a 50", "de 50 a 100", "mais que 100")))
colfunc <- colorRampPalette(c(cornegativo))
ggplot(unigram, aes(area = palavras,
fill = colfunc(5)[as.numeric(unigram$classe)],
label = words,
subgroup=palavras)) +
geom_treemap(fill = "black") +
geom_treemap(aes(alpha=palavras)) +
geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
grow = F, reflow=TRUE) +
geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 1,
col="white", cex=10) +
ggtitle("Palavras mais comentadas")+
scale_fill_identity() +
scale_alpha_continuous(range = c(0.4, 1),guide = 'none')
p2 <- ggplot(unigram, aes(area = palavras,
fill = colfunc(5)[as.numeric(unigram$classe)],
label = words,
subgroup=palavras)) +
geom_treemap(fill = "black") +
geom_treemap(aes(alpha=palavras)) +
geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
grow = F, reflow=TRUE) +
geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 1,
col="white", cex=10) +
ggtitle("Palavras mais comentadas")+
scale_fill_identity() +
scale_alpha_continuous(range = c(0.4, 1),guide = 'none')
png("negativo.png",width = 3200, height = 1800,res = 300)
print(p2)
dev.off()
head(file)
file %>% group_by(Sentimento, IdFacebook) %>% count()
file %>% group_by(Sentimento, IdFacebook) %>% count() %>% arrange(Sentimento,n)
file %>% group_by(Sentimento, IdFacebook) %>% count() %>% arrange(Sentimento,n) %>% tail(10)
file %>% group_by(Sentimento, IdFacebook) %>% count() %>% arrange(Sentimento,n) %>% tail(20)
file %>% group_by(Sentimento, IdFacebook) %>% count() %>% arrange(Sentimento,n) %>% head(20)
file %>% group_by(Sentimento, IdFacebook) %>% count() %>% arrange(n) %>% head(20)
file %>% group_by(Sentimento, IdFacebook) %>% count() %>% arrange(n) %>% tail(20)
file %>% group_by(IdFacebook) %>% count() %>% arrange(n) %>% tail(20)
head(file)
file %>% group_by(Sentimento) %>% summarise(n = sum(TotalCurtidas)) %>% arrange(n) %>% tail(20)
file %>% group_by(Sentimento) %>% summarise(n = sum(TotalCurtidas)) %>% arrange(n)
file %>% group_by(Sentimento) %>% summarise(n = sum(TotalCurtidas)) %>% arrange(n)
file %>% group_by(Sentimento) %>% summarise(n = sum(TotalCurtidas))
file %>% count(TotalCurtidas)
file %>% group_by(Sentimento) %>% summarise(n=sum(TotalCurtidas))
file %>% group_by(Sentimento)
file %>% count(Sentimento)
364/(364+169+161)
round(364/(364+169+161),2)
round(169/(364+169+161),2)
round(161/(364+169+161),2)
file %>% arrange(TotalCurtidas)
file %>% group_by(Sentimento) %>% summarise(total = sum(TotalCurtidas)) %>% arrange(total)
file %>% group_by(Sentimento) %>% summarise(total = sum(TotalCurtidas, na.rm = TRUE)) %>% arrange(total)
file %>% group_by(Sentimento) %>% summarise(total = sum(TotalCurtidas, na.rm = TRUE))
file %>% group_by(Sentimento) %>% summarise(total = sum(TotalCurtidas, na.rm = TRUE)) %>% filter(Sentimento != "")
file %>% group_by(Sentimento) %>% summarise(total = sum(TotalCurtidas, na.rm = TRUE)) %>% filter(Sentimento != "") %>% ggplot(aes(x = Sentimento, y = total, fill = Sentimento)) + geom_bar(stat="identity")
file %>% group_by(Sentimento) %>% summarise(total = sum(TotalCurtidas, na.rm = TRUE)) %>% filter(Sentimento != "") %>% ggplot(aes(x = Sentimento, y = total, fill = Sentimento)) + geom_bar(stat="identity") + theme_bw()
file %>% group_by(Sentimento) %>% summarise(total = sum(TotalCurtidas, na.rm = TRUE)) %>% filter(Sentimento != "") %>% ggplot(aes(x = Sentimento, y = total, fill = Sentimento)) + geom_bar(stat="identity") + theme_bw() + labs(title = "Número de Curtidas", y = "Curtidas", x = "Tipos de Comentários")
p2 <- file %>% group_by(Sentimento) %>% summarise(total = sum(TotalCurtidas, na.rm = TRUE)) %>% filter(Sentimento != "") %>% ggplot(aes(x = Sentimento, y = total, fill = Sentimento)) + geom_bar(stat="identity") + theme_bw() + labs(title = "Número de Curtidas", y = "Curtidas", x = "Tipos de Comentários")
png("curtidas.png",width=3200,height=1800,res=300)
print(p2)
dev.off()
p2 <- file %>% group_by(Sentimento) %>% summarise(total = sum(TotalCurtidas, na.rm = TRUE)) %>% filter(Sentimento != "") %>% ggplot(aes(x = Sentimento, y = total, fill = factor(Sentimento))) + geom_bar(stat="identity") + theme_bw() + labs(title = "Número de Curtidas", y = "Curtidas", x = "Tipos de Comentários")
p2 <- file %>% group_by(Sentimento) %>% summarise(total = sum(TotalCurtidas, na.rm = TRUE)) %>% filter(Sentimento != "") %>% ggplot(aes(x = Sentimento, y = total, fill = factor(Sentimento))) + geom_bar(stat="identity") + theme_bw() + labs(title = "Número de Curtidas", y = "Curtidas", x = "Tipos de Comentários") + scale_fill_manual(values= c(cornegativo,corneutro,corpositivo))
p2
p2 <- file %>% group_by(Sentimento) %>% summarise(total = sum(TotalCurtidas, na.rm = TRUE)) %>% filter(Sentimento != "") %>% ggplot(aes(x = Sentimento, y = total, fill = factor(Sentimento))) + geom_bar(stat="identity") + theme_bw() + labs(title = "Número de Curtidas", y = "Curtidas", x = "Tipos de Comentários", fill = "Sentimento") + scale_fill_manual(values= c(cornegativo,corneutro,corpositivo))
p2
png("curtidas.png",width=3200,height=1800,res=300)
print(p2)
dev.off()
p2 <- file %>% group_by(Sentimento) %>% summarise(total = n()) %>% filter(Sentimento != "") %>% ggplot(aes(x = Sentimento, y = total, fill = factor(Sentimento))) + geom_bar(stat="identity") + theme_bw() + labs(title = "Número de Comentários", y = "Comentários", x = "Tipos de Comentários", fill = "Sentimento") + scale_fill_manual(values= c(cornegativo,corneutro,corpositivo))
p2
png("comentarios.png",width=3200,height=1800,res=300)
print(p2)
dev.off()
savehistory("historico.R")
