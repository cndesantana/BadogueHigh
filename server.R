library(shiny)
library(plotrix)
library(stringr)
library(quanteda)
library(readtext)
library(tm)
library(stringr)
library(dplyr)
library(wordcloud)
library(scales)
library(jpeg)
library(qdapRegex)
library(grDevices)
library(treemap)
library(stylo)
library(tidytext)
library(tokenizers)
library(tm)
library(stringr)
library(dplyr)
library(treemapify)


corpositivo <- "#20B2AA";
cornegativo <- "#c00000";
corneutro <- "#FFA500";
badwords <- c("compartilhado","boa","scontent.xx.fbcdn.net","https","oh","oe","pra","v",
              "como","para","de","do","da","das","dos","isso","esse",
              "nisso","nesse","aquele","nesses","aqueles","aquela",
              "aquelas","que","q","é","sr","senhor","comentário","perfil",
              "mais","com","está","por","uma","tem","vai","pelo","meu",
              "sobre","não","já","nos","sem","quando","xed","xbd","ser",
              "xbe","xa0","x8f","xb9","xb2","xb0","xb1","xb8","x8c","xa3",
              "xbc","xaa","www.youtube.com","scontent.xx.fbcdn.net","https",
              "oh","oe","pra","v","como","para","de","do","da","das","dos",
              "isso","esse","nisso","nesse","aquele","nesses","aqueles","aquela",
              "aquelas","que","q","é","sr","senhor","comentário","perfil","r","que",
              "nao","sim","comentário","feito","comentario","imagem","comentario feito no perfil de secretaria",
              "secretaria","foi","photos","http","bit.ly","sou","mais","bahia","vídeo","timeline","video","er",
              "enem","vçpt","vç","x","vc", "aqui", "você", "tá", "dia", "amanhã", "ba","aqui","governador",
              "Governador","GOVERNADOR","governado","Governado","GOVERNADO","rui","Rui","costa","Costa","RUI",
              "COSTA","Governo","governo","GOVERNO","Bahia","bahia","com","que","nao","meu","mais","por","uma",
              "pra","para","um","mais","mas","clap","para","tone","skin","type","heart","facebook","iticas","munici","3","4",
              "unamused","esses","essas","até","são","ate","sao","todas","todos","toda","todo","essa", "esse","2")
palette <- c("#ff9ff3","#feca57","#ff6b6b","#48dbfb","#1dd1a1")

getUnigram <- function(text){
  text <- removeWords(text,c(stopwords("portuguese"),badwords))
#  text <- rm_nchar_words(text, n= "1")#remove words with only one character
#  text <- rm_nchar_words(text, n="2")#remove words with two characters
  text <- rm_nchar_words(text, n="3")#remove words with two characters
  text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", text) # Remove 1-2 letter words
  text <- gsub("^ +| +$|( ) +", "\\1", text) # Remove excessive spacing
  text <- stringi::stri_trans_general(text, "latin-ascii")
  unigram <- data.frame(words = unlist(tokenize_ngrams(text, n = 1L, n_min = 1L, simplify = TRUE)))
  
  unigram$words[which(!is.na(str_extract(unigram$words,"comentario")))] <- NA
  unigram$words[which(!is.na(str_extract(unigram$words,"timeline")))] <- NA
  unigram$words[which(!is.na(str_extract(unigram$words,"video")))] <- NA
  unigram$words[which(!is.na(str_extract(unigram$words,"photos")))] <- NA
  unigram$words[which(!is.na(str_extract(unigram$words,"conta")))] <- NA
  unigram$words[which(!is.na(str_extract(unigram$words,"eliminatoria")))] <- NA
  unigram$words[which(!is.na(str_extract(unigram$words,"eliminatória")))] <- NA
  
  return(unigram)
}


cleaningRelNI1Temas <- function(relNI){

	relNI$Temas <- toupper(relNI$Temas)
   relNI$Temas <- str_replace_all(relNI$Temas, "DITRIBUIÇÃO DE ÁGUA","DISTRIBUIÇÃO DE ÁGUA")
      
   relNI$Temas <- str_replace_all(relNI$Temas, "\'","\"")
   relNI$Temas <- str_replace_all(relNI$Temas, "\\] \\[",",")
   relNI$Temas <- str_replace_all(relNI$Temas, "\\]  \\[",",")
   relNI$Temas <- str_replace_all(relNI$Temas, " , ",",")
	relNI$Temas <- str_replace_all(relNI$Temas, ", ",",")
	    
	relNI$Temas <- str_replace_all(relNI$Temas, "\"]","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\"] ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \"]","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \"] ","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" ]","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" ] ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \" ]","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \" ] ","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\\]","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\\] ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \\]","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \\] ","")

	relNI$Temas <- str_replace_all(relNI$Temas, " \"\\[ ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \"\\[","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\"\\[ ","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\"\\[","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" \\[ ","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" \\[","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \" \\[ ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \" \\[","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\\[ ","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\\[","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \\[ ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \\[","")
	
	relNI$Temas <- str_replace_all(relNI$Temas, "[\\[\\]]","")
										       
	relNI$Temas <- str_replace_all(relNI$Temas, "\",",",")
	relNI$Temas <- str_replace_all(relNI$Temas, " \",",",")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" ,",",")
	relNI$Temas <- str_replace_all(relNI$Temas, " \" ,",",")
	relNI$Temas <- str_replace_all(relNI$Temas, "\"","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \"","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \" ","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" ","\"")
	
	relNI <- relNI %>%
	   mutate(Temas = strsplit(as.character(Temas), ", ")) %>% 
	   unnest(Temas) 

	relNI <- relNI %>%
	   mutate(Temas = strsplit(as.character(Temas), ",")) %>% 
		unnest(Temas)
															         
	relNI$Temas <- str_replace_all(relNI$Temas, "SAÚDE ","SAÚDE")
	relNI$Temas <- str_replace_all(relNI$Temas, "SEGURANÇA ","SEGURANÇA")
	relNI$Temas <- str_replace_all(relNI$Temas, "OBRAS DA INFRAESTRUTURA ","OBRAS DA INFRAESTRUTURA")
	relNI$Temas <- str_replace_all(relNI$Temas, "INCENTIVO AO ESPORTE E LAZER ","INCENTIVO AO ESPORTE E LAZER")
	relNI$Temas <- str_replace_all(relNI$Temas, "TRANSPORTE PÚBLICO/MOBILIDADE ","TRANSPORTE PÚBLICO/MOBILIDADE")
	relNI$Temas <- str_replace_all(relNI$Temas, "HABITAÇÃO/MORADIA ","HABITAÇÃO/MORADIA")
	relNI$Temas <- str_replace_all(relNI$Temas, "GERAÇÃO DE EMPREGOS ","GERAÇÃO DE EMPREGO")
	relNI$Temas <- str_replace_all(relNI$Temas, "GERAÇÃO DE EMPREGO E RENDA ","GERAÇÃO DE EMPREGO")
	relNI$Temas <- str_replace_all(relNI$Temas, "GERAÇÃO DE EMPREGO E RENDA","GERAÇÃO DE EMPREGO")
	relNI$Temas <- str_replace_all(relNI$Temas, "GERAÇÃO DE EMPREGOS","GERAÇÃO DE EMPREGO")
	relNI$Temas <- str_replace_all(relNI$Temas, "APOIO À AGRICULTURA ","APOIO À AGRICULTURA")
	relNI$Temas <- str_replace_all(relNI$Temas, "APOIO À CULTURA ","APOIO À CULTURA")
	relNI$Temas <- str_replace_all(relNI$Temas, "APOIO AO TURISMO ","APOIO AO TURISMO")
	relNI$Temas <- str_replace_all(relNI$Temas, "DISTRIBUIÇÃO DE ÁGUA ","DISTRIBUIÇÃO DE ÁGUA")
	relNI$Temas <- str_replace_all(relNI$Temas, "ESTRADAS E RODOVIAS ","ESTRADAS E RODOVIAS")
	relNI <- relNI %>% filter(!is.na(Temas))
	
	return(relNI)
}

cleaningRelNI1Grupos <- function(relNI1){
	relNI1$Grupos <- str_replace_all(relNI1$Grupos, " \"","")
   relNI1$Grupos <- str_replace_all(relNI1$Grupos, "\"]","")
   relNI1$Grupos <- str_replace_all(relNI1$Grupos, "\\[","")
   relNI1$Grupos <- str_replace_all(relNI1$Grupos, "\" ,",",")
	relNI1$Grupos <- str_replace_all(relNI1$Grupos, "Região 2 - Re","Região 4 - Re")
	    
   relNI1 <-  relNI1 %>%
             mutate(Grupos = strsplit(as.character(Grupos), ", ")) %>% 
   	        unnest(Grupos)
   relNI1 <-  relNI1 %>%
	         mutate(Grupos = strsplit(as.character(Grupos), ",")) %>% 
		       unnest(Grupos)
		          
    relNI1 <- relNI1 %>% 
        mutate(Grupos = stringr::str_replace_all(Grupos, "[\\[\\]]","")) %>% 
	     mutate(Grupos = strsplit(as.character(Grupos), ",")) %>% 
	     unnest(Grupos) %>% 
		  filter(Grupos != "CPBA_3629944") %>%
		  filter(Grupos != "Rui Correria_9356576")  %>%
		  filter(Grupos != "Neutro")%>%
		  filter(Grupos != "Carnaval_974981")%>%
		  filter(Grupos != "Carnaval") %>%
		  filter(Grupos != "Notícias Carnaval") %>%
		  filter(Grupos != "Rui Correria") %>%
		  filter(Grupos != "teste_6503868") %>%
		  filter(Grupos != "TESTE Região 11 Caetité")
    return(relNI1)
}

cleaningRelNI1Data <- function(relNI1){

	   ################### CLEANING GRUPOS
	   relNI1 <- cleaningRelNI1Grupos(relNI1)
      
   ################### CLEANING 'TEMAS'
   
   relNI1 <- cleaningRelNI1Temas(relNI1)
      
      relNI1$Temas <- str_replace_all(relNI1$Temas, '_.*', '')
      relNI1$Grupos <- str_replace_all(relNI1$Grupos, '_.*', '')
         
   return(relNI1)
}

getScreenshot <- function(link1, filename1){
   workdir <- "figures_badogue"
   if(file.exists(workdir)){
      resolution <- "1366x768"
      
      outputfile1 <- file.path(workdir,filename1)
      command1 = paste("pageres ", link1, " ", resolution, " --delay=2 --format='jpg' --verbose --sector='.contentArea' --filename=",outputfile1,sep="")
      if(length(system(paste("ls ",outputfile1),intern=TRUE))==0){
         system(command1)
      }else{
         system(paste("sudo rm ",workdir,"/",outputfile1,sep=""))
         system(command1)
      }
   }
}

fa <- function(x) iconv(x, to = "ASCII//TRANSLIT")
getMatrizDeOcorrencias <- function(text){
   text <- stringi::stri_trans_tolower(text)
   temp <- fa(text)
   temp <- rm_nchar_words(temp, "1")#remove words with only one character
   temp <- rm_nchar_words(temp, "2")#remove words with two characters
   temp <- rm_nchar_words(temp, "3")#remove words with two characters
   # Lowercase
   # Shrink down to just one white space
   temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
   temp=str_replace_all(temp,"[^[:graph:]]", " ") 
   # Split it
   any(grepl("I_WAS_NOT_ASCII", iconv(temp, "latin1", "ASCII", sub="I_WAS_NOT_ASCII")))
   temp <- stringi::stri_trans_general(temp, "latin-ascii")
   temp <- removePunctuation(temp)
   temp <- unlist(stringr::str_split(temp, " "))
   # Get rid of trailing "" if necessary
   indexes <- which(temp == "")
   if(length(indexes) > 0){
      temp <- temp[-indexes]
   }
   docs <- Corpus(VectorSource(temp))
   docs <- tm_map(docs, removeNumbers)
   # Remove english common stopwords
   docs <- tm_map(docs, removeWords, stopwords("portuguese"))
   # Remove your own stop word
   docs <- tm_map(docs, removeWords, c("blabla1", "blabla2","que","ser","pelo","tem","o","lhe","por","pra","de","da","do","essa","esse","isso","aquele","aquilo","desse","disso","daquilo","uma","um","NA")) 
#  Remove punctuations
   docs <- tm_map(docs, stripWhitespace)
#
   dtm <- TermDocumentMatrix(docs)
   m <- as.matrix(dtm)
   v <- sort(rowSums(m),decreasing=TRUE)
   d <- data.frame(word = names(v),freq=v)
   
   return(d)
}

getIndiceDeFavorabilidade <- function(polarization){
   sentimentos <- toupper(polarization)
   allsentimentos <- c("NEUTRA","NEGATIVA","POSITIVA");
   mp <- length(which(sentimentos==allsentimentos[3]));#POSITIVA
   mt <- length(polarization);#Total
  
   indicefavorabilidade <- ifelse(mt == 0, 0, as.numeric(mp/mt))#changing sentiment index to a "positive feedback index"
   indicefavorabilidade <- round((indicefavorabilidade),digits=2) 

   return(indicefavorabilidade)
}

getPositionY <- function(test){
   labelpos <- array(NA,length(test$polarization))
   labelpos[ which(test$polarization == "positiva") ] <- "0.02"
   labelpos[ which(test$polarization == "negativa") ] <- "0.98"
   datasb <- test$Data[which(test$polarization == "neutra")];
   posvarb <- which(test$polarization == "neutra");
   for(i in 1:length(datasb)){
      datasb_ <- datasb[i];
      positionobsb <- which(test$Data == datasb_ & test$polarization == "positiva")
      obsb <- ifelse(length(positionobsb) > 0, test$freq[positionobsb], 0);
      labelpos[posvarb[i]] <- obsb + 0.02
   }
   return(as.numeric(labelpos))
}

removeUsersOutliers <- function(file,percent){
   ncommentoutliers <- as.numeric(quantile(as.numeric(table(file$user_id)),percent))
   usersoutliers <- names(table(file$user_id)[which(as.numeric(table(file$user_id)) > ncommentoutliers)]);
   file <- file %>% filter(!(user_id %in% usersoutliers))
   return(file)
}

getDFMatrix <- function(text){

   text <- stringr::str_replace_all(text,"[\\s]+", " ")
   text=str_replace_all(text,"[^[:graph:]]", " ") 
   # Split it
   text <- stringi::stri_trans_general(text, "latin-ascii")
   text <- removePunctuation(text)
   text <- unlist(stringr::str_split(text, " "))
   # Get rid of trailing "" if necessary
   text <- rm_nchar_words(text, "1")
   text <- rm_nchar_words(text, "2")
   text <- rm_nchar_words(text, "3")
   indexes <- which(text == "")
   if(length(indexes) > 0){
      text <- text[-indexes]
   }
   myCorpus <- corpus(text)
   metadoc(myCorpus, "language") <- "portuguese"
   mydfm <- dfm(myCorpus, remove = c(stopwords("portuguese"),badwords), remove_punct = TRUE, remove_numbers= TRUE)
   return(mydfm)
   #   ap_td <- tidy(mydfm)
   #   names(ap_td) <- c("sentimento","term","count")
   #   return(ap_td);
}

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
   if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
   hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

function(input, output) {
   
   plotIndiceFavorabilidade = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      allpolarization <- toupper(file$polarization)
      isent <- getIndiceDeFavorabilidade(allpolarization);

      colfunc <- colorRampPalette(c(corpositivo,corneutro))
      legend_image <- as.raster(matrix(colfunc(20), ncol=1))
      plot(c(1,20),c(0,10),type = 'n', axes = F,xlab = '', ylab = '', main = '')
      text(x=3, y = 10*signif(isent,2), labels = paste(intToUtf8(9664),paste0(signif(isent,2))),pos=4)
      text(x = 0.45, y=10,  labels = 1,pos = 4)
      text(x = 0.4, y=0, labels = 0,pos = 4)
      rasterImage(legend_image, 1,0.1,3,9.9)
   }

   plotDetratoresApoiadores = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      file %>% 
         dplyr::select(user_id, polarization) %>%
         group_by(user_id) %>% 
         count(user_id, polarization) %>%
         arrange(n, user_id) %>%
         tail(50) %>% 
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(user_id,as.numeric(n)), y = as.numeric(n), fill = polarization)) + 
         ylab("Número de comentários") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = corpositivo, "Negativo" = cornegativo, "Neutro" = corneutro)) +
         geom_text( aes (x = reorder(user_id,as.numeric(n)), y = as.numeric(n), label = as.numeric(n) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip() +
        theme_bw()
   }
   
   plotDetratoresAssiduos = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      file %>% 
         dplyr::select(user_id, polarization) %>%
         group_by(user_id, polarization) %>% 
         arrange(user_id, polarization) %>%
         filter(toupper(polarization) == "NEGATIVO") %>%
         arrange(user_id) %>%
         summarise(total = n()) %>%
         arrange(total) %>%
         filter(total >= 1) %>%
         tail(50) %>%
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(user_id,as.numeric(total)), y = as.numeric(total), fill = polarization)
         ) + 
         ylab("Número de Comentários") +
         xlab("") +
         labs(title = "Principais detratores")+
         scale_fill_manual("Sentimento", values = c("Positivo" = corpositivo, "Negativo" = cornegativo, "Neutro" = corneutro)) +
         geom_text( aes (x = reorder(user_id,as.numeric(total)), y = as.numeric(total), label = as.numeric(total) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip() +
        theme_bw()
   }

   plotApoiadoresAssiduos = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      file %>% 
         dplyr::select(user_id, polarization) %>%
         group_by(user_id, polarization) %>% 
         arrange(user_id, polarization) %>%
         filter(toupper(polarization) == "POSITIVO") %>%
         arrange(user_id) %>%
         summarise(total = n()) %>%
         arrange(total) %>%
         filter(total >= 1) %>%
         tail(30) %>%
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(user_id,as.numeric(total)), y = as.numeric(total), fill = polarization)
         ) + 
         ylab("Número de Comentários") +
         xlab("") +
         labs(title = "Principais apoiadores")+
         scale_fill_manual("Sentimento", values = c("Positivo" = corpositivo, "Negativo" = cornegativo, "Neutro" = corneutro)) +
         geom_text( aes (x = reorder(user_id,as.numeric(total)), y = as.numeric(total), label = as.numeric(total) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip() +
        theme_bw()
   }
   
   plotPalavrasDetratores = function(){
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
   text <- file %>%
         filter(toupper(polarization) == "NEGATIVO") %>%
         dplyr::select(text) %>%
         toupper()
      
      mydfm <- getDFMatrix(text);
      words_td <- topfeatures(mydfm,30)
      ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(names(words_td),as.numeric(words_td)), y = as.numeric(words_td)),
                  fill = cornegativo) + 
         ylab("Número de ocorrências") +
         xlab("") +
        labs(title = "Palavras mais citadas por detratores")+
         geom_text( aes (x = reorder(names(words_td),as.numeric(words_td)), y = words_td, label = words_td ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip() +
        theme_bw()      
      
   }
   
   plotPalavrasApoiadores = function(){
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      autores <- file %>% 
         filter(toupper(polarization) == "POSITIVO") %>%
         group_by(user, polarization) %>% 
         summarise(total = n()) %>%
         arrange(total) %>%
         filter(total > 3) %>%
         tail(50) %>%
         dplyr::select(user)
      
      text <- file %>%
         filter(as.character(user) %in% autores$user, 
                toupper(polarization) == "POSITIVO") %>%
         dplyr::select(text) %>%
         toupper()
      
      mydfm <- getDFMatrix(text);
      words_td <- topfeatures(mydfm, 30)
      ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(names(words_td),as.numeric(words_td)), y = as.numeric(words_td)),
                  fill = corpositivo) + 
         ylab("Número de ocorrências") +
         xlab("") + ggtitle("Palavras mais citadas por apoiadores") +
         geom_text( aes (x = reorder(names(words_td),as.numeric(words_td)), y = words_td, label = words_td ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip() +
        theme_bw()
   }
      
   plotSerieTemporal = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)

      amostra <- file
      df_datas <- amostra %>%
         mutate( Data = ymd_hms(created_at) %>%
                    as.Date() %>%
                    format("%d/%m/%Y"),
                 polarization = as.factor(toupper(polarization))
         ) %>%
         group_by(Data, polarization) %>%
         summarise(count = n()) %>%
         group_by(Data) %>%
         mutate(freq = count / sum(count))
      
      primeirodia <- min(dmy(df_datas$Data));
      ultimodia <- max(dmy(df_datas$Data))
      ggplot(df_datas, aes(x=dmy(Data), y=freq, fill=polarization)) +
         geom_bar(position = "stack", stat = "identity") +
         scale_x_date(date_breaks = "1 day",
                      labels = date_format("%d/%m/%Y")) +
         theme(text = element_text(size=6), axis.text.x = element_text(angle=45, hjust=1)) +
         scale_y_continuous(labels=scales::percent) +
         labs (title = "") +
         labs (x = "", y = "Porcentagem de Posts") +
         theme(text = element_text(size=6), axis.text.x = element_text(angle=45, hjust=1)) +
         coord_cartesian(xlim = c(primeirodia, ultimodia)) +
         scale_fill_manual("Polaridade", values = c("POSITIVO" = corpositivo, "NEGATIVO" = cornegativo, "NEUTRO" = corneutro, "GOVERNO" = ggplotColours(n=4)[4])) +
         geom_text(size = 2, col = "white", aes(x = dmy(Data), y = getPositionY(df_datas), label = paste(as.character(100*round(df_datas$freq,2)),"%",sep=""))) + theme_bw();
      
   }
   
   plotPalavras = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      text <- toupper(file$text)
      unigram <- getUnigram(text)
      unigram <- unigram %>% 
        filter(!(words %in% badwords))%>% 
        filter(!is.na(words)) %>% 
        group_by(words) %>% 
        summarise(nwords = n()) %>% 
        arrange(nwords) %>% tail(30)
    unigram %>% 
      ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(words,nwords), y = nwords),
                  fill = "magenta") + 
         ylab("Número de ocorrências") +
         xlab("") +
         labs(title = "Palavras mais citadas")+
         geom_text( aes (x = reorder(words,nwords), y = nwords, label = nwords ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
   
   plotPalavrasNegativas = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      text <- toupper(file$text[which(toupper(file$polarization) == "NEGATIVO")])
      unigram <- getUnigram(text)
      unigram <- unigram %>% 
        filter(!(words %in% badwords))%>% 
        filter(!is.na(words)) %>% 
        group_by(words) %>% 
        summarise(nwords = n()) %>% 
        arrange(nwords) %>% tail(30)
      unigram %>% 
        ggplot() + 
        geom_bar(stat = "identity", 
                 aes(x = reorder(words,nwords), y = nwords),
                 fill = cornegativo) + 
        ylab("Número de ocorrências") +
        xlab("") +
        labs(title = "Palavras mais citadas em comentários negativos")+
        geom_text( aes (x = reorder(words,nwords), y = nwords, label = nwords ) , vjust = 0, hjust = 0, size = 2 ) + 
        coord_flip()
   }   
   
   plotPalavrasPositivas = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      text <- toupper(file$text[which(toupper(file$polarization) == "POSITIVO")])
      unigram <- getUnigram(text)
      unigram <- unigram %>% 
        filter(!(words %in% badwords))%>% 
        filter(!is.na(words)) %>% 
        group_by(words) %>% 
        summarise(nwords = n()) %>% 
        arrange(nwords) %>% tail(30)
      unigram %>% 
        ggplot() + 
        geom_bar(stat = "identity", 
                 aes(x = reorder(words,nwords), y = nwords),
                 fill = corpositivo) + 
        ylab("Número de ocorrências") +
        xlab("") +
        labs(title = "Palavras mais citadas em comentários positivos")+
        
        geom_text( aes (x = reorder(words,nwords), y = nwords, label = nwords ) , vjust = 0, hjust = 0, size = 2 ) + 
        coord_flip()
   }
   
   plotPalavrasNeutras = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      text <- toupper(file$text[which(toupper(file$polarization) == "NEUTRO")])
      unigram <- getUnigram(text)
      unigram <- unigram %>% 
        filter(!(words %in% badwords))%>% 
        filter(!is.na(words)) %>% 
        group_by(words) %>% 
        summarise(nwords = n()) %>% 
        arrange(nwords) %>% tail(30)
      unigram %>% 
        ggplot() + 
        geom_bar(stat = "identity", 
                 aes(x = reorder(words,nwords), y = nwords),
                 fill = corneutro) + 
        ylab("Número de ocorrências") +
        xlab("") +
        labs(title = "Palavras mais citadas em comentários neutros")+
        
        geom_text( aes (x = reorder(words,nwords), y = nwords, label = nwords ) , vjust = 0, hjust = 0, size = 2 ) + 
        coord_flip()
   }

   plotWordcloud = function(){
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      text <- toupper(file$text)
      mydfm <- getDFMatrix(text);
      set.seed(100)
      if(dim(mydfm)[1] <= 1000){
         textplot_wordcloud(mydfm, min_count = 1, colorrandom_orderrotation = FALSE,
                            rot.per = .25, 
                            colors = RColorBrewer::brewer.pal(8,"Dark2")[4:8])        
      }else{
         textplot_wordcloud(mydfm[1:1000], min_count = 1, colorrandom_orderrotation = FALSE,
                            rot.per = .25, 
                            colors = RColorBrewer::brewer.pal(8,"Dark2")[4:8])        
      }
      
   }
      
   plotWordcloudNegativo = function(){
     filepath <- input$file$datapath
     file <- read_xlsx(filepath)
     text <- toupper(file$text[which(toupper(file$polarization) == "NEGATIVO")])
     mydfm <- getDFMatrix(text);
     set.seed(100)
     if(dim(mydfm)[1] <= 1000){
       textplot_wordcloud(mydfm, min_count = 3, colorrandom_orderrotation = FALSE,rot.per = .25,colors = RColorBrewer::brewer.pal(8,"Reds")[4:8])        
     }else{
       textplot_wordcloud(mydfm[1:1000], min_count = 3, colorrandom_orderrotation = FALSE,rot.per = .25,colors = RColorBrewer::brewer.pal(8,"Reds")[4:8])        
     }
  }
  
  plotWordcloudPositivo = function(){
     filepath <- input$file$datapath
     file <- read_xlsx(filepath)
     text <- toupper(file$text[which(toupper(file$polarization) == "POSITIVO")])
     mydfm <- getDFMatrix(text);
     set.seed(100)
     if(dim(mydfm)[1] <= 1000){
      textplot_wordcloud(mydfm, min_count = 3, colorrandom_orderrotation = FALSE,rot.per = .25,colors = RColorBrewer::brewer.pal(8,"Blues")[4:8])       
     }else{
       textplot_wordcloud(mydfm[1:1000], min_count = 3, colorrandom_orderrotation = FALSE,rot.per = .25,colors = RColorBrewer::brewer.pal(8,"Blues")[4:8])        
     }
     
  }
  
  plotWordcloudNeutro = function(){
     filepath <- input$file$datapath
     file <- read_xlsx(filepath)
     text <- toupper(file$text[which(toupper(file$polarization) == "NEUTRO")])
     mydfm <- getDFMatrix(text);
     set.seed(100)
     if(dim(mydfm)[1] <= 1000){
       textplot_wordcloud(mydfm, min_count= 3, colorrandom_orderrotation = FALSE,rot.per = .25,colors = RColorBrewer::brewer.pal(8,"Greens")[4:8])        
     }else{
       textplot_wordcloud(mydfm[1:1000], min_count = 3, colorrandom_orderrotation = FALSE,rot.per = .25,colors = RColorBrewer::brewer.pal(8,"Greens")[4:8])        
     }
  }

## Treemap
   plotTreemap = function(){
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      text <- toupper(file$text)
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
      ggplot(unigram, aes(area = palavras, 
                          fill = palette[as.numeric(unigram$classe)], 
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
   }
      
   plotTreemapNegativo = function(){
     filepath <- input$file$datapath
     file <- read_xlsx(filepath)
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
  }
  
  plotTreemapPositivo = function(){
     filepath <- input$file$datapath
     file <- read_xlsx(filepath)
     text <- toupper(file$text[which(toupper(file$polarization) == "POSITIVO")])
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
  }
  
  plotTreemapNeutro = function(){
     filepath <- input$file$datapath
     file <- read_xlsx(filepath)
     text <- toupper(file$text[which(toupper(file$polarization) == "NEUTRO")])
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
     colfunc <- colorRampPalette(c(corneutro))
     
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
     }
  
  
  ###### Outputs
  
  output$indicefavorabilidade = downloadHandler(
    filename = 'indicefavorabilidade.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 16, height = 9,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotIndiceFavorabilidade(), device = device)
    })

  output$variabilidade = downloadHandler(
     filename = function() {
        paste("variabilidade.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = 16, height = 9,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotVariabilidade(), device = device)
        
     }
  )

  output$comentaristaspopulares = downloadHandler(
     filename = function() {
        paste("comentaristaspopulares.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotComentaristasPopulares(), device = device)
        
     }     
  )
    
  output$detratoresapoiadores = downloadHandler(
     filename = function() {
        paste("detratoresapoiadores.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotDetratoresApoiadores(), device = device)
        
     }     
  )
  
  output$detratorescurtidos = downloadHandler(
     filename = function() {
        paste("detratorescurtidos.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
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
