######################################################################################
######################################################################################
# PROJETO DA DISCIPLINA DE 'O CLIMA NOS SISTEMAS AMBIENTAIS'
# PPG - CTA - UFABC
######################################################################################
# Script desenvolvido por: AMANDA RODRIGUES DE SOUZA
# amanda.souza@ufabc.edu.br
######################################################################################
# Data da últimaa modificação: 2021-10-05
######################################################################################
######################################################################################

citation()

#install.packages("readxl")
#install.packages("writexl")
#install.packages("gplots")
#install.packages("lattice")
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("openxlsx")
#install.packages("extrafont")

#install.packages("extrafont")
library("readxl") # ler xls
library("writexl") # escrever 
library("gplots")  # gráficos
library("lattice") #visualização de dados
library("stringr") # trabalhar com string
library("ggplot2")
library("openxlsx") #para escrever em xlsx linha por linha
library("extrafont") # pacote com fontes para utilizar nos gráficos
####################### ATENÇÃO AQUI ###########################################
#extrafont::font_import() # pedirá confirmação para baixar [pode demorar]
# é necessário importar apenas uma vez
################################################################################
extrafont::loadfonts()
# OBS: para importar os pacotes, pode-se ou não usar as aspas

pastaScripts = 'C:/Users/amand/OneDrive/Área de Trabalho/Dados Projeto _ IAG'
setwd(pastaScripts)

arquivoEntrada = "Dados_IAG_Prec_1933_2020.xlsx"
#
nameDirBP = "C:/Users/amand/OneDrive/Área de Trabalho/Dados Projeto _ IAG/Resultados"
                    #caminho relativo para a criação da pasta

anoDeInicio = 1933
anoDeFim = 2020

windowsFonts(A = windowsFont("Times New Roman")) # Fonte dos gráficos
# para escolher outra fonte, use o comando font() e veja a lista, e troque no windowsFont()

cinza = rgb(red = 0.45,blue = 0.45,green = 0.45) # "setei" um tom de cinza diferente para preencher os gráficos
# para mudar a cor das barras e linhas do gráfico, altere em 'col'


# Cores: Segue a mesma ordem do vetor 'classificacao'
#                            MB - B - N - A - MA
coresClasses = c("DarkOrange","Gold","PaleGreen1", "LightSkyBlue", "DodgerBlue")

##
# tabela de cores: https://expanssiva.com.br/pg/tabela-de-cores-html-hexadecimal-e-rgb

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# criando o diretório para os resultados
if(!dir.exists(nameDirBP)) # se a pasta não existe
    dir.create(nameDirBP)

seriesVazao = read_excel(arquivoEntrada)
seriesVazao = subset(seriesVazao, seriesVazao[1]>=anoDeInicio)
seriesVazao = subset(seriesVazao, seriesVazao[1]<=anoDeFim)

ptObs=3    
estacao = NULL #  zerando a variável 
estacao = na.omit(data.frame(seriesVazao[1:2],seriesVazao[ptObs]))
                # excluindo as linhas com NA
    
nomePt = colnames(seriesVazao[ptObs])
    # PRECAUÇÃO: as vezes os nomes das colunas vem com ponto no lugar do espaço.
    # A função str_replace_all vai substituir [.] por [ ]
    
nameDir = str_replace_all(colnames(seriesVazao[ptObs]),"[[:blank:],[.]]", "")
    # tirando pontos e espaços para usar como nome de arquivo
path = paste(nameDirBP,"/",nameDir,sep='')
    # a função paste vai agrupar os argumentos, utilizando o separador indicado
    # o default de separador é um espaço simples

    #criando uma pasta para cada estação
if(!dir.exists(path)) # se a pasta não existe
    dir.create(path) # a pasta é criada

nameFileBP = paste(path, "/boxplot_",nameDir,".jpg",sep='')
    #criando o nome do gráfico boxplot

nameFileBar = paste(path, "/barra_media_",nameDir,".jpg",sep='')
    #criando o nome do gráfico barra média
    
nameFileQuantil = paste(path, "/quantil_media_",nameDir,".jpg",sep='')
    #criando o nome do gráfico barra média com quantil
    
nameFileQtdEventoMB = paste(path, "/barra_quantidadeEvento_MB_",nameDir,".jpg",sep='')
nameFileQtdEventoB = paste(path, "/barra_quantidadeEvento_B_",nameDir,".jpg",sep='')
nameFileQtdEventoN = paste(path, "/barra_quantidadeEvento_N_",nameDir,".jpg",sep='')
nameFileQtdEventoA = paste(path, "/barra_quantidadeEvento_A_",nameDir,".jpg",sep='')
nameFileQtdEventoMA = paste(path, "/barra_quantidadeEvento_MA_",nameDir,".jpg",sep='')

bp = NULL #  zerando a variável 
 
##################CONSTRUINDO O GRÁFICO BOXPLOT ########################################
graphics.off()
#family = fonte das letras
jpeg(filename = nameFileBP, width = 960, height = 660,family = "A",
    units = "px", pointsize = 20, quality = 100,  bg = "white",  res = NA)
 
bp <- boxplot(estacao[3][estacao$"MÊS" ==1,],
              estacao[3][estacao$"MÊS" ==2,],
              estacao[3][estacao$"MÊS" ==3,],
              estacao[3][estacao$"MÊS" ==4,],
              estacao[3][estacao$"MÊS" ==5,],
              estacao[3][estacao$"MÊS" ==6,],
              estacao[3][estacao$"MÊS" ==7,],
              estacao[3][estacao$"MÊS" ==8,],
              estacao[3][estacao$"MÊS" ==9,],
              estacao[3][estacao$"MÊS" ==10,],
              estacao[3][estacao$"MÊS" ==11,],
              estacao[3][estacao$"MÊS" ==12,],
                  main= nomePt,
                  col = cinza,
                  pch = 16, #plotting character, para outras opções digite '?pch'
                  names= c(1:12),
                  xlab = "Mês", ylab = "Precipitação")
dev.off() #terminou o gráfico Boxplot
    

##################CONSTRUINDO O GRÁFICO DE BARRA - média ################################
media = NULL
media = c(mean(estacao[3][estacao$"MÊS" == 1,]),mean(estacao[3][estacao$"MÊS" == 2,]),
          mean(estacao[3][estacao$"MÊS" == 3,]),mean(estacao[3][estacao$"MÊS" == 4,]),
          mean(estacao[3][estacao$"MÊS" == 5,]),mean(estacao[3][estacao$"MÊS" == 6,]),
          mean(estacao[3][estacao$"MÊS" == 7,]),mean(estacao[3][estacao$"MÊS" == 8,]),
          mean(estacao[3][estacao$"MÊS" == 9,]),mean(estacao[3][estacao$"MÊS" ==10,]),
          mean(estacao[3][estacao$"MÊS" ==11,]),mean(estacao[3][estacao$"MÊS" ==12,]))

   
############################### CONSTRUINDO O SAÍDA DOS QUANTIS ###############################
q15= NULL
q35= NULL
q65= NULL
q85= NULL
    
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
mesesDoAno = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio","Junho",
               "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
writeData(wb, "Sheet1", "15%", startCol = 1, startRow = 2, rowNames = TRUE)
writeData(wb, "Sheet1", "35%", startCol = 1, startRow = 3, rowNames = TRUE)
writeData(wb, "Sheet1", "65%", startCol = 1, startRow = 4, rowNames = TRUE)
writeData(wb, "Sheet1", "85%", startCol = 1, startRow = 5, rowNames = TRUE)

for(i in 1:12) # meses do ano
{
    writeData(wb, "Sheet1", mesesDoAno[i] , startCol = 1+i, startRow = 1, rowNames = TRUE)
        # calculando os quantis
    q15[i] = quantile(estacao[3][estacao$"MÊS" == i,],  probs = 0.15)
    q35[i] = quantile(estacao[3][estacao$"MÊS" == i,],  probs = 0.35)
    q65[i] = quantile(estacao[3][estacao$"MÊS" == i,],  probs = 0.65)
    q85[i] = quantile(estacao[3][estacao$"MÊS" == i,],  probs = 0.85)
    #quantil = quantile(estacao[3][estacao$"MÊS" == i,],  probs = c(0.15, 0.35,0.65,0.85))
        
    writeData(wb, "Sheet1", q15[i], startCol = 1+i, startRow = 2, rowNames = TRUE)
    writeData(wb, "Sheet1", q35[i], startCol = 1+i, startRow = 3, rowNames = TRUE)
    writeData(wb, "Sheet1", q65[i], startCol = 1+i, startRow = 4, rowNames = TRUE)
    writeData(wb, "Sheet1", q85[i], startCol = 1+i, startRow = 5, rowNames = TRUE)
}
#salvando no arquivo
saveWorkbook(wb, paste(path,"/ValoresQuantil_",nameDir,".xlsx",sep=''), overwrite = TRUE)
    

##################CONSTRUINDO O GRÁFICO MÉDIA COM QUANTIS ###############################

charMeses = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN","JUL","AGO", "SET","OUT","NOV","DEZ")

graphics.off()
jpeg(filename = nameFileQuantil, width = 1200, height = 680,
    units = "px", pointsize = 20, 
    quality = 100,  bg = "white",  res = NA)
    
plot(media~c(1:12),
    main = nomePt, type = "h",lwd = 25, col = cinza ,pch = 1,omi=c(5,60,4,2),
    xaxt = "n", family = "A", cex.axis = 1.25, cex.lab = 1.5,cex.main = 2,
    ylab = expression(paste("Precipitação (", mm,")", sep=" ")), xlab = "Mês",par(mar=c(5,4,4,2)+0.6),
    ylim=range(min(q15):(max(q85)*1.05)))
    
    #O proporcional no limite máximo é para a legenda não sobrepor o gráficos
axis(side=1, at=seq(1,12,1), labels=charMeses, cex.axis = 1.25, family="A")
lines(q15, col = "blue",lwd = 3) #lwd = espessura da linha
lines(q35, col = "green", lwd = 3)
lines(q65, col = "orange", lwd = 3)
lines(q85, col = "red", lwd = 3)
legend("top", c("Média", "Quantil de 15%", "Quantil de 35%","Quantil de 65%", "Quantil de 85%"),
       fill=c(cinza, "blue", "green", "orange","red"),text.font = 6,cex = 1.15,
       xpd=NA,horiz=TRUE, bty='n')
### text.font= 6  -> times new roman

dev.off() #terminou o gráfico de barra com quantis em linhas

   
############################## CLASSIFICAÇÃO - QUANTIS #######################################
classificacao = c("Muito Baixa","Baixa","Normal","Alta","Muito Alta")
    
MuitoBaixa = NULL
Baixa      = NULL
Normal     = NULL
Alta       = NULL
MuitoAlta  = NULL
    
    #estacao[LINHA,COLUNA]
    #COLUNA 1 = ANO
    #COLUNA 2 = MÊS
    #COLUNA 3 = VAZAO
    #COLUNA 4 = CLASSIFICAÇÃO
    # ou seja:
    # estacao[i,3] -> vazão na linha i
    # estacao[i,2] -> o mês da linha i
    
for(i in 1:nrow(estacao))
{ # loop que percorre as linhas do dataframe estação
    if(estacao[i,3] <= q15[estacao[i,2]])# se o valor da linha i é menor que o q15 do mês da linha i
    { # A classificação da vazão vai ser "Muito Baixa"
        estacao$"Classificação"[i] = classificacao[1]
    }else # caso não for, vai entrar no primeiro else
    {
        # se o valor é maior que o q15 do mês E menor que o q35 do mês
        if(estacao[i,3] > q15[estacao[i,2]] && estacao[i,3]<= q35[estacao[i,2]])
        {# Classificação = Baixa
            estacao$"Classificação"[i]= classificacao[2]
        }else # caso não for, também vai entrar no segundo else
        {
            # se o valor for maior que o q35 E menor que o q65 do mês
            if(estacao[i,3] > q35[estacao[i,2]] && estacao[i,3]<= q65[estacao[i,2]])
            {# classificacao=Normal
                estacao$"Classificação"[i] = classificacao[3]
            }else # caso não for, também vai entrar no terceiro else
            {
                # se o valor for maior que q65 e menor que o q85
                if(estacao[i,3] > q65[estacao[i,2]] && estacao[i,3]<= q85[estacao[i,2]])
                {# classificação = Alta
                    estacao$"Classificação"[i] = classificacao[4]
                }else # caso não for, também vai entrar no quarto else
                {# se o valor for maior que o q85 do mês
                    if(estacao[i,3]> q85[estacao[i,2]])
                    {# classificação = Muito Alta
                        estacao$"Classificação"[i] = classificacao[5]
                    }else # testando se está funcionando
                    { # se estiver tudo certo, não aparecerá nenhum print
                        print(i)
                    } # fim do else de teste
                }# fim do quarto else
            }# fim do terceiro else
        }# fim do segundo else
    }# fim do primeiro else    
} # fim do for que percorre as linhas da estacao
# agora o dataframe estação tem uma coluna a mais, chamada "Classificação"

# filtrando os dados pela classificação
# foi criado um dataframe para cada uma das classificações
# o dataframe tem as mesmas colunas que o df da estacao
MuitoBaixa = subset(estacao, estacao[4]==classificacao[1])
Baixa = subset(estacao, estacao[4]==classificacao[2])
Normal = subset(estacao, estacao[4]==classificacao[3])
Alta = subset(estacao, estacao[4]==classificacao[4])
MuitoAlta = subset(estacao, estacao[4]==classificacao[5])

    # salvando os dataframes em planilas
write_xlsx(MuitoBaixa, paste(path,"/MuitoBaixa_",nameDir,".xlsx",sep=''), col_names=TRUE)
write_xlsx(Baixa, paste(path,"/Baixa_",nameDir,".xlsx",sep=''), col_names=TRUE)
write_xlsx(Normal, paste(path,"/Normal_",nameDir,".xlsx",sep=''), col_names=TRUE)
write_xlsx(Alta, paste(path,"/Alta_",nameDir,".xlsx",sep=''), col_names=TRUE)
write_xlsx(MuitoAlta, paste(path,"/MuitoAlta_",nameDir,".xlsx",sep=''), col_names=TRUE)
write_xlsx(estacao, paste(path,"/Total_",nameDir,".xlsx",sep=''), col_names=TRUE)  
    
    # EXPORTANDO OS DADOS
    #planilha com as quantidades de cada uma das classificações por mês e o total
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
for(i in 1:length(classificacao))
    writeData(wb, "Sheet1", classificacao[i] , startCol = 1+i, startRow = 1, rowNames = TRUE)
for(i in 1:12) # valores por mês
{
    writeData(wb, "Sheet1", i, startCol = 1, startRow = 1+i, rowNames = TRUE)
    writeData(wb, "Sheet1", nrow(subset(MuitoBaixa,MuitoBaixa[2]==i)),
            startCol = 2, startRow = 1+i, rowNames = TRUE)
    writeData(wb, "Sheet1", nrow(subset(Baixa,Baixa[2]==i)),
            startCol = 3, startRow = 1+i, rowNames = TRUE)
    writeData(wb, "Sheet1", nrow(subset(Normal,Normal[2]==i)),
            startCol = 4, startRow = 1+i, rowNames = TRUE)
    writeData(wb, "Sheet1", nrow(subset(Alta,Alta[2]==i)),
            startCol = 5, startRow = 1+i, rowNames = TRUE)
    writeData(wb, "Sheet1", nrow(subset(MuitoAlta,MuitoAlta[2]==i)),
            startCol = 6, startRow = 1+i, rowNames = TRUE)
}
# total
writeData(wb, "Sheet1", "Total", startCol = 1, startRow = 14, rowNames = TRUE)
writeData(wb, "Sheet1", nrow(MuitoBaixa), startCol = 2, startRow = 14, rowNames = TRUE)
writeData(wb, "Sheet1", nrow(Baixa), startCol = 3, startRow = 14, rowNames = TRUE)
writeData(wb, "Sheet1", nrow(Normal), startCol = 4, startRow = 14, rowNames = TRUE)
writeData(wb, "Sheet1", nrow(Alta), startCol = 5, startRow = 14, rowNames = TRUE)
writeData(wb, "Sheet1", nrow(MuitoAlta), startCol = 6, startRow = 14, rowNames = TRUE)

    #salvando no arquivo
saveWorkbook(wb, paste(path,"/QuantidadeQuantil_",nameDir,".xlsx",sep=''), overwrite = TRUE) 


########################### GRÁFICOS DA QUANTIDADE DE EVENTO POR ANO #########################
contEventosMB = NULL
contEventosB  = NULL
contEventosN  = NULL
contEventosA  = NULL
contEventosMA = NULL
    
periodo=c(anoDeInicio:anoDeFim)
for(i in c(1:length(periodo)))
{
    contEventosMB[i] = nrow(subset(MuitoBaixa,MuitoBaixa[1]==periodo[i]))
    contEventosB[i]  = nrow(subset(Baixa,Baixa[1]==periodo[i]))
    contEventosN[i]  = nrow(subset(Normal,Normal[1]==periodo[i]))
    contEventosA[i] = nrow(subset(Alta,Alta[1]==periodo[i]))
    contEventosMA[i] = nrow(subset(MuitoAlta,MuitoAlta[1]==periodo[i]))
}
quantEventos= data.frame(MB = contEventosMB, B =contEventosB, N = contEventosN, A = contEventosA, MA = contEventosMA)

graphics.off() # MUITO BAIXA
jpeg(filename = nameFileQtdEventoMB, width = 1800, height = 680,
     units = "px", pointsize = 20, 
    quality = 100,  bg = "white",  res = NA)
barplot(quantEventos[,1]~periodo, main=paste("Eventos de Precipitação Muito Baixa:",nomePt),
            ylim=range(0:12),omi=c(5,60,4,2),family = "A",las=2,#xaxt = "n",
            cex.axis = 1.25, cex.lab = 1.5,cex.main = 2, par(mar=c(5,4,4,2)+0.6),
            ylab="Quantidade de Eventos",xlab="Ano")
grid(nx=NA, ny=NULL)
dev.off() #terminou o gráfico barplot

graphics.off() # BAIXA
jpeg(filename = nameFileQtdEventoB, width = 1800, height = 680,
            units = "px", pointsize = 20, 
            quality = 100,  bg = "white",  res = NA)
barplot(quantEventos[,2]~periodo, main=paste("Eventos de Precipitação Baixa:",nomePt),
        ylim=range(0:12),omi=c(5,60,4,2),family = "A",las=2,#xaxt = "n",
        cex.axis = 1.25, cex.lab = 1.5,cex.main = 2, par(mar=c(5,4,4,2)+0.6),
        ylab="Quantidade de Eventos",xlab="Ano")
grid(nx=NA, ny=NULL)
dev.off() #terminou o gráfico barplot

graphics.off() # NORMAL
jpeg(filename = nameFileQtdEventoN, width = 1800, height = 680,
     units = "px", pointsize = 20, 
     quality = 100,  bg = "white",  res = NA)
barplot(quantEventos[,3]~periodo, main=paste("Eventos de Precipitação Normal:",nomePt),
        ylim=range(0:12),omi=c(5,60,4,2),family = "A",las=2,#xaxt = "n",
        cex.axis = 1.25, cex.lab = 1.5,cex.main = 2, par(mar=c(5,4,4,2)+0.6),
        ylab="Quantidade de Eventos",xlab="Ano")
grid(nx=NA, ny=NULL)
dev.off() #terminou o gráfico barplot

graphics.off() # ALTA
jpeg(filename = nameFileQtdEventoA, width = 1800, height = 680,
    units = "px", pointsize = 20, 
    quality = 100,  bg = "white",  res = NA)
barplot(quantEventos[,4]~periodo, main=paste("Eventos de Precipitação Alta:",nomePt),
        ylim=range(0:12),omi=c(5,60,4,2),family = "A",las=2,#xaxt = "n",
        cex.axis = 1.25, cex.lab = 1.5,cex.main = 2, par(mar=c(5,4,4,2)+0.6),
        ylab="Quantidade de Eventos",xlab="Ano")
grid(nx=NA, ny=NULL)
dev.off() #terminou o gráfico barplot

graphics.off() # MUITO ALTA
jpeg(filename = nameFileQtdEventoMA, width = 1800, height = 680,
     units = "px", pointsize = 20, 
     quality = 100,  bg = "white",  res = NA)
barplot(quantEventos[,5]~periodo, main=paste("Eventos de Precipitação Muito Alta:",nomePt),
        ylim=range(0:12),omi=c(5,60,4,2),family = "A",las=2,#xaxt = "n",
        cex.axis = 1.25, cex.lab = 1.5,cex.main = 2, par(mar=c(5,4,4,2)+0.6),
        ylab="Quantidade de Eventos",xlab="Ano")
grid(nx=NA, ny=NULL)
dev.off() #terminou o gráfico barplot    
    

    ########################### MAPA DE CLASSES ###########################################

nomeTitulo = paste("Classificação da Precipitação:", nomePt)
nameFileBitMap = paste(path, "/bitmap_",nameDir,".jpg",sep='')
graphics.off()

myplot1 <- ggplot(estacao,aes(x=ANO, y=MÊS,fill = Classificação)) + #iniciando o plot

coord_fixed(ratio=1.7)+ # proporção do "pixels"
    # maior que 1 => retângulo com altura maiores que a largura
    # igual a 1   => quadrado
    # menor que 1 => retângulo com largura maior que a altura
theme(# formatação
    #axis.text.x = element_text( angle = 320), #escrita do eixo x na vertical
    #axis.text.y = element_text( face = "bold"),
    plot.title = element_text(hjust = 0.5), # centralizar título
    text = element_text(size=15),# tamanho da letra e negrito

    legend.key = element_rect(fill = "white", colour = cinza), #caixa de legenda
    legend.position="bottom", # posição da caixa de legenda
    panel.background = element_blank())+ # tirando o fundo do gráfico

geom_raster()+
geom_tile(color="grey")+ #linha que separa dos "pixels"
labs(x = "Ano", y = "Mês",fill = "",title =nomeTitulo) +

scale_fill_manual(values=coresClasses,breaks=classificacao) +

scale_x_continuous(breaks = seq(anoDeInicio,anoDeFim,5)) +

scale_y_continuous(breaks = c(1:12))

jpeg(filename = nameFileBitMap, width = 630, height = 300,family = "A",
         units = "px", quality = 100,  bg = "white",  res = NA)
print(myplot1)
dev.off() #terminou o gráfico 
    
    ##################### FIM DO MAPA DE CLASSES ############################################ 

estacao1 = estacao[1:264,] ## separando os dados para separar os gráficos
    ########################### MAPA DE CLASSES ###########################################

nomeTitulo = paste("Classificação da Precipitação de",estacao1[1,1],'a',estacao1[264,1])
nameFileBitMap = paste(path, "/bitmap_",estacao1[1,1],"_",estacao1[264,1],".jpg",sep='')
graphics.off()

myplot1 <- ggplot(estacao1,aes(x=ANO, y=MÊS,fill = Classificação)) + #iniciando o plot

coord_fixed(ratio=0.8)+ # proporção do "pixels"
    # maior que 1 => retângulo com altura maiores que a largura
    # igual a 1   => quadrado
    # menor que 1 => retângulo com largura maior que a altura
theme(# formatação
    #axis.text.x = element_text( angle = 320), #escrita do eixo x na vertical
    #axis.text.y = element_text( face = "bold"),
    plot.title = element_text(hjust = 0.5), # centralizar título
    text = element_text(size=20),# tamanho da letra e negrito

    legend.key = element_rect(fill = "white", colour = cinza), #caixa de legenda
    legend.position="bottom", # posição da caixa de legenda
    panel.background = element_blank())+ # tirando o fundo do gráfico

geom_raster()+
geom_tile(color="grey")+ #linha que separa dos "pixels"
labs(x = "Ano", y = "Mês",fill = "",title =nomeTitulo) +

scale_fill_manual(values=coresClasses,breaks=classificacao) +

scale_x_continuous(breaks = seq(estacao1[1,1],estacao1[264,1],2)) +

scale_y_continuous(breaks = c(1:12))

jpeg(filename = nameFileBitMap,width = 530, height = 300, family = "A",
         units = "px", quality = 100,  bg = "white",  res = NA)
print(myplot1)
dev.off() #terminou o gráfico 
    
    ##################### FIM DO MAPA DE CLASSES ############################################ 

estacao1 = estacao[265:528,] ## separando os dados para separar os gráficos
    ########################### MAPA DE CLASSES ###########################################

nomeTitulo = paste("Classificação da Precipitação de",estacao1[1,1],'a',estacao1[264,1])
nameFileBitMap = paste(path, "/bitmap_",estacao1[1,1],"_",estacao1[264,1],".jpg",sep='')
graphics.off()

myplot1 <- ggplot(estacao1,aes(x=ANO, y=MÊS,fill = Classificação)) + #iniciando o plot

coord_fixed(ratio=0.8)+ # proporção do "pixels"
    # maior que 1 => retângulo com altura maiores que a largura
    # igual a 1   => quadrado
    # menor que 1 => retângulo com largura maior que a altura
theme(# formatação
    #axis.text.x = element_text( angle = 320), #escrita do eixo x na vertical
    #axis.text.y = element_text( face = "bold"),
    plot.title = element_text(hjust = 0.5), # centralizar título
    text = element_text(size=20),# tamanho da letra e negrito

    legend.key = element_rect(fill = "white", colour = cinza), #caixa de legenda
    legend.position="bottom", # posição da caixa de legenda
    panel.background = element_blank())+ # tirando o fundo do gráfico

geom_raster()+
geom_tile(color="grey")+ #linha que separa dos "pixels"
labs(x = "Ano", y = "Mês",fill = "",title =nomeTitulo) +

scale_fill_manual(values=coresClasses,breaks=classificacao) +

scale_x_continuous(breaks = seq(estacao1[1,1],estacao1[264,1],2)) +

scale_y_continuous(breaks = c(1:12))

jpeg(filename = nameFileBitMap,width = 530, height = 300, family = "A",
         units = "px", quality = 100,  bg = "white",  res = NA)
print(myplot1)
dev.off() #terminou o gráfico 
    
    ##################### FIM DO MAPA DE CLASSES ############################################

estacao1 = estacao[529:792,] ## separando os dados para separar os gráficos
    ########################### MAPA DE CLASSES ###########################################

nomeTitulo = paste("Classificação da Precipitação de",estacao1[1,1],'a',estacao1[264,1])
nameFileBitMap = paste(path, "/bitmap_",estacao1[1,1],"_",estacao1[264,1],".jpg",sep='')
graphics.off()

myplot1 <- ggplot(estacao1,aes(x=ANO, y=MÊS,fill = Classificação)) + #iniciando o plot

coord_fixed(ratio=0.8)+ # proporção do "pixels"
    # maior que 1 => retângulo com altura maiores que a largura
    # igual a 1   => quadrado
    # menor que 1 => retângulo com largura maior que a altura
theme(# formatação
    #axis.text.x = element_text( angle = 320), #escrita do eixo x na vertical
    #axis.text.y = element_text( face = "bold"),
    plot.title = element_text(hjust = 0.5), # centralizar título
    text = element_text(size=20),# tamanho da letra e negrito

    legend.key = element_rect(fill = "white", colour = cinza), #caixa de legenda
    legend.position="bottom", # posição da caixa de legenda
    panel.background = element_blank())+ # tirando o fundo do gráfico

geom_raster()+
geom_tile(color="grey")+ #linha que separa dos "pixels"
labs(x = "Ano", y = "Mês",fill = "",title =nomeTitulo) +

scale_fill_manual(values=coresClasses,breaks=classificacao) +

scale_x_continuous(breaks = seq(estacao1[1,1],estacao1[264,1],2)) +

scale_y_continuous(breaks = c(1:12))

jpeg(filename = nameFileBitMap,width = 530, height = 300, family = "A",
         units = "px", quality = 100,  bg = "white",  res = NA)
print(myplot1)
dev.off() #terminou o gráfico 
    
    ##################### FIM DO MAPA DE CLASSES ############################################

estacao1 = estacao[793:1056,] ## separando os dados para separar os gráficos
    ########################### MAPA DE CLASSES ###########################################

nomeTitulo = paste("Classificação da Precipitação de",estacao1[1,1],'a',estacao1[264,1])
nameFileBitMap = paste(path, "/bitmap_",estacao1[1,1],"_",estacao1[264,1],".jpg",sep='')
graphics.off()

myplot1 <- ggplot(estacao1,aes(x=ANO, y=MÊS,fill = Classificação)) + #iniciando o plot

coord_fixed(ratio=0.8)+ # proporção do "pixels"
    # maior que 1 => retângulo com altura maiores que a largura
    # igual a 1   => quadrado
    # menor que 1 => retângulo com largura maior que a altura
theme(# formatação
    #axis.text.x = element_text( angle = 320), #escrita do eixo x na vertical
    #axis.text.y = element_text( face = "bold"),
    plot.title = element_text(hjust = 0.5), # centralizar título
    text = element_text(size=20),# tamanho da letra e negrito

    legend.key = element_rect(fill = "white", colour = cinza), #caixa de legenda
    legend.position="bottom", # posição da caixa de legenda
    panel.background = element_blank())+ # tirando o fundo do gráfico

geom_raster()+
geom_tile(color="grey")+ #linha que separa dos "pixels"
labs(x = "Ano", y = "Mês",fill = "",title =nomeTitulo) +

scale_fill_manual(values=coresClasses,breaks=classificacao) +

scale_x_continuous(breaks = seq(estacao1[1,1],estacao1[264,1],2)) +

scale_y_continuous(breaks = c(1:12))

jpeg(filename = nameFileBitMap,width = 530, height = 300, family = "A",
         units = "px", quality = 100,  bg = "white",  res = NA)
print(myplot1)
dev.off() #terminou o gráfico 
    
    ##################### FIM DO MAPA DE CLASSES ############################################

      
    ################### Arquivo Infos_ ######################################################
nomeFileInfo = paste(path,"/infos_",nameDir,".txt",sep='')
    #criando nome do arquivo que salvará algumas das infomações geradas pelo boxplot
    
    #esse if é uma precaução para caso rodem o script mais de uma vez.
    # se o arquivo existe, ele vai ser removido antes que seja acrescentanda as infos
if(file.exists(nomeFileInfo))
    file.remove(nomeFileInfo)
    
write(paste("Estacão: ",nomePt) , file = nomeFileInfo, append = TRUE)
                                                    #escrevendo no arquivo o nome da estação
    # append = TRUE significa que será acrescentada ao arquivo se existente
write(paste("Total de Outliers: ",length(bp$out)) , file = nomeFileInfo, append = TRUE)
    #escrevendo no arquivo o total de outliers da estação

write("" , file = nomeFileInfo, append = TRUE)# pulando linha

write("Quantidade de Outliers por Mês: ", file = nomeFileInfo, append = TRUE)

write(paste("Janeiro: ",length(bp$group[bp$group==1])) , file = nomeFileInfo, append = TRUE)
write(paste("Fevereiro: ",length(bp$group[bp$group==2])) , file = nomeFileInfo, append = TRUE)
write(paste("Março: ",length(bp$group[bp$group==3])) , file = nomeFileInfo, append = TRUE)
write(paste("Abril: ",length(bp$group[bp$group==4])) , file = nomeFileInfo, append = TRUE)
write(paste("Maio: ",length(bp$group[bp$group==5])) , file = nomeFileInfo, append = TRUE)
write(paste("Junho: ",length(bp$group[bp$group==6])) , file = nomeFileInfo, append = TRUE)
write(paste("Julho: ",length(bp$group[bp$group==7])) , file = nomeFileInfo, append = TRUE)
write(paste("Agosto: ",length(bp$group[bp$group==8])) , file = nomeFileInfo, append = TRUE)
write(paste("Setembro: ",length(bp$group[bp$group==9])) , file = nomeFileInfo, append = TRUE)
write(paste("Outubro: ",length(bp$group[bp$group==10])) , file = nomeFileInfo, append = TRUE)
write(paste("Novembro: ",length(bp$group[bp$group==11])) , file = nomeFileInfo, append = TRUE)
write(paste("Dezembro: ",length(bp$group[bp$group==12])) , file = nomeFileInfo, append = TRUE)

write("" , file = nomeFileInfo, append = TRUE)# pulando linha
write("Total de Dados por Mês: ", file = nomeFileInfo, append = TRUE)
write(paste(c(1:12), bp$n) , file = nomeFileInfo, append = TRUE, sep='\t')



    ########################################################################################

    # setando em variáveis alguns dos resultados gerados pela função boxplot
limInferior = bp$stats[1,]
quartil1 = bp$stats[2,]
mediana = bp$stats[3,]
quartil3 = bp$stats[4,]
limSuperior = bp$stats[5,]

bpStats = data.frame(mes=c(1:12),limInferior,quartil1,mediana, quartil3,limSuperior, media)
    #criando um dataframa os as variáveis setadas
write_xlsx(bpStats, paste(path,"/stats_",nameDir,".xlsx",sep=''), col_names=TRUE)
    #escrevendo em um aquivo .xlsx
    
ano = NULL
for(i in 1:length(bp$out))
    ano[i] = estacao[1][estacao[2]==bp$group[i] & estacao[3] == bp$out[i]]
dadosOut = data.frame(ano, Mes= bp$group, outliers=bp$out)
write_xlsx(dadosOut, paste(path,"/outliers_",nameDir,".xlsx",sep=''), col_names=TRUE)

    
    # escrevendo numa planilha os valores dos outliers separados por mês 
    # separando os dados por mês
outPorMes =list(valor1  = dadosOut[3][dadosOut[2]== 1], ano1  = dadosOut[1][dadosOut[2]== 1],
                valor2  = dadosOut[3][dadosOut[2]== 2], ano2  = dadosOut[1][dadosOut[2]== 2],
                valor3  = dadosOut[3][dadosOut[2]== 3], ano3  = dadosOut[1][dadosOut[2]== 3],
                valor4  = dadosOut[3][dadosOut[2]== 4], ano4  = dadosOut[1][dadosOut[2]== 4],
                valor5  = dadosOut[3][dadosOut[2]== 5], ano5  = dadosOut[1][dadosOut[2]== 5],
                valor6  = dadosOut[3][dadosOut[2]== 6], ano6  = dadosOut[1][dadosOut[2]== 6],
                valor7  = dadosOut[3][dadosOut[2]== 7], ano7  = dadosOut[1][dadosOut[2]== 7],
                valor8  = dadosOut[3][dadosOut[2]== 8], ano8  = dadosOut[1][dadosOut[2]== 8],
                valor9  = dadosOut[3][dadosOut[2]== 9], ano9  = dadosOut[1][dadosOut[2]== 9],
                valor10 = dadosOut[3][dadosOut[2]==10], ano10 = dadosOut[1][dadosOut[2]==10],
                valor11 = dadosOut[3][dadosOut[2]==11], ano11 = dadosOut[1][dadosOut[2]==11],
                valor12 = dadosOut[3][dadosOut[2]==12], ano12 = dadosOut[1][dadosOut[2]==12])
    
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
# vetor com os nomes dos meses
mesesDoAno = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio","Junho",
               "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
#primeira linha: um título
writeData(wb, "Sheet1", paste("Valores e Anos dos 'outliers' da estação",nomePt), startCol = 1, startRow = 1)
    
for(i in seq(1,length(outPorMes),2)) #nome nas células
{
    writeData(wb, "Sheet1", mesesDoAno[(i%/%2)+1], startCol = i, startRow = 2, rowNames = TRUE)
    writeData(wb, "Sheet1", "Valor", startCol = i, startRow = 3, rowNames = TRUE)
    writeData(wb, "Sheet1", "Ano", startCol = i+1, startRow = 3, rowNames = TRUE)
}
    
for(i in 1:length(outPorMes)) #inserindo os valores
{
    writeData(wb, "Sheet1", outPorMes[i], startCol = i, startRow = 4, rowNames = TRUE)
}
#salvando no arquivo
saveWorkbook(wb, paste(path,"/outliersPorMes_",nameDir,".xlsx",sep=''), overwrite = TRUE)    
    
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
#primeira linha: um título
writeData(wb, "Sheet1", paste("Valores e Anos dos 'outliers' da estação",nomePt), startCol = 1, startRow = 1)
    
for(i in seq(1,length(outPorMes),2)) #nome nas células
{
    writeData(wb, "Sheet1", mesesDoAno[(i%/%2)+1], startCol = i, startRow = 2, rowNames = TRUE)
}
    
for(i in 1:length(outPorMes)) #inserindo os valores
{
    writeData(wb, "Sheet1", round(outPorMes[[i]]), startCol = i, startRow = 3, rowNames = TRUE)
}
    #salvando no arquivo
saveWorkbook(wb, paste(path,"/outliersPorMesV2_",nameDir,".xlsx",sep=''), overwrite = TRUE)
   

