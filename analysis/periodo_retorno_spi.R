## Código feito por: João Dehon

library(dplyr)

setwd("/Users/franciscomatos/onedrive/code/deha/PRHCE")

limiar <- 0
n.estacoes <- 12

spi = readRDS("data/EXTRM_SECAS/SPI_Bacias_df.rds")
spi_Curu = spi$Curu

spi <- spi[!is.na(spi[,1]),]
#spi <- spi[,c(1,8,3,4,9,11,5,12,10,2,7,6)] ## ordem alfabética
date <- as.Date(rownames(spi))
#ano <- seq.default(from = (year(ano[1]) + (month(ano[1]) - 1)/12), by = 1/12, length.out = length(ano))

##transformar em spi 12 dec
spi_12_dez <- spi[which(months(date) == "Dezembro"),]
spi <- spi_12_dez
ano <- as.numeric(substring(as.Date(rownames(spi_12_dez)),1,4))
dim(spi)

#arrumar spi por bacia de acordo com latitude
spi <- spi[,c(4,6, 5, 2, 11, 8, 3, 10, 12, 7, 1, 9)]
head(spi)
bacias <- names(spi)
bacias
bacias <- c("HR01", "HR02", "HR03", "HR04", "HR05", "HR06",
            "HR07", "HR08", "HR09", "HR010", "HR011", "HR012")


##2) plotar SPI  ####
png("data/EXTRM_SECAS/spi.png")
par(mfrow = c(3,4),                         # 6x4 layout
    oma = c(5,5,0,0) + 0.1,           # 5 rows of text at the outer left and bottom margin
    mar = c(1,1,3,1) + 0.1,
    
    mgp = c(2, 1, 0))                 # axis label at 2 rows distance, tick labels at 1 row

for (i in 1:n.estacoes){
    plot(ano, spi[,i], type = 'l', ylim = c(-4,3), 
         ylab = "SPI", 
         xlab = "", cex.lab = 1.2, cex.main = 1.2, 
         cex.ylim = 4)
    #lines(ano_2, spi_12_dez[,i], col = "blue") ##verificacao de influencia do spi continuo e discretizado em um valor por ano
    lines(ano, rep(0, length(ano)), col = "black")
    lines(ano, rep(-1, length(ano)), col = "yellow")
    lines(ano, rep(-2, length(ano)), col = "red")
    title( paste( bacias[i]), line = -0.0009, face = "plain")
}
dev.off()

##3) determinar seca abaixo limiar ####

library(data.table)

seca <- matrix(nrow = dim(spi)[1], ncol = dim(spi)[2])
for (i in 1:dim(spi)[2]) {
    seca[which(spi[,i] < limiar),i] = 1
    seca[which(spi[,i] >= limiar),i] = 0
    #setTxtProgressBar(pb,i)
}


## 3.1) agrupar cada evento de seca 
tab <- data.frame()
seca_show <- vector()
spi_show <- vector()
n.seca <- vector()
n.seca_lst <- list()

for (j in 1:dim(spi)[2]){
    grupo <- 1
    for (i in 1:dim(spi)[1]) {
        if (seca[i,j] == 0) { ### 0 indica não seca e 1 indica seca
            grupo <- grupo + 1
            n.seca[i] <- 0}  else 
            {n.seca[i] <- grupo}
        #spi_show <- spi[1:i,j]
        #seca_show <- seca[1:i,j]
        #tab <- cbind(grupo,seca_show, n.seca, spi_show)
    }
    n.seca_lst[[j]] <- n.seca ## atribui um numero para cada evento de seca diferente
    #setTxtProgressBar(pb,j)
    
} 
names(n.seca_lst) <- bacias

dt_list <- list()
for (i in 1:dim(spi)[2]){
    dt_list[[i]] <- setDT(data.frame(ano = ano,
                                     spi = spi[,i],
                                     seca =  seca[,i], 
                                     n_seca = n.seca_lst[[i]]))
    #setTxtProgressBar(pb,i)
    
}
names(dt_list) <- bacias
#dt_list <- setDT(as.data.frame((cbind(spi[,1], seca[,1], n.seca_lst[[1]] ))))


## 4) calcular indicadores de analise####

severidade <- vector()
duracao <- vector()
ano_i <- vector()
ano_f <- vector()
severidade_lst <- list()
duracao_lst <- list()
seca_analise <- list()
ano_i_lst <- list()
ano_f_lst <- list()

#pico <- vector()
pb = txtProgressBar(min = 1, max = ncol(spi), style = 3)  #esse comando fora do for
for (j in 1:ncol(spi)){
    #for (j in 1:2){
    for(i in 1:nrow(spi)){
        dt_seca <- dt_list[[j]][seca == 1]
        severidade[i] <- dt_list[[j]][n_seca == i, sum(spi)]
        duracao[i] <- dim(dt_list[[j]][which(dt_list[[j]]$n_seca == i),])[1]
        ano_i[i] <- dt_seca[n_seca == i, ano[1]]
        if (is.na(ano_i[i])){
            ano_f[i] = NA} else {
                ano_f[i] <- dt_seca[n_seca == i, ano[length(ano)]]}
        #pico[i] <- dt[n.seca == i, min(spi)]
        #ano_i[i] <- dim(dt_list[[j]][which(dt_list[[j]]$n_seca == i),])[1]
    }
    
    severidade_lst[[j]] <- severidade[which(severidade != 0)] *-1
    duracao_lst[[j]] <- duracao[which(duracao != 0)]
    ano_i_lst[[j]] <- ano_i[which(duracao > 0)]
    ano_f_lst[[j]] <- ano_f[which(duracao > 0)]
    seca_analise[[j]] <- cbind(ano_i = ano_i_lst[[j]],
                               ano_f = ano_f_lst[[j]],
                               duracao = duracao_lst[[j]],
                               severidade = severidade_lst[[j]])
    
    
    setTxtProgressBar(pb,j)
    
}

png("data/EXTRM_SECAS/scatterplot.png", width = 4200, height = 3200, antialias = "gray", pointsize = 17, res = 300)

par(mfrow = c(3,4),                         # 6x4 layout
    oma = c(5,5,0,0) + 0.1,           # 5 rows of text at the outer left and bottom margin
    mar = c(1.5,1.5,3,1) + 0.1,           
    mgp = c(2, 1, 0))                 # axis label at 2 rows distance, tick labels at 1 row

#library(RColorBrewer)
#cols <- brewer.pal(7, "YlOrRd")
for (j in 1:ncol(spi)){
    suppressWarnings(plot(seca_analise[[j]][,3],seca_analise[[j]][,4], 
                          xlim = c(0,11), ylim = c(0,11), yaxt = "n",xaxt = "n",
                          main = paste0(bacias[j]), cex.main=2))
    if (any(j == c(1,5,9))){
        axis(side = 2,cex.axis=2, las =2)
        axis(side = 1, labels = c("","","","","","","","","","")
             , at = c(1:10))
    }
    if (any(j == c(9,10,11,12))){
        axis(side = 1,cex.axis=2)
        axis(side = 2, labels = c("","","","","","","","","","")
             , at = c(1:10))
    }
    else {axis(side = 1, labels = c("","","","","","","","","","")
               , at = c(1:10))
        axis(side = 2, labels = c("","","","","","","","","","")
             , at = c(1:10))}
    
    #title(main = paste0(bacias[j]), lines = -10, cex.title =3)
    # points(seca_analise[[j]][seca_analise[[j]][,1] >= ano[1] & seca_analise[[j]][,1] < 1930,3],
    #        seca_analise[[j]][seca_analise[[j]][,1] >= ano[1] & seca_analise[[j]][,1] < 1930,4]
    #        , col =  cols[3], pch = 1)
    # points(seca_analise[[j]][seca_analise[[j]][,1] >= 1930 & seca_analise[[j]][,1] < 1930 + 20,3],
    #        seca_analise[[j]][seca_analise[[j]][,1] >= 1930 & seca_analise[[j]][,1] < 1930 + 20,4]
    #        , col =  cols[4], pch = 1)
    # points(seca_analise[[j]][seca_analise[[j]][,1] >= 1930 + 20 & seca_analise[[j]][,1] < 1930 + 40,3],
    #        seca_analise[[j]][seca_analise[[j]][,1] >= 1930 + 20 & seca_analise[[j]][,1] < 1930 + 40,4]
    #        , col =  cols[5], pch = 1)
    # points(seca_analise[[j]][seca_analise[[j]][,1] >= 1930 + 40 & seca_analise[[j]][,1] < 1930 + 60,3],
    #        seca_analise[[j]][seca_analise[[j]][,1] >= 1930 + 40 & seca_analise[[j]][,1] < 1930 + 60,4]
    #        , col =  cols[6], pch = 1)
    points(seca_analise[[j]][seca_analise[[j]][,1] == 2012 ,3],
           seca_analise[[j]][seca_analise[[j]][,1] == 2012 ,4]
           , col =  "red", pch = 16)
    #text(seca_analise[[j]][seca_analise[[j]][,1] >= 2012,3],
    #     seca_analise[[j]][seca_analise[[j]][,1] >= 2012 ,4],
    #     labels = paste(seca_analise[[i]][dim(seca_analise[[i]])[1],1],
    #               "-" , seca_analise[[i]][dim(seca_analise[[i]])[1],2]),
    #     pos = 4)
    # text(seca_analise[[j]][seca_analise[[j]][,3] >= 5,3],
    #     seca_analise[[j]][seca_analise[[j]][,3] >= 5,4],
    #     labels = paste(seca_analise[[j]][seca_analise[[j]][,3] >= 5,1],
    #              "-" , seca_analise[[j]][seca_analise[[j]][,3] >= 5,2]),
    #     pos = 1)
    
    
    
}

title(xlab = "Duration (years)",
      ylab = "Severity",
      outer = TRUE, line = 2.3,lheight = 6, cex.lab = 2.5)


dev.off()


names(seca_analise) <- bacias

##4.1) estatistica descritiva####
par(mfrow = c(1,1) )
library(pastecs)

stat_des_dur <- list()
stat_des_sev <- list()
stat_des_seca <- list()
cor_ken <- list()
media_ <- list()
for (i in 1:dim(spi)[2]){
    temp <- stat.desc(seca_analise[[i]][,3])
    stat_des_seca[[i]] <- temp[1]
    media_[[i]] <- dim(spi)[1]/stat_des_seca[[i]]
    cor_ken[[i]] <- cor(seca_analise[[i]][,3], seca_analise[[i]][,4], method = 'kendall')
    stat_des_dur[[i]] <- temp[c(9, 5, 13,14)]
    temp_1 <- stat.desc(seca_analise[[i]][,4])
    stat_des_sev[[i]] <- temp_1[c(9, 5, 13,14)]
}

stat_desc <- round(cbind(unlist(stat_des_seca), 
                         unlist(media_), 
                         unlist(cor_ken), 
                         matrix(unlist(stat_des_dur), ncol = 4, byrow = T),
                         matrix(unlist(stat_des_sev), ncol = 4, byrow = T)),2)

rownames(stat_desc) <- bacias
colnames(stat_desc) <- c("Drought Events", "Average Inter-Arrival Time (month)", "Kendall's Tau", "Mean", "Max", "SD", "CV", "Mean", "Max", "SD", "CV")
stat_desc

## 5) fit marginal ####
# 
# 
library(fitdistrplus)


cands <- list()
aic <- list()
bestfit <- list()
bestfit_par1 <- list()
bestfit_par2 <- list()
bestfit_name <- list()
cands_2 <- list()
aic_2 <- list()
bestfit_2 <- list()
bestfit_2_par1 <- list()
bestfit_2_par2 <- list()
bestfit_2_name <- list()
bestfit_aic<-list()
bestfit_2_aic<-list()


distris <- c('lnorm', "exp", "norm","weibull", "gamma", "logis")

for (j in 1: dim(spi)[2]){
    for (i in 1:2) { ##numero de variaveis analisadas (dur e sev)
        
        fit.lnorm <- fitdist(seca_analise[[j]][,i+2],distr = 'lnorm')
        fit.exp <- fitdist(data = seca_analise[[j]][,i+2],distr = 'exp')
        fit.norm <- fitdist(data = seca_analise[[j]][,i+2],distr = "norm")
        fit.weibull <- fitdist(seca_analise[[j]][,i+2],distr = 'weibull')
        fit.gamma <- fitdist(seca_analise[[j]][,i+2],distr = 'gamma')
        fit.logistic <- fitdist(seca_analise[[j]][,i+2],distr = 'logis')
        #fit.cauchy <- fitdist(seca_analise[[j]][,i+2],distr = 'cauchy')
        
        cands[[i]] <- list(lnorm = fit.lnorm, 
                           exp = fit.exp, 
                           norm = fit.norm, 
                           weibull = fit.weibull,  
                           gamma = fit.gamma, 
                           logistic = fit.logistic)
        #cands[[i]] <- list(lnorm = fit.lnorm, exp = fit.exp)
        
        aic[[i]] <- c(fit.lnorm$aic, 
                      fit.exp$aic,
                      fit.norm$aic, 
                      fit.weibull$aic,
                      fit.gamma$aic, 
                      fit.logistic$aic)
        
        par1 <- c(fit.lnorm$estimate[1],
                  fit.exp$estimate[1], 
                  fit.norm$estimate[1],
                  fit.weibull$estimate[1], 
                  fit.gamma$estimate[1], 
                  fit.logistic$estimate[1])
        
        par2 <- c(fit.lnorm$estimate[2],
                  fit.exp$estimate[2], 
                  fit.norm$estimate[2],
                  fit.weibull$estimate[2], 
                  fit.gamma$estimate[2], 
                  fit.logistic$estimate[2])
        #aic[[i]] <- list(lnorm = fit.lnorm$aic,exp = fit.exp$aic)
        
        bestfit[[i]] <- cands[[i]][which.min(aic[[i]])]
        bestfit_par1[[i]] <- par1[which.min(aic[[i]])]
        bestfit_par2[[i]] <- par2[which.min(aic[[i]])]
        bestfit_name[[i]] <- distris[which.min(aic[[i]])]
        bestfit_aic[[i]] <- aic[[i]][which.min(aic[[i]])]
        
        #plot(bestfit[[i]][[1]])  ##plot das dist marginais fitadas
        #names(bestfit) <- names(cands) <- c("duracao", "severidade")
    }
    names(bestfit_par1) <-names(bestfit_par2)<-names(bestfit_name)<- names(bestfit) <- names(cands) <- names(aic) <- c("duracao", "severidade")
    cands_2[[j]] <- cands
    aic_2[[j]] <- aic
    bestfit_2[[j]] <- bestfit
    bestfit_2_par1[[j]] <- bestfit_par1
    bestfit_2_par2[[j]] <- bestfit_par2
    bestfit_2_name[[j]] <- bestfit_name
    bestfit_2_aic[[j]] <- bestfit_aic
    
    
}
names(cands_2) <- names(aic_2) <- names(bestfit_2)<- names(bestfit_2_par1)<- names(bestfit_2_par2)<- names(bestfit_2_name) <- bacias



dur_aic<-sev_aic<-dur_marginal_name <- sev_marginal_name<- dur_marginal_par1<-sev_marginal_par1 <-dur_marginal_par2<-sev_marginal_par2<-list()

for (i in 1:dim(spi)[2]){
    dur_marginal_name[[i]] <- unlist(bestfit_2_name[i])[1]
    sev_marginal_name[[i]] <- unlist(bestfit_2_name[i])[2]
    dur_marginal_par1[[i]] <- unlist(bestfit_2_par1[i])[1]
    sev_marginal_par1[[i]] <- unlist(bestfit_2_par1[i])[2]
    dur_marginal_par2[[i]] <- unlist(bestfit_2_par2[i])[1]
    sev_marginal_par2[[i]] <- unlist(bestfit_2_par2[i])[2]
    dur_aic[[i]] <- unlist(bestfit_2_aic[i])[1]
    sev_aic[[i]] <- unlist(bestfit_2_aic[i])[2]
}

bestfit_tab <- cbind(dur_marg = unlist(dur_marginal_name), 
                     dur_par_1 = round(unlist(dur_marginal_par1),2),
                     dur_par_2 = round(unlist(dur_marginal_par2),2),
                     dur_aic = round(unlist(dur_aic),2),
                     sev_marg = unlist(sev_marginal_name),
                     sev_par_1 = round(unlist(sev_marginal_par1),2),
                     sev_par_2 = round(unlist(sev_marginal_par2),2),
                     sev_aic = round(unlist(sev_aic),2))

rownames(bestfit_tab) <- bacias

##6) dist conjunta e periodo de retorno####
library(VineCopula)
library(psych)
library(psychTools)
cop_lst <- list()
N <- dim(spi)[1] ## numero de anos analisados
historico <- list()
sumario <- list()
probs_marg_d <- list()
probs_marg_s <- list()
probs_conj <- list()
compar <- list()


media <- list()

par(mfrow = c(1,n.estacoes))
#for (i in 1:dim(spi)[2]){
for (i in 1:n.estacoes){
    n <- length(seca_analise[[i]][,1]) ## numero de eventos de seca durante os N anos
    media[[i]] <- N/n
    
    ##6.1) Dist marginais####
    
    ##duracao
    
    if (bestfit_tab[i,1] == "lnorm"){
        probs_marg_d[[i]] <- plnorm(seca_analise[[i]][,3],
                                    as.numeric(bestfit_tab[i,2]),
                                    as.numeric(bestfit_tab[i,3]))
    } else if (bestfit_tab[i,1] == "exp"){
        probs_marg_d[[i]] <-  pexp(seca_analise[[i]][,3],
                                   as.numeric(bestfit_tab[i,2]),
                                   as.numeric(bestfit_tab[i,3]))
    } else if (bestfit_tab[i,1] == "norm"){
        probs_marg_d[[i]] <-  pnorm(seca_analise[[i]][,3],
                                    as.numeric(bestfit_tab[i,2]),
                                    as.numeric(bestfit_tab[i,3]))
    } else if (bestfit_tab[i,1] == "weibull"){
        probs_marg_d[[i]] <- pweibull(seca_analise[[i]][,3],
                                      as.numeric(bestfit_tab[i,2]),
                                      as.numeric(bestfit_tab[i,3]))
    } else if (bestfit_tab[i,1] == "gamma"){
        probs_marg_d[[i]] <- pgamma(seca_analise[[i]][,3],
                                    as.numeric(bestfit_tab[i,2]),
                                    as.numeric(bestfit_tab[i,3]))
    } else if (bestfit_tab[i,1] == "logis"){
        probs_marg_d[[i]] <- plogis(seca_analise[[i]][,3],
                                    as.numeric(bestfit_tab[i,2]),
                                    as.numeric(bestfit_tab[i,3]))}
    
    ## severidade
    
    if (bestfit_tab[i,5] == "lnorm"){
        probs_marg_s[[i]] <- plnorm(seca_analise[[i]][,4],
                                    as.numeric(bestfit_tab[i,6]),
                                    as.numeric(bestfit_tab[i,7]))
    } else if (bestfit_tab[i,5] == "exp"){
        probs_marg_s[[i]] <-  pexp(seca_analise[[i]][,4],
                                   as.numeric(bestfit_tab[i,6]),
                                   as.numeric(bestfit_tab[i,7]))
    } else if (bestfit_tab[i,5] == "norm"){
        probs_marg_s[[i]] <-  pnorm(seca_analise[[i]][,4],
                                    as.numeric(bestfit_tab[i,6]),
                                    as.numeric(bestfit_tab[i,7]))
    } else if (bestfit_tab[i,5] == "weibull"){
        probs_marg_s[[i]] <- pweibull(seca_analise[[i]][,4],
                                      as.numeric(bestfit_tab[i,6]),
                                      as.numeric(bestfit_tab[i,7])) 
    } else if (bestfit_tab[i,5] == "gamma"){
        probs_marg_s[[i]] <- pgamma(seca_analise[[i]][,4],
                                    as.numeric(bestfit_tab[i,6]),
                                    as.numeric(bestfit_tab[i,7]))
    } else if (bestfit_tab[i,5] == "logis"){
        probs_marg_s[[i]] <- plogis(seca_analise[[i]][,4],
                                    as.numeric(bestfit_tab[i,6]),
                                    as.numeric(bestfit_tab[i,7]))}
    
    
    ##6.2) Dist conjunta####
    cop_lst[[i]] <- BiCopSelect(u1 = probs_marg_d[[i]],
                                u2 = probs_marg_s[[i]],
                                familyset = c(1:5))
    
    compar[[i]] <- BiCopEstList(u1 = probs_marg_d[[i]],
                                u2 = probs_marg_s[[i]],
                                familyset = c(1:5))
    
    #compar[[i]]$summary = compar[[i]]$summary[compar[[i]]$summary$AIC,]
    compar_2 <- compar[[i]]$summary
    #contour(cop_lst[[i]], margins = "unif", main = bacias[i])
    #points(cbind(probs_marg_d, probs_marg_s), col = "red")  
    probs_conj[[i]] <- BiCopCDF(probs_marg_d[[i]], probs_marg_s[[i]], obj = cop_lst[[i]])
    
    per_retrn_d <- (media[[i]]/(1 - probs_marg_d[[i]]))  
    per_retrn_s <- (media[[i]]/(1 - probs_marg_s[[i]]))  
    
    
    per_retrn_ou <- (media[[i]]/(1 - probs_conj[[i]]))  ##converte para ano
    per_retrn_e <- (media[[i]]/(1 - probs_marg_d[[i]] - probs_marg_s[[i]] + probs_conj[[i]]))
    

    
    historico[[i]] <- dfOrder(as.data.frame(cbind(
        D = seca_analise[[i]][,3], 
        S = round(seca_analise[[i]][,4],3), 
        Ano_i = seca_analise[[i]][,1], 
        Ano_f = seca_analise[[i]][,2], 
        P_D =  round(probs_marg_d[[i]], 2),
        P_S =  round(probs_marg_s[[i]], 2),
        P_DS =  round(probs_conj[[i]], 2),
        TDS_d = round(per_retrn_d, 2),
        TDS_s = round(per_retrn_s, 2),
        TDS_ou = round(per_retrn_ou, 2),
        TDS_e = round(per_retrn_e, 2))),columns = c(1,2))
    
    
    
    temp_Maior_TDS_ou <- max(historico[[i]][,8])  
    temp_Maior_TDS_e <- max(historico[[i]][,9])  
    
    # busca pelo maior periodo de retorno
    
    sumario[[i]] <- as.data.frame(cbind(N_eventos = length(seca_analise[[i]][,3]),
                                        Maior_TDS_ou = temp_Maior_TDS_ou ,
                                        Maior_TDS_e = temp_Maior_TDS_e ,
                                        Ano_i = historico[[i]][which(historico[[i]][,9] == temp_Maior_TDS_e), 3],
                                        Ano_f = historico[[i]][which(historico[[i]][,9] == temp_Maior_TDS_e), 4]))
    
}
names(historico) <- names(cop_lst) <- names(sumario) <- bacias
sumario_dt <- as.data.frame(matrix(unlist(sumario), nrow = n.estacoes, byrow = T))
rownames(sumario_dt) <- bacias
colnames(sumario_dt) <- c("Nº Eventos", "Maior TDS_ou", "Maior TDS_e", "Ano_i", "Ano_f")

historico[[1]]

result <- data.frame(matrix(unlist(cop_lst), nrow = n.estacoes, byrow=T))
result <- result[,c(5, 2,6, 13)]
#result[,2:4] <- round(as.numeric(result[,2:4]),2)
rownames(result) <- bacias
result_names  <- c("family", "par", " tau", 
                   "AIC")


colnames(result) <- result_names

##6.3) Joint CDF perspective####

# ##7) duração e severidade media ####


f_media_dur <- function(x, i) {
    mean(x[i][,3])
}

f_media_sev <- function(x, i) {
    mean(x[i][,4])
}

dur_med <- round(mean(unlist(lapply(seca_analise, f_media_dur))),2)
sev_med <- round(mean(unlist(lapply(seca_analise, f_media_sev))),2)



##8) Figura contour T interesse e valor T medio####

T_med_e <- list()
T_med_ou <- list()
xx <- seq(0, 11, length.out = 25)
yy <- seq(0, 11, length.out = 25)

png("data/EXTRM_SECAS/T_e_ou.png", width = 4200, height = 4200, antialias = "subpixel", pointsize = 17, res = 300)

par(mfrow = c(6,4),                         # 6x4 layout
    oma = c(5,5,0,0) + 0.1,           # 5 rows of text at the outer left and bottom margin
    mar = c(1,1,1,1) + 0.1,           
    mgp = c(2, 1, 0))                 # axis label at 2 rows distance, tick labels at 1 row

for (i in 1:dim(spi)[2]) {
    
    
    # T_e_fun <- function(d,s){
    #   #define objeto d
    #   probs_marg_d_3 <-  plnorm(d, cands_2[[i]][[1]][[1]][["estimate"]][[1]],
    #                             cands_2[[i]][[1]][[1]][["estimate"]][[2]])
    #   #define objeto s
    #   probs_marg_s_3 <-  pexp(s, cands_2[[i]][[1]][[2]][["estimate"]])
    #   
    #   probs_conj_3 <- BiCopCDF(probs_marg_d_3, probs_marg_s_3, obj = cop_lst[[i]])
    #   
    #   media[[i]]/(1 - probs_marg_d_3 - probs_marg_s_3 + probs_conj_3)
    # }
    
    ## Defino funcao para bacia em analise
    T_e_fun <- function(d,s){
        
        #define objeto d
        if (bestfit_tab[i,1] == "lnorm"){
            probs_marg_d_3 <- plnorm(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "exp"){
            probs_marg_d_3 <-  pexp(d,
                                    as.numeric(bestfit_tab[i,2]),
                                    as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "norm"){
            probs_marg_d_3 <-  pnorm(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "weibull"){
            probs_marg_d_3 <- pweibull(d,
                                       as.numeric(bestfit_tab[i,2]),
                                       as.numeric(bestfit_tab[i,3])) 
        } else if (bestfit_tab[i,1] == "gamma"){
            probs_marg_d_3 <- pgamma(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "logis"){
            probs_marg_d_3 <- plogis(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))}
        #define objeto s
        if (bestfit_tab[i,5] == "lnorm"){
            probs_marg_s_3 <- plnorm(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "exp"){
            probs_marg_s_3 <-  pexp(s,
                                    as.numeric(bestfit_tab[i,6]),
                                    as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "norm"){
            probs_marg_s_3 <-  pnorm(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "weibull"){
            probs_marg_s_3 <- pweibull(s,
                                       as.numeric(bestfit_tab[i,6]),
                                       as.numeric(bestfit_tab[i,7])) 
        } else if (bestfit_tab[i,5] == "gamma"){
            probs_marg_s_3 <- pgamma(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "logis"){
            probs_marg_s_3 <- plogis(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))}
        ##calcula probabilidade conjunta
        probs_conj_3 <- BiCopCDF(probs_marg_d_3, probs_marg_s_3, obj = cop_lst[[i]])
        
        #calcula periodo de retorno
        (media[[i]]/(1 - probs_marg_d_3 - probs_marg_s_3 + probs_conj_3))
    }
    
    
    T_ou_fun <- function(d,s){
        #define objeto d
        if (bestfit_tab[i,1] == "lnorm"){
            probs_marg_d_4 <- plnorm(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "exp"){
            probs_marg_d_4 <-  pexp(d,
                                    as.numeric(bestfit_tab[i,2]),
                                    as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "norm"){
            probs_marg_d_4 <-  pnorm(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "weibull"){
            probs_marg_d_4 <- pweibull(d,
                                       as.numeric(bestfit_tab[i,2]),
                                       as.numeric(bestfit_tab[i,3])) 
        } else if (bestfit_tab[i,1] == "gamma"){
            probs_marg_d_4 <- pgamma(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "logis"){
            probs_marg_d_4 <- plogis(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))}
        #define objeto s
        if (bestfit_tab[i,5] == "lnorm"){
            probs_marg_s_4 <- plnorm(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "exp"){
            probs_marg_s_4 <-  pexp(s,
                                    as.numeric(bestfit_tab[i,6]),
                                    as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "norm"){
            probs_marg_s_4 <-  pnorm(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "weibull"){
            probs_marg_s_4 <- pweibull(s,
                                       as.numeric(bestfit_tab[i,6]),
                                       as.numeric(bestfit_tab[i,7])) 
        } else if (bestfit_tab[i,5] == "gamma"){
            probs_marg_s_4 <- pgamma(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "logis"){
            probs_marg_s_4 <- plogis(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))}
        
        probs_conj_4 <- BiCopCDF(probs_marg_d_4, probs_marg_s_4, obj = cop_lst[[i]])
        
        (media[[i]]/(1 - probs_conj_4))
        
    }
    
    #8.1 plot T_DS_ou####
    lvl <- 4^seq(0, 10, by = 1)
    
    T_ou_mat <- outer(xx, yy, FUN = T_ou_fun)
    contour.default(xx,yy, T_ou_mat, levels = lvl, labcex = 1.1,
                    main = paste("T DorS", bacias[i]), cex.main=1.5,axes = F)
    axis(side = 1, 
         at = seq(from = 0, to = max(xx), by = 2),
         cex.axis = 2,
         labels = if (i == 11) seq(from = 0, to = max(xx), by = 2) 
         else if (i == 12) seq(from = 0, to = max(xx), by = 2)
         else F)
    axis(side = 2,
         at = seq(from = 0, to = max(yy), by = 2),
         cex.axis = 2, las = 2,
         labels = if (i == 1) seq(from = 0, to = max(yy), by = 2) 
         else if (i == 3) seq(from = 0, to = max(yy), by = 2)
         else if (i == 5) seq(from = 0, to = max(yy), by = 2)
         else if (i == 7) seq(from = 0, to = max(yy), by = 2)
         else if (i == 7) seq(from = 0, to = max(yy), by = 2)
         else if (i == 9) seq(from = 0, to = max(yy), by = 2)
         else if (i == 11) seq(from = 0, to = max(yy), by = 2)
         else F)
    box(which = "plot")
    points(historico[[i]]$D, historico[[i]]$S, col = "blue" )
    points(historico[[i]]$D[historico[[i]]$Ano_i==2012], 
           historico[[i]]$S[historico[[i]]$Ano_i==2012], col = "red", pch = 16 )
    
    #8.2)plot T_DS_e####
    
    
    T_e_mat <- outer(xx, yy, FUN = T_e_fun)
    contour.default(xx,yy, T_e_mat, levels = lvl, labcex = 1.1,
                    main = paste("T D&S", bacias[i]), cex.main=1.5, axes = F)
    axis(side = 1, 
         at = seq(from = 0, to = max(xx), by = 2),
         labels = if (i == 11) seq(from = 0, to = max(xx), by = 2) 
         else if (i == 12) seq(from = 0, to = max(xx), by = 2)
         else F,
         cex.axis = 2)
    axis(side = 2,
         at = seq(from = 0, to = max(yy), by = 2), 
         labels = F, las = 2,
         cex.axis = 2)
    box(which = "plot")
    points(historico[[i]]$D, historico[[i]]$S, col = "blue" )
    points(historico[[i]]$D[historico[[i]]$Ano_i==2012], 
           historico[[i]]$S[historico[[i]]$Ano_i==2012], col = "red", pch = 16 )
    
    ##Periodo de retonrno medio
    T_med_e[[i]]  <- round(T_e_fun(6, 6),2)
    T_med_ou[[i]] <- round(T_ou_fun(6, 6),2)
}

title(xlab = "Duration (years)",
      ylab = "Severity",
      outer = TRUE, line = 2.3,lheight = 6, cex.lab = 2.5)

dev.off()
names(T_med_e) <- names(T_med_ou) <- bacias



########
colnames(spi)
nomes_bacias = c("Coreau", "Litoral", "Curu", "Acarau",
                 "Serra da Ibiapaba",  "Metropolitana", "Baixo Jaguaribe", "Banabuiu",
                 "Sertoes de Crateús", "Medio Jaguaribe", "Alto Jaguaribe", "Salgado")

i=2
for(i in 1:12){
    T_med_e <- list()
    T_med_ou <- list()
    xx <- seq(0, 11, length.out = 25)
    yy <- seq(0, 11, length.out = 25)
    
    png(paste0("data/EXTRM_SECAS/",nomes_bacias[i],"/T_e_ou_.png"), width = 4200, height = 2100, antialias = "subpixel", pointsize = 17, res = 300)
    
    par(mfrow = c(1,2),                         # 6x4 layout
        oma = c(5,5,0,0) + 0.1,           # 5 rows of text at the outer left and bottom margin
        mar = c(1,1,1,1) + 0.1,           
        mgp = c(2, 1, 0))                 # axis label at 2 rows distance, tick labels at 1 row
    

    
    ## Defino funcao para bacia em analise
    T_e_fun <- function(d,s){
        
        #define objeto d
        if (bestfit_tab[i,1] == "lnorm"){
            probs_marg_d_3 <- plnorm(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "exp"){
            probs_marg_d_3 <-  pexp(d,
                                    as.numeric(bestfit_tab[i,2]),
                                    as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "norm"){
            probs_marg_d_3 <-  pnorm(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "weibull"){
            probs_marg_d_3 <- pweibull(d,
                                       as.numeric(bestfit_tab[i,2]),
                                       as.numeric(bestfit_tab[i,3])) 
        } else if (bestfit_tab[i,1] == "gamma"){
            probs_marg_d_3 <- pgamma(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "logis"){
            probs_marg_d_3 <- plogis(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))}
        #define objeto s
        if (bestfit_tab[i,5] == "lnorm"){
            probs_marg_s_3 <- plnorm(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "exp"){
            probs_marg_s_3 <-  pexp(s,
                                    as.numeric(bestfit_tab[i,6]),
                                    as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "norm"){
            probs_marg_s_3 <-  pnorm(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "weibull"){
            probs_marg_s_3 <- pweibull(s,
                                       as.numeric(bestfit_tab[i,6]),
                                       as.numeric(bestfit_tab[i,7])) 
        } else if (bestfit_tab[i,5] == "gamma"){
            probs_marg_s_3 <- pgamma(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "logis"){
            probs_marg_s_3 <- plogis(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))}
        ##calcula probabilidade conjunta
        probs_conj_3 <- BiCopCDF(probs_marg_d_3, probs_marg_s_3, obj = cop_lst[[i]])
        
        #calcula periodo de retorno
        (media[[i]]/(1 - probs_marg_d_3 - probs_marg_s_3 + probs_conj_3))
    }
    
    
    T_ou_fun <- function(d,s){
        #define objeto d
        if (bestfit_tab[i,1] == "lnorm"){
            probs_marg_d_4 <- plnorm(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "exp"){
            probs_marg_d_4 <-  pexp(d,
                                    as.numeric(bestfit_tab[i,2]),
                                    as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "norm"){
            probs_marg_d_4 <-  pnorm(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "weibull"){
            probs_marg_d_4 <- pweibull(d,
                                       as.numeric(bestfit_tab[i,2]),
                                       as.numeric(bestfit_tab[i,3])) 
        } else if (bestfit_tab[i,1] == "gamma"){
            probs_marg_d_4 <- pgamma(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))
        } else if (bestfit_tab[i,1] == "logis"){
            probs_marg_d_4 <- plogis(d,
                                     as.numeric(bestfit_tab[i,2]),
                                     as.numeric(bestfit_tab[i,3]))}
        #define objeto s
        if (bestfit_tab[i,5] == "lnorm"){
            probs_marg_s_4 <- plnorm(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "exp"){
            probs_marg_s_4 <-  pexp(s,
                                    as.numeric(bestfit_tab[i,6]),
                                    as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "norm"){
            probs_marg_s_4 <-  pnorm(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "weibull"){
            probs_marg_s_4 <- pweibull(s,
                                       as.numeric(bestfit_tab[i,6]),
                                       as.numeric(bestfit_tab[i,7])) 
        } else if (bestfit_tab[i,5] == "gamma"){
            probs_marg_s_4 <- pgamma(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))
        } else if (bestfit_tab[i,5] == "logis"){
            probs_marg_s_4 <- plogis(s,
                                     as.numeric(bestfit_tab[i,6]),
                                     as.numeric(bestfit_tab[i,7]))}
        
        probs_conj_4 <- BiCopCDF(probs_marg_d_4, probs_marg_s_4, obj = cop_lst[[i]])
        
        (media[[i]]/(1 - probs_conj_4))
        
    }
    
    #8.1 plot T_DS_ou####
    lvl <- 4^seq(0, 10, by = 1)
    
    T_ou_mat <- outer(xx, yy, FUN = T_ou_fun)
    contour.default(xx,yy, T_ou_mat, levels = lvl, labcex = 1.1,
                    main = "T DorS", cex.main=1.5,axes = F)
    axis(side = 1, 
         at = seq(from = 0, to = max(xx), by = 2),
         cex.axis = 2,
         labels = if (i == 11) seq(from = 0, to = max(xx), by = 2) 
         else if (i == 12) seq(from = 0, to = max(xx), by = 2)
         else F)
    axis(side = 2,
         at = seq(from = 0, to = max(yy), by = 2),
         cex.axis = 2, las = 2,
         labels = if (i == 1) seq(from = 0, to = max(yy), by = 2) 
         else if (i == 3) seq(from = 0, to = max(yy), by = 2)
         else if (i == 5) seq(from = 0, to = max(yy), by = 2)
         else if (i == 7) seq(from = 0, to = max(yy), by = 2)
         else if (i == 7) seq(from = 0, to = max(yy), by = 2)
         else if (i == 9) seq(from = 0, to = max(yy), by = 2)
         else if (i == 11) seq(from = 0, to = max(yy), by = 2)
         else F)
    box(which = "plot")
    points(historico[[i]]$D, historico[[i]]$S, col = "blue" )
    points(historico[[i]]$D[historico[[i]]$Ano_i==2012], 
           historico[[i]]$S[historico[[i]]$Ano_i==2012], col = "red", pch = 16 )
    
    #8.2)plot T_DS_e####
    
    
    T_e_mat <- outer(xx, yy, FUN = T_e_fun)
    contour.default(xx,yy, T_e_mat, levels = lvl, labcex = 1.1,
                    main = "T D&S", cex.main=1.5, axes = F)
    axis(side = 1, 
         at = seq(from = 0, to = max(xx), by = 2),
         labels = if (i == 11) seq(from = 0, to = max(xx), by = 2) 
         else if (i == 12) seq(from = 0, to = max(xx), by = 2)
         else F,
         cex.axis = 2)
    axis(side = 2,
         at = seq(from = 0, to = max(yy), by = 2), 
         labels = F, las = 2,
         cex.axis = 2)
    box(which = "plot")
    points(historico[[i]]$D, historico[[i]]$S, col = "blue" )
    points(historico[[i]]$D[historico[[i]]$Ano_i==2012], 
           historico[[i]]$S[historico[[i]]$Ano_i==2012], col = "red", pch = 16 )
    
    ##Periodo de retonrno medio
    T_med_e[[i]]  <- round(T_e_fun(6, 6),2)
    T_med_ou[[i]] <- round(T_ou_fun(6, 6),2)
    
    
    probs_conj_4 <- BiCopCDF(probs_marg_d_4, probs_marg_s_4, obj = cop_lst[[i]])
    
    (media[[i]]/(1 - probs_conj_4))
    
    
    title(xlab = "Duração (anos)",
          ylab = "Severidade",
          outer = TRUE, line = 2.3,lheight = 3, cex.lab = 2.5)
    
    dev.off()
    names(T_med_e) <- names(T_med_ou) <- bacias
    
    
        
}
#############

## Defino funcao para bacia em analise
T_e_fun <- function(d,s){
  
  #define objeto d
  if (bestfit_tab[i,1] == "lnorm"){
    probs_marg_d_3 <- plnorm(d,
                             as.numeric(bestfit_tab[i,2]),
                             as.numeric(bestfit_tab[i,3]))
  } else if (bestfit_tab[i,1] == "exp"){
    probs_marg_d_3 <-  pexp(d,
                            as.numeric(bestfit_tab[i,2]),
                            as.numeric(bestfit_tab[i,3]))
  } else if (bestfit_tab[i,1] == "norm"){
    probs_marg_d_3 <-  pnorm(d,
                             as.numeric(bestfit_tab[i,2]),
                             as.numeric(bestfit_tab[i,3]))
  } else if (bestfit_tab[i,1] == "weibull"){
    probs_marg_d_3 <- pweibull(d,
                               as.numeric(bestfit_tab[i,2]),
                               as.numeric(bestfit_tab[i,3])) 
  } else if (bestfit_tab[i,1] == "gamma"){
    probs_marg_d_3 <- pgamma(d,
                             as.numeric(bestfit_tab[i,2]),
                             as.numeric(bestfit_tab[i,3]))
  } else if (bestfit_tab[i,1] == "logis"){
    probs_marg_d_3 <- plogis(d,
                             as.numeric(bestfit_tab[i,2]),
                             as.numeric(bestfit_tab[i,3]))}
  #define objeto s
  if (bestfit_tab[i,5] == "lnorm"){
    probs_marg_s_3 <- plnorm(s,
                             as.numeric(bestfit_tab[i,6]),
                             as.numeric(bestfit_tab[i,7]))
  } else if (bestfit_tab[i,5] == "exp"){
    probs_marg_s_3 <-  pexp(s,
                            as.numeric(bestfit_tab[i,6]),
                            as.numeric(bestfit_tab[i,7]))
  } else if (bestfit_tab[i,5] == "norm"){
    probs_marg_s_3 <-  pnorm(s,
                             as.numeric(bestfit_tab[i,6]),
                             as.numeric(bestfit_tab[i,7]))
  } else if (bestfit_tab[i,5] == "weibull"){
    probs_marg_s_3 <- pweibull(s,
                               as.numeric(bestfit_tab[i,6]),
                               as.numeric(bestfit_tab[i,7])) 
  } else if (bestfit_tab[i,5] == "gamma"){
    probs_marg_s_3 <- pgamma(s,
                             as.numeric(bestfit_tab[i,6]),
                             as.numeric(bestfit_tab[i,7]))
  } else if (bestfit_tab[i,5] == "logis"){
    probs_marg_s_3 <- plogis(s,
                             as.numeric(bestfit_tab[i,6]),
                             as.numeric(bestfit_tab[i,7]))}
  ##calcula probabilidade conjunta
  probs_conj_3 <- BiCopCDF(probs_marg_d_3, probs_marg_s_3, obj = cop_lst[[i]])
  
  #calcula periodo de retorno
  (media[[i]]/(1 - probs_marg_d_3 - probs_marg_s_3 + probs_conj_3))
}


T_ou_fun <- function(d,s){
  #define objeto d
  if (bestfit_tab[i,1] == "lnorm"){
    probs_marg_d_4 <- plnorm(d,
                             as.numeric(bestfit_tab[i,2]),
                             as.numeric(bestfit_tab[i,3]))
  } else if (bestfit_tab[i,1] == "exp"){
    probs_marg_d_4 <-  pexp(d,
                            as.numeric(bestfit_tab[i,2]),
                            as.numeric(bestfit_tab[i,3]))
  } else if (bestfit_tab[i,1] == "norm"){
    probs_marg_d_4 <-  pnorm(d,
                             as.numeric(bestfit_tab[i,2]),
                             as.numeric(bestfit_tab[i,3]))
  } else if (bestfit_tab[i,1] == "weibull"){
    probs_marg_d_4 <- pweibull(d,
                               as.numeric(bestfit_tab[i,2]),
                               as.numeric(bestfit_tab[i,3])) 
  } else if (bestfit_tab[i,1] == "gamma"){
    probs_marg_d_4 <- pgamma(d,
                             as.numeric(bestfit_tab[i,2]),
                             as.numeric(bestfit_tab[i,3]))
  } else if (bestfit_tab[i,1] == "logis"){
    probs_marg_d_4 <- plogis(d,
                             as.numeric(bestfit_tab[i,2]),
                             as.numeric(bestfit_tab[i,3]))}
  #define objeto s
  if (bestfit_tab[i,5] == "lnorm"){
    probs_marg_s_4 <- plnorm(s,
                             as.numeric(bestfit_tab[i,6]),
                             as.numeric(bestfit_tab[i,7]))
  } else if (bestfit_tab[i,5] == "exp"){
    probs_marg_s_4 <-  pexp(s,
                            as.numeric(bestfit_tab[i,6]),
                            as.numeric(bestfit_tab[i,7]))
  } else if (bestfit_tab[i,5] == "norm"){
    probs_marg_s_4 <-  pnorm(s,
                             as.numeric(bestfit_tab[i,6]),
                             as.numeric(bestfit_tab[i,7]))
  } else if (bestfit_tab[i,5] == "weibull"){
    probs_marg_s_4 <- pweibull(s,
                               as.numeric(bestfit_tab[i,6]),
                               as.numeric(bestfit_tab[i,7])) 
  } else if (bestfit_tab[i,5] == "gamma"){
    probs_marg_s_4 <- pgamma(s,
                             as.numeric(bestfit_tab[i,6]),
                             as.numeric(bestfit_tab[i,7]))
  } else if (bestfit_tab[i,5] == "logis"){
    probs_marg_s_4 <- plogis(s,
                             as.numeric(bestfit_tab[i,6]),
                             as.numeric(bestfit_tab[i,7]))}
  
  probs_conj_4 <- BiCopCDF(probs_marg_d_4, probs_marg_s_4, obj = cop_lst[[i]])
  
  (media[[i]]/(1 - probs_conj_4))
  
}



library(ggrepel)
library(tidyverse)

nomes_bacias = c("Coreau", "Litoral", "Curu", "Acarau",
                 "Serra da Ibiapaba",  "Metropolitana", "Baixo Jaguaribe", "Banabuiu",
                 "Sertoes de Crateús", "Medio Jaguaribe", "Alto Jaguaribe", "Salgado")

i = 1

for(i in 1:length(nomes_bacias)){
  print(nomes_bacias[i])
  T_med_e <- list()
  T_med_ou <- list()
  xx <- seq(0, 11, length.out = 25)
  yy <- seq(0, 11, length.out = 25)
  
  png(paste0("data/EXTRM_SECAS/",nomes_bacias[i],"/T_e_ou_.png"), width = 4200,
            height = 2100, antialias = "subpixel", pointsize = 17, res = 300)
  
  par(mfrow = c(1,2),                         # 6x4 layout
      oma = c(5,5,0,0) + 0.1,           # 5 rows of text at the outer left and bottom margin
      mar = c(1,1,1,1) + 0.1,           
      mgp = c(2, 1, 0))                 # axis label at 2 rows distance, tick labels at 1 row
  
  
  #8.1 plot T_DS_ou####
  lvl <- 2^seq(0, 10, by = 1)
  
  T_ou_mat <- outer(xx, yy, FUN = T_ou_fun)
  contour.default(xx,yy, T_ou_mat, levels = lvl, labcex = 1.1,
                  main = "T DorS", cex.main=1.5,axes = F)
  axis(side = 1, 
       at = seq(from = 0, to = max(xx), by = 2),
       cex.axis = 2,
       labels = seq(from = 0, to = max(xx), by = 2))
  axis(side = 2,
       at = seq(from = 0, to = max(yy), by = 2),
       cex.axis = 2, las = 2,
       labels = seq(from = 0, to = max(yy), by = 2))
  box(which = "plot")
  points(historico[[i]]$D, historico[[i]]$S, col = "blue" )
  
  piores_anos = historico[[i]] %>%
    arrange(desc(S)) %>%
    #dplyr::select(Ano_i) %>%
    slice_head(n = 3)
  piores_anos$Label = paste0(piores_anos$Ano_i,'-',piores_anos$Ano_f)
  
  points(piores_anos$D[1], 
         piores_anos$S[1], col = "red", pch = 16 )
  text(piores_anos$D,piores_anos$S,
       labels=piores_anos$Label,cex= 0.5, pos=3)
  
  #geom_label_repel(data=piores_anos,x=D,y=S,aes(label = Label),
   #                size = 5, box.padding = unit(0.35, "lines"),
    #               point.padding = unit(0.5, "lines"))
  #8.2)plot T_DS_e####
  
  
  T_e_mat <- outer(xx, yy, FUN = T_e_fun)
  contour.default(xx,yy, T_e_mat, levels = lvl, labcex = 1.1,
                  main = "T D&S", cex.main=1.5, axes = F)
  axis(side = 1, 
       at = seq(from = 0, to = max(xx), by = 2),
       labels = seq(from = 0, to = max(xx), by = 2),
       cex.axis = 2)
  axis(side = 2,
       at = seq(from = 0, to = max(yy), by = 2), 
       labels = F, las = 2,
       cex.axis = 2)
  box(which = "plot")
  points(historico[[i]]$D, historico[[i]]$S, col = "blue" )
  points(piores_anos$D[1], 
         piores_anos$S[1], col = "red", pch = 16 )
  text(piores_anos$D,piores_anos$S,
       labels=piores_anos$Label,cex= 0.5, pos=3)
  ##Periodo de retonrno medio
  T_med_e[[i]]  <- round(T_e_fun(6, 6),2)
  T_med_ou[[i]] <- round(T_ou_fun(6, 6),2)
  
  
  
  
  title(xlab = "Duração (anos)",
        ylab = "Severidade",
        outer = TRUE, line = 2.3,lheight = 3, cex.lab = 2.5)
  
  dev.off()
  
  
  
}