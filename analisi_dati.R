# install.packages(c("mplot", "leaps", "bestglm", "car"))
###############################################################################

setwd("C:\\aaa_lavori\\lav_pigna_metabolic\\analisi")


overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}


###############################################################################

library(leaps)
library(openxlsx)
library(tidyverse)
library(skimr)
library(corrplot)
library(caret)
library(lme4)
library(modelsummary)
library(pandoc)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(sjPlot)
library(emmeans)
library(ggpmisc)
library(MuMIn)
library(ggstatsplot)
library(gtsummary)


 
#########################################################################

df<-read.xlsx("pigna_data_finale.xlsx",1)

str(df)
skim(df)
cat("\14")

names(df)[6]="ALA_D"

dependent_vars=c("ALA","ALA_D","AsA","Car","Chl.a","Chl.b","GSH","H202","MDA","P_tot")

laurus_df<-df[df$species=="Laurus",]    
artemisia_df<-df[df$species=="Artemisia",]    
arundo_df<-df[df$species=="Arundo",]    


res_lm_laurus=list()
res_lm_artemisia=list()
res_lm_arundo=list()

########################################################################################
# t_lag0_mean + t_lag7_max + t_lag7_mean + t_lag7_min



########################################################################################
# correlation matrix by month




########################################################################################

for ( i in 1:length(dependent_vars)) {

   
laurus_lm <- lm(paste0(dependent_vars[i],"~ ."), data = laurus_df[,c(i+4,16:37)])
laurus_step<- try(step(laurus_lm, direction = "backward"))
res_lm_laurus[[i]]<-laurus_step

artemisia_lm <- lm(paste0(dependent_vars[i],"~ ."), data = artemisia_df[,c(i+4,16:37)])
artemisia_step<- step(artemisia_lm, direction = "backward") 
res_lm_artemisia[[i]]<-artemisia_step

arundo_lm <- lm(paste0(dependent_vars[i],"~ ."), data = arundo_df[,c(i+4,16:37)])
arundo_step<- step(arundo_lm, direction = "backward") 
res_lm_arundo[[i]]<-arundo_step


}



###############################################################################################

capture.output(lapply(res_lm_laurus,summary),file="res_lm_laurus.txt")

capture.output(lapply(res_lm_artemisia,summary),file="res_lm_artemisia.txt")

capture.output(lapply(res_lm_arundo,summary),file="res_lm_arundo.txt")

###############################################################################################




#########################################################################
names(res_lm_laurus)=dependent_vars

modelsummary(res_lm_laurus,
             estimate = "{estimate}{stars}",
             gof_omit = "Deviance|BIC|Log.Lik.|F|RMSE",
             , output = "res_lm_laurus.html")


names(res_lm_artemisia)=dependent_vars

modelsummary(res_lm_artemisia,
             estimate = "{estimate}{stars}",
             gof_omit = "Deviance|BIC|Log.Lik.|F|RMSE",
             , output = "res_lm_artemisia.html")


names(res_lm_arundo)=dependent_vars

modelsummary(res_lm_arundo,
             estimate = "{estimate}{stars}",
             gof_omit = "Deviance|BIC|Log.Lik.|F|RMSE",
             , output = "res_lm_arundo.html")

###########################################################################################

lapply(res_lm_laurus,formula)
lapply(res_lm_arundo,formula)
lapply(res_lm_artemisia,formula)


###########################################################################################


ggcorrmat(
  data = arundo_df[arundo_df$sort_month %in% c(3,4,5),],
  cor.vars = dependent_vars,
  ggcorrplot.args =list(insig ="blank"),
  annotation.args = list(
    tag_levels = "a",
    title = "Matrix of correlation metabolites Arundo donax",
    caption = "BBB"
  )
)


ggcorrmat(
  data = laurus_df[laurus_df$sort_month %in% c(3,4,5),],
  cor.vars = dependent_vars,
  ggcorrplot.args =list(insig ="blank"),
  annotation.args = list(
    tag_levels = "a",
    title = "Matrix of correlation metabolites Laurus nobilis",
    caption = "BBB"
  )
)




ggcorrmat(
  data = artemisia_df[artemisia_df$sort_month %in% c(3,4,5),],
  cor.vars = dependent_vars,
  ggcorrplot.args =list(insig ="blank"),
  annotation.args = list(
    tag_levels = "a",
    title = "Matrix of correlation metabolites Artermisia verlotiorum",
    caption = "BBB"
  )
)

#####################################################################àààààààààà

# https://rpubs.com/Alema/1000474


# r.squaredGLMM

# https://modelsummary.com/vignettes/modelsummary.html
# https://zief0002.github.io/benevolent-anteater/notes/13-lmer-average-change-over-time.html
# https://www.r-bloggers.com/2014/11/visualizing-generalized-linear-mixed-effects-models-part-2-rstats-lme4/

