library(haven)
library(lmtest)
library(plm)
library(sjPlot)
library(sjlabelled)
setwd("~/Desktop/HKBUY3S1/ECON3096/ECON 3096 Project 2/Group_A_Paper_3")
A3DATA <- read.table("A3_data.txt",header = TRUE,sep = ",")
str(A3DATA)
subset(A3DATA,)
length(unique(A3DATA$uspto_class)) #7248
length(unique(A3DATA$count_usa))
sum(A3DATA$count_usa)
unique(A3DATA$grntyr)
length(unique(A3DATA$class_id)) #7248

#Q1 
#Regression:
A3DATA2 <- pdata.frame(A3DATA,index = c("class_id"),drop.index = TRUE,row.names = FALSE)

model1 <- plm(count_usa~treat+count_for+factor(grntyr), model = "within",data = A3DATA2)
model1$vcov <- vcovHC(model1, type = "HC1")

model2 <- plm(count_usa~treat+factor(grntyr), model = "within",data = A3DATA2)
model2$vcov <- vcovHC(model2, type = "HC1")

model3 <- plm(count_usa~count_cl+count_cl_2+count_for+factor(grntyr), model = "within",data = A3DATA2)
model3$vcov <- vcovHC(model3, type = "HC1")

model4 <- plm(count_usa~count_cl+count_for+factor(grntyr), model = "within",data = A3DATA2)
model4$vcov <- vcovHC(model4, type = "HC1")

model5 <- plm(count_usa~count_cl+factor(grntyr), model = "within",data = A3DATA2)
model5$vcov <- vcovHC(model5, type = "HC1")

model6 <- plm(count_usa~year_conf+year_conf_2+count_for+factor(grntyr), model = "within",data = A3DATA2)
model6$vcov <- vcovHC(model6, type = "HC1")

model7 <- plm(count_usa~year_conf+count_for+factor(grntyr), model = "within",data = A3DATA2)
model7$vcov <- vcovHC(model7, type = "HC1")

model8 <- plm(count_usa~year_conf+factor(grntyr), model = "within",data = A3DATA2)
model8$vcov <- vcovHC(model8, type = "HC1")

#Table:
tab_model(model1, model2, model3, model4, model5,model6,model7,model8,
          show.ci = F, 
          show.se = T, 
          collapse.se = T, 
          digits = 3,
          p.style = "stars", 
          show.intercept = F,
          strings = NULL,
          terms = c("treat","count_cl","count_cl_2","year_conf",
                    "year_conf_2","count_for"),
          pred.labels = c("Subclass has at least one license",
                          "Number of patents by foreign investor",
                          "Number of licenses",
                          "Number of licenses squared",
                          "Remaining lifetime of licensed patents",
                          "Remaining lifetime of licensed patents squared (× 100)"),
          dv.labels = c("","","Table 2","OLS","Regressions","",""),
          string.pred = "Coeffcient")

#Q2:
print("As see above in model (1) and model (2), the coefficient of treat is significantly positive at 0.151 and 0.255, indicating that the policy compulsory licensing after 1919 encouraged more invention in USA. The result is consistent with the author’s hypothesis. Besides, the model (3)~(8) show the number of licenses, remaining lifetime of licensed, number of patents by foreign inventors could also affect number of patents by the US inventors at different levels.")

#Q3:
library(dplyr)
library(ggplot2)
library(reshape2)
subDATA <- A3DATA %>% group_by(grntyr,licensed_class) %>% summarise(ave_patents=mean(count_usa),.groups="keep")
subDATA$group <- ifelse(subDATA$licensed_class==0,"control","treat")

ggplot(subDATA,aes(x=grntyr, y= ave_patents, group=group,color=group)) + 
  geom_line(show.legend = TRUE) + 
  geom_vline(xintercept = 1919,lty=2) + 
  xlab("Year") + ylab("Average Number of Patents by the US Inventors")

print("Parallel time trend assumption is that the difference in the treatment and control groups (bias) is constant over time. To confirm the assumption, we draw a line plot. As shown above in the plot, these two lines seem parallel to each other pre-war (before 1918) in general.")

#Q4:
A3DATA2$newvar <- A3DATA2$grntyr*A3DATA2$licensed_class
model9 <- plm(count_usa~treat+newvar+factor(grntyr), model = "within",data = A3DATA2)
model9$vcov <- vcovHC(model9, type = "HC1")

tab_model(model9,
          show.ci = F, 
          show.se = T, 
          collapse.se = T, 
          digits = 3,
          p.style = "stars", 
          show.intercept = F,
          strings = NULL,
          terms = c("treat","newvar"),
          string.pred = "Coeffcient")

print("Consider parallel time trend assumption, we add “licensed_class* grntyr” as a new variable “newvar” into former regression model (2), and we get the coefficient  is 0.001, which is not  significant, so we assume the parallel time trend.")

#Q5:
#Regression:
model10 <- plm(count_germany~treat+count_for+factor(grntyr), model = "within",data = A3DATA2)
model10$vcov <- vcovHC(model10, type = "HC1")


model11 <- plm(count_germany~treat+factor(grntyr), model = "within",data = A3DATA2)
model11$vcov <- vcovHC(model11, type = "HC1")


model12 <- plm(count_germany~count_cl+count_cl_2+count_for+factor(grntyr), model = "within",data = A3DATA2)
model12$vcov <- vcovHC(model12, type = "HC1")

model13 <- plm(count_germany~count_cl+count_for+factor(grntyr), model = "within",data = A3DATA2)
model13$vcov <- vcovHC(model13, type = "HC1")

model14 <- plm(count_germany~count_cl+factor(grntyr), model = "within",data = A3DATA2)
model14$vcov <- vcovHC(model14, type = "HC1")


model15 <- plm(count_germany~year_conf+year_conf_2+count_for+factor(grntyr), model = "within",data = A3DATA2)
model15$vcov <- vcovHC(model15, type = "HC1")

model16 <- plm(count_germany~year_conf+count_for+factor(grntyr), model = "within",data = A3DATA2)
model16$vcov <- vcovHC(model16, type = "HC1")

model17 <- plm(count_germany~year_conf+factor(grntyr), model = "within",data = A3DATA2)
model17$vcov <- vcovHC(model17, type = "HC1")

#Table:
tab_model(model10, model11, model12, model13, model14,model15,model16,model17,
          show.ci = F, 
          show.se = T, 
          collapse.se = T, 
          digits = 5,
          p.style = "stars", 
          show.intercept = F,
          strings = NULL,
          terms = c("treat","count_cl","count_cl_2","year_conf",
                    "year_conf_2","count_for"),
          pred.labels = c("Subclass has at least one license",
                          "Number of patents by foreign investor",
                          "Number of licenses",
                          "Number of licenses squared",
                          "Remaining lifetime of licensed patents",
                          "Remaining lifetime of licensed patents squared (× 100)"),
          dv.labels = c("Question", "5","Table"),
          string.pred = "Coeffcient")

print("Here we just replace the dependent variable.
      By using the number of patents by Germany inventors, if only the “treat” involved in the regression i.e. model(2), it seems that the Act encouraged German invention, however, by adding “count_for” into model i.e. model(1), we see that the treat is not significant and even the sign of coefficient is negative, so we may infer that the Act does not encourage German invention. It is reasonable that the Act violate the interest of Germany.")

#Q6:
model18 <- plm(count_cl~count_cl_itt+factor(grntyr), model = "within",data = A3DATA2)
model18$vcov <- vcovHC(model18, method="white1",type = "HC1")

model19<- plm(year_conf~year_conf_itt+factor(grntyr), model = "within",data = A3DATA2)
model19$vcov <- vcovHC(model19, method="white1",type = "HC1")

A3DATA2$fitted_count_cl <- fitted(model18)
A3DATA2$fitted_year_conf <- fitted(model19)

model20<- plm(count_usa~fitted_count_cl+factor(grntyr), model = "within",data = A3DATA2)

model21<- plm(count_usa~fitted_year_conf +factor(grntyr), model = "within",data = A3DATA2)
model21$vcov <- vcovHC(model21, method="white1",type = "HC1")

tab_model(model18,model19, model20, model21,
          show.ci = F, 
          show.se = T, 
          collapse.se = T, 
          digits = 3,
          p.style = "stars", 
          show.intercept = F,
          strings = NULL,
          terms = c("count_cl_itt","year_conf_itt", "fitted_count_cl","fitted_year_conf"),
          pred.labels = c("Number of enemy patents",
                          "Remaining lifetime of enemy patents",
                          "Number of licenses",
                          "Remaining lifetime of licensed patents"),
          dv.labels = c("(First","Stage)","(Second","Stage)"),
          string.pred = "Coeffcient")
