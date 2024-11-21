factors <- read.csv("C:/Users/Admin/Desktop/heart_disease_health_indicators_BRFSS2015.csv")
doplotu<- factors %>% mutate(bmi = case_when(BMI <18.5 ~"niedowaga", BMI <25 ~"prawidlowa", BMI<30 ~"nadwaga", .default = "otylosc")) %>% 
  group_by(bmi) %>%
  summarise(BP = mean(HighBP), chol = mean(HighChol), zawal = mean(HeartDiseaseorAttack), udar =  mean(Stroke), Palacze = mean(Smoker))
name <- doplotu$bmi
doplotu <- as.matrix(doplotu)
rownames(doplotu) <- name
doplotu <- doplotu[,2:ncol(doplotu)]
doplotu <- apply(doplotu,c(1,2), as.numeric)
t(doplotu)
heatmap(t(as.matrix(doplotu)))
heatmap(as.matrix(doplotu), scale = "column")
r1 <- factors %>%
  filter(Smoker == 1) %>%
  summarise(BP = mean(HighBP), chol = mean(HighChol), zawal = mean(HeartDiseaseorAttack), udar =  mean(Stroke))
r2 <- factors %>%
  filter(PhysActivity == 1) %>%
  summarise(BP = mean(HighBP), chol = mean(HighChol), zawal = mean(HeartDiseaseorAttack), udar =  mean(Stroke))
r3 <- factors %>%
  filter(HvyAlcoholConsump == 1) %>%
  summarise(BP = mean(HighBP), chol = mean(HighChol), zawal = mean(HeartDiseaseorAttack), udar =  mean(Stroke))
r4 <- factors %>%
  filter(Fruits == 1) %>%
  summarise(BP = mean(HighBP), chol = mean(HighChol), zawal = mean(HeartDiseaseorAttack), udar =  mean(Stroke))
r5 <- factors %>%
  filter(Veggies == 1) %>%
  summarise(BP = mean(HighBP), chol = mean(HighChol), zawal = mean(HeartDiseaseorAttack), udar =  mean(Stroke))
doplotu2 <- matrix(c(r1,r2,r3,r4,r5), ncol = 4, byrow = TRUE) 
colnames(doplotu2) <- c("BP", "chol", "zawal", "udar")
rownames(doplotu2) <- c("Smoker","PhysActivity", "Alcohol", "Owoce", "Warzywa")
doplotu2 <- apply(doplotu2,c(1,2), as.numeric)
heatmap(doplotu2, scale = "column")
macierz <- cor(factors %>%select(HeartDiseaseorAttack,HighBP,HighChol,BMI, Smoker, Stroke, Diabetes,PhysActivity))