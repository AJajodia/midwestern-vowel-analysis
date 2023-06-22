data(pb52)

write_csv(pb52, "/Users/anu/Sociophonetics Project/pb52.csv")
tibble52 <- as_tibble(pb52)

women <- dplyr::filter(women, Vowel != "3'")
# men vowels Labov normalized (final)
men_vowels

mathias <- data.frame(read_tsv("mathias_normalized.txt"))
mathias <- arrange(mathias, Vowel)
isabel <- data.frame(read_tsv("isabel_normalized.txt"))
isabel <- arrange(isabel,Vowel)
summary(women)
summary(men)

# preparing statistics
# standard deviation for each vowel men
men_sd <- men %>% group_by(Vowel) %>% summarise(stdevf1=sd(F1.), stdevf2=sd(F2.), stdevEuc=sd(sqrt(F2.^2+F1.^2))) %>% arrange(Vowel)
women_sd <- women %>% group_by(Vowel) %>% summarise(stdevf1=sd(F1.), stdevf2=sd(F2.), stdevEuc=sd(sqrt(F2.^2+F1.^2))) %>% arrange(Vowel)
women_labov <- as_tibble(women_labov)
women_labov <- dplyr::filter(women_labov, Vowel != "3'")
women_labov
men_statistics_f1 = tibble()
# t tests
for (i in 1:9){
  x = men_labov[i,4]
  mu = mathias[i,4]
  sd = men_sd[i,2]
  men_statistics_f1[i,1] = mathias[i,2]
  men_statistics_f1[i,2] = (x-mu)/sd
  men_statistics_f1[i,3]=dnorm(as.double(men_statistics_f1[i,2]), mean = 0, sd = 1)
}

men_statistics_f2 = tibble()
for (i in 1:9){
  x = men_labov[i,5]
  mu = mathias[i,5]
  sd = men_sd[i,3]
  men_statistics_f2[i,1] = mathias[i,2]
  men_statistics_f2[i,2] = (x-mu)/sd
  men_statistics_f2[i,3]=dnorm(as.double(men_statistics_f2[i,2]), mean = 0, sd = 1)
}
men_statistics_euc = tibble()
for (i in 1:9){
  x = sqrt((mathias[i,4]-men_labov[i,4])^2+(mathias[i,5]-men_labov[i,5])^2)
  sd = men_sd[i,4]
  men_statistics_euc[i,1] = mathias[i,2]
  men_statistics_euc[i,2] = (x)/sd
  men_statistics_euc[i,3]=dnorm(as.double(men_statistics_euc[i,2]), mean = 0, sd = 1)
}
men_statistics_euc

women_statistics_f1 = tibble()
# t tests
for (i in 1:9){
  x = women_labov[i,4]
  mu = isabel[i,4]
  sd = women_sd[i,2]
  women_statistics_f1[i,1] = isabel[i,2]
  women_statistics_f1[i,2] = (x-mu)/sd
  women_statistics_f1[i,3]=dnorm(as.double(women_statistics_f1[i,2]), mean = 0, sd = 1)
}

women_statistics_f2 = tibble()
for (i in 1:9){
  x = women_labov[i,5]
  mu = isabel[i,5]
  sd = women_sd[i,3]
  women_statistics_f2[i,1] = isabel[i,2]
  women_statistics_f2[i,2] = (x-mu)/sd
  women_statistics_f2[i,3]=dnorm(as.double(women_statistics_f2[i,2]), mean = 0, sd = 1)
}
women_statistics_euc = tibble()
for (i in 1:9){
  x = sqrt((isabel[i,4]-women_labov[i,4])^2+(isabel[i,5]-women_labov[i,5])^2)
  sd = women_sd[i,4]
  women_statistics_euc[i,1] = isabel[i,2]
  women_statistics_euc[i,2] = (x)/sd
  women_statistics_euc[i,3]=dnorm(as.double(women_statistics_euc[i,2]), mean = 0, sd = 1)
}
