View(bes19)
install.packages("tidyverse")
library(tidyverse)

tempdir()
dir.create(tempdir())

#create variables for political knowledge questions that total to give a score out of 6
bes19$qq1 <- ifelse(bes19$x01_1 == 1, 1, 0)
bes19$qq2 <- ifelse(bes19$x01_2 == 1, 1, 0)
bes19$qq3 <- ifelse(bes19$x01_3 == 2, 1, 0)
bes19$qq4 <- ifelse(bes19$x01_4 == 2, 1, 0)
bes19$qq5 <- ifelse(bes19$x01_5 == 1, 1, 0)
bes19$qq6 <- ifelse(bes19$x01_6 == 2, 1, 0)
bes19$qqt <- bes19$qq1 + bes19$qq2 + bes19$qq3 + bes19$qq4 + bes19$qq5 + bes19$qq6 #total out of 6

#create political correctness variable. Returns NA if ANY of the three questions are NA/DK. Otherwise sums how PC
#the response is. Minimum score is 1 per answer ergo the -3. Final score ranges from 0 (least PC) to 12 (most PC)
bes19$pc <- ifelse(bes19$r03 == -999 | bes19$r03 == -1 | bes19$r04 == -999 | bes19$r04 == -1 | bes19$r07 == -999 |  
bes19$r07 == -1, NA, bes19$r03 + bes19$r04 + bes19$r07 - 3)

#needed to make plots!
w18$generalElectionVote <- as.factor(w18$generalElectionVote) #any x-axis variable needs this before plotting

#recode W18 left-right spectrum "Don't knows" as NA
install.packages("car")
library(car)
w18$leftRight <- ifelse(w18$leftRight == 9999, NA, w18$leftRight)

#mean left-right position of voters by voting intention
(tapply(w18$leftRight, w18$generalElectionVote, mean, na.rm=T))

#subset W18 to only include those intending to vote Con, Lab, LD, BXP or Don't Knows
w18a <- filter(w18, generalElectionVote == 1 | generalElectionVote == 2 | generalElectionVote == 3 | 
  generalElectionVote == 8 | generalElectionVote == 9999)

#Plotting these with working scale, colours, labels, etc.
plota <- ggplot(w18a, aes(leftRight, colour=generalElectionVote)) + geom_density(bw=1, size=0.8) + 
  labs(colour = "Vote Intention", title="Proportion of Voters' Self-Identification on a Left-Right Axis, by Vote Intention", x="Left to Right Spectrum", y="Proportion") +  
  scale_color_manual(values=c("blue", "red", "orange", "black"), labels=c("Con", "Lab", "LD", "Don't Know")) + 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10), limits=c(0,10))

#ANOVA for spectrum based on party
lma <- lm(leftRight ~ generalElectionVote, data=w18a)
summary(lma)

#weighting party ID percentages
table(w18$partyId)
tapply(w18$wt_daily, w18$partyId, mean, na.rm=T)
11055*0.860690/378.25 #Conservative values, 1 from partyId
9160*0.9865568/378.25 #Labour values, 2 from partyId

#subset W18 to only include those who identify as Conservative or Labour
w18b <- filter(w18, partyId == 1 | partyId == 2)

#recode W18b Party ID strengths "Don't knows" as NA
w18b$partyIdStrength <- ifelse(w18b$partyIdStrength == 9999, NA, w18b$partyIdStrength)

#Proportion table for strength of Con and Lab IDs
idt <- table(w18b$partyId, w18b$partyIdStrength)
prop.table(idt, 1)

#create w18c and recode Johnson and Corbyn approval "Don't knows" as NA
w18c <- w18
w18c$likeJohnson <- ifelse(w18c$likeJohnson == 9999, NA, w18c$likeJohnson)
w18c$likeCorbyn <- ifelse(w18c$likeCorbyn == 9999, NA, w18c$likeCorbyn)

#two data subsets: Lab&DKs and Con&DKs:
w18cc <- filter(w18c, generalElectionVote == 2 | generalElectionVote == 9999) #c for Corbyn
w18cj <- filter(w18c, generalElectionVote == 1 | generalElectionVote == 9999) #j for Johnson

#create w18d
w18d <- w18
w18d <- filter(w18d, generalElectionVote == 1 | generalElectionVote == 2 | generalElectionVote == 9999)
w18d$age <- ifelse(w18d$age > 90, 91, w18d$age)
w18d$generalElectionVote <- as.factor(w18d$generalElectionVote)
plotd <- ggplot(w18d, aes(age, colour=generalElectionVote)) + geom_density(bw=1.5, size=0.8) + 
  labs(colour = "Vote Intention", title="Proportion of Voters' Ages, by Vote Intention", x="Age", y="Proportion") +  
  scale_color_manual(values=c("blue", "red", "black"), labels=c("Con", "Lab", "Don't Know")) + 
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80), limits=c(18,80))

#create w18e and remove "Would not vote", then recategorise all decided voters as 1, and Don't Knows stay as 9999
w18e <- w18
w18e <- filter(w18e, generalElectionVote == 1 | generalElectionVote == 2 | generalElectionVote == 3 | 
  generalElectionVote == 4 |generalElectionVote == 8 | generalElectionVote == 5 | generalElectionVote == 6 | 
  generalElectionVote == 7 |generalElectionVote == 11 | generalElectionVote == 12 | generalElectionVote == 13 | 
  generalElectionVote == 0 |  generalElectionVote == 9999)
w18e$generalElectionVote <- ifelse(w18e$generalElectionVote == 9999, 9999, 1)
w18e$age <- ifelse(w18e$age > 90, 91, w18e$age)
w18e$generalElectionVote <- as.factor(w18e$generalElectionVote)
plote <- ggplot(w18e, aes(age, colour=generalElectionVote)) + geom_density(bw=1.7, size=0.8) + 
  labs(colour = "Vote Intention", title="Proportion of Voters' Ages, by Decided and Undecided", x="Age", y="Proportion") + 
  scale_color_manual(values=c("blue", "black"), labels=c("Decided", "Don't Know")) + 
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80))

x1 <- subset(w18e$age, w18e$generalElectionVote == 1)
x9999 <- subset(w18e$age, w18e$generalElectionVote == 9999)
t.test(x1, x9999)

#create w18f for political engagement based on days discussing politics last week
w18f <- w18
w18f <- subset(w18f, w18f$discussPolDays == 0 | w18f$discussPolDays == 1 | w18f$discussPolDays == 2 | 
  w18f$discussPolDays == 3 | w18f$discussPolDays == 4 | w18f$discussPolDays == 5 | w18f$discussPolDays == 6 | 
  w18f$discussPolDays == 7)
w18f <- filter(w18f, generalElectionVote == 1 | generalElectionVote == 2 | generalElectionVote == 3 | 
  generalElectionVote == 4 |generalElectionVote == 8 | generalElectionVote == 5 | generalElectionVote == 6 | 
  generalElectionVote == 7 |generalElectionVote == 11 | generalElectionVote == 12 | generalElectionVote == 13 | 
  generalElectionVote == 0 |  generalElectionVote == 9999)
w18f$generalElectionVote <- ifelse(w18f$generalElectionVote == 9999, 9999, 1)
w18f$generalElectionVote <- as.factor(w18f$generalElectionVote)
plotf <- ggplot(w18f, aes(discussPolDays, colour=generalElectionVote)) + geom_density(bw=1.7, size=0.8) + 
  labs(colour = "Vote Intention", title="Proportion of Voters' Political Sophistication, by Decided and Undecided", x="Days Discussing Politics Last Week", y="Proportion") + 
  scale_color_manual(values=c("blue", "black"), labels=c("Decided", "Don't Know")) + 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7))

#correct classes in w1819 and create w1819a 
w1819$generalElectionVote.x <- as.factor(w1819$generalElectionVote.x)
w1819$generalElectionVote.y <- as.factor(w1819$generalElectionVote.y)
w1819a <- w1819

#crosstable for reason for vote choice
library(gmodels)
CrossTable(w1819a$generalElectionVote.x, w1819a$reasonForVote)

#party liking in w18g - CON
w18g <- w18
w18g <- subset(w18g, w18g$generalElectionVote == 9999)
w18g$ptvCon <- ifelse(w18g$ptvCon == 9999, NA, w18g$ptvCon)
w18g$likeCon <- ifelse(w18g$likeCon == 9999, NA, w18g$likeCon)
plotg <- ggplot(w18g, aes(x=likeCon, y=ptvCon)) + geom_jitter(aes(colour="blue")) + 
  labs(colour = "Favourability", title="Favourability of Conservative Party and Likelihood to vote Conservative - Undecided Voters", x="Favourability", y="Likelihood") + 
  scale_colour_identity()  + geom_smooth(method="lm",se=T, aes(colour="red")) + 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10), limits=c(0,10)) + 
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10), limits=c(0,10)) 

#ditto but w18h for Labour
w18h <- w18
w18h <- subset(w18h, w18h$generalElectionVote == 9999)
w18h$ptvLab <- ifelse(w18gh$ptvLab == 9999, NA, w18h$ptvLab)
w18h$ptvLab <- ifelse(w18h$ptvLab == 9999, NA, w18h$ptvLab)
w18h$likeLab <- ifelse(w18h$likeLab == 9999, NA, w18h$likeLab)
ploth <- ggplot(w18h, aes(x=likeLab, y=ptvLab)) + geom_jitter(aes(colour="red")) + 
  labs(colour = "Favourability", title="Favourability of Labour Party and Likelihood to vote Labour - Undecided Voters", x="Favourability", y="Likelihood") + 
  scale_colour_identity()  + geom_smooth(method="lm",se=T, aes(colour="blue")) + 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10), limits=c(0,10)) + 
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10), limits=c(0,10)) 

#w18i for Lab and Con on their own ones
w18i <- w18
w18i <- subset(w18i, w18i$generalElectionVote == 1 | w18i$generalElectionVote == 2)
w18i$ownLike <- ifelse(w18i$generalElectionVote == 1, w18i$likeCon, w18i$likeLab)
w18i$ptvOwn <- ifelse(w18i$generalElectionVote == 1, w18i$ptvCon, w18i$ptvLab)
w18i$ownLike <- ifelse(w18i$ownLike == 9999, NA, w18i$ownLike)
w18i$ptvOwn <- ifelse(w18i$ptvOwn == 9999, NA, w18i$ptvOwn)

#w19a for analysing Con/Lab voters - EU Ref
w19a <- w19
w19a <- subset(w19a, w19a$p_eurefvote == 0 | w19a$p_eurefvote == 1)
w19a <- subset(w19a, w19a$generalElectionVote != 9999)
w19a$generalElectionVote <- ifelse(w19a$generalElectionVote == 1 | w19a$generalElectionVote == 2, w19a$generalElectionVote, 3)
CrossTable(w19a$generalElectionVote, w19a$p_eurefvote)

#w19b for age
w19b <- w19
w19b$generalElectionVote <- ifelse(w19b$generalElectionVote == 1 | w19b$generalElectionVote == 2, w19b$generalElectionVote, NA)
w19b$age <- ifelse(w19b$age == 119, NA, w19b$age)
tapply(w19b$age, w19b$generalElectionVote, mean, na.rm=T)
x1 <- subset(w19b$age, w19b$generalElectionVote == 1)
x2 <- subset(w19b$age, w19b$generalElectionVote == 2)
t.test(x1, x2)
plotj <- ggplot(w19b, aes(y=age, color=generalElectionVote)) + geom_boxplot() + 
  labs(title="Boxplots of Distribution of Ages of Conservative and Labour Voters", x="Party", y="Age", color="GE 2019 Vote") + 
  scale_x_continuous(breaks= NULL) + scale_color_manual(values=c("blue", "red"), labels=c("Con", "Lab"))

#w19c for class and education as one (UNUSED)
w19c <- w19
w19c$generalElectionVote <- ifelse(w19c$generalElectionVote == 1 | w19c$generalElectionVote == 2, w19c$generalElectionVote, NA)
w19c$generalElectionVote <- as.factor(w19c$generalElectionVote)
w19c$p_education <- ifelse(w19c$p_education >= 19, NA, w19c$p_education)
w19c$p_education <- ifelse(w19c$p_education >= 13, "HE", "NonHE")
w19c$subjClass <- ifelse(w19c$subjClass == 9999, NA, w19c$subjClass)
w19c$subjClass <- ifelse(w19c$subjClass == 1, "MC", "WC")
w19c <- unite(w19c, classed, subjClass, p_education, sep="&")
w19c$classed <- ifelse(w19c$classed== "MC&HE" | w19c$classed== "MC&NonHE" | w19c$classed== "WC&HE" | 
  w19c$classed== "WC&NonHE", w19c$classed, NA)
CrossTable(w19c$generalElectionVote, w19c$classed)
wtd.chi.sq(w19c$generalElectionVote, w19c$classed, weight=w19c$wt)

#w19d for class and education separately (used)
install.packages("weights")
library(weights)
w19d <- w19
w19d$generalElectionVote <- ifelse(w19d$generalElectionVote == 1 | w19d$generalElectionVote == 2, w19d$generalElectionVote, NA)
w19d$generalElectionVote <- as.factor(w19d$generalElectionVote)
w19d$p_education <- ifelse(w19d$p_education >= 19, NA, w19d$p_education)
w19d$p_education <- ifelse(w19d$p_education >= 13, "HE", "NonHE")
w19d$subjClass <- ifelse(w19d$subjClass == 9999, NA, w19d$subjClass)
w19d$subjClass <- ifelse(w19d$subjClass == 1, "MC", "WC")
CrossTable(w19d$generalElectionVote, w19d$subjClass)
wtd.chi.sq(w19d$generalElectionVote, w19d$subjClass, weight=w19d$wt)
CrossTable(w19d$generalElectionVote, w19d$p_education)
wtd.chi.sq(w19d$generalElectionVote, w19d$p_education, weight=w19d$wt)

#w1819c for undecideds who then voted Lab and Con - Parent dataset for others
w1819c <- w1819
w1819c <- subset(w1819c, w1819c$generalElectionVote.x == 9999)
w1819c <- subset(w1819c, w1819c$generalElectionVote.y == 1 | w1819c$generalElectionVote.y == 2)

#w1819d - Leave/Remain (formed from unaltered joined dataset as need non-Con/Lab voters included)
w1819d <- w1819
w1819d <- subset(w1819d, w1819d$generalElectionVote.x == 9999)
w1819d <- subset(w1819d, w1819d$generalElectionVote.y != 9999)
w1819d$generalElectionVote.y <- ifelse(w1819d$generalElectionVote.y > 2, 3, w1819d$generalElectionVote.y)
w1819d <- subset(w1819d, w1819d$p_eurefvote.x == 0 | w1819d$p_eurefvote.x == 1)
w1819d$p_eurefvote.x <- ifelse(w1819d$p_eurefvote.x == 0, "Remain", "Leave")
CrossTable(w1819d$generalElectionVote.y, w1819d$p_eurefvote.x)

#comparing on EU votes
array1 <- array(c(2606, 9736, 6108, 1635, 4947, 1922,
  162, 386, 277, 185, 311, 225),
  dim = c(2, 3, 2),
  dimnames = list(RefVote = c("Remain", "Leave"),
  GE19Vote = c("Con", "Lab", "Other"),
  Wave18 = c("Decided", "Undecided")))
mantelhaen.test(array1, alternative = "two.sided")

#w1819e - undecided ages
w1819e <- w1819c
tapply(w1819e$age.y, w1819e$generalElectionVote.y, mean, na.rm=T)
x1 <- subset(w1819e$age.y, w1819e$generalElectionVote.y == 1)
x2 <- subset(w1819e$age.y, w1819e$generalElectionVote.y == 2)
t.test(x1, x2)
w1819e$generalElectionVote.y <- as.factor(w1819e$generalElectionVote.y)
plotn <- ggplot(w1819e, aes(y=age.y, color=generalElectionVote.y)) + geom_boxplot() + 
  labs(title="Boxplots of Distribution of Ages of Late Deciding Conservative and Labour Voters", x="Party", y="Age", color="GE 2019 Vote") + 
  scale_x_continuous(breaks= NULL) + scale_color_manual(values=c("blue", "red"), labels=c("Con", "Lab"))

#w1819f - undecided class and education (separate, used)
w1819f <- w1819c
w1819f$generalElectionVote.y <- ifelse(w1819f$generalElectionVote.y == 1 | w1819f$generalElectionVote.y == 2, 
  w1819f$generalElectionVote.y, NA)
w1819f$generalElectionVote.y <- as.factor(w1819f$generalElectionVote.y)
w1819f$p_education.y <- ifelse(w1819f$p_education.y >= 19, NA, w1819f$p_education.y)
w1819f$p_education.y <- ifelse(w1819f$p_education.y >= 13, "HE", "NonHE")
w1819f$subjClass <- ifelse(w1819f$subjClass == 1 | w1819f$subjClass == 2, w1819f$subjClass, w1819f$subjClassSqueeze)
w1819f <- subset(w1819f, w1819f$subjClass != 9999)
w1819f$subjClass <- ifelse(w1819f$subjClass == 1, "MC", "WC")
CrossTable(w1819f$generalElectionVote.y, w1819f$subjClass)
wtd.chi.sq(w1819f$generalElectionVote.y, w1819f$subjClass, weight=w1819f$wt.y)
CrossTable(w1819f$generalElectionVote.y, w1819f$p_education.y)
wtd.chi.sq(w1819f$generalElectionVote.y, w1819f$p_education.y, weight=w1819f$wt.y)

#comparing education vote models
array2 <- array(c(197, 273, 223, 194,
                  5526, 6923, 5046, 3138),
                dim = c(2, 2, 2),
                dimnames = list(Education = c("HE", "NonHE"),
                                GE19Vote = c("Con", "Lab"),
                                Wave18 = c("Undecided", "Decided")))
mantelhaen.test(array2, alternative = "two.sided")

#preparing for tree
install.packages("partykit")
library(partykit)
w1819b <- w1819
w1819b <- subset(w1819b, generalElectionVote.x == 9999)
w1819b <- as_tibble(w1819b)
w1819b <- w1819b %>% select(generalElectionVote.y, electionInterest, 
  discussPolDays, partyId.y, likeJohnson.y, likeCorbyn.y, leftRight.y, subjClass, subjClassSqueeze, 
  age.y, p_education.y, p_eurefvote.y)
w1819b$subjClass <- ifelse(w1819b$subjClass == 1 | w1819b$subjClass == 2, w1819b$subjClass, w1819b$subjClassSqueeze)
w1819b <- w1819b %>% select(-c(subjClassSqueeze))
w1819b <- w1819b %>% mutate_if(is.double, as.numeric)
w1819ba <- subset(w1819b, !is.na(generalElectionVote.y))
w1819ba$generalElectionVote.y <- as.factor(w1819ba$generalElectionVote.y)
w1819ba$generalElectionVote.y <- ifelse(w1819ba$generalElectionVote.y == 9999, NA, w1819ba$generalElectionVote.y)
w1819ba$generalElectionVote.y <- ifelse(w1819ba$generalElectionVote.y == 1 | w1819ba$generalElectionVote.y == 2, w1819ba$generalElectionVote.y, 3)
w1819ba$p_eurefvote.y <- ifelse(w1819ba$p_eurefvote.y == 9999, NA, w1819ba$p_eurefvote.y)
w1819ba$electionInterest <- ifelse(w1819ba$electionInterest == 9999, NA, w1819ba$electionInterest)
w1819ba$discussPolDays <- ifelse(w1819ba$discussPolDays == 9999, NA, w1819ba$discussPolDays)
w1819ba$partyId.y <- ifelse(w1819ba$partyId.y == 9999, NA, w1819ba$partyId.y)
w1819ba$likeJohnson.y <- ifelse(w1819ba$likeJohnson.y == 9999, NA, w1819ba$likeJohnson.y)
w1819ba$likeCorbyn.y <- ifelse(w1819ba$likeCorbyn.y == 9999, NA, w1819ba$likeCorbyn.y)
w1819ba$subjClass <- ifelse(w1819ba$subjClass == 9999, NA, w1819ba$subjClass)
w1819ba$p_education.y <- ifelse(w1819ba$p_education.y >= 19, NA, w1819ba$p_education.y)
w1819ba$leftRight.y <- ifelse(w1819ba$leftRight.y == 9999, NA, w1819ba$leftRight.y)
w1819ba$p_eurefvote.y <- as.factor(w1819ba$p_eurefvote.y)
w1819ba$partyId.y <- ifelse(w1819ba$partyId.y == 1 | w1819ba$partyId.y == 2, w1819ba$partyId.y, 3)
w1819ba$partyId.y <- as.factor(w1819ba$partyId.y)
w1819ba$subjClass <- as.factor(w1819ba$subjClass)
w1819ba <- w1819ba %>% mutate_if(is.character, as.factor)
w1819ba$generalElectionVote.y <- as.factor(w1819ba$generalElectionVote.y)
w1819ba$p_education.y <- as.factor(w1819ba$p_education.y)
w1819ba <- rename(w1819ba, Corbyn = likeCorbyn.y)
w1819ba <- rename(w1819ba, Johnson = likeJohnson.y)
w1819ba <- rename(w1819ba, LeftRight = leftRight.y)
w1819ba <- rename(w1819ba, PartyID = partyId.y)
w1819ba <- rename(w1819ba, EURefVote = p_eurefvote.y)
w1819ba <- rename(w1819ba, Age = age.y)
w1819ba <- rename(w1819ba, GE19Interest = electionInterest)
w1819ba$EURefVote <- ifelse(w1819ba$EURefVote == 0, "R", "L")
w1819ba$EURefVote <- as.factor(w1819ba$EURefVote)
w1819ba <- subset(w1819ba, !is.na(generalElectionVote.y))

ta <- ctree(generalElectionVote.y ~ ., data = w1819ba, control = ctree_control(maxdepth=4))

#Other trees - no Corbyn or Johnson subset
w1819ba2 <- select(w1819ba, -c(Johnson, Corbyn))

#forest
fa <- cforest(generalElectionVote.y ~ ., data = w1819ba)
faa <- stablelearner::as.stabletree(fa)
summary(faa, original=F)

#maybe as part of preparing for tree?
w1819ba$subjClass <- ifelse(w1819ba$subjClass == 1, "MC", "WC")
w1819ba$p_education.y <- ifelse(w1819ba$p_education.y >= 13, "HE", "No HE")