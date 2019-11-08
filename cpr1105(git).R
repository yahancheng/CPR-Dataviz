# clean environment and import data
rm(list = ls())
gdp <- read.csv("2018gdp.csv", stringsAsFactors = F, strip.white = T)
personal <- read.csv("2018personal.csv", check.names = F, stringsAsFactors = F)
score <- read.csv("greenscore.csv", check.names = F, stringsAsFactors = F)

# clean data
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
gdp$X <- trim(gdp$X)
d <- data.frame(state = gdp$X[1:51], gdp = gdp$gdp[1:51],
                gdp_growth = gdp$gdp_growth[1:51], politic = gdp$red[1:51])
d$personal_consumption <- NA
d$nondurable <- NA
d$score <- NA

personal$Description <- trim(personal$Description)
for(i in d$state){
  p_index <- which(personal$GeoName == i & personal$Description == "Personal consumption expenditures")
  n_index <- which(personal$GeoName == i & personal$Description == "Nondurable goods")
  s_index <- which(score$state == i)
  d_index <- which(d$state == i)
  if (length(p_index) == 0 | length(d_index) == 0 | length(s_index) == 0){next}
  d$personal_consumption[d_index] <- personal$`2018`[p_index]
  d$nondurable[d_index] <- personal$`2018`[n_index]
  d$score[d_index] <- score$score[s_index]
}

# standardize
colnames(d)
for(col in c(2, 3, 5, 6)){
  d[col] <- scale(d[col])
}
d$z_score <- scale(d$score)

# lm
lm_result <- lm(z_score ~ gdp + gdp_growth + politic + 
                  personal_consumption + nondurable, data = d)

summary(lm_result)

# subgroup by color
blue.d <- subset(d, politic == 0)
red.d <- subset(d, politic == 1)

lm.blue <- lm(z_score ~ gdp + gdp_growth + 
                personal_consumption + nondurable, data = d)
lm.red <- lm(z_score ~ gdp + gdp_growth + 
                personal_consumption + nondurable, data = d)
summary(lm.blue)
summary(lm.red)

# plot
library(ggplot2)
# box plot: score ~ politic
ggplot(aes(x = factor(politic), y = score), data = d) +
  geom_boxplot(fill = c("#0f52ba", "#e0115f"), color = "black") +
  theme(panel.grid = element_blank()) +
  scale_x_discrete("Political ideology", labels = c("0" = "Democrats", "1" = "Republican")) +
  scale_y_continuous("Green score")

library(usmap)
# score map
plot_usmap(data = d, values = "score", color = "black") + 
  scale_fill_continuous(name = "Green score", label = scales::comma,
                        low = "white", high = "#006d5b") + 
  theme(legend.position = "right")

