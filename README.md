Exploring White Wine Dataset with R
========================================================
* [Project.Rmd](https://github.com/LiChangNY/White-Wine-Analysis/blob/master/project.Rmd): The RMD file containing the analysis (final plots and summary, and reflection)
* [Project.html](https://github.com/LiChangNY/White-Wine-Analysis/blob/master/project.html): The HTML file knitted from the RMD file using the knitr package
* [Reference](https://github.com/LiChangNY/White-Wine-Analysis/blob/master/reference.txt): A list of Web sites, books, forums, blog posts, github repositories, etc.

```{r echo=FALSE, message=FALSE, warning=FALSE, packages}
# Load all of the packages
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(GGally))
suppressMessages(library(reshape2))
suppressMessages(library(RColorBrewer))
suppressMessages(library(corrplot))
suppressMessages(library(Hmisc))
suppressMessages(library(MASS))
suppressMessages(library(randomForest))
suppressMessages(library(caret))
suppressMessages(library(psych))
```

As the final project for the Udacity Data Analysis with R, I decided to educate myself about the white wine. I downloaded dataset from [here](https://docs.google.com/document/d/1qEcwltBMlRYZT-l699-71TzInWfk4W9q5rTCSvDVMpc/pub) and loaded it in R. The research question is what physicochemical properties will affect the taste preference. 
```{r echo=FALSE, Load_the_Data}
# Load the Data
wine <- read.csv('wineQualityWhites.csv')
```

I - Univariate Plots Section
============================

Below I examined each variable in the dataset. I started with a basic plot and then revised the plot to be more clear and user-frienly. It helped me understand the distribution of each variable and decide if I need to tidy up something.

```{r echo=FALSE}
str(wine)
summary(wine)
```

###quality
In our dataset, the "quality" variable ranges between 3 and 9 with a median of 6, so there is neither very bad nor very excellent wine but mostly averge wines. Also, there are only 25 wines rated either 3 or 9. From bivariate section, I excluded these 25 cases from the analysis. Though quality is an integer, it makes more sense to be converted to an ordinal variable so I can compare the physicochemicals across different wine qualities. 
```{r echo=FALSE}
table(wine$quality)
wine$quality.cat <- factor(wine$quality)
ggplot( aes( x= quality.cat), data = wine) + 
  geom_histogram()
```

###fixed acidity
The basic histogram shows that fixed acidity has really few values less than 3 and a long tail after 10. So I limit the x axis range. Changing binwidth also shows more clearly that the majority of the fixed acidities fall between 5.5 and 8.5.
```{r echo=FALSE}
basic <- ggplot( aes( x= fixed.acidity), data = wine) + 
  geom_histogram() + 
  ggtitle('Basic plot')

revised <- ggplot( aes( x= fixed.acidity), data = wine) + 
  geom_histogram(binwidth = 0.05) + 
  ggtitle('Revised plot') + 
  scale_x_continuous( limit = c(3,10), breaks = seq(3, 10, by = 0.5) )

grid.arrange(basic, revised)
```

###volatile acidity
After adjusting bin width, I can see that most wines have an acetic acid between 0.15-0.4g/l, with a median value at 0.28g/l. I know that a high level of acetic acid will cause an unpleasant vigenar taste and therefore poor sensory rating. I can test it in the next section. 
```{r echo=FALSE}
basic <- ggplot( aes( x= volatile.acidity), data = wine) + 
  geom_histogram() + 
  ggtitle('Basic plot')

revised <- ggplot( aes( x= volatile.acidity), data = wine) + 
  geom_histogram(binwidth = 0.005) + 
  scale_x_continuous( limit= c(0.1, 0.6), breaks = seq(0.1, 0.55, by = 0.05) ) + 
  ggtitle('Revised plot')

grid.arrange(basic, revised)
```

###citric acid
The majority of citric acidity level fall between 0.15-0.5g/l with a spike at the level of 0.49g/l. In contrast to volatile acidity, citric acidity add freshness to the wine.
```{r echo=FALSE}
basic <- ggplot( aes( x= citric.acid), data = wine) + 
  geom_histogram() + 
  ggtitle('Basic plot')

revised <- ggplot( aes( x= citric.acid), data = wine) + 
  geom_histogram(binwidth = 0.005) + 
  scale_x_continuous( limit = c(0, 0.8), breaks = seq(0, 0.8, by = 0.05) ) + 
  ggtitle('Revised plot')

grid.arrange(basic, revised)
```

###residual sugar
Residual sugar has a wide range between 0.6-65.8g/l while the median is only 5.2g/l. This is because wine producers try to cater to varying consumers' preference of sweetness. Some people like me favor sweet wines, while others might prefer bone dry. 
```{r echo=FALSE}
basic <- ggplot( aes( x= residual.sugar), data = wine) + 
  geom_histogram() + 
  ggtitle('Basic plot')

revised <- ggplot( aes( x= residual.sugar), data = wine) + 
  geom_histogram(binwidth = 0.1) + 
  scale_x_continuous( limit = c(0, 20), breaks = seq(0, 20, by = 1) ) + 
  ggtitle('Revised plot')

grid.arrange(basic, revised)
```

###chlorides
Most wines has an amount of sodium chloride between 0.025-0.06g/l, with a median of 0.043g/l. The highest level in this dataset is 0.346g/l.
```{r echo=FALSE}
basic <- ggplot( aes( x= chlorides), data = wine) + 
  geom_histogram() + 
  ggtitle('Basic plot')

revised <- ggplot( aes( x= chlorides), data = wine) + 
  geom_histogram(binwidth = 0.001) + 
  scale_x_continuous( limit = c(0, 0.1), breaks = seq(0, 0.1, by = 0.01) ) + 
  ggtitle('Revised plot')

grid.arrange(basic, revised)
```

###free sulfur dioxide
The median value of free sulfur dioxide is 34 mg/l and it has a wide range from 2 to 289 mg/l with the majority of the value falling between 10-55 mg/l. Since free sulfur dioxide becomes noticeable at 50 mg/l, I assume it will affect the taste. 
```{r echo=FALSE}
basic <- ggplot( aes( x= free.sulfur.dioxide), data = wine) + 
  geom_histogram() + 
  ggtitle('Basic plot')

revised <- ggplot( aes( x= free.sulfur.dioxide), data = wine) + 
  geom_histogram(binwidth = 0.5) + 
  scale_x_continuous( limit = c(0, 80), breaks = seq(0, 100, by = 5) ) + 
  ggtitle('Revised plot')

grid.arrange(basic, revised)

wine$free.sulfur.dioxide.cat <- ifelse(wine$free.sulfur.dioxide <= 50, '<= 50mg/l, not noticeable', '> 50mg/l, noticeable')
wine$free.sulfur.dioxide.cat <- factor(wine$free.sulfur.dioxide.cat)
```

###total sulfur dioxide
Similar to free sulfur dioxide, total sulfur dioxide also has a wide range from 9 to 440 mg/l with a median value at 134 mg/l.

```{r echo=FALSE}
basic <- ggplot( aes( x= total.sulfur.dioxide), data = wine) + 
  geom_histogram() + 
  ggtitle('Basic plot')

revised <- ggplot( aes( x= total.sulfur.dioxide), data = wine) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous( limit = c(10, 250), breaks = seq(10, 250, by = 10) ) + 
  ggtitle('Revised plot')
grid.arrange(basic, revised)
```

###density
Density has a small range between 0.99 to 1.04g/cc. It mostly depends on the percent of alcohol and sugar in the wine.
```{r echo=FALSE}
basic <- ggplot( aes( x= density), data = wine) + 
  geom_histogram() + 
  ggtitle('Basic plot')

revised <- ggplot( aes( x= density), data = wine) + 
  geom_histogram(binwidth = 0.00005) + 
  scale_x_continuous( limit = c(0.99, 1), breaks = seq(0.99, 1, by = 0.001) ) + 
  ggtitle('Revised plot')
grid.arrange(basic, revised)
```

###PH
PH has a small range between 2.7 to 3.8 but obviously highly acetic!
```{r echo=FALSE}
basic <- ggplot( aes( x= pH), data = wine) + 
  geom_histogram() + 
  ggtitle('Basic plot')

revised <- ggplot( aes( x= pH), data = wine) + 
  geom_histogram(binwidth = 0.005) + 
  scale_x_continuous( limit = c(2.9, 3.5), breaks = seq(3, 3.5, by = 0.05) ) + 
  ggtitle('Revised plot')
grid.arrange(basic, revised)
```

###alcohol
An appropriate level of alcohol enhances the flavor but a high level of alcohol will cause a negative burning sensation. But our white wine dataset doesn't appear to have very high alcohol level. The median is 10.4% and the majority of values fall between 9% to 13%. 

```{r echo=FALSE}
basic <- ggplot( aes( x= alcohol), data = wine) + 
  geom_histogram() + 
  ggtitle('Basic plot')

revised <- ggplot( aes( x= alcohol), data = wine) + 
  geom_histogram(binwidth = 0.05) + 
  scale_x_continuous( limit = c(8.5, 13), breaks = seq(8.5, 13, by = 0.5) ) + 
  ggtitle('Revised plot')
grid.arrange(basic, revised)
```

Univariate Analysis
-------------------

The dataset has a total of 4898 observations and 12 variables plus one indicating the wine ID ('X'). Almost all variables except wine quality are numeric. 

1. Majority of white wine in this dataset is rated between 4-8 on a scale of 10 with a median of 6. We don't have very bad or very excellent wines. Quality is the main feature I am interested in. I wonder what physicochemical elements will influence the taste preference. However, there are only 20 wines rated 3 and 5 wines rated 9. I think it's better to drop these few cases before the bivariate and multi-variate analysis just so that we can focus more on the bulk of the data. Other than that, the dataset is relatively clean.
    
2. Residual sugar appear to have a wide range between 0.6-65.8g/l, supposedly to accomdate customers' varying palettes for sweetness.  
    
3. Free sulfur dioxide ranges between 2 to 289mg/l but be aware that the smell becomes noticeable at the level above 50 mg/l. 
    
4. White wine is highly acetic with pH level ranging from 2.7 to 3.8.
    
5. Below I used recursive feature elmination(RFE) to explore what the important features are. RFE repeats the process of choosing the best peforming feature(s) and refit the model with the remaining features to determine the importance of each feature. It is more often used in model building process but I think it would be valuable to display the importance of each feature. Here is a [good tutorial](http://topepo.github.io/caret/rfe.html). The model I used for the feature selection is [random forest](http://www.listendata.com/2014/11/random-forest-with-r.html), a method aiming to overcome the overfitting problem of [decision trees](http://en.wikipedia.org/wiki/Decision_tree_learning). The graph below shows that the R-squared value keeps rising with more variables added but degree of increase slows down after three factors: volatile acidity, alcohol, and free sulfur dioxide. Alcohol is no doubt a very important feature as no one would prefer bland wines. Both volatile acidity and free sulfur dioxide have such "volatile" properties that an exessive amount will cause unpleasant smell or taste. So I decided to use volatile acidity, alcohol and free sulfur dioxide as the main features of my interest for further investigation.
    
```{r echo=FALSE}
set.seed(1)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# run the RFE algorithm
results <- rfe(wine[,2:12], wine[,13], sizes=c(2:12), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot1 <- plot(results, type=c("g", "o"))
plot2 <- plot(results, type = c("g", "o"), metric = "Rsquared")
print(plot1, split=c(1,1,1,2), more=TRUE)
print(plot2, split=c(1,2,1,2))
```


6. Quality is converted from integer to factor. I also used 50mg/l to create a new variable "free.sulfur.dioxide.cat" as "noticeable" (free.sulfur.dioxide > 50) and "not noticeable" (free.sulfur.dioxide <= 50).
    
7. Looking through the summary data above, residual.sugar, free.sulfur.dioxide, and total.sulfur.dioxide appear to have wide ranges. In particular, wines with residual sugar above 45 g/l are considered very sweet. However, I don't think the wide distribution should be adjusted as some wines may display very far-stretching characteristics. 




II - Bivariate Plots and Analysis
=================================

Below I used correlation matrix to visualize the pair-wise correlation between two variables. Quality and alcohol appear to have moderate positive correlation (0.44). In very contrast to the feature importance generated under RFE in Section I, volatile acidity and quality exhibit very weak negative relationship (corr = -0.19). Between free sulfur dioxide and quality, there is barely any correlation (corr = 0.02). It's possible that the relationship of two variables is masked in the presence of other factors. 

```{r echo=FALSE}
wine <- subset(wine, quality > 3 & quality < 9)
ctab <- cor(wine[2:13])
col <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow",
                          '#9AD09A',"cyan", "#007FFF","#00007F"))
corrplot.mixed(ctab, col = col(10), cl.cex = 0.6,
               tl.col = "black", tl.srt = 180,tl.cex = 0.6) 
```



Main Features of Interest
-------------------------

Interestingly, the relationship between alcohol and rating doesn't seems to be linearly positive. This leads me to think that there must be other factors that cause this parabola curve between alcohol and quanlity rating. Anyway, for good wines (quality rating above 5), the higher the alcohol level is, the better the rating is. For example, the median alcohol level among wines that are rated 8 is 12%, a quarter more than the 9.5% among wines that are rated 5.  

```{r echo=FALSE}
p <- ggplot(wine, aes(quality.cat, alcohol))
p +  geom_boxplot(alpha = 0.2, col = 'navy blue')+ geom_jitter(alpha = 0.1)
describeBy(wine$alcohol,wine$quality.cat)
```


Alcohol level reinforces acidity but the chart below shows a non-linear relationship between the two. This explains why in Section I we saw a small correlation at -0.19. I then divided alcohol level into five groups with equal intervals. The median of volatile acidity among wines with a high level of alcohol (13-14.2%) is 0.35, which is significantly higer than alcohol group between 10.5-11.7% (median = 0.24).
```{r echo=FALSE}
ggplot( aes( x= alcohol, y = volatile.acidity), data = wine) + 
  geom_jitter(alpha = 0.5, size = 0.75) + 
  geom_smooth()

wine$alcohol.level.5 <- cut(wine$alcohol, 5)
summary(wine$alcohol.level.5)
describeBy(wine$volatile.acidity,wine$alcohol.level.5)
```


Free sulfur dioxide decreases when alcohol level rises. For example, among wines with alcohol level at 7.99-9.24%, the median value of free sulfur dioxide is 42 mg/l, 50% higher than the median free sulfur dioxide (28 mg/l) found in the alcohol group between 13-14.2%.
```{r echo=FALSE}
ggplot( aes( x= alcohol, y = free.sulfur.dioxide), data = wine) + 
  geom_jitter(alpha = 0.5, size = 1) + 
  coord_cartesian(ylim = c(0, 100)) + 
  geom_smooth()

describeBy(wine$free.sulfur.dioxide,wine$alcohol.level.5)
```

Although the feature selection placed free sulfur dioxide and volatile acidity among the top three important features, charting against quality rating doesn't show strong relatinship, which could be masked by other factors.
```{r echo=FALSE}
p <- ggplot(wine, aes(quality.cat, volatile.acidity)) + 
     geom_jitter(alpha = 0.1) + 
     geom_boxplot(alpha = 0.1, color = 'orange')
q <- ggplot(wine, aes(quality.cat, free.sulfur.dioxide)) + 
     geom_jitter(alpha = 0.1) + 
     scale_y_continuous( limit = c(0, 150)) + 
     geom_boxplot(alpha = 0.1, color = 'purple')

grid.arrange(p, q, ncol = 2)
```

Other Interesting Features
--------------------------


Density depends on the percentage of alcohol in the water. The plot below clearly shows that the density decreases when the amount of alcohol increases. Additionally, density increases with sugar content. Residual sugar and alcohol explains more than 90% of the variance in density. So density shouldn't be included in the presence of alcohol or sugar in any modelling, just to minimize multicullinearity.

```{r echo=FALSE}
ggplot( aes( x= alcohol, y = density), data = wine) + 
  geom_jitter(alpha = 0.5, size = 0.75) + 
  ggtitle('Alcohol vs. Density') + 
  coord_cartesian(ylim = c(0.98, 1.01)) + 
  geom_smooth(method = lm)

ggplot( aes( x= residual.sugar, y = density), data = wine) + 
  geom_jitter(alpha = 0.5, size = 0.75) + 
  coord_cartesian(xlim = c(0, 25), ylim = c(0.98, 1.01)) + 
  geom_smooth()

fit <- lm(density ~ alcohol + residual.sugar, data=wine)
summary(fit)
```


It's said that at free sulfur dioxide over 50mg/l, you can smell the odor of sulfur in wine. There is an infliction point when free sulfur dioxide reaches 50mg/l. The correlation between the two is 0.51 among wines with less than 50 mg/l of free sulfur dioxide but reduces to 0.19 among wines with higher free sulfur dioxide. 


```{r echo=FALSE}
ggplot( aes( x= free.sulfur.dioxide, y = total.sulfur.dioxide), data = wine) + 
  geom_jitter(alpha= 0.3, size = 0.75) + 
  coord_cartesian(xlim = c(0, 100)) + 
  geom_smooth()

cor(wine[wine$free.sulfur.dioxide <= 50,]$free.sulfur.dioxide, wine[wine$free.sulfur.dioxide <= 50,]$total.sulfur.dioxide )
cor(wine[wine$free.sulfur.dioxide > 50,]$free.sulfur.dioxide, wine[wine$free.sulfur.dioxide > 50,]$total.sulfur.dioxide )
```


III - Multivariate Plots and Analysis
=====================================


Within the same quality group, a wine without noticeable sulfur smell is likely to have higher alcohol level. For example, among wines that are rated 6 and don't have noticeable sulfur smell, the median alcohol by volume is 10.6% as compared to 9.6% among wines with the same rating but noticeable sulfur smell. In other words, keeping alcohol constant, you will likely get a better rated wine if the sulfur level is unnoticeable. Also, for people who are concerned with the health issue caused by sulfur, choosing a wine with higher concentration of alcohol would likely reduce the intake of sulfur (well, if alcohol is less of concern to them). 

```{r echo=FALSE}
ggplot(wine, aes(quality.cat, alcohol, fill = free.sulfur.dioxide.cat)) +
    geom_jitter(alpha = 0.1) + 
    geom_boxplot() 

describeBy(wine$alcohol, list(wine$free.sulfur.dioxide.cat, wine$quality.cat))
```


When I break down quality by alcohol level and volatile acidity, for alcohol group between 7.99-10.1%, the negative relationship between volatile acidity and quality becomes the strongest. For example, among 7.99-10.1% alcohol categories, the median value of volatile acidity decreases from 0.34 g/l for less desirable wines (quality = 4) to 0.19 g/l for highly rated ones (quality = 8); the former group also has a higher variation of volatile acidity (sd = 0.31) than the latter one (sd = 0.03).


```{r echo=FALSE}
wine$alcohol.level.3 <- cut(wine$alcohol, 3)
summary(wine$alcohol.level.3)
ggplot( aes( x= quality.cat, y = volatile.acidity), data = wine) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 0.9)) +
  coord_flip() + 
  facet_wrap(~alcohol.level.3)

describeBy(wine$volatile.acidity, list(wine$alcohol.level.3, wine$quality.cat))
```


If 50 mg/l makes the unpleasant smell noticeable by wines, the negative relationship between wine quality and total sulfur dioxide should be stronger among wines with free sulfur dioxide above 50 mg/l (negative correlation at -0.26) than those below 50 mg/l (negative correlation at -0.13), as the boxplot below shows. 


```{r echo=FALSE}
ggplot(wine, aes( x=quality.cat, y=total.sulfur.dioxide)) + 
  geom_boxplot() +
  facet_grid(~free.sulfur.dioxide.cat) + 
  geom_smooth(method = "lm", se=FALSE, color="red", aes(group=1))

cor(wine[wine$free.sulfur.dioxide <= 50, ]$quality, wine[wine$free.sulfur.dioxide <= 50, ]$total.sulfur.dioxide )
cor(wine[wine$free.sulfur.dioxide > 50, ]$quality, wine[wine$free.sulfur.dioxide > 50, ]$total.sulfur.dioxide )
```






IV - Final Plots and Summary
=============================


### Plot One - Quality vs. Alcohol
```{r echo=FALSE, Plot_Three}

p1 <- ggplot(wine, aes( alcohol)) +
      geom_density(alpha = 0.5, fill = 'red') +  
      xlab('Alcohol Level (% by Volume)') 

p2 <- ggplot(wine, aes( quality.cat)) + 
      geom_bar(alpha = 0.8, fill = "#56B4E9") +  
      xlab('Wine Quality') 

p3 <- ggplot(wine, aes( x=quality.cat, y=alcohol, fill = quality.cat)) + 
  scale_fill_brewer() + 
  geom_boxplot() +
  geom_smooth(method = "lm", se=FALSE, color="red", aes(group=1)) + 
  theme(legend.position = 'top') + 
  ggtitle('Wine Quality vs. Alcohol Level') + 
  xlab('Wine Quality') + 
  ylab('Alcohol Level (% by Volume)') 

grid.arrange(p3, arrangeGrob(p1, p2, ncol = 1), ncol=2, widths=c(2,1))

describeBy(wine$alcohol,wine$quality.cat)
```

### Description One
The reason why I chose this graph as one of the final plots is because among all the features, alcohol level shows the strongest correlation (corr = 0.44) with wine rating. Alcohol level has a relatively small range from 8% to 14.2% and a median value at 10.4%. The majority of our wine ratings fall between 5-7. Except for rating 4 category probably due to relative small sample size, a better-rated wine has a higher alcohol level (the left chart). For example, the median alcohol level among wines that are rated 8 on a scale of 0-10 is 12%, much higher than 9.5% among wines rated 5.




### Plot Two - Quality vs. Alcohol and Free Sulfur Dioxide
```{r echo=FALSE, Plot_One}
#placeholder plot - prints nothing at all
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
     theme(                              
       plot.background = element_blank(), 
       panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(), 
       panel.border = element_blank(), 
       panel.background = element_blank(),
       axis.title.x = element_blank(),
       axis.title.y = element_blank(),
       axis.text.x = element_blank(),
       axis.text.y = element_blank(),
       axis.ticks = element_blank()
     )
#boxplot
boxplot<- ggplot(wine,aes(quality.cat, alcohol)) + 
  geom_boxplot(aes(fill= free.sulfur.dioxide.cat), alpha = 0.5) + 
  theme(legend.position=c(1,1),legend.justification=c(1,1)) + 
  xlab('Wine Quality') + 
  ylab('Alcohol (% by volume)')

#marginal density of quality plot on top
#used quality instead of quality.cat
plot_top <- ggplot(wine, aes(quality, fill= free.sulfur.dioxide.cat)) + 
  geom_density(alpha=.5) + 
  theme(legend.position = "none") + 
  xlab('Wine Quality')

#marginal density of  alcohol plot on the right
plot_right <- ggplot(wine, aes(alcohol, fill= free.sulfur.dioxide.cat)) + 
  geom_density(alpha=.5) + 
  coord_flip() +  
  theme(legend.position = "none") + 
  xlab('Alcohol (% by volume)')

#arrange the plots together, with appropriate height and width for each row and column
grid.arrange(plot_top, empty, boxplot, plot_right, 
             ncol=2, nrow=2,
             widths=c(4, 1), heights=c(1, 4), 
             main=('Wine Quality vs. Alcohol and Free Sulfur Dioxide Level'))

describeBy(wine$alcohol, list(wine$free.sulfur.dioxide.cat, wine$quality.cat))
```


### Description Two
Free sulfur dioxide and alcohol are among the top three most important features (See Section 1 recursive feature elimination). Something about the odor makes things interesting. Following this [great visualization example](http://www.r-bloggers.com/ggplot2-cheatsheet-for-visualizing-distributions/), I was able to combine three graphs to one in a different way from the first plot. Charts on the top and right side show the distribution of quality ratings and alcohol level, respectively, by whether the free sulfur dioxide is noticeable or not. As you can see, most wines cluster between 5-7 (okay wines) but wines with no noticeable free sulfur dioxide have slightly higher ratings between 6-7 and those with free sulfur dioxide above 50 mg/l have lower alcohol concentration. This pattern holds true no matter what quality rating is (middle graph). Holding wine quality consistent, for example, within wines that are rated 6, the median alcohol level (10.6%) is higher among those with no noticeable smell of sulfur than those otherwise (9.6%). For people who don't like the sulfur smell and are worried about its health concern, one rule of thumb is perhaps to choose wines with higher alcohol level. 



### Plot Three -  Free Sulfur Dioxide & Volatile Acidity vs. Alcohol
```{r echo=FALSE, Plot_Two}
p1 <- ggplot(wine, aes( x = alcohol.level.5, y = free.sulfur.dioxide)) + 
      geom_point(alpha = 0.05) +
      coord_cartesian( ylim = c(0,75)) + 
      geom_boxplot(alpha = 0.3, color = 'orange') +
      ylab('Free Sulfur Dioxide(mg/l)') + 
      xlab('Alcohol Groups (% by volume)')

p2 <- ggplot(wine, aes( x = alcohol.level.5, y = volatile.acidity)) + 
      geom_point(alpha = 0.05) +
      coord_cartesian( ylim = c(0,0.6)) + 
      geom_boxplot(alpha = 0.3, , color = 'purple') + 
      ylab('Volatile Acidity(g/l)') + 
      xlab('Alcohol Groups (% by volume)')

grid.arrange(p1, p2, ncol = 1, main = ("Free Sulfur Dioxide and Volatile Acidity vs. Alcohol Groups"))

describeBy(wine$free.sulfur.dioxide,wine$alcohol.level.5)
describeBy(wine$volatile.acidity,wine$alcohol.level.5)

```


### Description Three
Alcohol, free sulfur dioxide and volatile acidity became main features of interest from Section I and I think it's important to understand how they interact with each other. Alcohol level appears to be an interesting element because it is not only correlated with wine ratings but also in some way it can [enhance acidity and mask unpleasant odor](http://www.bjcp.org/mead/Factors_Wine_Eval.pdf). The combined two charts below plot alcohol against free sulfur dioxide and volatile acidity. The higher the alcohol level is, the less the free sulfur dioxide will be. For example, the median free sulfur dioxide amount among 13-14.2% alcohol group is only 28 mg/l, much less than 42 mg/l among 7.99-9.24% alcohol group. The relatinship between acidity and alcohol becomes more clear among higher alcohol groups. For instance, the median volatile acidity amount among 13-14.2% alcohol group is 0.35 g/l as compared to 0.24 g/l among 10.5-11.7% alcohol group. 




V - Reflection
===============

My learnings after exploring the white wine dataset:

1. Alcohol is an important factor for the wine taste. At the same times, it interacts with other physcochemicals. For example, it can suppress the unpleasant odor and enhance acidity. 

2. Free sulfur dioxide is really critical. At the level of 50m g/l, it becomes excessive and makes unpleasant smell noticeable that hurts the taste bud. 

This white wine dataset is the most tidy one I've ever used for Udacity projects. However, I was frustrated in the beginning because except alcohol, almost all other input variables don't have a strong relationship with wine quality. Reading correlation matrix is not enough. When conditioning on other relevant variables, the relationships between the physicochemical properties and quality became clear. Also, all input variables are continous variables which limited the type of graphs I could make. One solution I made was to recode to categorical variables. 

The other problem I had is my knowledge about the physicochemicals and how they interacted were limited before starting this project. I had to resort to additional readings to brush up my wine knowledge.  

This dataset is pretty limited with 13 input variables (technically 12 can be used for analysis because one of them is ID variable), it will be great if other variables such as grape type and wine age can be included for further investigation. 

