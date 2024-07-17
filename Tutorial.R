print("sup")

#vectors
x = 2
x <- 3
print(x)
print(class(x))
            
V=TRUE
print(class(V))

apple=c('green','red','yellow',3, V, x)

print(apple)
print(class(apple))

#lists
list1=list(c(2,4,6,7,8,9), 21.3, cos, sin, V, TRUE)
print(list1)

#matrices
M = matrix(c(2,5,3,6,7,9), nrow=2, ncol=3, byrow=TRUE)
print(M)


#arrays
a <- array(c('green', 'yellow'), dim=c(3,3,1))
print(a)

#factors
apple_colors <- c('green', 'yellow', 'red', 'red', 'green', 'yellow', 'red')
fact_apple <- factor(apple_colors)
print(apple_colors)
print(fact_apple)
print(nlevels(fact_apple))

#data frames
BMI <- data.frame(
  gender=factor(c("Male", "Male", "Female")),
  height=c(154, 185.3, 165),
  age=c(42, 21, 20)
)
print(BMI)

#tidyverse
install.packages("tidyverse")
library(tidyverse)

#dplyr
#Use the conflicted package to force all conflicts to become errors
mpg
mpg%>%
dplyr::filter(model =='a4') %>% #return all rows that satisfy conditions
dplyr::arrange(displ, cyl) %>%   #reorder the rows in your columns
dplyr::mutate(old=year<2000, full_wheel_drive=trans=='auto(l5') %>% #add new info preserve old
dplyr::select(-drv)#keep or remove columns, can be individual or everything minus some columns
# dplyr::transmute()#add new info drop old
mpg%>%
dplyr::count(model, sort=TRUE)#create aggregate stats
print(mpg, n=37)#print n rows 

#ggplot
ggplot(data=mpg)+
geom_point(mapping = aes(x=displ, y=hwy))  #scatter plot mapping the aesthetic on the x and y-axis

ggplot(data=mpg)+ #assigns color to each type of car or class of car
  geom_point(mapping = aes(x=displ, y=hwy, color=class))

ggplot(data=mpg)+ #faceting by class, each class gets its own chart
  geom_point(mapping = aes(x=displ, y=hwy))+
  facet_wrap(~class, nrow=2)

ggplot(data=mpg)+ #smooth for curved regression line (line of best fit?) 
  geom_point(mapping = aes(x=displ, y=hwy))+
  geom_smooth(mapping= aes(x=displ, y=hwy))

ggplot(data=mpg, mapping=aes(x=displ, y=hwy))+
  geom_point(mapping=aes(color=class))+
  geom_smooth()

ggplot(data=diamonds)+#bar graphs
  geom_bar(mapping=aes(x=cut))
print(diamonds)  

#exploratory data analysis
smaller <- diamonds %>% #smaller receives the value of the output of diamonds being filtered where carat is < 3
  filter(carat>3)#smaller is a subset of diamonds where only the rows where carat < 3 are assigned
smaller
ggplot(data=smaller, mapping=aes(x=carat))+
  geom_histogram(binwidth=0.1)#distribution where the bin increments by 0.1 and puts whatever it is into different bins

ggplot(data=mpg)+#shows the median(bold line), outliers(dots), inner quartile range(top of box to bottom), and data dregs(vertical lines)
  geom_boxplot(mapping=aes(x=reorder(class, hwy, FUN=median), y=hwy))

diamonds %>% 
  dplyr::count(color, cut) %>% 
  ggplot(mapping=aes(x=color, y=cut))+
  geom_tile(mapping=aes(fill=n))#basically a heat map

#importing data
library(readr)
Rexcel_Sheet1_ <- read_csv("Documents/LearnR/Rexcel(Sheet1).csv")
View(Rexcel_Sheet1_)
Rexcel_Sheet1_

#tribbles from tidyverse - a row-wise tibble (a sort of data frame)
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes", NA, 10,
  "no", 20, 12
)
preg

#some very useful packages



library(datapasta)#datapasta package is useful for tibbles which you can just copy paste, no need to import
      #copy data -> Addins-> paste as tribble
var <- tibble::tribble(
  ~`0`,        ~`1`,        ~`0`, ~`321`, ~`1.3621E+168`, ~`387.1433385`, ~`1.3621E+168`, ~`387.1433385`,
    1L,         150, 5.010635294,   320L,    7.6709e+167,    386.5691432,    1.1506e+170,    391.5797785,
    2L,       11325,  9.33476795,   319L,     4.314e+167,    385.9935838,    4.8856e+171,    395.3283518,
    3L,      573800, 13.26003618,   318L,    2.4228e+167,    385.4166535,    1.3902e+173,    398.6766897,
    4L,    21947850, 16.90417974,   317L,    1.3588e+167,    384.8383456,    2.9824e+174,    401.7425253,
    5L,   675993780, 20.33169443,   316L,    7.6105e+166,    384.2586533,    5.1446e+175,    404.5903477,
    6L, 17463172650, 23.58336008,   315L,    4.2565e+166,    383.6775698,    7.4332e+176,    407.2609298,
    7L, 3.89179e+11, 26.68730594,   314L,    2.3773e+166,    383.0950881,    9.2519e+177,    409.7823941,
    8L, 7.63764e+12,  29.6641102,   313L,    1.3259e+166,    382.5112015,    1.0127e+179,    412.1753117,
    9L, 1.34083e+14, 32.52948066,   312L,    7.3843e+165,    381.9259028,    9.9012e+179,    414.4553835,
   10L, 2.13192e+15, 35.29579977,   311L,    4.1068e+165,    381.3391851,    8.7554e+180,    416.6349849,
   11L, 3.10098e+16, 37.97307831,   310L,    2.2807e+165,    380.7510412,    7.0725e+181,    418.7241196,
   12L, 4.16048e+17, 40.56957602,   309L,    1.2648e+165,    380.1614641,    5.2622e+182,    420.7310401,
   13L, 5.18459e+18,   43.092223,   308L,    7.0041e+164,    379.5704464,    3.6313e+183,    422.6626694,
   14L, 6.03635e+19, 45.54691587,   307L,     3.873e+164,    378.9779809,    2.3379e+184,    424.5248968,
   15L, 6.59974e+20,  47.9387321,   306L,    2.1385e+164,    378.3840604,    1.4114e+185,    426.3227925,
   16L, 6.80598e+21, 50.27208885,   305L,    1.1791e+164,    377.7886774,    8.0247e+185,    428.0607662,
   17L, 6.64584e+22,  52.5508633,   304L,    6.4912e+163,    377.1918245,     4.314e+186,    429.7426878,
   18L, 6.16586e+23, 54.77848535,   303L,    3.5684e+163,    376.5934942,    2.2002e+187,    431.3719795,
   19L, 5.45192e+24, 56.95801035,   302L,    1.9588e+163,    375.9936789,    1.0679e+188,    432.9516893,
   20L, 4.60687e+25, 59.09217679,   301L,    1.0736e+163,    375.3923711,    4.9459e+188,    434.4845479
  )
var

library(esquisse)#french package useful for analyzing code in retrospect

library(ggplot2)

ggplot(BMI) +
 aes(x = age, y = height) +
 geom_point(colour = "#112446") +
 theme_minimal()



library(rayshader)
# Using ggplot2 to create a barplot and then converting it into a rayshader plot
library(dplyr)
install.packages("rgl")
# Create a data frame
data <- expand.grid(x = 1:5, y = 1:5)
data$z <- as.vector(data_matrix)

# Create a 2D bar plot with ggplot2
gg <- ggplot(data, aes(x, y, fill = z)) + 
  geom_tile() + 
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "3D Bar Graph", x = "X-axis", y = "Y-axis", fill = "Height")

# Convert ggplot2 plot to rayshader
plot_gg(gg, multicore = TRUE, width = 5, height = 5, scale = 250, windowsize = c(800, 800),
        zoom = 0.75, phi = 45, theta = 135)

