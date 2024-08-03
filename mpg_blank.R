         #########################################
         ###  Data Visualization with RStudio  ###
         ###  Day 5 - February 1, 2022         ###
         #########################################

   # Learning goals:
   # 1) the grammar of graphics using ggplot2
   # 2) scatterplots that encode multiple variables
   # 3) faceting: stratifying a basic plot by a third variable
         
# by now you should have installed the tidyverse
# remember that we have to run the following every 
# time we start a new R session to access tidyverse functions:
   
         library (tidyverse)
      
# ggplot functions are included in the tidyverse package
# but you should go ahead and install the ggplot2 package 
# on its own if you've not already with:
install.packages('ggplot2')
library(ggplot2)

# today we will work with the built-in data set called mpg
# fuel economy data for 38 popular car models
# load the mpg data set (included with tidyverse)
data(mpg)
head (mpg)
View (mpg)
# first few lines of the data set
# every row is a car, every column is a feature of that car

         
# let's try R's basic plotting command: plot(dataset$x, dataset$y)

    # Pros: simple syntax
    # Cons: not that pretty, and very hard to do complex things
plot (mpg$displ,mpg$hwy)

# Let's work with ggplot2 instead
    # Cons: commands are less intuitive at first
    # Pros: much easier to make sophisticated, beautiful plots

### Basic structure of all statistical graphics:
   # A graphic is a mapping of data variables to
   # aesthetic attributes of geometric objects.
   # all ggplot2 graphs have these three elements:
         #   (1) a data set (data)
         #   (2) a geometry (geom)
         #   (3) an aesthetic mapping (aes)

### Below: creating a ggplot with the "grammar of graphics".
  # The first layer tells ggplot where to look for variables (data)
  # The second layer makes an "aesthetic mapping" (aes) from:
      #   - data variable displ to aesthetic property x (horizontal location)
      #   - data variable hwy to aesthetic property y (vertical location)

# It then displays the data in a scatter plot (geom_point)

         ggplot(mpg) + 
           geom_point(aes(x = displ, y = hwy))

         ggplot(colleges) + 
           geom_point(aes(x =AdmissionYield , y = PercentOnFinancialAid))

### AESTHETIC MAPPINGS can get more complicated, with >2 variables

# here we vary map class to point color

         ggplot(mpg) + 
           geom_point(aes(x = displ, y = hwy, color = class) )
         
   # Lots of options for point properties that can be changed.
   # Some aesthetic mappings are more effective than others!
   # For example, compare the following with our use of color above...
   # The three mappings below are not particularly effective 
   # for this combination of variables, but you can get the idea of the 
   # range of options at your fingertips with ggplot

# size of point
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# transparency
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# point shape
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))


### FACETS
   # Faceting = stratify a scatter plot by some third variable (here,
   # categorical variable vehicle class).
   # This is a more successful way to show this information than color.

# facet_wrap is added as a third layer to our plot

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~class, nrow = 2)


# Now adding our own title, caption, axis labels with labs()
# Here labs() is a fourth layer added to our previous plot.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~class, nrow = 2) + 
  labs(
    title = "Fuel efficiency generally decreases with engine size",
    caption = "Data from fueleconomy.gov",
    x="Engine displacement (liters)",
    y="Highway gas mileage (mpg)"
  )

ggplot(data = power_christmas) + 
  geom_line(mapping = aes(x = hour, y = ERCOT)) + 
  facet_wrap(~date)
### NOTES

# 1) You may manually set an aesthetic property by
# placing it outside parentheses of the aes() command.
      # ggplot(data = mpg) + 
      #    geom_point(mapping = aes(x = displ, y = hwy), color = "blue")


# 2) You may save a ggplot as an R object, then add layers:
p1 = ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# Now adding a facet layer to p1


# Now adding a theme layer to p1 + facet



