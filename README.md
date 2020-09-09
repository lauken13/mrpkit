# mrp-kit
Tools and tutorials for multi-level regression and post-stratification of survey data

# Sept 3, 2020
- Need to move the folder out of the folder
- Need to change the MRP object so that the names are not seperate lists they're linked lists
- Need to make an MRP object 
  -> make the object structure
  -> methods: - method
- rename survey object and map object to "surveyStr" and "surveyMap"
  
 Need to work out MRP class spec:
 -> survey object (for survey) 
    these are not mutable (immutable?), which means we have to be smart about it - pointers to the same place in memory
    make anything you need to do to them a method - rather than messing with an object. Mutable is not the write word to it
    we want to make them point to the same place in memory. Survey object has to be immutable because you absolutely shouldn't change the data. 
    If you recode your survey then you need to create a new mrp object. 
 -> survey object (for popn)
 -> survey map. The map can be immutable. 
 
 -> absolutely need to support multiple analyses on the same questions. 
 -> these return things that are not kept but instead return another class (can have many objects in the same class). e.g., a mrp model results class. But this is 
 complicated. 
 
- no variable should be unmapped. Some confusion there. 

- what do we want the code to look like? (Jonah adds in code) - see below: 
```r
samp <- surveyStr$new(...)
pop <- surveyStr$new(...)
map <- surveyMap$new(samp, pop, ...)
mrp1 <- MRP$new(samp, pop, map)
```

# option 1
```r
mod1 <- mrp1$fit(y ~ x + (1|group))
mod2 <- mrp1$fit(y ~ x + (1|group) + (1|group2))
mod1$poststratify()
mod2$poststratify()
mod1$plot()
mrp_plot(mod1, mod2, what = "mse")
```

# option 2
```r
mrp1$fit(y ~ x + (1|group), name = "model1")
mrp1$fit(y ~ x + (1|group) + (1|group2), name = "model2")
mrp1$poststratify("model1")
mrp1$poststratify("model2")
mrp1$plot("model1")
mrp1$plot(models = c("model1", "model2"))
mrp1$save(models = c("model1"))
mrp1$sample() # returns survey sample data 
mrp1$population() # returns population data
```

Option 1 isn't mutable and compartmentalised

Option 2 means the mrp object isn't immutable 

One of the issues > is whether you want to consider multiple models in viz etc. Then option 1 you end up making hashing and option 2 you end up carrying around a lot
of model fits. would DEFINITELY need a drop function. 

Option 2 means that we would essentially have a God object (see wikipedia https://en.wikipedia.org/wiki/God_object)

Will memory force us one option or the other?
- Memory in R or memory in disc? 
- Option 2 might be easier if we are going to make calls to places where data will be stored (if we can no longer store everything in R). 

This week's todos:
- Think about the options wrt memory issues, usable issues
- Add the small change to the mrp object

