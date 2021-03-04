# mrp-kit
Tools and tutorials for multi-level regression and post-stratification of survey data

# March 4, 2021
What to do next:
- survey package integration 
- tests 
- missing and improve documentation
- naming conventions
- structure the objects 
- vignette 
- new data (makes sense to the general public)
- working through more examples
- visualizations 
      -> same model adjusted vs unadjusted
      -> same model adjusted vs weighted
      -> checking fitted model (compatable with bayesplot already) 
      -> violin plot vs line plot. 
      -> takes multiple models and comparing predictions 
    

# Oct 1, 2020

Issue RE spec : current spec:
fit_rstanarm
fit_brms
fit_lme4

Suggestion: one function called fit: 
- with other method any model we want would need to be a dependency. 
- User loads a package and provides us with whatever is needed to get that to run
- Other than posteriors we don't really need much from these packages. 
- R -> more of a grammar, nice from a workflow perspective
- M -> What would the imputs be? Would be producing fitted model object. 
- L -> What about lme4? 
- J -> Do we want to implement it? Should that be R responsibility? A lot for us to 
implement that bootstrapping. 
- L -> Helper function/common methods examples + documentation on how you do your own!
- M -> In Stan ran up into this in the same way with distributions, implemented common ones and then documented how to generalize
- J -> If we want to generalize this, the poststratify function might need different input. 
- J -> Why not take into account the predictions? 
- L -> From a user perspective it might be easier to specify fit_brms, fit_rstanarm, fit_lme4, fit_bart

- What plots do we want to support? (Lauren to scribble estimates)
- Research how to bootstrap lme4 - looks like lme4 has a bootstrap component in it (https://rdrr.io/cran/lme4/man/bootMer.html)
- Start with brms so we know what we need for lme4/bart and we know rstanarm will be synonymous. 
- posterior_epred, new levels allowed to be TRUE, var is guassian
- Still unresolved questions like the multinomial case. 
>> Jonah left <<
R: on the paper. We have a rmd file for the paper. Some of the content is a bit old.
We have a grammar of MRP that is agnostic similarly to the package like grammar of graphics
M: A taxonomy of a set of things in your universe plus a rules about how they are combined. 
Test data is cat survey 
Survey
Population
Map
Model 
Predict
Poststratify

To do:
Need to resolve issues:
M -> need a list of pairs
  -> plus some helper functions to help Jonah for checking. 
  -> helper functions for munging
L -> What plots do we want to support? (Lauren to scribble estimates)
 
Future to do:
J to look at survey fit assuming certain things about the map
 -> it won't be in the map if it's not in the survey and popn 


# Sept 24, 2020
Jonah shows the spec he wrote
Two ways of  creating an object
MRP$new()
or a regular R function that wraps
mrp_setup() - wraps the above - decided we preferred because it's more similar with the other R functionality

Need to use the "..." part of R because different types of functions have different functionality
Should the function - e.g., "stan_glmer" be at the endof the modelling component because then they're standard?

How to deal with different outcomes? LK: not so simple, we're working out how to do that. 
Logistic case - we know is likely to be prob
Summary arguement -> "probability"

What does poststrat return?
- if binomial outcome -> NxM matrix
- if multinomial outcome -> NxMXK array
- if continuous outcome -> mean and variance? (will leave this too complicated!)
-> poststrat should return the same outcomes

Poststrat returns \[poststrat cells, posterior draws\] matrix

Agenda for next week:

- What plots do we want to support? (Lauren to scribble estimates)
- Do we need a function -  that so long as it produces a certain outcome it can be included.
- Take a look at Rviz? 
- Talk about paper! :)


# Sept 17, 2020
- Decided to go with no God object

Spec for MRP object:
- Takes two survey obj and map object
- Methods: 
 -> create a poststrat matrix - the maximum ps matrix 
 -> run a models with back ends and return the modelling function
 -> see issue for more details
 
Suggests names (v2)
surveyData 
surveyMap
 
Three datasets:
- Mitzi put data in the data folder
- Rohan's two datasets are removed because they were from previous examples

Jonah moved the repo up (closed an issue)
Rohan added an issue RE: writing
Decide to write paper in rmarkdown

Next stages and todos:
- Jonah to add folder and ignore file for paper
- Lauren to change the issue #3
- Mitzi Rohan's functions to make sure we have coverage 
- Jonah to work on spec (level of detail - don't need to write algorithm but need names and signatures (what it takes in
and what is returned))



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


