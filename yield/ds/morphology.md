
# Classification on a galactic scale

Slides for this are available [HERE](/aanvullend/galaxies_presentation.pdf).



## Intro

So ever since I was a young kid I've been interested in space, an interest that I've carried with me to this point in my life, although I don't find many ways to explore it. I'm not even an amateur astronomer, I think that as a youth I was good at memorizing and there were a lot of space facts to memorize, but that's not to say I'm not genuinely interested however. I always say that going to space is one of the two things that would make my life complete.

Even if I have no chance of physically exploring another galaxy, however, I can still explore the data. So for my third project at Metis, I downloaded a data set of galactic info from the [Galaxy Zoo](https://www.zooniverse.org/projects/zookeeper/galaxy-zoo/) [data download page](https://data.galaxyzoo.org/). Specifically, I used the data set from the paper on AGN host galaxies ([Schawinski et al. 2010](http://adsabs.harvard.edu/abs/2010ApJ...711..284S)) linked on that page.

The Galaxy Zoo (the first one, there have been several) was a project to categorize galaxies by morphology (shape and structure) using a crowdsourced approach, where citizen scientists voted on properties of the galaxies and then using some scheme a set of likely morphological classifications was found.



There have been a lot of developments since 2010 in machine learning, however, so given the data about the galaxies that's provided in the set, can we classify them ourselves, using the GZ tags as the ground truth?





## Feature selection

Right away we can toss out RA and declension, since those are just the location in the sky and presumably the universe looks the same in every direction on the scales that we're considering (this is the cosmological principle, isn't it? yeah, I don't want to get into that here).

The dataset comes with features computed for each galaxy such as luminosity, velocity dispersion, redshift, and a few others.


I also included NVOTE, the number of votes cast on a given galaxy, just to see if the system was biased towards presenting one type of galaxy more often, though (spoiler) it doesn't turn out that way... I mean including NVOTE is kind of against the spirit of the project anyway, as we want these classifications without the hassle of a 200,000+ person effort.



## The logistic model

Let's try a logistic regression first.



The accuracy is 87%, and the ROC AUC is 0.923. Our precision for both classes is in the mid to upper .8s, and while our recall for late galaxies is 0.92, recall for non-late-type galaxies is only 0.72.


The coefficients are weird, because there's so much collinearity between the spectra features.

I'm going to move on here because I'm most interested to get to...





## The random forest

Can we do better with a different model?
I ran several, including your standard decision tree, but had the best luck with random forests. Again, I'm using the implementation in sklearn, `sklearn.ensemble.RandomForestClassifier`. 

For reference, a small part of one of the decision trees that make up one of the forests looks like this:

![Part of a decision tree](/img/yield/ds/dec_tree.png)

The default is for 100 of these to make up one forest.
Using this and the other standard hyperparameters that come with scikit-learn,
our accuracy jumps up to 89.6%, more than two percent.
Our ROC AUC goes up by about the same amount. 





I also trained a neural net on this model, although I did it on the fly in the terminal and don't have code for it (sorry John). The accuracy, precision, and recall all went up more, but keep in mind it's not by much and the ANN takes time to train. For a real-world application I would probably go with the neural network because the training time wasn't that long, but if we want a little more interpretability (not a lot, just some), maybe the random forest would be a good alternative.



## Hiking the random forests

We got pretty good results with the default hyperparameters for the random forest that come with sklearn, but could we do even *better* by tuning those parameters?
I tried this by varying 

* `max_depth` from 2 to 9 plus an unlimited option (the default)
* `max_features` from 2 to 9 (the square root of the features is the default, so 3 or 4 in my case)
* `n_estimators` from 1, 5, 10, 25, 50, 75, 100 (default), 125, 150, 200, 300



The whole grid search took something like four hours to run on my computer. One nice feature of the grid search implementation in sklearn, aside from the built-in cross-validation, is it tracks the training and scoring times of each cross-val run and returns the mean and standard deviation of each. 


<p>![Random forest gridsearch](/img/yield/ds/rf_gridsearch.gif)</p>

This is a GIF of me tuning the *n_estimators* parameter, which controls how many trees make up a forest. The bluer the square, the higher the rank among the 700-some forests tested here. You'll see complete yellow when I turn the slider all the way to the left, where it collapses to a single decision tree, as it will overfit here and do poorly. Along the x axis we have *max_features*, which is the number of features that a single tree in the forest can split on, and along the y axis we have *max_depth*, which we want to set at maximum (or in this case 0, along the top row (the axis is numbered backwards, sorry)). 

## Takeaways



I had other data sets I wanted to work with but lacked the expertise to compute information like velocity dispersion, luminosity in the right band, redshift, etc. from the raw spectral data. This amounts to a form of feature engineering which I wasn't nearly capable of, which limited me to the AGN dataset that I used for this project. With an astronomer's help, I could have potentially done those calculations and worked with the more expansive datasets.





So, the morale of the story is that data science can't be done in isolation, all the more so if the data is difficult to interpret without domain-specific expertise. You can't just walk in and start slinging numbers and models around, as sexy as that sounds.











