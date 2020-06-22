
# Predicting record prices


There are accompanying slides for this [HERE](/aanvullend/discogs_presentation.pdf).

## Introduction

If you're a collector of music (on physical media at least) and don't have your collection on [Discogs](https://www.discogs.com) you're missing out,
it's a database of all kinds of physical and digital releases.
A *release* in this context is a specific issue of an album, EP, or whatever in a specific country
on a specific date, etc. Releases are what make up Discogs, both the database and the marketplace.


So, here's a question: are prices of sellers in the marketplace linear with respect to information about the release?
This excludes data about what that release has sold for in the past or is being listed for currently,
both of which Discogs provides.

## Feature and seller selection

I selected thirteen features out of many that I thought might be predictive of price. These included format (vinyl mostly but some cassettes), year, number of ratings, the ratings themselves, and the ratio of people who "have" an item to those who "want" it, among others. There was a good deal of feature engineering that went into this: the ratio, logarithm-ing several features, taking year minus 1990. Then there also was cleaning of the data: I removed anything that wasn't mint condition since, while that would be a pretty good predictor, the amount of non-mint stock in the sellers I looked at was pretty low. I also removed anything before 1990, anything that has sold for over 80 dollars, anything with over 40 tracks, and some other filtering.


The first seller I wanted to look at is [theslutbunny](https://www.discogs.com/user/theslutbunny), a seller I've ordered from before based out of Seattle. 
They have about 7,000 listings, which makes for a pretty good data set.


If we look at the crosses for the independent and dependent variables (dependent is the last row and column), we might see some correlation.

![Features against the price](/img/yield/ds/tsb_pairplot.png)

We see that the strongest correlation is between some of the independent variables themselves, which isn't the greatest situation to be in, since it implies our model will suffer from multicollinearity.
I decided to go forward and run a first model in this condition, and worry about taking out features on a a second pass.

The other thing we notice is it's not entirely clear what kind of linear relationships exist between any of our independent variables and what we're trying to predict, i.e. item price in theslutbunny's store. We'll have to actually run a model in order to see what we're dealing with.



## Process and initial results

I performed a basic linear regression, with five-fold cross validation.
I also tested some regularized models but they didn't end up performing any better than
the standard linear regression, so I'll leave discussion of that out of this post.


For the 13-feature model we get an $R^2$ of 0.462
with root mean squared error of 5.283, so about five an a quarter. I'm satisfied with this, actually,
because these records can range in price from five to thirty or forty dollars
and if we're within five, that's doing pretty well in my book.



Now, can we do better with fewer features? We did notice some linear relationships between several of the features, so if we limit 
The following is a LARS path analysis. You can read this from the right to the left, as in, the features that drop out (drop to 0) sooner (hit the center line) are what the model considers less important, so we should consider taking those features out.

![LARS von Path](/img/yield/ds/tsb_lars.png)


I selected five features to drop, leaving a model with eight features.
We find out that the 8-feature model does almost as good. Our $R^2$ is around 0.439, only 0.023 less,
and our RMSE is only about a dime greater at 5.39.


So, with only two-thirds the features, we get a satisfactory model relative to the 13-feature from earlier.



## Other sellers

What about sellers who aren't rabbits?

I selected two more sellers, [All Day](https://www.discogs.com/seller/All-Day/profile) and
[Wolvie Records](https://www.discogs.com/user/Wolvie-Records) and ran both the 13-feature and the 8-feature
models on them. Here's the results, with theslutbunny thrown in for comparison.

||$R^2$|*RMSE*
---|---:|---:
theslutbunny|0.462|5.283
All Day|0.597|4.814
Wolvie|0.526|5.569

Similarly to theslutbunny, the 8-feature model performed almost as well.
Also notice with these sellers we actually do better on both metrics.

||$R^2$|*RMSE*
---|---:|---:
theslutbunny|0.439|5.392
All Day|0.546|5.113
Wolvie|0.504|5.694



## Coefficient analysis

Let's look at the coefficients of the 13-feature model for All Day.

feature | coefficient
---|---:
for sale count | 0.049
media count | 5.793
reissue | 1.951
colored | 0.776
log(have count) | 0.178
log(want count) | 1.209
want − have | 0.0004
have / want | −1.030
log(ratings count) | −0.328
log(5 − rating) | −0.106
log(tracks count) | 3.826
log(format elems. count) | 2.175
year | 0.554


You'll see most everything points in the direction you might expect, although keep in mind the magnitude of the effect varies wildly.

Reissues I had an issue with at first, I expected that to have a negative coefficient because reissues tend to be less prized than the original releases, however, then I realized that not everything *gets* a reissue, and if a record is popular enough to earn one, it will likely have more selling power even in reissued form.




## Takeaways

It seems as though some of the sellers I looked at have a slightly more "linear" pricing strategy and, importantly, determine their prices more purely based off the characteristics of the releases that we examined here in this analysis than other considerations. Like I said, chief among these would be data about what the record has sold for in the past (low, high, and median), as well as all of the prices that it is currently being sold for now. That model would likely be much more accurate, but my original goal was to see how much the prices depended *only* on the release data, not that price data.


Also, the sellers have information that I don't have, namely, how much *they* paid for the record in the first place. It could be (actually I think this is pretty possible) that by and large the variability is back on the end of the original seller, since for the most part these record stores are all resellers, and the sellers we examine on Discogs apply a pretty linear markup. This has its limits, since there are some records I know don't sell for too much originally but are very limited in how many are cut, so there would be a higher markup there, but I think this is the exception.