---
title: EFA and Vloeistof
author: Eli T. Drumm
date: 2019-12-20
---

## A personal accomplishment

Thursday night I graduated from my JavaScript coding bootcamp, through [Eleven Fifty Academy](https://elevenfifty.org/). Shoutout to my friend SteveTom for suggesting a bootcamp in the first place.

I want to say that this was a really great twelve weeks for me. I’ve been at some not-so-great points in my life over the past three years and this has been probably the highest I’ve felt in awhile.

What made this bootcamp really special for me was the people. After a few dropped we were left with a core group of ten students including myself. They were/are all quality folks coming from different walks of life and previous expreiences, but we really worked out as a group.

And I know JavaScript! And React and Redux and Angular and TypeScript.

If you’re thinking of attending EFA or any coding bootcamp, let me know and I’d definitely be willing to chat and answer what questions I can.


## The final projects

Our “red badge” project was *Vloeistof*, an app to track mixed drinks. The front end is built with Angular, which we essentially taught ourselves from the online docs and with occasional help from our superb instructors at EFA; the back end is Express. I wanted to write a bit more about this app than what appears on my portfolio page, so here goes.

First of all: the app is at [vloeistof.co](https://vloeistof.co/).

* The app uses Express on the back end and Angular on the front. The EFA JS program teaches you Express and React in its second block but leaves you mostly on your own with direction to different Angular materials in the third block, so I’m proud of my team and I for learning enough Angular by ourselves to make what we did in the time we had, which was four weeks (three and a half, really, after Thanksgiving and Black Friday in the first).

* The front end is currently hosted on Heroku’s hobby plan. I don’t want to pay $7/month, but I’m forcing SSL (partially because it’s a good idea, partially because Stripe wants it) and in order to get the Heroku certs on a custom domain you can’t be on the free plan. I don’t mind paying $7/month *prorated to how much it’s in use…* which it is, but it’s running all the time since as far as I know there isn’t a sleep mode I can enable somehow like on free. This means I’ll probably keep the .co link up for a few months until we get jobs and then switch all these links back to the .herokuapp.com URL.

* The *back* end is currently hosted on the Heroku free plan, so if you check out the site please bear with it while it spins up from sleep mode. It takes a second.

* We really tried to make this app mobile friendly, which I think we succeeded at (with the exception of the donate page and that’s because Stripe’s card element shows up all on one line and I didn’t get around to figuring out how to fix it). So definitely check it out on your phones.

* Speaking of which, we got the donate page working! It’s hooked up to my Stripe account so if you give us more than like $5 please let me know so I notice and share the funds with my teammates, heh. The Stripe payment system has you initiate the transaction on the back end so that users can’t choose their own payment amounts, which we really don’t care about since this is a donation feature and we’re not really providing a discrete service, but that’s what we’re doing anyway.

* By enabling Google’s Progressive Web Apps Angular package (which checks off most of [these criteria](https://developers.google.com/web/fundamentals/app-install-banners) for you, especially the service worker), we were easily able to add the ability for users to “install” our app on desktop and “add to home screen” on mobile. On Android this even adds Vloeistof to the app drawer, and on desktop it adds it to the list of apps installed on the computer. Then when you run it it appears in its own windows without a URL bar and with color theming, depending on your device. This I thought was really cool.

* There were a number of things that I thought would be hard but ended up being easier than I had guessed to implement. The donate feature was one of those; another was pagination on the drinks list pages. All you gotta do is send up the page number and page size (currently I just have this set to 12 for divisibility purposes), send it up to the server, do some quick math and feed the parameters into the `limit` and `offset` arguments to the sequelize `.findAll` method. What I wanted to get working is, so, the easy check is to say don’t display the forward button if the current page has fewer than twelve results, but then what if the number of results is divisible by the page size, then you’ll end up with an accessible blank final page. I can think of one or two ways to fix it but I haven’t yet. It’s one of those things that bothers you but it doesn’t really matter so you can’t really get yourself to do anything about it, you know?

* Features we haven’t included but would like to, let’s see… email verification, OAuth/Auth0/whatever, favoriting other users’ drinks, adding other users’ drinks to your drinks, “Search” searches drinks on our side (not just from the API), hmmm… I’ll try to add to this list as time goes on.
