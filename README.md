# covid19-norway-nowcast

Experiments nowcasting Norway's covid-19 case counts.

## Local setup

You need JAGS. `brew install jags` on OS X or `apt-get install jags` on Debian/Ubuntu.

## Run API in Docker

	$ docker build . -t covid19-nowcast
	$ docker run --rm -p 8000:8000 covid19-nowcast

## API usage

	$ curl localhost:8080/nowcast \
		-H "application/json" \
		-d '{"dates":[{"testDate": "2020-01-01", "reportDate": "2020-02-01"}, ...]}'

### Relevant papers/code

* Höhle et al: [Bayesian nowcasting during the STEC O104:H4 outbreak in Germany 2011](https://onlinelibrary.wiley.com/doi/abs/10.1111/biom.12194),
  * [Code](https://github.com/rforge/surveillance)
  * [Author's blog post](https://staff.math.su.se/hoehle/blog/2016/07/19/nowCast.html)
  * [Presentation slides](https://staff.math.su.se/hoehle/talks/IBC2016-Hoehle.pdf)
* McGough et al: [Nowcasting by Bayesian Smoothing: A flexible, generalizable model for real-time epidemic tracking](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1007735)
  * [Code](https://github.com/sarahhbellum/NobBS/)
* Günther et al: [Nowcasting the COVID-19 Pandemic in Bavaria](https://www.medrxiv.org/content/10.1101/2020.06.26.20140210v2)
  * [Code](https://github.com/FelixGuenther/nc_covid19_bavaria)
  * Looks like surveillance::nowcast rewritten in Stan?
* [Nowcasting the Number of New Symptomatic Cases During Infectious Disease Outbreaks Using Constrained P-spline Smoothing](https://pubmed.ncbi.nlm.nih.gov/31205290/)
* [Improving the Estimation of the COVID-19 Basic Reproduction Number using Nowcasting](https://arxiv.org/abs/2007.09800)


