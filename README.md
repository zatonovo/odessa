odessa
======

```R
a <- fetch('nyc-sat-results-2010')
b <- fetch('nyc-school-district-demographics')
i <- fetch('nyc-school-district-index')
y <- conjoin(a,i, 'borough')
z <- conjoin(y,b, c('district','borough_name'))
z <- z[z$Number.of.Test.Takers!='s',]
z$Number.of.Test.Takers <- as.numeric(z$Number.of.Test.Takers)
z$Critical.Reading.Mean <- as.numeric(z$Critical.Reading.Mean)

fn <- function(df) {
  count <- sum(df$Number.of.Test.Takers)
  reading <- sum(df$Number.of.Test.Takers * df$Critical.Reading.Mean) / count
  male <- df$PERCENT.MALE[1]
  female <- df$PERCENT.FEMALE[1]
  data.frame(count, reading, male, female)
}
z1 <- ddply(z, .(odessa.key.district, odessa.key.borough), fn)
```

