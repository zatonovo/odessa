Odessa
======

Open data is a great concept that increases transparency in all sorts of
fields. Thanks to organizations like the Open Knowledge Foundation and 
Socrata, governmental bodies and public institutions are making 
data sets available to the public at large. While the potential 
impact of all this open data is huge, it can be a challenge 
connecting datasets together to perform aggregate analysis.
Why this is the case is the subject of a separate post as what is 
important here is how to easily connect this data. 

Inconsistent data formats is an issue that becomes more pronounced as 
more datasets become available. It's unrealistic to think that 
everything will become standardized. What is needed is a 
simple methodology for connecting arbitrary datasets together.
What does this mean? At a high level Odessa generalizes and 
automates the data munging process. 
Rather than going through the rigamarole of converting data formats 
and merging data sets every time new data is introduced,
Odessa defines a reusable set of metadata that describe the 
relationship between different datasets. The result is a simple way 
to quickly glue disparate data sets together. 

### Datasets
Odessa doesn't store any actual data. Rather, it is a repository of 
metadata along with a library (currently in R) that understands the metadata.
Thanks to CKAN, the data and metadata can be grouped into *packages* 
that can be easily downloaded. To see the available packages via Odessa,
execute the following command.

    > package_list()
     [1] "nyc-school-district-index"        "geolocation-1"
     [3] "nyc-energy-consumption-2010"      "nyc-demographics"
     [5] "geolocation-2"                    "datetime-2"
     [7] "nyc-ap-school-results-2010"       "world-countries"
     [9] "odessa-binding"                   "datetime-1"
    [11] "nyc-school-district-demographics" "nyc-sat-results-2010"

As more people add packages, it benefits everyone interested in 
analysis since the metadata need only be created once per dataset.
As the number of available packages increase, it will be necessary 
to add search capabilities.

A dataset is downloaded by using the `fetch` function. 
The available *bindings* for a dataset can be viewed with the 
`which.bindings` function.

    > a <- fetch('geolocation-1')
    > which.bindings(a)
    [1] "location"
    > b <- fetch('geolocation-2')
    > which.bindings(b)
    [1] "latitude"  "longitude"

It is possible to join using any of these bindings as well as 
bindings registered in Odessa's master graph. Here we can see that 
the `b` dataset can be joined on location as well:

    > which.ancestors(b)
    [1] "location"

### Geolocation
Many datasets are related to physical spaces, so joining and 
aggregating data using geographic information is rather useful.
The example above are snapshots of larger datasets for pedagogical purposes.

Let's look at what these files contain. First is a file that uses a 
field name 'Location' that contains the geo-coordinates for a school.

    > head(a)
                      Location             Feature                         Name
    1      (47.5636, -122.358) Alternative Schools                   Pathfinder
    2   (47.52374, -122.27201) Alternative Schools The New School At Southshore
    3 (47.642257, -122.399261) Alternative Schools         Catharine Blaine K-8
    4  (47.713431, -122.31466) Alternative Schools           As #1 At Pinehurst
    5    (47.6216, -122.30524) Alternative Schools                         Nova
    6 (47.622078, -122.353983) Alternative Schools            The Center School

This second file contains two separate fields containing latitude and longitude.

    > head(b,4)
        Longitude  Latitude             Address
    1    -122.358   47.5636  1901 SW Genesee St
    2  -122.27201  47.52374 4800 S Henderson St
    3 -122.399261 47.642257     2550 34th Ave W
    4  -122.31466 47.713431   11530 12th Ave NE
                                                            Website
    1 http://www.seattleschools.org/schools/pathfinder/welcome.html
    2             http://www.seattleschools.org/schools/southshore/
    3                 http://www.seattleschools.org/schools/blaine/
    4                                            http://as1web.com/

To join these together would typically require parsing and merging 
these fields for each person who wants to work with this data.
With Odessa, only one person makes the investment and others get to 
enjoy the dividend.

    > z <- conjoin(a,b, 'location')

### Temporal Keys
Odessa understands temporal keys as well, so it is possible to join
on dates as well as timestamps.

    z <- conjoin(x,y, c('month','year'))

This is almost too easy. That's the idea. 

### JSON Destroyer
Many web services return JSON. While JSON is useful for describing 
arbitrary data structures, it is a bear to use for data analysis. 
Often the first step after downloading JSON data is to transform it
into a denormalized table structure. Odessa provides the `denormalize`
function to do just that. Just specify the keys you want and, Odessa
does the rest.

Here's an example with data from govtrack.us. Note the dot notation which
specifies sub nodes.
```R
url <- 'https://www.govtrack.us/api/v2/role?current=true'
r <- GET(url)
o <- content(r)
cols <- c('state','role_type','person.birthday','person.gender','party')
df <- denormalize(o$objects, keep=cols)
```

The resulting data.frame looks like
```R
> head(df)
  state role_type person.birthday person.gender      party
  1    MO   senator      1950-01-10          male Republican
  2    AR   senator      1950-12-10          male Republican
  3    NC   senator      1955-11-30          male Republican
  4    GA   senator      1944-12-28          male Republican
  5    IL   senator      1959-09-15          male Republican
  6    KS   senator      1954-05-29          male Republican
```

Going the other way, creating JSON data is not as smooth as you might expect.
Out of the box using either RJSONIO or rjson, results in column-major data.
This is to be expected since R stores data in column format.
```R
> toJSON(head(df))
[1] "{\"state\":[\"MO\",\"AR\",\"NC\",\"GA\",\"IL\",\"KS\"],\"role_type\":[\"senator\",\"senator\",\"senator\",\"senator\",\"senator\",\"senator\"],\"person.birthday\":[\"1950-01-10\",\"1950-12-10\",\"1955-11-30\",\"1944-12-28\",\"1959-09-15\",\"1954-05-29\"],\"person.gender\":[\"male\",\"male\",\"male\",\"male\",\"male\",\"male\"],\"party\":[\"Republican\",\"Republican\",\"Republican\",\"Republican\",\"Republican\",\"Republican\"]}"
```

However, most Javascript libraries expect JSON to be in row-major format.
For example, D3.js expects data sets to be in this structure. 
Odessa provides the `row\_major` function to do this conversion so that
the JSON serializers do the right thing.
```R
> toJSON(row_major(head(df)))
[1] "[{\"state\":\"MO\",\"role_type\":\"senator\",\"person.birthday\":\"1950-01-10\",\"person.gender\":\"male\",\"party\":\"Republican\"},{\"state\":\"AR\",\"role_type\":\"senator\",\"person.birthday\":\"1950-12-10\",\"person.gender\":\"male\",\"party\":\"Republican\"},{\"state\":\"NC\",\"role_type\":\"senator\",\"person.birthday\":\"1955-11-30\",\"person.gender\":\"male\",\"party\":\"Republican\"},{\"state\":\"GA\",\"role_type\":\"senator\",\"person.birthday\":\"1944-12-28\",\"person.gender\":\"male\",\"party\":\"Republican\"},{\"state\":\"IL\",\"role_type\":\"senator\",\"person.birthday\":\"1959-09-15\",\"person.gender\":\"male\",\"party\":\"Republican\"},{\"state\":\"KS\",\"role_type\":\"senator\",\"person.birthday\":\"1954-05-29\",\"person.gender\":\"male\",\"party\":\"Republican\"}]"
```

Installation
============
The Odessa R package is not yet published to CRAN, so you must install
via github.
```R
library(devtools)
install_github('odessa','zatonovo')
```

Other Examples
==============
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

