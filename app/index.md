
## Introduction

1. There is a lot of traffic data and device reported safety scores available.
2. The analysis of reliablility of relationship between traffic and safety scores is lacking.
3. This system could provide visulization to help police to analyze the relationship.

---

## Background & Problem 

Previously, due to unawareness of information between different departments in police system, it is hard to analyze the relationship between huge traffic statistics (in screen-monitoring department) and road safety / danger (in road safety department).


---

## Methodology & Solution

The system provides traffic statistics and safety scores at the same time to help police to compare and analyze the relationship.

What is more, the data is showing interactively while mouse over, which makes the data comparision much easier.

__Next section is showing the code to generate an interactive plot, and it will be evaluated during "slidifying" if embedded into slidify.__

---

## How To Use Interactive Checking of Relationship

To make the relationship easier to discover, an interactive is necessary which could increase the efficiency dramatically.


```r
require(rCharts)
load('./t.Rdata')
m2 = mPlot(x="time", y=c("speed", "volume", "danger"), type="Line", data=t)
m2$set(pointSize = 2, lineWidth = 1)
m2$set(dom = "rChart1")
m2$print()
```

In the system, you could select the matrics to compare, and the sensitivity of danger could be modified. 
By comparing the data from different metrics and danger score, the user could find out the relationship between traffic speed / volume and traffic danger.



---

<br/> <br/> <br/> 

Copyright © SunnyBingoMe, 2014~2015

<br/> <br/> <br/> 

