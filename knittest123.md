knittest123
================
Zach Zaiken
10/20/2020

  - [Introduction](#introduction)
  - [Data Import](#data-import)
  - [Breweries By State](#breweries-by-state)

``` r
knitr::opts_chunk$set(fig.path='Figs/')
```

# Introduction

The analysis and code below are based off of the beer and brewery
information provided by executive teams at Budweiser. The findings from
the SMU Consulting Company, were prepared and are represented by data
scientists: Zach Zaiken, Feby Thomas and Rinku Lichti. In the Code that
follows, the team will be providing data inferences and key takeaways
for each of the Budweiser Team’s questions and inquiries. The
presentation is divided up into the sections below.

1.  Data Import

2.  Breweries by State

3.  Impact of Missing Data

4.  ABV & IBU by State

5.  Distribution and Statistics of ABV

6.  Relationship between ABV & IBU

7.  IPA vs Ale

8.  Opportunities for Market Expansion

# Data Import

The following code imports the beer and brewery data into R Studio, from
here the data will be cleaned, transformed and modeled to answer various
analysis questions and provide insight and key takeaways.

``` r
library (readr)

beer_urlfile="https://raw.githubusercontent.com/BivinSadler/MSDS_6306_Doing-Data-Science/Master/Unit%208%20and%209%20Case%20Study%201/Beers.csv"
breweries_urlfile ="https://raw.githubusercontent.com/BivinSadler/MSDS_6306_Doing-Data-Science/Master/Unit%208%20and%209%20Case%20Study%201/Breweries.csv"

beer<-read_csv(url(beer_urlfile))
```

    ## Parsed with column specification:
    ## cols(
    ##   Name = col_character(),
    ##   Beer_ID = col_double(),
    ##   ABV = col_double(),
    ##   IBU = col_double(),
    ##   Brewery_id = col_double(),
    ##   Style = col_character(),
    ##   Ounces = col_double()
    ## )

``` r
breweries<-read_csv(url(breweries_urlfile))
```

    ## Parsed with column specification:
    ## cols(
    ##   Brew_ID = col_double(),
    ##   Name = col_character(),
    ##   City = col_character(),
    ##   State = col_character()
    ## )

# Breweries By State

The following code sums up the total amount of breweries by state, then
assigns them to their respective geographical region of the country. The
data is then graphed in a bar charts showing the number of breweries by
state or % of breweries by state in descending order, with the column
colors highlighting which region of the country they reside. The Data
shows that CO,CA,MI,OR & TX have the most breweries by state. The
majority of breweries are located in Midwest and pacific regions,
whereas there is a smaller market presence in the southwest and rocky
mountain regions.

``` r
library(tidyverse)

breweries_count <- breweries%>%group_by(State)%>%dplyr::summarize(count = n())%>%arrange(desc(count))

breweries_count$State = factor(breweries_count$State,level = breweries_count$State[order((breweries_count$count))])

breweries_count$Region = case_when(
  breweries_count$State == "CT" ~ "NorthEast",
  breweries_count$State == "ME" ~ "NorthEast",
  breweries_count$State == "MA" ~ "NorthEast",
  breweries_count$State == "NH" ~ "NorthEast",
  breweries_count$State == "RI" ~ "NorthEast",
  breweries_count$State == "VT" ~ "NorthEast",
  
  breweries_count$State == "NJ" ~ "NorthEast",
  breweries_count$State == "NY" ~ "NorthEast",
  breweries_count$State == "PA" ~ "NorthEast",
  
  breweries_count$State == "IL" ~ "Midwest",
  breweries_count$State == "IN" ~ "Midwest",
  breweries_count$State == "MI" ~ "Midwest",
  breweries_count$State == "OH" ~ "Midwest",
  breweries_count$State == "WI" ~ "Midwest",
  
  breweries_count$State == "IA" ~ "Midwest",
  breweries_count$State == "KS" ~ "Midwest",
  breweries_count$State == "MN" ~ "Midwest",
  breweries_count$State == "MN" ~ "Midwest",
  breweries_count$State == "MO" ~ "Midwest",
  breweries_count$State == "NB" ~ "Midwest",
  breweries_count$State == "ND" ~ "Midwest",
  breweries_count$State == "SD" ~ "Midwest",
  
  breweries_count$State == "DE" ~ "SouthEast",
  breweries_count$State == "FL" ~ "SouthEast",
  breweries_count$State == "GA" ~ "SouthEast",
  breweries_count$State == "MD" ~ "SouthEast",
  breweries_count$State == "NC" ~ "SouthEast",
  breweries_count$State == "SC" ~ "SouthEast",
  breweries_count$State == "VA" ~ "SouthEast",
  breweries_count$State == "WV" ~ "SouthEast",
  breweries_count$State == "DC" ~ "SouthEast",
  breweries_count$State == "AL" ~ "SouthEast",
  breweries_count$State == "KY" ~ "SouthEast",
  breweries_count$State == "MS" ~ "SouthEast",
  breweries_count$State == "TN" ~ "SouthEast",
  breweries_count$State == "AR" ~ "SouthEast",
  breweries_count$State == "LA" ~ "SouthEast",
  
  breweries_count$State == "OK" ~ "SouthWest",
  breweries_count$State == "TX" ~ "SouthWest",
  breweries_count$State == "AZ" ~ "SouthWest",
  breweries_count$State == "NM" ~ "SouthWest",
  
  breweries_count$State == "CO" ~ "Rocky Mountains",
  breweries_count$State == "ID" ~ "Rocky Mountains",
  breweries_count$State == "MT" ~ "Rocky Mountains",
  breweries_count$State == "NV" ~ "Rocky Mountains",
  breweries_count$State == "UT" ~ "Rocky Mountains",
  breweries_count$State == "WY" ~ "Rocky Mountains",
  
  TRUE ~ "Pacific")
  

breweries_count%>%ggplot(aes(x=State,y=count,fill=Region))+
geom_col(position = "dodge",col="black")+ylab("Number of Breweries")+
theme(axis.text.x = element_text(angle = 45))+
scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ggtitle("Brewery Count by State")+
  geom_text(aes(label=State),hjust = -0.5,size = 3)+ coord_flip()
```

![](Figs/unnamed-chunk-2-1.png)<!-- -->

``` r
breweries_count%>%ggplot(aes(x=State,y=count/length(breweries$Brew_ID),fill=Region))+
  geom_col(position = "dodge",col="black")+ylab("% of Breweries")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_y_continuous(labels = scales::percent_format(accuracy = .5),breaks = scales::pretty_breaks(n = 10))+
  ggtitle("Brewery % by State")+
  geom_text(aes(label=State),hjust = -0.5,size = 3)+ coord_flip()
```

![](Figs/unnamed-chunk-2-2.png)<!-- -->

The following code will create a table showing the % of breweries and
cumulative % of breweries by geographic region. The majority of
breweries are located in Midwest and pacific regions, whereas there is a
smaller market presence in the southwest and rocky mountain regions.

``` r
library(gt)
breweries_count$Percent = breweries_count$count/length(breweries$Brew_ID)

BrewbyRegion <- breweries_count%>%group_by(Region)%>%dplyr::summarize(Total_Count = sum(count) ,Total_Percent=sum(Percent))%>%arrange(desc(Total_Percent))

BrewbyRegion%>%mutate(Cumulative_Percent = cumsum(BrewbyRegion$Total_Percent))%>%gt()%>%tab_header(title = "Breweries by Region")
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#rtfwgkkhyj .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#rtfwgkkhyj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rtfwgkkhyj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rtfwgkkhyj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rtfwgkkhyj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rtfwgkkhyj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rtfwgkkhyj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#rtfwgkkhyj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#rtfwgkkhyj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rtfwgkkhyj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rtfwgkkhyj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#rtfwgkkhyj .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#rtfwgkkhyj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#rtfwgkkhyj .gt_from_md > :first-child {
  margin-top: 0;
}

#rtfwgkkhyj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rtfwgkkhyj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#rtfwgkkhyj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#rtfwgkkhyj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rtfwgkkhyj .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#rtfwgkkhyj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rtfwgkkhyj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rtfwgkkhyj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rtfwgkkhyj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rtfwgkkhyj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rtfwgkkhyj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#rtfwgkkhyj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rtfwgkkhyj .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#rtfwgkkhyj .gt_left {
  text-align: left;
}

#rtfwgkkhyj .gt_center {
  text-align: center;
}

#rtfwgkkhyj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rtfwgkkhyj .gt_font_normal {
  font-weight: normal;
}

#rtfwgkkhyj .gt_font_bold {
  font-weight: bold;
}

#rtfwgkkhyj .gt_font_italic {
  font-style: italic;
}

#rtfwgkkhyj .gt_super {
  font-size: 65%;
}

#rtfwgkkhyj .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="rtfwgkkhyj" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="4" class="gt_heading gt_title gt_font_normal" style>

Breweries by Region

</th>

</tr>

<tr>

<th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Region

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Total\_Count

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Total\_Percent

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Cumulative\_Percent

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

Midwest

</td>

<td class="gt_row gt_center">

138

</td>

<td class="gt_row gt_right">

0.24731183

</td>

<td class="gt_row gt_right">

0.2473118

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Pacific

</td>

<td class="gt_row gt_center">

107

</td>

<td class="gt_row gt_right">

0.19175627

</td>

<td class="gt_row gt_right">

0.4390681

</td>

</tr>

<tr>

<td class="gt_row gt_left">

NorthEast

</td>

<td class="gt_row gt_center">

102

</td>

<td class="gt_row gt_right">

0.18279570

</td>

<td class="gt_row gt_right">

0.6218638

</td>

</tr>

<tr>

<td class="gt_row gt_left">

SouthEast

</td>

<td class="gt_row gt_center">

91

</td>

<td class="gt_row gt_right">

0.16308244

</td>

<td class="gt_row gt_right">

0.7849462

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Rocky Mountains

</td>

<td class="gt_row gt_center">

71

</td>

<td class="gt_row gt_right">

0.12724014

</td>

<td class="gt_row gt_right">

0.9121864

</td>

</tr>

<tr>

<td class="gt_row gt_left">

SouthWest

</td>

<td class="gt_row gt_center">

49

</td>

<td class="gt_row gt_right">

0.08781362

</td>

<td class="gt_row gt_right">

1.0000000

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

The following code produces a heat map showing the brewery count by
state. Red indicates higher brewery count. Similar to above shows that
CO,CA,MI,OR & TX have the most breweries by state.

``` r
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)

states <- map_data("state")

findStateAbb <- function(state_name) {
  m <- match(tools::toTitleCase(state_name), state.name)
  if (is.na(m)) {
    return(NA)
  } else {
    return(state.abb[m])
  }
}
states$state <- map_chr(states$region, findStateAbb)

sbcm <- left_join(states, breweries_count, by=c("state" = "State"))



snames <- data.frame(state=state.abb, long=state.center$x, lat=state.center$y)
snames <- left_join(snames, breweries_count, by=c("state" = "State"))
snames <- snames%>%unite("comb", c("state", "count"), sep = ":", remove = FALSE)



ggplot(sbcm, aes(long, lat)) + 
  geom_polygon(aes(group=group, fill=count )) + scale_fill_gradient(low="white", high="red") +
  geom_text(data=snames, aes(long, lat, label=comb), color="black", fontface = "bold") +
  theme_void() + 
  ggtitle("Breweries by State") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
```

![](Figs/unnamed-chunk-4-1.png)<!-- -->
