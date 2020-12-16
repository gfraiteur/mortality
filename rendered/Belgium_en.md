Analysis of Belgian mortality data in 2020
================
Gael Fraiteur

  - [Introduction](#introduction)
  - [About the author](#about-the-author)
  - [Data sources](#data-sources)
  - [Building a predictive model](#building-a-predictive-model)
  - [Excess death rate](#excess-death-rate)
  - [Cumulative excess mortality](#cumulative-excess-mortality)
  - [Comparison with reported COVID
    deaths](#comparison-with-reported-covid-deaths)
  - [Decreased death rate of the youngest age
    group](#decreased-death-rate-of-the-youngest-age-group)
  - [Revisiting controversial
    questions](#revisiting-controversial-questions)
  - [Conclusion](#conclusion)

## Introduction

When discussing the effect of the COVID19 epidemic, Belgium is an
interesting country to study for several reasons. First, it has the
highest reported death rate from COVID19 in the world. Secondly,
Statbel, the Belgian governmental agency for statistics, publishes
mortality information every week with a delay of just 3 weeks. Last and
not least, this is the country where the author grew up.

Our approach is to compare the number of deaths in 2020 (for weeks 1 to
48) with the number of deaths that would be *expected* based on the
structure of the population on January 1st, 2020 and on the usual death
rates. The difference between expectations and reality gives us a metric
named *excess deaths* when it is positive, or *deaths deficit* when it
is negative.

We compare this metric with the number of deaths due to COVID as
reported by Sciensano, and with historical death and mortality data.

At the end of the article, we try to answer a few controversial
questions based on data.

This article, as well as all R scripts that have been used to compute
the data, are hosted on GitHub at
<https://github.com/gfraiteur/mortality>. All data comes from open
sources and can be freely downloaded. If you have a question or remark
related to this article, or if you have found a bug or inaccuracy,
please open an issue on GitHub or, better, submit a pull request.

## About the author

Gael Fraiteur graduated from the Louvain School of Engineering in 2001
as a civil engineer in applied mathematics. He has worked since then in
the software industry. In 2004, he started an open-source project named
PostSharp. In 2009, he founded a company to market and develop the
product. As the CEO and principal engineer of PostSharp Technologies,
Gael now shares his time between R\&D, management and marketing.

## Data sources

The data were downloaded from the following sources:

  - Number of deaths: Statbel, [Number of deaths per day, sex, age,
    region, province,
    district](https://statbel.fgov.be/en/open-data/number-deaths-day-sex-district-age).
    For 2020, this data is available until the 48-th week.

  - Structure of population: Statbel, [Population by place of residence,
    nationality, marital status, age and
    sex](https://statbel.fgov.be/en/open-data?category=23)

  - Cause of mortality: Statbel, [Causes of death by month, sex, age
    group and
    region](https://statbel.fgov.be/en/open-data/causes-death-month-sex-age-group-and-region)

  - COVID mortality: Sciensano, [Mortality by date, age, sex, and
    region](https://epistat.wiv-isp.be/covid/)

  - General mortality: Human Mortality Database, [Death rates for
    Belgium, 1x1](https://www.mortality.org/cgi-bin/hmd/country.php?cntr=BEL&level=1)

  - Temperatures: Royal Meteorological Institute of Belgium, [Automatic
    weather station (AWS) daily
    observations](https://opendata.meteo.be/geonetwork/srv/eng/catalog.search;jsessionid=B123D8FF9D843F6B8721B8878EB55479#/metadata/RMI_DATASET_AWS_1DAY)

The script gives the exact URLs of the source data files we used, as
well as the commands we have used to parse and prepare the dataset.

## Building a predictive model

If we want to compare the actual number of deaths in 2020 with what
would be a “normal” number of deaths, we need to define what “normal”
is. Many authors have taken the average of the past 5 or 10 years as
their point of comparison. However, this choice gives numbers that are
lower than what is realistic because the number of deaths has constantly
increased of about 0.5% per year in the last 10 years. Comparing 2020 to
2015 would then cause a difference of 2,500 deaths, which is already
significant.

![](Belgium_en_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Our model of yearly mortality is built in three steps:

1.  The first input is the structure of the population the 1st of
    January of each year between 2009 and 2020 (i.e. the number of
    residents of a given age and sex alive on that day).

2.  The second input is the mortality data for Belgium for the
    corresponding age, sex and year, i.e. the probability that a person
    who was alive on January 1st morning would be dead on December 31st
    evening. The expected number of deaths for a given group and year is
    simply the product of the first input with the second input.

3.  We split the yearly data into weeks based on weekly historic death
    data.

### 1\. Structure of the population

The first input of our model is the structure of the population
published by Statbel (see data sources). The data set gives us the
number of inhabitants of each sex who are alive and have a specific age
on January 1st of a given year. This data is available since 2009.

As you can see on the next graph, the number of people aged 65 years or
more is increasing every year by approximately 34,000, a 1.6% growth.

![](Belgium_en_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### 2\. Death rate

The *death rate* for a given period is the probability that someone of a
given group (i.e. given age group and sex) who is alive at the beginning
of the period would be alive at the end of the period. Unless stated
otherwise, the period is a calendar year.

Let’s look at the death rate for a few ages. As you can see, the death
rate is continuously decreasing except for the age group over 100. This
graph also shows a greater variance of the death rate for older age
groups, which seems to mean that these age groups are more sensitive to
events that happen less than once a year, such as epidemics or
exceptional weather.

![](Belgium_en_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

We model the death rate with a linear regression for each age and sex.
This model allows us to extrapolate the data to 2019 and 2020, but it
also removes the year-to-year variations for all previous years. That
is, this death rate model removes the effect of epidemics and weather
conditions that happen less frequently than yearly.

Once we have a death rate model for each group and year, we multiply
this coefficient by the actual population for this group and year, which
should give us the number of expected deaths. However, the expected and
observed number of deaths, summed from 2009 to 2019, don’t match
exactly. This discrepancy is expected and its cause is not important. To
cancel the discrepancy, we apply the following correction factors:

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#kvibszufmt .gt_table {
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

#kvibszufmt .gt_heading {
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

#kvibszufmt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#kvibszufmt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#kvibszufmt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kvibszufmt .gt_col_headings {
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

#kvibszufmt .gt_col_heading {
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

#kvibszufmt .gt_column_spanner_outer {
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

#kvibszufmt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#kvibszufmt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#kvibszufmt .gt_column_spanner {
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

#kvibszufmt .gt_group_heading {
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

#kvibszufmt .gt_empty_group_heading {
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

#kvibszufmt .gt_from_md > :first-child {
  margin-top: 0;
}

#kvibszufmt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#kvibszufmt .gt_row {
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

#kvibszufmt .gt_stub {
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

#kvibszufmt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kvibszufmt .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#kvibszufmt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kvibszufmt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#kvibszufmt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#kvibszufmt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kvibszufmt .gt_footnotes {
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

#kvibszufmt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#kvibszufmt .gt_sourcenotes {
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

#kvibszufmt .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#kvibszufmt .gt_left {
  text-align: left;
}

#kvibszufmt .gt_center {
  text-align: center;
}

#kvibszufmt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#kvibszufmt .gt_font_normal {
  font-weight: normal;
}

#kvibszufmt .gt_font_bold {
  font-weight: bold;
}

#kvibszufmt .gt_font_italic {
  font-style: italic;
}

#kvibszufmt .gt_super {
  font-size: 65%;
}

#kvibszufmt .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="kvibszufmt" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Age group

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

F

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

M

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left gt_stub">

0-24

</td>

<td class="gt_row gt_right">

100.1%

</td>

<td class="gt_row gt_right">

99.5%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

25-44

</td>

<td class="gt_row gt_right">

99.2%

</td>

<td class="gt_row gt_right">

100.2%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

45-64

</td>

<td class="gt_row gt_right">

100.0%

</td>

<td class="gt_row gt_right">

100.0%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

65-74

</td>

<td class="gt_row gt_right">

100.7%

</td>

<td class="gt_row gt_right">

100.6%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

75-84

</td>

<td class="gt_row gt_right">

99.3%

</td>

<td class="gt_row gt_right">

100.0%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

85+

</td>

<td class="gt_row gt_right">

102.0%

</td>

<td class="gt_row gt_right">

102.7%

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

The following graphs shows the resulting projections of number of deaths
per year and age group.

![](Belgium_en_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Let’s now compare the projections with the reality:

![](Belgium_en_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Splitting years into weeks

We now have a yearly model, but we need weekly projections. For our
weekly model, we first compute the percent of deaths, for each age
group, that happens in a given week of the year, and we multiply this
coefficient with the yearly death rate for this year. Note that we
didn’t compute a different coefficient for each sex because the data
is already noisy enough, especially for younger age groups.

![](Belgium_en_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

We can notice a higher seasonality of death rate for the more aged
groups. For the younger groups, there is almost no seasonal correlation
but there is however more noise due to the lower amount of data points.
We chose not to address this problem as it should have only minor impact
on the conclusions.

With this weekly model, we can finally compute the weekly predictive
model and compare it with the reality:

![](Belgium_en_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Excess death rate

*Excess deaths* is the different between the actual number and the
expected number of deaths. When there are fewer deaths than expected,
this is called *death deficit*.

The following graphs represents excess deaths from 2009:

![](Belgium_en_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Cumulative excess mortality

Weekly excess mortality does not give a good insight of the impact of a
specific *event*, such as an epidemic or a special weather condition,
over a long period of time. It does not answer the following questions:

  - How many people died during a specific event?
  - How long does it take to “recover” from the event?

To answer these questions, the cumulative excess mortality (i.e. the
integral of the curve over time) is a better metric.

The following graphs show the cumulative death, first starting from
January 1st, 2009, and then zooming in on 2019-2020.

![](Belgium_en_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->![](Belgium_en_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

Note that the initial value of the cumulative metric is arbitrary. Here,
we chose that the zero point would be January 1st, 2009. Because our
model is based on a linear regression over years, and because of the
yearly fluctuations, we can expect that the zero point is crossed every
second year.

The second arbitrary zero is the one of January 1st, 2020. This one is
the consequence of the calibration of our model with real data for the
period from 2009 to 2019. It is important to understand how arbitrary
this zero is.

The cumulative maximal death deficit was observed on March 11th, 2020,
with a value of 1868 (point represented in green in the graph above). So
when comparing the expected cumulative mortality with the real
cumulative mortality, a difference of 1868 can be obtained just by
choosing a different beginning of the comparison time window.

### Excess death rate by age group

Taking this remark into account, we can now look at the increased death
rate for the year 2020 (between weeks 1 and 48) for different age
groups. The following table shows the difference in death rate during
the studied period in absolute terms and in terms that are relative to
the expected values for 2020.

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#fguxuxvbpt .gt_table {
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

#fguxuxvbpt .gt_heading {
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

#fguxuxvbpt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fguxuxvbpt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fguxuxvbpt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fguxuxvbpt .gt_col_headings {
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

#fguxuxvbpt .gt_col_heading {
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

#fguxuxvbpt .gt_column_spanner_outer {
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

#fguxuxvbpt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fguxuxvbpt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fguxuxvbpt .gt_column_spanner {
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

#fguxuxvbpt .gt_group_heading {
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

#fguxuxvbpt .gt_empty_group_heading {
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

#fguxuxvbpt .gt_from_md > :first-child {
  margin-top: 0;
}

#fguxuxvbpt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fguxuxvbpt .gt_row {
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

#fguxuxvbpt .gt_stub {
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

#fguxuxvbpt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fguxuxvbpt .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#fguxuxvbpt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fguxuxvbpt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fguxuxvbpt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fguxuxvbpt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fguxuxvbpt .gt_footnotes {
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

#fguxuxvbpt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#fguxuxvbpt .gt_sourcenotes {
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

#fguxuxvbpt .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#fguxuxvbpt .gt_left {
  text-align: left;
}

#fguxuxvbpt .gt_center {
  text-align: center;
}

#fguxuxvbpt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fguxuxvbpt .gt_font_normal {
  font-weight: normal;
}

#fguxuxvbpt .gt_font_bold {
  font-weight: bold;
}

#fguxuxvbpt .gt_font_italic {
  font-style: italic;
}

#fguxuxvbpt .gt_super {
  font-size: 65%;
}

#fguxuxvbpt .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="fguxuxvbpt" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Age group

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Expected death rate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Actual death rate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Absolute excess death rate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Relative excess death rate

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="5" class="gt_group_heading">

F

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

0-24

</td>

<td class="gt_row gt_right">

0.02%

</td>

<td class="gt_row gt_right">

0.01%

</td>

<td class="gt_row gt_right">

−0.01%

</td>

<td class="gt_row gt_right">

−34%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

25-44

</td>

<td class="gt_row gt_right">

0.04%

</td>

<td class="gt_row gt_right">

0.05%

</td>

<td class="gt_row gt_right">

0.00%

</td>

<td class="gt_row gt_right">

4%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

45-64

</td>

<td class="gt_row gt_right">

0.30%

</td>

<td class="gt_row gt_right">

0.31%

</td>

<td class="gt_row gt_right">

0.00%

</td>

<td class="gt_row gt_right">

1%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

65-74

</td>

<td class="gt_row gt_right">

1.03%

</td>

<td class="gt_row gt_right">

1.14%

</td>

<td class="gt_row gt_right">

0.11%

</td>

<td class="gt_row gt_right">

10%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

75-84

</td>

<td class="gt_row gt_right">

2.93%

</td>

<td class="gt_row gt_right">

3.54%

</td>

<td class="gt_row gt_right">

0.61%

</td>

<td class="gt_row gt_right">

21%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

85+

</td>

<td class="gt_row gt_right">

12.60%

</td>

<td class="gt_row gt_right">

14.24%

</td>

<td class="gt_row gt_right">

1.64%

</td>

<td class="gt_row gt_right">

13%

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="5" class="gt_group_heading">

M

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

0-24

</td>

<td class="gt_row gt_right">

0.03%

</td>

<td class="gt_row gt_right">

0.02%

</td>

<td class="gt_row gt_right">

−0.01%

</td>

<td class="gt_row gt_right">

−35%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

25-44

</td>

<td class="gt_row gt_right">

0.08%

</td>

<td class="gt_row gt_right">

0.09%

</td>

<td class="gt_row gt_right">

0.00%

</td>

<td class="gt_row gt_right">

5%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

45-64

</td>

<td class="gt_row gt_right">

0.47%

</td>

<td class="gt_row gt_right">

0.52%

</td>

<td class="gt_row gt_right">

0.04%

</td>

<td class="gt_row gt_right">

9%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

65-74

</td>

<td class="gt_row gt_right">

1.80%

</td>

<td class="gt_row gt_right">

2.02%

</td>

<td class="gt_row gt_right">

0.22%

</td>

<td class="gt_row gt_right">

12%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

75-84

</td>

<td class="gt_row gt_right">

4.54%

</td>

<td class="gt_row gt_right">

5.42%

</td>

<td class="gt_row gt_right">

0.89%

</td>

<td class="gt_row gt_right">

20%

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

85+

</td>

<td class="gt_row gt_right">

14.72%

</td>

<td class="gt_row gt_right">

16.83%

</td>

<td class="gt_row gt_right">

2.11%

</td>

<td class="gt_row gt_right">

14%

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

The following graph shows the relative values only:

![](Belgium_en_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

As you can see, the age group whose death rate is the most different in
2020 than the expectation is the younger one, which had 35% less risk to
die in 2020 than in another year. We will come back to this number.

The age group whose death rate is the most negatively affected in 2020
is the 75-84 one, with a risk of dying 20% higher than expected.

### Absolute death rate in historical context

To give some context, here is the historic death rate since 2000
compared to what was expected for 2020. By construction, all lines
converge to 100% in 2020.

![](Belgium_en_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

It can be seen on this graph that the death rates for the groups
exhibiting the largest difference are ones that would be typical in
2012.

For more perspective, here the same graph starting from 1900 on a
logarithmic vertical scale.

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Belgium_en_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Death rate variation in historical context

We have seen that the death rate in 2020 was typical for 2012. How can
we then explain the framing of the epidemic, by the government and
media, as a catastrophe of unseen since Word War II or even the Spanish
flu? Looking at the *year-to-year variation* of death rate instead of
the death rate itself gives some hints.

How far should we look in the past to see a death rate that is, in
absolute terms, 1% to 2% higher than normal?

To answer this question, we will use the death rate data of the Human
Mortality Database since 1900. We will define “normal” as being a 5-year
rolling median. The median does a better job than the mean to eliminate
exceptional events like epidemics.

The following graph shows only strictly positive deviations and the
vertical axis is logarithmic, with horizontal lines for 0.2%, 1% and 2%,
the approximate values of excess death rate for the three oldest age
groups.

![](Belgium_en_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

The next graph aggregates the data by decade and shows the maximum
deviation of the yearly death rate from the median death rate during the
whole decade, again with horizontal lines for 0.2%, 1% and 2%.

![](Belgium_en_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

It is clear from these 2 graphs that a 2% deviation is usual and even on
the small end for the age group over 85 years.

However, for the groups 65-74 and 75-84, we have to go back to the 1960s
to see a similar deviation.

### Compared death rate: conclusion

How exceptional is 2020 to other years? The conclusion is paradoxical.

If we look at the absolute death rates, 2020 is similar to 2012. If we
look at the excess death rate for the 85+ age group, we also find
numbers that would be usual around 2010. We could then say that 2020 is
a one-in-ten-years occurrence, which would make it difficult to justify
the extraordinary restrictions on freedom of movement and gathering.

However, the real originality of 2020 from the point of view of historic
death data is not the absolute death rate, not even the excess death
rate for the oldest age group, but the *excess death rate of the 64-85
age group*, for which there is no equivalent since the 1960s.

This paradox brings light to the polarized discussion between those who
believe that the epidemic is a normal event that happens every 10 years
(it is, in terms of absolute death rate) and those who believe there was
no worse event since World War II (this is true but the 1960s were not
far).

## Comparison with reported COVID deaths

The following graph compares the number of weekly excess deaths with the
number of deaths attributed to COVID19, as reported by Sciensano.

![](Belgium_en_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

We can see that the number of deaths attributed to COVID19 by Sciensano
copies quite well the number of excess deaths estimated by our model, at
least during the two epidemic peaks. The most notable exception is
during the deaths peak of August. The second difference that needs to be
discussed is the death deficit after the spring peak.

### Death peak in August

The number excess deaths during the heat wave of August 2020 was 1398.
By comparison, the heat wave of July 2019 caused an excess death of 136:
10 times less.

Can this be explained by exceptional temperatures? The following graphs
shows the history of maximum daily temperatures in Belgium as well of a
7-day and a 30-day right-aligned moving average of this metric.

![](Belgium_en_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

As we can see, the maximal weekly temperatures were not exceptional
compared to 2019 or even 2018. Therefore, temperature alone cannot be a
sufficient explanation of the peek of deaths in August 2020. Other
factors need to be considered, for instance a generally weaker condition
of the population due to the epidemic or to the lockdown itself.

### Death deficit between death peeks

The second difference between the COVID19 death curve and the excess
death curve is that the second shows a death deficit after the first
epidemic peak. This means that a part of people who died during the
epidemic peak would have died anyway a couple of weeks later.

This is more visible on the cumulative excess deaths graph

![](Belgium_en_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

During the spring peak, the cumulative excess death was 8673. During the
summer, before the August peak, the death deficit was 1105, that is,
approximately 13% of people who died during of COVID19 during spring had
their life shortened by less than 3 months.

### Comparision of number of deaths by age group

The following chart compares the number of excess deaths in 2020 with
the number of COVID19 deaths.

![](Belgium_en_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

The numbers are quite similar, except for the oldest age group where
COVID19 deaths are 1663 (23%) higher than excess deaths. This means than
an approximate 20% of the people over 85 who died of COVID19 during 2020
(or were diagnosed as such) would have died from another cause in the
same period

## Decreased death rate of the youngest age group

The death rate of the 0-24 age group is 35% lower in 2020 than expected.
How can we explain this significant difference? Intuitively, this could
be explained by the lower exposure to risk of accidents. Let’s test this
hypothesis based on the *Causes of death by month, sex, age group and
region* data set published by Statbel (see data sources).

The cause code V01-Y98 includes all deaths by external causes, including
accidents and homicides. This data is available from 2009 to 2019. Let’s
visualize the percent of accidental deaths by age group and sex:

![](Belgium_en_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

![](Belgium_en_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

If we extrapolate this data linearly to 2020, we get a percent of
accidental deaths of 33% for males and 18% for females

This shows that we cannot explain the decrease of the death rate of the
youth in 2020 solely by the decrease of accidental mortality caused by
the lockdowns. For males, it would mean that there would be almost
deaths by accident during the whole period (not just during the
lockdowns). For females, accidental death is smaller than the mortality
drop.

Therefore, other factors have to be considered to explain this drop, and
the data we analyzed does not provide any useful hint.

Because of this, it is also useless to try to interpret the death rate
in 2020 as a negative factor to the the virus itself, and a positive
factor due to the restrictions of movements and activities. Both factors
probably existed, but the data available now do not allow to quantify
them.

## Revisiting controversial questions

This article allows to answer a few polemic questions that rose around
the COVID19 epidemic and the response of the government and mainstream
media:

  - **Is the number of deaths reported by Sciensano exaggerated, either
    by biased diagnostic methods or deliberately? **
    
    **No**, there is no indication that the death data reported by
    Sciensano are incorrect.

  - **Would a majority of people who died from COVID19 have died from
    another cause very soon?**
    
    **No**. Approximately 13% of people who died during of COVID19 would
    have died less than 3 months from other causes. Based on death data,
    we cannot take any conclusion for a longer time period or for the
    people who died in autumn.

  - **Has the restrictions of freedom of movement impacted the death
    rate?**
    
    **Yes**, the death rate on the 0-24 age group has decreased by 30%
    relatively to normal in 2020. However this decrease cannot be
    explained by a decrease of accidental mortality alone.

  - **Is the 2020 year exceptionally bad?**
    
    Yes and no.
    
    **Yes** if you look at excess death rate for the age group from 65
    to 74 years, i.e. the risk of dying in 2020 compared to
    expectations. There is no equivalent since the 1960s.
    
    **No** if you look at absolute death rate number. The absolute death
    rates of 2020 are close to the ones of 2010. That is, if there were
    in 2010 the same number of deaths than in 2020 for the same
    population structure, this would be a normal year.
    
    2020 is *at most* 20% worse than a normal year – for the most
    affected age group. That means that instead of this risk being
    approximately 5% at 80 years, it was at 6% in 2020.For a 40 year-old
    man, this risk is 0.118% instead of 0.111%.

This article also lets a few questions unanswered:

  - **How to explain the 30% decrease of mortality in the 0-24 age
    group?**
    
    A decrease of accidental mortality is not a sufficient explanation.

  - **How to explain the increased mortality during August 2020?**
    
    The mortality was 10 times higher during the heat peak of July 2020
    than during the one of July 2019. Temperatures were not
    significantly higher, therefore another explanation is necessary.

## Conclusion

Perhaps the most striking conclusion is the paradox between the fact
that, on one side, the death rate in 2020 was equivalent to the one
around 2010 and, on the other side, there has not been a comparable
year-to-year variation of death rate for the 65-84 age group until the
1960s.

What happened that a situation that would have been normal in 2010 now
cause major disruptions of our societies?

Several psychological, sociological and technological explanations could
be proposed, but they would be outside of the scope of an article
focusing on analysis of mortality data only.

What historic data can show is that both the death rate and the
year-to-year deviation from the “normal” death rate have constantly
decreased since the 1950s. Epidemic events that would have gone almost
unnoticed a few decades ago now clearly stand out from noise.

A second learning of this article is to challenge the believe that the
85+ age group is the most affected by the COVID19 epidemic. In terms of
absolute mortality, this is true that this group is the most affected.
But this group is affected by *any* epidemic or weather condition.
Crudely said, this age group dies anyway any time there is an
exceptional event – every second year or so. If you look at the relative
increase of the death rate, the 85+ age group is twice less affected
than the 75-74 one. If you look at the deviation from the expectation,
we find that such deviation was still usual in 2010 where we have to
look in the 1960s for the other age groups.
