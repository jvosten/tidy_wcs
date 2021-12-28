<!-- README.md for WCS git repo -->

# tidy World Color Survey

<!-- PROJECT LOGO -->
<br>
<br>
<p align="center">
<img src="https://www1.icsi.berkeley.edu/wcs/images/jrus-20100531/wcs-chart-4x.png" width="700">  
<p align="center"><small> &#169 <a href="https://www1.icsi.berkeley.edu/wcs/">The World Color Survey</a> </small></center></p>
</p>
<br>
<!-- ABOUT THE PROJECT -->

## About The Project

This project aims to facilitate access to the data of the World Color Survey archive. Initiated by (Berlin & Kay, 1969)<a href="#references">[1]</a>, the WCS (to be found here https://www1.icsi.berkeley.edu/wcs/) was an investigation, in which ››an average of 24 native speakers of each of 110 unwritten languages were asked 
1. to name each of 330 Munsell chips, shown in a constant, random order, and 
2. exposed to a palette of these chips and asked to to pick out the best example(s) ('foci') of the major terms elicited in the naming task.‹‹

The project began as an attempt to combine my interest in Philosophy with a need to practice `R`, a programming language used primarily for analyzing and reporting data. It has four components:
- There is a database (`data/`) in this repo, containing a tidied version of the original [WCS Archive](https://www1.icsi.berkeley.edu/wcs/data.html). The database comes in 3 formats: `csv`, `parquet`and `SQLite`.
- There is an R data package under (https://github.com/jvosten/wcs)[https://github.com/jvosten/wcs], for an easy access in `R`. It also contains two functions to depict each of the tasks 1 & 2 in a single data frame.
- Under `scripts/` there is one file explaining the reasoning behind the tidying process and another for some visualizations of the data.
- Furthermore there is a Shiny App in `app/` to query the data and return information in the form of plots, data tables etc.

I hope you find it interesting and/or useful. Any comments or questions are welcome at <21955156+jvosten@users.noreply.github.com>.

## Getting started

For installing a local copy of the project just follow these simple implementation steps.

### Built with

* [R 4.0.3](https://www.r-project.org/)

### Installation 

1. Clone the repo
   ```sh
   git clone https://github.com/jvosten/tidy_wcs.git
   ```
2. Go to project directory
   ```sh
   cd ~/path/to/project/directory/tidy_wcs
   ```   

## Running the app

1. Go to app directory
   ```sh
   cd ~/path/to/project/directory/tidy_wcs/app
   ```   
2. Call the run file
   ```sh
   bash run.sh
   ```   
3. The WCS app will open in your browser

Alternatively the app is currently hosted under <https://jvosten.shinyapps.io/wcs_app/>

## Interactive script

Furthermore the repo contains two interactive `R` scripts in `scripts/`
- `tidying_wcs_exp.R`: full overview on the reasoning behind the tidying process
- `visualization.R`: visualizations of some aspects of the representability of the data
 which allow for a deeper insight into the project code.

<!-- ## Usage

Use this space to show useful examples of how a project can be used. Additional screenshots, code examples and demos work well in this space. You may also link to more resources.

_For more examples, please refer to the [Documentation](https://example.com)_  -->

## References

 \[1\] Berlin, Brent and Paul Kay. Basic Color Terms: Their Universality and Evolution. Berkeley and Los Angeles. University of California Press, 1969. 

