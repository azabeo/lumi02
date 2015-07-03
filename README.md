# extrapolation
extrapolaton factors probabilistic application to BMD

## Installation

There are three alternative ways of installing (some may fail according to your personal set-up, just find the one which works for you), in any case dependencies has to be installed first.

### Installing dependencies

`install.packages("logging")`  
`install.packages("data.table")`
`install.packages("ggplot2")`

### Installation method 1 (straight from GitHub)
`install.packages("https://github.com/azabeo/extrapolation/archive/v0.2_-_visual_not_complete.tar.gz", repo = NULL, type = 'source')`

### Installation method 2 (From my personal repository)
`install.packages("extrapolation", repos = "http://www.dsi.unive.it/~zabeo/R/" )`

### Installation method 3 (From downloaded package)
Download the most updated `tar.gz` package from the [release](https://github.com/azabeo/extrapolation/releases) tab, move it to your R workspace folder, then from R type:  
`install.packages("extrapolation-0.2_-_visual_not_complete.tar.gz", repo = NULL, type = 'source')`

## Usage

The software reads its inputs directly from standard csv files (with headers and comma as separator). Inputs are three tables, one for bmd distributions (the column to be used can be spicified), one for Extrapolaiton Factors to be applied and one for the calculations to be done. The specifics of input tables' formats are reported below. 

To store results into a `res` variable:  
`res <- extrapolation(bmd.boot.col = "chengfang", bmd.file.name = "example.data/bmd.csv", efs.file.name = "example.data/Efs.cheng.csv", calc.file.name = "example.data/calc.cheng.csv", above.threshold = 0)`

To export result into tsv (tab separted files) which can be importted or copied/pasted into Excel (substitute `export/` with the location you want the file to be stored):
`export(res$bmds,"results","export/")`

### Arguments

* *bmd.boot.col* string. the name of the column containing the bmd bootstrapped data
* *bmd.file.name*	string. the location of the input table file for bmds ([see specification below](#bmds))
* *efs.file.name*	string. the location of the input table file of EFs to be applied ([see specification below](#efs))
* *calc.file.name*	string. the location of the input table file of claculations to be applied ([see specification below](#calc))
* *above.threshold*	numeric. If present values less equal to it are discarded from generated bmd distribution

### Returns

List of three tables, the updated bmds table with results, the EFs and calc tables being used

### <a name="bmds"></a> bmds table specification

| id            | bmd       |
|---------------|-----------|
| id of the row | bmd value |

### <a name="efs"></a>EFs table specification

| id            | name           | mu                         | sigma                       | dist.type                                     | is.geom                                                                                       | above.threshold                                              |
|---------------|----------------|----------------------------|-----------------------------|-----------------------------------------------|-----------------------------------------------------------------------------------------------|--------------------------------------------------------------|
| id of the row | name of the EF | means GM if "geom" is TRUE | means GSD if "geom" is TRUE | can be "rnorm"(normal) or "rlnorm"(lognormal) | if "dist.type" is "lognormal" and "is.geom" is TRUE "mu" and "sigma" are trated as GM and GSD | if present, values <= of the "above.threshold" are discarded |

### <a name="calc"></a>Calculation table specification

| id            | operand1                     | operator                            | operand2                      | result                                                              |
|---------------|------------------------------|-------------------------------------|-------------------------------|---------------------------------------------------------------------|
| id of the row | name of first operand column | can be `div`,`mult`,`sum` and `sub` | name of second operand column | name of resulting column (can be used as operand in following rows) |

## Data

* The software works directly with csv tables, exemplificative tables can be found in the `example.data` folder.
* The example data can also be directly loaded in variables to look at them from R by
    * `bmds <- readRDS("data/bmds.rds")` 
    * `efs <- readRDS("data/efs.rds")`
    * `calc <- readRDS("data/calc.rds")`
