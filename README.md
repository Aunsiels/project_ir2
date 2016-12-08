# ETH Information Retrieval 2016, Project 2, Group 11

Michael Merki, Julien Romero, Markus Greiner

December 12, 2016

## Installation

Provided is the zip file `ir-practical-2016-2-11.zip`. Unzip it:

    $ unzip ir-practical-2016-2-11.zip
    
This creates:
- this `README.md` file
- the directory `sources` which contains `build.sbt` and `src`
- teh result filea `ranking-t-11.run` and `ranking-n-11.run`

## Running
To run:

    $ sbt "run-main Main <path-to-data-folder>"
    
The `<path-to-data-folder>` must contain the directories `train`, `test`, and `validation`.

It is also possible to give options to influence the iterations and learning rates of the linear regression classifier.
These options are called:

- `ITERATION=<nof-iterations-integer>`
- `LEARNING=<learning-rate-double>`
- `SKIP=(BAYES|LINREG|SVM)+`

It can be run like this:

    $ sbt "run-main <path-to-data-folder> ITERATION=10000 LEARNING=0.001 SKIP=BAYES,SVM"


## Results
Upon running the program, it will

- *Naive Bayes Classifier*: train and generate list of tested documents and their codes
- *Logistic Regression Classifier*: train and generate the list
- *SVM*: train and generate the list.

The result files are called `ir-project-2016-1-11-[nb|lr|lsvm].txt` and they are located under `labelingtestdocs`

The project report is under `ir-2016-1-report-11.pdf` 


