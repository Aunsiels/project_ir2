# Development notes

## Assignment

http://www.da.inf.ethz.ch/files/ir2016/ir2016-ex-09.pdf

Related lectures:
- 
- 

## Progress Michael Nov. 24

Ok my code is up on github. Here a qick summary of what I did so far. We have documents from the tipster dataset. I reused the code from Markus for stop word removal/stemming from project 1 and included it into the classes TipsterStreamSmart and TipsterParseSmart. The class QueryParse is to read the queries.
For creating the frequency index I created the class PersistentFreqIndex which is able to persist the index in a database for later (faster) reuse. I used leveldb as they suggest in the faq section on the project homepage. Creation of the index is fine when using up to 50000 docs from the stream (totally 100000 docs). When I use more docs I run into heap problems. I guess we need to create the index in multiple passes and append it to the db. I will check that at the weekend. 
In terms of term-based model I thought about computing scores like described on slide 14/16 (lecture 4) and then selecting the top 100 docs with the highest score. I started an implementation in the class TermBasedModel.

## Skype discussion Nov. 26

- Michael: already implemented tf-idf according to [Lecture IV](http://www.da.inf.ethz.ch/files/ir2016/ir2016-04.pdf)
- Markus: to get Michael's code running with only 1000 docs in the database.
- Markus: to work on language model [Lecture VI](http://www.da.inf.ethz.ch/files/ir2016/ir2016-06.pdf).
- Markus: Map ID hash map
- Markus: Performance StreamSmart


## splitting zip files

The problem with too many files is that you cannot use bash wild cards, since there will be too many
arguments. `xargs` can be used to solve this problems.

    $ ls | grep PT | xargs zip -X pt.zip
    $ ls | grep PT | xargs rm