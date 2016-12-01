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
    
Extracting a single file from zip:

    $ unzip -l documents.zip | grep DOE | more
    $ unzip -l documents.zip DOE1-01-0128 > DOE1-01-0128.xml
    


## Topics

The Wall Street Journal articles have topics in the `<NS>` tag. Maybe we can use this.

    <?xml version='1.0' encoding='UTF-8'?>
    <DOC>
        <DOCNO>WSJ911015-0039</DOCNO>
        <DOCID>911015-0039.</DOCID>
        <HL>Labor Letter: A Special News Report on People And Their Jobs in Offices, Fields and Factories ---- By
            Christopher Conte
        </HL>
        <DATE>10/15/91</DATE>
        <SO>WALL STREET JOURNAL (J), PAGE A1</SO>
        <CO>BAC LABOR SPC</CO>
        <MS>FINANCIAL (FIN)INDUSTRIAL (IDU)CONSUMER NON-CYCLICAL (NCY)</MS>
        <IN>MAJOR MONEY CENTER BANKS (BAN)ALL REGIONAL BANKS (BAR)WESTERN U.S. BANKS (BAW)ALL BANKS, BANKING NEWS AND ISSUES
            (BNK)CDS, INTEREST RATES, COMMERCIAL PAPER (FIN)HEALTH CARE PROVIDERS, MEDICINE, DENTISTRY (HEA)INDUSTRIAL
            andamp; COMMERCIAL SERVICES, LEASING, CLEANING (ICS)LAW AND LEGAL AFFAIRS (LAW)INDUSTRIAL AND COMMERCIAL
            SERVICES (SVC)TENDER OFFERS, MERGERS, ACQUISITIONS (TNM)
        </IN>
        <NS>LABOR, UNIONS, STRIKES, WAGES, RECRUITMENT (LAB)LAW andamp; LEGAL ISSUES AND LEGISLATION (LAW)MANAGEMENT ISSUES
            (MNT)ACQUISITIONS andamp; MERGERS, TAKEOVERS, BOARD BATTLES (TNM)
        </NS>
            <GV>FEDERAL GOVERNMENT (FDL)LABOR DEPARTMENT (LBR)</GV>
            <RE>CALIFORNIA (CA)NORTH AMERICA (NME)PACIFIC RIM (PRM)UNITED STATES (US)</RE>
            <LP>PRECEDENT PENDING: Employee involvement programs facelegal hurdles. Dozens of cases heading toward the National
                LaborRelations Board will test whether self-directed work teams,quality circles and other joint labor-management
                groups areallowed under federal labor law. Such groups were designed toincrease worker participation and improve
                product quality,but some believe they run afoul of workers' right to formindependent labor organizations.
            </LP>
            <TEXT>A strict interpretation of the National Labor RelationsAct could force employers to disband 90% of existing
                groups,warns Washington attorney Robert Hunter. A case involving&quot;action committees&quot; established by


## Time comparison

All times measured on Markus's MacBook Air with 4GB max heap.

  Class |  Task | # Docs | TipsterStream | SmartStream |
|-----|-----|-------:|-------:|--------:|
| `PerformanceParseSmart` |`map(ID, name)`| 100'000 | 77.49 sec | 43.95 sec |
| `PerformanceParseSmart` |`map(ID, name)`| 100'000 | 56.83 sec | 38.63 sec |
| `PerformanceParseSmart` |`map(ID, tokens.length)` | 100'000 | 111.47 sec | 2874.48 secs |
| `TipsterStreamSmart` |`foreach(_.termFrequencies.headOption)` | 50'000 | -  | 320 secs |
| `TipsterStreamSmart` |`foreach(_.termFrequencies.headOption)` | 100'000 | - | 1336.06 secs |
| `PersistentFreqIndex` |`index += term > FreqPosting(id, freq) :: index.getOrElse(term, List[FreqPosting]())`| 100'000 | - | 5336.33 secs |
| `PersistentFreqIndex` |`index += term > FreqPosting(id, freq) :: index.getOrElse(term, List[FreqPosting]())` improved | 50'000 | - | 430.12 secs 477428 words |
 
 The result of the last indexing:
 
    (affairswa,List(FreqPosting(2063978077,2), FreqPosting(1402016186,2)))
 
 In general, here is a good cheat-sheet about the 
 [performance of collections](http://docs.scala-lang.org/overviews/collections/performance-characteristics.html).
 
 
    27500 APdocuments, completed in 430.12 secs
                   (affairswa,List(FreqPosting(2063978077,AP8912080202,2), FreqPosting(1402016186,AP8908210201,2))), Some(AP8912080202)
                   size: 477428

              
## Test 100'000
numbers=false, stopwords=false, stemming=false, chopping=-1
total tokens: 65793466, term freq: 2133425, (pastviolations,2) in 253.59 secs


## Testing query

Testing was done with stemming, but no chopping.

These terms cannot be found: `fine-diamet`, `alternative/renew`, `merit-pai`. They all contain dash or slashes.

    query 089 List(downstream, invest, opec, member, state) ****
                   67 1935 537 6393 14013
    query 080 List(presidenti, candid, platform) ****
                   2112 1786 617
    query 074 List(conflict, polici) ****
                   1103 4201
    query 077 List(poach) ****
                   112
    query 062 List(militari, coup, etat) ****
                   5066 937 39
    query 056 List(prime, lend, rate, move, predict) ****
                   2094 554 3814 5065 2101
    query 083 List(measur, protect, atmospher) ****
                   4157 3283 917
    query 071 List(border, incurs) ****
                   2021 177
    query 065 List(inform, retriev, system) ****
                   4123 198 6782
    query 086 List(bank, failur) ****
                   3451 1293
    query 059 List(weather, relat, fatal) ****
                   1309 3244 494
    query 053 List(leverag, buyout) ****
                   455 578
    query 068 List(health, hazard, fine-diamet, fiber) ****
                   2247 584 0 427
    query 079 List(frg, polit, parti, posit) ****
                   23 4966 4782 3451
    query 085 List(offici, corrupt) ****
                   11366 1039
    query 070 List(surrog, motherhood) ****
                   144 36
    query 073 List(demograph, shift, nation, boundari) ****
                   112 1045 13020 277
    query 067 List(polit, motiv, civil, disturb) ****
                   4966 484 2264 677
    query 088 List(crude, oil, price, trend) ****
                   908 2906 3955 922
    query 076 List(constitut, origin, intent) ****
                   1644 1498 949
    query 082 List(genet, engin) ****
                   382 1822
    query 061 List(isra, role, iran-contra, affair) ****
                   1217 2379 573 1990
    query 055 List(insid, trade) ****
                   1604 3901
    query 064 List(hostage-tak) ****
                   127
    query 058 List(rail, strike) ****
                   390 2137
    query 052 List(south, african, sanction) ****
                   4289 905 775
    query 081 List(financi, crunch, televangelist, wake, ptl, scandal) ****
                   2333 58 29 376 352 887
    query 090 List(data, proven, reserv, oil, natur, ga, produc) ****
                   2750 287 1804 2906 2494 2590 3809
    query 063 List(machin, translat) ****
                   1029 326
    query 078 List(greenpeac) ****
                   206
    query 084 List(alternative/renew, energi, plant, equip, instal) ****
                   0 3783 2921 1970 1202
    query 051 List(airbu, subsidi) ****
                   139 357
    query 087 List(crimin, action, offic, fail, financi, institut) ****
                   1515 3897 7148 2853 2333 2444
    query 066 List(natur, languag, process) ****
                   2494 761 3830
    query 072 List(demograph, shift) ****
                   112 1045
    query 060 List(merit-pai, senior) ****
                   0 1874
    query 069 List(attempt, reviv, salt, treati) ****
                   3137 340 526 1183
    query 075 List(autom) ****
                   346
    query 054 List(satellit, launch, contract) ****
                   651 1688 2114
    query 057 List(mci) ****
                   92
    survived


## Memory usage

see http://alvinalexander.com/scala/how-to-use-stream-class-lazy-list-scala-cookbook:

However, be careful with methods that aren’t transformers. Calls to the following strict methods are evaluated 
immediately and can easily cause `java.lang.OutOfMemoryError` errors:
- stream.max
- stream.size
- stream.sum`

