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
| `PersistentFreqIndex` |`index += term > FreqPosting(id, freq) :: index.getOrElse(term, List[FreqPosting]())`| 100'000 | - | - secs |
 
 In general, here is a good cheat-sheet about the 
 [performance of collections](http://docs.scala-lang.org/overviews/collections/performance-characteristics.html).
 
## Memory usage

see http://alvinalexander.com/scala/how-to-use-stream-class-lazy-list-scala-cookbook:

However, be careful with methods that aren’t transformers. Calls to the following strict methods are evaluated 
immediately and can easily cause `java.lang.OutOfMemoryError` errors:
- stream.max
- stream.size
- stream.sum`

