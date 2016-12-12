#!/bin/bash
# Information Retrieval, Project 2. Group 11 (Merki, Romero, Greiner).
# Everything has to be zipped to a directory called 'code'
#We ask you to send us one zip archive named exactly ir-practical-2016-2-[groupid].zip containing one folder and 4 files:

# create the readme.pdf
# pandoc -f markdown_github README.md -o README.pdf

# Create the zip package by first zipping the relevant folders, then unzipping them to 'code', then zipping again.
rm ir-practical-2016-2-11.zip

# create the folder sources which contains build.sbt and src
zip -rX sources.zip build.sbt src wordnet
unzip -X sources.zip -d sources
rm sources.zip
zip -rX ir-practical-2016-2-11.zip sources README.md ranking-t-11.run ranking-n-11.run
rm -rf sources

