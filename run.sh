#!/usr/bin/env bash

sbt assembly

jar="target/scala-2.12/mosaic-assembly-1.0.jar"

masterPicture="in.jpg"
outputPicture="out.jpg"

tileSize="64"
inputFolder="/input/"
tilesFolder="/tiles/"
indexFile="/index.csv"

echo "TILING"
java -jar $jar tiling $tileSize $inputFolder $tilesFolder

echo "INDEXING"
java -jar $jar indexing $tilesFolder $indexFile

echo "ASSEMBLING"
java -jar $jar assemble $masterPicture $outputPicture $tileSize $indexFile
