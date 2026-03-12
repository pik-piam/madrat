# Package index

## Core Workflow

Main functions to call other pipeline steps

- [`calcOutput()`](calcOutput.md) : calcOutput
- [`downloadSource()`](downloadSource.md) : downloadSource
- [`readSource()`](readSource.md) : readSource
- [`retrieveData()`](retrieveData.md) : retrieveData
- [`returnCalcOutput()`](returnCalcOutput.md) : Construct a list with
  the information required for calc-functions to return

## Available Functions

Discover available calc, read, and tool functions

- [`getCalculations()`](getCalculations.md) : getCalculations
- [`getLocation()`](getLocation.md) : getLocation
- [`getSources()`](getSources.md) : getSources
- [`installedMadratUniverse()`](installedMadratUniverse.md) :
  installedMadratUniverse

## Configuration

Configuration management for the madrat framework

- [`getConfig()`](getConfig.md) : getConfig
- [`madratAttach()`](madratAttach.md)
  [`madratDetach()`](madratAttach.md) : madratAttach / madratDetach
- [`setConfig()`](setConfig.md) [`localConfig()`](setConfig.md) :
  setConfig

## Cache Management

Functions for handling cached data

- [`cacheCleanup()`](cacheCleanup.md) : cacheCleanup
- [`cacheCopy()`](cacheCopy.md) : cacheCopy

## Dependency Graph

Analyze function dependencies in the madrat universe

- [`getDependencies()`](getDependencies.md) : getDependencies
- [`getMadratGraph()`](getMadratGraph.md) : getMadratGraph
- [`getMadratInfo()`](getMadratInfo.md) : getMadratInfo
- [`visualizeDependencies()`](visualizeDependencies.md) :
  visualizeDependencies

## Mapping

Tools for working with mappings

- [`addMapping()`](addMapping.md) : addMapping
- [`toolConvertMapping()`](toolConvertMapping.md) : Tool: ConvertMapping
- [`toolGetMapping()`](toolGetMapping.md) : Tool: GetMapping

## Aggregation

Spatial aggregation and disaggregation

- [`pucAggregate()`](pucAggregate.md) : pucAggregate
- [`toolAggregate()`](toolAggregate.md) : toolAggregate

## Tools: Countries

Country and ISO code handling

- [`getISOlist()`](getISOlist.md) : get official ISO list
- [`toolCountry2isocode()`](toolCountry2isocode.md) :
  toolCountry2isocode
- [`toolCountryFill()`](toolCountryFill.md) : Tool: CountryFill
- [`toolFillWithRegionAvg()`](toolFillWithRegionAvg.md) : Tool:
  FillWithRegionAvg
- [`toolISOhistorical()`](toolISOhistorical.md) : Tool: ISOhistorical

## Tools: Temporal Processing

Time series manipulation and interpolation

- [`toolFillYears()`](toolFillYears.md) : toolFillYears
- [`toolTimeAverage()`](toolTimeAverage.md) : toolTimeAverage
- [`toolTimeSpline()`](toolTimeSpline.md) : Smooth a magclass time
  series with optional anchor years

## Tools: Data Manipulation

General data transformation tools

- [`toolConditionalReplace()`](toolConditionalReplace.md) :
  toolConditionalReplace
- [`toolFixWeight()`](toolFixWeight.md) : toolFixWeight
- [`toolNAreplace()`](toolNAreplace.md) : Tool: NA replace
- [`toolOrderCells()`](toolOrderCells.md) : toolOrderCells
- [`toolXlargest()`](toolXlargest.md) : toolXlargest

## Tools: Source Downloading

Helpers for downloading source data

- [`metadataGFZ()`](metadataGFZ.md) : metadataGFZ
- [`toolManualDownload()`](toolManualDownload.md) : Tool: ManualDownload

## Tools: Subtype Handling

Tools for handling subtypes in read functions

- [`toolSplitSubtype()`](toolSplitSubtype.md) : Tool: SplitSubtype
- [`toolSubtypeSelect()`](toolSubtypeSelect.md) : Tool: SubtypeSelect

## Madrat Messages

Madrat-specific messaging system stored in cache

- [`getMadratMessage()`](getMadratMessage.md) : getMadratMessage
- [`putMadratMessage()`](putMadratMessage.md) : putMadratMessage
- [`resetMadratMessages()`](resetMadratMessages.md) :
  resetMadratMessages
- [`vcat()`](vcat.md) : Tool: Verbosity Cat

## Metadata Comments

Handling metadata comments on magclass objects

- [`getFromComment()`](getFromComment.md) : getFromComment

## Source Redirection

Redirect source data to alternative locations

- [`getLinkFunction()`](getLinkFunction.md) : getLinkFunction
- [`redirectSource()`](redirectSource.md) : redirectSource

## Code Labeling

Region code labeling utilities

- [`regionscode()`](regionscode.md) : Tool: regionscode
- [`toolCodeLabels()`](toolCodeLabels.md) : Tool: CodeLabels

## Validation

Data validation and comparison tools

- [`compareData()`](compareData.md) : compareData
- [`compareMadratOutputs()`](compareMadratOutputs.md) : Compare a madrat
  function's output with and without your changes
- [`toolCompareStatusLogs()`](toolCompareStatusLogs.md) :
  toolCompareStatusLogs

## Analysis

Performance analysis tools

- [`findBottlenecks()`](findBottlenecks.md) : findBottlenecks

## Example Functions

Example implementations for reference

- [`fullEXAMPLE()`](fullEXAMPLE.md) : fullExample
- [`readTau()`](readTau.md) : Read Tau
- [`convertTau()`](convertTau.md) : Convert Tau
- [`calcTauTotal()`](calcTauTotal.md) : Calculate total tau
- [`redirectTau()`](redirectTau.md) : redirectTau

## Internal

Internal functions

- [`madrat-package`](madrat-package.md) [`madrat`](madrat-package.md) :
  May All Data be Reproducible And Transparent (madrat) \*
