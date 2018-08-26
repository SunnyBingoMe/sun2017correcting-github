#### env
Sys.setlocale("LC_CTYPE","Chinese")
#Sys.setlocale("LC_CTYPE","English")
Sys.setenv(TZ='Europe/Stockholm')
## test Chinese: print('测试，汉语。  ' )

#### libraries
library(plyr) ## for adply()
require(shiny); library(ggplot2)
require(rCharts)
require(markdown)
require(DT)
library(reshape2) # for 'device list' panel
library(dygraphs); library(dplyr); library(xts); library(magrittr)
library(UsingR) # for variable galton in 'test' panel
#require(morris)
options(RCHART_WIDTH = '50%', RCHART_HEIGHT = '50%')
####　end libraries

#### variables
debugLevel = 4 # 0:variable; 1:detail (in a function), 2:info (big step, or big loop level), 3:warning, 4:error
accidentAnalysisDayOfWeek = NULL
accidentAnalysisMonthOfYear = NULL
accidentAnalysisHourOfDay = NULL
reactiveValueList = reactiveValues(
      test = 'test'
    ,lastSelectedIndex = 60
)
nonReactiveValueList = list(
    accidentDurationPosixctList = NULL
)
#### end variables

#### non-reactive functions
debug = function(message, variableToDisplay = NULL, level = NULL) {
    if((not.null(variableToDisplay)) & (is.null(level))) {
        level = 0
    }
    if(is.null(level)) {
        level = 1
    }
    if(level >= debugLevel) {
        print(message)
        logT <<- append(logT, list(variableToDisplay))
        if(not.null(variableToDisplay)) {
            print(variableToDisplay)
        }
    }
}

updateAccidentInformation = function(lastSelectedIndex, directionString, occurTimePosixct, endTimePosixct){
    debug('in F updateAccidentInformation')
    accidentDf[lastSelectedIndex,]$ANALYZED_DIRECTION <<- directionString
    accidentDf[lastSelectedIndex,]$ANALYZED_OCCUR_TIME <<- occurTimePosixct
    accidentDf[lastSelectedIndex,]$ANALYZED_END_TIME <<- endTimePosixct
}

findAccidentDurationPosixctList = function(mainDataDf, input, possibleOccurTimePosixct){
    debug('in F findAccidentDurationPosixctList')
    deviceId = attr(mainDataDf, 'deviceId')
    occurTimePosixct = subset(
        mainDataDf[order(-mainDataDf$DIFF_HOUR_OUTLIER_UPDATE), ]
        , DIFF_NUM_TOTAL < 0 & COUNT_TIME >= possibleOccurTimePosixct - 60*20 & COUNT_TIME <= possibleOccurTimePosixct + 60*20
    )[1, ]$COUNT_TIME
    if(is.na(occurTimePosixct)) {
        occurTimePosixct = mainDataDf[1, ]$COUNT_TIME
    }
    endTimePosixct = subset(
        mainDataDf
        , DIFF_NUM_TOTAL > 0 & COUNT_TIME >= occurTimePosixct + 60*5 & DIFF_HOUR_OUTLIER_UPDATE >= 1 * input$enlargeFactor
    )[1, ]$COUNT_TIME
    if(is.na(endTimePosixct)) {
        endTimePosixct = subset(
            mainDataDf[order(-mainDataDf$DIFF_HOUR_OUTLIER_UPDATE), ]
            , DIFF_NUM_TOTAL > 0 & COUNT_TIME >= occurTimePosixct + 60*5
        )[1, ]$COUNT_TIME
    }
    accidentCentroid = colMeans(subset(
        mainDataDf
        , COUNT_TIME > occurTimePosixct + 60 & COUNT_TIME < endTimePosixct - 60
        , select = c(NUM_TOTAL, AVR_SPEED)
    ))
    nonAccidentCentroid = colMeans(subset(
        mainDataDf
        , COUNT_TIME < occurTimePosixct - 60 | COUNT_TIME > endTimePosixct + 60
        , select = c(NUM_TOTAL, AVR_SPEED)
    ))
    diffCentroid = data.frame(t(accidentCentroid - nonAccidentCentroid))
    diffCentroid$COUNT_TIME = occurTimePosixct
    diffCentroid$deviceId = deviceId
    t = getDiffMDistAndThresholdUpdatable(diffCentroid, input = input)
    accidentIndicator = t$DIFF_MDIST_UPDATE / t$DIFF_THRESHOLD_UPDATE
    accidentIndicator = signif(accidentIndicator, 3)
    t = list(occurTime = occurTimePosixct, endTime = endTimePosixct, accidentIndicator = accidentIndicator)
    debug('in F findAccidentDurationPosixctList', accidentIndicator)
    return(t)
}

getMDistAndThreshold = function(oneRow, input = input){
    debug('in F getMDistAndThreshold')
    #oneRow = as.data.frame(t(oneRow))
    #print(oneRow)
    deviceId = oneRow$deviceId
    hourIndex = (as.POSIXlt(oneRow$COUNT_TIME)$hour + 1)
    data2D = oneRow[, c('NUM_TOTAL', 'AVR_SPEED')]
    debug('in F getMDistAndThreshold: data2D')
    debug('in F getMDistAndThreshold', data2D)
    debug(paste('in F getMDistAndThreshold: hourIndex', hourIndex))
    mdist = signif(mahalanobis(
        data2D
        , mResultList.aggregated[[deviceId]][[hourIndex]][['center']]
        , mResultList.aggregated[[deviceId]][[hourIndex]][['cov']]
    ), 3)
    threshold = signif(mResultList.aggregated[[deviceId]][[hourIndex]][['threshold']], 3)
    if(is.infinite(threshold)) {
        threshold = signif(maxThreshold.allHours.perMonth, 3)
    }
    hourOutlier = min(signif(mdist / threshold * input$enlargeFactor, 3), 150)
    debug('in F getMDistAndThreshold: paste(mdist, threshold)')
    debug('in F getMDistAndThreshold', paste(mdist, threshold))
    t = as.data.frame(t(c(mdist, threshold, hourOutlier)))
    colnames(t) = c('MDIST', 'THRESHOLD', 'HOUR_OUTLIER')
    #cat('\n', data2D, mResultList.aggregated[[deviceId]][[hourIndex]][['center']], mResultList.aggregated[[deviceId]][[hourIndex]][['cov']], mdist, threshold)
    debug('ut F getMDistAndThreshold')
    return(t)
}

getMDistAndThresholdUpdatable = function(oneRow, input = input){
    debug('in F getMDistAndThresholdUpdatable')
    #oneRow = as.data.frame(t(oneRow))
    debug('in F getMDistAndThresholdUpdatable', oneRow)
    deviceId = oneRow$deviceId
    hour = as.POSIXlt(oneRow$COUNT_TIME)$hour
    year = as.POSIXlt(oneRow$COUNT_TIME)$year + 1900
    month = as.POSIXlt(oneRow$COUNT_TIME)$mon + 1
    hourString = sprintf('h%02d', hour)
    yearAndMonthString = sprintf('y%04dm%02d', year, month)
    hourIndex = (hour + 1)

    #cat('\n', deviceId, hourString, yearAndMonthString)

    data2D = oneRow[, c('NUM_TOTAL', 'AVR_SPEED')]
    #print(data2D)
    mdist = signif(mahalanobis(
        data2D
        , mResultUpdatableList.aggregated.perMonth[[deviceId]][[hourString]][[yearAndMonthString]][['center']]
        , mResultUpdatableList.aggregated.perMonth[[deviceId]][[hourString]][[yearAndMonthString]][['cov']]
    ), 3)
    threshold = signif(mResultUpdatableList.aggregated.perMonth[[deviceId]][[hourString]][[yearAndMonthString]][['threshold']], 3)
    if(is.infinite(threshold)) {
        threshold = signif(maxThreshold.allHours.perMonth, 3)
    }
    hourOutlier = min(signif(mdist / threshold * input$enlargeFactor, 3), 150)
    #cat(paste(mdist, threshold))
    t = as.data.frame(t(c(mdist, threshold, hourOutlier)))
    colnames(t) = c('MDIST_UPDATE', 'THRESHOLD_UPDATE', 'HOUR_OUTLIER_UPDATE')
    #cat('\n'); print(data2D); cat(' center ', mResultUpdatableList.aggregated.perMonth[[deviceId]][[hourString]][[yearAndMonthString]][['center']]); cat(' cov ', mResultUpdatableList.aggregated.perMonth[[deviceId]][[hourString]][[yearAndMonthString]][['cov']], mdist, threshold)
    return(t)
}

getDiffMDistAndThreshold = function(oneRow, input = input){
    debug('in F getDiffMDistAndThreshold.')
    #oneRow = as.data.frame(t(oneRow))
    debug('in F getDiffMDistAndThreshold.', oneRow)
    deviceId = oneRow$deviceId
    hourIndex = (as.POSIXlt(oneRow$COUNT_TIME)$hour + 1)
    data2D = oneRow[, c('NUM_TOTAL', 'AVR_SPEED')]
    #print(data2D)
    #cat('\n', deviceId, hourIndex, '\n')
    mdist = signif(mahalanobis(
        data2D
        , mResultList.diff.aggregated[[deviceId]][[hourIndex]][['center']]
        , mResultList.diff.aggregated[[deviceId]][[hourIndex]][['cov']]
    ), 3)
    threshold = signif(mResultList.diff.aggregated[[deviceId]][[hourIndex]][['threshold']], 3)
    if(is.infinite(threshold)) {
        threshold = signif(maxThreshold.diff.allHours.perMonth, 3)
    }
    hourOutlier = min(signif(mdist / threshold * input$enlargeFactor, 3), 150)
    #cat('\nresults: ', paste(mdist, threshold, hourOutlier), '\n')
    t = as.data.frame(t(c(mdist, threshold, hourOutlier)))
    colnames(t) = c('DIFF_MDIST', 'DIFF_THRESHOLD', 'DIFF_HOUR_OUTLIER')
    #cat('\n', data2D, mResultList.aggregated[[deviceId]][[hourIndex]][['center']], mResultList.aggregated[[deviceId]][[hourIndex]][['cov']], mdist, threshold)
    #print(t)
    return(t)
}

getDiffMDistAndThresholdUpdatable = function(oneRow, input){
    debug('in F getDiffMDistAndThresholdUpdatable')
    #oneRow = as.data.frame(t(oneRow))
    debug('in F getDiffMDistAndThresholdUpdatable', oneRow)
    deviceId = oneRow$deviceId
    hour = as.POSIXlt(oneRow$COUNT_TIME)$hour
    year = as.POSIXlt(oneRow$COUNT_TIME)$year + 1900
    month = as.POSIXlt(oneRow$COUNT_TIME)$mon + 1
    hourString = sprintf('h%02d', hour)
    yearAndMonthString = sprintf('y%04dm%02d', year, month)
    hourIndex = (hour + 1)

    #cat('\n', deviceId, hourString, yearAndMonthString)

    data2D = oneRow[, c('NUM_TOTAL', 'AVR_SPEED')]
    #print(data2D)
    mdist = signif(mahalanobis(
        data2D
        , mResultUpdatableList.diff.aggregated.perMonth[[deviceId]][[hourString]][[yearAndMonthString]][['center']]
        , mResultUpdatableList.diff.aggregated.perMonth[[deviceId]][[hourString]][[yearAndMonthString]][['cov']]
    ), 3)
    threshold = signif(mResultUpdatableList.diff.aggregated.perMonth[[deviceId]][[hourString]][[yearAndMonthString]][['threshold']], 3)
    if(is.infinite(threshold)) {
        threshold = signif(maxThreshold.diff.allHours.perMonth, 3)
    }
    hourOutlier = min(signif(mdist / threshold * input$enlargeFactor, 3), 150)
    #cat(paste(mdist, threshold))
    t = as.data.frame(t(c(mdist, threshold, hourOutlier)))
    colnames(t) = c('DIFF_MDIST_UPDATE', 'DIFF_THRESHOLD_UPDATE', 'DIFF_HOUR_OUTLIER_UPDATE')
    #cat('\n'); print(data2D); cat(' center ', mResultUpdatableList.aggregated.perMonth[[deviceId]][[hourString]][[yearAndMonthString]][['center']]); cat(' cov ', mResultUpdatableList.aggregated.perMonth[[deviceId]][[hourString]][[yearAndMonthString]][['cov']], mdist, threshold)
    return(t)
}

getAccidentAnalysisTimelyResult = function(nBy){
    debug('in F getAccidentAnalysisTimelyResult')
    if( # if not calculated
        ((nBy == 'hour')  & is.null(accidentAnalysisHourOfDay))
        |((nBy == 'wday') & is.null(accidentAnalysisDayOfWeek))
        |((nBy == 'mon')  & is.null(accidentAnalysisMonthOfYear))
    ){
        mainDataDf = NORMAL_AND_SIMPLE.timeAndKsMeterOnly
        mainDataDf = as.data.frame(as.list(aggregate(
            cbind(ACCIDENT_OCCUR_TIME)
            ~ as.POSIXlt(mainDataDf$ACCIDENT_OCCUR_TIME)[[nBy]]
            ,data = mainDataDf
            ,FUN=function(mainDataDf) c(length = length(mainDataDf))
        )))
        colnames(mainDataDf) = c('x', 'COUNT')
        if(nBy == 'hour'){
            accidentAnalysisHourOfDay <<- mainDataDf
        }else if(nBy == 'wday'){
            mainDataDf$x = c('SUN','MON','TUE','WEN','THU','FRI','SAT')
            accidentAnalysisDayOfWeek <<- mainDataDf
        }else if(nBy == 'mon'){
            mainDataDf$x = mainDataDf$x + 1
            accidentAnalysisMonthOfYear <<- mainDataDf
        }else{
            cat(sprintf('ERR unexpected nBy timely input: %s.\n', as.character(nBy)))
        }
    }else{ # else: already calculated
        if(nBy == 'hour'){
            return(accidentAnalysisHourOfDay)
        }else if(nBy == 'wday'){
            return(accidentAnalysisDayOfWeek)
        }else if(nBy == 'mon'){
            return(accidentAnalysisMonthOfYear)
        }else{
            return(NULL)
        }
    }
}
higherOrLower = function(tInt){
    if(tInt < 0){
        return('lower')
    }else{
        return('higher')
    }
}
findFirstAvailableDeviceIndexInSubDf = function(deviceList.sub.t, occurTime, endTime) {
    debug('in F findFirstAvailableDeviceIndexInSubDf')
    deviceListIndexT = 0
    dataOk = FALSE
    while(!dataOk) {
        deviceListIndexT = deviceListIndexT + 1
        deviceIdT = deviceList.sub.t[deviceListIndexT,]$DEVICE_NBR
        t = subset(
            F_FLOW.asDevice[[deviceIdT]]
            , COUNT_TIME >= occurTime
            & COUNT_TIME <= endTime
        )
        dataOk = (nrow(t) >= 24)
    }
    return(deviceListIndexT)
}
#### end non-reactive functions

shinyServer(function(input, output, session) {
    #load('./t.Rdata')
    #mainDataDf = t.small
    #mainDataDf = transform(mainDataDf, time = as.character(time)) # morris.js accept epoc int as milliseconds, or string. see http://morrisjs.github.io/morris.js/lines.html >> xkey.
    #mainDataDf = mainDataDf[130:180,]

    output$accidentPlotMetricArray = renderPrint({input$accidentPlotMetricArray})
    output$enlargeFactor = renderPrint({input$enlargeFactor})
    output$test = renderPrint({input$ksMeterInputText})

    observeEvent(input$device1AccidentConfirm, {
        directionString = 'K'
        occurTimePosixct = as.POSIXct(
            sprintf('%s %02d:%02d:00'
                    , input$device1AccidentOccurTimeDateInput
                    , as.integer(input$device1AccidentOccurTimeHourSelectInput)
                    , as.integer(input$device1AccidentOccurTimeMinuteSelectInput)
            )
        )
        endTimePosixct = as.POSIXct(
            sprintf('%s %02d:%02d:00'
                    , input$device1AccidentEndTimeDateInput
                    , as.integer(input$device1AccidentEndTimeHourSelectInput)
                    , as.integer(input$device1AccidentEndTimeMinuteSelectInput)
            )
        )
        updateAccidentInformation(reactiveValueList$lastSelectedIndex, directionString, occurTimePosixct, endTimePosixct)
        session$sendCustomMessage(type = 'testmessage', message = paste('Accident infomation updated. Direction to: ', directionString, '.\n', occurTimePosixct, ' ~ ', endTimePosixct))
        #session$sendCustomMessage(type = 'testmessage', message = list(a = 1, b = input$device1AccidentConfirm)) # list --> json
    })
    observeEvent(input$device2AccidentConfirm, {
        directionString = 'K'
        occurTimePosixct = as.POSIXct(
            sprintf('%s %02d:%02d:00'
                    , input$device2AccidentOccurTimeDateInput
                    , as.integer(input$device2AccidentOccurTimeHourSelectInput)
                    , as.integer(input$device2AccidentOccurTimeMinuteSelectInput)
            )
        )
        endTimePosixct = as.POSIXct(
            sprintf('%s %02d:%02d:00'
                    , input$device2AccidentEndTimeDateInput
                    , as.integer(input$device2AccidentEndTimeHourSelectInput)
                    , as.integer(input$device2AccidentEndTimeMinuteSelectInput)
            )
        )
        updateAccidentInformation(reactiveValueList$lastSelectedIndex, directionString, occurTimePosixct, endTimePosixct)
        session$sendCustomMessage(type = 'testmessage', message = paste('Accident infomation updated. Direction to: ', directionString, '.\n', occurTimePosixct, ' ~ ', endTimePosixct))
        #session$sendCustomMessage(type = 'testmessage', message = list(a = 1, b = input$device1AccidentConfirm)) # list --> json
    })
    observeEvent(input$device3AccidentConfirm, {
        directionString = 'S'
        occurTimePosixct = as.POSIXct(
            sprintf('%s %02d:%02d:00'
                    , input$device3AccidentOccurTimeDateInput
                    , as.integer(input$device3AccidentOccurTimeHourSelectInput)
                    , as.integer(input$device3AccidentOccurTimeMinuteSelectInput)
            )
        )
        endTimePosixct = as.POSIXct(
            sprintf('%s %02d:%02d:00'
                    , input$device3AccidentEndTimeDateInput
                    , as.integer(input$device3AccidentEndTimeHourSelectInput)
                    , as.integer(input$device3AccidentEndTimeMinuteSelectInput)
            )
        )
        updateAccidentInformation(reactiveValueList$lastSelectedIndex, directionString, occurTimePosixct, endTimePosixct)
        session$sendCustomMessage(type = 'testmessage', message = paste('Accident infomation updated. Direction to: ', directionString, '.\n', occurTimePosixct, ' ~ ', endTimePosixct))
        #session$sendCustomMessage(type = 'testmessage', message = list(a = 1, b = input$device1AccidentConfirm)) # list --> json
    })
    observeEvent(input$device4AccidentConfirm, { # redundant compared with observeEvent(input$device1AccidentConfirm)
        directionString = 'S'
        occurTimePosixct = as.POSIXct(
            sprintf('%s %02d:%02d:00'
                    , input$device4AccidentOccurTimeDateInput
                    , as.integer(input$device4AccidentOccurTimeHourSelectInput)
                    , as.integer(input$device4AccidentOccurTimeMinuteSelectInput)
            )
        )
        endTimePosixct = as.POSIXct(
            sprintf('%s %02d:%02d:00'
                    , input$device4AccidentEndTimeDateInput
                    , as.integer(input$device4AccidentEndTimeHourSelectInput)
                    , as.integer(input$device4AccidentEndTimeMinuteSelectInput)
            )
        )
        updateAccidentInformation(reactiveValueList$lastSelectedIndex, directionString, occurTimePosixct, endTimePosixct)
        session$sendCustomMessage(type = 'testmessage', message = paste('Accident infomation updated. Direction to: ', directionString, '.\n', occurTimePosixct, ' ~ ', endTimePosixct))
        #session$sendCustomMessage(type = 'testmessage', message = list(a = 1, b = input$device1AccidentConfirm)) # list --> json
    })

    observeEvent(input$userInputConfirmButton, {
        reactiveValueList$lastSelectedIndex = isolate(lastSelectedIndexF())
    })
    observe({
        debug('==== lastSelectedIndex ==== : ', reactiveValueList$lastSelectedIndex, 2)
    })
    observeEvent(input$previousAccidentButton, {
        reactiveValueList$lastSelectedIndex = reactiveValueList$lastSelectedIndex - 1
        reactiveValueList$lastSelectedIndex = max(reactiveValueList$lastSelectedIndex, 1)
        #debugT <<- reactiveValueList$lastSelectedIndex
    })
    observeEvent(input$nextAccidentButton, {
        reactiveValueList$lastSelectedIndex = reactiveValueList$lastSelectedIndex + 1
        reactiveValueList$lastSelectedIndex = min(reactiveValueList$lastSelectedIndex, nrow(accidentDf))
        #debugT <<- reactiveValueList$lastSelectedIndex
    })

    ## observers: # ref http://www.inside-r.org/packages/cran/shiny/docs/updateDateInput
    observe({ # device1
        accidentDurationPosixctList = reactiveValueList$device1AccidentDurationPosixctList
        if(is.null(accidentDurationPosixctList)) return()

        occurTime = accidentDurationPosixctList$occurTime

        # date of occur
        t = as.Date(occurTime) # see bug.png
        updateDateInput(
            session
            , "device1AccidentOccurTimeDateInput"
            , value = t %>% as.character()
            , min   = paste(t - 1, sep="")
            , max   = paste(t + 1, sep="")
        )

        # hour of occur
        t = seq(lastSelectedAccidentOccurTimeEarlyBoundPosixctF(), length = 5, by = "1 hour")
        t = as.POSIXlt(as.data.frame(t)[,1])$hour
        updateSelectInput(session, 'device1AccidentOccurTimeHourSelectInput', choices = t, selected = (occurTime %>% as.POSIXlt())$hour)

        # minute of occur
        t = (occurTime %>% as.POSIXlt())$min
        updateSelectInput(session, 'device1AccidentOccurTimeMinuteSelectInput', selected = round(t/5)*5 )

        endTime = accidentDurationPosixctList$endTime

        # date of end
        t = as.Date(endTime)
        updateDateInput(
            session
            , "device1AccidentEndTimeDateInput"
            , value = t %>% as.character()
            , min   = paste(t - 1, sep="")
            , max   = paste(t + 1, sep="")
        )

        # hour of end
        t = seq(lastSelectedAccidentOccurTimeEarlyBoundPosixctF(), length = 10, by = "1 hour")
        t = as.POSIXlt(as.data.frame(t)[,1])$hour
        updateSelectInput(session, 'device1AccidentEndTimeHourSelectInput', choices = t, selected = (endTime %>% as.POSIXlt())$hour)

        # minute of end
        t = (endTime %>% as.POSIXlt())$min
        updateSelectInput(session, 'device1AccidentEndTimeMinuteSelectInput', selected = round(t/5)*5 )

        output$accidentIndicatorDevice1 = renderPrint({
            cat(as.character(accidentDurationPosixctList$accidentIndicator))
        })
    })
    observe({ # device2
        accidentDurationPosixctList = reactiveValueList$device2AccidentDurationPosixctList
        if(is.null(accidentDurationPosixctList)) return()

        occurTime = accidentDurationPosixctList$occurTime

        # date of occur
        t = as.Date(occurTime) # see bug.png
        updateDateInput(
            session
            , "device2AccidentOccurTimeDateInput"
            , value = t %>% as.character()
            , min   = paste(t - 1, sep="")
            , max   = paste(t + 1, sep="")
        )

        # hour of occur
        t = seq(lastSelectedAccidentOccurTimeEarlyBoundPosixctF(), length = 5, by = "1 hour")
        t = as.POSIXlt(as.data.frame(t)[,1])$hour
        updateSelectInput(session, 'device2AccidentOccurTimeHourSelectInput', choices = t, selected = (occurTime %>% as.POSIXlt())$hour)

        # minute of occur
        t = (occurTime %>% as.POSIXlt())$min
        updateSelectInput(session, 'device2AccidentOccurTimeMinuteSelectInput', selected = round(t/5)*5 )

        endTime = accidentDurationPosixctList$endTime

        # date of end
        t = as.Date(endTime)
        updateDateInput(
            session
            , "device2AccidentEndTimeDateInput"
            , value = t %>% as.character()
            , min   = paste(t - 1, sep="")
            , max   = paste(t + 1, sep="")
        )

        # hour of end
        t = seq(lastSelectedAccidentOccurTimeEarlyBoundPosixctF(), length = 10, by = "1 hour")
        t = as.POSIXlt(as.data.frame(t)[,1])$hour
        updateSelectInput(session, 'device2AccidentEndTimeHourSelectInput', choices = t, selected = (endTime %>% as.POSIXlt())$hour)

        # minute of end
        t = (endTime %>% as.POSIXlt())$min
        updateSelectInput(session, 'device2AccidentEndTimeMinuteSelectInput', selected = round(t/5)*5 )

        output$accidentIndicatorDevice2 = renderPrint({
            cat(as.character(accidentDurationPosixctList$accidentIndicator))
        })
    })
    observe({ # device3
        accidentDurationPosixctList = reactiveValueList$device3AccidentDurationPosixctList
        if(is.null(accidentDurationPosixctList)) return()

        occurTime = accidentDurationPosixctList$occurTime

        # date of occur
        t = as.Date(occurTime) # see bug.png
        updateDateInput(
            session
            , "device3AccidentOccurTimeDateInput"
            , value = t %>% as.character()
            , min   = paste(t - 1, sep="")
            , max   = paste(t + 1, sep="")
        )

        # hour of occur
        t = seq(lastSelectedAccidentOccurTimeEarlyBoundPosixctF(), length = 5, by = "1 hour")
        t = as.POSIXlt(as.data.frame(t)[,1])$hour
        updateSelectInput(session, 'device3AccidentOccurTimeHourSelectInput', choices = t, selected = (occurTime %>% as.POSIXlt())$hour)

        # minute of occur
        t = (occurTime %>% as.POSIXlt())$min
        updateSelectInput(session, 'device3AccidentOccurTimeMinuteSelectInput', selected = round(t/5)*5 )

        endTime = accidentDurationPosixctList$endTime

        # date of end
        t = as.Date(endTime)
        updateDateInput(
            session
            , "device3AccidentEndTimeDateInput"
            , value = t %>% as.character()
            , min   = paste(t - 1, sep="")
            , max   = paste(t + 1, sep="")
        )

        # hour of end
        t = seq(lastSelectedAccidentOccurTimeEarlyBoundPosixctF(), length = 10, by = "1 hour")
        t = as.POSIXlt(as.data.frame(t)[,1])$hour
        updateSelectInput(session, 'device3AccidentEndTimeHourSelectInput', choices = t, selected = (endTime %>% as.POSIXlt())$hour)

        # minute of end
        t = (endTime %>% as.POSIXlt())$min
        updateSelectInput(session, 'device3AccidentEndTimeMinuteSelectInput', selected = round(t/5)*5 )

        output$accidentIndicatorDevice3 = renderPrint({
            cat(as.character(accidentDurationPosixctList$accidentIndicator))
        })
    })
    observe({ # device4
        accidentDurationPosixctList = reactiveValueList$device4AccidentDurationPosixctList
        if(is.null(accidentDurationPosixctList)) return()

        occurTime = accidentDurationPosixctList$occurTime

        # date of occur
        t = as.Date(occurTime) # see bug.png
        updateDateInput(
            session
            , "device4AccidentOccurTimeDateInput"
            , value = t %>% as.character()
            , min   = paste(t - 1, sep="")
            , max   = paste(t + 1, sep="")
        )

        # hour of occur
        t = seq(lastSelectedAccidentOccurTimeEarlyBoundPosixctF(), length = 5, by = "1 hour")
        t = as.POSIXlt(as.data.frame(t)[,1])$hour
        updateSelectInput(session, 'device4AccidentOccurTimeHourSelectInput', choices = t, selected = (occurTime %>% as.POSIXlt())$hour)

        # minute of occur
        t = (occurTime %>% as.POSIXlt())$min
        updateSelectInput(session, 'device4AccidentOccurTimeMinuteSelectInput', selected = round(t/5)*5 )

        endTime = accidentDurationPosixctList$endTime

        # date of end
        t = as.Date(endTime)
        updateDateInput(
            session
            , "device4AccidentEndTimeDateInput"
            , value = t %>% as.character()
            , min   = paste(t - 1, sep="")
            , max   = paste(t + 1, sep="")
        )

        # hour of end
        t = seq(lastSelectedAccidentOccurTimeEarlyBoundPosixctF(), length = 10, by = "1 hour")
        t = as.POSIXlt(as.data.frame(t)[,1])$hour
        updateSelectInput(session, 'device4AccidentEndTimeHourSelectInput', choices = t, selected = (endTime %>% as.POSIXlt())$hour)

        # minute of end
        t = (endTime %>% as.POSIXlt())$min
        updateSelectInput(session, 'device4AccidentEndTimeMinuteSelectInput', selected = round(t/5)*5 )

        output$accidentIndicatorDevice4 = renderPrint({
            cat(as.character(accidentDurationPosixctList$accidentIndicator))
        })
    })

    # rCharts titles
    rChart1TitleF = reactive({
        returnString = paste('To K <== , K side ')
        oneDeviceRow = lastSelectedDeviceIdDfF()[1,]
        ksMeterSelected = lastSelectedKsMeterF()
        ksMeterDevice = oneDeviceRow$KS_METER
        t = signif((ksMeterSelected - ksMeterDevice)/1000, 3)
        returnString = paste(returnString, sprintf('( %s km ahead )', t))
        t = oneDeviceRow$DEVICE_NBR
        returnString = paste(returnString, sprintf(' %s', t))
        return(returnString)
    })
    output$rChart1Title = renderPrint({
        cat(rChart1TitleF())
    })
    rChart2TitleF = reactive({
        returnString = paste('To K <== , S side ')
        oneDeviceRow = lastSelectedDeviceIdDfF()[2,]
        ksMeterSelected = lastSelectedKsMeterF()
        ksMeterDevice = oneDeviceRow$KS_METER
        t = signif(-(ksMeterSelected - ksMeterDevice)/1000, 3)
        returnString = paste(returnString, sprintf('( %s km behind )', t))
        t = oneDeviceRow$DEVICE_NBR
        returnString = paste(returnString, sprintf(' %s', t))
        return(returnString)
    })
    output$rChart2Title = renderPrint({
        cat(rChart2TitleF())
    })
    output$rChart3Title = renderPrint({
        cat('==> To S, K side ')
        invisible({oneDeviceRow = lastSelectedDeviceIdDfF()[3,]})
        ksMeterSelected = lastSelectedKsMeterF()
        ksMeterDevice = oneDeviceRow$KS_METER
        t = signif((ksMeterSelected - ksMeterDevice)/1000, 3)
        cat(sprintf('( %s km behind )', t))
        t = oneDeviceRow$DEVICE_NBR
        cat(sprintf(' %s', t))
    })
    output$rChart4Title = renderPrint({
        cat('==> To S, S side ')
        invisible({oneDeviceRow = lastSelectedDeviceIdDfF()[4,]})
        ksMeterSelected = lastSelectedKsMeterF()
        ksMeterDevice = oneDeviceRow$KS_METER
        t = signif(-(ksMeterSelected - ksMeterDevice)/1000, 3)
        cat(sprintf('( %s km ahead )', t))
        t = oneDeviceRow$DEVICE_NBR
        cat(sprintf(' %s', t))
    })
    #### end accident device plot titles

    ####  reaction to select: deviceId & time
    lastSelectedIndexF = reactive({
        selectedIndices = input$accidentTable_selected
#         if(is.null(selectedIndices)) {
#             return(as.integer(920))
#         }
        returnT = selectedIndices[end(selectedIndices)[1]]
    })
    lastSelectedAccidentOccurTimePosixctF = reactive({
        input$userInputConfirmButton
        input$previousAccidentButton
        input$nextAccidentButton
        lastSelectedIndex = isolate(reactiveValueList$lastSelectedIndex)
        dateInput = isolate(input$dateInput)
        hourInput = isolate(input$hourInput)
        if(input$inputSelectSource == 'accident'){
            return(as.POSIXct(accidentDf[lastSelectedIndex,]$ACCIDENT_OCCUR_TIME))
        }else{
            t = sprintf('%s %d:00:00', as.character(dateInput), hourInput)
            return(as.POSIXct(t))
        }
    })
    lastSelectedAccidentOccurTimeEarlyBoundPosixctF = reactive({
        t = lastSelectedAccidentOccurTimePosixctF() - 3600*2 - 90 ## x hours data before accident occure time
        #print(t);return(t)
    })
    lastSelectedAccidentOccurTimeLateBoundPosixctF = reactive({
        t = lastSelectedAccidentOccurTimePosixctF() + 3600*3 + 90 ## x hours data after accident occure time
        #print(t);return(t)
    })
    lastSelectedKsMeterF = reactive({
        input$userInputConfirmButton
        input$previousAccidentButton
        input$nextAccidentButton
        isolate(
            if(input$inputSelectSource == 'accident'){
                returnT = accidentDf[reactiveValueList$lastSelectedIndex,]$KS_METER
            }else{
                returnT = as.integer(input$ksMeterInputText)
            }
        )
    })

    lastSelectedWDirectionWSideDeviceIdListF = reactive({
        deviceList.sub.t = D_DEVICE.flowOnly[order(-D_DEVICE.flowOnly$KS_METER),] # order for side
        deviceList.sub.t = subset(deviceList.sub.t
                                  ,KS_METER<=lastSelectedKsMeterF() # side
                                  & grepl('K', DIRECTION_SUNNY)     # direction
                                  #,select = c(DEVICE_NBR, DEVICE_NBR_STANDARD, KS_METER, DIRECTION_SUNNY)
                                  )
        deviceListIndexT = findFirstAvailableDeviceIndexInSubDf(deviceList.sub.t, lastSelectedAccidentOccurTimeEarlyBoundPosixctF(), lastSelectedAccidentOccurTimeLateBoundPosixctF())
        return(deviceList.sub.t[deviceListIndexT,])
    })
    lastSelectedWDirectionESideDeviceIdListF = reactive({
        deviceList.sub.t = D_DEVICE.flowOnly[order(D_DEVICE.flowOnly$KS_METER),]
        deviceList.sub.t = subset(deviceList.sub.t
                                  ,KS_METER>=lastSelectedKsMeterF()
                                  & grepl('K', DIRECTION_SUNNY)
                                  #,select = c(DEVICE_NBR, DEVICE_NBR_STANDARD, KS_METER, DIRECTION_SUNNY)
                                  )
        deviceListIndexT = findFirstAvailableDeviceIndexInSubDf(deviceList.sub.t, lastSelectedAccidentOccurTimeEarlyBoundPosixctF(), lastSelectedAccidentOccurTimeLateBoundPosixctF())
        return(deviceList.sub.t[deviceListIndexT,])
    })
    lastSelectedEDirectionWSideDeviceIdListF = reactive({
        deviceList.sub.t = D_DEVICE.flowOnly[order(-D_DEVICE.flowOnly$KS_METER),]
        deviceList.sub.t = subset(deviceList.sub.t
                                  ,KS_METER<=lastSelectedKsMeterF()
                                  & grepl('S', DIRECTION_SUNNY)
                                  #,select = c(DEVICE_NBR, DEVICE_NBR_STANDARD, KS_METER, DIRECTION_SUNNY)
                                  )
        deviceListIndexT = findFirstAvailableDeviceIndexInSubDf(deviceList.sub.t, lastSelectedAccidentOccurTimeEarlyBoundPosixctF(), lastSelectedAccidentOccurTimeLateBoundPosixctF())
        return(deviceList.sub.t[deviceListIndexT,])
    })
    lastSelectedEDirectionESideDeviceIdListF = reactive({
        deviceList.sub.t = D_DEVICE.flowOnly[order(D_DEVICE.flowOnly$KS_METER),]
        deviceList.sub.t = subset(deviceList.sub.t
                                  ,KS_METER>=lastSelectedKsMeterF()
                                  & grepl('S', DIRECTION_SUNNY)
                                  #,select = c(DEVICE_NBR, DEVICE_NBR_STANDARD, KS_METER, DIRECTION_SUNNY)
                                  )
        deviceListIndexT = findFirstAvailableDeviceIndexInSubDf(deviceList.sub.t, lastSelectedAccidentOccurTimeEarlyBoundPosixctF(), lastSelectedAccidentOccurTimeLateBoundPosixctF())
        return(deviceList.sub.t[deviceListIndexT,])
    })
    lastSelectedDeviceIdDfF = reactive({
        lastSelectedDeviceIdDf = NULL
        lastSelectedDeviceIdDf = rbind(lastSelectedDeviceIdDf, as.data.frame(lastSelectedWDirectionWSideDeviceIdListF()))
        lastSelectedDeviceIdDf = rbind(lastSelectedDeviceIdDf, as.data.frame(lastSelectedWDirectionESideDeviceIdListF()))
        lastSelectedDeviceIdDf = rbind(lastSelectedDeviceIdDf, as.data.frame(lastSelectedEDirectionWSideDeviceIdListF()))
        lastSelectedDeviceIdDf = rbind(lastSelectedDeviceIdDf, as.data.frame(lastSelectedEDirectionESideDeviceIdListF()))
        #print(lastSelectedDeviceIdDf)
        return(lastSelectedDeviceIdDf)
    })

    ### accident-related plots; 4 devices
    output$device1Plot = renderDygraph({ # device1
        deviceId = lastSelectedDeviceIdDfF()[1,]$DEVICE_NBR
        mainDataDf = subset(F_FLOW.asDevice[[deviceId]]
                            ,COUNT_TIME >= lastSelectedAccidentOccurTimeEarlyBoundPosixctF()
                            & COUNT_TIME <= lastSelectedAccidentOccurTimeLateBoundPosixctF()
        )
        attr(mainDataDf, 'deviceId') = deviceId
        mainDataDf = mainDataDfAggregateAndDiff(mainDataDf, input)
        accidentDurationPosixctList = findAccidentDurationPosixctList(mainDataDf, input, lastSelectedAccidentOccurTimePosixctF())
        reactiveValueList$device1AccidentDurationPosixctList = accidentDurationPosixctList
        plot = mainDataDfToDygraph(mainDataDf, input, groupId = 'accidentDevice') %>%
            dyShading(from = accidentDurationPosixctList$occurTime, to = accidentDurationPosixctList$endTime, color = "#FC9DED")
    })
    output$device2Plot = renderDygraph({ # device2
        deviceId = lastSelectedDeviceIdDfF()[2,]$DEVICE_NBR
        mainDataDf = subset(F_FLOW.asDevice[[deviceId]]
                            ,COUNT_TIME >= lastSelectedAccidentOccurTimeEarlyBoundPosixctF()
                            & COUNT_TIME <= lastSelectedAccidentOccurTimeLateBoundPosixctF()
        )
        attr(mainDataDf, 'deviceId') = deviceId
        mainDataDf = mainDataDfAggregateAndDiff(mainDataDf, input)
        accidentDurationPosixctList = findAccidentDurationPosixctList(mainDataDf, input, lastSelectedAccidentOccurTimePosixctF())
        reactiveValueList$device2AccidentDurationPosixctList = accidentDurationPosixctList
        plot = mainDataDfToDygraph(mainDataDf, input, groupId = 'accidentDevice') %>%
            dyShading(from = accidentDurationPosixctList$occurTime, to = accidentDurationPosixctList$endTime, color = "#FC9DED")
    })
    output$device3Plot = renderDygraph({ # device3
        deviceId = lastSelectedDeviceIdDfF()[3,]$DEVICE_NBR
        mainDataDf = subset(F_FLOW.asDevice[[deviceId]]
                            ,COUNT_TIME >= lastSelectedAccidentOccurTimeEarlyBoundPosixctF()
                            & COUNT_TIME <= lastSelectedAccidentOccurTimeLateBoundPosixctF()
        )
        attr(mainDataDf, 'deviceId') = deviceId
        mainDataDf = mainDataDfAggregateAndDiff(mainDataDf, input)
        accidentDurationPosixctList = findAccidentDurationPosixctList(mainDataDf, input, lastSelectedAccidentOccurTimePosixctF())
        reactiveValueList$device3AccidentDurationPosixctList = accidentDurationPosixctList
        plot = mainDataDfToDygraph(mainDataDf, input, groupId = 'accidentDevice') %>%
            dyShading(from = accidentDurationPosixctList$occurTime, to = accidentDurationPosixctList$endTime, color = "#FC9DED")
    })
    output$device4Plot = renderDygraph({ # device4
        deviceId = lastSelectedDeviceIdDfF()[4,]$DEVICE_NBR
        mainDataDf = subset(F_FLOW.asDevice[[deviceId]]
                            ,COUNT_TIME >= lastSelectedAccidentOccurTimeEarlyBoundPosixctF()
                            & COUNT_TIME <= lastSelectedAccidentOccurTimeLateBoundPosixctF()
        )
        attr(mainDataDf, 'deviceId') = deviceId
        mainDataDf = mainDataDfAggregateAndDiff(mainDataDf, input)
        accidentDurationPosixctList = findAccidentDurationPosixctList(mainDataDf, input, lastSelectedAccidentOccurTimePosixctF())
        reactiveValueList$device4AccidentDurationPosixctList = accidentDurationPosixctList
        plot = mainDataDfToDygraph(mainDataDf, input, groupId = 'accidentDevice') %>%
            dyShading(from = accidentDurationPosixctList$occurTime, to = accidentDurationPosixctList$endTime, color = "#FC9DED")
    })

    output$selectedOneAccidentTable = renderDataTable({
        t = subset(accidentDf, select = c(FROM_TABLE, ACCIDENT_OCCUR_TIME, KS_METER, ACCIDENT_FACT_BRIEF_DETAIL))
        row.names(t) = NULL
        datatable(
            t[reactiveValueList$lastSelectedIndex,]
            ,options = list(
                paging = FALSE
                ,searching = FALSE
                ,ordering = FALSE
            )
        )
    })
    output$accidentTable = renderDataTable({
        datatable(
            subset(accidentDf, select = c(FROM_TABLE, ACCIDENT_OCCUR_TIME, KS_METER, ACCIDENT_FACT_BRIEF_DETAIL))
            ,rownames = checkboxRows(accidentDf)
            ,escape = -1
            ,filter = 'bottom'
            ,options = list(pageLength = 2
                           ,lengthMenu = c(2,5,10,25,50)
            )
        )
    })
    output$selectedAccidentDeviceTable = renderDataTable({
        t = subset(lastSelectedDeviceIdDfF(), select = -c(nbr_in_flow, nbr_in_violation))
        row.names(t) = list(
             'K <=, K side'
            ,'K <=, S side'
            ,'=> S, K side'
            ,'=> S, S side'
        )
        datatable(
            subset(t, select = -c(DEVICE_NBR_STANDARD, G78_METER, DEVICE_DESC))
            ,options = list(
                paging = FALSE
                ,searching = FALSE
                ,ordering = FALSE
                )
        )
    })


    # print the selected input
    output$accidentListSelected = renderPrint({
#         selectedIndices = input$accidentTable_selected
#         cat('Selected rows indices: ')
#         if (length(selectedIndices)) {
#             cat(selectedIndices, sep = ',')
#             cat('.\n')
#         }else{
#             cat('non.\n')
#         }
        cat('Time & KsMeter: ')
        #cat(reactiveValueList$lastSelectedIndex); cat(', ')
        cat(as.character(lastSelectedAccidentOccurTimePosixctF())); cat(', ')
        cat(lastSelectedKsMeterF())
    })

    ## calculate averaged metrics, when the user choose enlargeFactor
    output$accidentPossibility = renderPrint({
        selectedPosixlt = as.POSIXlt(lastSelectedAccidentOccurTimePosixctF())

        mainDataDf = getAccidentAnalysisTimelyResult('hour')
        percentage = ((mainDataDf[selectedPosixlt$hour + 1, 2] / mean(mainDataDf[ ,2])) - 1.0) * 100
        cat(sprintf("Hour of Day: %3.2f%% %s than average.\n", abs(percentage), higherOrLower(percentage)))

        mainDataDf = getAccidentAnalysisTimelyResult('wday')
        percentage = ((mainDataDf[selectedPosixlt$wday + 1, 2] / mean(mainDataDf[ ,2])) - 1.0) * 100
        cat(sprintf("Day of Week: %3.2f%% %s than average.\n", abs(percentage), higherOrLower(percentage)))

        mainDataDf = getAccidentAnalysisTimelyResult('mon')
        percentage = ((mainDataDf[selectedPosixlt$mon + 1, 2] / mean(mainDataDf[ ,2])) - 1.0) * 100
        cat(sprintf("Month of Year: %3.2f%% %s than average.\n", abs(percentage), higherOrLower(percentage)))

        selectedKsMeterKm = floor(lastSelectedKsMeterF() / 1000)
        selectedKsMeterKm = max(0, min(79, selectedKsMeterKm))
        mainDataDf = as.data.frame(matrix(c(accidentAnalysis80BinHist$breaks[1:80], accidentAnalysis80BinHist$counts[1:80]), ncol = 2))
        colnames(mainDataDf) = c('x', 'COUNT')
        percentage = ((mainDataDf[selectedKsMeterKm + 1, 2] / mean(mainDataDf[ ,2])) - 1.0) * 100
        cat(sprintf("Milestone: %3.2f%% %s than average.\n", abs(percentage), higherOrLower(percentage)))
    })
    #output$averageSpeed = renderPrint({mean(mainDataDf$speed)})
    #output$averageVolume = renderPrint({mean(mainDataDf$volume)})


    #### Panel Statistical
    output$statisticalPlot_KS_METER_km = renderPlot({
        KS_METER_km = NORMAL_AND_SIMPLE.timeAndKsMeterOnly$KS_METER / 1000
        bins = seq(0, 80, length.out = input$nBin_KS_METER_km + 1)

        # draw the histogram with the specified number of bins
        hist(
            KS_METER_km
            ,breaks = bins
            ,col = 'lightblue'
        )
    })
    output$statisticalPlot_ACCIDENT_timely = renderChart({
        nBy = input$nBy_ACCIDENT_timely
        mainDataDf = getAccidentAnalysisTimelyResult(nBy)

        mP = mPlot(x="x", y=c('COUNT'), type="Line", data=mainDataDf, parseTime=FALSE)
        mP$set(pointSize = 2, lineWidth = 1)

        mP$set(dom = "statisticalPlot_ACCIDENT_timely", width = '50%')
        return(mP)
    })
    output$statisticalPlot_ACCIDENT_OCCUR_TIME = renderPlot({
        ACCIDENT_OCCUR_TIME = as.POSIXct(NORMAL_AND_SIMPLE.timeAndKsMeterOnly$ACCIDENT_OCCUR_TIME)
        bins = seq(min(ACCIDENT_OCCUR_TIME), max(ACCIDENT_OCCUR_TIME), length.out = input$nBin_ACCIDENT_OCCUR_TIME + 1)

        # draw the histogram with the specified number of bins
        hist(
            ACCIDENT_OCCUR_TIME
            ,breaks = bins
            ,col = 'lightblue'
        )
    })

    #### Panel test
    output$testPlot = renderPlot({
        hist(galton$child, xlab='child height', col='lightblue',main='Histogram')
        mu <- input$mu
        lines(c(mu, mu), c(0, 200),col="red",lwd=5)
        mse <- mean((galton$child - mu)^2)
        text(63, 150, paste("mu = ", mu))
        text(63, 140, paste("MSE = ", round(mse, 2)))
    })

    #### Panel Device List
    output$deviceTable = renderDataTable({
        datatable(
            subset(D_DEVICE.flowOnly, select = -c(nbr_in_flow, nbr_in_violation))
        )
    })

    #### Panel Historical Detail
    timeForDetailPosixctF = reactive({
        input$userForDetailInputConfirmButton
        isolate(
            if(1){
                t = sprintf(
                    '%s %d:00:00'
                    , as.character(input$dateForDetailInput)
                    , input$hourForDetailInput
                )
                return(as.POSIXct(t))
            }
        )
    })
    timeForDetailEarlyBoundPosixctF = reactive({
        t = timeForDetailPosixctF() - 3600*2 - 90  ## x hours data before accident occure time
        print(t);return(t)
    })
    timeForDetailLateBoundPosixctF = reactive({
        t = timeForDetailPosixctF() + 3600*3 + 90 ## x hours data after accident occure time
        print(t);return(t)
    })

    output$detailDygraph = renderDygraph({
        deviceId = input$deviceForDetail
        mainDataDf = subset(
            F_FLOW.asDevice[[deviceId]]
            , COUNT_TIME >= timeForDetailEarlyBoundPosixctF()
            & COUNT_TIME <= timeForDetailLateBoundPosixctF()
        )
        attr(mainDataDf, 'deviceId') = deviceId
        mainDataDf = mainDataDfAggregateAndDiff(mainDataDf, input)
        plot = mainDataDfToDygraph(mainDataDf, input)
    })
    output$detailTable = renderDataTable({
        mainDataDf = subset(
            F_FLOW.asDevice[[input$deviceForDetail]]
            , COUNT_TIME >= timeForDetailEarlyBoundPosixctF()
            & COUNT_TIME <= timeForDetailLateBoundPosixctF()
            , select = -c(FLOW_ID, PERIOD, NUM_LARGE, NUM_MIDDLE, NUM_SMALL, NUM_OTHER, NUM_MOTOR, AVR_LENGTH, AVR_TIMESPAN, ORG_CODE)
        )
        rownames(mainDataDf) = NULL
        mainDataDf$COUNT_TIME = substr(as.character(mainDataDf$COUNT_TIME), 12, 20)

        datatable(
            mainDataDf
            , rownames = FALSE
            , options = list(pageLength = 2
                            ,lengthMenu = c(2,5,10,50,100)
            )
        )
    })

})

mainDataDfAggregateAndDiff = function(
    mainDataDf
    , input
    , aggregating = TRUE
) {
    debug('in F mainDataDfAggregateAndDiff')
    deviceId = attr(mainDataDf, 'deviceId')
    mainDataDf$LANE = as.integer(mainDataDf$LANE)
    debug('in F mainDataDfAggregateAndDiff', sprintf("mainDataDf nrow == %d.\n", nrow(mainDataDf)), 1)
    if (aggregating) {
        if(nrow(mainDataDf) < 1) {
            cat(paste('!!!!!!!!!! Warning: mainDataDf nrow = 0. !!!!!!!!!!'))
            return(NA)
        }
        mainDataDf = as.data.frame(as.list(aggregate(
            cbind(COUNT_TIME, NUM_TOTAL, AVR_SPEED, OCCUPANCY, LANE, NUM_LARGE, NUM_MIDDLE, NUM_SMALL, NUM_MOTOR, NUM_OTHER, AVR_SPACE)
            ~ as.POSIXlt(mainDataDf$COUNT_TIME)$hour + as.POSIXlt(mainDataDf$COUNT_TIME)$min
            ,data = mainDataDf
            ,FUN=function(mainDataDf) c(sum = sum(mainDataDf)
                                        ,mean = mean(mainDataDf)
            )
        )))
        debug('in F mainDataDfAggregateAndDiff', sprintf("mainDataDf aggregated nrow == %d.\n", nrow(mainDataDf)), 1)

        mainDataDf$COUNT_TIME = as.POSIXct(mainDataDf$COUNT_TIME.mean, origin="1970-01-01")

        mainDataDf$NUM_TOTAL = mainDataDf$NUM_TOTAL.sum
        mainDataDf$AVR_SPEED = mainDataDf$AVR_SPEED.mean
        mainDataDf$OCCUPANCY = mainDataDf$OCCUPANCY.mean * input$enlargeFactor / 10
        mainDataDf$LANE = mainDataDf$LANE.sum
        mainDataDf$NUM_LARGE = mainDataDf$NUM_LARGE.sum
        mainDataDf$NUM_MIDDLE = mainDataDf$NUM_MIDDLE.sum
        mainDataDf$NUM_SMALL = mainDataDf$NUM_SMALL.sum
        mainDataDf$NUM_MOTOR = mainDataDf$NUM_MOTOR.sum
        mainDataDf$NUM_OTHER = mainDataDf$NUM_OTHER.sum
        mainDataDf$AVR_SPACE = mainDataDf$AVR_SPACE.mean
    }
    if (!is.null(deviceId)) {
        # non diff outlier:
        mainDataDf$deviceId = deviceId
        mainDataDf = adply(mainDataDf, 1, getMDistAndThreshold, input = input) # apply will ADD the function returned columns, instead of only the returned columns
        mainDataDf = adply(mainDataDf, 1, getMDistAndThresholdUpdatable, input = input)

        # pre diff:
        row.names(mainDataDf) = NULL
        mainDataDf = mainDataDf[with(mainDataDf, order(COUNT_TIME)), ]
        dataSelectedForDiffDf = subset(mainDataDf, select = c(NUM_TOTAL, AVR_SPEED))
        nrowCount = nrow(dataSelectedForDiffDf)
        dataSelectedForDiffDf = dataSelectedForDiffDf[2:nrowCount,] - dataSelectedForDiffDf[1:(nrowCount - 1),]
        dataSelectedForDiffDf$COUNT_TIME = mainDataDf[2:nrowCount,]$COUNT_TIME

        # diff:
        dataSelectedForDiffDf$deviceId = deviceId
        dataSelectedForDiffDf = adply(dataSelectedForDiffDf, 1, getDiffMDistAndThreshold, input = input)
        dataSelectedForDiffDf = adply(dataSelectedForDiffDf, 1, getDiffMDistAndThresholdUpdatable, input = input)

        # post diff:
        mainDataDf = mainDataDf[2:nrow(mainDataDf), ]
        row.names(mainDataDf) = NULL
        mainDataDf$DIFF_NUM_TOTAL = dataSelectedForDiffDf$NUM_TOTAL
        dataSelectedForDiffDf = dataSelectedForDiffDf[, c('DIFF_HOUR_OUTLIER', 'DIFF_HOUR_OUTLIER_UPDATE')]
        row.names(dataSelectedForDiffDf) = NULL
        mainDataDf = cbind(mainDataDf, dataSelectedForDiffDf)

        attr(mainDataDf, 'deviceId') = deviceId
    } else {
        warning('in F mainDataDfAggregateAndDiff: Cannot find attr deviceId in mainDataDf !\n')
    }
    debug('ut F mainDataDfAggregateAndDiff')
    return(mainDataDf)
}

mainDataDfToDygraph = function(
    mainDataDf
    , input
    , extraMetricArray = NULL
    , groupId = NULL
) {
    debug('in F mainDataDfToDygraph')
    deviceId = attr(mainDataDf, 'deviceId')
    basicMetricArray = NULL#c("NUM_TOTAL", "AVR_SPEED")
    if(is.null(extraMetricArray)) {
        extraMetricArray = input$detailPlotMetricArray
    }
    allMetricArray = c(basicMetricArray, input$accidentPlotMetricArray, extraMetricArray)
    mainDataDf$FLOW_RATE = mainDataDf$NUM_TOTAL
    mainDataXts = xts(mainDataDf[, allMetricArray], order.by = mainDataDf$COUNT_TIME) ### ??? problem: if xts(x=df) with COUNT_TIME(? or something else) in x=df, num will become char and 0 will be skiped in label div.
    plotDygraph = dygraph(mainDataXts, group = groupId) %>%
        dyLegend(labelsSeparateLines = TRUE) %>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 3), highlightSeriesBackgroundAlpha = 0.9) %>%
        dyRangeSelector(height = 20, strokeColor = "") %>%
        dyAxis('y', label = 'Flow Rate (veh/5min) ; Average Speed (km/h)', labelWidth = 18) %>%
        dyOptions(colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF", "#999999", "#000000", RColorBrewer::brewer.pal(8, "Set2"))) # RColorBrewer::brewer.pal(18, "Set1")   # removed light yellow "#FFFF33"
    return(plotDygraph)
}
