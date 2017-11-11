## testShinyApps.R contains testShinyApps()
## Purpose: This function tests shiny apps submitted on Canvas
## Author: H. Seltman
## Date: 11/11/2017

## Usage notes: It is assumed each student submits a "ui.R"
## file and a "server.R" file.  Use "Download Submissions"
## in Canvas, and unzip in a folder.  Run this function in
## the folder. This function understands Canvas "-#" versions.
## By default the lastest version in the folder is run,
## or you can choose a specific version to run.
## If, e.g., a fixed data file or teacher-generated helper
## file is required, put that in the "needed" argument.
## If the student submits other helper files, they will
## automatically be put in the sandbox with their ui.R and
## server.R files.

# Description: Run each student submission from Canvas'
#   "Download Submissions" function as a Shiny App in a
#   sandbox.  Each app shows the student id in the browser
#   title.  (This is a complex programming task that either
#   adds a titlePanel(), adds a windowTitle= argument to
#   the existing titlePanel(), or hijacks the windowTitle=
#   argument.  If the code is too non-standard to do any
#   of these, a message is printed in the console.)
#   When you are done testing one student's app, close it
#   and the next student's app will appear.  By default,
#   only the most recent Canvas submission is used for each
#   student.
#
# Usage: testShinyApps(needed=NULL, ids=NULL,
#                      version=NULL, start=NULL,
#                      sandbox="sandbox", display.mode="showcase")
#
# Arguments:
#   needed: character vector of files needed for the app
#           (excluding "ui.R" and "server.R")
#   ids: NULL or character vector of student ids;  if
#         provided only those students' apps are run
#   version: NULL of numeric vector of length one
#         giving a specific Canvas-determing version number
#         (0-9) to analyze, where 0 means initial submission
#         (only) and NULL means the latest version
#   start: a student id (character) at which testing starts
#         (alphabetically earlier ids are skipped)
#   sandbox: name of the sandbox
#   display.mode: "showcase" or "normal"
#
# Value: student ids, invisibly
#
testShinyApps = function(needed=NULL, ids=NULL,
                         version=NULL, start=NULL,
                         sandbox="sandbox", display.mode="showcase") {
  # Get all Canvas ui.R and server.R files
  uiServerFiles = list.files(pattern="[[:alnum:]]{3,}_(ui|server)(-\\d)?.R$")
  
  # Setup for "last" version
  useLastVersion = is.null(version)

  # Get all student ids
  if (is.null(ids)) {
    ids = sort(unique(sub("^([^_]+).*", "\\1", uiServerFiles)))
    idsProvided = FALSE
  } else {
    idsProvided = TRUE
  }
  
  # Handle a "start"ing student
  if (!is.null(start)) {
    index = match(start[1], ids)
    if (is.na(index)) stop("starting id '", start, "' not found")
    ids = ids[index:length(ids)]
  }
  
  # Make sandbox and place needed files in it and erase other files
  if (!dir.exists(sandbox)) {
    if (!dir.create(sandbox))
      stop("cannot create '", sandbox, "'")
  }
  for (f in needed) {
    if (!file.exists(f)) {
      stop("missing file: '", f, "'")
    }
    fSand = file.path(sandbox, f)
    if (!file.exists(fSand)) {
      if (!file.copy(f, fSand, overwrite=FALSE)) {
        stop("cannot write '", f, "' to '", fSand, "'")
      }
    }
  }
  for (f in setdiff(list.files(sandbox), needed)) {
    fSand = file.path(sandbox, f)
    if (!file.remove(fSand)) {
      stop("cannot remove extraneous file '", fSand, "'")
    }
  }

  # Shiny must be loaded to run runApp()
  library("shiny")
  
  # Code to add "stop" functionality so quitting one app
  # does not exit this function.
  allowStop = c("  session$onSessionEnded(function() {",
                "    stopApp()",
                "  })")
  
  # Run each student's files in the sandbox
  for (id in ids) {
    # Remove old files from previous student
    toKill = setdiff(list.files(sandbox), needed)
    for (f in toKill) {
      fSand = file.path(sandbox, f)
      if (!file.remove(fSand)) {
        stop("cannot remove '", fSand, "'")
      }
    }

    # Get correct version for this student
    if (useLastVersion) {
      fVec = grep(paste0(id, ".*_.*?-\\d.[[:alnum:]]{1,4}$"),
                  uiServerFiles, value=TRUE)
      if (length(fVec) == 0) {
        version = 0
      } else {
        vVec = gsub(".*-(\\d).*", "\\1", fVec)
        if (length(vVec) == 0) {
          version = 0
        } else {
          version = max(as.numeric(vVec))
        }
      }
    }
    
    # Get student UI and Server filenames
    vREText = paste0("-", version)
    fUi = grep(paste0(id, "_.*ui", 
                      ifelse(version==0, "", vREText),
                      ".R"), 
               uiServerFiles, value=TRUE)
    fServer = grep(paste0(id, "_.*server", 
                          ifelse(version==0, "", vREText),
                          ".R"), 
                   uiServerFiles, value=TRUE)
    #
    # Give useful, but not excessive messages about missing files
    if (length(fUi) > 1 || length(fServer) > 1)
      stop("Canvas assumption error")
    missCnt = 2 - length(fUi) - length(fServer)
    if (missCnt == 2) {
      if (!idsProvided) next
    } else if (missCnt==1) {
      cat("\nFor student '", id, "', version=", version,
          ", ", ifelse(length(fUi) == 0, "'ui.R'", "'server.R'"),
          " is missing\n", sep="")
      next
    }
    
    # Get student UI file contents and add functionality to label the 
    # browser window with the student's id.
    uiCode = try(readLines(fUi, warn=FALSE))
    if (is(uiCode, "try-error")) {
      cat("\ncannot read '", fUi, "' for '", id, "'\n")
      next
    }
    titleLoc = grep("titlePanel[(].*[)]", uiCode)
    # if titlePanel() is not present, add one
    if (length(titleLoc) == 0) {
      preTitle = grep("(navbar|fluid)Page\\s*[(]", uiCode)
      if (length(preTitle) == 0) {
        cat("bad UI code (no fluid/navbarPage) for", id, "\n")
        next
      }
      preTitle = preTitle[1]
      uiCode = c(uiCode[1:preTitle],
                 paste0("  titlePanel('no titlePanel', '", id, "'),"),
                 uiCode[(preTitle+1):length(uiCode)])
    # else if titlePanel() is already present, edit it (very complex!)
    } else {
      OK = length(titleLoc) == 1
      if (OK) {
        line = uiCode[titleLoc]
        noQuote = gsub("(('|\").*?('|\"))", "q", line)
        commaCount = length(gregexpr(",", noQuote, fixed=TRUE)[[1]])
        if (commaCount < 1 || commaCount > 2) {
          OK = FALSE
        }
      }
      if (!OK) {
        cat("\nNote: titlePanel() too complex to modify for '",
            id, "'\n", sep="")
      } else {
        if (commaCount == 1) {
          # Process a "titlePanel(title)," styled line
          uiCode[titleLoc] = sub("(titlePanel\\s*[(].*)[)]", 
                                 paste0("\\1, windowTitle='", id, "')"),
                                 line)
        } else {
          # Process a "titlePanel(title, windowTitle)," styled line
          stringLocs = gregexpr("(('|\").*?('|\"))", line)[[1]]
          if (length(stringLocs) == 0) {
            line = gsub("(^.*?,)(.*)([)]\\s*,\\s*(#|$))",
                        paste0("\\1 windowTitle='", id, "'\\3"),
                        line)
          } else {  # find the first unquoted comma and edit the line
            segCnt = length(stringLocs) + 1
            lineLen = nchar(line)
            startEnd = cbind(starts = c(1, as.vector(stringLocs) + 
                                           attr(stringLocs, "match.length")),
                             ends = c(as.vector(stringLocs) - 1, lineLen))
            if (startEnd[2, "starts"] == 1) {
              startEnd = startEnd[2:segCnt, ]
              segCnt = segCnt - 1
            }
            if (startEnd[segCnt - 1, "ends"] == lineLen) {
              segCnt = segCnt - 1
              startEnd = startEnd[1:segCnt, ]
            }
            commaLoc = regexpr(",",
                               apply(startEnd, 1,
                                     function(x) substring(line, x[1], x[2])),
                               fixed=TRUE)
            commaIndex = which(commaLoc != -1)
            if (length(commaIndex) != 2) {
              OK = FALSE
              cat("\nNote: titlePanel() too complex to modify for '",
                  id, "'\n", sep="")
            } else {
              commaIndex = commaIndex[1]
              line = paste0(substring(line, 1,
                                      startEnd[commaIndex, "starts"] + 
                                        commaLoc[commaIndex] - 1),
                            paste0(" windowTitle='", id, "'),"))
            }
          }
          if (OK) uiCode[titleLoc] = line
        }  # end if replacing second argument to titlePanel()
      }  # end if titlePanel() can be modified
    }  # end if titlePanel() is present
    #
    # save altered ui code to ui.R in sandbox
    fSand = file.path(sandbox, "ui.R")
    rtn = try(write(uiCode, fSand))
    if (is(rtn, "try-error")) {
      cat("\ncannot write '", fSand, "' for '", id, "'\n")
      next
    }
    
    # Get student server file and add functionality to stop app without
    # exiting this function. 
    serverCode = try(readLines(fServer, warn=FALSE))
    if (is(serverCode, "try-error")) {
      cat("\ncannot read '", fServer, "' for '", id, "'\n")
      next
    }
    funLoc = grep("function\\s*[(]\\s*input\\s*,\\s*output", serverCode)
    if (length(funLoc) != 1) {
      cat("bad server code (server function definition) for", id, "\n")
      next
    }
    serverCode[funLoc] = sub("function\\s*[(]\\s*input\\s*,\\s*output\\s*[)]",
                             "function(input, output, session)",
                             serverCode[funLoc])
    serverCode = c(serverCode[1:funLoc], allowStop, 
                   serverCode[(funLoc+1):length(serverCode)])
    #
    # save altered server code to ui.R in sandbox
    fSand = file.path(sandbox, "server.R")
    rtn = try(write(serverCode, fSand))
    if (is(rtn, "try-error")) {
      cat("\ncannot write '", fSand, "' for '", id, "'\n")
      next
    }
    
    # Add any student helper files corresponding to this "version"
    idPat = paste0("^", id, ".*_.*?", vREText, ".[[:alnum:]]{1,4}$")
    fVec = setdiff(list.files(pattern=idPat), c(fUi, fServer))
    for (f in fVec) {
      if (!file.exists(f)) {
        stop("cannot read file: '", f, "'")
      }
      shortName = sub(paste0(".*_(.*?", vREText, ".[[:alnum:]]{1,4}$)"),
                      "\\1", f)
      if (!file.copy(f, file.path(sandbox, shortName), overwrite=FALSE)) {
          stop("cannot write '", f, "' to the sandbox")
      }
    }
    
    # Run the app
    testEnv = new.env()
    tryResult = try(source(file.path(sandbox, "ui.R"), local=testEnv),
                    silent=TRUE)
    if (is(tryResult, "try-error")) {
      showError(tryResult, "ui.R", sandbox, id, display.mode)
    } else {
      tryResult = try(source(file.path(sandbox, "server.R"), local=testEnv),
                      silent=TRUE)
      if (is(tryResult, "try-error")) {
        showError(tryResult, "server.R", sandbox, id, display.mode)
      } else {
        runApp(sandbox, launch.browser=TRUE, display.mode=display.mode)
      }
    }  # end of "run the app"
    rm(testEnv)
  }  # end "for (id in ids)"
  
  invisible(ids)
}  # end testShinyApps()


# showError() renames 'ui.R' and 'server.R' to 'ui_usersId.R' and
# 'server_usersId.R', and creates ui.R and server.R that contain the
# error message in 'tryResult'
showError = function(tryResult, fileSource, sandbox, id, display.mode) {
  # renames
  fSandUi = file.path(sandbox, "ui.R")
  fUser = paste0("ui_", id, ".R")
  if (!file.rename(fSandUi, file.path(sandbox, fUser))) {
    stop("cannot rename '", fSandUi, '" to "', fUser, "'")
  }
  fSandServer = file.path(sandbox, "server.R")
  fUser = paste0("server_", id, ".R")
  if (!file.rename(fSandServer, file.path(sandbox, fUser))) {
    stop("cannot rename '", fSandServer, '" to "', fUser, "'")
  }
  
  # create stub Shiny app with error information
  uiCode = c("library(shiny)",
             "fluidPage(",
             paste0("  titlePanel('Student Error', '", 
                    id, "'),"),
             paste0("  p(strong('Error in ", fileSource,
                    " for \"", id, "\":')),"),
             paste0("  p('", 
                    trimws(as.character(attr(tryResult, "condition"))),
                    "')"),
             ")")
  serverCode = c("library(shiny)",
                 "function(input, output, session) {",
                 "  session$onSessionEnded(function() {",
                 "     stopApp()",
                 "  })",
                 "  ",
                 "}")
  rtn = try(write(uiCode, fSandUi))
  if (is(rtn, "try-error")) {
    stop("\ncannot write error version of'", fSandUi,
        "' for '", id, "'")
  }  
  rtn = try(write(serverCode, fSandServer))
  if (is(rtn, "try-error")) {
    stop("\ncannot write error version of'", fSandServer,
         "' for '", id, "'")
  }  
  
  runApp(sandbox, launch.browser=TRUE, display.mode=display.mode)
  
  return(NULL)
}


# Test code
if (exists("testingTestShinyApps")) {
  write(paste0("library(shiny)\nfluidPage(\nsidebarLayout(\n",
               "sidebarPanel(\nsliderInput('n', 'n', min=0, max=10, value=5)\n",
               "),\nmainPanel(p('Empty Main Panel'))))\n"),
        file="fake_ui.R")
  write("library(shiny)\nfunction(input, output, session) {\n}\n",
        file="fake_server.R")
  testShinyApps(ids="fake")
}
