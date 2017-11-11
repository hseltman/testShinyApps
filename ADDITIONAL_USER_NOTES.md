# Additional information for instructors
When running a student's Shiny app, you can edit their code in 'ui.R' and 'server.R' in the sandbox directory (saving your changes).  Then just use your browser's reload button to see the effects of those code changes.  Note that any changes you make are not saved when you go on to the next student (although you can do that manually), and the student's original code file (with the long Canvas name) is unchanged.

Currently, this approach will not work if a student's code is not runnable because it is the "error" stub that is run instead.  If you plan to debug the code for those students, copy the 'ui_lastfirst.R' and 'server_lastfirst.R' files to 'ui.R' and 'server.R' in a new directory.  Then after finishing testing all the students through testShinyApps(), you can use 'runApp(theNewDirectory)' to do your debugging.

