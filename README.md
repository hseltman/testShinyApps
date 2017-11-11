# testShinyApps
Test a set of Shiny apps submitted on the Canvas course management system

If a Shiny homework assignment is made on Canvas, students will submit 'ui.R' and 'server.r' files, and possibly other helper files.  If they resubmit any file, '-1' (etc.) is added before the extension, e.g., 'ui-1.R'.  When the instructor runs Download Submission on Canvas, the files appear in a zip file renamed as 'lastfirst_nc1_nc2_fn[-#].ext', where 'last' is the student's last name, 'first' is first name, 'nc1' and 'nc2' are numeric codes, 'fn' is filename, and 'ext' is extension.  Each time you download, the zip file contains only the highest numbered version of each submitted file.  Because Shiny's runApp() command only allows specification of a directory containing files named 'ui.R" and 'server.R',  this makes testing the dowloaded files tedious and inconvenient.

The R function testShinyApps() serves up each student's app in your browser for testing.  The name of the student (as 'lastfirst' because separation of the two is impossible) is put in the browser title, overriding any explicit browser title the student might set.  (If this function cannot figure out how to alter the code to add the browser title, it will display a message with the student's name in the console.)  If the student's code fails, a Shiny app with the error message will be displayed.  In all cases, closing one student's app automatically opens the next student's app.

When run with no arguments, the function shows the app corresponding to the latest versions of all files for each student, in alphabetical order, using the "sandbox" directory, in "showcase mode" which shows the student's .R files adjacent to the app.  If the working directory contains, e.g., 'aaa_ui.R' and 'aaa_server.R' which are your 'solutions', that app will be displayed first.

If there are any files common to all apps, e.g., a data file, they are provided with the 'needed=' argument.  If you interrupt your work, you can add the 'start=' argument to start at a particular "lastfirst" student.  You can run a specific subset of students using the 'ids=' argument.  You can run an old version of an app using the 'version=' argument.  You can also change the name of the sandbox and the display-mode if desired.

Technical details: This function first creates and/or cleans out the sandbox directory, and adds the 'needed=' files.  Then for each student, it does the following:
1) clear any files created in the sandbox by the previous student
2) find the latest versions of the 'ui.R' and 'server.R' files (or the specified versions)
3) alters the 'server.R' to insert the 'session' argument if not present, and to add stopApp() to session$onSessionEnded, so that when the users quits the app in the browser(), the testShinyApps() function continues rather than stopping
4) attempts to alter the ui.R function so that the student's name will appear in the browser title
5) writes the altered files into the sandbox with the 'ui.R' and 'server.R' names
6) looks for any additional files supplied by the student and writes the most recent version (or the version specified in 'version=') to the sandbox using the student's original filename (so that, e.g., source() will work in the student code.
7) trys to run the app
8) if the app fails, it renames the student files to 'ui.lastfirst.R" and 'server.lastfirst.R' so that the instructor can examine them using showcase mode, and writes a 'stub' app containing the error message to the sandbox

Future features: 
1) a way to put some files into a subdirectory of the sandbox, e.g., 'www'
2) a more robust way to make the needed changes to the student code
