CLEAR
ON ERROR GOTO errHandler
CHAIN "c:\laos7\main.bas"
RUN "c:\laos7\main.bas"
PRINT "LaOS kann nicht mehr gestartet werden. öberprÅfen Sie die Installation."
SLEEP 3
SYSTEM

errHandler:
RESUME NEXT

