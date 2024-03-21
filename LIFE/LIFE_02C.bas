10000 REM /////////////////////
10010 REM //    LIFE v0.1    //
10020 REM /////////////////////
10030 REM //  Blufftek 2024  //
10040 REM /////////////////////
10050 :
10060 :
10070 REM Find out what platform we're running on
10080 mo% = 0 : MODE mo%
10090 screenHeight% = FN_get_screen_height
10100 IF screenHeight% = 32 THEN platform$ = "SDL2"
10110 IF screenHeight% = 60 THEN platform$ = "Agon/Console8" : REM VDP1.04 or later
10120 IF (screenHeight% <> 32) AND (screenHeight% <> 60) THEN CLS : PRINT "Unknown platform": STOP
10130 :
10140 ON ERROR PROC_HandleErr
10150 :
10160 REM Set platform-specific variables
10170 IF platform$ = "SDL2" THEN PROC_SetSDL2Vars
10180 IF platform$ = "Agon/Console8" THEN PROC_SetAgonVars
10190 :
10200 DIM world%(width%, height%)
10210 DIM buffer%(width%, height%)
10220 :
10230 REM Cell states
10240 alive% = TRUE
10250 dead% = FALSE
10260 :
10270 gen% = 0 : REM Init generations
10280 :
10290 border_char$ = CHR$(129)
10300 :
10310 pauseTime% = 0 : REM Keep track of how long we pause
10320 :
10330 REM Main Loop
10340 MODE mo%
10350 screenHeight% = FN_get_screen_height
10360 PROC_ShowCursor(FALSE)
10370 REPEAT UNTIL INKEY(0) = -1 : REM Flush keyboard buffer
10380 PROC_InitWorld
10390 PROC_PrintBorder
10400 COLOUR 2
10410 PRINT TAB(top_left_X% - 1, top_left_Y% - 3);"JOHN CONWAY'S GAME OF LIFE";
10420 COLOUR 4
10430 PRINT TAB(top_left_X% + width% DIV 2 - 1, top_left_Y% - 3);"[Blufftek 2024]";
10440 COLOUR 2
10450 PRINT TAB(top_left_X% + width% - LEN(platform$) + 1, top_left_Y% - 3);platform$
10460 COLOUR 7
10470 PROC_SpacePrompt(TRUE)
10480 startTime% = TIME
10490 COLOUR 2
10500 PRINT TAB(top_left_X% + width% - 22, top_left_Y% + height% + 2);"ELAPSED TIME : ";
10510 COLOUR 7
10520 REPEAT
10530   PROC_DisplayWorld
10540   PROC_CheckCells
10550   PROC_BufferIntoWorld
10570   gen% = gen% + 1
10580   PROC_DisplayGeneration(gen%)
10590   timeElapsed% = TIME - startTime% - pauseTime%
10600   IF INKEY(-99) THEN PROC_Pause
10610   PROC_DisplayTime(timeElapsed%, top_left_X% + width% - 7, top_left_Y% + height% + 2)
10620 UNTIL FALSE
10630 END
10640 :
10650 REM Print border box
10660 DEFPROC_PrintBorder
10670 COLOUR 5
10680 FOR column% = -1 TO width% : REM Top border
10690   PRINT TAB(top_left_X% + column%, top_left_Y% - 1); border_char$
10700 NEXT column%
10710 FOR row% =  top_left_Y% TO top_left_Y% + height%
10720   PRINT TAB(top_left_X% - 1, row%); border_char$; : REM Left border
10730   PRINT TAB(top_left_X% + width%, row%); border_char$; : REM Right border
10740 NEXT row%
10750 FOR column% = -1 TO width% : REM Bottom border
10760   PRINT TAB(top_left_X% + column%, top_left_Y% + height%); border_char$
10770 NEXT column%
10780 COLOUR 7
10790 ENDPROC
10800 :
10810 REM Initialise world with random cells
10820 DEFPROC_InitWorld
10830 FOR row% = 0 TO height% - 1
10840   FOR column% = 0 TO width% - 1
10850     REM Generate a cell, or don't
10860     IF RND(3) = 1 THEN world%(column%, row%) = alive% ELSE world%(column%, row%) = dead%
10870   NEXT column%
10880 NEXT row%
10890 ENDPROC
10900 :
10910 REM Display world
10920 DEFPROC_DisplayWorld
10930 COLOUR 3
10940 FOR row% = 0 TO height% - 1
10950   FOR column% = 0 TO width% - 1
10960     IF world%(column%, row%) = alive% THEN cell$ = "o" ELSE cell$ = " "
10970     PRINT TAB(top_left_X% + column%, top_left_Y% + row%);cell$;
10980   NEXT column%
10990 NEXT row%
11000 COLOUR 7
11010 ENDPROC
11020 :
11030 REM Check cells for birth or death
11040 DEFPROC_CheckCells
11050 neighbours% = 0
11060 FOR row% = 0 TO height% - 1
11070   FOR column% = 0 TO width% - 1
11080     neighbours% = FN_NumNeighbours(column%, row%)
11090     IF world%(column%, row%) = alive% THEN IF (neighbours% < 2) OR (neighbours% > 3) THEN buffer%(column%, row%) = dead%
11100     IF world%(column%, row%) = alive% THEN IF (neighbours% = 2) OR (neighbours% = 3) THEN buffer%(column%, row%) = alive%
11110     IF world%(column%, row%) = dead% THEN IF neighbours% = 3 THEN buffer%(column%, row%) = alive%
11120   NEXT column%
11130 NEXT row%
11140 ENDPROC
11150 :
11160 REM Return number of alive neighbours
11170 DEFFN_NumNeighbours(cellX%, cellY%)
11180 num_alive% = 0
11190 REM Top left
11200 IF (cellX% > 0) AND (cellY% > 0) THEN IF world%(cellX% - 1, cellY% - 1) THEN PROC_IncAlive
11210 REM Top middle
11220 IF (cellY% > 0) THEN IF world%(cellX%, cellY% - 1) THEN PROC_IncAlive
11230 REM Top right
11240 IF (cellX% < width% - 1) AND (cellY% > 0) THEN IF world%(cellX% + 1, cellY% - 1) THEN PROC_IncAlive
11250 REM Left middle
11260 IF (cellX% > 0) THEN IF world%(cellX% - 1, cellY%) THEN PROC_IncAlive
11270 REM Right middle
11280 IF (cellX% < width% - 1) THEN IF world%(cellX% + 1, cellY%) THEN PROC_IncAlive
11290 REM Bottom left
11300 IF (cellX% > 0) AND (cellY% < height% - 1) THEN IF world%(cellX% - 1, cellY% + 1) THEN PROC_IncAlive
11310 REM Bottom middle
11320 IF (cellY% < height% - 1) THEN IF world%(cellX%, cellY% + 1) THEN PROC_IncAlive
11330 REM Bottom right
11340 IF (cellX% < width% - 1) AND (cellY% < height% - 1) THEN IF world%(cellX% + 1, cellY% + 1) THEN PROC_IncAlive
11350 = num_alive%
11360 :
11370 REM Increment number of alive cells
11380 DEFPROC_IncAlive
11390 num_alive% = num_alive% + 1
11400 ENDPROC
11500 :
11510 REM Copy buffer into world
11520 DEFPROC_BufferIntoWorld
11530 FOR row% = 0 TO height% - 1
11540   FOR column% = 0 TO width% - 1
11550     world%(column%, row%) = buffer%(column%, row%)
11560   NEXT column%
11570 NEXT row%
11580 ENDPROC
11590 :
11600 DEFPROC_DisplayGeneration(gen%)
11610 COLOUR 2
11620 PRINT TAB(top_left_X% - 1, top_left_Y% + height% + 2);"GENERATION : ";gen%;
11630 COLOUR 7
11640 ENDPROC
11650 :
11660 DEFPROC_DisplayTime(ti%, xPos%, yPos%)
11670 padSec$ = "" : padMin$ = "" : padHr$ = ""
11680 sec% = (ti% DIV 100) MOD 60 : IF sec% < 10 THEN padSec$ = "0"
11690 min% = (ti% DIV 6000) MOD 60 : IF min% < 10 THEN padMin$ = "0"
11700 hr% = (ti% DIV 360000) MOD 24 : IF hr% < 10 THEN padHr$ = "0"
11710 COLOUR 2
11720 PRINT TAB(xPos%, yPos%);padHr$;hr%;":";padMin$;min%;":";padSec$;sec%
11730 COLOUR 7
11740 ENDPROC
11750 :
11760 DEF FN_get_screen_height
11770 CLS : REM Start at top left of screen
11780 ve% = 1
11790 PRINT "*"
11800 REPEAT
11810   PRINT : REM Print a blank line
11820   ve% = ve% + 1 : REM Increment vertical position
11830 UNTIL GET$(0,0)<>"*" : REM Until "*" scrolls off top of screen
11840 = ve%
11850 :
11860 REM Set specific SDL2 variables
11870 DEFPROC_SetSDL2Vars
11880 height% = 54 : width% = 74 : REM Size of world
11890 top_left_X% = 3 : top_left_Y% = 5 : REM Where on screen
11900 mo% = 15 : REM Screen mode
11910 ENDPROC
11920 :
11930 REM Set specific Agon variables
11940 DEFPROC_SetAgonVars
11950 height% = 50 : width% = 72 : REM Size of world
11960 top_left_X% = 4 : top_left_Y% = 4 : REM Where on screen
11970 mo% = 0 : REM Screen mode
11980 ENDPROC
11990 :
12000 REM Pause generations and time elapsed
12010 DEFPROC_Pause
12020 PROC_SpacePrompt(FALSE)
12030 stopTime% = TIME
12040 IF INKEY(0) = 32 THEN REPEAT UNTIL INKEY$(-99) <> ""
12050 pauseTime% = pauseTime% + TIME - stopTime%
12060 PROC_SpacePrompt(TRUE)
12070 ENDPROC
12080 :
12090 REM Show/hide <SPACE> prompt
12100 DEFPROC_SpacePrompt(flag%)
12110 space$ = "<SPACE> to pause"
12120 IF NOT flag% THEN space$ = STRING$(LEN(space$), " ")
12130 COLOUR 4
12140 PRINT TAB(top_left_X% + (width% - LEN(space$))DIV 2, top_left_Y% + height% + 2);space$
12150 COLOUR 7
12160 ENDPROC
12170 :
12180 REM Show cursor if flag% is TRUE
12190 DEFPROC_ShowCursor(flag%)
12200 IF flag% THEN VDU 23,1,1;0;0;0;0; ELSE VDU 23,1,0;0;0;0;
12210 ENDPROC
12220 :
12230 DEFPROC_HandleErr
12240 PROC_ShowCursor(TRUE):PRINT TAB(top_left_X% -1, screenHeight%);:COLOUR 7:COLOUR 128:REPORT:PRINT " at line ";ERL : END
12250 ENDPROC
