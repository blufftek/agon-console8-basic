   10 REM =----------------------
   20 REM =     STUNT DRIVER
   30 REM =    Gary Woolridge
   40 REM =----------------------
   50 REM =    Acorn Programs
   60 REM =    June/July 1984
   70 REM =----------------------
   80 REM = Agon & SDL2 port by
   90 REM =       Blufftek
  100 REM =         2024
  110 REM =----------------------
  120 REM =         v2.1
  130 REM =----------------------
  140 :
  150 REM [blufftek] Find out what platform we're running on
  160 mo% = 0 : MODE mo%
  170 conHeight% = FN_GetNumTextRows
  180 IF conHeight% = 32 THEN platform$ = "SDL2"
  190 IF conHeight% = 60 THEN platform$ = "Agon/Console8" : REM VDP1.04 or later
  200 IF (conHeight% <> 32) AND (conHeight% <> 60) THEN CLS : PRINT "Unknown platform" : STOP
  210 :
  220 ON ERROR PROC_HandleErr : REM [blufftek]
  230 :
  240 REM [blufftek] Define physical colours
  250 BLACK=0:RED=1:GREEN=2:YELLOW=3:BLUE=4:MAGENTA=5:CYAN=6:WHITE=7
  260 GREY=8:iRED=9:iGREEN=10:iYELLOW=11:iBLUE=12:iMAGENTA=13:iCYAN=14:iWHITE=15
  270 :
  280 REM [blufftek] Set platform-specific variables
  290 IF platform$ = "SDL2" THEN PROC_SetSDL2Vars
  300 IF platform$ = "Agon/Console8" THEN PROC_SetAgonVars
  310 :
  320 REM Mode 7 control codes
  330 aRED=129 : aGREEN=130 : aYELLOW=131 : aBLUE=132 : aMAGENTA=133 : aCYAN=134 : aWHITE=135 : aBLACK=156
  340 FLASH=136 : nFLASH=137 : nBACKGROUND=157 : dHEIGHT=141 : nHEIGHT=140
  350 gRED=145 : gGREEN=146 : gYELLOW=147 : gBLUE=148 : gMAGENTA=149 : gCYAN=150 : gWHITE=151
  360 :
  370 REM Box characters
  380 TL_CORNER=190 : TR_CORNER=237 : BL_CORNER=237 : BR_CORNER=190
  390 T_SIDE=163 : B_SIDE=112 : L_SIDE=181 : R_SIDE=234
  400 :
  410 :
  420 MODE 7 : REM Teletext mode for intro screen
  430 :
  440 PROC_ShowCursor(FALSE)
  450 :
  460 PROC_Box7 : REM Print coloured box in MODE 7
  470 :
  480 PROC_Intro : REM Print intro screen in MODE 7
  490 :
  500 REM Wait for SPACE key
  510 REPEAT UNTIL INKEY$(0) = " "
  520 :
  530 MODE mo% : REM Set screen mode according to platform
  540 :
  550 :
  560 REM 'Acorn Programs' listing starts here
  570 PROCinitialize
  580 :
  590 REPEAT
  600   REPEAT
  610     REM [blufftek] MODE change relocated to before USGs
  620     PROCvars : REM Zero all variables & reset flags
  630     PROCscreen
  640     PROCscore
  650     REPEAT
  660       PROCkeys
  670       IF S%<>0 OR J% : PROCmovecar : REM Move car if delete or return pressed
  680       IF P%>0 OR Q%>0 : dead=TRUE
  690     UNTIL end OR dead
  700     IF dead: lives%=lives%-1
  710     IF end: score%=score%+10*nbars% : nbars%=nbars%+1 : PROC_EndSound : REM Increment score if successfully reached end. [blufftek] Play platform-dependent sound
  720     IF score%>99990 : score%=0 : REM If highest score possible, reset to 0
  730   UNTIL nbars%=13 OR lives%=0 : REM Max number of barrels is 13
  740   nbars%=1 : REM Exit previous loop if run out of lives or max barrels reached (reset barrels to 1)
  750   uu%=uu%+200 : REM Increment jump height
  760   IF uu%>700 : uu%=300 : REM Restrict jump height if max
  770 UNTIL lives%=0
  780 PROCnewgame
  790 MODE 7 : PROC_Box7 : PROC_Outro : REM [blufftek] Set teletext mode and end program with outro screen
  800 END
  810 :
  820 :
  830 DEF PROCkeys : REM [blufftek] Changed to DELETE key as Agon won't scan for SHIFT using this method
  840 K%=-1
  850 IF INKEY(-90) : K%=1 : REM If delete key pressed (negative if not pressed)
  860 S%=S%+K% : REM S% is length of time delete key still pressed (negative if still not pressed)
  870 IF S%>48 : S%=48 : REM Max time delete key pressed
  880 IF S%<0 : S%=0 : REM Min time delete key not pressed
  890 IF J% : ENDPROC : REM Exit if return key pressed
  900 IF INKEY(-74)  J%=TRUE : REM Set flag if return key pressed
  910 IF J% AND Z%=0 H%=4 : REM If return key pressed
  920 ENDPROC
  930 :
  940 :
  950 DEF PROCmovecar
  960 end=FALSE : dead=FALSE
  970 GCOL 3,WHITE : REM Graphics mode is Exclusive-OR
  980 *FX 19 : REM Wait for next video frame
  990 MOVE X%,Y%+Z% : VDU 128,129 : REM Update car position and print car
 1000 X%=X%+S% : Z%=Z%+H% : REM Update according to time delete key pressed
 1010 P%=POINT(X%+128,Y%-16+Z%) : REM Collision detection (allowing for graphical/physical co-ords ratio)
 1020 Q%=POINT(X%,Y%+Z%-24)
 1030 IF P%>0 OR Q%>0:PROCcrash:ENDPROC : REM Crash and exit if collision (any colour other than BLACK)
 1040 IF Z%>64 : H%=-4
 1050 IF Z%=0 : H%=0 : J%=0
 1060 IF X%>1152 : Y%=Y%-200 : X%=0
 1070 IF Y%<300 : end=TRUE
 1080 MOVE X%,Y%+Z% : VDU 128,129 : REM Print car at new position
 1090 ENDPROC
 1100 :
 1110 :
 1120 DEF PROCcrash
 1130 IF platform$ = "SDL2" THEN SOUND 0,2,100,50 ELSE PROC_CrashSoundAgon
 1140 MOVE X%+32,Y%
 1150 GCOL 1,iRED : VDU 135,8 : REM Graphics mode is OR : Print a first explosion and backspace cursor
 1160 GCOL 1,iBLUE: VDU 136 : REM Print second explosion (probably needs a delay before this)
 1170 TIME=0
 1180 REPEAT UNTIL TIME>200 : REM Wait 2 seconds
 1190 ENDPROC
 1200 :
 1210 :
 1220 DEF PROCscore
 1230 LOCAL X
 1240 X=5-LEN(STR$(score%))
 1250 VDU 4,23,1,0;0;0;0; : COLOUR CYAN : REM Print at text cursor : Disable cursor
 1260 PRINT TAB(X,28);score%
 1270 PRINT TAB(8,28);nbars%
 1280 PRINT TAB(15,28);lives%
 1290 VDU 5 : REM Print at graphics cursor
 1300 ENDPROC
 1310 :
 1320 :
 1330 DEF PROCscreen
 1340 GCOL 0, GREEN
 1350 FOR O%=868 TO 268 STEP -200 : REM Draw track lines
 1360   FOR I%=0 TO 16 STEP 4
 1370     PLOT 77,0,O%-I%
 1380   NEXT
 1390 NEXT
 1400 GCOL 0, BLUE : MOVE 128,uu%
 1410 VDU 5,131,132 : GCOL 0, YELLOW : REM Print up-ramp at graphics cursor position
 1420 FOR B%=1 TO nbars% : REM Print barrels
 1430   VDU 130
 1440 NEXT
 1450 GCOL 0, BLUE : VDU 133,134 : GCOL 0, WHITE : REM Print down-ramp
 1460 MOVE X%,Y% : VDU 128,129,4 : REM Print car & turn off printing text at graphics cursor
 1470 PRINT TAB(0,25):PRINT"SCORE";TAB(6);"BARRELS";TAB(14);"CARS"
 1480 COLOUR CYAN : PRINT TAB(0,28);"00000"
 1490 ENDPROC
 1500 :
 1510 :
 1520 DEF PROCnewgame
 1530 VDU 4,23,1,0;0;0;0; : REM Print at text cursor : Disable cursor
 1540 CLS : COLOUR YELLOW
 1550 PRINT'''"SCORE : ";score%
 1560 PRINT'''"New Game (Y/N) :";
 1570 REM *FX 15,0 : REM Flush buffers
 1580 REPEAT UNTIL INKEY(0) = -1 : REM [blufftek] Flush keyboard buffer
 1590 REPEAT
 1600   A$=CHR$(GET AND 223) : REM Wait for keyboard character and convert to upper case
 1610 UNTIL A$="Y" OR A$="N"
 1620 IF A$="Y" THEN RUN
 1630 ENDPROC
 1640 :
 1650 :
 1660 DEFPROCvars
 1670 V%=0 : S%=0 : Z%=0 : J%=0 : H%=0
 1680 X%=0 : Y%=900 : P%=0 : Q%=0 : K%=0
 1690 end=FALSE : dead=FALSE
 1700 ENDPROC
 1710 :
 1720 :
 1730 DEF PROCinitialize
 1740 @%=1 : REM Figures printed as exponential values
 1750 score%=0 : lives%=3 : nbars%=1 : REM nbars% is number of barrels
 1760 uu%=300
 1770 REM *FX 4,1 : REM Arrow keys generate ascii codes (ignored by BB4SDL2 & Agon/Console8)
 1780 REM *FX 16,0 : REM disable ADC channels (ignored by BB4SDL2 & Agon/Console8)
 1790 REM *FX 229,1 : REM Escape key generates ascii code (ignored by BB4SDL2 & Agon/Console8)
 1800 VDU 23,128,0,31,60,60,127,56,40,56 : REM Left half of car
 1810 VDU 23,129,0,192,112,60,254,28,20,28 : REM Right half of car
 1820 VDU 23,130,126,129,153,165,165,153,129,126 : REM Barrel
 1830 VDU 23,131,0,0,0,0,3,15,63,255 : REM Left half of up-ramp
 1840 VDU 23,132,0,12,60,252,252,252,252,252 : REM Right half of up-ramp
 1850 VDU 23,133,0,48,60,63,63,63,63,63 : REM Left half of down-ramp
 1860 VDU 23,134,0,0,0,0,192,240,252,255 : REM Right half of down-ramp
 1870 VDU 23,135,24,40,68,171,165,90,60,255 : REM First part of crash explosion
 1880 VDU 23,136,0,18,36,0,74,36,0,0 : REM Second part of crash explosion
 1890 IF platform$ = "SDL2" THEN ENVELOPE 1,5,0,0,0,0,0,0,126,-10,-5,-1,126,126 : REM Not implemented on Agon
 1900 IF platform$ = "SDL2" THEN ENVELOPE 2,3,2,4,2,2,-2,2,126,-10,-5,-1,126,126
 1910 ENDPROC
 1920 REM End of 'Acorn Programs' listing
 1930 :
 1940 :
 1950 REM [blufftek] Different use of sound depending on platform
 1960 DEFPROC_EndSound
 1970 IF platform$ = "SDL2" THEN SOUND 1,1,100,30 ELSE SOUND 1,-15,100,5
 1980 ENDPROC
 1990 :
 2000 REM Simulate noise (badly) on Agon
 2010 DEFPROC_CrashSoundAgon
 2020 FOR n = 255 TO 0 STEP -32
 2030   SOUND 0,-15,n,2
 2040 NEXT n
 2050 ENDPROC
 2060 :
 2070 REM [blufftek] Screen width
 2080 DEF FN_GetNumTextColumns
 2090 CLS : REM Start at top left of screen
 2100 ho% = 1
 2110 REPEAT
 2120   temp_ho% = ho%
 2130   PRINT " "; : REM Print a single character space at current cursor position
 2140   ho% = POS : REM Get horizontal position of cursor
 2150 UNTIL ho% < temp_ho% : REM Until cursor wraps to next line
 2160 = temp_ho% + 1
 2170 :
 2180 REM [blufftek] Screen height
 2190 DEF FN_GetNumTextRows
 2200 CLS : REM Start at top left of screen
 2210 ve% = 1
 2220 PRINT "*"
 2230 REPEAT
 2240   PRINT : REM Print a blank line
 2250   ve% = ve% + 1 : REM Increment vertical position
 2260 UNTIL GET$(0,0)<>"*" : REM Until "*" scrolls off top of screen
 2270 = ve%
 2280 :
 2290 :
 2300 REM [blufftek] Set specific SDL2 variables
 2310 DEFPROC_SetSDL2Vars
 2320 mo% = 9 : REM Screen mode
 2330 spacer% = 0 : REM Print on bottom row
 2340 ENDPROC
 2350 :
 2360 REM [blufftek] Set specific Agon variables
 2370 DEFPROC_SetAgonVars
 2380 mo% = 9 : REM Screen mode
 2390 spacer% = 1 : REM Don't print on bottom row
 2400 ENDPROC
 2410 :
 2420 :
 2430 REM Show cursor if flag% is TRUE
 2440 DEFPROC_ShowCursor(flag%)
 2450 IF flag% THEN VDU 23,1,1;0;0;0;0; ELSE VDU 23,1,0;0;0;0;
 2460 ENDPROC
 2470 :
 2480 :
 2490 DEFPROC_HandleErr
 2500 PROC_ShowCursor(TRUE):COLOUR 7:COLOUR 128:PRINT:REPORT:PRINT " at line ";ERL : END
 2510 ENDPROC
 2520 :
 2530 :
 2540 REM Print a box one character in from all four sides
 2550 DEFPROC_Box7
 2560 numTextRows% = 25 : numTextColumns% = 40 : REM In MODE 7
 2570 REM Top left corner
 2580 PRINT TAB(0, 1); : VDU gRED, TL_CORNER
 2590 REM Bottom left corner
 2600 PRINT TAB(0, numTextRows% - 2); : VDU gRED, BL_CORNER
 2610 REM Left & right sides
 2620 FOR y = 2 TO numTextRows% - 3
 2630   PRINT TAB(0, y); : REM Left side
 2640   VDU gRED, L_SIDE
 2650   PRINT TAB(37, y); : REM Right side
 2660   VDU gRED, R_SIDE
 2670 NEXT y
 2680 REM Top right corner
 2690 PRINT TAB(38, 1); : VDU TR_CORNER
 2700 REM Bottom right corner
 2710 PRINT TAB(38, numTextRows% - 2); : VDU BR_CORNER
 2720 REM Top and bottom sides
 2730 FOR x = 2 TO 37
 2740   PRINT TAB(x, 1); : VDU T_SIDE
 2750   PRINT TAB(x, numTextRows% - 2); : VDU B_SIDE
 2760 NEXT x
 2770 ENDPROC
 2780 :
 2790 :
 2800 DEFPROC_Intro
 2810 PRINT TAB(7, 3);
 2820 VDU aYELLOW
 2830 PRINT CHR$(dHEIGHT);"S T U N T   D R I V E R"
 2840 PRINT TAB(7, 4);
 2850 VDU aGREEN
 2860 PRINT CHR$(dHEIGHT);"S T U N T   D R I V E R"
 2870 PRINT TAB(11, 6);
 2880 VDU aCYAN
 2890 PRINT "by Gary Woolridge"
 2900 PRINT TAB(4, 7);
 2910 VDU aCYAN
 2920 PRINT "Acorn Programs  June/July 1984"
 2930 PRINT TAB(7, 9);
 2940 VDU aGREEN
 2950 PRINT "Use";CHR$(aMAGENTA);"DELETE";CHR$(aGREEN);"to accelerate,"
 2960 PRINT TAB(11, 10);
 2970 VDU aGREEN
 2980 PRINT CHR$(aMAGENTA);"RETURN";CHR$(aGREEN);"to jump"
 2990 PRINT TAB(4, 12);
 3000 VDU aRED
 3010 PRINT"Adapted for BBC Basic for SDL2"
 3020 PRINT TAB(10, 13);
 3030 VDU aRED
 3040 PRINT"and Agon/Console8"
 3050 PRINT TAB(12, 16);
 3060 VDU aRED
 3070 PRINT CHR$(aBLUE);CHR$(nBACKGROUND);CHR$(aCYAN);"Blufftek  ";CHR$(aBLACK);
 3080 PRINT TAB(16, 17);
 3090 VDU aBLACK
 3100 PRINT CHR$(aBLUE);"2024"
 3110 PRINT TAB(6, 20);
 3120 VDU aRED
 3130 PRINT"Press"+CHR$(FLASH)+CHR$(aMAGENTA)+"SPACE"+CHR$(aRED)+CHR$(nFLASH)+"to continue";
 3140 PRINT TAB((40 - LEN(platform$) - 2) DIV 2, 23);
 3150 VDU aBLUE
 3160 PRINT platform$;CHR$(gRED);
 3170 ENDPROC
 3180 :
 3190 :
 3200 DEFPROC_Outro
 3210 PRINT TAB(9, 8);
 3220 VDU aRED
 3230 PRINT"Thank you for playing"
 3240 PRINT TAB(7, 12);
 3250 VDU aYELLOW
 3260 PRINT CHR$(dHEIGHT);"S T U N T   D R I V E R"
 3270 PRINT TAB(7, 13);
 3280 VDU aGREEN
 3290 PRINT CHR$(dHEIGHT);"S T U N T   D R I V E R"
 3300 PRINT TAB((40 - LEN(platform$) - 2) DIV 2, 23);
 3310 VDU aBLUE
 3320 PRINT platform$;CHR$(gRED);
 3330 PRINT TAB(5, 20);CHR$(aWHITE);
 3340 ENDPROC
 3350 :
