   10 REM
   20 REM     ELIZA/DOCTOR
   30 REM     CREATED BY JOSEPH WEIZENBAUM
   40 REM     THIS VERSION BY JEFF SHRAGER
   50 REM     EDITED AND MODIFIED FOR MITS 8K BASIC 4.0 BY STEVE NORTH
   60 REM     CREATIVE COMPUTING PO BOX 789-M MORRISTOWN NJ 07960
   70 REM
   80 REM     Adapted by Blufftek for BB4SDL2 & Agon/Console8 March 2024
   90 REM
  100 :
  110 REM [blufftek] Find out what platform we're running on
  120 mo% = 0 : MODE mo%
  130 conHeight% = FN_GetNumTextRows
  140 IF conHeight% = 32 THEN platform$ = "SDL2"
  150 IF conHeight% = 60 THEN platform$ = "Agon/Console8" : REM VDP1.04 or later
  160 IF (conHeight% <> 32) AND (conHeight% <> 60) THEN CLS : PRINT "Unknown platform" : STOP
  170 :
  180 ON ERROR PROC_HandleErr : REM [blufftek]
  190 :
  200 REM [blufftek] Define physical colours
  210 BLACK=0:RED=1:GREEN=2:YELLOW=3:BLUE=4:MAGENTA=5:CYAN=6:WHITE=7
  220 GREY=8:iRED=9:iGREEN=10:iYELLOW=11:iBLUE=12:iMAGENTA=13:iCYAN=14:iWHITE=15
  230 :
  240 REM [blufftek] Set platform-specific variables
  250 IF platform$ = "SDL2" THEN PROC_SetSDL2Vars
  260 IF platform$ = "Agon/Console8" THEN PROC_SetAgonVars
  270 :
  280 MODE mo%
  290 conWidth% = FN_GetNumTextColumns : conHeight% = FN_GetNumTextRows - spacer% : CLS : PRINT : REM [blufftek] Re-check screen size after MODE change
  300 :
  310 PROC_Intro
  320 fgColour = boxColour : bgColour = introBgColour
  330 PROC_Box(fgColour, bgColour) : REM [blufftek] Draw coloured box around edge of screen
  340 COLOUR userColour
  350 :
  360 REM [blufftek] Original ELIZA program starts here
  370 :
  380 REM
  390 REM     -----INITIALIZATION-----
  400 REM
  410 C$ = "" : I$ = "" : K$ = "" : F$ = "" : S$ = "" : R$ = "" : P$ = "" : Z$ = "" : REM [blufftek] Initialise phrase strings
  420 DIM S(36),R(36),N(36)
  430 N1=36:N2=12:N3=112
  440 FOR X=1 TO N1+N2+N3:READ Z$:NEXT X:REM SAME AS RESTORE
  450 FORX=1 TO N1
  460   READ S(X),L:R(X)=S(X):N(X)=S(X)+L-1
  470 NEXT X
  480 COLOUR elizaColour
  490 PRINT "HI!  I'M ELIZA.  WHAT'S YOUR PROBLEM?"
  500 REM
  510 REM     -----USER INPUT SECTION-----
  520 REM
  530 COLOUR userColour : INPUT I$ : I$ = FN_upper_case(I$) : COLOUR elizaColour : REM [blufftek] Upper case & colour
  540 I$=" "+I$+"  "
  550 REM  GET RID OF APOSTROPHES
  560 FOR L=1 TO LEN(I$)
  570   IFMID$(I$,L,1)="'"THENI$=LEFT$(I$,L-1)+RIGHT$(I$,LEN(I$)-L):GOTO570
  580   IFL+4<=LEN(I$)THENIFMID$(I$,L,4)="SHUT"THENPRINT"SHUT UP...":PRINT:COLOUR 7:COLOUR 128:END : REM [blufftek] Restore colours before ending
  590 NEXT L
  600 IF I$=P$ THEN PRINT "PLEASE DON'T REPEAT YOURSELF!":GOTO 500
  610 REM
  620 REM     -----FIND KEYWORD IN I$-----
  630 REM
  640 RESTORE
  650 S=0
  660 FOR K=1 TO N1
  670   READ K$
  680   IF S>0 THEN720
  690   FOR L=1 TO LEN(I$)-LEN(K$)+1
  700     IF MID$(I$,L,LEN(K$))=K$THENS=K:T=L:F$=K$
  710   NEXT L
  720 NEXT K
  730 IF S>0 THEN K=S:L=T:GOTO760
  740 K=36:GOTO970:REM  WE DIDN'T FIND ANY KEYWORDS
  750 REM
  760 REM     TAKE RIGHT PART OF STRING AND CONJUGATE IT
  770 REM     USING THE LIST OF STRINGS TO BE SWAPPED
  780 REM
  790 RESTORE:FORX=1 TO N1:READ Z$:NEXT X:REM SKIP OVER KEYWORDS
  800 C$=" "+RIGHT$(I$,LEN(I$)-LEN(F$)-L+1)
  810 FOR X=1 TO N2/2
  820   READ S$,R$
  830   FOR L= 1 TO LEN(C$)
  840     IF L+LEN(S$)>LEN(C$) THEN 890
  850     IF MID$(C$,L,LEN(S$))<>S$ THEN 890
  860     C$=LEFT$(C$,L-1)+R$+RIGHT$(C$,LEN(C$)-L-LEN(S$)+1)
  870     L=L+LEN(R$)
  880     GOTO 930
  890     IF L+LEN(R$)>LEN(C$)THEN930
  900     IF MID$(C$,L,LEN(R$))<>R$ THEN 930
  910     C$=LEFT$(C$,L-1)+S$+RIGHT$(C$,LEN(C$)-L-LEN(R$)+1)
  920     L=L+LEN(S$)
  930   NEXT L
  940 NEXT X
  950 IF MID$(C$,2,1)=" "THENC$=RIGHT$(C$,LEN(C$)-1):REM ONLY 1 SPACE
  960 REM
  970 REM     NOW USING THE KEYWORD NUMBER (K) GET REPLY
  980 REM
  990 RESTORE:FOR X= 1 TO N1+N2:READ Z$:NEXT X
 1000 FORX=1TOR(K):READ F$:NEXT X:REM  READ RIGHT REPLY
 1010 R(K)=R(K)+1: IFR(K)>N(K) THEN R(K)=S(K)
 1020 IF RIGHT$(F$,1)<>"*" THEN PRINT F$:P$=I$:GOTO 500
 1030 PRINT LEFT$(F$,LEN(F$)-1);C$
 1040 P$=I$:GOTO 500
 1050 REM
 1060 REM     -----PROGRAM DATA FOLLOWS-----
 1070 REM
 1080 REM     KEYWORDS
 1090 REM
 1100 DATA "CAN YOU","CAN I","YOU ARE","YOURE","I DONT","I FEEL"
 1110 DATA "WHY DONT YOU","WHY CANT I","ARE YOU","I CANT","I AM","IM "
 1120 DATA "YOU ","I WANT","WHAT","HOW","WHO","WHERE","WHEN","WHY"
 1130 DATA "NAME","CAUSE","SORRY","DREAM","HELLO","HI ","MAYBE"
 1140 DATA " NO","YOUR","ALWAYS","THINK","ALIKE","YES","FRIEND"
 1150 DATA "COMPUTER","NOKEYFOUND"
 1160 REM
 1170 REM     STRING DATA FOR CONJUGATIONS
 1180 REM
 1190 DATA " ARE "," AM ","WERE ","WAS "," YOU "," I ","YOUR ","MY "
 1200 DATA " IVE "," YOUVE "," IM "," YOURE "
 1210 REM
 1220 REM     REPLIES
 1230 REM
 1240 DATA "DON'T YOU BELIEVE THAT I CAN*"
 1250 DATA "PERHAPS YOU WOULD LIKE TO BE ABLE TO*"
 1260 DATA "YOU WANT ME TO BE ABLE TO*"
 1270 DATA "PERHAPS YOU DON'T WANT TO*"
 1280 DATA "DO YOU WANT TO BE ABLE TO*"
 1290 DATA "WHAT MAKES YOU THINK I AM*"
 1300 DATA "DOES IT PLEASE YOU TO BELIEVE I AM*"
 1310 DATA "PERHAPS YOU WOULD LIKE TO BE*"
 1320 DATA "DO YOU SOMETIMES WISH YOU WERE*"
 1330 DATA "DON'T YOU REALLY*"
 1340 DATA "WHY DON'T YOU*"
 1350 DATA "DO YOU WISH TO BE ABLE TO*"
 1360 DATA "DOES THAT TROUBLE YOU?"
 1370 DATA "TELL ME MORE ABOUT SUCH FEELINGS."
 1380 DATA "DO YOU OFTEN FEEL*"
 1390 DATA "DO YOU ENJOY FEELING*"
 1400 DATA "DO YOU REALLY BELIEVE I DON'T*"
 1410 DATA "PERHAPS IN GOOD TIME I WILL*"
 1420 DATA "DO YOU WANT ME TO*"
 1430 DATA "DO YOU THINK YOU SHOULD BE ABLE TO*"
 1440 DATA "WHY CAN'T YOU*"
 1450 DATA "WHY ARE YOU INTERESTED IN WHETHER OR NOT I AM*"
 1460 DATA "WOULD YOU PREFER IF I WERE NOT*"
 1470 DATA "PERHAPS IN YOUR FANTASIES I AM*"
 1480 DATA "HOW DO YOU KNOW YOU CAN'T*"
 1490 DATA "HAVE YOU TRIED?"
 1500 DATA "PERHAPS YOU CAN NOW*"
 1510 DATA "DID YOU COME TO ME BECAUSE YOU ARE*"
 1520 DATA "HOW LONG HAVE YOU BEEN*"
 1530 DATA "DO YOU BELIEVE IT IS NORMAL TO BE*"
 1540 DATA "DO YOU ENJOY BEING*"
 1550 DATA "WE WERE DISCUSSING YOU-- NOT ME."
 1560 DATA "OH, I*"
 1570 DATA "YOU'RE NOT REALLY TALKING ABOUT ME, ARE YOU?"
 1580 DATA "WHAT WOULD IT MEAN TO YOU IF YOU GOT*"
 1590 DATA "WHY DO YOU WANT*"
 1600 DATA "SUPPOSE YOU SOON GOT*"
 1610 DATA "WHAT IF YOU NEVER GOT*"
 1620 DATA "I SOMETIMES ALSO WANT*"
 1630 DATA "WHY DO YOU ASK?"
 1640 DATA "DOES THAT QUESTION INTEREST YOU?"
 1650 DATA "WHAT ANSWER WOULD PLEASE YOU THE MOST?"
 1660 DATA "WHAT DO YOU THINK?"
 1670 DATA "ARE SUCH QUESTIONS ON YOUR MIND OFTEN?"
 1680 DATA "WHAT IS IT THAT YOU REALLY WANT TO KNOW?"
 1690 DATA "HAVE YOU ASKED ANYONE ELSE?"
 1700 DATA "HAVE YOU ASKED SUCH QUESTIONS BEFORE?"
 1710 DATA "WHAT ELSE COMES TO MIND WHEN YOU ASK THAT?"
 1720 DATA "NAMES DON'T INTEREST ME."
 1730 DATA "I DON'T CARE ABOUT NAMES-- PLEASE GO ON."
 1740 DATA "IS THAT THE REAL REASON?"
 1750 DATA "DON'T ANY OTHER REASONS COME TO MIND?"
 1760 DATA "DOES THAT REASON EXPLAIN ANYTHING ELSE?"
 1770 DATA "WHAT OTHER REASONS MIGHT THERE BE?"
 1780 DATA "PLEASE DON'T APOLOGIZE!"
 1790 DATA "APOLOGIES ARE NOT NECESSARY."
 1800 DATA "WHAT FEELINGS DO YOU HAVE WHEN YOU APOLOGIZE."
 1810 DATA "DON'T BE SO DEFENSIVE!"
 1820 DATA "WHAT DOES THAT DREAM SUGGEST TO YOU?"
 1830 DATA "DO YOU DREAM OFTEN?"
 1840 DATA "WHAT PERSONS APPEAR IN YOUR DREAMS?"
 1850 DATA "ARE YOU DISTURBED BY YOUR DREAMS?"
 1860 DATA "HOW DO YOU DO ... PLEASE STATE YOUR PROBLEM."
 1870 DATA "YOU DON'T SEEM QUITE CERTAIN."
 1880 DATA "WHY THE UNCERTAIN TONE?"
 1890 DATA "CAN'T YOU BE MORE POSITIVE?"
 1900 DATA "YOU AREN'T SURE?"
 1910 DATA "DON'T YOU KNOW?"
 1920 DATA "ARE YOU SAYING NO JUST TO BE NEGATIVE?"
 1930 DATA "YOU ARE BEING A BIT NEGATIVE."
 1940 DATA "WHY NOT?"
 1950 DATA "ARE YOU SURE?"
 1960 DATA "WHY NO?"
 1970 DATA "WHY ARE YOU CONCERNED ABOUT MY*"
 1980 DATA "WHAT ABOUT YOUR OWN*"
 1990 DATA "CAN YOU THINK OF A SPECIFIC EXAMPLE?"
 2000 DATA "WHEN?"
 2010 DATA "WHAT ARE YOU THINKING OF?"
 2020 DATA "REALLY, ALWAYS?"
 2030 DATA "DO YOU REALLY THINK SO?"
 2040 DATA "BUT YOU ARE NOT SURE YOU*"
 2050 DATA "DO YOU DOUBT YOU*"
 2060 DATA "IN WHAT WAY?"
 2070 DATA "WHAT RESEMBLANCE DO YOU SEE?"
 2080 DATA "WHAT DOES THE SIMILARITY SUGGEST TO YOU?"
 2090 DATA "WHAT OTHER CONNECTIONS DO YOU SEE?"
 2100 DATA "COULD THERE REALLY BE SOME CONNECTION?"
 2110 DATA "HOW?"
 2120 DATA "YOU SEEM QUITE POSITIVE."
 2130 DATA "ARE YOU SURE?"
 2140 DATA "I SEE."
 2150 DATA "I UNDERSTAND."
 2160 DATA "WHY DO YOU BRING UP THE TOPIC OF FRIENDS?"
 2170 DATA "DO YOUR FRIENDS WORRY YOU?"
 2180 DATA "DO YOUR FRIENDS PICK ON YOU?"
 2190 DATA "ARE YOU SURE YOU HAVE ANY FRIENDS?"
 2200 DATA "DO YOU IMPOSE ON YOUR FRIENDS?"
 2210 DATA "PERHAPS YOUR LOVE FOR FRIENDS WORRIES YOU."
 2220 DATA "DO COMPUTERS WORRY YOU?"
 2230 DATA "ARE YOU TALKING ABOUT ME IN PARTICULAR?"
 2240 DATA "ARE YOU FRIGHTENED BY MACHINES?"
 2250 DATA "WHY DO YOU MENTION COMPUTERS?"
 2260 DATA "WHAT DO YOU THINK MACHINES HAVE TO DO WITH YOUR PROBLEM?"
 2270 DATA "DON'T YOU THINK COMPUTERS CAN HELP PEOPLE?"
 2280 DATA "WHAT IS IT ABOUT MACHINES THAT WORRIES YOU?"
 2290 DATA "SAY, DO YOU HAVE ANY PSYCHOLOGICAL PROBLEMS?"
 2300 DATA "WHAT DOES THAT SUGGEST TO YOU?"
 2310 DATA "I SEE."
 2320 DATA "I'M NOT SURE I UNDERSTAND YOU FULLY."
 2330 DATA "COME COME ELUCIDATE YOUR THOUGHTS."
 2340 DATA "CAN YOU ELABORATE ON THAT?"
 2350 DATA "THAT IS QUITE INTERESTING."
 2360 REM
 2370 REM     DATA FOR FINDING RIGHT REPLIES
 2380 REM
 2390 DATA 1,3,4,2,6,4,6,4,10,4,14,3,17,3,20,2,22,3,25,3
 2400 DATA 28,4,28,4,32,3,35,5,40,9,40,9,40,9,40,9,40,9,40,9
 2410 DATA 49,2,51,4,55,4,59,4,63,1,63,1,64,5,69,5,74,2,76,4
 2420 DATA 80,3,83,7,90,3,93,6,99,7,106,6
 2430 :
 2440 DEFPROC_Intro
 2450 PROC_ShowCursor(FALSE)
 2460 REM COLOUR introBoxColour
 2470 PROC_Box(introBoxColour, introBgColour + 128)
 2480 REM COLOUR introTextColour : COLOUR introBgColour + 128 : CLS
 2490 PROC_CentreText("Welcome  to", introTextColour)
 2500 PRINT
 2510 PROC_CentreText("E L I Z A", iGREEN)
 2520 PRINT
 2530 PROC_CentreText("An early 'chatbot'", introTextColour)
 2540 PRINT
 2550 PROC_CentreText("Developed from 1964 to 1967", introTextColour)
 2560 PROC_CentreText("by Joseph Weizenbaum", introTextColour)
 2570 PRINT
 2580 PROC_CentreText("This version by Jeff Shrager", introTextColour)
 2590 PRINT
 2600 PROC_CentreText("Modified for MITS 8K BASIC 4.0", introTextColour)
 2610 PROC_CentreText("by Steve North", introTextColour)
 2620 PRINT
 2630 PROC_CentreText("https://en.wikipedia.org/wiki/ELIZA", MAGENTA)
 2640 PRINT
 2650 PROC_CentreText("Adapted for BBC BASIC for SDL2", introTextColour)
 2660 PROC_CentreText("and Agon/Console8 single-board computers", introTextColour)
 2670 PRINT :PRINT
 2680 PROC_CentreText("Blufftek 2024", iGREEN)
 2690 PRINT : PRINT
 2700 PROC_CentreText("Press <SPACE> to continue", WHITE)
 2710 REPEAT UNTIL INKEY(0)=-1 : REM Flush keyboard buffer
 2720 REPEAT UNTIL INKEY$(1000) = " "
 2730 VDU 26 : CLS : REM Restore windows and clear screen
 2740 COLOUR userColour
 2750 PROC_ShowCursor(TRUE)
 2760 ENDPROC
 2770 REM [blufftek] Convert a string to upper case
 2780 DEF FN_upper_case(IN$)
 2790 LOCAL Z$, L% : Z$=""
 2800 FOR J=1 TO LEN(IN$)
 2810   L% = ASC(MID$(IN$,J,1))
 2820   IF L%>=97 AND L%<=122 THEN L% = L%-32
 2830   Z$=Z$+CHR$(L%)
 2840 NEXT J
 2850 =Z$
 2860 :
 2870 REM [blufftek] Screen width
 2880 DEF FN_GetNumTextColumns
 2890 CLS : REM Start at top left of screen
 2900 ho% = 1
 2910 REPEAT
 2920   temp_ho% = ho%
 2930   PRINT " "; : REM Print a single character space at current cursor position
 2940   ho% = POS : REM Get horizontal position of cursor
 2950 UNTIL ho% < temp_ho% : REM Until cursor wraps to next line
 2960 = temp_ho% + 1
 2970 :
 2980 REM [blufftek] Screen height
 2990 DEF FN_GetNumTextRows
 3000 CLS : REM Start at top left of screen
 3010 ve% = 1
 3020 PRINT "*"
 3030 REPEAT
 3040   PRINT : REM Print a blank line
 3050   ve% = ve% + 1 : REM Increment vertical position
 3060 UNTIL GET$(0,0)<>"*" : REM Until "*" scrolls off top of screen
 3070 = ve%
 3080 :
 3090 REM [blufftek] Set specific SDL2 variables
 3100 DEFPROC_SetSDL2Vars
 3110 mo% = 8 : REM Screen modes 8, 10, 11 suitable
 3120 elizaColour = iYELLOW : userColour = iCYAN : boxColour = RED
 3130 introBoxColour = iBLUE : introTextColour = YELLOW : introBgColour = BLACK
 3140 spacer% = 0 : REM Print on bottom row
 3150 ENDPROC
 3160 :
 3170 REM [blufftek] Set specific Agon variables
 3180 DEFPROC_SetAgonVars
 3190 mo% = 4 : REM Screen mode
 3200 elizaColour = iYELLOW : userColour = iCYAN : boxColour = RED
 3210 introBoxColour = iBLUE : introTextColour = YELLOW : introBgColour = BLACK
 3220 spacer% = 1 : REM Don't print on bottom row
 3230 ENDPROC
 3240 :
 3250 REM Show cursor if flag% is TRUE
 3260 DEFPROC_ShowCursor(flag%)
 3270 IF flag% THEN VDU 23,1,1;0;0;0;0; ELSE VDU 23,1,0;0;0;0;
 3280 ENDPROC
 3290 :
 3300 DEFPROC_HandleErr
 3310 PROC_ShowCursor(TRUE):COLOUR 7:COLOUR 128:PRINT:REPORT:PRINT " at line ";ERL : END
 3320 ENDPROC
 3330 :
 3340 DEFPROC_Box(fgColour, bgColour)
 3350 COLOUR bgColour : CLS : REM Set background colour
 3360 COLOUR fgColour
 3370 REM Box characters (UDG)
 3380 top_line_char$ = CHR$(248)
 3390 right_line_char$ = CHR$(249)
 3400 bottom_line_char$ = CHR$(250)
 3410 left_line_char$ = CHR$(251)
 3420 tl_corner_char$ = CHR$(252)
 3430 tr_corner_char$ = CHR$(253)
 3440 br_corner_char$ = CHR$(254)
 3450 bl_corner_char$ = CHR$(255)
 3460 :
 3470 REM Redefine graphics chars
 3480 PROC_UDG
 3490 :
 3500 REM Top line of screen
 3510 FOR J = 0 TO conWidth%-1:
 3520   PRINT TAB(J, 0);top_line_char$;
 3530 NEXT J
 3540 :
 3550 REM Right edge of screen
 3560 FOR J = 0 TO conHeight%-1
 3570   PRINT TAB(conWidth%-1, J);right_line_char$;
 3580 NEXT J
 3590 :
 3600 REM Bottom line of screen
 3610 FOR J = conWidth%-1 TO 0 STEP -1
 3620   PRINT TAB(J, conHeight%-1);bottom_line_char$;
 3630 NEXT J
 3640 :
 3650 REM Left edge of screen
 3660 FOR J = conHeight%-1 TO 0 STEP -1
 3670   PRINT TAB(0, J);left_line_char$;
 3680 NEXT J
 3690 :
 3700 REM Print corners
 3710 PRINT TAB(0, 0);tl_corner_char$;
 3720 PRINT TAB(conWidth%-1, 0);tr_corner_char$;
 3730 PRINT TAB(conWidth%-1, conHeight%-1);br_corner_char$;
 3740 PRINT TAB(0, conHeight%-1);bl_corner_char$;
 3750 PRINT TAB((conWidth% - LEN(platform$)-2) DIV 2, conHeight%-1);" ";platform$;" ";
 3760 :
 3770 REM Set text viewport to area inside box (spaced by one character)
 3780 VDU 28, 2, conHeight%-3, conWidth%-3, 2
 3790 ENDPROC
 3800 :
 3810 REM User Defined Graphics
 3820 DEF PROC_UDG
 3830 VDU 23, 248, 0, 255, 0, 255, 0, 255, 0, 0 : REM top_line_char$
 3840 VDU 23, 249, 42, 42, 42, 42, 42, 42, 42, 42 : REM right_line_char$
 3850 VDU 23, 250, 0, 0, 255, 0, 255, 0, 255, 0 : REM bottom_line_char$
 3860 VDU 23, 251, 84, 84, 84, 84, 84, 84, 84, 84 : REM left_line_char$
 3870 VDU 23, 252, 0, 127, 64, 95, 80, 87, 84, 84 : REM tl_corner_char$
 3880 VDU 23, 253, 0, 254, 2, 250, 10, 234, 42, 42 : REM tr_corner_char$
 3890 VDU 23, 254, 42, 42, 234, 10, 250, 2, 254, 0 : REM br_corner_char$
 3900 VDU 23, 255, 84, 84, 87, 80, 95, 64, 127, 0 : REM bl_corner_char$
 3910 ENDPROC
 3920 :
 3930 REM Centre a line of text, in a specified colour
 3940 DEFPROC_CentreText(text$, textColour)
 3950 l% = LEN(text$)
 3960 COLOUR textColour
 3970 PRINT TAB((conWidth% - 4 - l%) DIV 2);text$
 3980 COLOUR userColour
 3990 ENDPROC