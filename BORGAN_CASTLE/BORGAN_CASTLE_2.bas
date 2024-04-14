   10 REM /////////////////////////////////////////////////////////////////////
   20 REM //                   The Curse of Borgan Castle                    //
   30 REM //       An adventure game for the BBC and Electron Computers      //
   40 REM // <C> 1984 S.W. LUCAS (published in AcornPrograms June/July 1984) //
   50 REM /////////////////////////////////////////////////////////////////////
   60 REM //                       BORGAN_CASTLE_2.BBC                       //
   70 REM //                               v2.0                              //
   80 REM //              Adapted for BB4SDL2 & Agon/Console8                //
   90 REM //                     [Blufftek]  April 2024                      //
  100 REM /////////////////////////////////////////////////////////////////////
  110 :
  120 REM [blufftek] Find out what platform we're running on
  130 mo% = 0 : MODE mo%
  140 conHeight% = FN_GetNumTextRows
  150 IF conHeight% = 32 THEN platform$ = "SDL2"
  160 IF conHeight% = 60 THEN platform$ = "Agon/Console8" : REM VDP1.04 or later
  170 IF (conHeight% <> 32) AND (conHeight% <> 60) THEN CLS : PRINT "Unknown platform" : STOP
  180 :
  190 ON ERROR PROC_HandleErr : REM [blufftek]
  200 :
  210 REM [blufftek] Define physical colours
  220 BLACK=0:RED=1:GREEN=2:YELLOW=3:BLUE=4:MAGENTA=5:CYAN=6:WHITE=7
  230 GREY=8:iRED=9:iGREEN=10:iYELLOW=11:iBLUE=12:iMAGENTA=13:iCYAN=14:iWHITE=15
  240 :
  250 defaultForeground = BLUE : REM [blufftek] Default text colours
  260 defaultBackground = GREEN + 128
  270 :
  280 REM [blufftek] Set platform-specific variables
  290 IF platform$ = "SDL2" THEN PROC_SetSDL2Vars
  300 IF platform$ = "Agon/Console8" THEN PROC_SetAgonVars
  310 :
  320 REM // DON'T ADD THE NEXT THREE LINES UNTIL THE PROGRAM IS FULLY DEBUGGED AND SAVE A COPY BEFORE RUNNING IT //
  330 REM *FX229,1: REM Disable Escape key (ignored by Agon)
  340 REM ON ERROR X$="A ghost attacks me":PROClose
  350 REM *KEY 10 OLD|M RUN|M: REM Redefine Break key to re-run program (ignored by Agon)
  360 :
  370 MODE mo% : conWidth% = FN_GetNumTextColumns : conHeight% = FN_GetNumTextRows : REM Get screen size after MODE change
  380 PROC_ShowCursor(FALSE): REM [blufftek] Disable cursor
  390 COLOUR defaultForeground : COLOUR defaultBackground : REM [blufftek] Start with default text colours
  400 CLS: REM [blufftek] Ensure screen colour matches text background.
  410 :
  420 REM Main part of Acorn Programs listing starts here
  430 :
  440 PRINTTAB(7,1)"The Curse of Borgan Castle"''TAB(12)"An adventure game"''TAB(14)"by S.W. Lucas"
  450 S%=0:RESTORE480:FORX=1TO20:READ D:SOUND1,-15,D,3:NEXT
  460 DIMS%(24,4),Q$(23),G$(30),V$(4),B%(30,1),N$(30),N%(30)
  470 AA%=0:AB%=0:AC%=0:AD%=0:AE%=0:AF%=0:AG%=0:AH%=0:AI%=0:AJ%=0:AK%=0:AL%=0:AM%=0:AN%=0:AO%=0:AP%=0:AQ%=0:AR%=0:AS%=0:AT%=0:AU%=0:AV%=0:AW%=0:AX%=0:AY%=0:AZ%=0:P%=1
  480 DATA61,53,69,53,61,53,69,53,97,89,81,73,61,53,69,53,61,53,69,53
  490 PRINT''" I am standing at the gates of Borgan    Castle and am about to embark on a      dangerous mission."
  500 PRINT''" Please help me to rid the Castle of the evil forces which threaten anyone who   enters the castle gates."
  510 PRINT''" You must give me instructions in the    form of two word sentences such as      GET FOOD etc."
  520 FORX=1TO21:READQ$(X):FORY=1TO4:READS%(X,Y):NEXTY,X
  530   FORX=1TO13:READG$(X),B%(X,1),N$(X):N%(X)=X:NEXT
  540   PRINTTAB(1,22)"Press <SPACE BAR> when ready to start";
  550   REPEAT UNTIL GET=32: CLS
  560   REPEAT
  570     IFP%>10ANDAB%<2THENX$="I trip over something in the dark!":PROClose
  580     IFP%=14THENAH%=AH%+1:IFAH%>2THENX$="The spider comes over and bites me!":PROClose
  590     IFP%=8THENAI%=AI%+1:IFAI%>2THENX$="The ghost attacks me!":PROClose
  600     IFP%=17THENAJ%=AJ%+1:IFAJ%>2THENX$="The monster attacks!":PROClose
  610     PRINT'"I am :-"'Q$(P%)
  620     A$="":IFS%(P%,1)>0THENA$="North"
  630     IFS%(P%,2)>0ANDLEN(A$)>0THENA$=A$+",South" ELSEIFS%(P%,2)>0THENA$="South"
  640     IFS%(P%,3)>0ANDLEN(A$)>0THENA$=A$+",East" ELSEIFS%(P%,3)>0THENA$="East"
  650     IFS%(P%,4)>0ANDLEN(A$)>0THENA$=A$+",West." ELSEIFS%(P%,4)>0THENA$="West"
  660     PRINT''"I can go :-"'A$'
  670     E=0:FORT%=1TO13:PP%=0:IFB%(T%,1)=P%THENPP%=1
  680       IFPP%=1THEN700
  690     NEXT:GOTO720
  700     IFE=0THENPRINT"I can see :-"
  710     PRINT'G$(T%):E=E+1:GOTO690
  720     PRINT'"What should I do now?";:COLOUR RED:PROC_ShowCursor(TRUE):INPUT" "Z$:PROC_ShowCursor(FALSE):COLOUR defaultForeground:Z$=FN_upper(Z$): REM [blufftek] Enable cursor for input, use different colour, convert to upper case
  730     B$=LEFT$(Z$,2):C$=LEFT$(Z$,3):D$=LEFT$(Z$,4):CLS:SOUND 2,-15,124,3:K=0 : PROC_PrintPlatform : REM [blufftek] SOUND in lieu of VDU7 (VDU7 doesn't work on SDL2), Print platform at bottom of screen
  740     IFC$="LOO"THENK=1 ELSEIFC$="EAT"THENPRINT"I can't see any food!":K=1 ELSEIFC$="DRI"THENPRINT"I can't see anything to drink!":K=1 ELSEIFC$="SCO"THENK=1:PRINT"What do you think this is, a game?"
  750     IFC$="RUB"THENPRINT"Don't be ridiculous!":K=1 ELSEIF(B$="N"ORLEFT$(Z$,4)="GO N")ANDS%(P%,1)>0THENP%=S%(P%,1):K=1
  760     IF(B$="S"ORLEFT$(Z$,4)="GO S")ANDS%(P%,2)>0THENP%=S%(P%,2):K=1 ELSEIF(B$="E"ORLEFT$(Z$,4)="GO E")ANDS%(P%,3)>0THENP%=S%(P%,3):K=1 ELSEIF(B$="W"ORLEFT$(Z$,4)="GO W")ANDS%(P%,4)>0THENP%=S%(P%,4):K=1
  770     IFC$="RIN"THENK=1:PROCring ELSEIFC$="SWI"THENK=1:PROCswim ELSEIFC$="LIG"THENPROClight ELSEIFC$="THR"ORC$="CHU"THENPROCthrow
  780     IFC$="USE"ORC$="SHO"ORC$="HOL"THENPROCshow ELSEIFC$="I"ORD$="GO I"THENPROCin ELSEIFC$="OUT"ORC$="O"ORD$="GO O"THENPROCout
  790     IFC$="ROW"ORC$="SAI"THENPROCrow ELSEIFC$="UNL"THENPROCunlock
  800     IFC$="UP"ORD$="GO U"THENPROCup ELSEIFC$="DOW"ORD$="GO D"THENPROCdown
  810     IFC$="DRO"ORC$="LEA"ORC$="PUT"THENPROCdrop ELSEIFC$="INV"THENPROCinventory
  820     IFC$="PRI"ORC$="INS"THENPROCinsert ELSEIFC$="KIL"ORC$="ATT"ORC$="STA"ORC$="HIT"THENX$="A ghost sneaks up behind me and says    'I won't have that here!'":PROClose
  830     IFC$="EXA"THENK=1:PRINT"I can't see anything special!":ELSEIFC$="SEA"THENPRINT"I didn't find anything!":K=1
  840     IFK=0AND(B$="N"ORB$="S"ORB$="E"ORB$="W")THENPRINT"I can't go that way here!":K=1
  850     IFC$="GET"ORC$="TAK"ORC$="GRA"THENPROCget ELSEIFC$="HEL"THENK=1:PRINT"Sorry I can't think of anything!" ELSEIFC$="PRA"THENPRINT"That did me good!":K=1 ELSEIFB$="FU"ORB$="PI"THENPRINT"How dare you speak to me like that!":K=1
  860   UNTILS%=10
  870   CLS:PRINT''"That's done the trick!.. The Princess is free!"
  880   PRINT''"Well done!"''"You have solved the game!"
  890   RESTORE480:FORX=1TO20:READ D:SOUND1,-15,D,3:NEXT
  900   END
  910   DATAstanding by the open gateway of the     castle. A vicious dog prevents my escape,0,0,0,2,by a large metal gate. It is shut.,0,0,1,0,on a dirt track. There is a metal door  to the North but it is locked,0,0,4,0
  920   DATAby a moat. There is a drawbridge on the far side but it is up at the moment.,0,0,0,3,by a moat. The drawbridge is up. A wide passage leads into the castle.,0,6,0,0
  930   DATAin a large cobbled courtyard. There is a lake to the east.,5,0,7,10,at the side of a wide lake.,0,0,0,6,by a deep lake. A path leads west from  here.,0,0,0,9
  940   DATAby the South Turret.,0,0,8,0,by the West Turret,0,0,6,0,in a dirty room. Steps lead up from here,0,0,16,0,at the top of some steps,0,15,13,0,in a dirty room. Spiders are crawling   all over the furniture,0,14,0,12
  950   DATAby a window overlooking a courtyard.,13,0,0,15,in an empty room.,12,0,14,0,at the top of a spiral staircase,0,0,0,11
  960   DATAat the bottom of a spiral staircase,19,181,0,0,by a locked door,17,0,0,0,in an armoury,0,17,0,0,in the dungeons.,18,21,0,0,outside a locked cell.,20,0,0,0
  970   DATAa large bell,2,BELL,a box of matches,1,MATCHES,an oi1 1amp,10,LAMP,a small rowing boat with oars,6,BOAT,an evil looking ghost,8,GHOST,a gleaming sword,15,SWORD,a giant spider,14,SPIDER,a key,12,KEY
  980   DATAAn old prisoner locked behind bars,20,PRISONER
  990   DATAa beautiful princess,21,PRINCESS,a three headed monster,17,MONSTER,a crucifix,4,CRUCIFIX,a menacing vampire bat,10,VAMPIRE
 1000   :
 1010   DEFPROCget
 1020   K= 1:PROCcheck
 1030   IFL%<>1THENENDPROC
 1040   E%=0
 1050   FORH%=1TO13:IFB%(H%,1)=P%ANDB%(N%(R),1)=P%THENE%=1
 1060   NEXT
 1070   IFE%=0THEN PRINT"I can't see it here!":ENDPROC
 1080   IFR=1THEN PRINT"Don't be an idiot!":ENDPROC ELSE IFR=2ANDAA%=0THENAA%=1ELSEIFR=4THENAC%=1
 1090   IFR=3ANDAF%<>2THENX$="The Vampire sinks his teeth into my neck":PROClose ELSEIFR=3THENAB%=1
 1100   IFR=5THENPRINT"What an idiot!":ENDPROC ELSEIFR=7THENPRINT"That's a stupid idea!":ENDPROC ELSEIFR=9THENPRINT"He's locked in dummy!":ENDPROC
 1110   IFR=11THENPRINT"Don't be absurd!":ENDPROC ELSEIFR=13THENPRINT"That's a ridiculous idea! ":ENDPROC
 1120   IFR=8THENAD%=1 ELSEIFR=6THENAE%=1ELSEIFR=12THENAF%=1
 1130   E%=0:FORD%=1TO3:IFV$(D%)=""THENV$(D%)=G$(N%(R)):E%=1:D%=5
 1140   NEXT
 1150   IFE%=0THENPRINT"Sorry my hands are full!!":K=1:ENDPROC
 1160   B%(N%(R),1)=0:ENDPROC
 1170   :
 1180   DEFPROCcheck
 1190   L$="":FORH%=1TOLEN(Z$)
 1200     IFMID$(Z$,H%,1)=" "THENL$=RIGHT$(Z$,(LEN(Z$)-H%)):H%=H%+40
 1210   NEXT
 1220   R=0:L%=0:IFLEN(Z$)<2THENENDPROC
 1230   FORH%=1TO13:IFLEFT$(N$(H%),LEN(L$))=L$ THENL%=1:R=H%
 1240   NEXT
 1250   ENDPROC
 1260   :
 1270   DEFPROCdrop
 1280   PROCcheck
 1290   K=1:IFL%<>1THENPRINT "I can't see a ";L$:ENDPROC
 1300   E%=0
 1310   FORD%=1TO3:IFV$(D%)=G$(N%(R))THENV$(D%)="":E%=1
 1320   NEXT:IFE%<>1THENPRINT"I haven't got it!":ENDPROC
 1330   B%(N%(R),1)=P%
 1340   IFR=2ANDAA%=1THENAA%=0 ELSEIFR=2THENAA%=3
 1350   IFR=4THENAC%=0 ELSEIFR=6THENAE%=0 ELSEIFR=8THENAD%=0ELSEIFR=13THENAF%=0
 1360   IFR=3THENIFAB%=2THENPRINT"The lamp blows out!"
 1370   IFR=3THENAB%=0
 1380   ENDPROC
 1390   :
 1400   DEFPROCinventory
 1410   K=1:PRINT"I have :-":F%=0
 1420   FORH%=1TO3:IFV$(H%)<>""THENPRINTV$(H%):F%=1
 1430   NEXT:IFF%=0THENPRINT"Not a sausage!"
 1440   PRINT:ENDPROC
 1450   :
 1460   DEFPROCring
 1470   IFP%<>2THENPRINT"I can't do that here dummy!":ENDPROC ELSEPRINT"The door opens, a hand comes out and     drags me through. The door slams shut!":P%=3:ENDPROC
 1480   :
 1490   DEFPROCswim
 1500   IFP%=4THENPRINT"O.K.":IFAA%=1THENAA%=2:PRINT"The matches are wet!"
 1510   IFP%=4THENP%=5:ENDPROC
 1520   IFP%=5THENP%=4:PRINT"O.K.":IFAA%=1THENAA%=2:PRINT"The matches are wet!":ENDPROC
 1530   IFP%=7ORP%=8THENX$="I swim into a whirlpool and drown!":PROClose
 1540   PRINT"Don't be silly":ENDPROC
 1550   :
 1560   DEFPROClose
 1570   CLS:PRINT''X$''"I AM DEAD!!!!!!!!!!"
 1580   PRINT'''"WOULD YOU LIKE ANOTHER GAME?   <Y/N>"
 1590   RESTORE480:FORX=1TO20:READD:SOUND1,-15,D,3:NEXT
 1600   REPEAT A$=GET$
 1610   UNTIL A$="Y" OR A$="y" OR A$="N" OR A$="n": REM [blufftek] Allow for upper or lower case response
 1620   IFA$="Y"ORA$="y"THENRUN ELSEPRINT''"Goodbye thank you for playing"
 1630   END
 1640   ENDPROC
 1650   :
 1660   DEFPROClight
 1670   K=1:IFAB%=0THENPRINT"I haven't got the lamp!":ENDPROC ELSEIFAB%=2 THENPRINT"It's already lit!":ENDPROC
 1680   IFAA%=2THENPRINT"The matches are wet!":ENDPROC ELSEIFAA%<>1THENPRINT"I have no matches!":ENDPROC
 1690   PRINT"I light the lamp":AB%=2:FORV%=1TO3:IFV$(V%)=G$(3)THENV$(V%)="an oil lamp (LIT)"
 1700   NEXT:ENDPROC
 1710   :
 1720   DEFPROCthrow
 1730   K=1:PROCcheck
 1740   IFR<>2THENPRINT"I can't see much point in throwing that!":ENDPROC
 1750   IFAA%=0THENPRINT"I haven't got them":ENDPROC ELSEIFAA%<>1THENPRINT"I don't see much point in doing that!":ENDPROC
 1760   IFP%=5THENB%(N%(R),1)=0:FORV%=1TO3:IFV$(V%)=G$(2)THENV$(V%)="":AA%=0
 1770     IFP%=5THENNEXT:PRINT"They have fallen in the water!":ENDPROC
 1780   IFP%=4THENB%(N%(R),1)=5:FORV%=1TO3:IFV$(V%)=G$(2)THENVS(V%)="":AA%=0
 1790     IFP%=4THENNEXT
 1800   ENDPROC
 1810   :
 1820   DEFPROCshow
 1830   K=1:IFAF%< 1THENPRINT"I haven't got the crucifix!":ENDPROC
 1840   IFP%<>10THENPRINT"I can't see any point in doing that here":ENDPROC
 1850   IFAF%>1THENPRINT"The Vampire's gone!":ENDPROC
 1860   PRINT"The Vampire disappears in fright!":AF%=2:G$(13)="":ENDPROC
 1870   :
 1880   DEFPROCin
 1890   K=1: IFP%=10THENPRINT"The door is locked!":ENDPROC ELSEIFP%=9THENPRINT"O.K.":P%=11:ENDPROC
 1900   PRINT"I can't do that here!":ENDPROC
 1910   :
 1920   DEFPROCout
 1930   K=1:IFP%=11THENP%=9:PRINT"O.K.":ENDPROC
 1940   PRINT"I can't do that !":ENDPROC
 1950   :
 1960   DEFPROCrow
 1970   K=1:IFAC%<>1THENPRINT"Just how am I supposed to do that?":ENDPROC
 1980   IFP%=7THENP%=8:PRINT"O.K.":ENDPROC ELSEIFP%=8THENP%=7:PRINT"O.K.":ENDPROC
 1990   PRINT"I can't do that here!!!":ENDPROC
 2000   :
 2010   DEFPROCup
 2020   K=1:IFP%=11THENP%=12:PRINT"O.K.":ENDPROC ELSEIFP%=17THENP%=16:PRINT"O.K.":ENDPROC
 2030   PRINT"Not here!":ENDPROC
 2040   :
 2050   DEFPROCdown
 2060   K=1:IFP%=12THENP%=11:PRINT"O.K.":ENDPROC ELSEIFP%=16THENP%=17:PRINT"O.K.":ENDPROC
 2070   PRINT"Not here!":ENDPROC
 2080   :
 2090   DEFPROCunlock
 2100   K=1:IFP%=21THENPRINT"The key doesn't work!":ENDPROC ELSEIFP%<>18THENPRINT"Just how am I supposed to do that here?":ENDPROC
 2110   IFAD%<>1THENPRINT"I don’t have the key":ENDPROC
 2120   PRINT"I unlock the door":Q$(18)="By an unlocked door":S%(18,2)=20:ENDPROC
 2130   :
 2140   DEFPROCinsert
 2150   K=1: IFAE%<>1THENPRINT"I don't have anything to do that with!":ENDPROC
 2160   IFP%<>21THENPRINT"I can't do that here!":ENDPROC
 2170   S%=10:ENDPROC
 2180   :
 2190   REM End of Acorn Programs listing
 2200   :
 2210   DEF FN_upper(IN$): REM [blufftek] Convert string to upper case.
 2220   LOCAL Z$, L% : Z$=""
 2230   FOR J=1 TO LEN(IN$)
 2240     L% = ASC(MID$(IN$,J,1))
 2250     IF L%>=97 AND L%<=122 THEN L% = L%-32
 2260     Z$=Z$+CHR$(L%)
 2270   NEXT J
 2280   =Z$
 2290   :
 2300   REM [blufftek] Screen width
 2310   DEF FN_GetNumTextColumns
 2320   CLS : REM Start at top left of screen
 2330   ho% = 1
 2340   REPEAT
 2350     temp_ho% = ho%
 2360     PRINT " "; : REM Print a single character space at current cursor position
 2370     ho% = POS : REM Get horizontal position of cursor
 2380   UNTIL ho% < temp_ho% : REM Until cursor wraps to next line
 2390   = temp_ho% + 1
 2400   :
 2410   REM [blufftek] Screen height
 2420   DEF FN_GetNumTextRows
 2430   CLS : REM Start at top left of screen
 2440   ve% = 1
 2450   PRINT "*"
 2460   REPEAT
 2470     PRINT : REM Print a blank line
 2480     ve% = ve% + 1 : REM Increment vertical position
 2490   UNTIL GET$(0,0)<>"*" : REM Until "*" scrolls off top of screen
 2500   = ve%
 2510   :
 2520   REM [blufftek] Set specific SDL2 variables
 2530   DEFPROC_SetSDL2Vars
 2540   mo% = 6 : REM Screen mode
 2550   spacer% = 0 : REM Print on bottom row
 2560   ENDPROC
 2570   :
 2580   REM [blufftek] Set specific Agon variables
 2590   DEFPROC_SetAgonVars
 2600   mo% = 9 : REM Screen mode
 2610   spacer% = 1 : REM Don't print on bottom row
 2620   ENDPROC
 2630   :
 2640   DEFPROC_PrintPlatform
 2650   l% = LEN(platform$)
 2660   COLOUR MAGENTA
 2670   PRINT TAB((conWidth% - l%) DIV 2, conHeight% - 1 - spacer%);platform$;
 2680   PRINT TAB(0, 0);
 2690   COLOUR defaultForeground
 2700   ENDPROC
 2710   :
 2720   REM Show cursor if flag% is TRUE
 2730   DEFPROC_ShowCursor(flag%)
 2740   IF flag% THEN VDU 23,1,1;0;0;0;0; ELSE VDU 23,1,0;0;0;0;
 2750   ENDPROC
 2760   :
 2770   DEFPROC_HandleErr
 2780   PROC_ShowCursor(TRUE):COLOUR WHITE:COLOUR 128+BLACK:PRINT:REPORT:PRINT " at line ";ERL:END
 2790   ENDPROC
 2800   :
