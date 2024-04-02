STUNT DRIVER was originally published in the June/July 1984 issue of Acorn Programs magazine, written by Gary Woolridge.

This is the text of the description:

> "Picture yourself as Evil Knievel in this program by Gary Woolridge, of Handsworth, Birmingham. Using SHIFT [See note below] to accelerate and RETURN to jump, your aim is to race your stunt car along the ramps and over the barrels. After every successful jump another barrel is added to the row.

> "You have three lives and each barrel you clear without crashing is worth 10 points. Stunt Driver will run on the BBC B or the Electron."

Ported to BBC Basic for SDL2 on an M1 Mac Mini in March 2024. Current version (STUNT_DRIVER_2A.bas) runs more or less correctly on BB4SDL2 and on the fab-agon-emulator, but not on the AgonLight2 with VDP1.04 and not on the Console8 with VDP2.7.0 (watch this space). The bugs in the Intro and Outro screens are likely to be due to my unfamiliarity with MODE 7 teletext graphics.

Note: SHIFT has been changed to DELETE since the key-scanning method used doesn't work with SHIFT on the Agon.

(I have added REM statements to the entire listing in an effort to understand how the published program works. Some of these are likely to be incorrect.)