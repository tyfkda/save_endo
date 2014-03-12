#!ruby

# Create call function code.
#
# Usage:
#  $ ruby tools/create-call.rb init contest-1998 terminate
#  IIPIFFCPICCFPICICFFFIIPIFFCPICCFPICICFFFIICIICIICIPPPCFFFCCCCCFCFCCFFFFCCCFICCFFFCCCCCFCFICIPPCPIICIIPIFFCPICCFPICICFFFIIPIFFCPICCFPICICFFFIICIICIICIPPPFFCCFFFCFFCFFCFCCFCFFICCFFCFCCFFCFFFFFICIPPCPIICIIPIFFCPICCFPICICFFFIIPIFFCPICCFPICICFFFIICIICIICIPPPFFFFCCCCCFFFCCFCCFCCCFICCFCFCCCFFICIPPCPIIC

FuncTable = {
  "AAA_geneTablePageNr" => { :offset => 0x000510, :size => 0x000018 },
  "M-class-planet" => { :offset => 0x2ccd88, :size => 0x03c7f0 },
  "__array_index" => { :offset => 0x0c4589, :size => 0x000018 },
  "__array_value" => { :offset => 0x0c45a1, :size => 0x000018 },
  "__bool" => { :offset => 0x0c45e9, :size => 0x000001 },
  "__bool_2" => { :offset => 0x0c45ea, :size => 0x000001 },
  "__funptr" => { :offset => 0x0c45b9, :size => 0x000030 },
  "__int1" => { :offset => 0x0c461b, :size => 0x000001 },
  "__int12" => { :offset => 0x0c4628, :size => 0x00000c },
  "__int12_2" => { :offset => 0x0c4634, :size => 0x00000c },
  "__int24" => { :offset => 0x0c45eb, :size => 0x000018 },
  "__int24_2" => { :offset => 0x0c4603, :size => 0x000018 },
  "__int3" => { :offset => 0x0c4625, :size => 0x000003 },
  "__int48" => { :offset => 0x0c4640, :size => 0x000030 },
  "__int9" => { :offset => 0x0c461c, :size => 0x000009 },
  "acc1" => { :offset => 0x0c4541, :size => 0x000018 },
  "acc2" => { :offset => 0x0c4559, :size => 0x000018 },
  "acc3" => { :offset => 0x0c4571, :size => 0x000018 },
  "activateAdaptationTree" => { :offset => 0x6fce9c, :size => 0x000b02 },
  "activateGene" => { :offset => 0x6fd99e, :size => 0x000273 },
  "adapter" => { :offset => 0x252fa1, :size => 0x0006db },
  "addFunctionsCBF" => { :offset => 0x41b532, :size => 0x0016ce },
  "addInts" => { :offset => 0x54b1ba, :size => 0x000325 },
  "anticompressant" => { :offset => 0x5580c4, :size => 0x002b42 },
  "apple" => { :offset => 0x65f785, :size => 0x0003fb },
  "appletree" => { :offset => 0x3c870e, :size => 0x00372b },
  "apply1_adaptation" => { :offset => 0x711dc6, :size => 0x000048 },
  "apply2_adaptation" => { :offset => 0x719633, :size => 0x000048 },
  "balloon" => { :offset => 0x6f31b6, :size => 0x001a83 },
  "beginRelativeMode" => { :offset => 0x6f08a9, :size => 0x0002af },
  "bioAdd_adaptation" => { :offset => 0x610436, :size => 0x000258 },
  "bioMorphPerturb" => { :offset => 0x0c9229, :size => 0x000588 },
  "bioMul_adaptation" => { :offset => 0x719153, :size => 0x000498 },
  "bioSucc_adaptation" => { :offset => 0x71068e, :size => 0x0000f0 },
  "bioZero_adaptation" => { :offset => 0x7103a6, :size => 0x000090 },
  "biomorph_adaptation" => { :offset => 0x717323, :size => 0x001ce0 },
  "blueZoneStarrt" => { :offset => 0x7295a1, :size => 0x000000 },
  "bmu" => { :offset => 0x0daedb, :size => 0x005806 },
  "bresenhamArray" => { :offset => 0x0c886b, :size => 0x000078 },
  "bresenhamIndex" => { :offset => 0x0c88e3, :size => 0x000018 },
  "bresenhamRadius" => { :offset => 0x0c8853, :size => 0x000018 },
  "bridge" => { :offset => 0x640e03, :size => 0x000b28 },
  "bridge-close" => { :offset => 0x56426f, :size => 0x001350 },
  "bridge-far" => { :offset => 0x44c262, :size => 0x0014f0 },
  "cachedFastCircle" => { :offset => 0x3fdd8a, :size => 0x00362e },
  "cachedFastCorner" => { :offset => 0x544d6f, :size => 0x000eb0 },
  "cachedFastEllipse" => { :offset => 0x45e69e, :size => 0x00b1af },
  "caravan" => { :offset => 0x5706a4, :size => 0x001365 },
  "caravan-axis" => { :offset => 0x5a58e9, :size => 0x00067d },
  "caravan-door" => { :offset => 0x56f483, :size => 0x00062f },
  "caravan-frame" => { :offset => 0x6ee1ca, :size => 0x0026c7 },
  "caravan-wheel" => { :offset => 0x2abe9c, :size => 0x000d07 },
  "caravan-window1" => { :offset => 0x1ad921, :size => 0x000a62 },
  "caravan-window2" => { :offset => 0x23d82a, :size => 0x000b34 },
  "cargobox" => { :offset => 0x21edd5, :size => 0x006022 },
  "caseNat_adaptation" => { :offset => 0x71ba1d, :size => 0x000048 },
  "casePair_adaptation" => { :offset => 0x7163ca, :size => 0x000048 },
  "casePictureDescr_adaptation" => { :offset => 0x72954c, :size => 0x000048 },
  "caseVar1_adaptation" => { :offset => 0x713860, :size => 0x000048 },
  "caseVar2_adaptation" => { :offset => 0x70d83c, :size => 0x000048 },
  "cbfArray" => { :offset => 0x0ca12a, :size => 0x0005a0 },
  "charClockCallback" => { :offset => 0x0c86eb, :size => 0x000030 },
  "charClockCallbackFade" => { :offset => 0x0c871b, :size => 0x000030 },
  "charCounter" => { :offset => 0x0c8d00, :size => 0x000018 },
  "charIndexArray" => { :offset => 0x0c8d30, :size => 0x0004b0 },
  "charIndexOffset" => { :offset => 0x0c8d18, :size => 0x000018 },
  "charInfo_Tempus-Bold-Huge_" => { :offset => 0x079e9f, :size => 0x000046 },
  "charInfo_Tempus-Bold-Huge_L" => { :offset => 0x079ee5, :size => 0x000a96 },
  "charInfo_Tempus-Bold-Huge_M" => { :offset => 0x07a97b, :size => 0x000e51 },
  "checkIntegrity" => { :offset => 0x3e9f1a, :size => 0x000868 },
  "checksum" => { :offset => 0x21bcc7, :size => 0x000d15 },
  "chick" => { :offset => 0x541d0e, :size => 0x003049 },
  "cloak-night" => { :offset => 0x652673, :size => 0x0006bf },
  "cloak-rain" => { :offset => 0x309590, :size => 0x03484d },
  "closureArguments" => { :offset => 0x0c97b2, :size => 0x000960 },
  "closureIndex" => { :offset => 0x0ca112, :size => 0x000018 },
  "cloud" => { :offset => 0x60fea4, :size => 0x001962 },
  "clouds" => { :offset => 0x5c909f, :size => 0x000bc1 },
  "colorBlack" => { :offset => 0x23adf8, :size => 0x000172 },
  "colorBlue" => { :offset => 0x25f3c4, :size => 0x000172 },
  "colorByIndex" => { :offset => 0x1a4e72, :size => 0x00064a },
  "colorCyan" => { :offset => 0x3c8584, :size => 0x000172 },
  "colorDuckBrown" => { :offset => 0x0d92ad, :size => 0x000596 },
  "colorDuckOrange" => { :offset => 0x3c647c, :size => 0x0001f4 },
  "colorDuckYellow" => { :offset => 0x6d730d, :size => 0x000208 },
  "colorGermanyYellow" => { :offset => 0x65e3c5, :size => 0x000244 },
  "colorGreen" => { :offset => 0x35cd8d, :size => 0x000172 },
  "colorLila" => { :offset => 0x23d434, :size => 0x0003de },
  "colorMagenta" => { :offset => 0x5c64e9, :size => 0x000172 },
  "colorRed" => { :offset => 0x3cbe51, :size => 0x000172 },
  "colorReset" => { :offset => 0x0c86ea, :size => 0x000001 },
  "colorSoftYellow" => { :offset => 0x5c9c78, :size => 0x0002b2 },
  "colorTable" => { :offset => 0x0c874b, :size => 0x0000f0 },
  "colorWhite" => { :offset => 0x58dfcb, :size => 0x000172 },
  "compose_adaptation" => { :offset => 0x7113a4, :size => 0x000048 },
  "contest-1998" => { :offset => 0x1a5b73, :size => 0x007d96 },
  "contest-1999" => { :offset => 0x34da12, :size => 0x00d1d3 },
  "contest-2000" => { :offset => 0x5655d7, :size => 0x009e94 },
  "contest-2001" => { :offset => 0x54ecfa, :size => 0x0093b2 },
  "contest-2002" => { :offset => 0x24748b, :size => 0x009bc7 },
  "contest-2003" => { :offset => 0x23eb4e, :size => 0x008925 },
  "contest-2004" => { :offset => 0x2ad4aa, :size => 0x00935b },
  "contest-2005" => { :offset => 0x0cc21e, :size => 0x00ae91 },
  "contest-2006" => { :offset => 0x435804, :size => 0x0095f5 },
  "contest-2007" => { :offset => 0x2084f9, :size => 0x00b052 },
  "correctErrors" => { :offset => 0x5b7bf3, :size => 0x00e8de },
  "cosineArray" => { :offset => 0x0c65a8, :size => 0x001800 },
  "cow-body" => { :offset => 0x4e151b, :size => 0x0011d8 },
  "cow-head" => { :offset => 0x6d752d, :size => 0x001234 },
  "cow-holy" => { :offset => 0x6d368f, :size => 0x00090e },
  "cow-left-eye" => { :offset => 0x6395fd, :size => 0x000ee8 },
  "cow-left-foot" => { :offset => 0x3e917e, :size => 0x000d84 },
  "cow-left-horn" => { :offset => 0x25f54e, :size => 0x0009c8 },
  "cow-middle-foot" => { :offset => 0x263b5e, :size => 0x000c80 },
  "cow-mouth" => { :offset => 0x207b0f, :size => 0x0009d2 },
  "cow-right-ear" => { :offset => 0x519eaf, :size => 0x000b38 },
  "cow-right-eye" => { :offset => 0x5a8550, :size => 0x000db0 },
  "cow-right-foot" => { :offset => 0x239ff4, :size => 0x000dec },
  "cow-right-horn" => { :offset => 0x0cb990, :size => 0x000876 },
  "cow-spot-butt" => { :offset => 0x4013d0, :size => 0x000b34 },
  "cow-spot-left" => { :offset => 0x23e376, :size => 0x0007c0 },
  "cow-spot-middle" => { :offset => 0x0d985b, :size => 0x000b34 },
  "cow-spot-middle-ecc" => { :offset => 0x0da3a7, :size => 0x000b34 },
  "cow-spot-right" => { :offset => 0x44b85b, :size => 0x0006bc },
  "cow-tail" => { :offset => 0x4aa77d, :size => 0x00145c },
  "crackChars" => { :offset => 0x0c0f1d, :size => 0x0006c0 },
  "crackKey" => { :offset => 0x5c6673, :size => 0x002a14 },
  "crackKeyAndPrint" => { :offset => 0x6c9469, :size => 0x001616 },
  "crackTestValue" => { :offset => 0x0c0f05, :size => 0x000018 },
  "crater" => { :offset => 0x592c81, :size => 0x001c97 },
  "crypt" => { :offset => 0x4d9d2e, :size => 0x005002 },
  "curX" => { :offset => 0x0c4670, :size => 0x000018 },
  "curY" => { :offset => 0x0c4688, :size => 0x000018 },
  "div2" => { :offset => 0x4d9966, :size => 0x0003b0 },
  "doSelfCheck" => { :offset => 0x000058, :size => 0x000001 },
  "drawChar" => { :offset => 0x65c06a, :size => 0x0009f6 },
  "drawCircleFast" => { :offset => 0x268004, :size => 0x000965 },
  "drawCornerFast" => { :offset => 0x25e401, :size => 0x0003cf },
  "drawEllipse" => { :offset => 0x6f1943, :size => 0x00185b },
  "drawEllipseFast" => { :offset => 0x6d8a8c, :size => 0x001fd1 },
  "drawFunctionBase" => { :offset => 0x45bfd1, :size => 0x0026b5 },
  "drawGoldFishL_adaptation" => { :offset => 0x7289d2, :size => 0x0000f0 },
  "drawGoldFishR_adaptation" => { :offset => 0x7287f2, :size => 0x0000f0 },
  "drawGoldenFish_adaptation" => { :offset => 0x71c537, :size => 0x000138 },
  "drawGradientCornerNW" => { :offset => 0x44d76a, :size => 0x000b78 },
  "drawGradientCornerSE" => { :offset => 0x591deb, :size => 0x000e7e },
  "drawGradientH" => { :offset => 0x56faca, :size => 0x000bc2 },
  "drawGradientV" => { :offset => 0x21c9f4, :size => 0x000bd6 },
  "drawGrassPatch" => { :offset => 0x282970, :size => 0x00223b },
  "drawHexDigit" => { :offset => 0x5324da, :size => 0x000a9a },
  "drawIntHex" => { :offset => 0x452999, :size => 0x000f94 },
  "drawPolyline" => { :offset => 0x5a5f7e, :size => 0x001b95 },
  "drawPolylinePolar" => { :offset => 0x65e621, :size => 0x00114c },
  "drawRect" => { :offset => 0x21d5e2, :size => 0x0017db },
  "drawRoundedRect" => { :offset => 0x35abfd, :size => 0x002178 },
  "drawString" => { :offset => 0x239a6d, :size => 0x00056f },
  "drawStringHere" => { :offset => 0x6ed9c0, :size => 0x0007f2 },
  "duolc" => { :offset => 0x61181e, :size => 0x001962 },
  "ellipseAngleIncr" => { :offset => 0x0c7da8, :size => 0x000018 },
  "emptyBox_adaptation" => { :offset => 0x71cf3f, :size => 0x0000f0 },
  "enableBioMorph" => { :offset => 0x033963, :size => 0x000001 },
  "enableBioMorph_adaptation" => { :offset => 0x71c4ef, :size => 0x000048 },
  "endRelativeMode" => { :offset => 0x2bbf0c, :size => 0x0003a7 },
  "endo" => { :offset => 0x58e155, :size => 0x001985 },
  "endo-bottom" => { :offset => 0x3c6958, :size => 0x001c14 },
  "endo-left-arm" => { :offset => 0x0ca6fa, :size => 0x00127e },
  "endo-left-eye" => { :offset => 0x0e06f9, :size => 0x001124 },
  "endo-right-arm" => { :offset => 0x262ab6, :size => 0x001090 },
  "endo-right-eye" => { :offset => 0x3f9b23, :size => 0x001088 },
  "endo-top" => { :offset => 0x2cbf74, :size => 0x000dfc },
  "endocow" => { :offset => 0x546d07, :size => 0x00449b },
  "fadingCOlors" => { :offset => 0x58fc7c, :size => 0x000f14 },
  "false" => { :offset => 0x6ffa31, :size => 0x000700 },
  "fastForward" => { :offset => 0x409142, :size => 0x00515f },
  "fastRandom" => { :offset => 0x2647f6, :size => 0x00108e },
  "fastError" => { :offset => 0x226492, :size => 0x001548 },
  "flower" => { :offset => 0x224fb1, :size => 0x0014c9 },
  "flowerbed" => { :offset => 0x45acb4, :size => 0x001305 },
  "fontCOmbinator" => { :offset => 0x0c88fc, :size => 0x000404 },
  "fontTable_Bird-Ring" => { :offset => 0x0aa059, :size => 0x002400 },
  "fontTable_Cyperus" => { :offset => 0x033965, :size => 0x002400 },
  "fontTable_Cyperus-Big" => { :offset => 0x0566b5, :size => 0x002400 },
  "fontTable_Cyperus-Italic" => { :offset => 0x044bdf, :size => 0x002400 },
  "fontTable_Dots" => { :offset => 0x0a1ac3, :size => 0x002400 },
  "fontTable_Messenger" => { :offset => 0x094fdb, :size => 0x002400 },
  "fontTable_Tempus-Bold-Huge" => { :offset => 0x077a9f, :size => 0x002400 },
  "fontTable-Tempus-Small" => { :offset => 0x07b7cc, :size => 0x002400 },
  "fontTable-Tempus-Small-Italic" => { :offset => 0x088b2d, :size => 0x002400 },
  "fpForward" => { :offset => 0x1af076, :size => 0x0010cc },
  "fpForwardPenUp" => { :offset => 0x545c37, :size => 0x0010b8 },
  "fpMoveAbsolute" => { :offset => 0x5a9318, :size => 0x0004a8 },
  "fpPop" => { :offset => 0x4a9798, :size => 0x000c74 },
  "fpPush" => { :offset => 0x6daa75, :size => 0x000853 },
  "fpTurnLeft" => { :offset => 0x0edcfb, :size => 0x000351 },
  "fpTurnRight" => { :offset => 0x6f0b70, :size => 0x00027b },
  "fri" => { :offset => 0x0c7dd8, :size => 0x000009 },
  "frj" => { :offset => 0x0c7de1, :size => 0x000009 },
  "fromNat_adaptation" => { :offset => 0x71aaf1, :size => 0x000048 },
  "frs" => { :offset => 0x0c7dea, :size => 0x000900 },
  "fstP_adaptation" => { :offset => 0x7107de, :size => 0x000048 },
  "functionAdd" => { :offset => 0x434308, :size => 0x0014e4 },
  "functionParabola" => { :offset => 0x5a7b2b, :size => 0x000a0d },
  "functionSine" => { :offset => 0x1ae39b, :size => 0x000cc3 },
  "fuundoc1" => { :offset => 0x3d67ae, :size => 0x0129b8 },
  "fuundoc2" => { :offset => 0x2279f2, :size => 0x012063 },
  "fuundoc3" => { :offset => 0x40ee09, :size => 0x00bccc },
  "ge" => { :offset => 0x35cf17, :size => 0x0047d2 },
  "germanColors" => { :offset => 0x2677de, :size => 0x00080e },
  "giveMeAPresent" => { :offset => 0x00005d, :size => 0x000480 },
  "goldenFish_adaptation" => { :offset => 0x71c66f, :size => 0x0008d0 },
  "goldfishLeft" => { :offset => 0x2812c9, :size => 0x000c78 },
  "foldfishRight" => { :offset => 0x40e2b9, :size => 0x000b38 },
  "goodVibrations" => { :offset => 0x000501, :size => 0x000009 },
  "grass" => { :offset => 0x25da00, :size => 0x0009e9 },
  "grass1" => { :offset => 0x532f8c, :size => 0x000c72 },
  "grass2" => { :offset => 0x4a8b3f, :size => 0x000c41 },
  "grass3" => { :offset => 0x23af82, :size => 0x000d62 },
  "grass4" => { :offset => 0x3fd02f, :size => 0x000d43 },
  "greenZoneStart" => { :offset => 0x000000, :size => 0x000000 },
  "height_adaptation" => { :offset => 0x726636, :size => 0x000048 },
  "help-activating-genes" => { :offset => 0x2bc2cb, :size => 0x00c0eb },
  "help-adaptive-genes" => { :offset => 0x197686, :size => 0x00d7d4 },
  "help-background" => { :offset => 0x590ba8, :size => 0x00122b },
  "help-background'" => { :offset => 0x44e2fa, :size => 0x001e83 },
  "help-beautiful-numbers" => { :offset => 0x0e5d10, :size => 0x007e2d },
  "help-beautiful-numbers_purchase_code" => { :offset => 0x033933, :size => 0x000018 },
  "help-catalog-page" => { :offset => 0x5eec16, :size => 0x008e94 },
  "help-compression-rna" => { :offset => 0x572530, :size => 0x0079e5 },
  "help-encodings" => { :offset => 0x5a97d8, :size => 0x00aa8d },
  "help-error-correcting-codes" => { :offset => 0x42ccf3, :size => 0x006ed8 },
  "help-error-correcting-codes_purchase_code" => { :offset => 0x033903, :size => 0x000018 },
  "help-fuun-security" => { :offset => 0x6db2e0, :size => 0x0126c8 },
  "help-fuun-structure" => { :offset => 0x401f1c, :size => 0x00720e },
  "help-initial-cond" => { :offset => 0x4abbf1, :size => 0x0097f2 },
  "help-integer-encoding" => { :offset => 0x43f9ce, :size => 0x004aac },
  "help-intro" => { :offset => 0x361701, :size => 0x0648f7 },
  "help-lsystems" => { :offset => 0x5ca81e, :size => 0x006ffd },
  "help-patching-dna" => { :offset => 0x579f2d, :size => 0x00e2e7 },
  "help-undocumented-rna" => { :offset => 0x2b681d, :size => 0x0056d7 },
  "help-virus" => { :offset => 0x613180, :size => 0x0078e9 },
  "help-vmu" => { :offset => 0x594930, :size => 0x006b41 },
  "helpScreen" => { :offset => 0x0004e4, :size => 0x000018 },
  "hitWithTheClueStick" => { :offset => 0x000528, :size => 0x032f3c },
  "i_adaptation" => { :offset => 0x716e41, :size => 0x000048 },
  "impdoc-background" => { :offset => 0x6d690c, :size => 0x0009e9 },
  "impdoc1" => { :offset => 0x1f9e38, :size => 0x00cee7 },
  "impdoc10" => { :offset => 0x4ded48, :size => 0x00173b },
  "impdoc2" => { :offset => 0x4f2c6f, :size => 0x00eb39 },
  "impdoc3" => { :offset => 0x61ba6c, :size => 0x0181e9 },
  "impdoc4" => { :offset => 0x4b53fb, :size => 0x0178eb },
  "impdoc5" => { :offset => 0x50b88b, :size => 0x00e60c },
  "impdoc6" => { :offset => 0x4e345c, :size => 0x00f7fb },
  "impdoc7" => { :offset => 0x41e665, :size => 0x00e676 },
  "impdoc8" => { :offset => 0x33ddf5, :size => 0x00fc05 },
  "impdoc9" => { :offset => 0x268981, :size => 0x00efdf },
  "init" => { :offset => 0x23ca0e, :size => 0x000a0e },
  "initDone" => { :offset => 0x0c88fb, :size => 0x000001 },
  "initFastRandom" => { :offset => 0x451146, :size => 0x00183b },
  "intBox" => { :offset => 0x6fc928, :size => 0x000574 },
  "k" => { :offset => 0x6ff34f, :size => 0x0006e2 },
  "lambda-id" => { :offset => 0x4d730e, :size => 0x002640 },
  "lightningBolt" => { :offset => 0x41af00, :size => 0x00061a },
  "lindenmayerAngle" => { :offset => 0x0ca6ca, :size => 0x000018 },
  "lindenmayerDistance" => { :offset => 0x0ca6e2, :size => 0x000018 },
  "lsystem-kochisland" => { :offset => 0x50a9ce, :size => 0x000ea5 },
  "lsystem-kochisland-F" => { :offset => 0x192e99, :size => 0x0047d5 },
  "lsystem-kochisland-f" => { :offset => 0x5b6aa6, :size => 0x001135 },
  "lsystem-sierpinski" => { :offset => 0x4aa424, :size => 0x000341 },
  "lsystem-sierpinski-l" => { :offset => 0x54df69, :size => 0x000d79 },
  "lsystem-sierpinski-r" => { :offset => 0x277978, :size => 0x000d79 },
  "lsystem-weed" => { :offset => 0x65fb98, :size => 0x000341 },
  "lsystem-weed-F" => { :offset => 0x25ff2e, :size => 0x002b70 },
  "lt" => { :offset => 0x53cac2, :size => 0x0047d1 },
  "main" => { :offset => 0x63a4fd, :size => 0x01303b },
  "makeDarkness" => { :offset => 0x6d8779, :size => 0x0002fb },
  "max" => { :offset => 0x1a54d4, :size => 0x000687 },
  "mkAbove_adaptation" => { :offset => 0x71e2dc, :size => 0x000048 },
  "mkBeforeAbove_adaptation" => { :offset => 0x71e36c, :size => 0x000048 },
  "mkEmp_adaptation" => { :offset => 0x71e324, :size => 0x000048 },
  "mkGoldfishL_adaptation" => { :offset => 0x71e294, :size => 0x000048 },
  "mkGoldfishR_adaptation" => { :offset => 0x71e24c, :size => 0x000048 },
  "mkPair_adaptation" => { :offset => 0x71aaa9, :size => 0x000048 },
  "mkSucc_adaptation" => { :offset => 0x71910b, :size => 0x000048 },
  "mkZero_adaptation" => { :offset => 0x7195eb, :size => 0x000048 },
  "mlephant" => { :offset => 0x5b427d, :size => 0x002811 },
  "modInts" => { :offset => 0x633c6d, :size => 0x005978 },
  "moon" => { :offset => 0x3fabc3, :size => 0x000984 },
  "most-wanted" => { :offset => 0x5d1833, :size => 0x03e659 },
  "motherDuck" => { :offset => 0x2c8aff, :size => 0x00345d },
  "motherDuckWithChicks" => { :offset => 0x25cca1, :size => 0x000d47 },
  "moveTo" => { :offset => 0x5412ab, :size => 0x000a4b },
  "moveToPolar" => { :offset => 0x251ba8, :size => 0x0013e1 },
  "mulInts" => { :offset => 0x0e1835, :size => 0x0044c3 },
  "mulModInts" => { :offset => 0x5017c0, :size => 0x0041c3 },
  "negateInt" => { :offset => 0x44bf2f, :size => 0x00031b },
  "night-or-day" => { :offset => 0x00050f, :size => 0x000001 },
  "ocaml" => { :offset => 0x59b489, :size => 0x00a448 },
  "ocamlrules" => { :offset => 0x0c9228, :size => 0x000001 },
  "originStackIndex" => { :offset => 0x0c46a0, :size => 0x000018 },
  "originStackX" => { :offset => 0x0c46b8, :size => 0x0001e0 },
  "originStackY" => { :offset => 0x0c4898, :size => 0x0001e0 },
  "parabolaCBF" => { :offset => 0x281f59, :size => 0x0009ff },
  "payloadBioMorph" => { :offset => 0x43ee11, :size => 0x00030b },
  "payloadBioMorph_adaptation" => { :offset => 0x71ba65, :size => 0x000048 },
  "pear" => { :offset => 0x278709, :size => 0x000701 },
  "peartree" => { :offset => 0x58822c, :size => 0x00372b },
  "pictureDescrRenderer_adaptation" => { :offset => 0x71e3b4, :size => 0x000048 },
  "polarAngleIncr" => { :offset => 0x0c91e0, :size => 0x000018 },
  "printCharSet" => { :offset => 0x54b4f7, :size => 0x002a5a },
  "printGeneTable" => { :offset => 0x284bc3, :size => 0x0272c1 },
  "randomInt" => { :offset => 0x2acbbb, :size => 0x0008d7 },
  "resetOrigin" => { :offset => 0x52df43, :size => 0x000834 },
  "resetOrigin_adaptation" => { :offset => 0x7288e2, :size => 0x0000f0 },
  "river" => { :offset => 0x450195, :size => 0x000f99 },
  "rotateColorVar" => { :offset => 0x0c883b, :size => 0x000018 },
  "s_adaptation" => { :offset => 0x716412, :size => 0x000048 },
  "scenario" => { :offset => 0x4cccfe, :size => 0x00a5f8 },
  "seed" => { :offset => 0x0c7dc0, :size => 0x000018 },
  "setFastCircleArray" => { :offset => 0x65ca78, :size => 0x001935 },
  "setGlobalPolarRotation" => { :offset => 0x0edb55, :size => 0x00018e },
  "setOrigin" => { :offset => 0x571a21, :size => 0x000af7 },
  "setOrigin_adaptation" => { :offset => 0x728ac2, :size => 0x000048 },
  "sineArray" => { :offset => 0x0c4da8, :size => 0x001800 },
  "sineCBF" => { :offset => 0x4e049b, :size => 0x001068 },
  "sky" => { :offset => 0x58b96f, :size => 0x001325 },
  "sky-day" => { :offset => 0x2c83ce, :size => 0x000719 },
  "sky-day-bodies" => { :offset => 0x6f9e01, :size => 0x000ba3 },
  "sky-night" => { :offset => 0x61aa81, :size => 0x000fd3 },
  "sky-night-bodies" => { :offset => 0x4e270b, :size => 0x000d39 },
  "sky-waves" => { :offset => 0x3fb55f, :size => 0x001ab8 },
  "smoke" => { :offset => 0x0d70c7, :size => 0x0021ce },
  "sndP_adaptation" => { :offset => 0x712c9a, :size => 0x000048 },
  "spirograph" => { :offset => 0x6f4c51, :size => 0x002a79 },
  "star" => { :offset => 0x6f76e2, :size => 0x000d87 },
  "stringLength" => { :offset => 0x21b66f, :size => 0x000640 },
  "subInts" => { :offset => 0x41aaed, :size => 0x0003fb },
  "sun" => { :offset => 0x206d37, :size => 0x0006e0 },
  "sunflower" => { :offset => 0x20742f, :size => 0x0006e0 },
  "superDuck" => { :offset => 0x52e78f, :size => 0x003d33 },
  "surfaceTransform" => { :offset => 0x6d3fb5, :size => 0x00293f },
  "terminate" => { :offset => 0x224e0f, :size => 0x00018a },
  "threeFish_adaptation" => { :offset => 0x71d02f, :size => 0x000048 },
  "tmpCurX" => { :offset => 0x0c91f8, :size => 0x000018 },
  "tmpCurY" => { :offset => 0x0c9210, :size => 0x000018 },
  "true" => { :offset => 0x700131, :size => 0x0006e2 },
  "tulip" => { :offset => 0x27eff6, :size => 0x0022bb },
  "tulips" => { :offset => 0x3d3b81, :size => 0x002c15 },
  "turtleFPX" => { :offset => 0x0c4a78, :size => 0x000018 },
  "turtleFPY" => { :offset => 0x0c4a90, :size => 0x000018 },
  "turtleHeading" => { :offset => 0x0c4aa8, :size => 0x000018 },
  "turtleStackFPX" => { :offset => 0x0c4ad8, :size => 0x0000f0 },
  "turtleStackFPY" => { :offset => 0x0c4bc8, :size => 0x0000f0 },
  "turtleStackHeading" => { :offset => 0x0c4cb8, :size => 0x0000f0 },
  "turtleStackIndex" => { :offset => 0x0c4ac0, :size => 0x000018 },
  "ufo" => { :offset => 0x652d4a, :size => 0x002d08 },
  "ufo-bottom" => { :offset => 0x43f134, :size => 0x000882 },
  "ufo-cup" => { :offset => 0x25106a, :size => 0x000b26 },
  "ufo-frame" => { :offset => 0x25e7e8, :size => 0x000bc4 },
  "ufo-left-foot" => { :offset => 0x65fef1, :size => 0x0008de },
  "ufo-middle-foot" => { :offset => 0x3c6010, :size => 0x000724 },
  "ufo-right-foot" => { :offset => 0x5c9f42, :size => 0x0008c4 },
  "ufo-with-smoke" => { :offset => 0x433be3, :size => 0x00070d },
  "useColorTable" => { :offset => 0x41cc18, :size => 0x001a35 },
  "var1_adaptation" => { :offset => 0x71077e, :size => 0x000060 },
  "var2_adaptation" => { :offset => 0x712c3a, :size => 0x000060 },
  "vmu" => { :offset => 0x23bcfc, :size => 0x000cfa },
  "vmu-code" => { :offset => 0x213563, :size => 0x0080f4 },
  "vmu-code_purchase_code" => { :offset => 0x03391b, :size => 0x000018 },
  "vmuMode" => { :offset => 0x03346b, :size => 0x000018 },
  "vmuRegCode" => { :offset => 0x033483, :size => 0x000480 },
  "void" => { :offset => 0x6fa9bc, :size => 0x000273 },
  "water" => { :offset => 0x52d361, :size => 0x000bca },
  "weather" => { :offset => 0x03394b, :size => 0x000018 },
  "weeds" => { :offset => 0x64d550, :size => 0x00510b },
  "whale" => { :offset => 0x26589c, :size => 0x001f2a },
  "width_adaptation" => { :offset => 0x723817, :size => 0x000048 },
  "windmill" => { :offset => 0x51a9ff, :size => 0x01294a },
  "wrapAdd_adaptation" => { :offset => 0x7286ea, :size => 0x000108 },
  "wrapInt" => { :offset => 0x6fea1f, :size => 0x0003fd },
  "wrapMax_adaptation" => { :offset => 0x72652e, :size => 0x000108 },
  "wrapSub_adaptation" => { :offset => 0x719003, :size => 0x000108 },
}

$:.unshift(File.dirname(File.expand_path(__FILE__)))
require 'util'

def create_call(func)
  unless FuncTable.has_key?(func)
    $stderr.puts "Unknown function `#{func}`"
    exit 1
  end
  offset = FuncTable[func][:offset]
  size = FuncTable[func][:size]
  offset_code = asnat(offset)
  size_code = asnat(size)

  return ("IIPIFFCPICCFPICICFFFIIPIFFCPICCFPICICFFFIICIIC" + "IIC" +
          # Template {0_0}offset_size
          "IPPP#{protect(1, offset_code)}IC#{protect(1, size_code)}ICIPPCP" +
          "IIC")
end


if $0 == __FILE__
  puts ARGV.map {|func|
    create_call(func)
  }.join
end
