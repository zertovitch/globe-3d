package Vrml_Goto is

    type Small_Integer is range -32_000 .. 32_000;

    type Goto_Entry is record
        Nonterm  : Small_Integer;
        Newstate : Small_Integer;
    end record;

  --pragma suppress(index_check);

    subtype Row is Integer range -1 .. Integer'Last;

    type Goto_Parse_Table is array (Row range <>) of Goto_Entry;

    Goto_Matrix : constant Goto_Parse_Table :=
       ((-1,-1)  -- Dummy Entry.
-- State  0
,(-5,4),(-4,3),(-3,1),(-2,7)
-- State  3
,(-5,4),(-4,3),(-3,8)
-- State  4
,(-44,46),(-43,45),(-42,44),(-41,43),(-40,42),(-39,41),(-38,40),(-37,39),(-36,38),(-35,37),(-34,36)
,(-33,35),(-32,34),(-31,33),(-30,32),(-29,31),(-28,30),(-27,29),(-26,28),(-25,27),(-24,26),(-23,25)
,(-22,24),(-21,23),(-20,22),(-19,21),(-18,20),(-17,19),(-16,18),(-15,17),(-14,16),(-13,15),(-12,14)
,(-11,13),(-10,12),(-9,11),(-8,10),(-7,9),(-6,85)
-- State  72
,(-119,114)
-- State  89
,(-45,127)
-- State  90
,(-45,128)
-- State  91
,(-45,129)
-- State  92
,(-45,130)
-- State  93
,(-45,131)
-- State  94
,(-45,132)
-- State  95
,(-45,133)
-- State  96
,(-5,4),(-4,3),(-3,134)
-- State  97
,(-45,135)
-- State  98
,(-45,136)
-- State  99
,(-45,137)
-- State  100
,(-45,138)
-- State  101
,(-45,139)
-- State  102
,(-45,140)
-- State  103
,(-45,141)
-- State  104
,(-45,142)
-- State  105
,(-45,143)
-- State  106
,(-45,144)
-- State  107
,(-45,145)
-- State  108
,(-45,146)
-- State  109
,(-45,147)
-- State  110
,(-45,148)
-- State  111
,(-45,149)
-- State  112
,(-45,150)
-- State  113
,(-115,151)
-- State  115
,(-45,153)
-- State  116
,(-45,154)
-- State  117
,(-45,155)
-- State  118
,(-45,156)
-- State  119
,(-45,157)
-- State  120
,(-45,158)
-- State  121
,(-45,159)
-- State  122
,(-45,160)
-- State  123
,(-45,161)
-- State  124
,(-45,162)
-- State  125
,(-45,163)
-- State  127
,(-47,164),(-46,169)
-- State  128
,(-53,170),(-52,174)
-- State  129
,(-57,175)
-- State  130
,(-60,176),(-59,180)
-- State  131
,(-62,181),(-61,185)
-- State  132
,(-64,186),(-63,191)
-- State  133
,(-69,192),(-68,196)
-- State  135
,(-71,198),(-70,203)
-- State  136
,(-76,204),(-75,209)
-- State  137
,(-77,211)
-- State  138
,(-80,212),(-79,215)
-- State  139
,(-81,217)
-- State  140
,(-85,218)
-- State  141
,(-88,219),(-87,221)
-- State  142
,(-90,222),(-89,224)
-- State  143
,(-95,225)
-- State  144
,(-98,226),(-97,228)
-- State  145
,(-100,229),(-99,234)
-- State  146
,(-103,235),(-102,240)
-- State  147
,(-105,241),(-104,246)
-- State  148
,(-107,247),(-106,250)
-- State  149
,(-110,251),(-109,253)
-- State  150
,(-112,254),(-111,256)
-- State  151
,(-45,257)
-- State  152
,(-45,258)
-- State  153
,(-121,259),(-120,261)
-- State  154
,(-123,262),(-122,270)
-- State  155
,(-125,271),(-124,273)
-- State  156
,(-127,274),(-126,279)
-- State  157
,(-130,280),(-129,285)
-- State  158
,(-133,286),(-132,288)
-- State  159
,(-136,289),(-135,295)
-- State  160
,(-5,4),(-4,3),(-3,296)
-- State  161
,(-138,297),(-137,299)
-- State  162
,(-140,300),(-139,304)
-- State  163
,(-142,305),(-141,309)
-- State  164
,(-47,164),(-46,310)
-- State  165
,(-78,311),(-48,314)
-- State  166
,(-49,317)
-- State  167
,(-50,319)
-- State  168
,(-51,322),(-49,320)
-- State  170
,(-53,170),(-52,324)
-- State  171
,(-54,327)
-- State  172
,(-49,328)
-- State  173
,(-49,329)
-- State  175
,(-55,332)
-- State  176
,(-60,176),(-59,333)
-- State  177
,(-49,334)
-- State  178
,(-49,335)
-- State  179
,(-49,336)
-- State  181
,(-62,181),(-61,338)
-- State  182
,(-54,339)
-- State  183
,(-49,340)
-- State  184
,(-49,341)
-- State  186
,(-64,186),(-63,343)
-- State  187
,(-65,345)
-- State  188
,(-49,346)
-- State  189
,(-66,348),(-49,347)
-- State  190
,(-67,350),(-49,349)
-- State  192
,(-69,192),(-68,352)
-- State  193
,(-49,353)
-- State  194
,(-50,354)
-- State  195
,(-54,355)
-- State  198
,(-71,198),(-70,357)
-- State  199
,(-72,358)
-- State  200
,(-108,359),(-74,362)
-- State  201
,(-108,359),(-74,363)
-- State  202
,(-108,359),(-74,364)
-- State  204
,(-76,204),(-75,366)
-- State  205
,(-108,359),(-74,367)
-- State  206
,(-108,359),(-74,368)
-- State  207
,(-108,359),(-74,369)
-- State  208
,(-108,359),(-74,370)
-- State  210
,(-78,372)
-- State  212
,(-80,212),(-79,374)
-- State  213
,(-51,375),(-49,320)
-- State  214
,(-67,376),(-49,349)
-- State  215
,(-5,4),(-4,3),(-3,377)
-- State  216
,(-51,378),(-49,320)
-- State  217
,(-5,4),(-4,3),(-3,379)
-- State  218
,(-83,380),(-82,387)
-- State  219
,(-88,219),(-87,388)
-- State  220
,(-50,389)
-- State  222
,(-90,222),(-89,391)
-- State  223
,(-91,393),(-49,392)
-- State  225
,(-93,395),(-92,397)
-- State  226
,(-98,226),(-97,398)
-- State  227
,(-50,399)
-- State  229
,(-100,229),(-99,401)
-- State  230
,(-67,402),(-49,349)
-- State  231
,(-101,404),(-49,403)
-- State  232
,(-49,405)
-- State  233
,(-49,406)
-- State  235
,(-103,235),(-102,408)
-- State  236
,(-67,409),(-49,349)
-- State  237
,(-101,410),(-49,403)
-- State  238
,(-49,411)
-- State  239
,(-49,412)
-- State  241
,(-105,241),(-104,414)
-- State  242
,(-65,415)
-- State  243
,(-49,416)
-- State  244
,(-66,417),(-49,347)
-- State  245
,(-67,418),(-49,349)
-- State  247
,(-107,247),(-106,420)
-- State  248
,(-108,421)
-- State  249
,(-108,422)
-- State  251
,(-110,251),(-109,424)
-- State  252
,(-101,425),(-49,403)
-- State  254
,(-112,254),(-111,427)
-- State  255
,(-67,428),(-49,349)
-- State  257
,(-114,430),(-113,432)
-- State  258
,(-118,433),(-117,438)
-- State  259
,(-121,259),(-120,439)
-- State  260
,(-49,440)
-- State  262
,(-123,262),(-122,442)
-- State  263
,(-65,443)
-- State  264
,(-49,444)
-- State  265
,(-66,445),(-49,347)
-- State  266
,(-67,446),(-49,349)
-- State  267
,(-67,447),(-49,349)
-- State  268
,(-49,448)
-- State  269
,(-49,449)
-- State  271
,(-125,271),(-124,451)
-- State  272
,(-108,452)
-- State  273
,(-5,4),(-4,3),(-3,453)
-- State  274
,(-127,274),(-126,454)
-- State  275
,(-78,455)
-- State  276
,(-128,457)
-- State  277
,(-50,458)
-- State  278
,(-50,459)
-- State  280
,(-130,280),(-129,461)
-- State  281
,(-131,463),(-49,462)
-- State  282
,(-49,464)
-- State  283
,(-131,465),(-49,462)
-- State  284
,(-131,466),(-49,462)
-- State  286
,(-133,286),(-132,468)
-- State  287
,(-134,471),(-131,469),(-49,462)
-- State  289
,(-136,289),(-135,473)
-- State  290
,(-67,474),(-49,349)
-- State  291
,(-101,475),(-49,403)
-- State  292
,(-67,476),(-49,349)
-- State  293
,(-101,477),(-49,403)
-- State  294
,(-67,478),(-49,349)
-- State  297
,(-138,297),(-137,481)
-- State  298
,(-67,482),(-49,349)
-- State  300
,(-140,300),(-139,484)
-- State  301
,(-78,485)
-- State  302
,(-78,486)
-- State  303
,(-50,487)
-- State  304
,(-5,4),(-4,3),(-3,488)
-- State  305
,(-142,305),(-141,489)
-- State  306
,(-78,490)
-- State  307
,(-67,491),(-49,349)
-- State  308
,(-67,492),(-49,349)
-- State  312
,(-157,495),(-78,494)
-- State  321
,(-156,497),(-49,496)
-- State  326
,(-143,499)
-- State  331
,(-166,500),(-56,502)
-- State  332
,(-58,503)
-- State  347
,(-144,504)
-- State  349
,(-147,505)
-- State  358
,(-153,506),(-73,508)
-- State  360
,(-150,510),(-108,509)
-- State  380
,(-83,380),(-82,513)
-- State  381
,(-84,516),(-66,514),(-49,347)
-- State  382
,(-84,517),(-66,514),(-49,347)
-- State  383
,(-84,518),(-66,514),(-49,347)
-- State  384
,(-84,519),(-66,514),(-49,347)
-- State  385
,(-51,520),(-49,320)
-- State  386
,(-51,521),(-49,320)
-- State  387
,(-86,522)
-- State  392
,(-49,523)
-- State  395
,(-93,395),(-92,524)
-- State  396
,(-161,525),(-94,527)
-- State  397
,(-96,528)
-- State  403
,(-49,529)
-- State  430
,(-114,430),(-113,530)
-- State  431
,(-50,531)
-- State  432
,(-5,4),(-4,3),(-3,532)
-- State  433
,(-118,433),(-117,533)
-- State  434
,(-50,534)
-- State  435
,(-50,535)
-- State  436
,(-50,536)
-- State  437
,(-49,537)
-- State  462
,(-49,541)
-- State  470
,(-158,543),(-131,542),(-49,462)
-- State  500
,(-67,551),(-49,349)
-- State  501
,(-167,552)
-- State  504
,(-49,554)
-- State  505
,(-49,555)
-- State  506
,(-108,556)
-- State  507
,(-154,557)
-- State  515
,(-149,561),(-66,560),(-49,347)
-- State  523
,(-49,563)
-- State  525
,(-67,564),(-49,349)
-- State  526
,(-162,565)
-- State  529
,(-49,567)
-- State  532
,(-116,568)
-- State  545
,(-157,572),(-78,494)
-- State  547
,(-156,573),(-49,496)
-- State  552
,(-164,576),(-67,575),(-49,349)
-- State  554
,(-145,577)
-- State  555
,(-148,578)
-- State  557
,(-151,580),(-108,579)
-- State  558
,(-150,581),(-108,509)
-- State  563
,(-49,584)
-- State  565
,(-159,586),(-67,585),(-49,349)
-- State  567
,(-49,587)
-- State  569
,(-146,590)
-- State  570
,(-158,591),(-131,542),(-49,462)
-- State  576
,(-168,593)
-- State  577
,(-49,594)
-- State  578
,(-49,595)
-- State  580
,(-155,597)
-- State  582
,(-149,598),(-66,560),(-49,347)
-- State  584
,(-49,599)
-- State  586
,(-163,601)
-- State  589
,(-146,602)
-- State  592
,(-165,603)
-- State  596
,(-152,605)
-- State  599
,(-49,607)
-- State  600
,(-160,608)
-- State  603
,(-164,610),(-67,575),(-49,349)
-- State  605
,(-151,611),(-108,579)
-- State  607
,(-49,612)
-- State  608
,(-159,613),(-67,585),(-49,349)
-- State  612
,(-49,614)
-- State  614
,(-49,615)
-- State  615
,(-49,616)
-- State  616
,(-49,617)
-- State  617
,(-49,618)
-- State  618
,(-49,619)
-- State  619
,(-49,620)
-- State  620
,(-49,621)
-- State  621
,(-49,622)
);
--  The offset vector
  Goto_Offset : array (0 .. 622) of Integer :=
  (0,
4,4,4,7,46,46,46,46,46,46,46,46,46,46,46,46,46,
46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,
46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,
46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,
46,46,46,46,47,47,47,47,47,47,47,47,47,47,47,47,47,
47,47,47,47,48,49,50,51,52,53,54,57,58,59,60,61,62,
63,64,65,66,67,68,69,70,71,72,73,74,74,75,76,77,78,
79,80,81,82,83,84,85,85,87,89,90,92,94,96,98,98,100,
102,103,105,106,107,109,111,112,114,116,118,120,122,124,126,127,128,
130,132,134,136,138,140,142,145,147,149,151,153,155,156,157,159,159,
161,162,163,164,164,165,167,168,169,170,170,172,173,174,175,175,177,
178,179,181,183,183,185,186,187,188,188,188,190,191,193,195,197,197,
199,201,203,205,207,207,208,208,210,212,214,217,219,222,224,226,227,
227,229,231,231,233,235,236,236,238,240,242,243,244,244,246,248,250,
251,252,252,254,255,256,258,260,260,262,263,264,264,266,268,268,270,
272,272,274,276,278,279,279,281,282,283,285,287,289,290,291,291,293,
294,297,299,300,301,302,303,303,305,307,308,310,312,312,314,317,317,
319,321,323,325,327,329,329,329,331,333,333,335,336,337,338,341,343,
344,346,348,348,348,348,350,350,350,350,350,350,350,350,350,352,352,
352,352,352,353,353,353,353,353,355,356,356,356,356,356,356,356,356,
356,356,356,356,356,356,356,357,357,358,358,358,358,358,358,358,358,
358,360,360,362,362,362,362,362,362,362,362,362,362,362,362,362,362,
362,362,362,362,362,362,364,367,370,373,376,378,380,381,381,381,381,
381,382,382,382,384,386,387,387,387,387,387,387,388,388,388,388,388,
388,388,388,388,388,388,388,388,388,388,388,388,388,388,388,388,388,
388,388,388,388,388,390,391,394,396,397,398,399,400,400,400,400,400,
400,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400,
400,400,400,401,401,401,401,401,401,401,401,404,404,404,404,404,404,
404,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404,
404,404,404,404,404,404,404,406,407,407,407,408,409,410,411,411,411,
411,411,411,411,411,414,414,414,414,414,414,414,414,415,415,417,418,
418,418,419,419,419,420,420,420,420,420,420,420,420,420,420,420,420,
420,422,422,424,424,424,424,424,427,427,428,429,429,431,433,433,433,
433,433,434,434,437,437,438,438,439,442,442,442,442,442,442,443,444,
445,445,446,446,449,449,450,450,451,451,451,452,452,452,453,453,453,
453,454,454,454,455,456,456,456,459,459,461,461,462,465,465,465,465,
466,466,467,468,469,470,471,472,473, 474);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  323) of Natural := (2,
1,1,2,0,2,2,2,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,0,1,2,0,2,2,2,2,5,2,0,2,2,2,5,2,0,0,0,7,2,0,2,2,2,5,2,
0,2,2,2,5,2,0,2,2,2,2,5,2,0,2,2,2,5,4,2,0,0,3,2,2,2,5,2,0,2,2,2,2,5,2,0,5,
2,0,2,2,6,2,0,6,2,0,2,2,2,2,2,2,0,0,7,2,0,2,5,2,0,2,5,2,0,2,0,0,7,2,0,2,5,
2,0,2,2,2,2,5,2,0,2,2,2,2,5,2,0,2,2,2,2,5,2,0,2,2,5,2,0,2,5,2,0,2,5,2,0,2,
0,0,8,2,0,2,2,2,2,0,6,2,0,2,5,2,0,2,2,2,2,2,2,2,5,2,0,2,6,2,0,2,2,2,2,5,2,
0,2,2,2,2,5,2,0,2,5,2,0,2,2,2,2,2,5,5,2,0,2,5,2,0,2,2,2,6,2,0,2,2,2,5,1,3,
1,3,1,0,0,5,1,1,1,2,0,4,1,16,4,1,2,0,0,5,1,3,1,3,1,3,1,3,1,0,4,0,2,0,0,5,1,
3,1,3,1,3,1,3,1,3,1,3,1,0,4,0,2,0,0,5,1,0,4,0,2,0,0,5);
   Get_LHS_Rule: array (Rule range  0 ..  323) of Nonterminal := (-1,
-2,-2,-3,-3,-4,-4,-5,-5,-6,-6,-6,-6,-6,-6,
-6,-6,-6,-6,-6,-6,-6,-6,-6,-6,-6,-6,-6,-6,
-6,-6,-6,-6,-6,-6,-6,-6,-6,-6,-6,-6,-6,-6,
-6,-6,-6,-6,-45,-45,-44,-46,-46,-47,-47,-47,-47,-7,
-52,-52,-53,-53,-53,-8,-55,-55,-57,-58,-9,-59,-59,-60,
-60,-60,-10,-61,-61,-62,-62,-62,-11,-63,-63,-64,-64,-64,
-64,-12,-68,-68,-69,-69,-69,-13,-14,-70,-70,-72,-71,-71,
-71,-71,-15,-75,-75,-76,-76,-76,-76,-16,-77,-77,-17,-79,
-79,-80,-80,-18,-81,-81,-19,-82,-82,-83,-83,-83,-83,-83,
-83,-85,-86,-20,-87,-87,-88,-21,-89,-89,-90,-22,-92,-92,
-93,-95,-96,-23,-97,-97,-98,-24,-99,-99,-100,-100,-100,-100,
-25,-102,-102,-103,-103,-103,-103,-26,-104,-104,-105,-105,-105,-105,
-27,-106,-106,-107,-107,-28,-109,-109,-110,-29,-111,-111,-112,-30,
-113,-113,-114,-115,-116,-31,-117,-117,-118,-118,-118,-118,-119,-32,
-120,-120,-121,-33,-122,-122,-123,-123,-123,-123,-123,-123,-123,-34,
-124,-124,-125,-35,-126,-126,-127,-127,-127,-127,-36,-129,-129,-130,
-130,-130,-130,-37,-132,-132,-133,-38,-135,-135,-136,-136,-136,-136,
-136,-39,-40,-137,-137,-138,-41,-139,-139,-140,-140,-140,-42,-141,
-141,-142,-142,-142,-43,-143,-143,-54,-54,-65,-144,-145,-66,-50,
-49,-49,-146,-146,-128,-108,-91,-101,-78,-131,-147,-148,-67,-149,
-149,-84,-84,-150,-150,-74,-74,-151,-152,-151,-153,-73,-154,-155,
-73,-156,-156,-51,-51,-157,-157,-48,-48,-158,-158,-134,-134,-159,
-160,-159,-161,-94,-162,-163,-94,-164,-165,-164,-166,-56,-167,-168,
-56);
end Vrml_Goto;
