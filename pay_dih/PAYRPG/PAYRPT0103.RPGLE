000100950626     H DECEDIT('.') DATEDIT(*DMY/)
000200950817     F** EMPLOYEES EARNINGS BREAKDOWN
000300950630     FEMPANLL   IP   E           K DISK
000400971014     Fdeptml01  IF   E           K DISK
000600950626     FREPORT    O    F  132        PRINTER OFLIND(*INOF)
000700990823     **
000800011009
000900110729      /copy qcpysrc,genpr
001000011009
001100950925     D                SDS
001200011009     D  @pgm             *PROC
001300011009     D  @user                254    263
001400990823     **
001500011009
001600011009     Darr              s             40    dim(1) ctdata perrcd(1)
001700011009     D @title          s             40
001800011009     D coname          s             30
001900011009     D date#           s              8  0
002000011009
002100011009     D txt#            C                   CONST('Salary')
002200990825     D hdrtxt1         C                   CONST('CASUAL ')
002300990825     D hdrtxt2         C                   CONST('REGULAR')
002400990825     D hdrtxt3         C                   CONST('EXECUTIVE')
002500990825     D hdrtxt4         C                   CONST('SPECIAL')
002600990825     D hdrtxt5         C                   CONST('XMAS BONUS')
002700990823     **
002800950327     IANLREC        03
002900950410     I                                          DEPT          L1
003000971014     **
003100950713     C     *ENTRY        PLIST
003200950713     C                   PARM                    TYPE              1
003300990825     C                   parm                    updsts            8
003400990823     **
003500990823     ** Parameter list to send to G/L Writing PGM
003600990823
003700990823     C     Parm_lst      plist
003800990823     c                   parm                    prnumb
003900990823     c                   parm      999           Out_cod           3 0
004000990823     c                   parm                    out_acct          6 0
004100990823     c                   parm                    out_suba          4 0
004200990823     c                   parm                    out_spc           1 0
004300990823     c                   parm                    out_subl          4 0
004400990823     c                   parm                    out_bill          8 0
004500990823     c                   parm                    out_com           3 0
004600990823     c                   parm      'PA'          out_source        2
004700990823     c                   parm      'YR'          out_source2       2
004800990825     c                   parm                    out_text         37
004900990823     c                   parm                    out_amt          12 2
005000990823     ** Flag to decide on commitment scope
005100990823     c                   parm      '0'           jrn_flag          1
005200971014     **
005300971014     C     acc_key       klist
005400971014     C                   kfld                    dept
005500971014     C                   kfld                    acc_typ
005600011009     C                   exsr      aainit
006000990825     **
006100990825     C                   if        not(*in99)
006200990825
006300990825     C                   select
006400990825     C                   when      type = 'C'
006500990825     C                   move      hdrtxt1       hdrtxt#          11
006600990825     C                   seton                                        79
006700990825     C                   when      type = 'R'
006800990825     C                   move      hdrtxt2       hdrtxt#
006900990825     C                   when      type = 'E'
007000990825     C                   move      hdrtxt3       hdrtxt#
007100990825     C                   when      type = 'S'
007200990825     C                   move      hdrtxt4       hdrtxt#
007300990825     C                   seton                                        76
007400990825     C                   when      type = 'X'
007500990825     C                   move      hdrtxt5       hdrtxt#
007600990825     C                   endsl
007700990825     **
007800990825     C                   move      mthdat        chrdat#           6
007900990825     C     hdrtxt#       cat(p)    txt#:1        out_text
008000990825     C                   cat(p)    'W/E':1       out_text
008100990825     C                   cat(p)    chrdat#:1     out_text
008200990825     **
008300011009     C                   eval      *in99 = '1'
008400990825     C                   endif
008500990823     **
008600011009     C                   if        not(*in80) or *inof
008700011009     C                   Except    HEDR
008800011009     C                   Endif
008900011009     C                   eval      *in80 = '1'
009000950626     C                   ADD       1             TCOUNT            5 0
009100950713     C                   ADD       NRHRS         TOTNRS            7 2
009200950713     C                   ADD       OTHRS         TOTORS            7 2
009300961211     C  N76              ADD       GROS          TOTGRO           10 2
009400961211     C   76              ADD       NTWAGE        TOTGRO           10 2
009500961211     C                   ADD       TEAMT         TOTGRO           10 2
009600961211     C                   ADD       OTWAGE        TOTOVR           10 2
009700990825     **
009800990825     CL1                 exsr      dpt_calcs
009900990825
010000990825
010100990825     **
010200990825     C     dpt_calcs     begsr
010300971014     **
010400990825     C                   z-add     1             acc_typ           2 0
010500990825     C     acc_key       chain     defmt                              45
010600990825     C                   z-add     deacc         dacccs            6 0
010700990825     C                   z-add     desuba        dsubcs            4 0
010800990825     C                   z-add     despc         dspccs            1 0
010900990825     C                   z-add     desubl        dledcs            4 0
011000990825     C                   SETOFF                                       222324
011100951124     **
011200011009     C                   if        desubl > 0
011300011009     C                   eval      *in22 = '1'
011400011009     C                   Endif
011500990823     **
011600990825     C                   z-add     2             acc_typ           2 0
011700990825     C     acc_key       chain     defmt                              45
011800990825     C                   z-add     deacc         daccnt            6 0
011900990825     C                   z-add     desuba        dsubnt            4 0
012000990825     C                   z-add     despc         dspcnt            1 0
012100990825     C                   z-add     desubl        dlednt            4 0
012200011009     C                   if        desubl > 0
012300011009     C                   eval      *in23 = '1'
012400011009     C                   Endif
012500011009     **
012600990825     C                   z-add     3             acc_typ           2 0
012700990825     C     acc_key       chain     defmt                              45
012800990825     C                   z-add     deacc         daccot            6 0
012900990825     C                   z-add     desuba        dsubot            4 0
013000990825     C                   z-add     despc         dspcot            1 0
013100990825     C                   z-add     desubl        dledot            4 0
013200971014     **
013300011009     C                   if        desubl > 0
013400011009     C                   eval      *in24 = '1'
013500011009     C                   Endif
013600951124     **
013700990825     C                   ADD       TOTNRS        FINNRS            7 2
013800990825     C                   ADD       TOTORS        FINORS            7 2
013900990825     C                   ADD       TOTGRO        FINGRO           10 2
014000990825     C                   ADD       TOTOVR        FINOVR           10 2
014100990825     C     FINGRO        ADD       FINOVR        FINSAL           10 2
014200971014     **
014300990823
014400990823     ** call pgm to write to g/l files
014500990823     ** NORMAL TIME
014600990825     C                   if        updsts = 'Gl.Write'
014700011009
014800990825     C                   if        totgro <> *zeros
014900990825     C                   if        *in79
015000990825     C                   z-add     dacccs        out_acct
015100990825     C                   z-add     dsubcs        out_suba
015200990825     C                   z-add     dspccs        out_spc
015300990825     C                   z-add     dledcs        out_subl
015400990825     C                   else
015500990825     C                   z-add     daccnt        out_acct
015600990825     C                   z-add     dsubnt        out_suba
015700990825     C                   z-add     dspcnt        out_spc
015800990825     C                   z-add     dlednt        out_subl
015900990825     C                   endif
016000011009
016100990825     C                   z-add     totgro        out_amt
016200990825     C                   call      'APPIGLDR3'   parm_lst
016300990825     C                   endif
016400990823
016500990823     ** OVER-TIME
016600990825     C                   if        totovr <> *zeros
016700990825     C                   if        *in79
016800990825     C                   z-add     dacccs        out_acct
016900990825     C                   z-add     dsubcs        out_suba
017000990825     C                   z-add     dspccs        out_spc
017100990825     C                   z-add     dledcs        out_subl
017200990825     C                   else
017300990825     C                   z-add     daccot        out_acct
017400990825     C                   z-add     dsubot        out_suba
017500990825     C                   z-add     dspcot        out_spc
017600990825     C                   z-add     dledot        out_subl
017700990825     C                   endif
017800990825     C                   z-add     totovr        out_amt
017900990825     C                   call      'APPIGLDR3'   parm_lst
018000990825     C                   endif
018100990825     C                   endif
018200990825     C                   endsr
018300011009     ***************************************************
018400011009     ** First initilisation routine for all programs  **
018500011009     ***************************************************
018600011009     C     aainit        Begsr
018700011009     C                   eval      @title = arr(1)
018800011009     C                   eval      coname = get_coname
018900011009     C                   eval      @date = get_sys_date(date#)
019000011009     C                   Endsr
019100011009     ***************************************************
019200950626     OREPORT    H    1P                     2 04
019300950626     O         OR    OF
019400950925     O                       @PGM                10
019500011009     O                       coname              72
019600011009     O*         H    1P                  0  2
019700011009     O*        OR    OF
019800011009     O*                      @USER               10
019900011009     O*                                          65 'D"AGUIAR"S   INDUSTRIES'
020000011009     O*                                          78 '&  HOLDINGS'
020100990825     **
020200950713     O          E            HEDR           2
020300011009     O                       @USER               10
020400990825     O                       hdrtxt#             45
020500011009     O                       coname              72
020600011009     O*                                        +  1 'WAGES FOR FORTNIGHT ENDING'
020700011009     O                       @DATE             +  1
020800990825     **
020900950713     O          E            HEDR           2
021000950713     O                                           10 'N.T CODE'
021100950713     O                                           21 'O.T CODE'
021200950626     O                                           42 'DEPT NO'
021300950626     O                                           55 'NORMAL HRS'
021400950626     O                                           71 'OVERTIME HRS'
021500950626     O                                           85 'NORMAL TIME'
021600950626     O                                           98 'OVERTIME'
021700950626     OREPORT    T    L1                     2
021800951124     **
021900971107     O                  79   dacccs               8
022000971107     O                  79   dspccs        Z     10
022100971107     O                  79 22dledcs        Z     13
022200971107     O                     22                     9 '/'
022300971107     O                                            9 '-'
022400951124     **
022500971107     O                 N79   daccnt               8
022600971107     O                 N79   dspcnt        Z     10
022700971107     O                 N79 23dlednt        Z     13
022800971107     O                 N79 23                     9 '/'
022900951124     **
023000971107     O                 N79   daccot              20
023100951124     O                 N79                       21 '-'
023200971107     O                 N79   dspcot        Z     22
023300971107     O                 N79 24dledot        Z     25
023400951124     O                 N79 24                    21 '/'
023500951124     **
023600950626     O                       DEPT          ZB    39
023700950626     O                       TOTNRS        1B    54
023800950626     O                       TOTORS        1B    70
023900950713     O                       TOTGRO        1B    84
024000950626     O                       TOTOVR        1B    99
024100950713     O          T    LR                  2
024200950713     O                                           15 'TOTAL  EMPS.'
024300950713     O                       TCOUNT        Z     28
024400950713     O                       FINNRS        1     55
024500950713     O                       FINORS        1     71
024600950713     O                       FINGRO        1     85
024700950713     O                       FINOVR        1    100
024800950713     O                                           56 '*'
024900950713     O                                           72 '*'
025000950713     O                                           86 '*'
025100950713     O                                          101 '*'
025200950713     O          T    LR                  2
025300950713     O                                           25 'TOTAL AMOUNT PAID:'
025400950713     O                       FINSAL        1     40
025500011009**  Program Title
025600011009      Wages For Fortnight Ending-
