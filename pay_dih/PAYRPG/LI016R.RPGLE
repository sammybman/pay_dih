000100950907     H DECEDIT('.') DATEDIT(*DMY/)
000200990510     **
000300990510     ** PROGRAM:-EXECUTIVES DRINK ALLOWANCE
000400990511     ** SAMUEL BRITTON **
000500990510     **
000600990508     ** Customer Master file
000700990510     Fcusml11   if   e           k disk
000800990508     ** Customer Transaction Master
000900990511     Fcustnl05  if   e           k disk
001000020515     Fempmast   if   e           k disk
001100990511     Falwtfl01  if   e           k disk
001200990511     Falwmfl01  if   e           k disk
001300990511     FLISTING   o    f  132        printer
001400990510     **
001500950614     D TABCOD          S              2  0 DIM(12) CTDATA PERRCD(7) ASCEND
001600950614     D TABMTH          S              9    DIM(12) ALT(TABCOD)
001700971205     **
001800990511     D                sds
001900990511     D  @pgm             *proc
002000990511     D  @user                254    263
002100990511     **
002200990511     Ddnkytd           s              9  2
002201020515     Dcmnumb#          s                   like(cmnumb)
002300950907     **
002400990508     C     *entry        plist
002500000928     C                   parm                    prno              6 0
002600990511     C                   z-add     prno          mth               2 0
002700990510     **
002800990510     ** Parameter list to get customers balances
002900990510     **
003000990510
003100990510     C     get_list      plist
003200990510     C                   parm                    cmarea
003300020515     C                   parm                    cmnumb#
003400990510     C                   parm                    prno
003500990510     **
003600990510     C                   parm                    cmovr9
003700990510     C                   parm                    cmovr6
003800990510     C                   parm                    cmovr3
003900990510     C                   parm                    cmrent
004000950907     **
004100981203     C     *like         define    class         grade
004200990510     **
004300990510     c     cus_key       klist
004400990510     c                   kfld                    cmnumb#
004500990510     c                   kfld                    prno
004600990510     c                   kfld                    cmarea
004700990510     ** Print Report Header
004800990511     C     mth           lookup    tabcod        tabmth                   07
004900990510     C                   except    hedr1
005000990511     ** Specify area as Accounts Receivable (type 1)
005100990511     C                   z-add     1             #area             2 0
005200990510     **
005300990510     ** Set lower limit on Customer Master file to select
005400990510     ** Accounts Receivable records
005500990510     c     #area         setll     cmsfmt
005600990510     c     #area         reade     cmsfmt                                 lr
005700990511     c                   dow       not(*inlr)
005800990510
005900990510     ** Get opening balances for customer
006000990510     c                   z-add     cmnumb        cmnumb#
006100990510     C                   call      'CUSOPNBAL'   get_list
006200990510     **
006300990511     C                   z-add     0             dnkdif
006400990511     C                   z-add     0             totdnk            9 2
006500990510     **
006600020515     C     cmemp         chain     emprec                             99
006700990510     ** Select Executives Only
006800990511     C     *in99         ifeq      '0'
006900990510
007000990510     ** Print Customer/Employee Header records
007100990511     C                   except    hedr
007200990510     C                   except    dnhdr
007300990510     **
007400990511     ** Remove Probationer status from employee grade to get
007500990511     ** true grade
007600990511     C     'P':' '       xlate     class         grade             3
007700950908     **
007800950907     **
007900990508     ** Get Drinks Entitlement
008000971205     C                   exsr      alw_calcs
008100990510     **
008200990510     c                   z-add     cmnumb        cmnumb#
008300990510     C     cus_key       chain     ctfmt                              05
008400990510     C     *in05         downe     '1'
008500990510     **
008600990510     **get new month end bal
008700990510     C                   add       ctamt         totdnk
008800990510     **
008900990510     c                   except    dndet
009000990510     c     cus_key       reade     ctfmt                                  05
009100990510     C                   Enddo
009200990510     **
009300990510     C                   exsr      prnt
009400990510     C                   exsr      totsub
009500990510     **
009600990511     C                   Endif
009700990510     c     #area         reade     cmsfmt                                 lr
009800990511     C                   enddo
009900950908     **
010000990510     **
010100990510     ************************************************************
010200990510     ** Routine To Print Month's transactions totals
010300990510     ************************************************************
010400990510     C     PRNT          BEGSR
010500990510     C     totdnk        ifgt      *zeros
010600990510     C                   except    emptot
010700990510     C                   endif
010800990510     C                   ENDSR
010900950908     **
011000990510     ************************************************************
011100990510     ** Routine To Print Customer / Employee Final Totals      **
011200990510     ************************************************************
011300950127     C     TOTSUB        BEGSR
011400990511     C                   eval      dnkytd = cmovr9 + cmovr6 + cmovr3 + cmrent
011500990511     C                                      + totdnk
011600990511     C     mth           IFGE      10
011700990511     C     mth           SUB       9             mthcom            2 0
011800950127     C                   ELSE
011900990511     C     mth           ADD       3             mthcom
012000950127     C                   ENDIF
012100950127     C     DNKNTL        DIV(H)    12            DNKM              8 2
012200990511     C     DNKM          MULT      mthcom        DNKMTL            9 2
012300950614     C     DNKYTD        SUB       DNKMTL        DNKDIF            8 2
012400960329     ** CHECKS TO SEE IF EMPLOYEE UNDER SPENT
012500960329     C                   SETOFF                                       78
012600960329     C     DNKDIF        IFLT      0
012700960329     C                   SETON                                        78
012800960329     C                   ENDIF
012900960329     ** CHECKS TO SEE IF EMPLOYEE OVER SPENT
013000960329     C                   SETOFF                                       79
013100960329     C     DNKDIF        IFGT      0
013200960329     C                   SETON                                        79
013300960329     C                   ENDIF
013400960329     **
013500990511     C                   add       dnkytd        ytddnk           10 2
013600990511     C                   add       totdnk        dnkmth           10 2
013700990511     C                   add       dnkntl        totntl           10 2
013800990510     C                   except    empfin
013900950127     C                   ENDSR
014000990508
014100971205     **
014200990511     ************************************************************
014300990511     **    Routine To Get Employee Drinks Allowances           **
014400990511     ************************************************************
014500971205     C     alw_calcs     BEGSR
014600971205     **
014700990508     **
014800990508     C     alw_key1      klist
014900990508     C                   kfld                    alttyp
015000990508     C                   kfld                    grade
015100990508     C                   kfld                    emp
015200990508     **
015300990508     C     alw_key2      klist
015400990508     C                   kfld                    alttyp
015500990508     C                   kfld                    grade
015600990508     **
015700990508      *** ROUTINE HANDLING ALLOWANCE PERCENTAGE ALLOCATION FOR GRADES 8 & 9
015800990508      *** EMPLOYEES
015900990508     **
016000990508     **
016100990508     C                   call      'GETALLPR'
016200990508     C                   parm                    emp
016300990508     C                   parm                    class
016400990508     C                   parm                    per_alloc         3 2
016500990508     C                   parm      *zeros        mnts              5 0
016600990508     **
016700990508     ** Accumulate amounts for all allowances
016800990508     C     *loval        setll     altrec
016900990508     C                   read      altrec                                 33
017000990508     C                   dow       not(*in33)
017100990508     **
017200990508     C     alw_key1      chain     alwrec                             77
017300990508     C   77alw_key2      chain     alwrec                             77
017400990508
017500990508     C                   if        not(*in77)
017600990508     C
017700990508     **
017800990508     C                   mult      per_alloc     alwamt                         from allocation
017900990508     **
018000990508     **  Total the different allowances
018100990508     C                   select
018200000928     **  Drink allowance
018300990508     C                   when      alttyp = 5                                   Drinks allowance
018400990511     C                   z-add     alwamt        dnkntl            9 2
018500990508     C                   endsl
018600990508
018700990508     C                   endif
018800990508     **
018900990508     C                   read      altrec                                 33
019000990508     C                   enddo
019100990508     **
019200990508     C                   endsr
019300971205     **
019400990511
019500950614     OLISTING   H    1P                     1 03
019600950928     O                       @PGM                10
019700950614     O                                           77 'B A N K S  D I H  L T D'
019800950614     O          H    1P                     2
019900950928     O                       @USER               10
020000950614     O                                           77 '======================='
020100950614     O          H    1P                  0  1
020200950614     O                                           71 'D"AGUIAR"S   INDUSTRIES'
020300950614     O                                           84 '&  HOLDINGS'
020400950614     O          H    1P                  0  2
020500950614     O                                           72 '========================'
020600950614     O                                           84 '============'
020700950908     O          E            HEDR1          1
020800950614     O                                           61 'D R I N K  A L L O W A N'
020900950614     O                                           75 'C E  A S  A T'
021000950614     O                     07TABMTH              85
021100950907     O                       *YEAR               91
021200950908     O          E            HEDR1          2
021300950614     O                                           61 '------------------------'
021400950614     O                                           80 '------------------------'
021500950614     O                                           91 '-----------'
021600950908     O          E            HEDR           2
021700940829     O                       CHRIST              36
021800950614     O                       SURNAM              50
021900990510     O                                           72 'Customer # - '
022000990511     O                       Cmnumb        Z   +  1
022100990511     O                                           95 'Employee # - '
022200990511     O                       Cmemp         Z   +  1
022300990510     **
022400950908     O          E            DNHDR          2
022500950614     O                                           11 'DATE'
022600950614     O                                           52 'PARTICULARS'
022700950614     O                                           95 'AMOUNT'
022800950614     O                                          110 'TOT.AMT.'
022900950908     O          E            DNHDR          1
023000950614     O                                           42 'D R I N K'
023100950908     O          E            DNHDR          2
023200950614     O                                           42 '---------'
023300990510     ** Employee Detail Records
023400950908     O          E            DNDET          1
023500990510     O                       ctdate              13 '    /  /  '
023600000928     O                       ctnumb              28
023700000928     O                       ctorg               32
023800990510     O                       cttext              70
023900990510     O                       ctamt         A     97
024000990510     ** Employee Total Record
024100950908     O          E            EMPTOT         1
024200950908     O                       TOTDNK        A    112
024300990510     ** Employee YTD. record
024400950908     O          E            EMPFIN         1
024500950614     O                                           30 'D R I N K'
024600950908     O          E            EMPFIN         2
024700950614     O                                           30 '---------'
024800950908     O          E            EMPFIN         2
024900950614     O                                           26 'YEARLY ALLOWANCE'
025000950614     O                                           72 'MONTH-TO-DATE ALLOWANCE'
025100950614     O                       DNKNTL        A     43
025200950614     O                       DNKMTL        A     95
025300950908     O          E            EMPFIN         1
025400950614     O                                           58 'TO DATE--'
025500950614     O                       UDATE         Y     68
025600950614     O                       DNKYTD        A     95
025700950908     O          E            EMPFIN         1
025800950614     O                                           95 '------------'
025900950908     O          E            EMPFIN         1
026000960329     O               78                          60 'UNDER SPENT'
026100960329     O               79                          60 'OVER SPENT '
026200950614     O                       DNKDIF        A     95
026300950908     O          E            EMPFIN         1
026400950614     O                                           95 '------------'
026500950908     O          E            EMPFIN         1
026600950126     O                                           25 'PLEASE NOTE: EXCESS WILL'
026700950126     O                                           49 ' BE CHARGED TO LOAN ACC.'
026800950126     O                                           73 ' ON AN HALF YEARLY BASIS'
026900950126     O                                           97 ' I.E. MARCH & SEPT. OF E'
027000950126     O                                          105 'ACH YEAR'
027100950908     O          E            EMPFIN         2
027200950614     O                                           24 '========================'
027300950614     O                                           48 '========================'
027400950614     O                                           72 '========================'
027500950614     O                                           96 '========================'
027600950614     O                                          120 '========================'
027700950614     O                                          132 '============'
027800990510
027900990510     ** Report Final Totals
028000950614     OLISTING   T    LR                     3
028100950614     O                                           24 'MONTH''S TOTAL'
028200950614     O                       DNKMTH        A     43
028300950614     O                                           46 '*'
028400950614     O                       DNKMTH        A    111
028500950614     O                                          114 '**'
028600950614     O          T    LR                     3
028700950614     O                                           24 'TO DATE TOTALS'
028800950614     O                       YTDDNK        A     43
028900950614     O                                           46 '*'
029000950614     O                       YTDDNK        A    111
029100950614     O                                          114 '**'
029200940829     O          T    LR                     3
029300940829     O                                           24 'TOTAL ENTITLEMENT'
029400940829     O                       TOTNTL        A     43
029500940829     O                                           46 '*'
029600950614     OLISTING   T    LR
029700950614     O                                           77 '** END  OF  REPORT  **'
029800950110**
02990095011001JANUARY  02FEBRUARY 03MARCH    04APRIL    05MAY      06JUNE     07JULY
03000095011008AUGUST   09SEPTEMBER10OCTOBER  11NOVEMBER 12DECEMBER
