000100181031     H DECEDIT('.') DATEDIT(*DMY/)
000200110803     H dftactgrp(*no) bnddir('DISDIR':'PAYDIR')
000300110630      // XMAS BONUS LETTERS - SALESMEN & OTHERS
000400021125     Fxmsbonl1  ip   E           K DISK
000500021125     Fdeptm     if   E           K DISK
000600021125     FABSSUM    if   E           K DISK
000700950727     FREPORT    O    F  132        PRINTER
000800950620     F                                     FORMLEN(066)
000900950620     F                                     FORMOFL(066)
001000110630      //
001100011123     D award1          s              3  3
001200011123     D fulper          s              9  2
001300011123     D coeful          s              9  2
001400011123     D thrnet          s              9  2
001500011123     D coethr          s              9  2
001600011123     D coefst          s              9  2
001700011123     D coesec          s              9  2
001800011123     D fstnet          s              9  2
001900011123     D secnet          s              9  2
002000011123     D tabs            s              4  1
002100011123     D name#           s             40
002200011123     D name            s             26
002300110630      //class           s              3
002400110630      //status          s              1
002500110630      //
002600950921     D                SDS
002700950921     D  @PGM             *PROC
002800141203     D  @USER                358    367
002900181029       //
002901181029          // DCL-S fstper PACKED(4:1) INZ(0);
002902181029           //
002903181029       DCL-S tper PACKED(4:1) INZ(0);
002904181029       //
002905181029       DCL-DS xmasconDS EXTNAME('XMASCON');
002906181029       END-DS ;
002907181029      //
003000110630      /copy qcpysrc,genpr
003100110630      /copy qcpysrc,paypr
003200110630      //********************************************************************
003300110630      // 1-NO.OF DAYS ABSENT. 2-% FOR EACH DAY ABSENT. 3-TOTAL % PENALTY.
003400110630      // 4-TOTAL % PENALTY WITH TWO DECIMAL POSITIONS.
003500110630      //********************************************************************
003600950726     IABSSREC
003700950726     I              TOTSIC                      SICK
003800950726     I              TOTIND                      INDACC
003900950726     I              TOTAWL                      AWL
004000950726     I              TOTAWO                      AWOL
004100950726     I              TOTJUR                      JURY
004200950726     I              TOTSUS                      SUSP
004300110630     I              TOTLAT                      LATE
004400110630      //
004500110630      //
004600110630      /FREE
004700110630         CHAIN EMP ABSSUM;
004800110630         CHAIN DEPT dhrec;
004900110630         //
005000110630         if nempflag = 'Y';
005100110630         *in06 =  *on;
005200110630         else;
005300110630         *in06 =  *off;
005400110630         endif;
005500110630          //
005600110630         *IN50 = *OFF;
005800181029             TOTBON    = 0;
005900181029              FULPER   = 0;
006000181029             COEFUL   = 0;
006200110630         status = get_emp_info(emp:dept:*blanks:
006300110630           name#:class);
006400110630        if status <> 'G';
006500110630         name = name#;
006600110630         //
006700110630      /END-FREE
006800011123     C                   ADD       1             COUNT             3 0
006900950727     C     *YEAR         SUB       1             YEAR              4 0
007000001129     C                   Z-ADD     GCOE          TCOE              9 2
007100001129     C                   Z-ADD     LETBAS        TAX25             9 2
007200001129     C                   Z-ADD     LETCOE        NTX25             9 2
007300950727     C                   Z-ADD     AWARD         MANAWR            3 1
007400110630      /FREE
007500110630         AWARD1 = award / 100;
007600110630         //***********************
007700110630         // BASIC BONUS ROUTINE **
007800110630         //***********************
007900110630      /END-FREE
008000011123     C  N06              eval      fulper = gros * aaward / 100                 7 % FOR ATTEN.
008100110630      /FREE
008200110630         fstnet = gros * fstper / 100;                                        //7 % FOR ATTEN.
008300110630         secnet = gros * secper / 100;                                        //7 % FOR ATTEN.
008400110630         thrnet = gros * awolpr / 100;                                        //AWOL
008500110630      /END-FREE
008600001129     C     GROS          MULT      AWARD1        MANAMT            9 2          MAN. AWARD
008700110630      //******************
008800110630      // C.O.E. ROUTINE **
008900110630      //******************
009000011123     C  N06              eval      coeful = tcoe * aaward / 100                 7 % FOR ATTEN.
009100110630      /FREE
009200110630         coefst = tcoe * fstper / 100;                                        //7 % FOR ATTEN.
009300110630         coesec = tcoe * secper / 100;                                        //7 % FOR ATTEN.
009400110630         coethr = tcoe * awolpr / 100;                                        //AWOL
009500110630      /END-FREE
009600001129     C     TCOE          MULT      AWARD1        COEAWA            9 2          COE AWARD
009700110630      //
009800110630      // TOTALS ACCUMULATION
009900110630      //
010000001129     C     FSTNET        ADD       SECNET        TPENAL            9 2          AWARDS & PENAL.
010100001129     C     THRNET        ADD       TPAWOL        TPAWOL            9 2
010200001129     C     COEFST        ADD       COESEC        NPENAL            9 2
010300001129     C     COETHR        ADD       NPAWOL        NPAWOL            9 2
010400001129     C     TPENAL        ADD       NPENAL        PENAL             9 2
010500001129     C     TPAWOL        ADD       NPAWOL        PAWOL             9 2
010600001129     C     FULPER        ADD       COEFUL        TRAWA             9 2
010700001129     C     MANAMT        ADD       COEAWA        TAWARD            9 2
010800001129     C     GROS          ADD       FULPER        GTAX              9 2
010900950727     C     MANAMT        ADD       GTAX          GTAX
011000001129     C     TCOE          ADD       COEFUL        GNTAX             9 2
011100950727     C     COEAWA        ADD       GNTAX         GNTAX
011200001129     C     TPENAL        ADD       TPAWOL        GTPEN             9 2
011300001129     C     NPENAL        ADD       NPAWOL        GNPEN             9 2
011400001129     C     PENAL         ADD       PAWOL         GPEN              9 2
011500001129     C     GTAX          SUB       GTPEN         NTAX              9 2
011600001129     C     GNTAX         SUB       GNPEN         NNTAX             9 2
011700001129     C     GROS          ADD       TCOE          TOTBON            9 2
011800001129     C     TOTBON        ADD       TRAWA         GTOT              9 2
011900950727     C     TAWARD        ADD       GTOT          GTOT
012000001129     C     GTOT          SUB       GPEN          NTOT              9 2
012100011123     C                   z-add     nntax         NPNTAX            9 2
012200011123     C                   z-add     ntot          NPTOT             9 2
012300011123
012400110630         TABS = awl + awol + jury
012500110630             + susp + sick + indacc;
012600110630         *in50 = tabs > *zeros;
012700181029     C*    FSTPER        ADD       SECPER        TPER              4 1
012800181029     C*    TPER          ADD       AWOLPR        FSTPER            4 1
012801181029               tper = fstper + secper;
012802181029               fstper = awolpr + tper;
012900181010
013000110630         if fstper >= 75
013100110630               and nempflag <> 'Y';
013200110630           ntax = tax25;
013300110630           npntax = ntx25;
013400110630           nptot = ntax + npntax;
013500110630         endif;
013600011123
013700110630         // Print employee letter
013800110630         exsr print_letter;
013900141203         //if not printing, clear all relevant fields
014000141203         else;
014100141203         TABS = *zero;
014200141203          awl = 0;
014300141203          awol = 0;
014400141203          jury= 0;
014500141203          susp = 0;
014600141203          sick = 0;
014700141203          indacc = 0;
014800141203          *in50 = *off;
014900141203
015000110630       endif;
015100011123
015200110630       //*****************************
015300110630       // Iniz subroutine           **
015400110630       //*****************************
015500110630       begsr *inzsr;
015600110630         in *lock *dtaara;
015700011122     C     *DTAARA       define    xmsseqchk     xmseq#            2 0
015800110630      //
015900110630         if xmseq#  <> 4;
016000011122     C                   call      'ESCPMSG'
016100011122     C                   parm                    @pgm
016200011122     C                   parm      'DIH0131'     msgid             7
016300011122     C                   parm      'QGPL'        @lib             10
016400011122     C                   parm      'DIHMSGF'     @FILE            10
016500110630         endif;
016600110630         //
016700110630           Xmseq# = 5;
016800110630         out xmseq#;
016900110630       endsr;
017000110630       //*****************************
017100110630       // Print Letter Routine      **
017200110630       //*****************************
017300110630       begsr print_letter;
017400011123
017500110630         // name info. gross bonus etc.
017600110630         except header;
017700110630         // special text for sales personnel
017800110630         if dtype = 'S' or dtype = 'B'
017900110630               or dept = 52;
018000110630           except saldetl;
018100110630         endif;
018200181010
018300110630         except grsdetl;
018400110630         // Text for New employees Vs. Old employees
018500110630         if nempflag = 'Y';
018600110630           if aaward > *zeros;
018700110630             except nempdetl;
018800110630           endif;
018900110630         else;
019000110630           except oempdetl;
019100110630         endif;
019200110630         except detail;
019300110630         // Text for employees with awol records
019400110630         if awolpr > *zeros;
019500110630           except awoldetl;
019600110630         endif;
019700110630         // summaries etc.
019800110630         except totals;
019900110630       endsr;
020000011123
020100011123     OREPORT    e            header         1 01
020200950921     O                       @PGM                10
020300950620     O                                           71 'BANKS  D.I.H  LTD'
020400011123     O          e            header         1
020500950620     O                                           71 '================='
020600011123     O          e            header      0  1
020700950620     O                                           68 'D"AGUIAR"S   INDUSTRIES'
020800950620     O                                           81 '&  HOLDINGS'
020900011123     O          e            header      0  1
021000950620     O                                           69 '========================'
021100950620     O                                           81 '============'
021200011123     O          e            header         1
021300950620     O                                           68 'XMAS BONUS AWARD --'
021400950620     O                       UDATE         Y     77
021500011123     O          e            header         1
021600950620     O                                           73 '========================'
021700950620     O                                           77 '===='
021800011123     O          e            header         2
021900011123     O                       name                26
022000011123     O                       emp           Z     36
022100950620     O                                           37 '-'
022200011123     O                       dept          Z     40
022300011123     O          e            header         1
022400950620     O                                           16 'FELLOW EMPLOYEE,'
022500011123     O          e            header         1
022600950620     O                                           26 'ATTACHED HERETO IS YOUR '
022700950620     O                                           50 'XMAS BONUS PAYSLIP. THE '
022800950620     O                                           75 'XMAS BONUS FOR THIS YEAR'
022900950620     O                                           99 ' HAS BEEN FIXED ACCORDIN'
023000950620     O                                          123 'G TO YOUR GRADE. AS A GR'
023100950620     O                                          126 'ADE'
023200011123     O                       class          B   129
023300011123     O          e            header         2
023400950620     O                                           24 'EMPLOYEE,YOU ARE ENTITLE'
023500001129     O                                           29 'D TO '
023600001129     O                       TOTBON        3     42 '$'
023700001129     O                                           51 'OF WHICH'
023800001129     O                       TCOE          3     62 '$'
023900001129     O                                           68 'IS TA'
024000001129     O                                           92 'X FREE. PENALTIES OR AWA'
024100001129     O                                          116 'RDS ARE THEN SUBTRACTED '
024200001129     O                                          125 'OR ADDED.'
024300011123     O          e            saldetl        1
024400950620     O                                           31 'N.B:- XMAS BONUS FOR SAL'
024500950620     O                                           55 'ESMEN & SALES MANAGERS H'
024600950620     O                                           79 'AS BEEN CALCULATED AT 80'
024700950620     O                                           90 '% OF GROSS.'
024800011123     O          e            saldetl        2
024900950620     O                                           31 '========================'
025000950620     O                                           55 '========================'
025100950620     O                                           79 '========================'
025200950620     O                                           90 '==========='
025300011123     O          e            grsdetl        1
025400950620     O                                           24 'THE CALCULATIONS ARE AS '
025500950620     O                                           37 'SHOWN BELOW:-'
025600091130     O                                          118 'XMAS BONUS      XMAS COE'
025700950620     O                                          132 'TOTAL'
025800011123     O          e            grsdetl        1
025900091130     O                                          118 '----------    ----------'
026000950620     O                                          132 '-----'
026100011123     O          e            grsdetl        2
026200950620     O                                           20 'GROSS AMOUNT PAYABLE'
026300950620     O                                           80 '--'
026400001129     O                       GROS          3B   103 '$'
026500001129     O                       TCOE          3B   117 '$'
026600001129     O                       TOTBON        3    132 '$'
026700011123     O          e            oempdetl       2
026800950620     O                                           13 'PLUS AWARD OF'
026900011123     O                       aaward        1B    17
027000940516     O                                           24 ' % FOR '
027100950620     O                                           48 'ABSENCE OF NOT MORE THAN'
027200950620     O                                           55 ' 3 DAYS'
027300950620     O                                           92 '--           +'
027400001129     O                       FULPER        3    103
027500001129     O                       COEFUL        3    117
027600001129     O                       TRAWA         3B   132
027700950620     O                                          107 '+'
027800950620     O                                          121 '+'
027900011123     O          e            nempdetl       2
028000950620     O                                            8 'AWARD OF'
028100011123     O                       aaward        1B    12
028200940516     O                                           36 ' % FOR ABSENCE OF 3 DAYS'
028300950620     O                                           60 ' OR LESS IS ADDED TO YOU'
028400950620     O                                           80 'R GROSS AMOUNT    --'
028500011123     O          e            detail         1
028600950620     O                                           24 'PLUS MANAGERIAL AWARD OF'
028700950620     O                       MANAWR        1B    28
028800950620     O                                           52 '% FOR EXCEPTIONAL WORK O'
028900950620     O                                           70 'R GOOD PERFORMANCE'
029000950620     O                                           92 '--           +'
029100001129     O                       MANAMT        3B   103
029200001129     O                       COEAWA        3B   117
029300001129     O                       TAWARD        3B   132
029400950620     O                                          107 '+'
029500950620     O                                          121 '+'
029600011123     O          e            detail         1
029700950620     O                                          114 '------------------------'
029800950620     O                                          132 '------------------'
029900011123     O          e            detail         1
030000011123     O                                           80 'GROSS BONUS        --'
030100950620     O                                           92 '+'
030200950620     O                                          107 '+'
030300950620     O                                          121 '+'
030400001129     O                       GTAX          3B   103
030500001129     O                       GNTAX         3B   117
030600001129     O                       GTOT          3B   132
030700011123     O          e            detail         1
030800950620     O                                          114 '------------------------'
030900950620     O                                          132 '------------------'
031000011123     O          e            detail         1
031100950620     O                                           16 'LESS PENALTIES:-'
031200011123     O          e            detail         1
031300950620     O                                           14 '--------------'
031400011123     O          e            detail         1
031500950620     O                                           24 'BASED ON THE TABLE SHOWN'
031600950620     O                                           48 ' FOR PENALTIES BELOW YOU'
031700950620     O                     50                    64 ' WERE ABSENT FOR'
031800950620     O                     50TABS          1     70
031900950620     O                     50                    75 'DAYS'
032000950620     O                    N50                    72 ' WERE NOT ABSENT DURING '
032100950620     O                    N50                    81 'THE YEAR.'
032200011123     O          e          50detail         2
032300950620     O                                           24 'FOR WHICH YOU HAVE BEEN '
032400950620     O                                           33 'PENALISED'
032500950620     O                       TPER          1B    39
032600950620     O                                           64 '% OF YOUR GROSS BONUS OF'
032700001129     O                       TOTBON        3     75 '$'
032800950620     O                                           92 '.  --           -'
032900950620     O                                          107 '-'
033000950620     O                                          121 '-'
033100001129     O                       TPENAL        3B   103
033200001129     O                       NPENAL        3B   117
033300001129     O                       PENAL         3B   132
033400011123     O          e            awoldetl       1
033500950620     O                                           11 'IN ADDITION'
033600950620     O                       AWOL          1     16
033700950620     O                                           23 'OF THE'
033800950620     O                       TABS          1     29
033900950620     O                                           54 'DAYS WERE AWOL FOR WHICH'
034000950620     O                                           70 ' YOU HAVE LOST A'
034100011123     O          e            awoldetl       1
034200950620     O                                            7 'FURTHER'
034300950727     O                       AWOLPR        1     12
034400950620     O                                           37 '% OF THE GROSS AMOUNT OF'
034500001129     O                       TOTBON        3     48 '$'
034600950620     O                                           49 '.'
034700950620     O                                           92 '--           -'
034800950620     O                                          107 '-'
034900950620     O                                          121 '-'
035000001129     O                       TPAWOL        3B   103
035100001129     O                       NPAWOL        3B   117
035200001129     O                       PAWOL         3B   132
035300011123     O          e            totals         1
035400950620     O                                          114 '------------------------'
035500950620     O                                          132 '------------------'
035600011123     O          e            totals         1
035700950620     O                                           79 'TOTAL PENALTIES        -'
035800950620     O                                           92 '-           -'
035900950620     O                                          107 '-'
036000950620     O                                          121 '-'
036100001129     O                       GTPEN         3B   103
036200001129     O                       GNPEN         3B   117
036300001129     O                       GPEN          3B   132
036400011123     O          e            totals         1
036500950620     O                                          114 '------------------------'
036600950620     O                                          132 '------------------'
036700011123     O          e            totals         1
036800950620     O                                           80 'NET BONUS        --'
036900001129     O                       NTAX          3    103 '$'
037000001129     O                       NNTAX         3B   117 '$'
037100001129     O                       NTOT          3B   132 '$'
037200011123     O          e            totals         1
037300950620     O                                          114 '------------------------'
037400950620     O                                          132 '------------------'
037500011123     O          e            totals         1
037600950620     O                                           44 'THE NET BONUS PAYABLE AC'
037700950620     O                                           68 'CORDING TO YOUR PAYSLIP '
037800950620     O                                           90 'IS        --       ***'
037900001129     O                       NTAX          3B   103 '$'
038000001129     O                       NPNTAX        3B   117 '$'
038100001129     O                       NPTOT         3B   132 '$'
038200011123     O          e            totals         2
038300950620     O                                          114 '------------------------'
038400950620     O                                          132 '------------------'
038500011123     O          e            totals         2
038600950620     O                                           24 'THE TABLE FOR PENALTIES '
038700950620     O                                           39 'IS AS FOLLOWS:-'
038800011123     O          e            totals         1
038900950620     O                                           24 '(A) FOR ABSENCE MORE THA'
039000950620     O                                           32 'N 3 DAYS'
039100011123     O          e            totals         1
039200950620     O                                           28 'ABSENCE FROM THE 4TH DAY'
039300021125     O                                           52 ' TO THE 10TH DAY,LESS 4 '
039400021125     O                                           76 '% FOR EACH DAY OR 28 % F'
039500051201     O                                           87 'OR 10 DAYS.'
039600110630      //                                         124 ' TOTAL PENALTIES EXCEED '
039700110630      //                                         132 '100 %   *'
039800110630      //         e            totals         1
039900110630      //                                         119 '*  OF YOUR GROSS TAXABLE'
040000110630      //                                         132 ' PAY,THE    *'
040100011123     O          e            totals         1
040200950620     O                                           24 '(B) ABSENCE ABOVE 10 DAY'
040300950620     O                                           25 'S'
040400051201     O                                          119 '************************'
040500051201     O                                          132 '*************'
040600181029     O**        e            totals         1
040700181029     O**                                         96 '*'
040800181029     O**                                        132 '*'
040900110630      //                                         119 '*  DIFFERENCE IS SUBTRAC'
041000110630      //                                         132 'TED FROM    *'
041100110630      //         e            totals         1
041200110630      //                                         116 '*  YOUR TAX FREE PAY.'
041300110630      //                                         132 '*'
041400011123     O          e            totals         1
041500021125     O                                           28 'LESS 28 % FOR THE FIRST '
041600950620     O                                           52 '10 DAYS AND 5 % FOR EACH'
041700950620     O                                           68 ' ADDITIONAL DAY.'
041800051201     O                                          121 '*  PLEASE NOTE THAT CHRIST' +
041900051201     O                                              'MAS'
042000051201     O                                          132 '*'
042100051201     O          e            totals         1
042200181029     O                                          119 '*  BONUS IS NOW PENALISED'
042300051201     O                                          132 '*'
042400051201     O          e            totals         1
042500181029     O                                          119 '*  UP TO 75%.           '
042600051201     O                                          132 '*'
042700011123     O          e            totals         1
042800950620     O                                           24 '(C) ABSENCE WITHOUT LEAV'
042900181029     O                                           48 'E INCURS ADDITIONAL PENA'
042901181029     O                                           72 'LTIES OF 4% FOR EACH DAY'
043000950620     O                                           96 '*'
043100950620     O                                          132 '*'
043200011123     O          e            totals         1
043300950620     O                                           96 '*'
043400061113     O                                          100 'IF'
043500061113     O                                          124 ' THERE IS ANY DISAGREEME'
043600061113     O                                          126 'NT'
043700950620     O                                          132 '*'
043800011123     O          e            totals         1
043801181029     O                                           18 '(D) LATENESS ABOVE '
043802181029     O                       LATEDAYS            21
043900181029     O                                           46 'DAYS INCURS PENALIES OF '
044000181029     O                       LATEPEN             51
044001181029     O                                           72 ' % FOR ADDITIONAL DAY'
044100061113     O                                           96 '*'
044200061113     O                                          122 'WITH YOUR ATTENDANCE REC'
044300061113     O                                          127 'ORD, '
044400061113     O                                          132 '*'
044500011123     O          e            totals         1
044600950620     O                                          119 '*  KINDLY SEE THE HEAD O'
044700950620     O                                          132 'F YOUR      *'
044800011123     O          e            totals         1
044900950620     O                                           24 'ACCORDING TO OUR FILES, '
045000950620     O                                           48 'YOUR ATTENDANCE RECORD F'
045100950620     O                                           72 'OR THE YEAR FROM 1ST.NOV'
045200950727     O                                           73 '.'
045300950620     O                       YEAR                77
045400950727     O                                           90 'TO 31ST.OCT.'
045500950727     O                       *YEAR               94
045600950620     O                                          109 '*  DEPARTMENT.'
045700950620     O                                          132 '*'
045800011123     O          e            totals         1
045900950620     O                                           16 'WAS AS FOLLOWS:-'
046000950620     O                                           96 '*'
046100950620     O                                          132 '*'
046200011123     O          e            totals         2
046300950620     O                                          119 '************************'
046400950620     O                                          132 '*************'
046500011123     O          e            totals         1
046600950620     O                                           24 'X-----------------------'
046700950620     O                                           48 '----------DAYS----------'
046800950620     O                                           72 '-----------------------X'
046900011123     O          e            totals         1
047000950620     O                                           24 'SICK LEAVE  A.W.L  A.W.O'
047100950620     O                                           48 '.L   JURY SERV. IND.ACC.'
047200110630     O                                           61 '  LATE  SUSP.'
047300110630     O                                           77 ' TOTAL PENALTIES'
047400011123     O          e            totals         1
047500950726     O                       SICK          2B     7
047600950726     O                       AWL           2B    17
047700950620     O                       AWOL          2B    24
047800950726     O                       JURY          2B    36
047900950620     O                       INDACC        2B    46
048000110630     O                       LATE          2B    54
048100110630     O                       SUSP          2B    61
048200110630     O                       FSTPER        2B    72
048300011123     O          e            totals         1
048400061113     O                                          115 'T.I. BYNOE'
048500011123     O          e            totals
048600061113     O                                          120 'SECRETARY/M.I.S. EXECUTI'
048700061113     O                                          122 'VE'
048701181029
048702181029       // --------------------------------------------------
048703181029       // Procedure name: retrieveXmasConRecord
048704181029       // Purpose:
048705181029       // Returns:
048706181029       // --------------------------------------------------
048707181029       DCL-PROC retrieveXmasConRecord ;
048708181029         // Your local fields go here
048709181029
048710181029         // Your calculation code goes here
048711181029
048712181029         return ;
048713181029       END-PROC ;
048714181029
