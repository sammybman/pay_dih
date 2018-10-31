000100110921     H DECEDIT('.') DATEDIT(*DMY/)
000101111201     H bnddir('DISDIR':'PAYDIR') dftactgrp(*no) actgrp(*new)
000200111005      // XMAS BONUS LETTERS - SALESMEN & OTHERS   -new version(2011/09/01
000300021125     Fxmsbonl1  ip   E           K DISK
000400021125     Fdeptm     if   E           K DISK
000500021125     FABSSUM    if   E           K DISK
000600950727     FREPORT    O    F  132        PRINTER
000700950620     F                                     FORMLEN(066)
000800950620     F                                     FORMOFL(066)
000900110630      //
001000011123     D award1          s              3  3
001100011123     D fulper          s              9  2
001200011123     D coeful          s              9  2
001300011123     D thrnet          s              9  2
001400011123     D coethr          s              9  2
001500011123     D coefst          s              9  2
001600011123     D coesec          s              9  2
001700011123     D fstnet          s              9  2
001701111201     D npntax          s              9  2
001702111201     D nntax           s              9  2
001800011123     D secnet          s              9  2
001900011123     D tabs            s              4  1
001901110705     D lateper         s              4  2
001902110907     D tper            s              4  1
001903110705     D latetype        s              2  0 inz(8)
002000011123     D name#           s             40
002001110907     D coname          s             30
002100011123     D name            s             26
002400110630      //
002500950921     D                SDS
002600950921     D  @PGM             *PROC
002700950921     D  @USER                254    263
002800110630      //
002900110630      /copy qcpysrc,genpr
003000110630      /copy qcpysrc,paypr
003100110630      //********************************************************************
003200110630      // 1-NO.OF DAYS ABSENT. 2-% FOR EACH DAY ABSENT. 3-TOTAL % PENALTY.
003300110630      // 4-TOTAL % PENALTY WITH TWO DECIMAL POSITIONS.
003400110630      //********************************************************************
003500950726     IABSSREC
003600950726     I              TOTSIC                      SICK
003700950726     I              TOTIND                      INDACC
003800950726     I              TOTAWL                      AWL
003900950726     I              TOTAWO                      AWOL
004000950726     I              TOTJUR                      JURY
004100950726     I              TOTSUS                      SUSP
004101110630     I              TOTLAT                      LATE
004200110630      //
004300110630      //
004301110630      /FREE
004302110907          exsr aainit;
004400110630         CHAIN EMP ABSSUM;
004500110630         CHAIN DEPT dhrec;
004600110630         //
004700110630         if nempflag = 'Y';
004800110630         *in06 =  *on;
004900110630         else;
005000110630         *in06 =  *off;
005100110630         endif;
005200110630          //
005300110630         *IN50 = *OFF;
005301110630      /END-FREE
005400950725     C                   Z-ADD     0             TOTBON
005500950725     C                   Z-ADD     0             FULPER
005600950725     C                   Z-ADD     0             COEFUL
005601110630      /FREE
005602110705              lateper = 0;
005700110630         status = get_emp_info(emp:dept:*blanks:
005800110630           name#:class);
005900110630        if status <> 'G';
006000110630         name = name#;
006100110630         //
006101110630      /END-FREE
006200011123     C                   ADD       1             COUNT             3 0
006300950727     C     *YEAR         SUB       1             YEAR              4 0
006400001129     C                   Z-ADD     GCOE          TCOE              9 2
006500001129     C                   Z-ADD     LETBAS        TAX25             9 2
006600001129     C                   Z-ADD     LETCOE        NTX25             9 2
006700950727     C                   Z-ADD     AWARD         MANAWR            3 1
006701110630      /FREE
006800110630         AWARD1 = award / 100;
006900110630         //***********************
007000110630         // BASIC BONUS ROUTINE **
007100110630         //***********************
007101110630      /END-FREE
007200011123     C  N06              eval      fulper = gros * aaward / 100                 7 % FOR ATTEN.
007201110630      /FREE
007300110630         fstnet = gros * fstper / 100;                                        //7 % FOR ATTEN.
007400110630         secnet = gros * secper / 100;                                        //7 % FOR ATTEN.
007500110630         thrnet = gros * awolpr / 100;                                        //AWOL
007501110630      /END-FREE
007600001129     C     GROS          MULT      AWARD1        MANAMT            9 2          MAN. AWARD
007700110630      //******************
007800110630      // C.O.E. ROUTINE **
007900110630      //******************
008000011123     C  N06              eval      coeful = tcoe * aaward / 100                 7 % FOR ATTEN.
008001110630      /FREE
008100110630         coefst = tcoe * fstper / 100;                                        //7 % FOR ATTEN.
008200110630         coesec = tcoe * secper / 100;                                        //7 % FOR ATTEN.
008300110630         coethr = tcoe * awolpr / 100;                                        //AWOL
008301110630      /END-FREE
008400001129     C     TCOE          MULT      AWARD1        COEAWA            9 2          COE AWARD
008500110630      //
008600110630      // TOTALS ACCUMULATION
008700110630      //
008800001129     C     FSTNET        ADD       SECNET        TPENAL            9 2          AWARDS & PENAL.
008900001129     C     THRNET        ADD       TPAWOL        TPAWOL            9 2
009000001129     C     COEFST        ADD       COESEC        NPENAL            9 2
009100001129     C     COETHR        ADD       NPAWOL        NPAWOL            9 2
009200001129     C     TPENAL        ADD       NPENAL        PENAL             9 2
009300001129     C     TPAWOL        ADD       NPAWOL        PAWOL             9 2
009400001129     C     FULPER        ADD       COEFUL        TRAWA             9 2
009500001129     C     MANAMT        ADD       COEAWA        TAWARD            9 2
009600001129     C     GROS          ADD       FULPER        GTAX              9 2
009700950727     C     MANAMT        ADD       GTAX          GTAX
009800001129     C     TCOE          ADD       COEFUL        GNTAX             9 2
009900950727     C     COEAWA        ADD       GNTAX         GNTAX
010000001129     C     TPENAL        ADD       TPAWOL        GTPEN             9 2
010100001129     C     NPENAL        ADD       NPAWOL        GNPEN             9 2
010200001129     C     PENAL         ADD       PAWOL         GPEN              9 2
010300001129     C     GTAX          SUB       GTPEN         NTAX              9 2
010400111201     C     GNTAX         SUB       GNPEN         NNTAX
010500001129     C     GROS          ADD       TCOE          TOTBON            9 2
010600001129     C     TOTBON        ADD       TRAWA         GTOT              9 2
010700950727     C     TAWARD        ADD       GTOT          GTOT
010800001129     C     GTOT          SUB       GPEN          NTOT              9 2
011000011123     C                   z-add     ntot          NPTOT             9 2
011100011123
011101110630      /FREE
011102111201         npntax = nntax;
011200110630         TABS = awl + awol + jury
011300110630             + susp + sick + indacc;
011400110630         *in50 = tabs > *zeros;
011404110907          tper = fstper + secper + latepr;
011405110907            latepr = 0;
011406110630      /END-FREE
011500110907     C**   FSTPER        ADD       SECPER        TPER              4 1
011600950727     C     TPER          ADD       AWOLPR        FSTPER            4 1
011700110630      //
011701110630      /FREE
011800110630         if fstper >= 75
011900110630               and nempflag <> 'Y';
012000110630           ntax = tax25;
012100110630           npntax = ntx25;
012200110630           nptot = ntax + npntax;
012300110630         endif;
012400110907                 //sply latepr;
012500110630         // Print employee letter
012600110630         exsr print_letter;
012700110630         //
012800110630       endif;
012900011123
013000110630       //*****************************
013100110630       // Iniz subroutine           **
013200110630       //*****************************
013300110630       begsr *inzsr;
013400110630         in *lock *dtaara;
013401110630      /END-FREE
013500011122     C     *DTAARA       define    xmsseqchk     xmseq#            2 0
013600110630      //
013601110630      /FREE
013700110630         if xmseq#  <> 4;
013701110630      /END-FREE
013800011122     C                   call      'ESCPMSG'
013900011122     C                   parm                    @pgm
014000011122     C                   parm      'DIH0131'     msgid             7
014100011122     C                   parm      'QGPL'        @lib             10
014200011122     C                   parm      'DIHMSGF'     @FILE            10
014201110630      /FREE
014300110630         endif;
014400110630         //
014401110630           Xmseq# = 5;
014600110630         out xmseq#;
014700110630       endsr;
014701110907        //** First initilisation routine for all programs  **
014702110907         //***************************************************
014703110907             begsr aainit;
014704110907                     CONAME = get_coname;
014705110907                  //@DATE = get_sys_date(#date:#time);
014706110907                  //@title = arr(1);
014707110907
014708110907                 // get_prd_info('*prd':*blanks:prno
014709110907                //                               :#date);
014710110907                 // userid = @user;
014711110907
014712110907              endsr;
014800110630       //*****************************
014900110630       // Print Letter Routine      **
015000110630       //*****************************
015100110630       begsr print_letter;
015200011123
015300110630         // name info. gross bonus etc.
015400110630         except header;
015500110630         // special text for sales personnel
015600110630         if dtype = 'S' or dtype = 'B'
015700110630               or dept = 52;
015800110630           except saldetl;
015900110630         endif;
016000110630         //
016100110630         except grsdetl;
016200110630         // Text for New employees Vs. Old employees
016300110630         if nempflag = 'Y';
016400110630           if aaward > *zeros;
016500110630             except nempdetl;
016600110630           endif;
016700110630         else;
016800110630           except oempdetl;
016900110630         endif;
017000110630         except detail;
017100110630         // Text for employees with awol records
017200110630         if awolpr > *zeros;
017300110630           except awoldetl;
017400110630         endif;
017500110630         // summaries etc.
017501110705            exec sql
017502110705              select abtpen1 into :lateper from abstypl01
017503110705                where abttype = :latetype;
017600110630         except totals;
017700110630       endsr;
017800011123
017801110630      /END-FREE
017900011123     OREPORT    e            header         1 01
018000950921     O                       @PGM                10
018100110907     O                       coname              75
018200011123     O          e            header         1
018300950620     O                                           71 '================='
018400011123     O          e            header      0  1
018500950620     O                                           68 'D"AGUIAR"S   INDUSTRIES'
018600950620     O                                           81 '&  HOLDINGS'
018700011123     O          e            header      0  1
018800950620     O                                           69 '========================'
018900950620     O                                           81 '============'
019000011123     O          e            header         1
019100950620     O                                           68 'XMAS BONUS AWARD --'
019200950620     O                       UDATE         Y     77
019300011123     O          e            header         1
019400950620     O                                           73 '========================'
019500950620     O                                           77 '===='
019600011123     O          e            header         2
019700011123     O                       name                26
019800011123     O                       emp           Z     36
019900950620     O                                           37 '-'
020000011123     O                       dept          Z     40
020100011123     O          e            header         1
020200950620     O                                           16 'FELLOW EMPLOYEE,'
020300011123     O          e            header         1
020400950620     O                                           26 'ATTACHED HERETO IS YOUR '
020500950620     O                                           50 'XMAS BONUS PAYSLIP. THE '
020600950620     O                                           75 'XMAS BONUS FOR THIS YEAR'
020700950620     O                                           99 ' HAS BEEN FIXED ACCORDIN'
020800950620     O                                          123 'G TO YOUR GRADE. AS A GR'
020900950620     O                                          126 'ADE'
021000011123     O                       class          B   129
021100011123     O          e            header         2
021200950620     O                                           24 'EMPLOYEE,YOU ARE ENTITLE'
021300001129     O                                           29 'D TO '
021400001129     O                       TOTBON        3     42 '$'
021500001129     O                                           51 'OF WHICH'
021600001129     O                       TCOE          3     62 '$'
021700001129     O                                           68 'IS TA'
021800001129     O                                           92 'X FREE. PENALTIES OR AWA'
021900001129     O                                          116 'RDS ARE THEN SUBTRACTED '
022000001129     O                                          125 'OR ADDED.'
022100011123     O          e            saldetl        1
022200950620     O                                           31 'N.B:- XMAS BONUS FOR SAL'
022300950620     O                                           55 'ESMEN & SALES MANAGERS H'
022400950620     O                                           79 'AS BEEN CALCULATED AT 80'
022500950620     O                                           90 '% OF GROSS.'
022600011123     O          e            saldetl        2
022700950620     O                                           31 '========================'
022800950620     O                                           55 '========================'
022900950620     O                                           79 '========================'
023000950620     O                                           90 '==========='
023100011123     O          e            grsdetl        1
023200950620     O                                           24 'THE CALCULATIONS ARE AS '
023300950620     O                                           37 'SHOWN BELOW:-'
023400091130     O                                          118 'XMAS BONUS      XMAS COE'
023500950620     O                                          132 'TOTAL'
023600011123     O          e            grsdetl        1
023700091130     O                                          118 '----------    ----------'
023800950620     O                                          132 '-----'
023900011123     O          e            grsdetl        2
024000950620     O                                           20 'GROSS AMOUNT PAYABLE'
024100950620     O                                           80 '--'
024200001129     O                       GROS          3B   103 '$'
024300001129     O                       TCOE          3B   117 '$'
024400001129     O                       TOTBON        3    132 '$'
024500011123     O          e            oempdetl       2
024600950620     O                                           13 'PLUS AWARD OF'
024700011123     O                       aaward        1B    17
024800940516     O                                           24 ' % FOR '
024900950620     O                                           48 'ABSENCE OF NOT MORE THAN'
025000950620     O                                           55 ' 3 DAYS'
025100950620     O                                           92 '--           +'
025200001129     O                       FULPER        3    103
025300001129     O                       COEFUL        3    117
025400001129     O                       TRAWA         3B   132
025500950620     O                                          107 '+'
025600950620     O                                          121 '+'
025700011123     O          e            nempdetl       2
025800950620     O                                            8 'AWARD OF'
025900011123     O                       aaward        1B    12
026000940516     O                                           36 ' % FOR ABSENCE OF 3 DAYS'
026100950620     O                                           60 ' OR LESS IS ADDED TO YOU'
026200950620     O                                           80 'R GROSS AMOUNT    --'
026300011123     O          e            detail         1
026400950620     O                                           24 'PLUS MANAGERIAL AWARD OF'
026500950620     O                       MANAWR        1B    28
026600950620     O                                           52 '% FOR EXCEPTIONAL WORK O'
026700950620     O                                           70 'R GOOD PERFORMANCE'
026800950620     O                                           92 '--           +'
026900001129     O                       MANAMT        3B   103
027000001129     O                       COEAWA        3B   117
027100001129     O                       TAWARD        3B   132
027200950620     O                                          107 '+'
027300950620     O                                          121 '+'
027400011123     O          e            detail         1
027500950620     O                                          114 '------------------------'
027600950620     O                                          132 '------------------'
027700011123     O          e            detail         1
027800011123     O                                           80 'GROSS BONUS        --'
027900950620     O                                           92 '+'
028000950620     O                                          107 '+'
028100950620     O                                          121 '+'
028200001129     O                       GTAX          3B   103
028300001129     O                       GNTAX         3B   117
028400001129     O                       GTOT          3B   132
028500011123     O          e            detail         1
028600950620     O                                          114 '------------------------'
028700950620     O                                          132 '------------------'
028800011123     O          e            detail         1
028900950620     O                                           16 'LESS PENALTIES:-'
029000011123     O          e            detail         1
029100950620     O                                           14 '--------------'
029200011123     O          e            detail         1
029300950620     O                                           24 'BASED ON THE TABLE SHOWN'
029400950620     O                                           48 ' FOR PENALTIES BELOW YOU'
029500950620     O                     50                    64 ' WERE ABSENT FOR'
029600111205     O                     50TABS          1     70
029700950620     O                     50                    75 'DAYS'
029800950620     O                    N50                    72 ' WERE NOT ABSENT DURING '
029900950620     O                    N50                    81 'THE YEAR.'
030000011123     O          e          50detail         2
030100950620     O                                           24 'FOR WHICH YOU HAVE BEEN '
030200950620     O                                           33 'PENALISED'
030300950620     O                       TPER          1B    39
030400950620     O                                           64 '% OF YOUR GROSS BONUS OF'
030500001129     O                       TOTBON        3     75 '$'
030600950620     O                                           92 '.  --           -'
030700950620     O                                          107 '-'
030800950620     O                                          121 '-'
030900001129     O                       TPENAL        3B   103
031000001129     O                       NPENAL        3B   117
031100001129     O                       PENAL         3B   132
031200011123     O          e            awoldetl       1
031300950620     O                                           11 'IN ADDITION'
031400950620     O                       AWOL          1     16
031500950620     O                                           23 'OF THE'
031600111205     O                       TABS          1b    29
031700950620     O                                           54 'DAYS WERE AWOL FOR WHICH'
031800950620     O                                           70 ' YOU HAVE LOST A'
031900011123     O          e            awoldetl       1
032000950620     O                                            7 'FURTHER'
032100950727     O                       AWOLPR        1     12
032200950620     O                                           37 '% OF THE GROSS AMOUNT OF'
032300001129     O                       TOTBON        3     48 '$'
032400950620     O                                           49 '.'
032500950620     O                                           92 '--           -'
032600950620     O                                          107 '-'
032700950620     O                                          121 '-'
032800001129     O                       TPAWOL        3B   103
032900001129     O                       NPAWOL        3B   117
033000001129     O                       PAWOL         3B   132
033100011123     O          e            totals         1
033200950620     O                                          114 '------------------------'
033300950620     O                                          132 '------------------'
033400011123     O          e            totals         1
033500950620     O                                           79 'TOTAL PENALTIES        -'
033600950620     O                                           92 '-           -'
033700950620     O                                          107 '-'
033800950620     O                                          121 '-'
033900001129     O                       GTPEN         3B   103
034000001129     O                       GNPEN         3B   117
034100001129     O                       GPEN          3B   132
034200011123     O          e            totals         1
034300950620     O                                          114 '------------------------'
034400950620     O                                          132 '------------------'
034500011123     O          e            totals         1
034600950620     O                                           80 'NET BONUS        --'
034700001129     O                       NTAX          3    103 '$'
034800111201     O                       NNTAX         lB   117 '$'
034900111201     O                       NTOT          lB   132 '$'
035000011123     O          e            totals         1
035100950620     O                                          114 '------------------------'
035200950620     O                                          132 '------------------'
035300011123     O          e            totals         1
035400950620     O                                           44 'THE NET BONUS PAYABLE AC'
035500950620     O                                           68 'CORDING TO YOUR PAYSLIP '
035600950620     O                                           90 'IS        --       ***'
035700001129     O                       NTAX          3B   103 '$'
035800001129     O                       NPNTAX        3B   117 '$'
035900001129     O                       NPTOT         3B   132 '$'
036000011123     O          e            totals         2
036100950620     O                                          114 '------------------------'
036200950620     O                                          132 '------------------'
036300011123     O          e            totals         2
036400950620     O                                           24 'THE TABLE FOR PENALTIES '
036500950620     O                                           39 'IS AS FOLLOWS:-'
036600011123     O          e            totals         1
036700950620     O                                           24 '(A) FOR ABSENCE MORE THA'
036800950620     O                                           32 'N 3 DAYS'
036900011123     O          e            totals         1
037000950620     O                                           28 'ABSENCE FROM THE 4TH DAY'
037100021125     O                                           52 ' TO THE 10TH DAY,LESS 4 '
037200021125     O                                           76 '% FOR EACH DAY OR 28 % F'
037300051201     O                                           87 'OR 10 DAYS.'
037400110630      //                                         124 ' TOTAL PENALTIES EXCEED '
037500110630      //                                         132 '100 %   *'
037600110630      //         e            totals         1
037700110630      //                                         119 '*  OF YOUR GROSS TAXABLE'
037800110630      //                                         132 ' PAY,THE    *'
037900011123     O          e            totals         1
038000950620     O                                           24 '(B) ABSENCE ABOVE 10 DAY'
038100950620     O                                           25 'S'
038200051201     O                                          119 '************************'
038300051201     O                                          132 '*************'
038400051201     O          e            totals         1
038500051201     O                                           96 '*'
038600051201     O                                          132 '*'
038700110630      //                                         119 '*  DIFFERENCE IS SUBTRAC'
038800110630      //                                         132 'TED FROM    *'
038900110630      //         e            totals         1
039000110630      //                                         116 '*  YOUR TAX FREE PAY.'
039100110630      //                                         132 '*'
039200011123     O          e            totals         1
039300021125     O                                           28 'LESS 28 % FOR THE FIRST '
039400950620     O                                           52 '10 DAYS AND 5 % FOR EACH'
039500950620     O                                           68 ' ADDITIONAL DAY.'
039600051201     O                                          121 '*  PLEASE NOTE THAT CHRIST' +
039700051201     O                                              'MAS'
039800051201     O                                          132 '*'
039900051201     O          e            totals         1
040000051201     O                                          120 '*  BONUS IS NOW PENALISED'
040100051201     O                                          132 '*'
040200051201     O          e            totals         1
040300111201     O                                          120 '*  UP TO 75%.           '
040400051201     O                                          132 '*'
040500011123     O          e            totals         1
040501110711     O                                           24 '(C) ABSENCE WITHOUT LEAV'
040502110711     O                                           25 'E'
040800950620     O                                           96 '*'
040900950620     O                                          132 '*'
041000011123     O          e            totals         1
041001110711     O                                           28 'ADDITIONAL PENALTIES OF '
041002110711     O                                           45 '4 % FOR EACH DAY.'
041100950620     O                                           96 '*'
041200110705     O*                                         100 'IF'
041300110705     O*                                         124 ' THERE IS ANY DISAGREEME'
041400110705     O*                                         126 'NT'
041500950620     O                                          132 '*'
041501110705     O          e            totals         1
041506110705     O                                           96 '*'
041507110705     O                                          132 '*'
041508110705     O          e            totals         1
041509110711     O                                           24 '(D) FOR LATENESS MORE TH'
041510110711     O                                           34 'AN 10 DAYS'
041511110705     O                                           96 '*'
041512110705     O                                          100 'IF'
041513110705     O                                          124 ' THERE IS ANY DISAGREEME'
041514110705     O                                          126 'NT'
041515110705     O                                          132 '*'
041600011123     O          e            totals         1
041601110711     O                                            9 'LESS '
041602110711     O                       lateper       1     14
041603110907     O                                           34 '% FOR EACH DAY AFTER'
041900061113     O                                           96 '*'
042000061113     O                                          122 'WITH YOUR ATTENDANCE REC'
042100061113     O                                          127 'ORD, '
042200061113     O                                          132 '*'
042300011123     O          e            totals         1
042400950620     O                                          119 '*  KINDLY SEE THE HEAD O'
042500950620     O                                          132 'F YOUR      *'
042600011123     O          e            totals         1
042700950620     O                                           24 'ACCORDING TO OUR FILES, '
042800950620     O                                           48 'YOUR ATTENDANCE RECORD F'
042900950620     O                                           72 'OR THE YEAR FROM 1ST.NOV'
043000950727     O                                           73 '.'
043100950620     O                       YEAR                77
043200950727     O                                           90 'TO 31ST.OCT.'
043300950727     O                       *YEAR               94
043400950620     O                                          109 '*  DEPARTMENT.'
043500950620     O                                          132 '*'
043600011123     O          e            totals         1
043700950620     O                                           16 'WAS AS FOLLOWS:-'
043800950620     O                                           96 '*'
043900950620     O                                          132 '*'
044000011123     O          e            totals         2
044100950620     O                                          119 '************************'
044200950620     O                                          132 '*************'
044300011123     O          e            totals         1
044400950620     O                                           24 'X-----------------------'
044500950620     O                                           48 '----------DAYS----------'
044600110705     O                                           72 '-----------------------'
044601110705     O                                           78 '------X'
044700011123     O          e            totals         1
044800110711     O                                           24 ' LATE SICK LEAVE  A.W.L '
044801110711     O                                           48 ' A.W.O.L  JURY SERV.  IN'
044803110711     O                                           61 'D.ACC.  SUSP.'
044804110711     O*                                          24 'SICK LEAVE  A.W.L  A.W.O'
044901110711     O*                                          48 '.L   JURY SERV. IND.ACC.'
045000110711     O*                                          61 '  LATE  SUSP.'
045001110711     O                                           78 ' TOTAL PENALTIES'
045100011123     O          e            totals         1
045200110711     O                       LATE          2B     5
045201110711     O                       SICK          2B    13
045300110711     O                       AWL           2B    22
045400110711     O                       AWOL          2B    31
045500110711     O                       JURY          2B    43
045600110711     O                       INDACC        2B    53
045701110711     O                       SUSP          2B    60
045800110711     O                       FSTPER        2B    78
045900011123     O          e            totals         1
046000061113     O                                          115 'T.I. BYNOE'
046100011123     O          e            totals
046200061113     O                                          120 'SECRETARY/M.I.S. EXECUTI'
046300061113     O                                          122 'VE'
