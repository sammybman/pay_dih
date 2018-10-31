000100950412     H DATEDIT(*DMY)
000200091121     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300111013     H/define profoundui
000400990208     F*
000500990208     **  SUBFILE SELECTION DISPLAY FILE
000600020219     Femp005d   cf   e             workstn
000700020219     F                                     sfile(sflrec:rrn)
000800111013     F/IF DEFINED(WEB)
000900111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001000111013     F/ENDIF
001100111013     Fliaba_ytd if   e           K disk    usropn
001200050530     FQsysprt   o    f  132        printer oflind(*inof) usropn
001300020206     ** General/Payroll PROTOTYPES
001400111013      /copy QCPYSRC,genPR
001500111013      /copy QCPYSRC,PAYPR
001600111013      /copy QCPYSRC,autPR
001700111013      /copy QCPYSRC,empPR
001800990212     **
001900050530     D arr             S             40    dim(1) CTDATA PERRCD(1)
002000050530     D dedu            S             11  2
002100050530     D prebal          S             11  2
002200050530     D totpre          S             11  2
002300050530     D totbal          S             11  2
002400050602     D totded          S             11  2
002500050530     D desc#           S             20
002600050530     D prno            S              6  0
002700050530     D prn             S              6  0
002800021005     D date1           S              8  0
002900050514     D countr          S              5  0
003000050530     D cntr            s              5  0                                      TOTAL AMOUNT
003100050530     D rrn             s              4  0                                      TOTAL AMOUNT
003200050530     D curbal          s             11  2                                      TOTAL AMOUNT
003300050530     D class#          s              3                                         TOTAL AMOUNT
003400050530     D lttyp##         s              2  0                                      TOTAL AMOUNT
003500050602     D payrun_factor   s              7  3 inz(1)
003600050602     D payrun_qty      s              2  0 inz(1)
003700050602     D hrs_dummy       s              5  2
003800050602     D pay_amt         s             11  2
003900111013     D errmsg          s             75
004000050602
004100050602     D YEAR_CONST      c                   12
004200990216     **
004300950930     D                SDS
004400950930     D  @PGM             *PROC
004500111013     D  @USER                358    367
004600050530
004700040207      /free
004800111013         // authority check
004900111013         monitor;
005000111013           authority_check('EMT':@pgm);
005100111013         on-error;
005200111013           show_msg(get_err_msg('DIH0054'));
005300111013           *inlr = *on;
005400111013           return;
005500111013         endmon;
005600050530
005700050530          dedu = *zeros;
005800050530
005900050530          exsr  aainit;
006000050530          exsr  subfsubr;
006100111013
006200111013         //*****************************
006300111013         // Session Init
006400111013         //*****************************
006500111013
006600111013          begsr *inzsr;
006700111013            get_session_info('EMT':errmsg:@pgm);
006800111013            if errmsg <> *blanks;
006900111013              show_msg(errmsg);
007000111013              *inlr = *on;
007100111013              return;
007200111013            endif;
007300111013
007400111013            open(e) liaba_ytd;
007500111013
007600111013          endsr;
007700111013
007800050530
007900050530        //****************************************************
008000050530        //*** subfile subroutine - SELECT DEDUCTION TYPE   ***
008100050530        //****************************************************
008200050530        begsr  subfsubr;
008300050530
008400050530           rrn = *zeros;
008500050530           reset sflrec;
008600050530           *in33 = *on;
008700050530           write  sflctl;
008800050530           *in33 = *off;
008900050530
009000050514          *in55 = *off;
009100050514          countr = *zeros;
009200050514          dow not get_ltype_info('RD':-1:ematf_struc:countr);
009300050530
009400050530            if  emautpyd = 'Y';
009500050530              emseltyp# = emseltyp;
009600050530              emdsp# = emdsp;
009700050530              emacct# = emacct;
009800050530
009900050530              rrn += 1;
010000050530              write  Sflrec;
010100050530            endif;
010200050530
010300050530          Enddo;
010400050530
010500050514          *in55 = *on;
010600050530
010700050530
010800050530          exfmt  sflctl;
010900050530
011000050530          if  *in03;
011100050530            *inlr  = *on;
011200050530            Return;
011300050530          Endif;
011400050530
011500050530          // CHECK ANY SELECTION MADE
011600050530
011700050530          readc  sflrec;
011800050530          dow not %eof;
011900050530
012000050530            if  sel# = 'X';
012100050530               lttyp## = emseltyp#;
012200050530              // open report files
012300050530              open  qsysprt;
012400050530
012500050530              exsr  gen_ded;
012600990216
012700050530             // close report files after transactions are processed
012800050530              close  qsysprt;
012900050530
013000050530            endif;
013100050530
013200050530           readc  sflrec;
013300050530          enddo;
013400050530        endsr;
013500050530
013600050530
013700050530        //***************************
013800050530        //** Initialize subroutine
013900050530        //***************************
014000050530         begsr  AAINIT;
014100050530
014200050530           coname = get_coname;
014300050530           @date = get_sys_date(date1);
014400050530          // GET CURRENT PERIOD
014500050530           get_prd_info('*prd':*blanks:prno:date1);
014600050530           prn = get_new_prd(prno:1:'S');
014700050530           @title = arr(1);
014800050530
014900050530         Endsr;
015000050530
015100050530
015200050530         begsr  prt_header;
015300050530
015400050530             except  hedr1;
015500050530             except  typ_hedr;
015600050530
015700050530         Endsr;
015800990216
015900050530        //*******************************************                       **
016000050530        //*** GENERATE PAYROLL DEDUCTIONS & REPORTS                         **
016100050530        //*******************************************                       **
016200050530        begsr  gen_ded;
016300050530
016400050530          cntr = *zeros;
016500050530          exsr prt_header;
016600050530          dow not get_emp_info2('R':-1:emp_struc:cntr);
016700050530
016800050530             if   get_Cat(status:desc#) = 'A';
016900050530
017000050530                class# = %xlate('P':' ':class);
017100050530                get_emplimbal(*zeros:lttyp##:
017200050530                        *zeros:emp:*zeros:prno:curbal);
017300050530
017400050530                get_emplimbal(*zeros:lttyp##:
017500050530                        *zeros:emp:*zeros:prn:prebal);
017600990710
017700050530                if  prebal <> *zeros
017800050530                  or  curbal <> *zeros;
017900050530                 // Cater for over flow
018000050530                   if   *inof;
018100050530                       exsr  prt_header;
018200050530                   endif;
018300990222
018400050601                   if  curbal > *zeros;
018500050530
018600050530                    // Perform  deduction checks
018700050530
018800050602                   // Always deduct (if class record not found)
018900050530                  if  get_paded_info('RD':lttyp##:*zeros:
019000050530                               class#:paded_struc);
019100050530                      dedu = curbal;
019200050530                    else;
019300050530                      chain (lttyp##:*zeros:emp) lafmt;
019400050530                      if not %found;
019500050530                         laamt = *zeros;
019600050530                      endif;
019700050530
019800050530                      select;
019900050602
020000050602                      // Above yearly balance
020100050531                        when lrul3ded = 1;
020200050602                           pay_amt = (get_emp_pay(emp:dept:empaytype:
020300050602                                       hrs_dummy:payrun_factor:
020400050602                                       payrun_qty:status));
020500050602
020600050602                           if curbal > pay_amt;
020700050602                              dedu = curbal - pay_amt;
020800050602                           endif;
020900050602
021000050602                      // Do not deduct
021100050531                        when lrul3ded = 2;
021200050530                           dedu = *zeros;
021300050602
021400050602                      // Above Monthly allowance
021500050531                        when lrul3ded = 3;
021600050602                           pay_amt = (get_emp_pay(emp:dept:empaytype:
021700050602                                       hrs_dummy:payrun_factor:
021800050602                                       payrun_qty:status)) /
021900050602                                        YEAR_CONST;
022000050602                           if curbal > pay_amt;
022100050602                              dedu = curbal - pay_amt;
022200050530                           endif;
022300050602
022400050602                      // Above Year-to-date allowance
022500050531                        when lrul3ded = 4;
022600050531                           if curbal > laamt;
022700050531                              dedu = curbal - laamt;
022800050531                           endif;
022900050530                        other;
023000050530                      endsl;
023100050530
023200050530                  endif;
023300050530
023400050602                    totpre +=  prebal;
023500050602                    totbal += curbal;
023600050602                    totded += dedu;
023700050530                    // Print Detail record
023800050602                    except   detail;
023900050530
024000050530                   endif;
024100050530                endif;
024200050530
024300050530              dedu = *zeros;
024400050530
024500050530            endif;
024600050530
024700050530          enddo;
024800050530
024900050530          except  final;
025000050530        endsr;
025100050530
025200050530      /end-free
025300050530
025400050530     Oqsysprt   e            hedr1          2 04
025500950930     O                       @PGM                10
025600020206     O                       coname              85
025700990212     O          e            hedr1          2
025800950930     O                       @USER               10
025900050531
026000990213     O          e            typ_hedr       2
026100050531     O                       emdsp#              30
026200050602     O                                           78 'DEDUCTIONS REPORT AS AT '
026300050602     O                       UDATE         Y     86
026400990212     O          e            typ_hedr       2
026500950412     O                                           17 'EMP. NO'
026600950412     O                                           30 'DEPT NO.'
026700950412     O                                           48 'NAME'
026800950412     O                                           72 'PREV.'
026900050602     O                                           88 'BALANCE'
027000050602     O                                          105 'DEDUCTION'
027100050602     O                                          116 'STATUS'
027200050530     O          E            detail         1
027300050530     O                       EMP           Z     16
027400950412     O                       DEPT          Z     27
027500950412     O                       CHRIST              43
027600950412     O                       SURNAM              59
027700050530     O                       prebal        AB    74
027800050602     O                       curbal        A     90
027900050602     O                       dedu          A    107
028000050602     O                       desc#              130
028100050530
028200050530     O          E            final       2
028300990216     O                       totpre        A     74
028400050602     O                       totbal        A     90
028500050602     O                       totded        A    107
028600950412     O                                          126 '****'
028700990209**
028800990209          Employees Charges
