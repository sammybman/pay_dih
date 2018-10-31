000100010904     H DATEDIT(*ymd)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300111002     H/define profoundui
000400170215      // Employee PAYROLL ANALYSIS
000500111002     Fpayrtyp   IF   E           K DISK    usropn
000600111002     Fpayhisl07 IF   E           K DISK    usropn
000700111002     Fdeptl06   IF   E           K DISK    usropn
000800111002     Fpaydiv    IF   E           K DISK    usropn
000900010904     FQSYSPRT   O    F  132        PRINTER
001000010904     Fpayrpt01d cf   e             workstn
001100111002     F/IF DEFINED(WEB)
001200111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001300111002     F/ENDIF
001400110803      /copy QCPYSRC,genpr
001500110803      /copy QCPYSRC,autpr
001600050227
001700170215      //--------------------------------------
001800170215      // Prototype for procedure: Sum Division
001900170215      //--------------------------------------
002000050227     D SumDivision     PR            11  2
002100050227
002200950530     D                SDS
002300950530     D  @PGM             *PROC
002400111002     D  @USER                358    367
002500980123     D
002600010902
002700010902     Damts             S             11  2
002800010902     Damts#            S             11  2
002900050227     DEmpAmt           S             11  2
003000010903     Dfin_amts         S             13  2
003100010902     Dcnt              S              5  0
003200010904     Dsav_cnt          S                   like(cnt)
003300010902     Dtot_cnt          S              7  0
003400010903     Dfin_tot_cnt      S              7  0
003500050227     Dpydate           s               D   datfmt(*dmy)
003600050322     Ddmydate          s              6  0
003700050322     Demp_prv          s              6  0
003800050322     DamtNull          s              2b 0
003900010902
004000010902     Drun_desc         S                   like(prtydesc)
004100010902     Dlname            S                   like(dname)
004200010902     Dlastdp           S                   like(phisdept)
004300010902
004400170215      // system fields for report
004500010902     D@date            S             10
004600010904     D@title_rep       S             60
004700010902     Dconame           S             30
004800010904     D arr             S             40    DIM(2) CTDATA PERRCD(1)
004900010904     Ddate_err         c                   'Invalid Date Entered'
005000010904     Dtype_err         c                   'Invalid Payroll Type Entered'
005100010902
005200040207      /free
005300111002         // authority check
005400111002         monitor;
005500111002           authority_check('PAY':@pgm);
005600111002         on-error;
005700111002           show_msg(get_err_msg('DIH0054'));
005800111002           *inlr = *on;
005900111002           return;
006000111002         endmon;
006100040207      /end-free
006200170215      // get start up data
006201170215      /FREE
006300170215       exsr aainit;
006400170215       //**************************************************************
006500170215       //*         Prepare Report Criteria                          ***
006600170215       //**************************************************************
006700010902
006800170215       dow 0 = 0;
006900170215         exfmt fmt1;
007000170215         exsr aainit;
007100170215         clear errmsg;
007101170215      /END-FREE
007200010904     C                   movea     '00'          *in(25)
007300170215      // F3 = End Pgm
007301170215      /FREE
007400170215         if *in03;
007500170215           *inlr = '1';
007600170215           return;
007700170215         endif;
007800010904
007900170215         if *in01;
007901170215      /END-FREE
008000010904     C                   call      'PAYENQ02R'
008100010904     C                   parm                    run_type
008101170215      /FREE
008200170215           iter;
008300170215         endif;
008400170215         // validating date field
008500170215         test(dE) run_date;
008501170215         *IN25 = %ERROR;
008600170215         if *in25;
008700170215           errmsg = date_err;
008800170215           iter;
008900170215         endif;
009000010904
009100170215         // validating payroll type
009200170215         chain run_type prtyfmt;
009300170215         if not %found;
009400170215           *in26 = '1';
009500170215           errmsg = type_err;
009600170215           iter;
009700170215         endif;
009800050227
009900170215         leave;
010000170215       enddo;
010100170215       //
010200170215       //**************************************************************
010300170215       //*         PRINT REPORT REQUESTED                           ***
010400170215       //**************************************************************
010500170215       //
010600170215       //
010700170215       setll *loval divfmt;
010800170215       read divfmt;
010900170215       Except Header;
011000170215       dow not (%eof(paydiv));
011100170215         //****************************************************************
011200170215         //* THIS ROUTINE IS NOT FOR EXECUTIVE STAFF ***
011300170215         //****************************************************************
011400010903
011500170215         setll *loval dhrec;
011600170215         read dhrec;
011700170215         dow not (%eof(deptl06));
011800170215           lname  =dname;
011901170215
012100050227         setll (run_date:dept:run_type) phisfmt;
012200050227         reade (run_date:dept:run_type) phisfmt;
012300050227         cnt = 0;
012400050322         emp_prv = phisemp;
012500050227         dow  not (%eof(payhisl07));
012600010904
012700050227          // Print first division only when there is a rec
012800050227          EmpAmt = Sumdivision();
012900050227          if  EmpAmt > *zeros;
013000010904
013100050227             cnt += 1;
013200050227             sav_cnt += 1;
013300050227             if  sav_cnt = 1;
013400050227                except  div_hdr;
013500050227                except  dpt_hdr;
013600050227             endif;
013700050227
013800050227             amts += Empamt;
013900050227             fin_amts += Empamt;
014000050227             amts# += Empamt;
014100050227          endif;
014200050227
014300050322         // ignore duplicates from history file read
014400050322         dow 0 = 0;
014500050227         reade (run_date:dept:run_type) phisfmt;
014600050322         if phisemp = emp_prv and
014700050322            not %eof;
014800050322            iter;
014900050322          else;
015000050322           emp_prv = phisemp;
015100050322           leave;
015200050322         endif;
015300050322         enddo;
015400050322
015500050227         enddo;
015600050227      /end-free
015700010902
015701170215      /FREE
015800170215           if cnt > 0;
015900170215             except detail;
016000170215             tot_cnt +=  cnt;
016100170215             fin_tot_cnt +=  cnt;
016200170215           endif;
016300010902
016400170215           read dhrec;
016500170215         enddo;
016600010902
016700170215         Read divfmt;
016800170215         if sav_cnt > 0;
016900170215           except det;
017000170215           except header;
017100170215           sav_cnt = 0;
017200170215         endif;
017300010903
017400010903
017500010903
017600170215         //****************************************************************
017700170215       Enddo;
017800010902
017900170215       // PRINT LAST GRAND TOTAL
018000170215       EXCEPT TOTAL;
018100170215       *inlr = '1';
018200170215       return;
018201170215      /END-FREE
018300111002      /free
018400111002
018500111002         //*****************************
018600111002         // Session Init
018700111002         //*****************************
018800111002
018900111002          begsr *inzsr;
019000111006            get_session_info('PAY':errmsg:@pgm);
019100111002            if errmsg <> *blanks;
019200111002              show_msg(errmsg);
019300111002              *inlr = *on;
019400111002              return;
019500111002            endif;
019600111002
019700111002            open(e) payrtyp;
019800111002            open(e) payhisl07;
019900111002            open(e) deptl06;
020000111002            open(e) paydiv;
020100111002
020200111002          endsr;
020300111002
020400111002      /end-free
020500170215      //*************************************************
020600170215      // First initilisation routine for all programs  **
020700170215      //*************************************************
020701170215      /FREE
020800170215       BEGSR AAINIT;
020900170215         coname = get_coname;
021000170215         @date = get_sys_date;
021100170215         chain run_type prtyfmt;
021200170215         // select header for program
021300170215         @title = ARR(2);
021400170215         @title_rep = %trim(ARR(1)) +  ' ' +
021500170215             %trim(prtydesc)+ ' For ' +
021600170215             %editc(run_date:'Y');
021700170215         //
021800170215       ENDSR;
021900170215       //*************************************************
021901170215      /END-FREE
022000951120     OQSYSPRT   E            HEADER           01
022100950629     O                       @PGM                10
022200010902     O                       coname              70
022300951120     OQSYSPRT   E            HEADER           02
022400950629     O                       @USER               10
022500010904     O                       @title_rep          91
022600950629     O                                          120 'RUN Date: '
022700010902     O                       @date             +  1
022800010904     OQSYSPRT   E            div_hdr     2
022900950717     O                                           15 'DIVISION NO.:'
023000010904     O                       dtno          Z   +  2
023100950717     O                                         +  5 'NAME:'
023200010903     O                       DTNAME            +  2
023300010904     OQSYSPRT   E            dpt_hdr     2
023400950717     O                                           12 'DEPT NO'
023500950717     O                                           24 'DEPT NAME'
023600950717     O                                           71 'PAY SLIPS'
023700950717     O                                           89 'NETT PAY'
023800010904     OQSYSPRT   E            dpt_hdr     1
023900950717     O                                           12 '-------'
024000950717     O                                           24 '---------'
024100950717     O                                           71 '---------'
024200950717     O                                           89 '--------'
024300980105     OQSYSPRT   E            DETAIL      1
024400010903     O                       dept          z     10
024500010902     O                       lname               55
024600010902     O                       cnt           Z     66
024700010902     O                       AMTS          jB    90
024800010903     OQSYSPRT   E            Det         2
024900950710     O                                         +  2 'TOTAL'
025000010902     O                       tot_cnt       ZB    66
025100010902     O                       AMTS#         jB    90
025200971230     OQSYSPRT   E            TOTAL       2
025300950629     O                                           20 'TOTAL ENTRIES'
025400010903     O                       fin_tot_cnt   Z     66
025500010903     O                       fin_amts      j     90
025600971230     OQSYSPRT   E            TOTAL       2
025700950629     O                                           40 '*** END OF REPORT ***'
025800050227
025900170215      //--------------------------------------
026000170215      // Procedure name: SumDivision
026100170215      // Purpose:        get division summary
026200170215      //--------------------------------------
026300050227     P SumDivision     B
026400050227     D SumDivision     PI            11  2
026500050227
026600050227     D Totamt#         s             11  2
026700050227
026800050302
026900170215      // Getting payroll date in dmy format
027000050302     C     *iso          move      phispdate     pydate
027100050302     C                   move      pydate        dmydate
027200071007      /free
027300050322            // read Payment account file
027400071007            exec sql
027500071007              Select Sum(payamt) into :totamt# :amtNull
027600110121                from  genpysum  Where payemp  = :phisemp
027700071007                 and paydiv = :dtno and payrtype = :run_type
027800110121                  and paydate  = :dmydate and payorg = 'PAY';
027900050227
028000050322              return totamt#;
028100050322
028200050227      /end-free
028300050227     P SumDivision     E
028400050227
028500010902**
028600010902 Summary of Payments For -
028700010904     Summary of Payments By Division
