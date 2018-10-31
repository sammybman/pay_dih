000100000000     H DECEDIT('.') DATEDIT(*DMY/)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300111002     H/define profoundui
000400000000      //* EMPLOYEES EARNINGS BREAKDOWN
000500111002     Fdeptm     if   E           K DISK    usropn
000600111002     Fpayhisl07 if   E           K DISK    usropn
000700111002     Fpaytranl05if   E           K DISK    usropn
000800111002     Fpayrtyp   IF   E           K DISK    usropn
000900031004     Fqsysprt   O    f  132        PRINTER OFLIND(*INOF)
001000000000     Fpayrpt05d cf   e             workstn
001100111002     F/IF DEFINED(WEB)
001200111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001300111002     F/ENDIF
001400060217
001500110729      /copy qcpysrc,genpr
001600110729      /copy qcpysrc,autpr
001700110729      /copy qcpysrc,emppr
001800000000      //
001900000000     D arr             S             40    DIM(1) CTDATA PERRCD(1)
002000000000     Dsave_dept        s                   like(dept)
002100031004     Dstep_date        s              8  0
002200031117     Dhours            s             10  2
002300000000     Ddep_Rec          s              1  0 inz(0)
002400000000     Ddept_net         s             13  2
002500050525     Dcountr           s              5  0
002600000000     Dcnt              s              3  0
002700000000     Dpay_val          s             13  2
002800000000     Dptrnamt#         s                   like(ptrnamt)
002900000000     Dfin_amount       s             13  2
003000000000     Drep_title        s             30
003100000000     D date#           s                   like(phispdate)
003200000000     D last_date       s                   like(phispdate)
003300000000     D last_emp        s                   like(phisemp)
003400000000      //
003500000000     Ddate_err         c                   'Invalid Date(s) Entered'
003600000000     Dtype_err         c                   'Invalid Payroll Type Entered'
003700000000     Dcode_err         c                   'Invalid Transaction Code Entered'
003800000000     Drec_err          c                   'No Payroll Found For this Date'
003900000000     Dnorec_err        c                   'No Record Was Found for The -
004000000000     d                                      Criteria Entered'
004100000000     D                SDS
004200000000     D  @PGM             *PROC
004300111002     D  @USER                358    367
004400060217     d  @parm            *parms
004500031004
004600091119     D  main_pgm       pr                  extpgm('PAYRPT05R')
004700120215
004800060217     D  main_pgm       pi
004900120215
005000000000      //
005100000000     C     his_key       klist
005200031004     C                   kfld                    step_date
005300000000     C                   kfld                    dept
005400000000     C                   kfld                    run_type
005500000000
005600000000     C     trn_key       klist
005700031004     C                   kfld                    step_date
005800000000     C                   kfld                    run_type
005900000000     C                   kfld                    ptyptype
006000000000     C                   kfld                    phisemp
006100000000
006200040207      /FREE
006300111002         // authority check
006400111002         monitor;
006500111002           authority_check('PAY':@pgm);
006600111002         on-error;
006700111002           show_msg(get_err_msg('DIH0054'));
006800111002           *inlr = *on;
006900111002           return;
007000111002         endmon;
007100060217
007200060217            exsr aainit;
007300060217            dow 0 = 0;
007400060217            exfmt fmt1;
007500060217
007600000000         // clear all msgs & indicators
007700000000      /END-FREE
007800000000     c                   movea     '0000'        *in(25)
007900000000      /FREE
008000000000         errmsg = *blanks;
008100000000
008200000000         if *in03;
008300000000           *inlr = '1';
008400000000           return;
008500000000         endif;
008600000000         //Run type Enquiry
008700000000         if *in01;
008800000000      /END-FREE
008900000000     C                   call      'PAYENQ02R'
009000000000     C                   parm                    run_type
009100000000      /FREE
009200000000           iter;
009300000000         endif;
009400000000         // Pay code enquiry
009500000000         if *in05;
009600000000      /END-FREE
009700000000     C                   call      'PAYENQ01R'
009800000000     C                   parm                    pay_code
009900000000      /FREE
010000000000           iter;
010100000000         endif;
010200000000
010300000000         if pay_date = *zeros;
010400000000           if enddate < strdate;
010500000000             errmsg = date_Err;
010600000000             iter;
010700000000           endif;
010800000000         else;
010900000000         test(dE) pay_date;
011000000000         if %error;
011100000000           errmsg = date_err;
011200000000           iter;
011300000000         endif;
011400000000           chain pay_date phisfmt;
011500000000           if not %found;
011600000000             *in27 = '1';
011700000000             errmsg = rec_err;
011800000000             iter;
011900000000           endif;
012000000000           strdate = pay_date;
012100000000           enddate = pay_date;
012200000000         endif;
012300000000
012400000000         // validating payroll type
012500000000         chain run_type prtyfmt;
012600000000         if not %found;
012700000000           *in26 = '1';
012800000000           errmsg = type_err;
012900000000           iter;
013000000000         endif;
013100000000
013200000000         // pay_code is optional
013300000000
013400000000         if pay_code <> 0;
013500050519
013600050519            if get_payty_info('RD':pay_code:payty_struc);
013700050519
013800000000             *in28 = '1';
013900000000             errmsg = code_err;
014000000000             iter;
014100000000           endif;
014200000000         endif;
014300000000         leave;
014400000000       enddo;
014500000000
014600000000       Except header;
014700000000       //
014800000000       setll *loval dhrec;
014900000000       read dhrec;
015000000000       dow not (%eof(deptm));
015100000000         //
015200050519            countr = *zeros;
015300050519            dow not get_payty_info('RD':-1:payty_struc:countr);
015400000000
015500031004           step_date = strdate;
015600000000
015700000000          // run thru' range of dates
015800000000           dow 0 = 0;
015900031004           setll his_key phisfmt;
016000000000           exsr accumulate_totals;
016100031004
016200031004             setgt step_date payhisl07;
016300000000             read payhisl07;
016400031004            step_date = phisndate;
016500031004
016600031004             if step_date > enddate
016700000000                or %eof(payhisl07);
016800000000               leave;
016900000000             endif;
017000000000           enddo;
017100000000
017200000000           if pay_val <> 0;
017300000000             fin_amount = fin_amount + pay_val;
017400000000             dept_net = dept_net + pay_val;
017500000000
017600000000             if *inof;
017700000000               EXCEPT header;
017800000000               EXCEPT dept_hdr;
017900000000             endif;
018000000000
018100000000             EXCEPT details;
018200000000           else;
018300000000             hours = 0;
018400000000           endif;
018500050519
018600000000         enddo;
018700000000
018800000000         if dept_net <> 0;
018900000000           Except dept_sum;
019000000000         endif;
019100000000
019200000000         read dhrec;
019300000000         dep_rec = 0;
019400000000       enddo;
019500000000
019600000000       // if no record found show message
019700000000
019800000000       if fin_amount = 0;
019900000000         errmsg = norec_err;
020000000000         except norec_prt;
020100000000       else;
020200000000         EXCEPT fin_total;
020300000000         *inlr = '1';
020400000000         return;
020500000000       endif;
020600000000
020700111002
020800111002         //*****************************
020900111002         // Session Init
021000111002         //*****************************
021100111002
021200111002          begsr *inzsr;
021300111006            get_session_info('PAY':errmsg:@pgm);
021400111002            if errmsg <> *blanks;
021500111002              show_msg(errmsg);
021600111002              *inlr = *on;
021700111002              return;
021800111002            endif;
021900111002
022000111002            open(e) payrtyp;
022100111002            open(e) payhisl07;
022200111002            open(e) paytranl05;
022300111002            open(e) deptm;
022400111002
022500111002          endsr;
022600111002
022700000000       //*************************************************
022800000000       // Accumulate department and type totals **
022900000000       //*************************************************
023000000000           begsr accumulate_totals;
023100000000
023200031004           reade his_key  phisfmt;
023300000000           last_date = *zeros;
023400000000           last_emp  = *zeros;
023500000000           if not (%eof(payhisl07));
023600000000
023700000000             if dep_rec = 0;
023800000000               EXCEPT dept_hdr;
023900000000               dep_rec = 1;
024000000000             endif;
024100000000
024200000000           endif;
024300000000
024400000000           dow not (%eof(payhisl07));
024500031004
024600000000             if last_emp <> phisemp;
024700000000
024800000000               setll trn_key ptisfmt;
024900000000               reade trn_key ptisfmt;
025000000000               //
025100000000               dow not %eof(paytranl05);
025200000000
025300000000                 //if a transaction code is entered
025400000000                 if pay_code <>  0 and pay_code = ptrntcode;
025500000000                   if ptrncode = 'D';
025600000000                     pay_val = pay_val + (ptrnamt * -1);
025700031004                    else;
025800000000                     pay_val = pay_val + ptrnamt;
025900000000                   Endif;
026000000000                 else;
026100000000                   //show all that qualify
026200000000                   //
026300000000                   select;
026400000000                   when rep_opt = 'N';
026500000000                     if ptrncode = 'D';
026600000000                      pay_val = pay_val + (ptrnamt * -1);
026700000000                     else;
026800000000                      pay_val = pay_val + ptrnamt;
026900000000                     Endif;
027000000000
027100000000                   when rep_opt = 'D';
027200000000                     if ptrncode = rep_opt;
027300000000                      pay_val = pay_val + ptrnamt;
027400000000                     Endif;
027500000000                   when rep_opt = 'E';
027600000000                     if ptrncode = rep_opt;
027700000000                      pay_val = pay_val + ptrnamt;
027800000000                     Endif;
027900000000                   endsl;
028000000000
028100000000                 Endif;
028200000000
028300000000                 hours = hours + ptrnhrs;
028400000000                 reade trn_key ptisfmt;
028500000000               enddo;
028600000000               //                  eval      last_date = phispdate
028700000000               last_emp  = phisemp;
028800000000             endif;
028900000000             //
029000000000                reade his_key phisfmt;
029100000000           enddo;
029200000000           endsr;
029300000000
029400000000       //*************************************************
029500000000       // First initilisation routine for all programs  **
029600000000       //*************************************************
029700000000       Begsr aainit;
029800000000         @title = arr(1);
029900000000         rep_title = %trim(arr(1));
030000000000         coname = get_coname;
030100000000         @date = get_sys_date(date#);
030200000000         //
030300000000       ENDSR;
030400000000       //
030500000000      /END-FREE
030600031004     Oqsysprt   e            header            1
030700000000     O                       @PGM                10
030800000000     O                       coname              72
030900000000     O                                          120 'Run Date: '
031000000000     O                       @date             +  1
031100000000      //
031200031004     Oqsysprt   e            header      1  2
031300000000     O                       @USER               10
031400000000     O                       rep_title           72
031500000000      //
031600000000     O                                          120 'Payroll Date:'
031700000000     O                       pay_date          +  1 '    /  /  '
031800031004     Oqsysprt   e            header      1  2
031900000000     O                                           25 'Payroll Type:'
032000000000     O                       prtydesc            52
032100000000      //
032200000000     O          E            dept_hdr    1
032300000000     O                                           25 'Department:'
032400000000     O                       dept          Z     29
032500000000     O                       dname               70
032600000000     O          E            dept_hdr    1
032700000000     O                                           20 'Transaction Type:'
032800000000     O                                           37 'Hours:'
032900000000     O                                           52 'Amount'
033000000000     O          E            details     2  1
033100000000     O                       ptyptype            04
033200000000     O                       ptydesc             30
033300031117     O                       hours          b    37 '        :  '
033400000000     O                       pay_val       jb    52
033500000000     O          e            dept_sum    2
033600000000     O                                           25 'Department Amount:'
033700000000     O                       dept_net      1b    45
033800000000     O          e            fin_total   2
033900000000     O                                           25 'TOTAL AMOUNT PAID:'
034000000000     O                       fin_amount    1     40
034100000000     O          e            norec_prt   2
034200000000     O                                            5 '***'
034300000000     O                                           40 'No Record Was Found for Th'
034400000000     O                                           +0 'e Criteria Entered'
034500000000     O                                           +1 '***'
034600000000**
034700000000     Department Payment Analysis
