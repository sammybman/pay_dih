000100950906     H DECEDIT('.') DATEDIT(*DMY/)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000300170427     H alwnull(*usrctl)
000400111002     H/define profoundui
000500090415      //
000600090415      // LISTING DIFFERENCE IN EMPLOYEES EARNINGS AT START OF YEAR
000700090415      // PROGRAMMER------ S.BRITTON
000800090415      // EMPLOYEE MASTER
000900111002     Fmasall    if   e           k disk    usropn
001000090415      // EMPLOYEE WAGES MASTER
001100111002     Fpayhisl09 if   e           k disk    usropn
001200111002     Fpaytranl02if   e           k disk    usropn
001300111002     Fpayalwtl02if   e           k disk    usropn
001400090415      // EMPLOYEE Transaction History
001500111002     Fpayalwml02if   e           k disk    usropn
001600111002     Fpayrtypl01if   e           k disk    usropn
001700111002     Fpayrdefl03if   e           k disk    usropn
001800111002     Fpaycycd   if   e           K DISK    usropn
001900090415      // Employee change Log
002000111002     Fempchgll01if   e           k disk    usropn
002100010919     Fempchgll02if   e           k disk    rename(elogfmt:elogl02) prefix(x_)
002200111002     F                                     usropn
002300090415      // TIMESHEET FILE -FOR PAYROLL PROCESSING
002400011219     Fpaytimfl01uf a e           k disk    usropn
002500040207     Fqsysprt   o    f  132        printer
002600090415      // PROGRAM DISPLAY FILE
002700010901     Fpayr004d  cf   e             workstn sfile(sfile:rrn)
002800111002     F/IF DEFINED(WEB)
002900111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
003000111002     F/ENDIF
003100090415      // Payroll PROTOTYPES
003200111002      /copy QCPYSRC,PAYPR
003300111002      /copy QCPYSRC,genPR
003400111002      /copy QCPYSRC,autPR
003500111002      /copy QCPYSRC,emppr
003600111002      /copy QCPYSRC,progbarpr
003700090415      //
003800090415      //-- Get next value --*                                                --
003900010919     Dget_nxt_val      pr            13  2
004000010919     D                                8  0 value
004100090415      //
004200960924     D                sds
004300961206     D  @pgm             *proc
004400961206     D  @job                 244    253
004500111002     D  @user                358    367
004600090415      //
004700011006     D arr             s             40    dim(1) ctdata perrcd(1)
004800011013     D prd_arr         s              8  0 dim(99)
004900051116     D lprd_arr        s              8  0 dim(99)
005000011007     D coname_lines    s             30    inz(*all'=')
005100011218     D err#            s              7
005200010911     D rrn             s              3  0
005300010919     D xcnt            s                   inz(0) like(rrn)
005400010911     D dname           s             40
005500040222     D dept#           s              3s 0
005600011013     D hdrcnt          s              2  0
005700011024     D prdcnt          s              2  0
005800010911     D emptot          s              6  0 inz(0)
005900081206     D empcnt          s              6  0 inz(0)
006000010911     D last_date       s                   like(phispdate)
006100011023     D first_val       s                   like(elogpval)
006200011023     D last_cdate      s                   like(elogcdate)
006300011023     D payment_type    s                   like(ptrntcode)
006400011023     D pay_opt         s              1
006500010924     D diff            s             11  2
006600010924     D totdiff         s             11  2
006700081206     D first_record    s               n   inz(*on)
006800011218     D emp_selected    s               n   inz(*off)
006900011029     D payrun_factor   s              7  3
007000081206     D TimeStart       S               T
007100081206     D TimeNow         S               T
007200081206     D TimeHMS         S               T   TimFmt(*HMS)
007300081206     D TotalSecs       S              9S 0
007400081206
007500081206     D proper          s              4  1
007600081206     D counter         s              9  0
007700081206     D num_recs        s              9  0
007800081206     D pcta            s              5
007900081206     D progBar1Obj     S                   like(createProgressBar)
008000081206     D progBar1        DS                  likeDS(prgBars) based(progBar1Obj)
008100090415      //
008200081206     D FULPERCENT      c                   100
008300111109
008400011013     D err1            C                   'Type Used to Create Payroll must be-
008500011013     D                                      a General Type'
008600011013     D err2            C                   'No valid payroll run type found to -
008700011013     D                                      create Payroll'
008800011013     D err3            C                   'Start date greater than End date'
008900011013     D err4            C                   'Invalid "CREATE" Payroll Run Type'
009000011218     D err5            C                   'Invalid Employee Number'
009100061121     D err6            C                   'Invalid OMIT DATE(s) entered'
009200011013     D msg1            C                   'Searching for valid Transactions  -
009300011013     D                                         ....... Please Wait'
009400011013     D msg2            C                   'The above period(s) were found'
009500011013     D msg3            C                   'Preparing to Process Transactions  -
009600011013     D                                         ....... Please Wait'
009700111109
009800961113     C     key           klist
009900961113     C                   kfld                    fdate
010000961113     C                   kfld                    emp
010100111109
010200010911     C     trn_key       klist
010300010918     C                   kfld                    run_type
010400010911     C                   kfld                    sdate
010500111109
010600010911     C     trn_key2      klist
010700010911     C                   kfld                    run_type
010800010911     C                   kfld                    emp
010900010918     C                   kfld                    prd_arr(xcnt)
011000011027     C                   kfld                    prdftype
011100111109
011200010911     C     elog_key      klist
011300010911     C                   kfld                    emp
011400011023     C                   kfld                    payment_type
011500010924     C                   kfld                    sdate
011600111109
011700010918     C     elog_key2     klist
011800010918     C                   kfld                    emp
011900011023     C                   kfld                    payment_type
012000111109
012100010918     C     emp_key       klist
012200011029     C                   kfld                    payment_type
012300010918     C                   kfld                    emp
012400111109
012500011218     C     emp_selkey    klist
012600011218     C                   kfld                    dept#
012700011218     C                   kfld                    emp#
012800011013
012900040207      /FREE
013000111002         // authority check
013100111002         monitor;
013200111002           authority_check('PAY':@pgm);
013300111002         on-error;
013400111002           show_msg(get_err_msg('DIH0054'));
013500111002           *inlr = *on;
013600111002           return;
013700111002         endmon;
013800111002
013900090415       exsr aainit;
014000090415       reset emp_selected;
014100111002
014200090415       //***************************
014300090415       //*  DISPLAY MAIN SCREEN  ***
014400090415       //***************************
014500090415       dow 0 = 0;
014600090415         *in33 = *on;
014700090415         write sfctl;
014800090415         *in33 = *off;
014900090415         *in77 = *off;
015000090415         rrn = *zeros;
015100090415         message = *blanks;
015200090415         write msgdsp;
015300111109
015400090415         write footer;
015500090415         exfmt sfctl;
015600090415         errmsg = *blanks;
015700090415         //  End  Program
015800090415         if *in03;
015900090415           *inlr = *on;
016000090415           return;
016100090415         endif;
016200111109
016300090415         // validate dates
016400090415         test(dE) *iso sdate;
016500090415         *IN10 = %ERROR;
016600090415         if *in10;
016700090415           iter;
016800090415         endif;
016900090415         //
017000090415         test(dE) *iso edate;
017100090415         *IN10 = %ERROR;
017200090415         if *in10;
017300090415           iter;
017400090415         endif;
017500111109
017600061121          // validate omission dates
017700061121          if omitdate1 <> *zeros;
017800061121             test(de) *iso omitdate1;
017900061121             if %error;
018000061121               errmsg = err6;
018100061121               iter;
018200061121             endif;
018300061121          endif;
018400061121          if omitdate2 <> *zeros;
018500061121             test(de) *iso omitdate2;
018600061121             if %error;
018700061121               errmsg = err6;
018800061121               iter;
018900061121             endif;
019000061121          endif;
019100061121      /end-free
019200111109
019300090415      /FREE
019400090415         if sdate > edate;
019500090415           errmsg = err3;
019600090415           iter;
019700090415         endif;
019800090415         // validate Employee number
019900090415         if emp# <> *zeros;
020000090415           status = get_emp_info(emp#:dept#:*blanks:
020100090415               *omit:*omit:*omit:*omit:err#);
020200090415           if err# <> *blanks;
020300090415             errmsg = err5;
020400090415             iter;
020500090415           endif;
020600090415         endif;
020700011013
020800090415         // validate "Create Payroll" type
020900090415         chain spc_type prtyfmt;
021000090415         select;
021100090415         when %found;
021200090415           if prtyprno > *zeros;
021300090415             errmsg = err1;
021400090415             iter;
021500090415           endif;
021600090415         other;
021700090415           errmsg = err4;
021800090415           iter;
021900090415         endsl;
022000090415         // validate "Run Payroll" type
022100090415         chain run_type prtyfmt;
022200111109
022300090415         last_date = *zeros;
022400090415         reset emptot;
022500090415         message = msg1;
022600090415         write msgdsp;
022700111109
022800090415         setll trn_key phisfmt;
022900090415         reade run_type phisfmt;
023000090415         dow phispdate <= edate and not(%eof);
023100090415           emptot = emptot + 1;
023200090415           if last_date <> phisndate;
023300061121         if %scan(%char(phisndate):%char(omitdate1) +
023400061121                  '.' + %char(omitdate2)) = 0;
023500090415             fdate = phisndate;
023600090415             rrn = rrn + 1;
023700090415             prd_arr(rrn) = phisndate;
023800090415             lprd_arr(rrn) = phisrdate;
023900090415             write sfile;
024000061121         endif;
024100090415             last_date = phisndate;
024200090415           endif;
024300090415           reade run_type phisfmt;
024400090415         enddo;
024500090415         // ... More Indicator
024600090415         *in55 = *on;
024700090415         // get Run-type cycle
024800090415         chain prtycycode cycfmt;
024900090415         payrun_factor = cycyearf * prtycycqty;
025000090415         leave;
025100090415       enddo;
025200010911
025300090415       //********************
025400090415       // PERFORM CALCS.   **
025500090415       //********************
025600090415       dow 0 = 0;
025700090415         *in77 = rrn > 0;
025800090415         message = msg2;
025900090415         write msgdsp;
026000090415         write footer;
026100090415         exfmt sfctl;
026200090415         //  End Program
026300090415         if *in03;
026400090415           *inlr = *on;
026500090415           return;
026600090415         endif;
026700010911
026800090415         //  RESET FILE CHANGES
026900090415         if *in02;
027000090415           leave;
027100090415         endif;
027200090415         //
027300090415         //  GENERATE PAYROLL FILES
027400090415         if *in15 = *on;
027500090415           message = msg3;
027600090415           write msgdsp;
027700090415           exsr @gen_file;
027800090415           commit;
027900090415           return;
028000090415         endif;
028100090415         //
028200090415       enddo;
028300111002
028400111002
028500111002         //*****************************
028600111002         // Session Init
028700111002         //*****************************
028800111002
028900111002          begsr *inzsr;
029000111006            get_session_info('PAY':errmsg:@pgm);
029100111002            if errmsg <> *blanks;
029200111002              show_msg(errmsg);
029300111002              *inlr = *on;
029400111002              return;
029500111002            endif;
029600111002
029700111002            open(e) masall;
029800111002            open(e) payhisl09;
029900111002            open(e) paytranl02;
030000111002            open(e) payalwtl02;
030100111002            open(e) payalwml02;
030200111002            open(e) payrtypl01;
030300111002            open(e) payrdefl03;
030400111002            open(e) paycycd;
030500111002            open(e) empchgll01;
030600111002            open(e) empchgll02;
030700111002
030800111002          endsr;
030900111002
031000090415       //*********************************************************************
031100090415       //* END OF PROGRAM ***
031200090415       //*********************************************************************
031300090415       //===================================================================**
031400961116
031500090415       //===================================================================**
031600090415       begsr @gen_file;
031700961116
031800090415         // CLEAR FILE BEFORE WRITING NEW INFORMATION
031900111109
032000090415      /END-FREE
032100011029     C                   callb     'PAYCALCCLR'
032200011029     C                   parm                    spc_type
032300090415      //
032400090415      // OPEN FILE AFTER CLEARING IT
032500090415      /FREE
032600090415         open paytimfl01;
032700081206
032800081206           TimeStart = %time();
032900081206           progBar1Obj = createProgressBar();
033000081206           if progBar1Obj <> *Null;
033100081206             progBar1.WindowTop = 14;
033200081206             progBar1.WindowTitle = 'Retro. Payroll Calcs.';
033300081206             progBar1.Color = COLOR_BLUE;
033400081206             progBar1.ShowPercent = *ON;
033500081206           endif;
033600010911
033700091119         // for selected employee
033800090415         if emp# <> *zeros;
033900090415           chain emp_selkey emprec;
034000090415           *INlr = NOT %FOUND;
034100090415           emp_selected = *on;
034200090415         else;
034300090415           setll *loval emprec;
034400090415           read emprec;
034500090415           *INlr = %EOF;
034600090415         endif;
034700111109
034800090415         dow not(*inlr);
034900111109
035000090415           if get_cat(status:*omit:pay_opt) = 'A';
035100111109
035200090415             dname = get_dept_info(dept);
035300111109
035400090415             //*********************************************************************
035500090415             // ACCUMULATE HOURS AND CALCULATE AMOUNTS  **
035600090415             //*********************************************************************
035700090415             hdrcnt = *zeros;
035800090415             totdiff = *zeros;
035900090415             reset xcnt;
036000090415             //-------------------------------------------------------------------**
036100090415      /END-FREE
036200010918     C                   do        rrn           xcnt
036300090415      //
036400090415      /FREE
036500090415               prdcnt = *zeros;
036600011024
036700090415               chain run_type prdffmt;
036800090415               *IN99 = NOT %FOUND;
036900090415               //  Y.T.D. Payroll File
037000090415               dow not(*in99);
037100050519               get_payty_info('RD':prdftype:payty_struc);
037200050519
037300091119          // emoluments ONLY
037400090415                 if ptyrtype = 'E';
037500090415                   chain trn_key2 ptisfmt;
037600090415                   // show counter
037700090415                   select;
037800090415                   when %found;
037900090415                     //*                 if        first_record
038000090415                     empcnt += 1;
038100090415                     //*                 eval      first_record = *off
038200081206          if empcnt <= emptot;
038300081206             proper = empcnt * FULPERCENT / emptot;
038400081206             //(Below 5% the estimated time is probably badly misleading)
038500081206               If  proPer > 5;
038600081206                     TimeNow = %time();
038700081206                     // Time elapsed so far:?
038800081206                     TotalSecs = %diff(TimeNow :
038900081206                                          TimeStart : *seconds);
039000081206                     TotalSecs = TotalSecs/proper * (100 - proper);
039100081206                     TimeHMS = T'00.00.00' + %seconds(TotalSecs);
039200081206
039300081206               Endif;
039400081206
039500081206              progBar1.Percent = proper;
039600081206              pctA = %editw(progBar1.Percent: '  0%');
039700081206              progBar1.StatusMessage =
039800081206                     %Triml(%EditC(empcnt:'3')) +
039900081206                      ' records of ' +
040000081206                       %Triml(%EditC(emptot:'3')) + ' read.' +
040100081206                               pctA + '          ' +
040200081206                                 %char(TimeHMS) + ' remaining  ';
040300081206              displayProgressBar(progBar1Obj);
040400090415                   endif;
040500090415                 other;
040600090415                   ptrnamt = *zeros;
040700090415                 endsl;
040800090415                 //
040900090415                 exsr @calcs;
041000090415                 exsr @update;
041100090415                 exsr @rptwrite;
041200090415               endif;
041300111109
041400090415               reade run_type prdffmt;
041500090415               *IN99 = %EOF;
041600090415             enddo;
041700090415             //                  reset                   first_record
041800981125
041900090415           enddo;
042000011013
042100090415           // Print final totals if employee differences are not zero
042200090415           if totdiff <> *zeros;
042300090415             except em_total;
042400090415           endif;
042500111109
042600090415         endif;
042700090415         // for single employee selection
042800090415         if emp_selected;
042900090415           *inlr = *on;
043000090415         else;
043100090415           read emprec;
043200090415           *INlr = %EOF;
043300090415         endif;
043400090415         //
043500090415       enddo;
043600090415       close paytimfl01;
043700081206         destroyProgressBar(progBar1Obj);
043800091119
043900090415       endsr;
044000090415       //===================================================================**
044100961114
044200961114
044300090415       //===================================================================**
044400090415       //******************
044500090415       // Perform Calcs. **
044600090415       //******************
044700090415       begsr @calcs;
044800010918
044900090415         diff = *zeros;
045000090415         if ptypaytyp = 'H';
045100090415           payment_type = 1;
045200090415         else;
045300090415           payment_type = prdftype;
045400090415         endif;
045500090415         //
045600090415         setll elog_key elogfmt;
045700090415         reade elog_key2 elogfmt;
045800090415         //   Position log file for Processing
045900090415         dow elogedate <= prd_arr(xcnt) and
046000111110               elogcdate <= lprd_arr(xcnt) and not(%eof);// changes on date
046100111110           //  elogcdate < lprd_arr(xcnt) and not(%eof);
046200090415           reade elog_key2 elogfmt;
046300090415         enddo;
046400111109
046500090415         if not(%eof) and elogedate <= prd_arr(xcnt)
046600090415               //                            and elogcdate > prd_arr(xcnt)
046700111110               and elogcdate >  lprd_arr(xcnt); // changes made on p.date
046800111110           //  and elogcdate >= lprd_arr(xcnt);
046900090415           // save first change of value for calculation
047000090415           first_val = elogpval;
047100011006
047200090415           dow not(%eof) and elogedate <= prd_arr(xcnt)
047300090415                 //                            and elogcdate > prd_arr(xcnt)
047400090415                 and elogcdate >= lprd_arr(xcnt);
047500090415             // save last "change date" for calculation
047600090415             last_cdate = elogcdate;
047700090415             //
047800090415             reade elog_key2 elogfmt;
047900090415           enddo;
048000090415
048100090415           // IF Emolument is "Old" to employee
048200090415           select;
048300090415           when first_val <> *zeros;
048400090415
048500090415             diff = round_up(ptrnamt /first_val *
048600090415                 (get_nxt_val(last_cdate)
048700090415                 - first_val):'U');
048800090415             // Drop calculation of "Hourly" Payments for employee now in "Money"
048900090415             // type classification.
049000090415             if ptypaytyp = 'H' and pay_opt = 'M';
049100090415               diff = *zeros;
049200090415             endif;
049300090415             //
049400090415           other;
049500090415             // Cannot calculate differences for "Hourly" Payments for employee
049600090415             // not paid in pay period.
049700090415             if ptypaytyp = 'M' or ptypaytyp = 'B' and
049800090415                   pay_opt = 'M';
049900090415               //
050000090415               diff = round_up((get_nxt_val(last_cdate)
050100090415                   /payrun_factor):'U');
050200090415             endif;
050300090415           endsl;
050400090415         endif;
050500090415         //
050600090415         totdiff = totdiff + diff;
050700090415
050800090415       endsr;
050900010911
051000090415       //******************
051100090415       // Update Files   **
051200090415       //******************
051300090415       begsr @update;
051400090415         //
051500090415         // Write Info to timesheet file
051600090415      /END-FREE
051700011219     C     write_key     klist
051800011219     C                   kfld                    spc_type
051900011219     C                   kfld                    prdftype
052000011219     C                   kfld                    emp
052100090415      //
052200090415      /FREE
052300090415         if diff > *zeros;
052400090415           chain write_key timfmt;
052500090415           if %found;
052600090415             timamt = timamt + diff;
052700090415             update timfmt;
052800090415           else;
052900090415             timemp = emp;
053000090415             timcode = ptyrtype;
053100090415             timtyp = prdftype;
053200090415             timamt = diff;
053300090415             timrtype = spc_type;
053400090415             write timfmt;
053500090415           endif;
053600090415           //
053700090415         endif;
053800090415       endsr;
053900010918
054000090415       //******************
054100090415       // Write Report   **
054200090415       //******************
054300090415       begsr @rptwrite;
054400090415         //--------------------------------------------------------------------------
054500090415         // Print detail lines if employee differences are not zero
054600090415         if diff <> 0;
054700010911
054800090415           // Print Header lines once if employee differences are not zero
054900090415           if hdrcnt = 0;
055000010911
055100090415             except hedr;
055200090415             except hedr1;
055300090415             //
055400090415             hdrcnt = hdrcnt + 1;
055500090415           endif;
055600010911
055700090415           // Print Period lines once for series of transactions of the period
055800090415           if prdcnt = 0;
055900011024
056000090415             except prd_total;
056100090415             prdcnt = prdcnt + 1;
056200090415           endif;
056300090415           //
056400090415           except prd_total1;
056500090415         endif;
056600090415       endsr;
056700961116
056800090415       //*************************************************
056900090415       // First initilisation routine for all programs  **
057000090415       //*************************************************
057100090415       begsr aainit;
057200090415         coname = get_coname;
057300090415         @date = get_sys_date;
057400090415      /END-FREE
057500010918     C                   time                    @time             6 0
057600090415      // Select header for program
057700090415      /FREE
057800090415         @title = ARR(1);
057900090415         //
058000090415         setll *loval prtyfmt;
058100090415         read prtyfmt;
058200090415         dow not(%eof);
058300090415           if prtyprno = *zeros;
058400090415             spc_type = prtyptype;
058500090415             leave;
058600090415           endif;
058700090415           read prtyfmt;
058800090415         enddo;
058900090415         // Validate "create payroll type"
059000090415         if spc_type = *blanks;
059100090415           errmsg = err2;
059200090415         endif;
059300090415         //
059400090415       endsr;
059500961116
059600090415       //===================================================================**
059700090415      /END-FREE
059800040207     Oqsysprt   e            hedr           1 04
059900961206     O                       @job                10
060000011007     O                       coname              75
060100961209     O          e            hedr           2
060200961206     O                       @pgm                10
060300011007     O                       coname_lines        75
060400011024     O          e            hedr        0  1
060500961206     O                       @user               10
060600961209     O          e            hedr           1
060700011024     O                                           51 'EMPLOYEES'
060800011024     O                                           76 'RETROACTIVE PAYMENT REPO'
060900011024     O                                           78 'RT'
061000090415      //*                                        128 'PAGE'
061100090415      //*                     page               132
061200961209     O          e            hedr           2
061300011024     O                                           51 '========='
061400011024     O                                           75 '========================'
061500011024     O                                           78 '==='
061600961209     O          e            hedr           2
061700961209     O                                            4 'EMP:'
061800961209     O                       emp           z  +   1
061900961209     O                       christ           +   1
062000961209     O                       surnam           +   1
062100961210     O          e            hedr           2
062200010924     O                                           12 'DEPARTMENT:'
062300961210     O                       dname            +   1
062400010918     O          e            hedr1          1
062500011024     O                                           22 '|'
062600010924     O                                           32 'Payroll'
062700011024     O                                           55 'Hours'
062800011024     O                                           75 'Period'
062900011024     O                                          105 'Difference'
063000010918     O          e            hedr1          1
063100011024     O                                           22 '|'
063200011024     O                                           32 'Type'
063300011024     o                                           55 'Worked'
063400011024     O                                           75 'Amount'
063500011024     o                                          105 'In Wages'
063600010918     O          e            prd_total      1
063700010911     O                                           10 'PERIOD:'
063800010924     O                       prd_arr(xcnt)     +  1 '    /  /  '
063900961209     O                                         +  0 '|'
064000010918     O          e            prd_total1     1
064100011024     O                                           22 '|'
064200011024     O                       ptydesc             49
064300011024     O                       ptrnhrs       1     55
064400011024     O                       ptrnamt       1     75
064500011024     o                       diff          j    105
064600961209     O          e            em_total    1
064700961209     O                                           15 'FINAL TOTALS:'
064800090415      //                      tnrhrs        1     28
064900011024     O                       totdiff       j    132
065000090415      //*******************************
065100090415      // Validate Selected Types     **
065200090415      //*******************************
065300010918     Pget_nxt_val      b
065400090415      //
065500010918     Dget_nxt_val      pi            13  2
065600010919     Drun_date                        8  0 value
065700090415      //
065800010919     dreturn_val       s             13  2
065900090415      //
066000010918
066100090415      //
066200010919     C     ell2_key      klist
066300010919     C                   kfld                    emp
066400011023     C                   kfld                    payment_type
066500010919     C                   kfld                    run_date
066600090415      //
066700010919     C     ell2_key2     klist
066800010919     C                   kfld                    emp
066900011023     C                   kfld                    payment_type
067000010918
067100090415      //   get next change
067200090415      /FREE
067300090415       setgt ell2_key elogl02;
067400090415       reade ell2_key2 elogl02;
067500090415       //
067600090415       if not(%eof);
067700090415         return_val = x_elogpval;
067800090415       else;
067900090415         chain emp_key altfmt;
068000090415         return_val = altamt;
068100090415       endif;
068200090415       //
068300090415       return return_val;
068400090415      /END-FREE
068500010918     Pget_nxt_val      e
068600011006
068700090415      //
068800010918**  Program Title
068900010918      Retroactive Payroll Handler
