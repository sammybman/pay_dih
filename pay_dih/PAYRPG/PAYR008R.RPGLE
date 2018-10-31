000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111002     H/define profoundui
000300010427     Fpayr008d  cf   e             workstn
000400111002     F/IF DEFINED(WEB)
000500111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000600111002     F/ENDIF
000700111002     Fpayrtypl01uf   e           k disk    commit usropn
000800111002     Fpayrresl01if   e           k disk    usropn
000900111002     Fpayrdefl03if   e           k disk    usropn
001000060809     Fpaytnxtr  if   e           k disk    usropn
001100111002     Fpaycycd   if   e           k disk    usropn
001200010426     **
001300110531      /copy qcpysrc,genPR
001400110531      /copy qcpysrc,autPR
001500110531      /copy qcpysrc,empPR
001600010822     **
001700010822      *-- Validate pay types --*                                            --
001800010822     Dvalidate_type    pr              n
001900010822     D                                1    value
002000010822     **
002100010426     D                sds
002200010426     D  @pgm             *proc
002300111002     D  @user                358    367
002400971012     **
002500010411     Darr              s             40    dim(1) ctdata perrcd(1)
002600010426     **
002700010822     Ddate             s              8  0
002800010426     Dcurdate          s               d   datfmt(*iso)
002900010426     dnxtdate          s               d   datfmt(*iso)
003000010426     dno_days          s              4  0
003100010426     d cnt             s              2  0
003200010426     d qry_select      s            200
003300010426     d type_list       s              1    dim(20)
003400010720     **
003500010802     d msg1            c                   'Invalid Payroll run-type'
003600010802     d msg2            c                   'Payroll Not Available'
003700010724     d msg3            c                   'This Run type cannot be processed'
003800010802     d msg4            c                   'Payroll info. already created'
003900010822     d msg5            c                   'Some Type(s) selected for payment c-
004000010822     d                                     annot be used at this time'
004100010411
004200111002      /free
004300111002         // authority check
004400111002         monitor;
004500111002           authority_check('PAY':@pgm);
004600111002         on-error;
004700111002           show_msg(get_err_msg('DIH0054'));
004800111002           *inlr = *on;
004900111002           return;
005000111002         endmon;
005100060807       // Initial Setup
005200060804       exsr aainit;
005300060807
005400060804       //************************
005500060804       //* Start of Program   ***
005600060804       //************************
005700060804       dow 0 = 0;
005800060804         exfmt fmt1;
005900060804         // Terminate program
006000060804         if *in03;
006100060804           *inlr = *on;
006200060804           return;
006300060804         endif;
006400060804         // Validate run type entry
006500060804         chain run_type prtyfmt;
006600060804         if %found;
006700060804           // Run type cannot be processed
006800060804           if prtyprno = 0;
006900060804             errmsg = msg3;
007000060804             iter;
007100060804           endif;
007200060804           // payroll info already created
007300060804           if prtyflag = 'Y';
007400060804             errmsg = msg4;
007500060804             iter;
007600060804           endif;
007700060804           // validate payroll type for availability
007800060804           if validate_type(run_type);
007900060804             errmsg = msg5;
008000060804             iter;
008100060804           endif;
008200060804           //
008300060804           errmsg = *blanks;
008400060804         else;
008500060804           errmsg = msg1;
008600060804           iter;
008700060804         endif;
008800060804         //
008900060804         leave;
009000060804       enddo;
009100060804       //
009200060804       //************************
009300060804       //* Processing         ***
009400060804       //************************
009500060804       dow 0 = 0;
009600060804         exfmt fmt2;
009700060807
009800060804         // Terminate program
009900060804         if *in03;
010000060804           *inlr = *on;
010100060804           return;
010200060804         endif;
010300060807
010400060804         // Generate Timesheets
010500060804         if *in21;
010600060804           exsr generate_sheet;
010700060804           //
010800060804           if *in66;
010900060804             errmsg = msg2;
011000060804           else;
011100060804             errmsg = *blanks;
011200060804           endif;
011300060804           //
011400060804         endif;
011500060804         //
011600060804         leave;
011700060804       enddo;
011800111002
011900111002
012000111002         //*****************************
012100111002         // Session Init
012200111002         //*****************************
012300111002
012400111002          begsr *inzsr;
012500111006            get_session_info('PAY':errmsg:@pgm);
012600111002            if errmsg <> *blanks;
012700111002              show_msg(errmsg);
012800111002              *inlr = *on;
012900111002              return;
013000111002            endif;
013100111002
013200111002            open(e) payrtypl01;
013300111002            open(e) payrresl01;
013400111002            open(e) payrdefl03;
013500111002            open(e) paycycd;
013600111002
013700111002          endsr;
013800111002
013900111002
014000060804       begsr generate_sheet;
014100060804         chain run_type prrefmt;
014200060804         // Move type field to array
014300060804      /END-FREE
014400010619     C                   movea     prrelist      type_list
014500060804      /FREE
014600060804         cnt = 1;
014700060804         // setup query select parameter for payroll run
014800060804         qry_select = ' status = ''' +
014900060804             type_list(cnt) + '''';
015000060804         cnt = cnt + 1;
015100060807
015200060804         dow type_list(cnt) <> *blank;
015300060804           // setup query select parameter for payroll run
015400060804           //
015500060804           qry_select = %trim(qry_select) +
015600060804               ' *or status = ''' +
015700060804               type_list(cnt) + '''';
015800060804           cnt += 1;
015900060804         enddo;
016000060804         //  Select transactions
016100060804         if prtyndate > *zeros;
016200060804      /END-FREE
016300010427     C                   move      prtyndate     nxtdate
016400010927     C     nxtdate       subdur    curdate       no_days:*d
016500060804      //
016600060804      /FREE
016700060804           if no_days > prtyavail;
016800060804             *in66 = *on;
016900060804           else;
017000060804             exsr run_rtn;
017100060804           endif;
017200060804           //
017300060804         else;
017400060809
017500060804           exsr run_rtn;
017600060804         endif;
017700010427
017800060804         //
017900060804       endsr;
018000111002
018100060804       //****************************************************************
018200060804       //*  Create time sheet
018300060804       //****************************************************************
018400060804       //
018500060804       begsr run_rtn;
018600060804         //
018700060804      /END-FREE
018800010427     C     run_parm      plist
018900010427     C                   parm                    prtyptype
019000020304     C                   parm                    prtycycqty
019100020304     C                   parm                    prtycycode
019200010619     C                   parm                    qry_select
019300010427
019400010619     c                   callb     'PAYCREATE'   run_parm
019500060804      // Set Payroll Prep. flag
019600060804      /FREE
019700060804         prtyflag = 'Y';
019800060804         update prtyfmt;
019900060804         commit;
020000060804         //
020100060804       endsr;
020200060804       //*************************************************
020300060804       // First initilisation routine for all programs  **
020400060804       //*************************************************
020500060804       BEGSR AAINIT;
020600060804         //
020700060804         // Get Company Name
020800060804         coname = get_coname;
020900060804         @DATE = get_sys_date(date);
021000060804      /END-FREE
021100010411     C                   time                    @TIME             6 0
021200060804      // Get system date
021300010426     C                   move      date          curdate
021400060804      // select header for program
021500060804      /FREE
021600060804         @title = arr(1);
021700060804         *in = *off;
021800060807
021900060804       ENDSR;
022000010822
022100060804       //*******************************
022200060804       // Validate Selected Types     **
022300060804       //*******************************
022400060804      /END-FREE
022500010822     Pvalidate_type    b
022600060804      //
022700010822     Dvalidate_type    pi              n
022800010822     Drun_type                        1    value
022900060804      //
023000010822     D error           s               n
023100010901     Ddays_type        s              7  3
023200010901     Ddays_run         s              7  3
023300010901     Ddate_type        s               d   datfmt(*iso)
023400050525     Dnotfound         s               n
023500010901     ddate_run         s               d   datfmt(*iso)
023600060807
023700060804      /FREE
023800060804       setll run_type prdffmt;
023900060804       reade run_type prdffmt;
024000060804       dow not(%eof);
024100010822
024200050525           notfound =  get_payty_info('RD':prdftype:payty_struc);
024300050520
024400060807         // Check for cycle different
024500060804         if not notfound  and ptyclcode <> prtycycode
024600060804               and ptyclcode <> *blanks;
024700060804           chain ptyclcode cycfmt;
024800060804           days_type  = cycyearf;
024900060804           chain prtycycode cycfmt;
025000060804           days_run   = cycyearf * prtycycqty;
025100060807
025200060809           open(e) paytnxtr;
025300060809           chain (run_type:prdftype) pnxrfmt;
025400060807
025500060809           if %found;
025600060809
025700060804           // Check for type date greater than run date
025800060809           if pnxrndate >  prtyndate;
025900060809            //if ptyndate > prtyndate;
026000060807
026100060809               date_type = %date(pnxrndate);
026200060809            //     date_type = %date(ptyndate);
026300060807               date_run = %date(prtyndate) + %days(5);
026400060807
026500060807             // Send error
026600060804             if date_type > date_run;
026700060804               error = *on;
026800060804             endif;
026900060804           endif;
027000060809
027100060804           // Send error
027200060804           if days_run < days_type;
027300060804             error = *on;
027400060804           endif;
027500060804           //
027600060809           ENDIF;
027700060809           close(e) paytnxtr;
027800060809
027900060804         endif;
028000060804         reade run_type prdffmt;
028100060804       enddo;
028200060807
028300060804       return error;
028400170207
028500010822     Pvalidate_type    e
028600971012     **
028700010411**  Program Title
028800010427      Payroll Timesheet Creation
