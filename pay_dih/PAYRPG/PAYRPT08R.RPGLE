000100950726     H DECEDIT('.') DATEDIT(*DMY/)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000300111002     H/define profoundui
000400950726     F** LISTING PAYROLL YEAR TO DATES
000500011003     F** ADJUSTMENTS BY------T.I.BYNOE  *** R.WILLIAMS
000600011003     F**                     C.WILLIAMS   & S.BRITTON
000700011003     Fpayrpt08d cf   e             workstn
000800111002     F/IF DEFINED(WEB)
000900111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001000111002     F/ENDIF
001100021029     Fpaycon    if   e             disk    usropn
001200111002     Femptypel01if   e           k disk    usropn
001300011003     ** Payroll PROTOTYPES
001400110627      /copy QCPYSRC,PAYPR
001500110627      /copy QCPYSRC,genPR
001600110627      /copy QCPYSRC,autPR
001700011003     **
001800021031     D arr             s             40    dim(1) ctdata perrcd(1)
001900021031     d pay_org_txt     s              3    inz('PAY')
002000090220
002100090225     D MSG1            c                   'Invalid criteria Not Allowed'
002200090220     D MSG2            c                   'Report Generation Complete'
002300090220     D MSG3            c                   'Invalid Employee Type -'
002400090220     D TAXTYPE         c                   61
002401160301     D NISTYPE         c                   63
002500011003     **
002600950930     D                SDS
002700950930     D  @PGM             *PROC
002800111002     D  @USER                358    367
002900021029     **
003000021218     D  ytd_list       ds                  export inz
003100021030     D  pcdftytdDS                   10
003200021029     D  p_type1                       2  0 overlay(pcdftytdDS)
003300021029     D  p_type2                       2  0 overlay(pcdftytdDS:3)
003400021029     D  p_type3                       2  0 overlay(pcdftytdDS:5)
003500021029     D  p_type4                       2  0 overlay(pcdftytdDS:7)
003600021029     D  p_type5                       2  0 overlay(pcdftytdDS:9)
003700021029     **
003800021029     Dytd_report       PR                  extproc('PAYRPT08C')
003900140307     D seqfld                         1  0
004000021029     D yrfld                          4  0
004100021029     D queryfld                     200
004200021029     D selfld                         1  0
004300090220     D irdflag                        1
004400021029     **
004500021029     d qry_select      s            200
004600090225     d qry_select#     s             50
004700060413     D run_list        s              1    dim(20) based(lstptr)
004800060412     D error           s               n
004900060413     D emp_class2      s                   like(emp_class)
005000090225     D cnt             s              2  0 inz(0)
005100090225     D cnt2            s              1  0 inz(0)
005200060412     D pos             s              1  0
005300060413     D lstptr          s               *   inz(%addr(list))
005400021029
005500040207      /FREE
005600111002         // authority check
005700111002         monitor;
005800111002           authority_check('PAY':@pgm);
005900111002         on-error;
006000111002           show_msg(get_err_msg('DIH0054'));
006100111002           *inlr = *on;
006200111002           return;
006300111002         endmon;
006400060412
006500021029          exsr  aainit;
006600021031          qry_select = *blanks;
006700021029          dow  0 = 0;
006800021029            exfmt  fmt1;
006900090220
007000090220            // reset error tags
007100090220            clear errmsg;
007200090220            *in28 = '0';
007300011003
007400021029             if  *in03;
007500021029                *inlr = '1';
007600021029                return;
007700021029             endif;
007800021029
007900021029             if  pay_year <= 0;
008000021029                *in28 = '1';
008100021029                errmsg = msg1;
008200021029                iter;
008300021029             endif;
008400021029
008500021029             if  sel = 0;
008600021029                sel = 1;
008700021029             endif;
008800021029
008900021029             if  sel = 1;
009000021029                pcdftytdDS = pcdftytd;
009100021029             endif;
009200021029
009300021029             if  sel = 3;
009400021029                exfmt fmtwin;
009500021029             endif;
009600060412
009700060413             // Validate List
009800060413             if   list <> *blanks and list <> '*ALL'
009900060413                 and list <> '*ALLACTIVE';
010000060413
010100060413                 cnt = 1;
010200060413
010300060413                 dow   run_list(cnt) <> *blank;
010400060413                      get_cat(run_list(cnt):*omit:*omit:*omit
010500060413                                           :error);
010600060413                      if  error;
010700090225                         errmsg = msg1 + ' - ' + run_list(cnt);
010800090225                         reset cnt;
010900060413                         leave;
011000060413                      endif;
011100060413                      cnt += 1;
011200060413                 enddo;
011300060413
011400060413                 // display errmsg
011500060413                 if   errmsg <> *blanks;
011600060413                    iter;
011700060413                 endif;
011800060413                else;
011900060413                 if  list = *blanks;
012000060413                    list = '*ALLACTIVE';
012100060413                 endif;
012200060413             endif;
012300060412
012400060413             // Move type field to array
012500060413             if   list = '*ALLACTIVE';
012600060413               list = *blanks;
012700060413               setll  *loval  empfmt;
012800060413               read   empfmt;
012900060413               dow   not(%eof);
013000060413                   if   empflag = 'A';
013100060413                       cnt  += 1;
013200060413                       run_list(cnt) = emptype;
013300060413                   endif;
013400060413                  read   empfmt;
013500060413               enddo;
013600060413             endif;
013700021029
013800021029             exsr fill_qry_sel;
013900021029            leave;
014000021029          enddo;
014100021029
014200090220         // Special setting for IRD generated report
014300090220         //------------------------------------------
014400090220         if irdflag = 'Y';
014500090220            seq = 2;
014600090220            sel = 3;
014700090220            org = 0;
014800090220            P_type1 = 0;
014900090220            P_type2 = TAXTYPE;
015000160301            P_type3 = NISTYPE;
015100090220            P_type4 = 0;
015200090220            P_type5 = 0;
015300090220         endif;
015400090220
015500090220         ytd_report(seq:pay_year:qry_select:sel:irdflag);
015600021029
015700021029         errmsg = msg2;
015800090225         qry_select = *blanks;
015900090225         reset cnt;
016000090225         reset cnt2;
016100021029
016200021029
016300021029        //***************************************************
016400021029        //** First initilisation routine for all programs  **
016500021029        //***************************************************
016600021029          begsr fill_qry_sel;
016700021031             // Select year
016800021031             //----------------------------
016900021031                 qry_select = %trim(qry_select) +
017000021031                          '  year = ' +
017100021031                                  %char(pay_year);
017200021031             // Select Origin
017300021031             //----------------------------
017400021031              select;
017500021031               when org = 1;
017600021031                 exsr add_and_Sr;
017700021031                 qry_select = %trim(qry_select) +
017800021031                          '  ytdorg = ''' +
017900021031                                     pay_org_txt + '''';
018000021031               when org = 2;
018100021031                 exsr add_and_Sr;
018200021031                 qry_select = %trim(qry_select) +
018300021031                          '  ytdorg *ne ''' +
018400021031                                     pay_org_txt + '''';
018500021031              endsl;
018600021031
018700021031             // If selection using Paytypes
018800021029             //----------------------------
018900021029            if  sel = 1 or sel = 3;
019000021029
019100021031              exsr add_and_Sr;
019200021031                 qry_select = %trim(qry_select) +
019300021031                         '  ytdtype = %values(' + ' ' +
019400021031                                 %char(p_type1) + ' ' +
019500021031                                 %char(p_type2) + ' ' +
019600021031                                 %char(p_type3) + ' ' +
019700021031                                 %char(p_type4) + ' ' +
019800021031                                 %char(p_type5) + ')';
019900021029            endif;
020000060413
020100060413            select;
020200060413             // Employee Class selection
020300060413             //-------------------------
020400060413             when  emp_class <> *blanks;
020500060413                exsr add_and_Sr;
020600060413                if   %scan('P':emp_class) <> 0;
020700060413                    qry_Select = %trim(qry_select) + ' class = ''' +
020800060413                                          emp_class + '''';
020900060413                  else;
021000060413                    pos = %check (' ' : emp_class);
021100060413                    pos -= 1;
021200060413                    emp_class2 = %xlate(' ':'P':emp_class:pos);
021300060413                    qry_select = %trim(qry_select) +
021400060413                         '  class = %values(' + ' ' +
021500060413                         '''' + emp_class + '''' + ' ' +
021600060413                         '''' + emp_class2 + '''' + ')';
021700060413                 endif;
021800090220
021900060413             // Employee type selection
022000060413             //------------------------
022100060413             when  list <> *blanks;
022200060413                 if   list <> '*ALL';
022300060413                     exsr add_and_Sr;
022400060413                     cnt2 = 1;
022500060413                  // setup query select parameter for report run
022600060413                     if  cnt = 1;
022700060413                          qry_select = ' status = ''' +
022800060413                                          run_list(cnt) + '''' ;
022900060413                        else;
023000060413                          qry_select = %trim(qry_select) +
023100060413                                 '  status = %values(' + ' ';
023200060413
023300060413                          cnt2 = 1;
023400060413                          dow   run_list(cnt2) <> *blank;
023500060413                            // setup query select parameter for report run
023600060413
023700060413                            qry_select = %trim(qry_select) +  ' ' +
023800060413                                '''' + run_list(cnt2) + '''';
023900060413                            cnt2 += 1;
024000060413                          enddo;
024100060413                          qry_select = %trim(qry_select) +  ')';
024200060413                     endif;
024300060413                  endif;
024400060412
024500060413            endsl;
024600021029          endsr;
024700021029
024800021029        //***************************************************
024900021029        //** First initilisation routine for all programs  **
025000021029        //***************************************************
025100021029          begsr aainit;
025200021029             @title = arr(1);
025300021029             coname = get_coname;
025400021029             @date = get_sys_date;
025500021029
025600090220          endsr;
025700021029
025800021029        //**********************************
025900021029        //** Add 'AND' to query selection **
026000021029        //**********************************
026100021031          begsr add_and_Sr;
026200021029             if  qry_select <> *blanks;
026300021031               qry_select = %trim(qry_select) + ' *and';
026400021029             endif;
026500021029          endsr;
026600021029
026700021029        //************************************
026800021029        //** start up routine for programs  **
026900021029        //************************************
027000021029          begsr *inzsr;
027100111006            get_session_info('PAY':errmsg:@pgm);
027200111002            if errmsg <> *blanks;
027300111002              show_msg(errmsg);
027400111002              *inlr = *on;
027500111002              return;
027600111002            endif;
027700111002
027800111002            open(e) emptypel01;
027900111002
028000111002            open(e) paycon;
028100021029          read paycon;
028200021029            if not(%eof);
028300021029                pcdftytdDS = pcdftytd;
028400021029            endif;
028500111002          close(e) paycon;
028600021029          endsr;
028700021029      /end-free
028800011003**
028900011003 Payroll Earnings Year-to-Date Report
