000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111002     H/define profoundui
000300010426     Fpayr000d  cf   e             workstn
000400971012     F                                     sfile(sflrec:rrn)
000500111002     F/IF DEFINED(WEB)
000600111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000700111002     F/ENDIF
000800111002     Fpayrtypl01uf   e           k disk    commit  usropn
000900111002     Fpaycycd   if   e           k disk            usropn
001000111002     Fpayrresl01uf a e           k disk    commit  usropn
001100111002     Fpayrdefl03uf a e           k disk    commit  usropn
001200060809     Fpaytnxtr  uf a e           k disk    commit  usropn
001300111002     Fpaytimfl01if   e           k disk            usropn
001400050519     F
001500010508     ** Payroll PROTOTYPES
001600110829      /copy QCPYSRC,genPR
001700110829      /copy QCPYSRC,PAYPR
001800110829      /copy QCPYSRC,autPR
001900110829      /copy QCPYSRC,emppr
002000110427
002100110427     D TaxProgram      PR                  extpgm('PAYTAXCALC')
002200010426     **
002300010815     Ddate             s              8  0
002400010815     D                sds
002500010426     D  @pgm             *proc
002600111002     D  @user                358    367
002700111002
002800010724     D                 ds                  based(indptr)
002900010724     Dcancelled                2      2n
003000010724     Dend_program              3      3n
003100010724     Dconfirmed               15     15n
003200971012     **
003300010411     Darr              s             40    dim(1) ctdata perrcd(1)
003400010426     **
003500010724     Dindptr           s               *   inz(%addr(*in))
003600010426     Dcurdate          s               d   datfmt(*iso)
003700010426     dnxtdate          s               d   datfmt(*iso)
003800010724     dpayprno          s                   like(prtyprno)
003900010724     dpayproll#        s                   like(prtyproll#)
004000010724     dpayldate         s                   like(prtyldate)
004100010724     dpaycycqty        s                   like(prtycycqty)
004200010724     dpaycycode        s                   like(prtycycode)
004300010724     dpayyear          s                   like(prtyyear)
004400111002     derrmsg           s             75
004500010426     dno_days          s              4  0
004600010426     d cnt             s              2  0
004700010426     d qry_select      s            200
004800010426     d type_list       s              1    dim(20)
004900110427     d flag            s              1
005000010507     **
005100010507     d PayrollPgm      c                   const('PAYR000C  ')
005200010720     d text1           c                   'Available'
005300010720     d text2           c                   'Payroll not Prepared'
005400010724     **
005500010724     D msg1            c                   'Invalid Payroll date Entered'
005600010724     D msg2            c                   'Invalid Mimic Run Type Entered'
005700010724     D msg3            c                   'Mimic type cannot be Run type'
005800010724     D msg4            c                   'This mimic type cannot be used for +
005900010724     D                                     Processing'
006000010809     D msg5            c                   'No record found to process payroll'
006100061030     D msg6            c                   'Payroll date already passed'
006200110427     D msg7            c                   'Payroll Job is already active'
006300010228     **
006400010411
006500040207      /FREE
006600111002         // authority check
006700111002         monitor;
006800111002           authority_check('PAY':@pgm);
006900111002         on-error;
007000111002           show_msg(get_err_msg('DIH0054'));
007100111002           *inlr = *on;
007200111002           return;
007300111002         endmon;
007400060804        // Initial Setup
007500060804       exsr aainit;
007600111002
007700060804       //************************
007800060804       //* Start of Program   ***
007900060804       //************************
008000060804       dow 0 = 0;
008100060804         //
008200060804         *IN33 = *ON;
008300060804         sel = *blank;
008400060804         rrn = *zeros;
008500060804
008600060804         write sflctl;
008700060804         *IN33 = *OFF;
008800060804         //
008900060804         setll *loval prtyfmt;
009000060804         read(n) prtyfmt;
009100060804         *IN68 = %EOF;
009200060804         dow not(*in68);
009300060804           //  Select transactions
009400060804           if prtyndate > *zeros;
009500060804      /END-FREE
009600010426     C                   move      prtyndate     nxtdate
009700010430     C     nxtdate       subdur    curdate       no_days:*d
009800060804      //
009900060804      /FREE
010000060804             if no_days <= prtyavail
010100080123                   or nxtdate < curdate or
010200080123                   prtyptype = 'S';
010300060804               *in66 = *off;
010400060804             else;
010500060804               *in66 = *on;
010600060804               //
010700060804             endif;
010800060804             //
010900060804           else;
011000060804             //
011100060804             *in66 = *off;
011200060804           endif;
011300010426
011400060804           // Handling Screen Text
011500060804           if not(*in66);
011600060804             if prtyflag = 'Y';
011700060804               *in99 = *on;
011800060804               dsptext = text1;
011900060804             else;
012000060804               *in99 = *off;
012100060804               dsptext = text2;
012200060804             endif;
012300060804           else;
012400060804             dsptext = *blanks;
012500060804           endif;
012600110428
012700060804           prtyptype# = prtyptype;
012800060804           prtyprno# = prtyprno;
012900060804           prtyprol## = prtyproll#;
013000060804      /END-FREE
013100971012     c                   add       1             rrn               4 0
013200060804      /FREE
013300060804           write sflrec;
013400110428
013500060804           read(n) prtyfmt;
013600060804           *IN68 = %EOF;
013700060804         enddo;
013800110428
013900060804         leave;
014000060804       enddo;
014100060804       //************************
014200060804       //* Body of Program    ***
014300060804       //************************
014400060804       write footer;
014500060804       dow 0 = 0;
014600060804         exfmt sflctl;
014700060804         //  End Program
014800060804         if end_program;
014900060804           *inlr = *on;
015000060804           return;
015100060804         endif;
015200060804         //
015300060804         // IF RECORD WAS CHANGED REDISPLAY FORMAT
015400060804         if *in88;
015500060804           iter;
015600060804         endif;
015700060804         //  select payroll stuff
015800060804         exsr run_rtn;
015900010411
016000060804         leave;
016100060804       enddo;
016200111002
016300111002         //*****************************
016400111002         // Session Init
016500111002         //*****************************
016600111002
016700111002          begsr *inzsr;
016800111006            get_session_info('PAY':errmsg:@pgm);
016900111002            if errmsg <> *blanks;
017000111002              show_msg(errmsg);
017100111002              *inlr = *on;
017200111002              return;
017300111002            endif;
017400111002
017500111002            open(e) payrtypl01;
017600111002            open(e) payrdefl03;
017700111002            open(e) payrresl01;
017800111002            open(e) paytimfl01;
017900111002            open(e) paycycd;
018000111002
018100111002          endsr;
018200111002
018300111002
018400060804       //****************************************************************
018500060804       //*  Select Payrolls to Run
018600060804       //****************************************************************
018700060804       //
018800060804       begsr run_rtn;
018900060804         readc sflrec;
019000060804         *IN77 = %EOF;
019100060804         dow not(*in77);
019200060804           if sel = 'X';
019300010724
019400060804             // set payroll date  and confirm run
019500010724
019600060804             exsr set_paydate;
019700010724
019800060804           endif;
019900060804           readc sflrec;
020000060804           *IN77 = %EOF;
020100060804         enddo;
020200060804       endsr;
020300010724
020400060804       //**********************
020500060804       // set payroll date   **
020600060804       //**********************
020700060804       begsr set_paydate;
020800060804         // for MIMIC types
020900060804         if prtyprno# = 0;
021000060804           *in98 = *on;
021100060804         else;
021200060804           *in98 = *off;
021300060804           run_type = *blank;
021400060804         endif;
021500060804         //
021600060804         dow 0 = 0;
021700110427
021800060804           exfmt fmt1;
021900060804           clear errmsg1;
022000060804           // go back
022100060804           if cancelled;
022200060804             leave;
022300060804           endif;
022400110427
022500110428           //chk_activeJob(@pgm:flag);
022600110428           //if flag = 'Y';
022700110428           //   errmsg1 = msg7;
022800110428           //  iter;
022900110428           //endif;
023000110427
023100060804           chain prtyptype# timfmt;
023200060804           if not(%found);
023300060804             errmsg1 = msg5;
023400060804             iter;
023500060804           else;
023600060804             if timcode <> 'E';
023700060804               dou %eof(paytimfl01) or timcode = 'E';
023800060804                 reade prtyptype# timfmt;
023900060804               enddo;
024000060804               //
024100060804               if %eof(paytimfl01);
024200060804                 errmsg1 = msg5;
024300060804                 iter;
024400060804               endif;
024500060804             endif;
024600060804             //
024700060804           endif;
024800061030
024900060804           //  check for valid date
025000060804           test(de) *iso pay_date;
025100060804           if %error;
025200060804             errmsg1 = msg1;
025300060804             iter;
025400060804           endif;
025500061030
025600061030           // check that date is GREATER THAN current date
025700061030           if %date(pay_date) < %date();
025800061030             errmsg1 = msg6;
025900061030             iter;
026000061030           endif;
026100010724
026200060804           //*****************************
026300060804           // SPECIAL Run type entered  **
026400060804           //*****************************
026500060804           if prtyprno# = 0;
026600060804             //
026700060804             chain run_type prtyfmt;
026800060804             // Invalid Run type entered
026900060804             if not(%found);
027000060804               errmsg1 = msg2;
027100060804               iter;
027200060804             endif;
027300060804             // mimic = runtype
027400060804             if run_type = prtyptype#;
027500060804               errmsg1 = msg3;
027600060804               iter;
027700060804             endif;
027800060804             // Mimic cannot used for processing
027900060804             if prtyprno = 0;
028000060804               errmsg1 = msg4;
028100060804               iter;
028200060804             endif;
028300060804           endif;
028400060804           // confirm pay run
028500060804           if confirmed;
028600060804             exsr confirm;
028700060804             commit;
028800060804             leave;
028900060804           endif;
029000110428
029100060804         enddo;
029200010724
029300060804       endsr;
029400010724
029500060804       //***********************
029600060804       // Confirm Payroll Run **
029700060804       //***********************
029800060804       begsr confirm;
029900060804         if prtyprno# = 0;
030000060804           // Position restriction /default records
030100060804           setll run_type prrefmt;
030200060804           reade(n) run_type prrefmt;
030300060804           //
030400060804           setll run_type prdffmt;
030500060804           reade(n) run_type prdffmt;
030600060804           // write temporary Run restriction record to file
030700060804           dow not(%eof(payrresl01));
030800060804             prreptype = prtyptype#;
030900060804             write prrefmt;
031000060804             reade(n) run_type prrefmt;
031100060804           enddo;
031200060804           // write temporary Run type defaults record to file
031300060804           dow not(%eof(payrdefl03));
031400060804             prdfptype = prtyptype#;
031500060804             write prdffmt;
031600060804             reade(n) run_type prdffmt;
031700060804           enddo;
031800060804           // update  Run type master record
031900060804           chain(n) run_type prtyfmt;
032000060804           // save a few fields
032100060804           eval payprno   = prtyprno;
032200060804           eval payproll# = prtyproll#;
032300060804           eval paycycqty = prtycycqty;
032400060804           eval paycycode = prtycycode;
032500060804           eval payldate  = prtyldate;
032600060804           eval payyear   = prtyyear;
032700060804           chain prtyptype# prtyfmt;
032800060804           eval prtyproll# = payproll# - 1;
032900060804           eval prtyyear   = payyear;
033000060804           eval prtycycqty = paycycqty;
033100060804           eval prtycycode = paycycode;
033200060804           eval prtyndate = payldate;
033300060804           //  for start of year
033400060804           if prtyproll# = 0;
033500070315             //prtyproll# = payprno;
033600070315               prtyyear = payyear - 1;
033700060804             if payprno <> 1;
033800060804               //***********       eval       prtyyear = payyear - 1
033900060804             endif;
034000060804           endif;
034100070315
034200060804           update prtyfmt;
034300060804         else;
034400060804           if prtyprno# = prtyprol##;
034500060804             exsr New_Year_Sr;
034600060804           endif;
034700060804         endif;
034800010724
034900110428         // ensure Tax program is closed
035000110428         // before payroll is invoked
035100110428          taxProgram();
035200110428
035300060804         // Setup Payroll being RUN
035400060804         exsr paysetup;
035500110428
035600060804         // Do Final Processing
035700060804         exsr payclose;
035800110428
035900060804       endsr;
036000010725
036100060804       //****************************************
036200060804       // Setup  Pay periods for new year      **
036300060804       //****************************************
036400060804       begsr New_Year_Sr;
036500060804         clear yesfield;
036600060804         dow yesfield <> 'YES' and yesfield <> 'NO ';
036700060804           exfmt fmt2;
036800060804           if yesfield = 'NO ';
036900060804             chain prtyptype# prtyfmt;
037000060804             prtyproll# = prtyproll# - 1;
037100060804             update prtyfmt;
037200060804           endif;
037300060804         enddo;
037400060804       endsr;
037500060804       //*************************************************
037600060804       // Setup Payroll Parameters for Processing       **
037700060804       //*************************************************
037800060804       begsr paysetup;
037900060804         chain prtyptype# prrefmt;
038000060804         // Move type field to array
038100060804      /END-FREE
038200010725     C                   movea     prrelist      type_list
038300060804      //
038400060804      /FREE
038500060804         if %found;
038600060804           select;
038700060804           when prreflag = 'Y';
038800060804             cnt = 1;
038900060804             dou type_list(cnt)= *blank;
039000060804               // setup query select parameter for payroll run
039100060804               qry_select = ' status = ''' +
039200060804                   type_list(cnt) + '''';
039300010725
039400060804      /END-FREE
039500010725     C                   callb     Payrollpgm
039600010725     C                   parm                    qry_select
039700010725     C                   parm                    prtyptype#
039800010725     C                   parm                    pay_date
039900010809     C                   parm                    run_type
040000010918     C                   parm                    prreseqcod
040100011126     C                   parm                    prrertflag
040200021024     C                   parm                    prrepstyle
040300021024     C                   parm                    prreytdopt
040400060804      /FREE
040500060804               cnt = cnt + 1;
040600060804             enddo;
040700010725
040800060804           other;
040900010725
041000060804             cnt = 1;
041100060804             // setup query select parameter for payroll run
041200060804             qry_select = ' status = ''' +
041300060804                 type_list(cnt) + '''';
041400060807             cnt += 1;
041500060804             dow type_list(cnt) <> *blank;
041600060804               // setup query select parameter for payroll run
041700060804               //
041800060804               qry_select = %trim(qry_select) +
041900060804                   ' *or status = ''' +
042000060804                   type_list(cnt) + '''';
042100060804               cnt = cnt + 1;
042200060804             enddo;
042300060804             //
042400060804      /END-FREE
042500010725     C                   callb     Payrollpgm
042600010725     C                   parm                    qry_select
042700010725     C                   parm                    prtyptype#
042800010725     C                   parm                    pay_date
042900010809     C                   parm                    run_type
043000010918     C                   parm                    prreseqcod
043100011126     C                   parm                    prrertflag
043200021024     C                   parm                    prrepstyle
043300021024     C                   parm                    prreytdopt
043400060804      /FREE
043500060804           endsl;
043600010725
043700060804         endif;
043800060804       endsr;
043900010724
044000060804       //********************************
044100060804       // DO final payroll processing  **
044200060804       //********************************
044300060804       begsr payclose;
044400010724
044500060804         chain prtyptype# prtyfmt;
044600060804         chain prtycycode cycfmt;
044700060804         //
044800060804         if prtyprno# = 0;
044900060804           // delete temporary Run restriction record to file
045000060804           setll prtyptype# prrefmt;
045100060804           reade prtyptype# prrefmt;
045200060804           dow not(%eof(payrresl01));
045300060804             delete prrefmt;
045400060804             reade prtyptype# prrefmt;
045500060804           enddo;
045600060804           // delete temporary Run type defaults record to file
045700060804           setll prtyptype# prdffmt;
045800060804           reade prtyptype# prdffmt;
045900060804           dow not(%eof(payrdefl03));
046000060804             delete prdffmt;
046100060804             reade prtyptype# prdffmt;
046200060804           enddo;
046300060804           // Reset temp run type fields
046400060804           eval prtyproll# = *zeros;
046500060804           eval prtycycqty = *zeros;
046600060804           eval prtycycode = *blanks;
046700060804           eval prtyyear   = *zeros;
046800060804           eval prtyldate = prtyndate;
046900110428
047000060804         else;
047100060804           //  Calculate Payperiod next date & move next date to last date********
047200060804           if prtyprno = prtyproll#;
047300060804             prtyproll# = 0;
047400060804             prtyyear = prtyyear + 1;
047500060804           endif;
047600060804           prtyproll# = prtyproll# + 1;
047700060804           prtyldate = prtyndate;
047800060804           prtyndate = get_next_date(prtyndate:
047900060804               prtycycode:prtycycqty:cycccode:
048000060804               cycconvf);
048100060804           //  Reset Payroll flag for "REGULAR " payroll types ***
048200060804           prtyflag = *blank;
048300060804         endif;
048400060804
048500060804         update prtyfmt;
048600140408
048700110427         // close Tax program
048800110427          taxProgram();
048900110427
049000110427         // set next payroll date
049100060804         exsr set_next_date;
049200140408
049300140408         // clear employee records
049400140408
049500140408     C                   callb     'PAYCALCCLR'
049600140408     C                   parm                    prtyptype#
049700140408     C                   parm                    run_type
049800140408
049900060804       endsr;
050000060804       //************************************************
050100060804       // Set next date for paytypes with OWN cycle   **
050200060804       //************************************************
050300060804       begsr set_next_date;
050400060804         setll prtyptype# prdffmt;
050500060804         reade prtyptype# prdffmt;
050600060804         dow not(%eof);
050700010901
050800050519         if not  get_payty_info('RL':prdftype:payty_struc)
050900050519                       and ptyclcode <> prtycycode
051000050519                                 and ptyclcode <> *blanks;
051100060807           // Check for cycle different
051200060804
051300140408         //chain prtycycode cycfmt;
051400140408           chain ptyclcode cycfmt;
051500060804           ptyldate = ptyndate;
051600060804           ptyndate = get_next_date(prtyldate:
051700060809                         ptyclcode:1:cycccode:cycconvf);
051800050519               get_payty_info('UP':prdftype:payty_struc);
051900060809
052000060809           // Update Individual Payroll run types for next run date
052100060809           open(e) paytnxtr;
052200060809           chain (prtyptype#:prdftype) pnxrfmt;
052300060809
052400060809            pnxrldate = ptyldate;
052500060809            pnxrndate = ptyndate;
052600060809           if %found;
052700060809
052800060809               // Update ONLY if a NEXT date if calculated  else Delete record
052900060809               if  pnxrndate <> *zeros;
053000060809                 update pnxrfmt %fields(pnxrldate:pnxrndate);
053100060809                else;
053200060809                 delete pnxrfmt;
053300060809               endif;
053400060809              else;
053500060809               // write ONLY if a NEXT date if calculated
053600060809               if  pnxrndate <> *zeros;
053700060809                 pnxrruntyp = prtyptype#;
053800060809                 pnxrptype = prdftype;
053900060809                 write pnxrfmt;
054000060809               endif;
054100060809           endif;
054200060809           close(e) paytnxtr;
054300060809
054400060804         endif;
054500060804         reade prtyptype# prdffmt;
054600060804       enddo;
054700060804       endsr;
054800060804       //*************************************************
054900060804       // First initilisation routine for all programs  **
055000060804       //*************************************************
055100060804       BEGSR AAINIT;
055200060804         //
055300060804         // Get Company Name
055400060804         CONAME = get_coname;
055500060804         @DATE = get_sys_date(date);
055600060804      /END-FREE
055700010411     C                   time                    @TIME             6 0
055800060804      // Get system date
055900010426     C                   move      date          curdate
056000060804      // select header for program
056100060804      /FREE
056200060804         @title = arr(1);
056300060804         *in = *off;
056400060807
056500060804       ENDSR;
056600060804      /END-FREE
056700971012     **
056800010411**  Program Title
056900010720           Payroll Processing
