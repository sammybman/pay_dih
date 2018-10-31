000100081212     H DECEDIT('.') DATEDIT(*ymd/)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000300081212     H*DECEDIT('.') DATEDIT(*ymd/) main(electronic_transfer)
000400100224     *********************************************************
000500100224     ** changed to treat citi bank files like repub Bank file
000600100224     ** by setting it up to be sent by email in excel format
000700100224     ** Dated 2010/02/24
000800100224     *********************************************************
000900100224     ** ELECTRONIC DATA TRANSFER
001000100224     F* PROGRAMMER:- *** S.BRITTON
001100100224     F* PROGRAMMER:- *** C.Williams (modified by)
001200030315     Fpayr010d  cf   e             workstn
001300030317     f                                     sfile(winrec:rrn)
001400030317     Fpayelctrn uf a e           k disk
001500030315     Fqsysprt   o    f  132        printer
001600030315     **
001700030315      /copy genPR
001800081204      /copy progbarpr
001900040207      /copy autPR
002000030315     **
002100081212     Delectronic_transfer...
002200081212     D                 PR
002300091019
002400090108     DmaintTransitData...
002500090129     D                 PR                  extproc('PAYR010C')
002600081212
002700090120      // financial institution config
002800081212     DFI_setup         PR
002900081212
003000081212      // transfer preparation routine
003100090120     Dtransfer_rtn     PR              n
003200081212
003300081212      // Print error list  routine
003400081212     DPrint_error_list...
003500081212     D                 PR
003600081212
003700081212      // Fix error
003800081212     DFix_error        PR
003900081212
004000081212      // Close routine
004100081212     DClose_rtn        PR
004200081212
004300081212      // Validation display
004400081212     Dvalidation_dsp   PR
004500081212
004600081212      // Progress display
004700081212     Dprogress_dsp     PR
004800081212
004900030315     Dget_tot_records  PR                  extpgm('CITI01R')
005000030315     D                                2  0
005100030315     D                                6  0
005200091010     D                                3    const
005300030315     **
005400090106     Dopen_comm        PR                  extpgm(pgmname3)
005500081212
005600090106     Dclose_comm       PR                  extpgm(pgmname4)
005700081212
005800091012     Dvalidate_data    PR                  extpgm(pgmname1)
005900091012     D   citi_errDS                        likeds(citi_err) dim(100)
006000030315     D                                6  0
006100030315     D                                 n
006200030315     D                                6  0
006300030315     D                                2  0
006400091010     D                                3    const
006500030315     Dcorrect_error    PR                  extpgm('CITI03R')
006600030315     D                                2  0
006700090618     D                               30
006800090618     D                               30
006900091012     D                                3    const
007000090105
007100090105     Dprocess_data     PR                  extpgm(pgmname2)
007200030315     D                                2  0
007300030315     D                                6  0
007400030315     D                                 n
007500030315     D                                6  0
007600030315     D                                1
007700030318     D                                6  0
007800030318     D                                6  0
007900030318     D                               25
008000091010     D                                3    const
008100030315     **
008200030317     D                 ds                  inz
008300030317     D  divtypeDS                    10  0
008400030317     D  div_list                      2  0 dim(5) overlay(divtypeDS)
008500030318     **
008600030318     D                 ds
008700030318     D  accountDS                    25
008800030318     D  citiacc                       1    dim(9) overlay(accountDS)
008900030315     **
009000091012     Dciti_err         ds                  dim(100) qualified
009100030315     D e_payemp                       6  0
009200090618     D e_payacc                      30
009300030315     D e_paycrd                       6  0
009400030315     D e_payname                     26
009500081212
009600030315     **
009700030315     Deof              s               n
009800030317     Drrn              s              4  0
009900030315     Dcount            s              6  0
010000030315     Derr_count        s              6  0
010100030317     Dconame_lines     s             30    inz(*all'=')
010200090105     Dpgmname1         s             10
010300090105     Dpgmname2         s             10
010400090106     Dpgmname3         s             10
010500090106     Dpgmname4         s             10
010600030315     **
010700980103     D arr             s             40    dim(2) ctdata perrcd(1)
010800030315     D err_ind         s              1
010900030315     D bnk_type        s              2  0
011000030315     D bnk_rrn         s              6  0
011100060626     D TimeStart       S               T
011200060626     D TimeNow         S               T
011300060626     D TimeHMS         S               T   TimFmt(*HMS)
011400060626     D TotalSecs       S              9S 0
011500091012     D hold_code       s              2
011600081204
011700081204     D proper          s              4  1
011800081204     D valper          s              4  1
011900081204     D pcta            s              5
012000081204     D progBar1Obj     S                   like(createProgressBar)
012100081204     D progBar2Obj     S                   like(createProgressBar)
012200081212     D progBar1        DS                  likeDS(prgBars) based(progBar1Obj)
012300081212     D progBar2        DS                  likeDS(prgBars) based(progBar2Obj)
012400030315     **
012500090105     D PGM1CITI        c                   'CITI02R'
012600100212     D PGM1CITI1       c                   'CB0001R'
012700090105     D PGM1RBG         c                   'RB0001R'
012800090105     D PGM2CITI        c                   'CITI04R'
012900100212     D PGM2CITI2       c                   'CB0002R'
013000090105     D PGM2RBG         c                   'RB0002R'
013100090106     D PGM3CITI        c                   'CITI00C'
013200100212     D PGM3CITI3       c                   'CB0000C'
013300090106     D PGM3RBG         c                   'RB0000C'
013400090106     D PGM4CITI        c                   'CITI01C'
013500100212     D PGM4CITI4       c                   'CB0001C'
013600090106     D PGM4RBG         c                   'RB0001C'
013700081211     D COMPLETETEXT    c                   'COMPLETED'
013800081211     D FULPERCENT      c                   100
013900090120     d DIGITS          c                   '0123456789'
014000030318     **
014100030315     D msg1            c                   'Preparing to transmit data'
014200030315     D msg2            c                   'Opening communication line ....'
014300030315     D msg3            c                   'Validating data ...............'
014400030315     D msg4            c                   'Processing data ...............'
014500031217     D msg5            c                   'Closing communication line : PRESS -
014600030411     D                                      ENTER to continue'
014700030315     D msg6            c                   'Error found while processing...'
014800030315     D msg7            c                   'No batch available for transfer'
014900030317     D msg8            c                   'Invalid selection or Parameters not-
015000030317     D                                      configured'
015100030317     D msg9            c                   'No Transaction Found for transfer'
015200030318     D msg10           c                   'Invalid Payment division'
015300090107     D msg11           c                   'Batch range setup incorrectly'
015400090107     D msg12           c                   'Function Key not valid on selected -
015500090107     D                                     field'
015600980103     **
015700980103     D                sds
015800980103     D  @pgm             *proc
015900980103     D  @user                254    263
016000030315
016100030315      /free
016200030315
016300100215           //authority_check('PAY':@pgm);
016400081212          // Initial Setup
016500081212           exsr  aainit;
016600030315
016700081212          // Main routine
016800081212         electronic_transfer();
016900030315
017000030315        //***************************************************
017100030315        //** First initilisation routine for all programs  **
017200030315        //***************************************************
017300030315         begsr aainit;
017400030315
017500081212           // Get Company Name
017600081212           coname = get_coname;
017700081212           @DATE = get_sys_date();
017800081212           // select header for program
017900081212           @title = arr(1);
018000030315
018100030315         endsr;
018200030318
018300030315      /end-free
018400030317     **
018500030317     Oqsysprt   E            HEDR           1 04
018600030317     O                       @pgm                10
018700030317     O                       coname              75
018800030317     O                       @date              128
018900030317     O          E            hedr           1
019000030317     O                       @user               10
019100030317     O                       coname_lines        75
019200030317     O          E            hedr           1
019300030317     O                       @title              80
019400030317     O                                          123 'PAGE:'
019500030317     O                       PAGE               128
019600030317     O          E            hedr           1
019700030317     O                                           11 'Employee'
019800030317     O                                           50 'Account No.'
019900030317     O                                           65 'Credit Date'
020000030317     **
020100030317     O          E            detail         1
020200030317     O                       payemp        z      8
020300030317     O                       payname           +  1
020400030317     O                       payacc              50
020500030317     O                       paycrd        y     65
020600030317     O          E            final_prt   2  2
020700030317     O                                           75 'End of Report'
020800081212
020900081212       //************************************                               **
021000081212       // Electric Transfer Main routine
021100081212       //************************************                               **
021200081212     P Electronic_transfer...
021300081212     P                 B
021400081212     D Electronic_transfer...
021500081212     D                 PI
021600090120
021700090120     D completeInd     s               n
021800081212      /free
021900081212        //**************************
022000081212        //*** Start of Program   ***
022100081212        //**************************
022200081212          dow  0 = 0;
022300081212
022400081212           exfmt  fmt1;
022500081212
022600081212           // Terminate program
022700081212            if  *in03;
022800081212               *inlr = *on;
022900081212               return;
023000081212            endif;
023100081212
023200081212            select;
023300091019
023400091019             // Setup transfer parameters
023500081212              when  option = 1;
023600081212                 dow 0 = 0;
023700081212                   exfmt fmt2;
023800081212                   errmsg = *blanks;
023900081212                   if *in02;
024000081212                     leave;
024100081212                   endif;
024200081212
024300081212                   clear pelcfmt;
024400081212                  chain banksel pelcfmt;
024500081212                   select;
024600081212                    when banksel = 1;
024700081212                     FI_setup();
024800081212                    when banksel = 2;
024900081212                     FI_setup();
025000090805                    when banksel = 5;
025100090805                     FI_setup();
025200081212                   endsl;
025300081212                 enddo;
025400081212
025500081212           // Processing transactions (electronic transfer)
025600081212              when  option = 2;
025700081212                 dow 0 = 0;
025800081212
025900081212                   exfmt fmt2;
026000081212                   errmsg = *blanks;
026100081212                   errmsg2 = *blanks;
026200081212                   errmsg3 = *blanks;
026300081212                   errmsg4 = *blanks;
026400081212
026500090119                  // Go back
026600090120                   if *in02 or
026700090120                       completeInd;
026800081212                     leave;
026900081212                   endif;
027000090119
027100090119                  // Terminate program
027200090119                   if *in03;
027300090119                     leave;
027400090119                   endif;
027500081212
027600081212                   chain banksel pelcfmt;
027700081212                   if not(%found);
027800081212                     errmsg = msg8;
027900081212                     iter;
028000081212                   endif;
028100090105
028200090105                   // select relavant programs for processing
028300090105                   select;
028400090105                     when banksel = 1;
028500100212                       pgmname1 = PGM1CITI1;
028600100212                       pgmname2 = PGM2CITI2;
028700100212                       pgmname3 = PGM3CITI3;
028800100212                       pgmname4 = PGM4CITI4;
028900090105                     when banksel = 2;
029000090105                       pgmname1 = PGM1RBG;
029100090105                       pgmname2 = PGM2RBG;
029200090106                       pgmname3 = PGM3RBG;
029300090106                       pgmname4 = PGM4RBG;
029400090105                   endsl;
029500081212
029600081212                 leave;
029700081212                 enddo;
029800081212
029900081212                 divtypeDs = pediv;
030000081212                 for count = 1 to %elem(div_list);
030100081212                    if div_list(count) <> *zeros;
030200081212                      bnk_type = div_list(count);
030300090120                      if transfer_rtn();
030400090120                         completeInd = *on;
030500090120                      endif;
030600081212                    endif;
030700081212                 endfor;
030800090119                 banksel = *zeros;
030900081212              other;
031000081212               errmsg = get_err_msg('DIH0026');
031100081212                 iter;
031200081212            endsl;
031300081212
031400081212           enddo;
031500081212      /end-free
031600081212     P Electronic_transfer...
031700081212     P                 E
031800081212
031900081212
032000081212       //************************************                               **
032100081212       // Transfer/preparation routine
032200081212       //************************************                               **
032300081212     P Transfer_rtn    B
032400090120     D Transfer_rtn    PI              n
032500081212
032600081212      /free
032700091014           write fmt4;
032800081212           progBar1Obj = createProgressBar();
032900081212           if progBar1Obj <> *Null;
033000091014             progBar1.WindowTop = 6;
033100081212             progBar1.WindowTitle = 'Validate Transactions';
033200081212             progBar1.Color = COLOR_BLUE;
033300081212             progBar1.ShowPercent = *ON;
033400081212           endif;
033500081212
033600081212           valper = *zeros;
033700081212           proper = *zeros;
033800081212           errmsg = *blanks;
033900081212           errmsg2 = *blanks;
034000081212           errmsg3 = *blanks;
034100081212           errmsg4 = *blanks;
034200081212
034300081212          // get total records
034400081212           errmsg = msg1;
034500091014           write fmt5;
034600091010           get_tot_records(bnk_type:bnk_rrn:'PAY');
034700081212            if bnk_rrn = *zeros;
034800081212               errmsg = msg9;
034900090120               return *off;
035000081212            endif;
035100081212
035200081212          // Open communications
035300081212          //---------------------
035400081212           %subst(errmsg:33:9) = COMPLETETEXT;
035500081212           errmsg2 = msg2;
035600091014           write fmt5;
035700081212           open_comm();
035800081212
035900081212          // Validate data
036000081212          //--------------
036100081212           %subst(errmsg2:33:9) = COMPLETETEXT;
036200081212           errmsg3 = msg3;
036300091014           write fmt5;
036400081212           eof = *off;
036500081212           TimeStart = %time();
036600100215           count = 0;
036700100215         // for count = 0 to 1;// bnk_rrn;
036800081212             validate_data(citi_err:err_count:
036900091010                             eof:count:bnk_type:'PAY');
037000100215             // if  eof;
037100100215             //  dow count <= bnk_rrn;
037200100215             //    Validation_dsp();
037300100215             //    count += 1;
037400100215             //  enddo;
037500100215           //    leave;
037600100215             // endif;
037700081212
037800081212               Validation_dsp();
037900100215         //endfor;
038000081212
038100081212           // Fix data error
038200081212           //---------------
038300081212         if err_count > *zeros;
038400081212            close_comm();
038500081212            fix_error();
038600090120            return *off;
038700081212         endif;
038800081212
038900081212          // Process data
039000081212          //-------------
039100081212        progBar2Obj = createProgressBar();
039200081212        if progBar2Obj <> *Null;
039300091014          progBar2.WindowTop = 14;
039400081212          progBar2.WindowTitle = 'Process Transactions';
039500081212          progBar2.Color = COLOR_BLUE;
039600081212          progBar2.ShowPercent = *ON;
039700081212        endif;
039800081212
039900081212           %subst(errmsg3:33:9) = COMPLETETEXT;
040000090119           errmsg4 = msg4;
040100091014           write fmt5;
040200081212           eof = *off;
040300081212           err_ind = *blank;
040400081212           TimeStart = %time();
040500081212           for count = 1 to bnk_rrn;
040600081212             process_data(bnk_type:count:eof:
040700081212                       bnk_rrn:err_ind:pelower:peupper:
040800091010                              peacct:'PAY');
040900081212              if eof;
041000081212               errmsg = msg5;
041100081212               close_rtn();
041200090120               return *on;
041300081212              endif;
041400081212
041500081212              if err_ind <> *blank;
041600081212               select;
041700081212                 when err_ind = '1';
041800081212                  errmsg = msg6;
041900081212                 when err_ind = '2';
042000081212                  errmsg = msg7;
042100081212               endsl;
042200081212               close_rtn();
042300090120               return *off;
042400081212              endif;
042500081212
042600081212             progress_dsp();
042700081212         endfor;
042800081212
042900081212         %subst(errmsg4:33:9) = COMPLETETEXT;
043000091014         write fmt5;
043100081212         close_rtn();
043200090120         return *on;
043300081212
043400081212      /end-free
043500081212     P Transfer_rtn    E
043600081212
043700081212       //************************************                               **
043800081212       // Financial Institution Config
043900081212       //************************************                               **
044000081212     P FI_setup        B
044100081212     D FI_setup        PI
044200081212
044300081212      /free
044400091019
044500090107          // prepare screen fields
044600090107          pesubject# = pesubject;
044700090107          petext# = petext;
044800090107          pefilenm# = pefilenm;
044900090107          pemfrom# = pemfrom;
045000090108          pechgamt# = pechgamt;
045100090107          pevpgmid = %editc(banksel:'X') +
045200090107                     %subst(@pgm:3);
045300090107
045400081212          dow  0 = 0;
045500091019
045600081212            exfmt fmt3;
045700081212            errmsg = *blanks;
045800081212
045900081212            if *in02;
046000081212              return;
046100081212            endif;
046200090106
046300090107            // Prompt for list/maintenance program
046400090107            if  *in04;
046500090107               select;
046600090107                 // Transit number range Maintenance
046700090107                 when fld = 'PEDFTTRN#';
046800090108                   maintTransitData();
046900090107                 // ALL other selections
047000090107                 other;
047100090107                   errmsg = msg12;
047200090107               endsl;
047300090107               iter;
047400090107            endif;
047500090107
047600090107            // F7 = Create/Modify Mailing List
047700090106            if *in07;
047800100215
047900090107              email_list('PAY':pevpgmid);
048000090106              iter;
048100090106            endif;
048200081212
048300081212            // Validate account number;
048400081212                accountDS = peacct;
048500081212               for count = 1 to %elem(citiacc);
048600090107               if  %scan(citiacc(count):digits) = 0;
048700090107               //   errmsg = get_err_msg('DIH0059');
048800090107                  leave;
048900081212               endif;
049000081212               endfor;
049100081212               if  errmsg <> *blanks;
049200081212                 iter;
049300081212               endif;
049400090107
049500081212            // Validate batch range;
049600090107            if peupper < pelower
049700081212                or peupper = *zeros
049800081212                or pelower = *zeros;
049900081212                    errmsg = msg11;
050000081212                    iter;
050100090107            endif;
050200081212
050300081212            divtypeDS = pediv;
050400081212            // Validate configuration;
050500081212              for count = 1 to %elem(div_list);
050600081212                 if div_list(count) <> *zeros;
050700091012                   bnk_type = div_list(count);
050800091012
050900091012                   exec sql
051000091012                     select dtno into :hold_code from paydiv
051100091012                      where dtno = :bnk_type and dtrsts = 'A';
051200091012                   if sqlstt = '02000';
051300091012                      errmsg = msg10;
051400091012                      iter;
051500091012                   endif;
051600081212                 endif;
051700081212               endfor;
051800081212
051900081212            // write configuration;
052000090107                 // prepare database fields
052100090107                 pesubject = %trim(pesubject#);
052200090107                 petext = %trim(petext#);
052300090107                 pefilenm = %trim(pefilenm#);
052400090107                 pemfrom = %trim(pemfrom#);
052500090108                 pechgamt = pechgamt#;
052600090107
052700081212                 if not(%found(payelctrn));
052800081212                  petype = banksel;
052900081212                  write pelcfmt;
053000081212                   else;
053100081212                  update pelcfmt;
053200081212                 endif;
053300081212
053400081212             return;
053500081212          enddo;
053600081212
053700081212      /end-free
053800081212     P FI_setup        E
053900081212
054000081212       //************                                                       **
054100081212       // Fix error
054200081212       //************                                                       **
054300081212     P Fix_error       B
054400081212     D Fix_error       PI
054500081212      /free
054600081212
054700081212            *in33 = *on;
054800081212              write  window;
054900081212            write  wdwctl;
055000081212            *in33 = *off;
055100081212
055200081212
055300081212            // load subfile
055400081212            for rrn = 1 to err_count;
055500091012                   payemp = citi_err(rrn).e_payemp;
055600091012                   paycrd = citi_err(rrn).e_paycrd;
055700091012                   payacc = citi_err(rrn).e_payacc;
055800091012                   oldpayacc = citi_err(rrn).e_payacc;
055900091012                   payname = citi_err(rrn).e_payname;
056000081212                   write   winrec;
056100081212            endfor;
056200081212
056300081212            dow  0 = 0;
056400081212              exfmt  wdwctl;
056500081212
056600081212            // F3 = End Program
056700081212              if  *in03;
056800081212                 *inlr = '1';
056900081212                 return;
057000081212              endif;
057100081212
057200081212              if  *in05;
057300081212                 Print_error_list();
057400081212                 iter;
057500081212              endif;
057600081212
057700081212               readc  winrec;
057800081212               dow  not(%eof);
057900091012                correct_error(bnk_type:oldpayacc:payacc:'PAY');
058000081212                oldpayacc = payacc;
058100081212                update winrec;
058200081212               readc  winrec;
058300081212               enddo;
058400081212            enddo;
058500081212      /end-free
058600081212     P Fix_error       E
058700081212
058800081212       //***************                                                    **
058900081212       // Close routine
059000081212       //***************                                                    **
059100081212     P Close_rtn       B
059200081212     D Close_rtn       PI
059300081212      /free
059400081212         close_comm();
059500081212         destroyProgressBar(progBar1Obj);
059600081212         destroyProgressBar(progBar2Obj);
059700091014         *in99 = *on;
059800091014         exfmt fmt5;
059900091014         *in99 = *off;
060000091014         errmsg = *blanks;
060100081212      /end-free
060200081212     P Close_rtn       E
060300081212
060400081212       //*********************                                              **
060500081212       // Validation display
060600081212       //*********************                                              **
060700081212     P Validation_dsp  B
060800081212     D Validation_dsp  PI
060900081212      /free
061000081212             valper = count * fulpercent / bnk_rrn;
061100081212             //(Below 5% the estimated time is probably badly misleading)
061200081212               If  valPer > 5;
061300081212                     TimeNow = %time();
061400081212                     // Time elapsed so far:?
061500081212                     TotalSecs = %diff(TimeNow :
061600081212                                          TimeStart : *seconds);
061700081212                     TotalSecs = TotalSecs/valper * (100 - valper);
061800081212                     TimeHMS = T'00.00.00' + %seconds(TotalSecs);
061900081212
062000081212               Endif;
062100081212
062200081212              progBar1.Percent = valper;
062300081212              pctA = %editw(progBar1.Percent: '  0%');
062400081212              evalr progBar1.StatusMessage =
062500081212                                   %char(TimeHMS) + ' remaining  ';
062600081212              displayProgressBar(progBar1Obj);
062700081212      /end-free
062800081212     P Validation_dsp  E
062900081212
063000081212
063100081212       //*********************                                              **
063200081212       // Progress display
063300081212       //*********************                                              **
063400081212     P Progress_dsp    B
063500081212     D Progress_dsp    PI
063600081212      /free
063700081212             proper = count * fulpercent / bnk_rrn;
063800081212             //(Below 5% the estimated time is probably badly misleading)
063900081212               If  proPer > 5;
064000081212                     TimeNow = %time();
064100081212                     // Time elapsed so far:?
064200081212                     TotalSecs = %diff(TimeNow :
064300081212                                          TimeStart : *seconds);
064400081212                     TotalSecs = TotalSecs/proPer * (100 - proPer);
064500081212                     TimeHMS = T'00.00.00' + %seconds(TotalSecs);
064600081212
064700081212               Endif;
064800081212
064900081212           progBar2.Percent = proper;
065000081212           pctA = %editw(progBar2.Percent: '  0%');
065100081212           evalr progBar2.StatusMessage =
065200081212                                   %char(TimeHMS) + ' remaining  ';
065300081212           displayProgressBar(progBar2Obj);
065400081212      /end-free
065500081212     P Progress_dsp    E
065600081212
065700081212       //**************************                                         **
065800081212       // Print error list routine
065900081212       //**************************                                         **
066000081212     P Print_error_list...
066100081212     P                 B
066200081212     D Print_error_list...
066300081212     D                 PI
066400081212      /free
066500081212
066600081212           // Print Header
066700081212            except hedr;
066800081212           // Print Detail
066900081212            for count = 1 to err_count;
067000081212                chain count winrec;
067100081212                except detail;
067200081212            endfor;
067300081212           // Print last record
067400081212            except final_prt;
067500081212      /end-free
067600081212     P Print_error_list...
067700081212     P                 E
067800030315     **
067900030315**  Program Title
068000030315      Electronic Payroll Transfer
