000100081212     H DECEDIT('.') DATEDIT(*ymd/)
000200081212     H*DECEDIT('.') DATEDIT(*ymd/) main(electronic_transfer)
000300031217     F* ELECTRONIC DATA TRANSFER
000400951104     F* PROGRAMMER:- *** S.BRITTON
000500030315     Fpayr010d  cf   e             workstn
000600030317     f                                     sfile(winrec:rrn)
000700030317     Fpayelctrn uf a e           k disk
000800030318     Fpaydivl01 if   e           k disk
000900030315     Fqsysprt   o    f  132        printer
001000030315     **
001100030315      /copy genPR
001200081204      /copy progbarpr
001300040207      /copy autPR
001400030315     **
001500081212     Delectronic_transfer...
001600081212     D                 PR
001700081212     D***              PR                  extpgm('PAYR010R')
001800090108     DmaintTransitData...
001900090129     D                 PR                  extproc('PAYR010C')
002000081212
002100090120      // financial institution config
002200081212     DFI_setup         PR
002300081212
002400081212      // transfer preparation routine
002500090120     Dtransfer_rtn     PR              n
002600081212
002700081212      // Print error list  routine
002800081212     DPrint_error_list...
002900081212     D                 PR
003000081212
003100081212      // Fix error
003200081212     DFix_error        PR
003300081212
003400081212      // Close routine
003500081212     DClose_rtn        PR
003600081212
003700081212      // Validation display
003800081212     Dvalidation_dsp   PR
003900081212
004000081212      // Progress display
004100081212     Dprogress_dsp     PR
004200081212
004300030315     Dget_tot_records  PR                  extpgm('CITI01R')
004400030315     D                                2  0
004500030315     D                                6  0
004600091010     D                                3    const
004700030315     **
004800090106     Dopen_comm        PR                  extpgm(pgmname3)
004900081212
005000090106     Dclose_comm       PR                  extpgm(pgmname4)
005100081212
005200090105     Dvalidate_data    PR                  extpgm(pgmname1)
005300030315     D                               50
005400030315     D                                6  0
005500030315     D                                 n
005600030315     D                                6  0
005700030315     D                                2  0
005800091010     D                                3    const
005900030315     Dcorrect_error    PR                  extpgm('CITI03R')
006000030315     D                                2  0
006100090618     D                               30
006200090618     D                               30
006300090105
006400090105     Dprocess_data     PR                  extpgm(pgmname2)
006500030315     D                                2  0
006600030315     D                                6  0
006700030315     D                                 n
006800030315     D                                6  0
006900030315     D                                1
007000030318     D                                6  0
007100030318     D                                6  0
007200030318     D                               25
007300091010     D                                3    const
007400030315     **
007500030317     **
007600030317     D                 ds                  inz
007700030317     D  divtypeDS                    10  0
007800030317     D  div_list                      2  0 dim(5) overlay(divtypeDS)
007900030318     **
008000030318     D                 ds
008100030318     D  accountDS                    25
008200030318     D  citiacc                       1    dim(9) overlay(accountDS)
008300030315     **
008400030315     Dciti_err         ds                  occurs(100)
008500030315     D e_payemp                       6  0
008600090618     D e_payacc                      30
008700030315     D e_paycrd                       6  0
008800030315     D e_payname                     26
008900081212
009000030315     **
009100030315     Deof              s               n
009200030317     Drrn              s              4  0
009300030315     Dcount            s              6  0
009400030315     Derr_count        s              6  0
009500030317     Dconame_lines     s             30    inz(*all'=')
009600090105     Dpgmname1         s             10
009700090105     Dpgmname2         s             10
009800090106     Dpgmname3         s             10
009900090106     Dpgmname4         s             10
010000030315     **
010100980103     D arr             s             40    dim(2) ctdata perrcd(1)
010200030315     D err_ind         s              1
010300030315     D bnk_type        s              2  0
010400030315     D bnk_rrn         s              6  0
010500060626     D TimeStart       S               T
010600060626     D TimeNow         S               T
010700060626     D TimeHMS         S               T   TimFmt(*HMS)
010800060626     D TotalSecs       S              9S 0
010900081204
011000081204     D proper          s              4  1
011100081204     D valper          s              4  1
011200081204     D pcta            s              5
011300081204     D progBar1Obj     S                   like(createProgressBar)
011400081204     D progBar2Obj     S                   like(createProgressBar)
011500081212     D progBar1        DS                  likeDS(prgBars) based(progBar1Obj)
011600081212     D progBar2        DS                  likeDS(prgBars) based(progBar2Obj)
011700030315     **
011800090105     D PGM1CITI        c                   'CITI02R'
011900090105     D PGM1RBG         c                   'RB0001R'
012000090105     D PGM2CITI        c                   'CITI04R'
012100090105     D PGM2RBG         c                   'RB0002R'
012200090106     D PGM3CITI        c                   'CITI00C'
012300090106     D PGM3RBG         c                   'RB0000C'
012400090106     D PGM4CITI        c                   'CITI01C'
012500090106     D PGM4RBG         c                   'RB0001C'
012600081211     D COMPLETETEXT    c                   'COMPLETED'
012700081211     D FULPERCENT      c                   100
012800090120     d DIGITS          c                   '0123456789'
012900030318     **
013000030315     D msg1            c                   'Preparing to transmit data'
013100030315     D msg2            c                   'Opening communication line ....'
013200030315     D msg3            c                   'Validating data ...............'
013300030315     D msg4            c                   'Processing data ...............'
013400031217     D msg5            c                   'Closing communication line : PRESS -
013500030411     D                                      ENTER to continue'
013600030315     D msg6            c                   'Error found while processing...'
013700030315     D msg7            c                   'No batch available for transfer'
013800030317     D msg8            c                   'Invalid selection or Parameters not-
013900030317     D                                      configured'
014000030317     D msg9            c                   'No Transaction Found for transfer'
014100030318     D msg10           c                   'Invalid Payment division'
014200090107     D msg11           c                   'Batch range setup incorrectly'
014300090107     D msg12           c                   'Function Key not valid on selected -
014400090107     D                                     field'
014500980103     **
014600980103     D                sds
014700980103     D  @pgm             *proc
014800980103     D  @user                254    263
014900030315
015000030315      /free
015100030315
015200081212           authority_check('PAY':@pgm);
015300081212          // Initial Setup
015400081212           exsr  aainit;
015500030315
015600081212          // Main routine
015700081212         electronic_transfer();
015800030315
015900030315        //***************************************************
016000030315        //** First initilisation routine for all programs  **
016100030315        //***************************************************
016200030315         begsr aainit;
016300030315
016400081212           // Get Company Name
016500081212           coname = get_coname;
016600081212           @DATE = get_sys_date();
016700081212           // select header for program
016800081212           @title = arr(1);
016900030315
017000030315         endsr;
017100030318
017200030315      /end-free
017300030317     **
017400030317     Oqsysprt   E            HEDR           1 04
017500030317     O                       @pgm                10
017600030317     O                       coname              75
017700030317     O                       @date              128
017800030317     O          E            hedr           1
017900030317     O                       @user               10
018000030317     O                       coname_lines        75
018100030317     O          E            hedr           1
018200030317     O                       @title              80
018300030317     O                                          123 'PAGE:'
018400030317     O                       PAGE               128
018500030317     O          E            hedr           1
018600030317     O                                           11 'Employee'
018700030317     O                                           50 'Account No.'
018800030317     O                                           65 'Credit Date'
018900030317     **
019000030317     O          E            detail         1
019100030317     O                       payemp        z      8
019200030317     O                       payname           +  1
019300030317     O                       payacc              50
019400030317     O                       paycrd        y     65
019500030317     O          E            final_prt   2  2
019600030317     O                                           75 'End of Report'
019700081212
019800081212       //************************************                               **
019900081212       // Electric Transfer Main routine
020000081212       //************************************                               **
020100081212     P Electronic_transfer...
020200081212     P                 B
020300081212     D Electronic_transfer...
020400081212     D                 PI
020500090120
020600090120     D completeInd     s               n
020700081212      /free
020800081212        //**************************
020900081212        //*** Start of Program   ***
021000081212        //**************************
021100081212          dow  0 = 0;
021200081212
021300081212           exfmt  fmt1;
021400081212
021500081212           // Terminate program
021600081212            if  *in03;
021700081212               *inlr = *on;
021800081212               return;
021900081212            endif;
022000081212
022100081212            select;
022200081212           // Setup transfer parameters
022300081212              when  option = 1;
022400081212                 dow 0 = 0;
022500081212                   exfmt fmt2;
022600081212                   errmsg = *blanks;
022700081212                   if *in02;
022800081212                     leave;
022900081212                   endif;
023000081212
023100081212                   clear pelcfmt;
023200081212                  chain banksel pelcfmt;
023300081212                   select;
023400081212                    when banksel = 1;
023500081212                     FI_setup();
023600081212                    when banksel = 2;
023700081212                     FI_setup();
023800090805                    when banksel = 5;
023900090805                     FI_setup();
024000081212                   endsl;
024100081212                 enddo;
024200081212
024300081212           // Processing transactions (electronic transfer)
024400081212              when  option = 2;
024500081212                 dow 0 = 0;
024600081212
024700081212                   exfmt fmt2;
024800081212                   errmsg = *blanks;
024900081212                   errmsg2 = *blanks;
025000081212                   errmsg3 = *blanks;
025100081212                   errmsg4 = *blanks;
025200081212
025300090119                  // Go back
025400090120                   if *in02 or
025500090120                       completeInd;
025600081212                     leave;
025700081212                   endif;
025800090119
025900090119                  // Terminate program
026000090119                   if *in03;
026100090119                     leave;
026200090119                   endif;
026300081212
026400081212                   chain banksel pelcfmt;
026500081212                   if not(%found);
026600081212                     errmsg = msg8;
026700081212                     iter;
026800081212                   endif;
026900090105
027000090105                   // select relavant programs for processing
027100090105                   select;
027200090105                     when banksel = 1;
027300090105                       pgmname1 = PGM1CITI;
027400090105                       pgmname2 = PGM2CITI;
027500090106                       pgmname3 = PGM3CITI;
027600090106                       pgmname4 = PGM4CITI;
027700090105                     when banksel = 2;
027800090105                       pgmname1 = PGM1RBG;
027900090105                       pgmname2 = PGM2RBG;
028000090106                       pgmname3 = PGM3RBG;
028100090106                       pgmname4 = PGM4RBG;
028200090105                   endsl;
028300081212
028400081212                 leave;
028500081212                 enddo;
028600081212
028700081212                 divtypeDs = pediv;
028800081212                 for count = 1 to %elem(div_list);
028900081212                    if div_list(count) <> *zeros;
029000081212                      bnk_type = div_list(count);
029100090120                      if transfer_rtn();
029200090120                         completeInd = *on;
029300090120                      endif;
029400081212                    endif;
029500081212                 endfor;
029600090119                 banksel = *zeros;
029700081212              other;
029800081212               errmsg = get_err_msg('DIH0026');
029900081212                 iter;
030000081212            endsl;
030100081212
030200081212           enddo;
030300081212      /end-free
030400081212     P Electronic_transfer...
030500081212     P                 E
030600081212
030700081212
030800081212       //************************************                               **
030900081212       // Transfer/preparation routine
031000081212       //************************************                               **
031100081212     P Transfer_rtn    B
031200090120     D Transfer_rtn    PI              n
031300081212
031400081212      /free
031500081212           progBar1Obj = createProgressBar();
031600081212           if progBar1Obj <> *Null;
031700090120             progBar1.WindowTop = 8;
031800081212             progBar1.WindowTitle = 'Validate Transactions';
031900081212             progBar1.Color = COLOR_BLUE;
032000081212             progBar1.ShowPercent = *ON;
032100081212           endif;
032200081212
032300081212           valper = *zeros;
032400081212           proper = *zeros;
032500081212           errmsg = *blanks;
032600081212           errmsg2 = *blanks;
032700081212           errmsg3 = *blanks;
032800081212           errmsg4 = *blanks;
032900081212
033000081212          // get total records
033100081212           errmsg = msg1;
033200081212           write fmt4;
033300091010           get_tot_records(bnk_type:bnk_rrn:'PAY');
033400081212            if bnk_rrn = *zeros;
033500081212               errmsg = msg9;
033600090120               return *off;
033700081212            endif;
033800081212
033900081212          // Open communications
034000081212          //---------------------
034100081212           %subst(errmsg:33:9) = COMPLETETEXT;
034200081212           errmsg2 = msg2;
034300081212           write fmt4;
034400081212           open_comm();
034500081212
034600081212          // Validate data
034700081212          //--------------
034800081212           %subst(errmsg2:33:9) = COMPLETETEXT;
034900081212           errmsg3 = msg3;
035000081212           write fmt4;
035100081212           eof = *off;
035200081212           TimeStart = %time();
035300081212           for count = 0 to bnk_rrn;
035400081212             validate_data(citi_err:err_count:
035500091010                             eof:count:bnk_type:'PAY');
035600081212              if  eof;
035700081212               dow count <= bnk_rrn;
035800081212                 Validation_dsp();
035900081212                 count += 1;
036000081212               enddo;
036100081212               leave;
036200081212              endif;
036300081212
036400081212               Validation_dsp();
036500081212         endfor;
036600081212
036700081212           // Fix data error
036800081212           //---------------
036900081212         if err_count > *zeros;
037000081212            close_comm();
037100081212            fix_error();
037200090120            return *off;
037300081212         endif;
037400081212
037500081212          // Process data
037600081212          //-------------
037700081212        progBar2Obj = createProgressBar();
037800081212        if progBar2Obj <> *Null;
037900090120          progBar2.WindowTop = 18;
038000081212          progBar2.WindowTitle = 'Process Transactions';
038100081212          progBar2.Color = COLOR_BLUE;
038200081212          progBar2.ShowPercent = *ON;
038300081212        endif;
038400081212
038500081212           %subst(errmsg3:33:9) = COMPLETETEXT;
038600090119           errmsg4 = msg4;
038700081212           write fmt4;
038800081212           eof = *off;
038900081212           err_ind = *blank;
039000081212           TimeStart = %time();
039100081212           for count = 1 to bnk_rrn;
039200081212             process_data(bnk_type:count:eof:
039300081212                       bnk_rrn:err_ind:pelower:peupper:
039400091010                              peacct:'PAY');
039500081212              if eof;
039600081212               errmsg = msg5;
039700081212               close_rtn();
039800090120               return *on;
039900081212              endif;
040000081212
040100081212              if err_ind <> *blank;
040200081212               select;
040300081212                 when err_ind = '1';
040400081212                  errmsg = msg6;
040500081212                 when err_ind = '2';
040600081212                  errmsg = msg7;
040700081212               endsl;
040800081212               close_rtn();
040900090120               return *off;
041000081212              endif;
041100081212
041200081212             progress_dsp();
041300081212         endfor;
041400081212
041500081212         %subst(errmsg4:33:9) = COMPLETETEXT;
041600081212         close_rtn();
041700090120         return *on;
041800081212
041900081212      /end-free
042000081212     P Transfer_rtn    E
042100081212
042200081212       //************************************                               **
042300081212       // Financial Institution Config
042400081212       //************************************                               **
042500081212     P FI_setup        B
042600081212     D FI_setup        PI
042700081212
042800081212      /free
042900090107          // prepare screen fields
043000090107          pesubject# = pesubject;
043100090107          petext# = petext;
043200090107          pefilenm# = pefilenm;
043300090107          pemfrom# = pemfrom;
043400090108          pechgamt# = pechgamt;
043500090107          pevpgmid = %editc(banksel:'X') +
043600090107                     %subst(@pgm:3);
043700090107
043800081212          dow  0 = 0;
043900081212            exfmt fmt3;
044000081212            errmsg = *blanks;
044100081212
044200081212            if *in02;
044300081212              return;
044400081212            endif;
044500090106
044600090107            // Prompt for list/maintenance program
044700090107            if  *in04;
044800090107               select;
044900090107                 // Transit number range Maintenance
045000090107                 when fld = 'PEDFTTRN#';
045100090108                   maintTransitData();
045200090107                 // ALL other selections
045300090107                 other;
045400090107                   errmsg = msg12;
045500090107               endsl;
045600090107               iter;
045700090107            endif;
045800090107
045900090107            // F7 = Create/Modify Mailing List
046000090106            if *in07;
046100090107              email_list('PAY':pevpgmid);
046200090106              iter;
046300090106            endif;
046400081212
046500081212            // Validate account number;
046600081212                accountDS = peacct;
046700081212               for count = 1 to %elem(citiacc);
046800090107               if  %scan(citiacc(count):digits) = 0;
046900090107               //   errmsg = get_err_msg('DIH0059');
047000090107                  leave;
047100081212               endif;
047200081212               endfor;
047300081212               if  errmsg <> *blanks;
047400081212                 iter;
047500081212               endif;
047600090107
047700081212            // Validate batch range;
047800090107            if peupper < pelower
047900081212                or peupper = *zeros
048000081212                or pelower = *zeros;
048100081212                    errmsg = msg11;
048200081212                    iter;
048300090107            endif;
048400081212
048500081212            divtypeDS = pediv;
048600081212            // Validate configuration;
048700081212              for count = 1 to %elem(div_list);
048800081212                 if div_list(count) <> *zeros;
048900081212                  bnk_type = div_list(count);
049000081212                  chain bnk_type divfmt;
049100081212                  if not(%found(paydivl01));
049200081212                    errmsg = msg10;
049300081212                    iter;
049400081212                  endif;
049500081212                 endif;
049600081212               endfor;
049700081212
049800081212            // write configuration;
049900090107                 // prepare database fields
050000090107                 pesubject = %trim(pesubject#);
050100090107                 petext = %trim(petext#);
050200090107                 pefilenm = %trim(pefilenm#);
050300090107                 pemfrom = %trim(pemfrom#);
050400090108                 pechgamt = pechgamt#;
050500090107
050600081212                 if not(%found(payelctrn));
050700081212                  petype = banksel;
050800081212                  write pelcfmt;
050900081212                   else;
051000081212                  update pelcfmt;
051100081212                 endif;
051200081212
051300081212             return;
051400081212          enddo;
051500081212
051600081212      /end-free
051700081212     P FI_setup        E
051800081212
051900081212       //************                                                       **
052000081212       // Fix error
052100081212       //************                                                       **
052200081212     P Fix_error       B
052300081212     D Fix_error       PI
052400081212      /free
052500081212
052600081212            *in33 = *on;
052700081212              write  window;
052800081212            write  wdwctl;
052900081212            *in33 = *off;
053000081212
053100081212
053200081212            // load subfile
053300081212            for rrn = 1 to err_count;
053400081212                   %occur(citi_err) = rrn;
053500081212                   payemp = e_payemp;
053600081212                   paycrd = e_paycrd;
053700081212                   payacc = e_payacc;
053800081212                   oldpayacc = e_payacc;
053900081212                   payname = e_payname;
054000081212                   write   winrec;
054100081212            endfor;
054200081212
054300081212            dow  0 = 0;
054400081212              exfmt  wdwctl;
054500081212
054600081212            // F3 = End Program
054700081212              if  *in03;
054800081212                 *inlr = '1';
054900081212                 return;
055000081212              endif;
055100081212
055200081212              if  *in05;
055300081212                 Print_error_list();
055400081212                 iter;
055500081212              endif;
055600081212
055700081212               readc  winrec;
055800081212               dow  not(%eof);
055900081212                correct_error(bnk_type:oldpayacc:payacc);
056000081212                oldpayacc = payacc;
056100081212                update winrec;
056200081212               readc  winrec;
056300081212               enddo;
056400081212            enddo;
056500081212      /end-free
056600081212     P Fix_error       E
056700081212
056800081212       //***************                                                    **
056900081212       // Close routine
057000081212       //***************                                                    **
057100081212     P Close_rtn       B
057200081212     D Close_rtn       PI
057300081212      /free
057400081212         close_comm();
057500081212         destroyProgressBar(progBar1Obj);
057600081212         destroyProgressBar(progBar2Obj);
057700081212         exfmt fmt4;
057800081212      /end-free
057900081212     P Close_rtn       E
058000081212
058100081212       //*********************                                              **
058200081212       // Validation display
058300081212       //*********************                                              **
058400081212     P Validation_dsp  B
058500081212     D Validation_dsp  PI
058600081212      /free
058700081212             valper = count * fulpercent / bnk_rrn;
058800081212             //(Below 5% the estimated time is probably badly misleading)
058900081212               If  valPer > 5;
059000081212                     TimeNow = %time();
059100081212                     // Time elapsed so far:?
059200081212                     TotalSecs = %diff(TimeNow :
059300081212                                          TimeStart : *seconds);
059400081212                     TotalSecs = TotalSecs/valper * (100 - valper);
059500081212                     TimeHMS = T'00.00.00' + %seconds(TotalSecs);
059600081212
059700081212               Endif;
059800081212
059900081212              progBar1.Percent = valper;
060000081212              pctA = %editw(progBar1.Percent: '  0%');
060100081212              evalr progBar1.StatusMessage =
060200081212                                   %char(TimeHMS) + ' remaining  ';
060300081212              displayProgressBar(progBar1Obj);
060400081212      /end-free
060500081212     P Validation_dsp  E
060600081212
060700081212
060800081212       //*********************                                              **
060900081212       // Progress display
061000081212       //*********************                                              **
061100081212     P Progress_dsp    B
061200081212     D Progress_dsp    PI
061300081212      /free
061400081212             proper = count * fulpercent / bnk_rrn;
061500081212             //(Below 5% the estimated time is probably badly misleading)
061600081212               If  proPer > 5;
061700081212                     TimeNow = %time();
061800081212                     // Time elapsed so far:?
061900081212                     TotalSecs = %diff(TimeNow :
062000081212                                          TimeStart : *seconds);
062100081212                     TotalSecs = TotalSecs/proPer * (100 - proPer);
062200081212                     TimeHMS = T'00.00.00' + %seconds(TotalSecs);
062300081212
062400081212               Endif;
062500081212
062600081212           progBar2.Percent = proper;
062700081212           pctA = %editw(progBar2.Percent: '  0%');
062800081212           evalr progBar2.StatusMessage =
062900081212                                   %char(TimeHMS) + ' remaining  ';
063000081212           displayProgressBar(progBar2Obj);
063100081212      /end-free
063200081212     P Progress_dsp    E
063300081212
063400081212       //**************************                                         **
063500081212       // Print error list routine
063600081212       //**************************                                         **
063700081212     P Print_error_list...
063800081212     P                 B
063900081212     D Print_error_list...
064000081212     D                 PI
064100081212      /free
064200081212
064300081212           // Print Header
064400081212            except hedr;
064500081212           // Print Detail
064600081212            for count = 1 to err_count;
064700081212                chain count winrec;
064800081212                except detail;
064900081212            endfor;
065000081212           // Print last record
065100081212            except final_prt;
065200081212      /end-free
065300081212     P Print_error_list...
065400081212     P                 E
065500030315     **
065600030315**  Program Title
065700030315      Electronic Payroll Transfer
