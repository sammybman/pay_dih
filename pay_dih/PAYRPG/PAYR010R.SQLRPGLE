000100081212     H DECEDIT('.') DATEDIT(*ymd/)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000300111002     H/define profoundui
000400081212     H*DECEDIT('.') DATEDIT(*ymd/) main(electronic_transfer)
000500031217     F* ELECTRONIC DATA TRANSFER
000600951104     F* PROGRAMMER:- *** S.BRITTON
000700030315     Fpayr010d  cf   e             workstn
000800030317     f                                     sfile(winrec:rrn)
000900111002     F/IF DEFINED(WEB)
001000111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001100111002     F/ENDIF
001200111002     Fpayelctrn uf a e           k disk    usropn
001300030315     Fqsysprt   o    f  132        printer
001400030315     **
001500111002      /copy QCPYSRC,genPR
001600111002      /copy QCPYSRC,progbarpr
001700111002      /copy QCPYSRC,autPR
001800030315     **
001900081212     Delectronic_transfer...
002000081212     D                 PR
002100091019
002200090108     DmaintTransitData...
002300111027     D                 PR                  extpgm('PAYR010CR')
002400081212
002500090120      // financial institution config
002600081212     DFI_setup         PR
002700081212
002800081212      // transfer preparation routine
002900090120     Dtransfer_rtn     PR              n
003000081212
003100081212      // Print error list  routine
003200081212     DPrint_error_list...
003300081212     D                 PR
003400081212
003500081212      // Fix error
003600081212     DFix_error        PR
003700081212
003800081212      // Close routine
003900081212     DClose_rtn        PR
004000081212
004100081212      // Validation display
004200081212     Dvalidation_dsp   PR
004300081212
004400081212      // Progress display
004500081212     Dprogress_dsp     PR
004600081212
004700030315     Dget_tot_records  PR                  extpgm('CITI01R')
004800030315     D                                2  0
004900030315     D                                6  0
005000091010     D                                3    const
005100030315     **
005200090106     Dopen_comm        PR                  extpgm(pgmname3)
005300081212
005400090106     Dclose_comm       PR                  extpgm(pgmname4)
005500081212
005600091012     Dvalidate_data    PR                  extpgm(pgmname1)
005700091012     D   citi_errDS                        likeds(citi_err) dim(100)
005800030315     D                                6  0
005900030315     D                                 n
006000030315     D                                6  0
006100030315     D                                2  0
006200091010     D                                3    const
006300030315     Dcorrect_error    PR                  extpgm('CITI03R')
006400030315     D                                2  0
006500090618     D                               30
006600090618     D                               30
006700091012     D                                3    const
006800090105
006900090105     Dprocess_data     PR                  extpgm(pgmname2)
007000030315     D                                2  0
007100030315     D                                6  0
007200030315     D                                 n
007300030315     D                                6  0
007400030315     D                                1
007500030318     D                                6  0
007600030318     D                                6  0
007700030318     D                               25
007800091010     D                                3    const
007900030315     **
008000030317     D                 ds                  inz
008100030317     D  divtypeDS                    10  0
008200030317     D  div_list                      2  0 dim(5) overlay(divtypeDS)
008300030318     **
008400030318     D                 ds
008500030318     D  accountDS                    25
008600030318     D  citiacc                       1    dim(9) overlay(accountDS)
008700030315     **
008800091012     Dciti_err         ds                  dim(100) qualified
008900030315     D e_payemp                       6  0
009000090618     D e_payacc                      30
009100030315     D e_paycrd                       6  0
009200030315     D e_payname                     26
009300081212
009400030315     **
009500030315     Deof              s               n
009600030317     Drrn              s              4  0
009700030315     Dcount            s              6  0
009800030315     Derr_count        s              6  0
009900030317     Dconame_lines     s             30    inz(*all'=')
010000090105     Dpgmname1         s             10
010100090105     Dpgmname2         s             10
010200090106     Dpgmname3         s             10
010300090106     Dpgmname4         s             10
010400030315     **
010500980103     D arr             s             40    dim(2) ctdata perrcd(1)
010600030315     D err_ind         s              1
010700030315     D bnk_type        s              2  0
010800030315     D bnk_rrn         s              6  0
010900060626     D TimeStart       S               T
011000060626     D TimeNow         S               T
011100060626     D TimeHMS         S               T   TimFmt(*HMS)
011200060626     D TotalSecs       S              9S 0
011300091012     D hold_code       s              2
011400081204
011500081204     D proper          s              4  1
011600081204     D valper          s              4  1
011700081204     D pcta            s              5
011800081204     D progBar1Obj     S                   like(createProgressBar)
011900081204     D progBar2Obj     S                   like(createProgressBar)
012000081212     D progBar1        DS                  likeDS(prgBars) based(progBar1Obj)
012100081212     D progBar2        DS                  likeDS(prgBars) based(progBar2Obj)
012200030315     **
012301121119     D PGM1CITI        c                   'CITI02R'
012400100212     D PGM1CITI1       c                   'CB0001R'
012500090105     D PGM1RBG         c                   'RB0001R'
012600100923     D PGM1DMB         c                   'DB0001R'
012601121119     D PGM1GBTI        c                   'GB0001R'
012700110211     D PGM1SCB         c                   'SB0001R'
012800090105     D PGM2CITI        c                   'CITI04R'
012900100212     D PGM2CITI2       c                   'CB0002R'
013000090105     D PGM2RBG         c                   'RB0002R'
013100100923     D PGM2DMB         c                   'DB0002R'
013101121120     D PGM2GBTI        c                   'GB0002R'
013200121120     D PGM2SCB         c                   'SB0002R'
013300090106     D PGM3CITI        c                   'CITI00C'
013400100212     D PGM3CITI3       c                   'CB0000C'
013500090106     D PGM3RBG         c                   'RB0000C'
013600100923     D PGM3DMB         c                   'DB0000C'
013601121120     D PGM3GBTI        c                   'GB0000C'
013700110211     D PGM3SCB         c                   'SB0000C'
013800090106     D PGM4CITI        c                   'CITI01C'
013900100212     D PGM4CITI4       c                   'CB0001C'
014000090106     D PGM4RBG         c                   'RB0001C'
014100100923     D PGM4DMB         c                   'DB0001C'
014101121119     D PGM4GBTI        c                   'GB0001C'
014200110211     D PGM4SCB         c                   'SB0001C'
014300081211     D COMPLETETEXT    c                   'COMPLETED'
014400081211     D FULPERCENT      c                   100
014500090120     d DIGITS          c                   '0123456789'
014600030318     **
014700030315     D msg1            c                   'Preparing to transmit data'
014800030315     D msg2            c                   'Opening communication line ....'
014900030315     D msg3            c                   'Validating data ...............'
015000030315     D msg4            c                   'Processing data ...............'
015100031217     D msg5            c                   'Closing communication line : PRESS -
015200030411     D                                      ENTER to continue'
015300030315     D msg6            c                   'Error found while processing...'
015400030315     D msg7            c                   'No batch available for transfer'
015500030317     D msg8            c                   'Invalid selection or Parameters not-
015600030317     D                                      configured'
015700030317     D msg9            c                   'No Transaction Found for transfer'
015800030318     D msg10           c                   'Invalid Payment division'
015900090107     D msg11           c                   'Batch range setup incorrectly'
016000090107     D msg12           c                   'Function Key not valid on selected -
016100090107     D                                     field'
016200980103     **
016300980103     D                sds
016400980103     D  @pgm             *proc
016500111002     D  @user                358    367
016600030315
016700030315      /free
016800030315
016900111002         // authority check
017000111002         monitor;
017100111002           authority_check('PAY':@pgm);
017200111002         on-error;
017300111002           show_msg(get_err_msg('DIH0054'));
017400111002           *inlr = *on;
017500111002           return;
017600111002         endmon;
017700081212          // Initial Setup
017800081212           exsr  aainit;
017900030315
018000081212          // Main routine
018100081212         electronic_transfer();
018200030315
018300111002
018400111002         //*****************************
018500111002         // Session Init
018600111002         //*****************************
018700111002
018800111002          begsr *inzsr;
018900111006            get_session_info('PAY':errmsg:@pgm);
019000111002            if errmsg <> *blanks;
019100111002              show_msg(errmsg);
019200111002              *inlr = *on;
019300111002              return;
019400111002            endif;
019500111002
019600111002            open(e) payelctrn;
019700111002
019800111002          endsr;
019900030315        //***************************************************
020000030315        //** First initilisation routine for all programs  **
020100030315        //***************************************************
020200030315         begsr aainit;
020300030315
020400081212           // Get Company Name
020500081212           coname = get_coname;
020600081212           @DATE = get_sys_date();
020700081212           // select header for program
020800081212           @title = arr(1);
020900030315
021000030315         endsr;
021100030318
021200030315      /end-free
021300030317     **
021400030317     Oqsysprt   E            HEDR           1 04
021500030317     O                       @pgm                10
021600030317     O                       coname              75
021700030317     O                       @date              128
021800030317     O          E            hedr           1
021900030317     O                       @user               10
022000030317     O                       coname_lines        75
022100030317     O          E            hedr           1
022200030317     O                       @title              80
022300030317     O                                          123 'PAGE:'
022400030317     O                       PAGE               128
022500030317     O          E            hedr           1
022600030317     O                                           11 'Employee'
022700030317     O                                           50 'Account No.'
022800030317     O                                           65 'Credit Date'
022900030317     **
023000030317     O          E            detail         1
023100030317     O                       payemp        z      8
023200030317     O                       payname           +  1
023300030317     O                       payacc              50
023400030317     O                       paycrd        y     65
023500030317     O          E            final_prt   2  2
023600030317     O                                           75 'End of Report'
023700081212
023800081212       //************************************                               **
023900081212       // Electric Transfer Main routine
024000081212       //************************************                               **
024100081212     P Electronic_transfer...
024200081212     P                 B
024300081212     D Electronic_transfer...
024400081212     D                 PI
024500090120
024600090120     D completeInd     s               n
024700081212      /free
024800081212        //**************************
024900081212        //*** Start of Program   ***
025000081212        //**************************
025100081212          dow  0 = 0;
025200081212
025300081212           exfmt  fmt1;
025400081212
025500081212           // Terminate program
025600081212            if  *in03;
025700081212               *inlr = *on;
025800081212               return;
025900081212            endif;
026000081212
026100081212            select;
026200091019
026300091019             // Setup transfer parameters
026400081212              when  option = 1;
026500081212                 dow 0 = 0;
026600081212                   exfmt fmt2;
026700081212                   errmsg = *blanks;
026800100924                   if *in02 or *in03;
026900081212                     leave;
027000081212                   endif;
027100081212
027200081212                   clear pelcfmt;
027300081212                  chain banksel pelcfmt;
027400081212                   select;
027500081212                    when banksel = 1;
027600081212                     FI_setup();
027700081212                    when banksel = 2;
027800081212                     FI_setup();
027900110211                    when banksel = 3; // SCOTIA BANK
028000110211                     FI_setup();
028100100923                    when banksel = 4;
028200100923                     FI_setup();
028300121119                    when banksel = 5; //GBTI
028400090805                     FI_setup();
028500100924                    when banksel = 6;
028600100924                     FI_setup();
028601121119                    when banksel = 7;
028602121119                     FI_setup();
028700081212                   endsl;
028800081212                 enddo;
028900081212
029000081212           // Processing transactions (electronic transfer)
029100081212              when  option = 2;
029200081212                 dow 0 = 0;
029300081212
029400081212                   exfmt fmt2;
029500081212                   errmsg = *blanks;
029600081212                   errmsg2 = *blanks;
029700081212                   errmsg3 = *blanks;
029800081212                   errmsg4 = *blanks;
029900081212
030000090119                  // Go back
030100090120                   if *in02 or
030200090120                       completeInd;
030300081212                     leave;
030400081212                   endif;
030500090119
030600090119                  // Terminate program
030700090119                   if *in03;
030800101125                    *inlr = *on;
030900101125                    return;
031000090119                   endif;
031100081212
031200081212                   chain banksel pelcfmt;
031300081212                   if not(%found);
031400081212                     errmsg = msg8;
031500081212                     iter;
031600081212                   endif;
031700090105
031800090105                   // select relavant programs for processing
031900090105                   select;
032000090105                     when banksel = 1;
032100100212                       pgmname1 = PGM1CITI1;
032200100212                       pgmname2 = PGM2CITI2;
032300100212                       pgmname3 = PGM3CITI3;
032400100212                       pgmname4 = PGM4CITI4;
032500090105                     when banksel = 2;
032600090105                       pgmname1 = PGM1RBG;
032700090105                       pgmname2 = PGM2RBG;
032800090106                       pgmname3 = PGM3RBG;
032900090106                       pgmname4 = PGM4RBG;
033000110211                     when banksel = 3;  // SCOTIA BANK
033100110211                       pgmname1 = PGM1SCB;
033200110211                       pgmname2 = PGM2SCB;
033300110211                       pgmname3 = PGM3SCB;
033400110211                       pgmname4 = PGM4SCB;
033500100923                     when banksel = 4;
033600100923                       pgmname1 = PGM1DMB;
033700100923                       pgmname2 = PGM2DMB;
033800100923                       pgmname3 = PGM3DMB;
033900100923                       pgmname4 = PGM4DMB;
033901121119                     when banksel = 5;
033902121119                       pgmname1 = PGM1GBTI;
033903121119                       pgmname2 = PGM2GBTI;
033904121119                       pgmname3 = PGM3GBTI;
033905121119                       pgmname4 = PGM4GBTI;
034000090105                   endsl;
034100081212
034200081212                 leave;
034300081212                 enddo;
034400121121
034500081212                 divtypeDs = pediv;
034600081212                 for count = 1 to %elem(div_list);
034700081212                    if div_list(count) <> *zeros;
034800081212                      bnk_type = div_list(count);
034900090120                      if transfer_rtn();
035000090120                         completeInd = *on;
035100090120                      endif;
035200081212                    endif;
035300081212                 endfor;
035400090119                 banksel = *zeros;
035500081212              other;
035600081212               errmsg = get_err_msg('DIH0026');
035700081212                 iter;
035800081212            endsl;
035900081212
036000101125                    *inlr = *on;
036100101125                    return;
036200081212           enddo;
036300081212      /end-free
036400081212     P Electronic_transfer...
036500081212     P                 E
036600081212
036700081212
036800081212       //************************************                               **
036900081212       // Transfer/preparation routine
037000081212       //************************************                               **
037100081212     P Transfer_rtn    B
037200090120     D Transfer_rtn    PI              n
037300100927     D temp_rrn        S              4  0 inz(0)
037400100927
037500081212      /free
037600091014           write fmt4;
037700081212           progBar1Obj = createProgressBar();
037800081212           if progBar1Obj <> *Null;
037900091014             progBar1.WindowTop = 6;
038000081212             progBar1.WindowTitle = 'Validate Transactions';
038100081212             progBar1.Color = COLOR_BLUE;
038200081212             progBar1.ShowPercent = *ON;
038300081212           endif;
038400081212
038500081212           valper = *zeros;
038600081212           proper = *zeros;
038700081212           errmsg = *blanks;
038800081212           errmsg2 = *blanks;
038900081212           errmsg3 = *blanks;
039000081212           errmsg4 = *blanks;
039100081212
039200081212          // get total records
039300081212           errmsg = msg1;
039400091014           write fmt5;
039500091010           get_tot_records(bnk_type:bnk_rrn:'PAY');
039600081212            if bnk_rrn = *zeros;
039700081212               errmsg = msg9;
039800090120               return *off;
039900081212            endif;
040000081212
040100081212          // Open communications
040200081212          //---------------------
040300081212           %subst(errmsg:33:9) = COMPLETETEXT;
040400081212           errmsg2 = msg2;
040500091014           write fmt5;
040600081212           open_comm();
040700081212
040800081212          // Validate data
040900081212          //--------------
041000081212           %subst(errmsg2:33:9) = COMPLETETEXT;
041100081212           errmsg3 = msg3;
041200091014           write fmt5;
041300081212           eof = *off;
041400081212           TimeStart = %time();
041500081212           for count = 0 to bnk_rrn;
041600081212             validate_data(citi_err:err_count:
041700091010                             eof:count:bnk_type:'PAY');
041800081212              if  eof;
041900081212               dow count <= bnk_rrn;
042000081212                 Validation_dsp();
042100081212                 count += 1;
042200081212               enddo;
042300081212               leave;
042400081212              endif;
042500081212
042600081212               Validation_dsp();
042700081212         endfor;
042800100927
042900081212           // Fix data error
043000081212           //---------------
043100081212         if err_count > *zeros;
043200081212            close_comm();
043300081212            fix_error();
043400090120            return *off;
043500081212         endif;
043600081212
043700081212          // Process data
043800081212          //-------------
043900081212        progBar2Obj = createProgressBar();
044000081212        if progBar2Obj <> *Null;
044100091014          progBar2.WindowTop = 14;
044200081212          progBar2.WindowTitle = 'Process Transactions';
044300081212          progBar2.Color = COLOR_BLUE;
044400081212          progBar2.ShowPercent = *ON;
044500081212        endif;
044600081212
044700081212           %subst(errmsg3:33:9) = COMPLETETEXT;
044800090119           errmsg4 = msg4;
044900091014           write fmt5;
045000081212           eof = *off;
045100081212           err_ind = *blank;
045200081212           TimeStart = %time();
045300081212           for count = 1 to bnk_rrn;
045400081212             process_data(bnk_type:count:eof:
045500081212                       bnk_rrn:err_ind:pelower:peupper:
045600091010                              peacct:'PAY');
045700081212              if eof;
045800081212               errmsg = msg5;
045900081212               close_rtn();
046000090120               return *on;
046100081212              endif;
046200081212
046300081212              if err_ind <> *blank;
046400081212               select;
046500081212                 when err_ind = '1';
046600081212                  errmsg = msg6;
046700081212                 when err_ind = '2';
046800081212                  errmsg = msg7;
046900081212               endsl;
047000081212               close_rtn();
047100090120               return *off;
047200081212              endif;
047300081212
047400081212             progress_dsp();
047500081212         endfor;
047600081212
047700081212         %subst(errmsg4:33:9) = COMPLETETEXT;
047800091014         write fmt5;
047900081212         close_rtn();
048000090120         return *on;
048100081212
048200081212      /end-free
048300081212     P Transfer_rtn    E
048400081212
048500081212       //************************************                               **
048600081212       // Financial Institution Config
048700081212       //************************************                               **
048800081212     P FI_setup        B
048900081212     D FI_setup        PI
049000081212
049100081212      /free
049200091019
049300090107          // prepare screen fields
049400090107          pesubject# = pesubject;
049500090107          petext# = petext;
049600090107          pefilenm# = pefilenm;
049700090107          pemfrom# = pemfrom;
049800090108          pechgamt# = pechgamt;
049900090107          pevpgmid = %editc(banksel:'X') +
050000090107                     %subst(@pgm:3);
050100090107
050200081212          dow  0 = 0;
050300091019
050400081212            exfmt fmt3;
050500081212            errmsg = *blanks;
050600081212
050700081212            if *in02;
050800081212              return;
050900081212            endif;
051000090106
051100090107            // Prompt for list/maintenance program
051200090107            if  *in04;
051300090107               select;
051400090107                 // Transit number range Maintenance
051500090107                 when fld = 'PEDFTTRN#';
051600090108                   maintTransitData();
051700090107                 // ALL other selections
051800090107                 other;
051900090107                   errmsg = msg12;
052000090107               endsl;
052100090107               iter;
052200090107            endif;
052300090107
052400090107            // F7 = Create/Modify Mailing List
052500090106            if *in07;
052600110201
052700090107              email_list('PAY':pevpgmid);
052800090106              iter;
052900090106            endif;
053000081212
053100081212            // Validate account number;
053200081212                accountDS = peacct;
053300081212               for count = 1 to %elem(citiacc);
053400090107               if  %scan(citiacc(count):digits) = 0;
053500090107               //   errmsg = get_err_msg('DIH0059');
053600090107                  leave;
053700081212               endif;
053800081212               endfor;
053900081212               if  errmsg <> *blanks;
054000081212                 iter;
054100081212               endif;
054200090107
054300081212            // Validate batch range;
054400090107            if peupper < pelower
054500081212                or peupper = *zeros
054600081212                or pelower = *zeros;
054700081212                    errmsg = msg11;
054800081212                    iter;
054900090107            endif;
055000081212
055100081212            divtypeDS = pediv;
055200081212            // Validate configuration;
055300081212              for count = 1 to %elem(div_list);
055400081212                 if div_list(count) <> *zeros;
055500091012                   bnk_type = div_list(count);
055600091012
055700091012                   exec sql
055800091012                     select dtno into :hold_code from paydiv
055900091012                      where dtno = :bnk_type and dtrsts = 'A';
056000091012                   if sqlstt = '02000';
056100091012                      errmsg = msg10;
056200091012                      iter;
056300091012                   endif;
056400081212                 endif;
056500081212               endfor;
056600081212
056700081212            // write configuration;
056800090107                 // prepare database fields
056900090107                 pesubject = %trim(pesubject#);
057000090107                 petext = %trim(petext#);
057100090107                 pefilenm = %trim(pefilenm#);
057200090107                 pemfrom = %trim(pemfrom#);
057300090108                 pechgamt = pechgamt#;
057400090107
057500081212                 if not(%found(payelctrn));
057600081212                  petype = banksel;
057700081212                  write pelcfmt;
057800081212                   else;
057900081212                  update pelcfmt;
058000081212                 endif;
058100081212
058200081212             return;
058300081212          enddo;
058400081212
058500081212      /end-free
058600081212     P FI_setup        E
058700081212
058800081212       //************                                                       **
058900081212       // Fix error
059000081212       //************                                                       **
059100081212     P Fix_error       B
059200081212     D Fix_error       PI
059300081212      /free
059400081212
059500081212            *in33 = *on;
059600081212              write  window;
059700081212            write  wdwctl;
059800081212            *in33 = *off;
059900081212
060000081212
060100081212            // load subfile
060200081212            for rrn = 1 to err_count;
060300091012                   payemp = citi_err(rrn).e_payemp;
060400091012                   paycrd = citi_err(rrn).e_paycrd;
060500091012                   payacc = citi_err(rrn).e_payacc;
060600091012                   oldpayacc = citi_err(rrn).e_payacc;
060700091012                   payname = citi_err(rrn).e_payname;
060800081212                   write   winrec;
060900081212            endfor;
061000081212
061100081212            dow  0 = 0;
061200081212              exfmt  wdwctl;
061300081212
061400081212            // F3 = End Program
061500081212              if  *in03;
061600081212                 *inlr = '1';
061700081212                 return;
061800081212              endif;
061900081212
062000081212              if  *in05;
062100081212                 Print_error_list();
062200081212                 iter;
062300081212              endif;
062400081212
062500081212               readc  winrec;
062600081212               dow  not(%eof);
062700091012                correct_error(bnk_type:oldpayacc:payacc:'PAY');
062800081212                oldpayacc = payacc;
062900081212                update winrec;
063000081212               readc  winrec;
063100081212               enddo;
063200081212            enddo;
063300081212      /end-free
063400081212     P Fix_error       E
063500081212
063600081212       //***************                                                    **
063700081212       // Close routine
063800081212       //***************                                                    **
063900081212     P Close_rtn       B
064000081212     D Close_rtn       PI
064100081212      /free
064200081212         close_comm();
064300081212         destroyProgressBar(progBar1Obj);
064400081212         destroyProgressBar(progBar2Obj);
064500091014         *in99 = *on;
064600091014         exfmt fmt5;
064700091014         *in99 = *off;
064800091014         errmsg = *blanks;
064900081212      /end-free
065000081212     P Close_rtn       E
065100081212
065200081212       //*********************                                              **
065300081212       // Validation display
065400081212       //*********************                                              **
065500081212     P Validation_dsp  B
065600081212     D Validation_dsp  PI
065700081212      /free
065800100927
065900081212             valper = count * fulpercent / bnk_rrn;
066000081212             //(Below 5% the estimated time is probably badly misleading)
066100081212               If  valPer > 5;
066200081212                     TimeNow = %time();
066300081212                     // Time elapsed so far:?
066400081212                     TotalSecs = %diff(TimeNow :
066500081212                                          TimeStart : *seconds);
066600081212                     TotalSecs = TotalSecs/valper * (100 - valper);
066700081212                     TimeHMS = T'00.00.00' + %seconds(TotalSecs);
066800081212
066900081212               Endif;
067000081212
067100081212              progBar1.Percent = valper;
067200081212              pctA = %editw(progBar1.Percent: '  0%');
067300081212              evalr progBar1.StatusMessage =
067400081212                                   %char(TimeHMS) + ' remaining  ';
067500081212              displayProgressBar(progBar1Obj);
067600081212      /end-free
067700081212     P Validation_dsp  E
067800081212
067900081212
068000081212       //*********************                                              **
068100081212       // Progress display
068200081212       //*********************                                              **
068300081212     P Progress_dsp    B
068400081212     D Progress_dsp    PI
068500081212      /free
068600081212             proper = count * fulpercent / bnk_rrn;
068700081212             //(Below 5% the estimated time is probably badly misleading)
068800081212               If  proPer > 5;
068900081212                     TimeNow = %time();
069000081212                     // Time elapsed so far:?
069100081212                     TotalSecs = %diff(TimeNow :
069200081212                                          TimeStart : *seconds);
069300081212                     TotalSecs = TotalSecs/proPer * (100 - proPer);
069400081212                     TimeHMS = T'00.00.00' + %seconds(TotalSecs);
069500081212
069600081212               Endif;
069700081212
069800081212           progBar2.Percent = proper;
069900081212           pctA = %editw(progBar2.Percent: '  0%');
070000081212           evalr progBar2.StatusMessage =
070100081212                                   %char(TimeHMS) + ' remaining  ';
070200081212           displayProgressBar(progBar2Obj);
070300081212      /end-free
070400081212     P Progress_dsp    E
070500081212
070600081212       //**************************                                         **
070700081212       // Print error list routine
070800081212       //**************************                                         **
070900081212     P Print_error_list...
071000081212     P                 B
071100081212     D Print_error_list...
071200081212     D                 PI
071300081212      /free
071400081212
071500081212           // Print Header
071600081212            except hedr;
071700081212           // Print Detail
071800081212            for count = 1 to err_count;
071900081212                chain count winrec;
072000081212                except detail;
072100081212            endfor;
072200081212           // Print last record
072300081212            except final_prt;
072400081212      /end-free
072500081212     P Print_error_list...
072600081212     P                 E
072700030315     **
072800030315**  Program Title
072900030315      Electronic Payroll Transfer
