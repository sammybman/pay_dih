000100081212     H DECEDIT('.') DATEDIT(*ymd/)
000200081212     H*DECEDIT('.') DATEDIT(*ymd/) main(electronic_transfer)
000300031217     F* ELECTRONIC DATA TRANSFER
000400951104     F* PROGRAMMER:- *** S.BRITTON
000500091014     Fpayr010dnwcf   e             workstn
000600030317     f                                     sfile(winrec:rrn)
000700030317     Fpayelctrn uf a e           k disk
000800030315     Fqsysprt   o    f  132        printer
000900030315     **
001000030315      /copy genPR
001100081204      /copy progbarpr
001200040207      /copy autPR
001300030315     **
001400081212     Delectronic_transfer...
001500081212     D                 PR
001600091014     D  origin                        3    const
001700091014
001800091014      // financial institution config
001900091014     DFI_setup         PR
002000091014     D  origin                        3    const
002100091014     D  elcDS                              likeds(payelcDS)
002200091014
002300090108     DmaintTransitData...
002400090129     D                 PR                  extproc('PAYR010C')
002500081212      // transfer preparation routine
002600090120     Dtransfer_rtn     PR              n
002700091014     D  origin                        3    const
002800081212
002900081212      // Print error list  routine
003000081212     DPrint_error_list...
003100081212     D                 PR
003200081212
003300081212      // Fix error
003400081212     DFix_error        PR
003500091014     D  origin                        3    const
003600081212
003700081212      // Close routine
003800081212     DClose_rtn        PR
003900081212
004000081212      // Validation display
004100081212     Dvalidation_dsp   PR
004200081212
004300081212      // Progress display
004400081212     Dprogress_dsp     PR
004500081212
004600030315     Dget_tot_records  PR                  extpgm('CITI01R')
004700030315     D                                2  0
004800030315     D                                6  0
004900091010     D                                3    const
005000030315     **
005100090106     Dopen_comm        PR                  extpgm(pgmname3)
005200081212
005300090106     Dclose_comm       PR                  extpgm(pgmname4)
005400081212
005500091012     Dvalidate_data    PR                  extpgm(pgmname1)
005600091012     D   citi_errDS                        likeds(citi_err) dim(100)
005700030315     D                                6  0
005800030315     D                                 n
005900030315     D                                6  0
006000030315     D                                2  0
006100091010     D                                3    const
006200030315     Dcorrect_error    PR                  extpgm('CITI03R')
006300030315     D                                2  0
006400090618     D                               30
006500090618     D                               30
006600091012     D                                3    const
006700090105
006800090105     Dprocess_data     PR                  extpgm(pgmname2)
006900030315     D                                2  0
007000030315     D                                6  0
007100030315     D                                 n
007200030315     D                                6  0
007300030315     D                                1
007400030318     D                                6  0
007500030318     D                                6  0
007600030318     D                               25
007700091010     D                                3    const
007800030315     **
007900030317     D                 ds                  inz
008000030317     D  divtypeDS                    10  0
008100030317     D  div_list                      2  0 dim(5) overlay(divtypeDS)
008200030318     **
008300030318     D                 ds
008400030318     D  accountDS                    25
008500030318     D  citiacc                       1    dim(9) overlay(accountDS)
008600030315     **
008700091012     Dciti_err         ds                  dim(100) qualified
008800030315     D e_payemp                       6  0
008900090618     D e_payacc                      30
009000030315     D e_paycrd                       6  0
009100030315     D e_payname                     26
009200091014
009300091014     D payelcDS      e ds                  extname(payelctrn) inz
009400091014     D                                     qualified prefix('P':1)
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
012600090105     D PGM1RBG         c                   'RB0001R'
012700090105     D PGM2CITI        c                   'CITI04R'
012800090105     D PGM2RBG         c                   'RB0002R'
012900090106     D PGM3CITI        c                   'CITI00C'
013000090106     D PGM3RBG         c                   'RB0000C'
013100090106     D PGM4CITI        c                   'CITI01C'
013200090106     D PGM4RBG         c                   'RB0001C'
013300081211     D COMPLETETEXT    c                   'COMPLETED'
013400081211     D FULPERCENT      c                   100
013500090120     d DIGITS          c                   '0123456789'
013600030318     **
013700030315     D msg1            c                   'Preparing to transmit data'
013800030315     D msg2            c                   'Opening communication line ....'
013900030315     D msg3            c                   'Validating data ...............'
014000030315     D msg4            c                   'Processing data ...............'
014100031217     D msg5            c                   'Closing communication line : PRESS -
014200030411     D                                      ENTER to continue'
014300030315     D msg6            c                   'Error found while processing...'
014400030315     D msg7            c                   'No batch available for transfer'
014500030317     D msg8            c                   'Invalid selection or Parameters not-
014600030317     D                                      configured'
014700030317     D msg9            c                   'No Transaction Found for transfer'
014800030318     D msg10           c                   'Invalid Payment division'
014900090107     D msg11           c                   'Batch range setup incorrectly'
015000090107     D msg12           c                   'Function Key not valid on selected -
015100090107     D                                     field'
015200980103     **
015300980103     D                sds
015400980103     D  @pgm             *proc
015500980103     D  @user                254    263
015600030315
015700030315      /free
015800030315
015900081212           authority_check('PAY':@pgm);
016000081212          // Initial Setup
016100081212           exsr  aainit;
016200030315
016300081212          // Main routine
016400091014         electronic_transfer('PAY');
016500030315
016600030315        //***************************************************
016700030315        //** First initilisation routine for all programs  **
016800030315        //***************************************************
016900030315         begsr aainit;
017000030315
017100081212           // Get Company Name
017200081212           coname = get_coname;
017300081212           @DATE = get_sys_date();
017400081212           // select header for program
017500081212           @title = arr(1);
017600030315
017700030315         endsr;
017800030318
017900030315      /end-free
018000030317     **
018100030317     Oqsysprt   E            HEDR           1 04
018200030317     O                       @pgm                10
018300030317     O                       coname              75
018400030317     O                       @date              128
018500030317     O          E            hedr           1
018600030317     O                       @user               10
018700030317     O                       coname_lines        75
018800030317     O          E            hedr           1
018900030317     O                       @title              80
019000030317     O                                          123 'PAGE:'
019100030317     O                       PAGE               128
019200030317     O          E            hedr           1
019300030317     O                                           11 'Employee'
019400030317     O                                           50 'Account No.'
019500030317     O                                           65 'Credit Date'
019600030317     **
019700030317     O          E            detail         1
019800030317     O                       payemp        z      8
019900030317     O                       payname           +  1
020000030317     O                       payacc              50
020100030317     O                       paycrd        y     65
020200030317     O          E            final_prt   2  2
020300030317     O                                           75 'End of Report'
020400081212
020500081212       //************************************                               **
020600081212       // Electric Transfer Main routine
020700081212       //************************************                               **
020800081212     P Electronic_transfer...
020900081212     P                 B
021000081212     D Electronic_transfer...
021100081212     D                 PI
021200091014     D origin                         3    const
021300090120
021400090120     D completeInd     s               n
021500091014
021600091014     D payelcDS      e ds                  extname(payelctrn) inz
021700091014     D                                     qualified prefix('P':1)
021800091014
021900091014     D shrelcDS      e ds                  extname(shrelctrn) inz
022000091014     D                                     qualified prefix('P':1)
022100091014
022200081212      /free
022300081212        //**************************
022400081212        //*** Start of Program   ***
022500081212        //**************************
022600081212          dow  0 = 0;
022700081212
022800081212           exfmt  fmt1;
022900081212
023000081212           // Terminate program
023100081212            if  *in03;
023200081212               *inlr = *on;
023300081212               return;
023400081212            endif;
023500081212
023600091014
023700091014            select;
023800091014              when origin  = 'PAY';
023900091014                exec sql
024000091014                  select * into :payelcDS from payelctrn
024100091014                    where petype = :banksel;
024200091014
024300091014
024400091014               // if called from shares system
024500091014              when origin  = 'SHR';
024600091014                exec sql
024700091014                  select * into :ShrelcDS from shrelctrn
024800091014                    where setype = :banksel;
024900091014
025000091014                eval-corr payelcDS = shrelcDS;
025100091014            endsl;
025200091014
025300081212            select;
025400081212           // Setup transfer parameters
025500081212              when  option = 1;
025600081212                 dow 0 = 0;
025700091014
025800081212                   exfmt fmt2;
025900081212                   errmsg = *blanks;
026000081212                   if *in02;
026100081212                     leave;
026200081212                   endif;
026300081212
026400091014           ////////clear pelcfmt;
026500091014
026600081212                   select;
026700081212                    when banksel = 1;
026800091014                     FI_setup(origin:payelcDS);
026900081212                    when banksel = 2;
027000091014                     FI_setup(origin:payelcDS);
027100090805                    when banksel = 5;
027200091014                     FI_setup(origin:payelcDS);
027300081212                   endsl;
027400081212                 enddo;
027500081212
027600081212           // Processing transactions (electronic transfer)
027700081212              when  option = 2;
027800081212                 dow 0 = 0;
027900081212
028000081212                   exfmt fmt2;
028100081212                   errmsg = *blanks;
028200081212                   errmsg2 = *blanks;
028300081212                   errmsg3 = *blanks;
028400081212                   errmsg4 = *blanks;
028500081212
028600090119                  // Go back
028700090120                   if *in02 or
028800090120                       completeInd;
028900081212                     leave;
029000081212                   endif;
029100090119
029200090119                  // Terminate program
029300090119                   if *in03;
029400090119                     leave;
029500090119                   endif;
029600081212
029700091014                   if sqlstt = '02000';
029800081212                     errmsg = msg8;
029900081212                     iter;
030000081212                   endif;
030100090105
030200090105                   // select relavant programs for processing
030300090105                   select;
030400090105                     when banksel = 1;
030500090105                       pgmname1 = PGM1CITI;
030600090105                       pgmname2 = PGM2CITI;
030700090106                       pgmname3 = PGM3CITI;
030800090106                       pgmname4 = PGM4CITI;
030900090105                     when banksel = 2;
031000090105                       pgmname1 = PGM1RBG;
031100090105                       pgmname2 = PGM2RBG;
031200090106                       pgmname3 = PGM3RBG;
031300090106                       pgmname4 = PGM4RBG;
031400090105                   endsl;
031500081212
031600081212                 leave;
031700081212                 enddo;
031800081212
031900091014                 // Perform Electronic transfer
032000091014                 divtypeDs = payelcDS.pediv;
032100081212                 for count = 1 to %elem(div_list);
032200081212                    if div_list(count) <> *zeros;
032300081212                      bnk_type = div_list(count);
032400091014                      if transfer_rtn(origin);
032500090120                         completeInd = *on;
032600090120                      endif;
032700081212                    endif;
032800081212                 endfor;
032900090119                 banksel = *zeros;
033000081212              other;
033100081212               errmsg = get_err_msg('DIH0026');
033200081212                 iter;
033300081212            endsl;
033400081212
033500081212           enddo;
033600081212      /end-free
033700081212     P Electronic_transfer...
033800081212     P                 E
033900081212
034000081212
034100081212       //************************************                               **
034200081212       // Transfer/preparation routine
034300081212       //************************************                               **
034400081212     P Transfer_rtn    B
034500090120     D Transfer_rtn    PI              n
034600091014     D origin                         3    const
034700081212
034800081212      /free
034900091014           write fmt4;
035000081212           progBar1Obj = createProgressBar();
035100081212           if progBar1Obj <> *Null;
035200091014             progBar1.WindowTop = 6;
035300081212             progBar1.WindowTitle = 'Validate Transactions';
035400081212             progBar1.Color = COLOR_BLUE;
035500081212             progBar1.ShowPercent = *ON;
035600081212           endif;
035700081212
035800081212           valper = *zeros;
035900081212           proper = *zeros;
036000081212           errmsg = *blanks;
036100081212           errmsg2 = *blanks;
036200081212           errmsg3 = *blanks;
036300081212           errmsg4 = *blanks;
036400081212
036500081212          // get total records
036600081212           errmsg = msg1;
036700091014           write fmt5;
036800091014           get_tot_records(bnk_type:bnk_rrn:origin);
036900081212            if bnk_rrn = *zeros;
037000081212               errmsg = msg9;
037100090120               return *off;
037200081212            endif;
037300081212
037400081212          // Open communications
037500081212          //---------------------
037600081212           %subst(errmsg:33:9) = COMPLETETEXT;
037700081212           errmsg2 = msg2;
037800091014           write fmt5;
037900081212           open_comm();
038000081212
038100081212          // Validate data
038200081212          //--------------
038300081212           %subst(errmsg2:33:9) = COMPLETETEXT;
038400081212           errmsg3 = msg3;
038500091014           write fmt5;
038600081212           eof = *off;
038700081212           TimeStart = %time();
038800081212           for count = 0 to bnk_rrn;
038900081212             validate_data(citi_err:err_count:
039000091014                             eof:count:bnk_type:origin);
039100081212              if  eof;
039200081212               dow count <= bnk_rrn;
039300081212                 Validation_dsp();
039400081212                 count += 1;
039500081212               enddo;
039600081212               leave;
039700081212              endif;
039800081212
039900081212               Validation_dsp();
040000081212         endfor;
040100081212
040200081212           // Fix data error
040300081212           //---------------
040400081212         if err_count > *zeros;
040500081212            close_comm();
040600091014            fix_error(origin);
040700090120            return *off;
040800081212         endif;
040900081212
041000081212          // Process data
041100081212          //-------------
041200081212        progBar2Obj = createProgressBar();
041300081212        if progBar2Obj <> *Null;
041400091014          progBar2.WindowTop = 14;
041500081212          progBar2.WindowTitle = 'Process Transactions';
041600081212          progBar2.Color = COLOR_BLUE;
041700081212          progBar2.ShowPercent = *ON;
041800081212        endif;
041900081212
042000081212           %subst(errmsg3:33:9) = COMPLETETEXT;
042100090119           errmsg4 = msg4;
042200091014           write fmt5;
042300081212           eof = *off;
042400081212           err_ind = *blank;
042500081212           TimeStart = %time();
042600081212           for count = 1 to bnk_rrn;
042700081212             process_data(bnk_type:count:eof:
042800091014                       bnk_rrn:err_ind:payelcDS.pelower:payelcDS.peupper:
042900091014                                 payelcDS.peacct:origin);
043000081212              if eof;
043100081212               errmsg = msg5;
043200081212               close_rtn();
043300090120               return *on;
043400081212              endif;
043500081212
043600081212              if err_ind <> *blank;
043700081212               select;
043800081212                 when err_ind = '1';
043900081212                  errmsg = msg6;
044000081212                 when err_ind = '2';
044100081212                  errmsg = msg7;
044200081212               endsl;
044300081212               close_rtn();
044400090120               return *off;
044500081212              endif;
044600081212
044700081212             progress_dsp();
044800081212         endfor;
044900081212
045000081212         %subst(errmsg4:33:9) = COMPLETETEXT;
045100091014         write fmt5;
045200081212         close_rtn();
045300090120         return *on;
045400081212
045500081212      /end-free
045600081212     P Transfer_rtn    E
045700081212
045800081212       //************************************                               **
045900081212       // Financial Institution Config
046000081212       //************************************                               **
046100081212     P FI_setup        B
046200081212     D FI_setup        PI
046300091014     D  origin                        3    const
046400091014     D  elcDS                              likeds(payelcDS)
046500091014
046600091014     D*screlcDS        ds                  likerec(fmt3:*all)
046700091014     D scrDS         e ds                  extname(payr010dnw:fmt3:*all)
046800091019     D                                     qualified prefix('P':1)
046900091014     D dummy           s              1
047000081212
047100081212      /free
047200090107          // prepare screen fields
047300091014          eval-corr scrDS = elcDS;
047400091014          elcDS.pevpgmid = %editc(banksel:'X') +
047500090107                     %subst(@pgm:3);
047600090107
047700081212          dow  0 = 0;
047800091014            exfmt fmt3 scrDS;
047900081212            errmsg = *blanks;
048000081212
048100081212            if *in02;
048200081212              return;
048300081212            endif;
048400090106
048500090107            // Prompt for list/maintenance program
048600090107            if  *in04;
048700090107               select;
048800090107                 // Transit number range Maintenance
048900090107                 when fld = 'PEDFTTRN#';
049000090108                   maintTransitData();
049100090107                 // ALL other selections
049200090107                 other;
049300090107                   errmsg = msg12;
049400090107               endsl;
049500090107               iter;
049600090107            endif;
049700090107
049800090107            // F7 = Create/Modify Mailing List
049900090106            if *in07;
050000091014              email_list(origin:elcDS.pevpgmid);
050100090106              iter;
050200090106            endif;
050300081212
050400081212            // Validate account number;
050500091014             accountDS = scrDS.peacct;
050600091014         //  accountDS = scrDS.#eacct;
050700091014            for count = 1 to %elem(citiacc);
050800090107               if  %scan(citiacc(count):digits) = 0;
050900090107               //   errmsg = get_err_msg('DIH0059');
051000090107                  leave;
051100081212               endif;
051200091014            endfor;
051300091014
051400091014            if  errmsg <> *blanks;
051500091014               iter;
051600091014            endif;
051700090107
051800081212            // Validate batch range;
051900091014            if scrDS.peupper < scrDS.pelower
052000091014              or scrDS.peupper = *zeros
052100091014               or scrDS.pelower = *zeros;
052200091014                  errmsg = msg11;
052300091014                  iter;
052400090107            endif;
052500081212
052600091014            divtypeDS = scrDS.pediv;
052700081212            // Validate configuration;
052800081212              for count = 1 to %elem(div_list);
052900081212                 if div_list(count) <> *zeros;
053000091012                   bnk_type = div_list(count);
053100091012
053200091012                   exec sql
053300091012                     select dtno into :hold_code from paydiv
053400091012                      where dtno = :bnk_type and dtrsts = 'A';
053500091012                   if sqlstt = '02000';
053600091012                      errmsg = msg10;
053700091012                      iter;
053800091012                   endif;
053900081212                 endif;
054000081212               endfor;
054100081212
054200081212            // write configuration;
054300090107                 // prepare database fields
054400091014                 eval-corr elcDS = scrDS;
054500090107
054600091014                  select;
054700091014                   when origin = 'PAY';
054800091014                   exec sql
054900091014                  // select * into :payelcDS from payelctrn
055000091014                     select petype into :dummy from payelctrn
055100091014                       where petype = :banksel;
055200091014                   if sqlstt = '02000';
055300091014                     exec sql
055400091014                       insert into payelctrn values(:elcDS);
055500091014                     else;
055600091014                     exec sql
055700091014                       update payelctrn set petype = :banksel;
055800091014                   endif;
055900091014                   when origin = 'SHR';
056000091014                   exec sql
056100091014                     select setype into :dummy from shrelctrn
056200091014                       where setype = :banksel;
056300091014                   if sqlstt = '02000';
056400091014                     exec sql
056500091014                       insert into shrelctrn values(:elcDS);
056600091014                     else;
056700091014                     exec sql
056800091014                       update shrelctrn set setype = :banksel;
056900091014                   endif;
057000091014                  endsl;
057100081212
057200081212             return;
057300081212          enddo;
057400081212
057500081212      /end-free
057600081212     P FI_setup        E
057700081212
057800081212       //************                                                       **
057900081212       // Fix error
058000081212       //************                                                       **
058100081212     P Fix_error       B
058200081212     D Fix_error       PI
058300091014     D  origin                        3    const
058400081212      /free
058500081212
058600081212            *in33 = *on;
058700081212              write  window;
058800081212            write  wdwctl;
058900081212            *in33 = *off;
059000081212
059100081212
059200081212            // load subfile
059300081212            for rrn = 1 to err_count;
059400091012                   payemp = citi_err(rrn).e_payemp;
059500091012                   paycrd = citi_err(rrn).e_paycrd;
059600091012                   payacc = citi_err(rrn).e_payacc;
059700091012                   oldpayacc = citi_err(rrn).e_payacc;
059800091012                   payname = citi_err(rrn).e_payname;
059900081212                   write   winrec;
060000081212            endfor;
060100081212
060200081212            dow  0 = 0;
060300081212              exfmt  wdwctl;
060400081212
060500081212            // F3 = End Program
060600081212              if  *in03;
060700081212                 *inlr = '1';
060800081212                 return;
060900081212              endif;
061000081212
061100081212              if  *in05;
061200081212                 Print_error_list();
061300081212                 iter;
061400081212              endif;
061500081212
061600081212               readc  winrec;
061700081212               dow  not(%eof);
061800091014                correct_error(bnk_type:oldpayacc:payacc:origin);
061900081212                oldpayacc = payacc;
062000081212                update winrec;
062100081212               readc  winrec;
062200081212               enddo;
062300081212            enddo;
062400081212      /end-free
062500081212     P Fix_error       E
062600081212
062700081212       //***************                                                    **
062800081212       // Close routine
062900081212       //***************                                                    **
063000081212     P Close_rtn       B
063100081212     D Close_rtn       PI
063200081212      /free
063300081212         close_comm();
063400081212         destroyProgressBar(progBar1Obj);
063500081212         destroyProgressBar(progBar2Obj);
063600091014         *in99 = *on;
063700091014         exfmt fmt5;
063800091014         *in99 = *off;
063900091014         errmsg = *blanks;
064000081212      /end-free
064100081212     P Close_rtn       E
064200081212
064300081212       //*********************                                              **
064400081212       // Validation display
064500081212       //*********************                                              **
064600081212     P Validation_dsp  B
064700081212     D Validation_dsp  PI
064800081212      /free
064900081212             valper = count * fulpercent / bnk_rrn;
065000081212             //(Below 5% the estimated time is probably badly misleading)
065100081212               If  valPer > 5;
065200081212                     TimeNow = %time();
065300081212                     // Time elapsed so far:?
065400081212                     TotalSecs = %diff(TimeNow :
065500081212                                          TimeStart : *seconds);
065600081212                     TotalSecs = TotalSecs/valper * (100 - valper);
065700081212                     TimeHMS = T'00.00.00' + %seconds(TotalSecs);
065800081212
065900081212               Endif;
066000081212
066100081212              progBar1.Percent = valper;
066200081212              pctA = %editw(progBar1.Percent: '  0%');
066300081212              evalr progBar1.StatusMessage =
066400081212                                   %char(TimeHMS) + ' remaining  ';
066500081212              displayProgressBar(progBar1Obj);
066600081212      /end-free
066700081212     P Validation_dsp  E
066800081212
066900081212
067000081212       //*********************                                              **
067100081212       // Progress display
067200081212       //*********************                                              **
067300081212     P Progress_dsp    B
067400081212     D Progress_dsp    PI
067500081212      /free
067600081212             proper = count * fulpercent / bnk_rrn;
067700081212             //(Below 5% the estimated time is probably badly misleading)
067800081212               If  proPer > 5;
067900081212                     TimeNow = %time();
068000081212                     // Time elapsed so far:?
068100081212                     TotalSecs = %diff(TimeNow :
068200081212                                          TimeStart : *seconds);
068300081212                     TotalSecs = TotalSecs/proPer * (100 - proPer);
068400081212                     TimeHMS = T'00.00.00' + %seconds(TotalSecs);
068500081212
068600081212               Endif;
068700081212
068800081212           progBar2.Percent = proper;
068900081212           pctA = %editw(progBar2.Percent: '  0%');
069000081212           evalr progBar2.StatusMessage =
069100081212                                   %char(TimeHMS) + ' remaining  ';
069200081212           displayProgressBar(progBar2Obj);
069300081212      /end-free
069400081212     P Progress_dsp    E
069500081212
069600081212       //**************************                                         **
069700081212       // Print error list routine
069800081212       //**************************                                         **
069900081212     P Print_error_list...
070000081212     P                 B
070100081212     D Print_error_list...
070200081212     D                 PI
070300081212      /free
070400081212
070500081212           // Print Header
070600081212            except hedr;
070700081212           // Print Detail
070800081212            for count = 1 to err_count;
070900081212                chain count winrec;
071000081212                except detail;
071100081212            endfor;
071200081212           // Print last record
071300081212            except final_prt;
071400081212      /end-free
071500081212     P Print_error_list...
071600081212     P                 E
071700030315     **
071800030315**  Program Title
071900030315      Electronic Payroll Transfer
