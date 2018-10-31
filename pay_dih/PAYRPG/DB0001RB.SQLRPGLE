000100091019     H dftactgrp(*no) actgrp(*CALLER) bnddir('DISDIR')
000200090106     H******  PROGRAMMER - S. BRITTON , c.williams
000300090106     H******  First Coded - 09/01/06
000400100923     Fbnkpysuml4uf   e           k disk    commit  usropn
000500091010     Fshrpysuml4uf   e           k disk    commit  usropn
000600100923     Fcuspysuml4uf   e           k disk    commit  usropn
000700991208     **
000800000707     Daccount_fld      ds
000900000707     D payacc
001000090623     D  payarr                        1    overlay(payacc) dim(30)
001100000707     **
001200090106     DCiti_acc_fld     ds
001300100209     D bankacc                       16  0
001400100215     D bankarr                        1    overlay(bankacc) dim(16)
001500000707     **
001600100923     D payDS         e ds                  extname(bnkpysum) inz
001700100629     d                                     qualified
001800100629     D shrDS         e ds                  extname(shrpysum) inz
001900100923     D cusDS         e ds                  extname(cuspysum) inz
002000100922     d                                     qualified
002100100629
002200090106     D payelcDS      e ds                  extname(payelctrn) inz
002300091013     D                                     qualified prefix('P':1)
002400091013
002500091013     D shrelcDS      e ds                  extname(shrelctrn) inz
002600091013     D                                     qualified prefix('P':1)
002700090105
002800100209     D cuselcDS      e ds                  extname(cuselctrn) inz
002900100209     D                                     qualified prefix('P':1)
003000000711     **
003100090105     Dciti_err         ds                  template
003200000711     D e_payemp                       6  0
003300000711     D e_payacc                      12
003400000711     D e_paycrd                       6  0
003500000711     D e_payname                     26
003600090105
003700090106     Drb_struc         ds                  qualified  dim(30) inz
003800091013     D rb_transdet                         likeDS(rb_data) dim(1500)
003900100209     D rb_transsum                         likeDS(rb_summ)
004000090105
004100090105     D rb_summ         ds                  template
004200090105     D rb_amount                     15  2
004300100209     D rb_cracc                      16  0
004400090105
004500090105     Drb_data          ds                  template
004600100209     D rb_account                    16  0
004700100209     D rb_comma                       1    inz(',')
004800090105     D rb_total                      15  2
004900090105
005000000707     **
005100090105
005200100922     Ddb_text_header   ds
005300100922     D db_head_trn                   16     inz('Transit#')
005400100922     D db_text_comma                  1    inz(',')
005500100922     D db_head_acct                  16     inz('Bank Account')
005600100922     D db_text_comma2                 1    inz(',')
005700100922     D db_head_amt                    7     inz('Amount')
005800090105
005900100922     D*db_hder_record   ds
006000100922     D* db_hdr_trn                     3  0 inz(100)
006100100922     D* db_hdr_comma                   1    inz(',')
006200100922     D* db_hdr_acc                    16  0
006300100922     D* db_hdr_comma2                  1    inz(',')
006400100922     D* db_amount                     15  0
006500100209
006600100922     Ddb_text_detail   ds
006700100922     D db_det_trn                     3  0 inz(100)
006800100922     D db_det_comma                   1    inz(',')
006900100922     D db_det_account                16  0
007000100922     D db_det_comma2                  1    inz(',')
007100100922     D db_det_total                  15  0
007200090105
007300091019
007400091019      // General PROTOTYPES
007500091019      /copy GENPR
007600090105
007700090106     D DIGITS          c                   '0123456789'
007800090106     D CRLF            c                   x'0D25'
007900090106
008000090623     D xcnt            s              2  0 inz(30)
008100100210     D ycnt            s              2  0 inz(16)
008200100215     D fnumber         s              1  0 inz(0)
008300090105     D acnt            s              4  0
008400090105     D gcnt            s              4  0
008500090107     D crddate         s               d   datfmt(*jul)
008600090107     D julDate         s              7  0
008700090106     D seqcnt          s              6  0
008800100215     Daccount_wtr      s             16  0
008900100215     Damount_wtr       s             15  2
009000090107       // Identify BANK/FI
009100100209     D cbtype          s              2  0 inz(1)
009200090106     D rbarr           s              8  0 dim(30)
009300100209     D comma           s              1    inz(',')
009400090107     D zeros           s              9  0 inz(*zeros)
009500090107     D year            s              4  0
009600000209     **
009700090105     D*--------------------------------------
009800090105     D* Prototype for procedure: get_bank_acc
009900090105     D*--------------------------------------
010000090105     D get_bank_acc    PR
010100090105
010200090105
010300090105     D*-----------------------------------------
010400090106     D* Prototype for procedure: prepare_batch
010500090105     D*-----------------------------------------
010600100210     D sendBatch       PR
010700090105
010800090106     D*-----------------------------------------
010900090106     D* Prototype for procedure: write_to_file
011000090106     D*-----------------------------------------
011100090106     D write_to_file   PR
011200090106     D option                         1    const
011300090106        // '1' - create/open file
011400090106        // '2' - write data
011500090106        // '3' - close file
011600090106     D data                         100    const varying options(*nopass)
011700090107
011800090107     D write_batch_detail...
011900090107     D                 PR
012000100209     D account                       16  0 const
012100100209     D space                          1    const
012200100215     D total                         15  0 const
012300090106
012400090106     D*-----------------------------------------
012500090106     D* Prototype for procedure: Main_pgm
012600090106     D*-----------------------------------------
012700100922     D  main_pgm       pr                  extpgm('DB0001R')
012800090105     D   citi_errDS                        likeds(citi_err) dim(100)
012900090105     D   err_count                    6  0
013000090105     D   eof                           n
013100090105     D   count                        6  0
013200090105     D   bnk_type                     2  0
013300091010     D   origin                       3    options(*nopass)
013400090106
013500090106     D  main_pgm       pi
013600090106     D   citi_errDS                        likeds(citi_err) dim(100)
013700090106     D   err_count                    6  0
013800090106     D   eof                           n
013900090106     D   count                        6  0
014000090106     D   bnk_type                     2  0
014100091010     D   origin                       3    options(*nopass)
014200091019     **
014201100927
014202100927     Dtemp_rrn         S              6  0
014300100210     D*--------------------------------------------------
014400100210     D* Procedure name: open_File
014500100210     D* Purpose:        open file, set cursor to retrieve data for sending ...
014600100210     D*                          to IFS
014700100210     D* Returns:
014800100210     D* Parameter:      fileNo
014900100210     D*--------------------------------------------------
015000100210     D open_File       PR
015100100210     D  fileNo                        1P 0
015200100210
015300100210     D*--------------------------------------------------
015400100210     D* Procedure name: closeFile
015500100210     D* Purpose:        close selected file
015600100210     D* Returns:
015700100210     D* Parameter:      fileNo
015800100210     D*--------------------------------------------------
015900100210     D closeFile       PR
016000100210     D  fileNo                        1P 0
016100100210
016200100210     D*--------------------------------------------------
016300100210     D* Procedure name: readFile
016400100210     D* Purpose:
016500100210     D* Returns:
016600100210     D* Parameter:      fileNo
016700100210     D*--------------------------------------------------
016800100210     D readFile        PR              N
016900100210     D  fileNo                        1P 0
017000100210
017100100210
017200100210
017300100210
017400090105      /free
017500090105              // Get Division and Bank account information from file
017600090106              if  count = 0;
017700091010                select;
017800091013                  when origin = 'PAY';
017900100923                    open bnkpysuml4;
018000100927                    setll bnk_type payfmt;
018001100927
018100091013                  when origin = 'SHR';
018200091013                    open shrpysuml4;
018300091013                    setll bnk_type sbkfmt;
018400100215                  when origin = 'CRD';
018500100923                    open cuspysuml4;
018600100923                    setll bnk_type cbkfmt;
018700091010                endsl;
018800100927          //  endif;
018900090105
019000091010                select;
019100091013                  when origin = 'PAY';
019200100923                    reade bnk_type payfmt;
019201100927
019300100215         //             paycrd = get_credit_date(paycrd:paydate);
019400100215
019500100210                  when origin = 'CRD';
019600100923                    reade bnk_type cbkfmt;
019700091013                  when origin = 'SHR';
019800091013                    reade bnk_type sbkfmt;
019900091013                    payamt = sbkamt;
020000091010                endsl;
020100091019
020200090106
020300100927         //     if  count = 0;
020400091010
020500090107
020600091010               select;
020700091013                 when origin  = 'PAY';
020800091013                   exec sql
020900091013                     select * into :payelcDS from payelctrn
021000100209                       where petype = :cbtype;
021100090106
021200100629               // get_bank_acc();
021300091010
021400091013                  // if called from shares system
021500091013                 when origin  = 'SHR';
021600091013                   exec sql
021700091013                     select * into :ShrelcDS from shrelctrn
021800100209                      where setype = :cbtype;
021900091013
022000091013                   eval-corr payelcDS = shrelcDS;
022100100209                  // if called from shares system
022200100209                 when origin  = 'CRD';
022300100209                   exec sql
022400100209                     select * into :cuselcDS from cuselctrn
022500100215                      where cetype = :cbtype;
022600100209
022700100209                   eval-corr payelcDS = cuselcDS;
022800100215               endsl;
022900091013
023000100209               // banks acct field to cb acct field
023100100215               payacc = %subst(payelcDS.peacct:1:16);
023200091013
023300100927           //   endif;
023400090105
023500090105              if   not(%eof());
023600090105                // get accounting info.
023700100215
023800090105                get_bank_acc();
023900100210                sendBatch();
024000100927
024100100210               select;
024200100210               when origin = 'PAY';
024300100927                dow not(%eof(payfmt));
024400091010                paysts = 'V';
024500091010                update  payfmt %fields(paysts);
024501100927
024600100923                    reade bnk_type payfmt;
024700100215                enddo;
024800100210               when origin = 'SHR';
024900100927                   dow not(%eof(sbkfmt));
025000100629                       sbksts = 'V';
025001100927
025100100629                       update  sbkfmt %fields(sbksts);
025200100629                       reade bnk_type sbkfmt;
025300100629                enddo;
025400100210               when origin = 'CRD';
025500100629                  dow not(%eof());
025600100927                      cbksts = 'V';
025601100927
025700100923                        update  cbkfmt %fields(cbksts);
025800100923                        reade bnk_type cbkfmt;
025900100629                enddo;
026000100210               endsl;
026100090105              endif;
026200090105
026300090105              if  %eof();
026301100927
026306100927
026307100927
026400090105                 eof = *on;
026500090105                 *inlr = *on;
026600100209                 select;
026700100209                 when origin = 'PAY';
026701100927                    exec sql
026702100927                     select count(*) into temp_rrn from bnkpysum where
026703100927                       paydiv = 9 and paysts = 'V';
026704100927                    dsply temp_rrn;
026800100923                   close bnkpysuml4;
026801100927               //
026804100927                  //
026900100209                 when origin = 'SHR';
027000091013                   close shrpysuml4;
027100100209                 when origin = 'CRD';
027200100923                   close cuspysuml4;
027300100209                 endsl;
027400091013
027500100215
027600090105              endif;
027700100927            endif;
027800090105
027900090105             // RETURN
028000090105             return;
028100090105      /end-free
028200090105
028300090105     P*-----------------------------------------
028400090105     P* Procedure name: get_bank_acc
028500090105     P* Purpose:        get actual bank account
028600090105     P* Returns:        0
028700090105     P* Parameter:      0
028800090105     P*-----------------------------------------
028900090105     P get_bank_acc    B
029000090106     D get_bank_acc    PI
029100090105
029200090105      /free
029300090105          bankacc =  *zeros;
029400100629           //   select;
029500100629           //   when origin = 'PAY';
029600091013            // already set
029700100629          //  when origin = 'SHR';
029800100629          //   payacc = sbkacc;
029900100629          //  when origin = 'CRD';
030000100629          //   payacc = cbkacc;
030100100629          //endsl;
030200090105
030300090105          // check for number in character field
030400090105          dow  xcnt > 0;
030500090105
030600090106              if  %check(digits:payarr(xcnt)) = 0
030700090105                      and ycnt > 0;
030800090105                bankarr(ycnt) = payarr(xcnt);
030900090105                ycnt -=  1;
031000090105              endif;
031100090105
031200090105              xcnt -=  1;
031300090105          enddo;
031400090105
031500090105          // Reset array fields
031600090105          reset  xcnt;
031700090105          reset  ycnt;
031800090105
031900090105      /end-free
032000090105     P get_bank_acc    E
032100090105
032200090105
032300090105
032400090105     P*-----------------------------------------
032500090106     P* Procedure name: prepare_batch
032600090106     P* Purpose:        prepare batch
032700090105     P* Returns:        0
032800090105     P* Parameter:      0
032900090105     P*-----------------------------------------
033000100210     P sendBatch       B
033100100210     D sendBatch       PI
033200090105
033300100210     Dsum_total        S             15  2 inz(0)
033400100210
033500090105      /free
033600090105
033700090105         //  write header record;
033800090106         //  ...create/open file;
033900090107         //----------------------
034000090106         write_to_file('1');
034100100922         write_to_file('2':Db_text_header);
034200100922          // select;
034300100922          //  when origin = 'PAY';
034400100922           //  exec sql
034500100924            //   select sum(payamt) into :sum_total from bnkpysuml1
034600100922              //  where paysts =  'A' and payorg = :origin
034700100922              //and paydiv = :bnk_type;
034800100922            //when origin = 'SHR';
034900100922            // exec sql
035000100922             //  select sum(sbkamt) into :sum_total from shrpysuml1
035100100922              //  where sbksts =  'A' and sbkdiv = :bnk_type;
035200100629
035300100922            //when origin = 'CRD';
035400100922             //exec sql
035500100924              // select sum(payamt) into :sum_total from bnkpysuml1
035600100922               // where paysts =  'A'
035700100922                //and payorg = :origin and paydiv = :bnk_type;
035800100922           // endsl;
035900100922            //Db_amount = sum_total;
036000100922            //Db_hdr_acc = bankacc;
036100100922         //write_to_file('2':db_hder_record);
036200090105
036300100209
036400100209
036500100209
036600090105         //  write detail records;
036700090107         //------------------------
036800100629          select;
036900100629          when origin = 'PAY';
037000100215            fnumber = 1;
037100100629          when origin = 'SHR';
037200100629            fnumber = 2;
037300100629          when origin = 'CRD';
037400100629            fnumber = 3;
037500100629          endsl;
037600100629
037700100215             open_file(fnumber);
037800100215             dow not readFile(fnumber);
037900100210
038000100629          select;
038100100629          when origin = 'PAY';
038200100215              payacc = payDS.payacc;
038300100629             amount_wtr  = payDs.payamt;
038400100629          when origin = 'SHR';
038500100629              payacc = shrDS.sbkacc;
038600100629             amount_wtr  = shrDs.sbkamt;
038700100629          when origin = 'CRD';
038800100923              payacc = cusDS.cbkacc;
038900100923             amount_wtr  = cusDs.cbkamt;
039000100629          endsl;
039100100215               get_bank_acc();
039200100215              account_wtr = bankacc;
039300090107                // write detail figures
039400100215               write_batch_detail(account_wtr:comma:amount_wtr);
039500100215
039600100210             enddo;
039700090107
039800100215            closeFile(fnumber);
039900090107
040000090107
040100090107
040200090107
040300090106             //close file
040400090106             write_to_file('3');
040500090105
040600090105      /end-free
040700100210     P sendBatch       E
040800090106
040900090106     P*-----------------------------------------
041000090106     P* Procedure name: write_to_file
041100090106     P* Purpose:        write to file
041200090106     P* Returns:        0
041300090106     P* Parameter:      0
041400090106     P*-----------------------------------------
041500090106     P write_to_file   B
041600090106     D write_to_file   PI
041700090106     D option                         1    const
041800090106     D data                         100    const varying options(*nopass)
041900090106
042000090106     D/copy IFSPR
042100090106     D fd              S             10I 0
042200090106     D wrdata          S            100A
042300090106     D rddata          S            100A
042400090106     D flags           S             10U 0
042500090106     D mode            S             10U 0
042600090106     D Msg             S             50A
042700090106     D Len             S             10I 0
042800090106      /free
042900090106
043000090106       //*************************************************************
043100090106       // writing data to a stream file
043200090106       //****************************************************************
043300090106         select;
043400090106           when option = '1';
043500090107
043600091013               fd = unlink(payelcDS.pefilenm);
043700090107
043800090107             flags = O_WRONLY + O_CREAT + O_TRUNC +
043900090106                     O_TEXTDATA + O_CODEPAGE;
044000090106
044100090106             mode =  S_IRUSR + S_IWUSR
044200090106                         + S_IRGRP + S_IROTH;
044300090106
044400090107             // Open IFS file in ASCII format
044500091010
044600091013             fd = open(payelcDS.pefilenm: flags: mode: 819);
044700091013
044800090106             if  fd < 0;
044900090107             // Msg = 'open(): failed for writing';
045000090107             // return;
045100090106             endif;
045200090106
045300090107             // Close IFS file
045400090106             callp  close(fd);
045500090107
045600090107             // Reopen FS file
045700091013              fd = open(payelcDS.pefilenm:O_WRONLY + O_TEXTDATA);
045800090106
045900090106           // Write some data
046000090106           when option = '2';
046100090106             wrdata = data + CRLF;
046200090106             callp write(fd: %addr(wrdata): %len(%trimr(wrdata)));
046300090106
046400090106           // close the file
046500090106           when option = '3';
046600090106             callp  close(fd);
046700090106         endsl;
046800090106
046900090106      /end-free
047000090106     P write_to_file   E
047100090107
047200090107     P*-----------------------------------------
047300090107     P* Procedure name: write_batch_detail
047400090107     P* Purpose:        write batch detail lines
047500090107     P* Returns:        0
047600090107     P* Parameter:      0
047700090107     P*-----------------------------------------
047800090107     P write_batch_detail...
047900090107     P                 B
048000090107     D write_batch_detail...
048100090107     D                 PI
048200100209     D account                       16  0 const
048300100209     D space                          1    const
048400100215     D total                         15  0 const
048500090107      /free
048600090107               //write detail lines
048700100922               db_det_account= account;
048800100922               db_det_comma = space;
048900090107
049000100922               db_det_total = total;
049100100922               write_to_file('2':db_text_detail);
049200090107      /end-free
049300090107     P write_batch_detail...
049400090107     P                 E
049500091019
049600100210
049700100210     P*--------------------------------------------------
049800100210     P* Procedure name: open_File
049900100210     P* Purpose:        open file, set cursor to retrieve data for sending ...
050000100210     P*                          to IFS
050100100210     P* Returns:
050200100210     P* Parameter:      fileNo
050300100210     P*--------------------------------------------------
050400100210     P open_File       B
050500100210     D open_File       PI
050600100210     D  fileNo                        1P 0
050700100210
050800100210
050900100210      /FREE
051000100210           select;
051100100210             when fileno = 1;
051200100210         exec sql
051300100924        declare payfile cursor for select * from bnkpysuml1
051400100924          where paydiv = :bnk_type and paysts = 'A';
051500100924        //where payorg = :origin and paydiv = :bnk_type and paysts = 'A';
051600100210        exec sql
051700100210        open payfile;
051800100629             when fileno = 2;
051900100629         exec sql
052000100629        declare payfile2 cursor for select * from shrpysuml1
052100100629        where sbkdiv = :bnk_type and sbksts = 'A';
052200100629
052300100629        exec sql
052400100629        open payfile2;
052500100210
052600100629             when fileno = 3;
052700100629         exec sql
052800100927        declare payfile3 cursor for select * from cuspysuml1
052900100927        where cbkdiv = :bnk_type and cbksts = 'A';
052901100924          //where paydiv = :bnk_type and paysts = 'A' and payorg = :origin;
053000100629
053100100629        exec sql
053200100629        open payfile3;
053300100210           ENDSL;
053400100210
053500100210      /END-FREE
053600100210     P open_File       E
053700100210
053800100210
053900100210     P*--------------------------------------------------
054000100210     P* Procedure name: closeFile
054100100210     P* Purpose:        close selected file
054200100210     P* Returns:
054300100210     P* Parameter:      fileNo
054400100210     P*--------------------------------------------------
054500100210     P closeFile       B
054600100210     D closeFile       PI
054700100210     D  fileNo                        1P 0
054800100210
054900100210
055000100210      /FREE
055100100210
055200100210           select;
055300100210             when fileNo = 1;
055400100210               exec sql
055500100210                 close payfile;
055600100629             when fileNo = 2;
055700100629               exec sql
055800100927                 close payfile2;
055900100629             when fileNo = 3;
056000100629               exec sql
056100100629                 close payfile3;
056200100210           ENDSL;
056300100210      /END-FREE
056400100210     P closeFile       E
056500100210
056600100210
056700100210     P*--------------------------------------------------
056800100210     P* Procedure name: readFile
056900100210     P* Purpose:
057000100210     P* Returns:
057100100210     P* Parameter:      fileNo
057200100210     P*--------------------------------------------------
057300100210     P readFile        B
057400100210     D readFile        PI              N
057500100210     D  fileNo                        1P 0
057600100210
057700100210
057800100210     D* Local fields
057900100210     D retField        S               N
058000100210
058100100210      /FREE
058200100629           select;
058300100629             when fileNo = 1;
058400100922           exec sql
058500100922           fetch next from payfile into :payds;
058600100629             when fileNo = 2;
058700100922          exec sql
058800100922           fetch next from payfile2 into :shrds;
058900100629             when fileNo = 3;
059000100922          exec sql
059100100922           fetch next from payfile3 into :cusds;
059200100629          endsl;
059300100629
059400100215            if sqlstt = '02000';
059500100215           retField = *on;
059600100215
059700100215          else;
059800100215           retField = *off;
059900100922         ENDIF;
060000100210        RETURN retField;
060100100210
060200100210      /END-FREE
060300100210     P readFile        E
060400100210
