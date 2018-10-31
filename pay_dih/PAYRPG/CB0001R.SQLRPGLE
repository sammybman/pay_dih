000100091019     H dftactgrp(*no) actgrp(*CALLER) bnddir('DISDIR')
000200090106     H******  PROGRAMMER - S. BRITTON , c.williams
000300090106     H******  First Coded - 09/01/06
000400110301     Fgenpysuml4uf   e           k disk     usropn
000500991208     **
000600000707     Daccount_fld      ds
000700000707     D payacc
000800090623     D  payarr                        1    overlay(payacc) dim(30)
000900000707     **
001000090106     DCiti_acc_fld     ds
001100100209     D bankacc                       16  0
001200100215     D bankarr                        1    overlay(bankacc) dim(16)
001300000707     **
001400110120     D payDS         e ds                  extname(genpysum) inz
001500100629     d                                     qualified
001600100629
001700090106     D payelcDS      e ds                  extname(payelctrn) inz
001800091013     D                                     qualified prefix('P':1)
001900091013
002000091013     D shrelcDS      e ds                  extname(shrelctrn) inz
002100091013     D                                     qualified prefix('P':1)
002200090105
002300100209     D cuselcDS      e ds                  extname(cuselctrn) inz
002400100209     D                                     qualified prefix('P':1)
002500000711     **
002600090105     Dciti_err         ds                  template
002700000711     D e_payemp                       6  0
002800000711     D e_payacc                      12
002900000711     D e_paycrd                       6  0
003000000711     D e_payname                     26
003100090105
003200090106     Drb_struc         ds                  qualified  dim(30) inz
003300091013     D rb_transdet                         likeDS(rb_data) dim(1500)
003400100209     D rb_transsum                         likeDS(rb_summ)
003500090105
003600090105     D rb_summ         ds                  template
003700090105     D rb_amount                     15  2
003800100209     D rb_cracc                      16  0
003900090105
004000090105     Drb_data          ds                  template
004100100209     D rb_account                    16  0
004200100209     D rb_comma                       1    inz(',')
004300090105     D rb_total                      15  2
004400090105
004500000707     **
004600090105
004700100209     Dcb_text_header   ds
004800100209     D cb_head_acct                  16     inz('Bank Account')
004900100209     D cb_text_comma                  1    inz(',')
005000100209     D cb_head_amt                    7     inz('Amount')
005100090105
005200100209     Dcb_hder_record   ds
005300100209     D cb_hdr_acc                    16  0
005400100209     D cb_hdr_comma                   1    inz(',')
005500100215     D cb_amount                     15  0
005600100209
005700100209     Dcb_text_detail   ds
005800100209     D cb_det_account                16  0
005900100209     D cb_det_comma                   1    inz(',')
006000100215     D cb_det_total                  15  0
006100090105
006200091019
006300091019      // General PROTOTYPES
006400091019      /copy GENPR
006500090105
006600090106     D DIGITS          c                   '0123456789'
006700090106     D CRLF            c                   x'0D25'
006800090106
006900090623     D xcnt            s              2  0 inz(30)
007000100210     D ycnt            s              2  0 inz(16)
007100100215     D fnumber         s              1  0 inz(0)
007200090105     D acnt            s              4  0
007300090105     D gcnt            s              4  0
007400090107     D crddate         s               d   datfmt(*jul)
007500090107     D julDate         s              7  0
007600090106     D seqcnt          s              6  0
007700100215     Daccount_wtr      s             16  0
007800100215     Damount_wtr       s             15  2
007900090107       // Identify BANK/FI
008000100209     D cbtype          s              2  0 inz(1)
008100090106     D rbarr           s              8  0 dim(30)
008200100209     D comma           s              1    inz(',')
008300090107     D zeros           s              9  0 inz(*zeros)
008400090107     D year            s              4  0
008500000209     **
008600090105     D*--------------------------------------
008700090105     D* Prototype for procedure: get_bank_acc
008800090105     D*--------------------------------------
008900090105     D get_bank_acc    PR
009000090105
009100090105
009200090105     D*-----------------------------------------
009300090106     D* Prototype for procedure: prepare_batch
009400090105     D*-----------------------------------------
009500100210     D sendBatch       PR
009600090105
009700090106     D*-----------------------------------------
009800090106     D* Prototype for procedure: write_to_file
009900090106     D*-----------------------------------------
010000090106     D write_to_file   PR
010100090106     D option                         1    const
010200090106        // '1' - create/open file
010300090106        // '2' - write data
010400090106        // '3' - close file
010500090106     D data                         100    const varying options(*nopass)
010600090107
010700090107     D write_batch_detail...
010800090107     D                 PR
010900100209     D account                       16  0 const
011000100209     D space                          1    const
011100100215     D total                         15  0 const
011200090106
011300090106     D*-----------------------------------------
011400090106     D* Prototype for procedure: Main_pgm
011500090106     D*-----------------------------------------
011600100209     D  main_pgm       pr                  extpgm('CB0001R')
011700090105     D   citi_errDS                        likeds(citi_err) dim(100)
011800090105     D   err_count                    6  0
011900090105     D   eof                           n
012000090105     D   count                        6  0
012100090105     D   bnk_type                     2  0
012200091010     D   origin                       3    options(*nopass)
012300090106
012400090106     D  main_pgm       pi
012500090106     D   citi_errDS                        likeds(citi_err) dim(100)
012600090106     D   err_count                    6  0
012700090106     D   eof                           n
012800090106     D   count                        6  0
012900090106     D   bnk_type                     2  0
013000091010     D   origin                       3    options(*nopass)
013100091019     **
013200100210     D*--------------------------------------------------
013300100210     D* Procedure name: open_File
013400100210     D* Purpose:        open file, set cursor to retrieve data for sending ...
013500100210     D*                          to IFS
013600100210     D* Returns:
013700100210     D* Parameter:      fileNo
013800100210     D*--------------------------------------------------
013900100210     D open_File       PR
014000100210     D  fileNo                        1P 0
014100100210
014200100210     D*--------------------------------------------------
014300100210     D* Procedure name: closeFile
014400100210     D* Purpose:        close selected file
014500100210     D* Returns:
014600100210     D* Parameter:      fileNo
014700100210     D*--------------------------------------------------
014800100210     D closeFile       PR
014900100210     D  fileNo                        1P 0
015000100210
015100100210     D*--------------------------------------------------
015200100210     D* Procedure name: readFile
015300100210     D* Purpose:
015400100210     D* Returns:
015500100210     D* Parameter:      fileNo
015600100210     D*--------------------------------------------------
015700100210     D readFile        PR              N
015800100210     D  fileNo                        1P 0
015900100210
016000100210
016100100210
016200100210
016300090105      /free
016400090105              // Get Division and Bank account information from file
016500090106              if  count = 0;
016600110120                    open genpysuml4;
016700110120                    setll (origin:bnk_type) payfmt;
016800110120                    reade (origin:bnk_type) payfmt;
016900091019
017000090106
017100110120              if  count = 0;
017200091010
017300090107
017400110120               select;
017500091013                 when origin  = 'PAY';
017600091013                   exec sql
017700091013                     select * into :payelcDS from payelctrn
017800100209                       where petype = :cbtype;
017900090106
018000100629               // get_bank_acc();
018100091010
018200091013                  // if called from shares system
018300091013                 when origin  = 'SHR';
018400091013                   exec sql
018500091013                     select * into :ShrelcDS from shrelctrn
018600100209                      where setype = :cbtype;
018700091013
018800091013                   eval-corr payelcDS = shrelcDS;
018900100209                  // if called from shares system
019000100209                 when origin  = 'CRD';
019100100209                   exec sql
019200100209                     select * into :cuselcDS from cuselctrn
019300100215                      where cetype = :cbtype;
019400100209
019500100209                   eval-corr payelcDS = cuselcDS;
019600100215               endsl;
019700091013
019800100209               // banks acct field to cb acct field
019900100215               payacc = %subst(payelcDS.peacct:1:16);
020000091013
020100091010              endif;
020200090105
020300090105              if   not(%eof());
020400090105                // get accounting info.
020500100215
020600090105                get_bank_acc();
020700100210                sendBatch();
020800090105
020900110120             //  select;
021000110120              // when origin = 'PAY';
021100100215                dow not(%eof());
021200091010                paysts = 'V';
021300091010                update  payfmt %fields(paysts);
021400110126                    reade (ORIGIN:bnk_type) payfmt;
021500100215                enddo;
021600090105              endif;
021700090105
021800090105              if  %eof();
021900090105                 eof = *on;
022000090105                 *inlr = *on;
022100110120                   close genpysuml4;
022200091013
022300100215
022400090105              endif;
022500100629              endif;
022600090105
022700090105             // RETURN
022800090105             return;
022900090105      /end-free
023000090105
023100090105     P*-----------------------------------------
023200090105     P* Procedure name: get_bank_acc
023300090105     P* Purpose:        get actual bank account
023400090105     P* Returns:        0
023500090105     P* Parameter:      0
023600090105     P*-----------------------------------------
023700090105     P get_bank_acc    B
023800090106     D get_bank_acc    PI
023900090105
024000090105      /free
024100090105          bankacc =  *zeros;
024200090105
024300090105          // check for number in character field
024400090105          dow  xcnt > 0;
024500090105
024600090106              if  %check(digits:payarr(xcnt)) = 0
024700090105                      and ycnt > 0;
024800090105                bankarr(ycnt) = payarr(xcnt);
024900090105                ycnt -=  1;
025000090105              endif;
025100090105
025200090105              xcnt -=  1;
025300090105          enddo;
025400090105
025500090105          // Reset array fields
025600090105          reset  xcnt;
025700090105          reset  ycnt;
025800090105
025900090105      /end-free
026000090105     P get_bank_acc    E
026100090105
026200090105
026300090105
026400090105     P*-----------------------------------------
026500090106     P* Procedure name: prepare_batch
026600090106     P* Purpose:        prepare batch
026700090105     P* Returns:        0
026800090105     P* Parameter:      0
026900090105     P*-----------------------------------------
027000100210     P sendBatch       B
027100100210     D sendBatch       PI
027200090105
027300100210     Dsum_total        S             15  2 inz(0)
027400100210
027500090105      /free
027600090105
027700090105         //  write header record;
027800090106         //  ...create/open file;
027900090107         //----------------------
028000090106         write_to_file('1');
028100100209         write_to_file('2':cb_text_header);
028200110126
028300110126            exec sql
028400110126               select sum(payamt) into :sum_total from GENpysuml1
028500110325                where paysts =  'A' and paydiv = :bnk_type and payorg =
028600110325                :origin;
028700110126            cb_amount = sum_total;
028800100215            cb_hdr_acc = bankacc;
028900100209         write_to_file('2':cb_hder_record);
029000090105
029100100209
029200100209
029300100209
029400090105         //  write detail records;
029500090107         //------------------------
029600110120          //select;
029700110120          //when origin = 'PAY';
029800100215            fnumber = 1;
029900110217
030000100629
030100100215             open_file(fnumber);
030200100215             dow not readFile(fnumber);
030300100210
030400110120          //select;
030500110120          //when origin = 'PAY';
030600100215              payacc = payDS.payacc;
030700100629             amount_wtr  = payDs.payamt;
030800110217
030900100215               get_bank_acc();
031000100215              account_wtr = bankacc;
031100090107                // write detail figures
031200100215               write_batch_detail(account_wtr:comma:amount_wtr);
031300100215
031400100210             enddo;
031500090107
031600100215            closeFile(fnumber);
031700090107
031800090107
031900090107
032000090107
032100090106             //close file
032200090106             write_to_file('3');
032300090105
032400090105      /end-free
032500100210     P sendBatch       E
032600090106
032700090106     P*-----------------------------------------
032800090106     P* Procedure name: write_to_file
032900090106     P* Purpose:        write to file
033000090106     P* Returns:        0
033100090106     P* Parameter:      0
033200090106     P*-----------------------------------------
033300090106     P write_to_file   B
033400090106     D write_to_file   PI
033500090106     D option                         1    const
033600090106     D data                         100    const varying options(*nopass)
033700090106
033800090106     D/copy IFSPR
033900090106     D fd              S             10I 0
034000090106     D wrdata          S            100A
034100090106     D rddata          S            100A
034200090106     D flags           S             10U 0
034300090106     D mode            S             10U 0
034400090106     D Msg             S             50A
034500090106     D Len             S             10I 0
034600090106      /free
034700090106
034800090106       //*************************************************************
034900090106       // writing data to a stream file
035000090106       //****************************************************************
035100090106         select;
035200090106           when option = '1';
035300090107
035400091013               fd = unlink(payelcDS.pefilenm);
035500090107
035600090107             flags = O_WRONLY + O_CREAT + O_TRUNC +
035700090106                     O_TEXTDATA + O_CODEPAGE;
035800090106
035900090106             mode =  S_IRUSR + S_IWUSR
036000090106                         + S_IRGRP + S_IROTH;
036100090106
036200090107             // Open IFS file in ASCII format
036300091010
036400091013             fd = open(payelcDS.pefilenm: flags: mode: 819);
036500091013
036600090106             if  fd < 0;
036700090107             // Msg = 'open(): failed for writing';
036800090107             // return;
036900090106             endif;
037000090106
037100090107             // Close IFS file
037200090106             callp  close(fd);
037300090107
037400090107             // Reopen FS file
037500091013              fd = open(payelcDS.pefilenm:O_WRONLY + O_TEXTDATA);
037600090106
037700090106           // Write some data
037800090106           when option = '2';
037900090106             wrdata = data + CRLF;
038000090106             callp write(fd: %addr(wrdata): %len(%trimr(wrdata)));
038100090106
038200090106           // close the file
038300090106           when option = '3';
038400090106             callp  close(fd);
038500090106         endsl;
038600090106
038700090106      /end-free
038800090106     P write_to_file   E
038900090107
039000090107     P*-----------------------------------------
039100090107     P* Procedure name: write_batch_detail
039200090107     P* Purpose:        write batch detail lines
039300090107     P* Returns:        0
039400090107     P* Parameter:      0
039500090107     P*-----------------------------------------
039600090107     P write_batch_detail...
039700090107     P                 B
039800090107     D write_batch_detail...
039900090107     D                 PI
040000100209     D account                       16  0 const
040100100209     D space                          1    const
040200100215     D total                         15  0 const
040300090107      /free
040400090107               //write detail lines
040500100209               cb_det_account= account;
040600100209               cb_det_comma = space;
040700090107
040800100209               cb_det_total = total;
040900100209               write_to_file('2':cb_text_detail);
041000090107      /end-free
041100090107     P write_batch_detail...
041200090107     P                 E
041300091019
041400100210
041500100210     P*--------------------------------------------------
041600100210     P* Procedure name: open_File
041700100210     P* Purpose:        open file, set cursor to retrieve data for sending ...
041800100210     P*                          to IFS
041900100210     P* Returns:
042000100210     P* Parameter:      fileNo
042100100210     P*--------------------------------------------------
042200100210     P open_File       B
042300100210     D open_File       PI
042400100210     D  fileNo                        1P 0
042500100210
042600100210
042700100210      /FREE
042800110120          // select;
042900110120            // when fileno = 1;
043000100210         exec sql
043100110120          declare payfile cursor for select * from genpysuml1
043200110325          where paydiv = :bnk_type and paysts = 'A' and payorg = :origin;
043300110325
043400100210        exec sql
043500100210        open payfile;
043600110120          //   when fileno = 2;
043700110120         //exec sql
043800110120          // declare payfile2 cursor for select * from shrpysuml1
043900110120          //where sbkdiv = :bnk_type and sbksts = 'A';
044000100629
044100110120          //exec sql
044200110120          //open payfile2;
044300100210
044400110120            // when fileno = 3;
044500110120           //exec sql
044600110120            //declare payfile3 cursor for select * from cuspysuml1
044700110120            //where cbkdiv = :bnk_type and cbksts = 'A';
044800100629
044900110120           //exec sql
045000110120           //open payfile3;
045100110120           //ENDSL;
045200100210
045300100210      /END-FREE
045400100210     P open_File       E
045500100210
045600100210
045700100210     P*--------------------------------------------------
045800100210     P* Procedure name: closeFile
045900100210     P* Purpose:        close selected file
046000100210     P* Returns:
046100100210     P* Parameter:      fileNo
046200100210     P*--------------------------------------------------
046300100210     P closeFile       B
046400100210     D closeFile       PI
046500100210     D  fileNo                        1P 0
046600100210
046700100210
046800100210      /FREE
046900100210
047000110217
047100100210               exec sql
047200100210                 close payfile;
047300110217
047400100210      /END-FREE
047500100210     P closeFile       E
047600100210
047700100210
047800100210     P*--------------------------------------------------
047900100210     P* Procedure name: readFile
048000100210     P* Purpose:
048100100210     P* Returns:
048200100210     P* Parameter:      fileNo
048300100210     P*--------------------------------------------------
048400100210     P readFile        B
048500100210     D readFile        PI              N
048600100210     D  fileNo                        1P 0
048700100210
048800100210
048900100210     D* Local fields
049000100210     D retField        S               N
049100100210
049200100210      /FREE
049300110120           //select;
049400110120            // when fileNo = 1;
049500100210         exec sql
049600100629          fetch next from payfile into :payds;
049700110120           //  when fileNo = 2;
049800110120         //exec sql
049900110120          //fetch next from payfile2 into :shrds;
050000110120            // when fileNo = 3;
050100110120         //exec sql
050200110120          //fetch next from payfile3 into :cusds;
050300110120          //endsl;
050400100629
050500100215            if sqlstt = '02000';
050600100215           retField = *on;
050700100215
050800100215          else;
050900100215           retField = *off;
051000100215        ENDIF;
051100100210        RETURN retField;
051200100210
051300100210      /END-FREE
051400100210     P readFile        E
051500100210
