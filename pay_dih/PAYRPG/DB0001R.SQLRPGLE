000100110228     H dftactgrp(*no) actgrp(*CALLER) bnddir('DISDIR')
000200090106     H******  PROGRAMMER - S. BRITTON , c.williams
000300090106     H******  First Coded - 09/01/06
000400110120     Fgenpysuml4uf   e           k disk    commit  usropn
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
001600100629     D shrDS         e ds                  extname(shrpysum) inz
001700100629     D cusDS         e ds                  extname(cuspysum) inz
001800100629
001900090106     D payelcDS      e ds                  extname(payelctrn) inz
002000091013     D                                     qualified prefix('P':1)
002100091013
002200091013     D shrelcDS      e ds                  extname(shrelctrn) inz
002300091013     D                                     qualified prefix('P':1)
002400090105
002500100209     D cuselcDS      e ds                  extname(cuselctrn) inz
002600100209     D                                     qualified prefix('P':1)
002700000711     **
002800090105     Dciti_err         ds                  template
002900000711     D e_payemp                       6  0
003000000711     D e_payacc                      12
003100000711     D e_paycrd                       6  0
003200000711     D e_payname                     26
003300090105
003400090106     Drb_struc         ds                  qualified  dim(30) inz
003500091013     D rb_transdet                         likeDS(rb_data) dim(1500)
003600100209     D rb_transsum                         likeDS(rb_summ)
003700090105
003800090105     D rb_summ         ds                  template
003900090105     D rb_amount                     15  2
004000100209     D rb_cracc                      16  0
004100090105
004200090105     Drb_data          ds                  template
004300100209     D rb_account                    16  0
004400100209     D rb_comma                       1    inz(',')
004500090105     D rb_total                      15  2
004600090105
004700000707     **
004800090105
004900131008     Ddb_text_header   ds
005000131008     D db_head_names                 40     inz('Name of Employee')
005100131008     D db_text_comma1                 1    inz(',')
005200131008     D db_head_acct1                 16     inz('Transit #')
005300131008     D db_text_comma                  1    inz(',')
005400131008     D db_head_acct                  16     inz('Bank Account')
005500131008     D db_text_comma2                 1    inz(',')
005600131008     D db_head_amt                    7     inz('Amount')
005700090105
005800100209
005900131008     DDb_text_detail   ds
006000131008     D db_det_name                   40    inz(' ')
006100131008     D db_det_trans                  16  0 inz(100)
006200131008     D db_det_comma                   1    inz(',')
006300131008     D db_det_account                16  0
006400131008     D db_det_comma2                  1    inz(',')
006500131008     D db_det_total                  15  0
006600090105
006700091019
006800091019      // General PROTOTYPES
006900091019      /copy GENPR
007000090105
007100090106     D DIGITS          c                   '0123456789'
007200090106     D CRLF            c                   x'0D25'
007300090106
007400090623     D xcnt            s              2  0 inz(30)
007500100210     D ycnt            s              2  0 inz(16)
007600100215     D fnumber         s              1  0 inz(0)
007700090105     D acnt            s              4  0
007800090105     D gcnt            s              4  0
007900090107     D crddate         s               d   datfmt(*jul)
008000090107     D julDate         s              7  0
008100090106     D seqcnt          s              6  0
008200131008     Dname_wtr         s             40
008300100215     Daccount_wtr      s             16  0
008400100215     Damount_wtr       s             15  2
008500090107       // Identify BANK/FI
008600100927     D cbtype          s              2  0 inz(4)
008700090106     D rbarr           s              8  0 dim(30)
008800100209     D comma           s              1    inz(',')
008900100927     D trans#          s              3  0 inz(100)
009000090107     D zeros           s              9  0 inz(*zeros)
009100090107     D year            s              4  0
009200000209     **
009300090105     D*--------------------------------------
009400090105     D* Prototype for procedure: get_bank_acc
009500090105     D*--------------------------------------
009600090105     D get_bank_acc    PR
009700090105
009800090105
009900090105     D*-----------------------------------------
010000090106     D* Prototype for procedure: prepare_batch
010100090105     D*-----------------------------------------
010200100210     D sendBatch       PR
010300090105
010400090106     D*-----------------------------------------
010500090106     D* Prototype for procedure: write_to_file
010600090106     D*-----------------------------------------
010700090106     D write_to_file   PR
010800090106     D option                         1    const
010900090106        // '1' - create/open file
011000090106        // '2' - write data
011100090106        // '3' - close file
011200090106     D data                         100    const varying options(*nopass)
011300090107
011400090107     D write_batch_detail...
011500090107     D                 PR
011600131008     D names                         40    const
011700131008     D trans                          3  0 const
011800100927     D space1                         1    const
011900100927     D account                       16  0 const
012000100927     D space                          1    const
012100100215     D total                         15  0 const
012200090106
012300090106     D*-----------------------------------------
012400090106     D* Prototype for procedure: Main_pgm
012500090106     D*-----------------------------------------
012600100927     D  main_pgm       pr                  extpgm('DB0001R')
012700090105     D   citi_errDS                        likeds(citi_err) dim(100)
012800090105     D   err_count                    6  0
012900090105     D   eof                           n
013000090105     D   count                        6  0
013100090105     D   bnk_type                     2  0
013200091010     D   origin                       3    options(*nopass)
013300090106
013400090106     D  main_pgm       pi
013500090106     D   citi_errDS                        likeds(citi_err) dim(100)
013600090106     D   err_count                    6  0
013700090106     D   eof                           n
013800090106     D   count                        6  0
013900090106     D   bnk_type                     2  0
014000091010     D   origin                       3    options(*nopass)
014100091019     **
014200100210     D*--------------------------------------------------
014300100210     D* Procedure name: open_File
014400100210     D* Purpose:        open file, set cursor to retrieve data for sending ...
014500100210     D*                          to IFS
014600100210     D* Returns:
014700100210     D* Parameter:      fileNo
014800100210     D*--------------------------------------------------
014900100210     D open_File       PR
015000100210     D  fileNo                        1P 0
015100100210
015200100210     D*--------------------------------------------------
015300100210     D* Procedure name: closeFile
015400100210     D* Purpose:        close selected file
015500100210     D* Returns:
015600100210     D* Parameter:      fileNo
015700100210     D*--------------------------------------------------
015800100210     D closeFile       PR
015900100210     D  fileNo                        1P 0
016000100210
016100100210     D*--------------------------------------------------
016200100210     D* Procedure name: readFile
016300100210     D* Purpose:
016400100210     D* Returns:
016500100210     D* Parameter:      fileNo
016600100210     D*--------------------------------------------------
016700100210     D readFile        PR              N
016800100210     D  fileNo                        1P 0
016900100210
017000100210
017100100210
017200100210
017300090105      /free
017400090105              // Get Division and Bank account information from file
017500090106              if  count = 0;
017600110120                    open genpysuml4;
017700110120                    setll (origin:bnk_type) payfmt;
017800090105
017900110120                    reade (origin:bnk_type) payfmt;
018000091019
018100090106
018200110120           //   if  count = 0;
018300091010
018400090107
018500091010               select;
018600091013                 when origin  = 'PAY';
018700091013                   exec sql
018800091013                     select * into :payelcDS from payelctrn
018900100209                       where petype = :cbtype;
019000090106
019100100629               // get_bank_acc();
019200091010
019300091013                  // if called from shares system
019400091013                 when origin  = 'SHR';
019500091013                   exec sql
019600091013                     select * into :ShrelcDS from shrelctrn
019700100209                      where setype = :cbtype;
019800091013
019900091013                   eval-corr payelcDS = shrelcDS;
020000100209                  // if called from shares system
020100100209                 when origin  = 'CRD';
020200100209                   exec sql
020300100209                     select * into :cuselcDS from cuselctrn
020400100215                      where cetype = :cbtype;
020500100209
020600100209                   eval-corr payelcDS = cuselcDS;
020700100215               endsl;
020800091013
020900100209               // banks acct field to cb acct field
021000100215               payacc = %subst(payelcDS.peacct:1:16);
021100091013
021200091010              endif;
021300090105
021400090105              if   not(%eof());
021500090105                // get accounting info.
021600100215
021700090105                get_bank_acc();
021800100210                sendBatch();
021900090105
022000100215                dow not(%eof());
022100091010                paysts = 'V';
022200091010                update  payfmt %fields(paysts);
022300110121                    reade (origin:bnk_type) payfmt;
022400100215                enddo;
022500090105              endif;
022600090105
022700090105              if  %eof();
022800090105                 eof = *on;
022900090105                 *inlr = *on;
023000110120                   close genpysuml4;
023100091013
023200100215
023300090105              endif;
023400090105
023500090105             // RETURN
023600090105             return;
023700090105      /end-free
023800090105
023900090105     P*-----------------------------------------
024000090105     P* Procedure name: get_bank_acc
024100090105     P* Purpose:        get actual bank account
024200090105     P* Returns:        0
024300090105     P* Parameter:      0
024400090105     P*-----------------------------------------
024500090105     P get_bank_acc    B
024600090106     D get_bank_acc    PI
024700090105
024800090105      /free
024900090105          bankacc =  *zeros;
025000090105
025100090105          // check for number in character field
025200090105          dow  xcnt > 0;
025300090105
025400090106              if  %check(digits:payarr(xcnt)) = 0
025500090105                      and ycnt > 0;
025600090105                bankarr(ycnt) = payarr(xcnt);
025700090105                ycnt -=  1;
025800090105              endif;
025900090105
026000090105              xcnt -=  1;
026100090105          enddo;
026200090105
026300090105          // Reset array fields
026400090105          reset  xcnt;
026500090105          reset  ycnt;
026600090105
026700090105      /end-free
026800090105     P get_bank_acc    E
026900090105
027000090105
027100090105
027200090105     P*-----------------------------------------
027300090106     P* Procedure name: prepare_batch
027400090106     P* Purpose:        prepare batch
027500090105     P* Returns:        0
027600090105     P* Parameter:      0
027700090105     P*-----------------------------------------
027800100210     P sendBatch       B
027900100210     D sendBatch       PI
028000090105
028100100210     Dsum_total        S             15  2 inz(0)
028200100210
028300090105      /free
028400090105
028500090105         //  write header record;
028600090106         //  ...create/open file;
028700090107         //----------------------
028800090106         write_to_file('1');
028900131008         write_to_file('2':db_text_header);
029000100209             exec sql
029100110121               select sum(payamt) into :sum_total from genpysuml1
029200100210                where paysts =  'A' and paydiv = :bnk_type;
029300100209            cb_amount = sum_total;
029400090105
029500100209
029600100209
029700100209
029800090105         //  write detail records;
029900090107         //------------------------
030000100215            fnumber = 1;
030100100629
030200100215             open_file(fnumber);
030300100215             dow not readFile(fnumber);
030400100210
030500131008              name_Wtr = payDS.paybname;
030600131008              payacc = payDS.payacc;
030700100629             amount_wtr  = payDs.payamt;
030800100215               get_bank_acc();
030900100215              account_wtr = bankacc;
031000090107                // write detail figures
031100131008           write_batch_detail(name_wtr:trans#:comma:account_wtr
031200131008                                :comma:amount_wtr);
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
040000131008     D names                         40    const
040100131008     D trans                          3  0 const
040200100927     D space1                         1    const
040300100927     D account                       16  0 const
040400100209     D space                          1    const
040500100215     D total                         15  0 const
040600090107      /free
040700090107               //write detail lines
040800131008               db_det_name  = names;
040900131008               db_det_account= account;
041000131008               db_det_comma = space;
041100131008               db_det_comma2 = space;
041200100927
041300090107
041400100209               cb_det_total = total;
041500131008               write_to_file('2':Db_text_detail);
041600090107      /end-free
041700090107     P write_batch_detail...
041800090107     P                 E
041900091019
042000100210
042100100210     P*--------------------------------------------------
042200100210     P* Procedure name: open_File
042300100210     P* Purpose:        open file, set cursor to retrieve data for sending ...
042400100210     P*                          to IFS
042500100210     P* Returns:
042600100210     P* Parameter:      fileNo
042700100210     P*--------------------------------------------------
042800100210     P open_File       B
042900100210     D open_File       PI
043000100210     D  fileNo                        1P 0
043100100210
043200100210
043300100210      /FREE
043400100210         exec sql
043500110120          declare payfile cursor for select * from genpysuml1
043600110126            where paydiv = :bnk_type and paysts = 'A' and payorg = :origin;
043700100210
043800110126             exec sql
043900110126               open payfile;
044000100210      /END-FREE
044100100210     P open_File       E
044200100210
044300100210
044400100210     P*--------------------------------------------------
044500100210     P* Procedure name: closeFile
044600100210     P* Purpose:        close selected file
044700100210     P* Returns:
044800100210     P* Parameter:      fileNo
044900100210     P*--------------------------------------------------
045000100210     P closeFile       B
045100100210     D closeFile       PI
045200100210     D  fileNo                        1P 0
045300100210
045400100210
045500100210      /FREE
045600100210
045700100210               exec sql
045800100210                 close payfile;
045900100210      /END-FREE
046000100210     P closeFile       E
046100100210
046200100210
046300100210     P*--------------------------------------------------
046400100210     P* Procedure name: readFile
046500100210     P* Purpose:
046600100210     P* Returns:
046700100210     P* Parameter:      fileNo
046800100210     P*--------------------------------------------------
046900100210     P readFile        B
047000100210     D readFile        PI              N
047100100210     D  fileNo                        1P 0
047200100210
047300100210
047400100210     D* Local fields
047500100210     D retField        S               N
047600100210
047700100210      /FREE
047800110120          // select;
047900110120            // when fileNo = 1;
048000100210         exec sql
048100100629          fetch next from payfile into :payds;
048200100629
048300100215            if sqlstt = '02000';
048400100215           retField = *on;
048500100215
048600100215          else;
048700100215           retField = *off;
048800100215        ENDIF;
048900100210        RETURN retField;
049000100210
049100100210      /END-FREE
049200100210     P readFile        E
049300100210
