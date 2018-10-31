000100091019     H dftactgrp(*no) actgrp(*CALLER) bnddir('DISDIR')
000200090106     H******  PROGRAMMER - S. BRITTON , c.williams
000300090106     H******  First Coded - 09/01/06
000400100922     F*nkpysuml4uf   e           k disk    commit  usropn
000500091010     Fshrpysuml4uf   e           k disk    commit  usropn
000600100922     Fgenpysuml4uf   e           k disk    commit  usropn
000700991208     **
000800000707     Daccount_fld      ds
000900000707     D payacc
001000090623     D  payarr                        1    overlay(payacc) dim(30)
001100000707     **
001200090106     DCiti_acc_fld     ds
001300100209     D bankacc                       16  0
001400100215     D bankarr                        1    overlay(bankacc) dim(16)
001500000707     **
001600100922     D payDS         e ds                  extname(genpysum) inz
001700100629     d                                     qualified
001800100629     D shrDS         e ds                  extname(shrpysum) inz
001900100922     D cusDS         e ds                  extname(genpysum) inz
001901100922     d                                     qualified
002000100629
002100090106     D payelcDS      e ds                  extname(payelctrn) inz
002200091013     D                                     qualified prefix('P':1)
002300091013
002400091013     D shrelcDS      e ds                  extname(shrelctrn) inz
002500091013     D                                     qualified prefix('P':1)
002600090105
002700100209     D cuselcDS      e ds                  extname(cuselctrn) inz
002800100209     D                                     qualified prefix('P':1)
002900000711     **
003000090105     Dciti_err         ds                  template
003100000711     D e_payemp                       6  0
003200000711     D e_payacc                      12
003300000711     D e_paycrd                       6  0
003400000711     D e_payname                     26
003500090105
003600090106     Drb_struc         ds                  qualified  dim(30) inz
003700091013     D rb_transdet                         likeDS(rb_data) dim(1500)
003800100209     D rb_transsum                         likeDS(rb_summ)
003900090105
004000090105     D rb_summ         ds                  template
004100090105     D rb_amount                     15  2
004200100209     D rb_cracc                      16  0
004300090105
004400090105     Drb_data          ds                  template
004500100209     D rb_account                    16  0
004600100209     D rb_comma                       1    inz(',')
004700090105     D rb_total                      15  2
004800090105
004900000707     **
005000090105
005100100922     Ddb_text_header   ds
005200100922     D db_head_trn                   16     inz('Transit#')
005300100922     D db_text_comma                  1    inz(',')
005301100922     D db_head_acct                  16     inz('Bank Account')
005302100922     D db_text_comma2                 1    inz(',')
005400100922     D db_head_amt                    7     inz('Amount')
005500090105
005600100922     D*db_hder_record   ds
005601100922     D* db_hdr_trn                     3  0 inz(100)
005602100922     D* db_hdr_comma                   1    inz(',')
005700100922     D* db_hdr_acc                    16  0
005800100922     D* db_hdr_comma2                  1    inz(',')
005900100922     D* db_amount                     15  0
006000100209
006100100922     Ddb_text_detail   ds
006101100922     D db_det_trn                     3  0 inz(100)
006102100922     D db_det_comma                   1    inz(',')
006200100922     D db_det_account                16  0
006300100922     D db_det_comma2                  1    inz(',')
006400100922     D db_det_total                  15  0
006500090105
006600091019
006700091019      // General PROTOTYPES
006800091019      /copy GENPR
006900090105
007000090106     D DIGITS          c                   '0123456789'
007100090106     D CRLF            c                   x'0D25'
007200090106
007300090623     D xcnt            s              2  0 inz(30)
007400100210     D ycnt            s              2  0 inz(16)
007500100215     D fnumber         s              1  0 inz(0)
007600090105     D acnt            s              4  0
007700090105     D gcnt            s              4  0
007800090107     D crddate         s               d   datfmt(*jul)
007900090107     D julDate         s              7  0
008000090106     D seqcnt          s              6  0
008100100215     Daccount_wtr      s             16  0
008200100215     Damount_wtr       s             15  2
008300090107       // Identify BANK/FI
008400100209     D cbtype          s              2  0 inz(1)
008500090106     D rbarr           s              8  0 dim(30)
008600100209     D comma           s              1    inz(',')
008700090107     D zeros           s              9  0 inz(*zeros)
008800090107     D year            s              4  0
008900000209     **
009000090105     D*--------------------------------------
009100090105     D* Prototype for procedure: get_bank_acc
009200090105     D*--------------------------------------
009300090105     D get_bank_acc    PR
009400090105
009500090105
009600090105     D*-----------------------------------------
009700090106     D* Prototype for procedure: prepare_batch
009800090105     D*-----------------------------------------
009900100210     D sendBatch       PR
010000090105
010100090106     D*-----------------------------------------
010200090106     D* Prototype for procedure: write_to_file
010300090106     D*-----------------------------------------
010400090106     D write_to_file   PR
010500090106     D option                         1    const
010600090106        // '1' - create/open file
010700090106        // '2' - write data
010800090106        // '3' - close file
010900090106     D data                         100    const varying options(*nopass)
011000090107
011100090107     D write_batch_detail...
011200090107     D                 PR
011300100209     D account                       16  0 const
011400100209     D space                          1    const
011500100215     D total                         15  0 const
011600090106
011700090106     D*-----------------------------------------
011800090106     D* Prototype for procedure: Main_pgm
011900090106     D*-----------------------------------------
012000100922     D  main_pgm       pr                  extpgm('DB0001R')
012100090105     D   citi_errDS                        likeds(citi_err) dim(100)
012200090105     D   err_count                    6  0
012300090105     D   eof                           n
012400090105     D   count                        6  0
012500090105     D   bnk_type                     2  0
012600091010     D   origin                       3    options(*nopass)
012700090106
012800090106     D  main_pgm       pi
012900090106     D   citi_errDS                        likeds(citi_err) dim(100)
013000090106     D   err_count                    6  0
013100090106     D   eof                           n
013200090106     D   count                        6  0
013300090106     D   bnk_type                     2  0
013400091010     D   origin                       3    options(*nopass)
013500091019     **
013600100210     D*--------------------------------------------------
013700100210     D* Procedure name: open_File
013800100210     D* Purpose:        open file, set cursor to retrieve data for sending ...
013900100210     D*                          to IFS
014000100210     D* Returns:
014100100210     D* Parameter:      fileNo
014200100210     D*--------------------------------------------------
014300100210     D open_File       PR
014400100210     D  fileNo                        1P 0
014500100210
014600100210     D*--------------------------------------------------
014700100210     D* Procedure name: closeFile
014800100210     D* Purpose:        close selected file
014900100210     D* Returns:
015000100210     D* Parameter:      fileNo
015100100210     D*--------------------------------------------------
015200100210     D closeFile       PR
015300100210     D  fileNo                        1P 0
015400100210
015500100210     D*--------------------------------------------------
015600100210     D* Procedure name: readFile
015700100210     D* Purpose:
015800100210     D* Returns:
015900100210     D* Parameter:      fileNo
016000100210     D*--------------------------------------------------
016100100210     D readFile        PR              N
016200100210     D  fileNo                        1P 0
016300100210
016400100210
016500100210
016600100210
016700090105      /free
016800090105              // Get Division and Bank account information from file
016900090106              if  count = 0;
017000091010                select;
017100091013                  when origin = 'PAY';
017200100922                    open genpysuml4;
017300100922                    setll (origin:bnk_type) payfmt;
017400091013                  when origin = 'SHR';
017500091013                    open shrpysuml4;
017600091013                    setll bnk_type sbkfmt;
017700100215                  when origin = 'CRD';
017800100922                    open genpysuml4;
017900100922                    setll (origin:bnk_type) payfmt;
018000091010                endsl;
018100100629           //   endif;
018200090105
018300091010                select;
018400091013                  when origin = 'PAY';
018500100922                    reade (origin:bnk_type) payfmt;
018600100215         //             paycrd = get_credit_date(paycrd:paydate);
018700100215
018800100210                  when origin = 'CRD';
018900100922                    reade (origin:bnk_type) payfmt;
019000091013                  when origin = 'SHR';
019100091013                    reade bnk_type sbkfmt;
019200091013                    payamt = sbkamt;
019300091010                endsl;
019400091019
019500090106
019600090105              if  count = 0;
019700091010
019800090107
019900091010               select;
020000091013                 when origin  = 'PAY';
020100091013                   exec sql
020200091013                     select * into :payelcDS from payelctrn
020300100209                       where petype = :cbtype;
020400090106
020500100629               // get_bank_acc();
020600091010
020700091013                  // if called from shares system
020800091013                 when origin  = 'SHR';
020900091013                   exec sql
021000091013                     select * into :ShrelcDS from shrelctrn
021100100209                      where setype = :cbtype;
021200091013
021300091013                   eval-corr payelcDS = shrelcDS;
021400100209                  // if called from shares system
021500100209                 when origin  = 'CRD';
021600100209                   exec sql
021700100209                     select * into :cuselcDS from cuselctrn
021800100215                      where cetype = :cbtype;
021900100209
022000100209                   eval-corr payelcDS = cuselcDS;
022100100215               endsl;
022200091013
022300100209               // banks acct field to cb acct field
022400100215               payacc = %subst(payelcDS.peacct:1:16);
022500091013
022600091010              endif;
022700090105
022800090105              if   not(%eof());
022900090105                // get accounting info.
023000100215
023100090105                get_bank_acc();
023200100210                sendBatch();
023300090105
023400100210               select;
023500100210               when origin = 'PAY';
023600100215                dow not(%eof());
023700091010                paysts = 'V';
023800091010                update  payfmt %fields(paysts);
023900100922                    reade (origin:bnk_type) payfmt;
024000100215                enddo;
024100100210               when origin = 'SHR';
024101100629                   dow not(%eof());
024200100629                       sbksts = 'V';
024400100629                       update  sbkfmt %fields(sbksts);
024401100629                       reade bnk_type sbkfmt;
024402100629                enddo;
024500100210               when origin = 'CRD';
024501100629                  dow not(%eof());
024600100922                        paysts = 'V';
024700100922                        update  payfmt %fields(paysts);
024701100922                        reade (origin:bnk_type) payfmt;
024702100629                enddo;
024800100210               endsl;
024900090105              endif;
025000090105
025100090105              if  %eof();
025200090105                 eof = *on;
025300090105                 *inlr = *on;
025400100209                 select;
025500100209                 when origin = 'PAY';
025600100922                   close genpysuml4;
025700100209                 when origin = 'SHR';
025800091013                   close shrpysuml4;
025900100209                 when origin = 'CRD';
026000100922                   close genpysuml4;
026100100209                 endsl;
026200091013
026300100215
026400090105              endif;
026401100629              endif;
026500090105
026600090105             // RETURN
026700090105             return;
026800090105      /end-free
026900090105
027000090105     P*-----------------------------------------
027100090105     P* Procedure name: get_bank_acc
027200090105     P* Purpose:        get actual bank account
027300090105     P* Returns:        0
027400090105     P* Parameter:      0
027500090105     P*-----------------------------------------
027600090105     P get_bank_acc    B
027700090106     D get_bank_acc    PI
027800090105
027900090105      /free
028000090105          bankacc =  *zeros;
028100100629           //   select;
028200100629           //   when origin = 'PAY';
028300091013            // already set
028400100629          //  when origin = 'SHR';
028500100629          //   payacc = sbkacc;
028600100629          //  when origin = 'CRD';
028700100629          //   payacc = cbkacc;
028800100629          //endsl;
028900090105
029000090105          // check for number in character field
029100090105          dow  xcnt > 0;
029200090105
029300090106              if  %check(digits:payarr(xcnt)) = 0
029400090105                      and ycnt > 0;
029500090105                bankarr(ycnt) = payarr(xcnt);
029600090105                ycnt -=  1;
029700090105              endif;
029800090105
029900090105              xcnt -=  1;
030000090105          enddo;
030100090105
030200090105          // Reset array fields
030300090105          reset  xcnt;
030400090105          reset  ycnt;
030500090105
030600090105      /end-free
030700090105     P get_bank_acc    E
030800090105
030900090105
031000090105
031100090105     P*-----------------------------------------
031200090106     P* Procedure name: prepare_batch
031300090106     P* Purpose:        prepare batch
031400090105     P* Returns:        0
031500090105     P* Parameter:      0
031600090105     P*-----------------------------------------
031700100210     P sendBatch       B
031800100210     D sendBatch       PI
031900090105
032000100210     Dsum_total        S             15  2 inz(0)
032100100210
032200090105      /free
032300090105
032400090105         //  write header record;
032500090106         //  ...create/open file;
032600090107         //----------------------
032700090106         write_to_file('1');
032800100922         write_to_file('2':Db_text_header);
032900100922          // select;
033000100922          //  when origin = 'PAY';
033100100922           //  exec sql
033200100922            //   select sum(payamt) into :sum_total from genpysuml1
033300100922              //  where paysts =  'A' and payorg = :origin
033301100922              //and paydiv = :bnk_type;
033400100922            //when origin = 'SHR';
033500100922            // exec sql
033600100922             //  select sum(sbkamt) into :sum_total from shrpysuml1
033700100922              //  where sbksts =  'A' and sbkdiv = :bnk_type;
033800100629
033900100922            //when origin = 'CRD';
034000100922             //exec sql
034100100922              // select sum(payamt) into :sum_total from genpysuml1
034200100922               // where paysts =  'A'
034201100922                //and payorg = :origin and paydiv = :bnk_type;
034300100922           // endsl;
034400100922            //Db_amount = sum_total;
034500100922            //Db_hdr_acc = bankacc;
034600100922         //write_to_file('2':db_hder_record);
034700090105
034800100209
034900100209
035000100209
035100090105         //  write detail records;
035200090107         //------------------------
035300100629          select;
035400100629          when origin = 'PAY';
035500100215            fnumber = 1;
035600100629          when origin = 'SHR';
035700100629            fnumber = 2;
035800100629          when origin = 'CRD';
035900100629            fnumber = 3;
036000100629          endsl;
036100100629
036200100215             open_file(fnumber);
036300100215             dow not readFile(fnumber);
036400100210
036500100629          select;
036600100629          when origin = 'PAY';
036700100215              payacc = payDS.payacc;
036701100629             amount_wtr  = payDs.payamt;
036800100629          when origin = 'SHR';
036900100629              payacc = shrDS.sbkacc;
036901100629             amount_wtr  = shrDs.sbkamt;
037000100629          when origin = 'CRD';
037100100922              payacc = cusDS.payacc;
037101100922             amount_wtr  = cusDs.payamt;
037200100629          endsl;
037300100215               get_bank_acc();
037400100215              account_wtr = bankacc;
037600090107                // write detail figures
037700100215               write_batch_detail(account_wtr:comma:amount_wtr);
037800100215
037900100210             enddo;
038000090107
038100100215            closeFile(fnumber);
038200090107
038300090107
038400090107
038500090107
038600090106             //close file
038700090106             write_to_file('3');
038800090105
038900090105      /end-free
039000100210     P sendBatch       E
039100090106
039200090106     P*-----------------------------------------
039300090106     P* Procedure name: write_to_file
039400090106     P* Purpose:        write to file
039500090106     P* Returns:        0
039600090106     P* Parameter:      0
039700090106     P*-----------------------------------------
039800090106     P write_to_file   B
039900090106     D write_to_file   PI
040000090106     D option                         1    const
040100090106     D data                         100    const varying options(*nopass)
040200090106
040300090106     D/copy IFSPR
040400090106     D fd              S             10I 0
040500090106     D wrdata          S            100A
040600090106     D rddata          S            100A
040700090106     D flags           S             10U 0
040800090106     D mode            S             10U 0
040900090106     D Msg             S             50A
041000090106     D Len             S             10I 0
041100090106      /free
041200090106
041300090106       //*************************************************************
041400090106       // writing data to a stream file
041500090106       //****************************************************************
041600090106         select;
041700090106           when option = '1';
041800090107
041900091013               fd = unlink(payelcDS.pefilenm);
042000090107
042100090107             flags = O_WRONLY + O_CREAT + O_TRUNC +
042200090106                     O_TEXTDATA + O_CODEPAGE;
042300090106
042400090106             mode =  S_IRUSR + S_IWUSR
042500090106                         + S_IRGRP + S_IROTH;
042600090106
042700090107             // Open IFS file in ASCII format
042800091010
042900091013             fd = open(payelcDS.pefilenm: flags: mode: 819);
043000091013
043100090106             if  fd < 0;
043200090107             // Msg = 'open(): failed for writing';
043300090107             // return;
043400090106             endif;
043500090106
043600090107             // Close IFS file
043700090106             callp  close(fd);
043800090107
043900090107             // Reopen FS file
044000091013              fd = open(payelcDS.pefilenm:O_WRONLY + O_TEXTDATA);
044100090106
044200090106           // Write some data
044300090106           when option = '2';
044400090106             wrdata = data + CRLF;
044500090106             callp write(fd: %addr(wrdata): %len(%trimr(wrdata)));
044600090106
044700090106           // close the file
044800090106           when option = '3';
044900090106             callp  close(fd);
045000090106         endsl;
045100090106
045200090106      /end-free
045300090106     P write_to_file   E
045400090107
045500090107     P*-----------------------------------------
045600090107     P* Procedure name: write_batch_detail
045700090107     P* Purpose:        write batch detail lines
045800090107     P* Returns:        0
045900090107     P* Parameter:      0
046000090107     P*-----------------------------------------
046100090107     P write_batch_detail...
046200090107     P                 B
046300090107     D write_batch_detail...
046400090107     D                 PI
046500100209     D account                       16  0 const
046600100209     D space                          1    const
046700100215     D total                         15  0 const
046800090107      /free
046900090107               //write detail lines
047000100922               db_det_account= account;
047100100922               db_det_comma = space;
047200090107
047300100922               db_det_total = total;
047400100922               write_to_file('2':db_text_detail);
047500090107      /end-free
047600090107     P write_batch_detail...
047700090107     P                 E
047800091019
047900100210
048000100210     P*--------------------------------------------------
048100100210     P* Procedure name: open_File
048200100210     P* Purpose:        open file, set cursor to retrieve data for sending ...
048300100210     P*                          to IFS
048400100210     P* Returns:
048500100210     P* Parameter:      fileNo
048600100210     P*--------------------------------------------------
048700100210     P open_File       B
048800100210     D open_File       PI
048900100210     D  fileNo                        1P 0
049000100210
049100100210
049200100210      /FREE
049300100210           select;
049400100210             when fileno = 1;
049500100210         exec sql
049600100922        declare payfile cursor for select * from genpysuml1
049700100922        where payorg = :origin and paydiv = :bnk_type and paysts = 'A';
049800100210
049900100210        exec sql
050000100210        open payfile;
050100100629             when fileno = 2;
050200100629         exec sql
050300100629        declare payfile2 cursor for select * from shrpysuml1
050400100629        where sbkdiv = :bnk_type and sbksts = 'A';
050500100629
050600100629        exec sql
050700100629        open payfile2;
050800100210
050900100629             when fileno = 3;
051000100629         exec sql
051100100922        declare payfile3 cursor for select * from genpysuml1
051200100922        where paydiv = :bnk_type and paysts = 'A' and payorg = :origin;
051300100629
051400100629        exec sql
051500100629        open payfile3;
051600100210           ENDSL;
051700100210
051800100210      /END-FREE
051900100210     P open_File       E
052000100210
052100100210
052200100210     P*--------------------------------------------------
052300100210     P* Procedure name: closeFile
052400100210     P* Purpose:        close selected file
052500100210     P* Returns:
052600100210     P* Parameter:      fileNo
052700100210     P*--------------------------------------------------
052800100210     P closeFile       B
052900100210     D closeFile       PI
053000100210     D  fileNo                        1P 0
053100100210
053200100210
053300100210      /FREE
053400100210
053500100210           select;
053600100210             when fileNo = 1;
053700100210               exec sql
053800100210                 close payfile;
053900100629             when fileNo = 2;
054000100629               exec sql
054100100629                 close payfile2;
054200100629             when fileNo = 3;
054300100629               exec sql
054400100629                 close payfile3;
054500100210           ENDSL;
054600100210      /END-FREE
054700100210     P closeFile       E
054800100210
054900100210
055000100210     P*--------------------------------------------------
055100100210     P* Procedure name: readFile
055200100210     P* Purpose:
055300100210     P* Returns:
055400100210     P* Parameter:      fileNo
055500100210     P*--------------------------------------------------
055600100210     P readFile        B
055700100210     D readFile        PI              N
055800100210     D  fileNo                        1P 0
055900100210
056000100210
056100100210     D* Local fields
056200100210     D retField        S               N
056300100210
056400100210      /FREE
056500100629           select;
056600100629             when fileNo = 1;
056700100922           exec sql
056800100922           fetch next from payfile into :payds;
056900100629             when fileNo = 2;
057000100922          exec sql
057100100922           fetch next from payfile2 into :shrds;
057200100629             when fileNo = 3;
057300100922          exec sql
057400100922           fetch next from payfile3 into :cusds;
057500100629          endsl;
057600100629
057700100215            if sqlstt = '02000';
057800100215           retField = *on;
057900100215
058000100215          else;
058100100215           retField = *off;
058200100922         ENDIF;
058300100210        RETURN retField;
058400100210
058500100210      /END-FREE
058600100210     P readFile        E
058700100210
