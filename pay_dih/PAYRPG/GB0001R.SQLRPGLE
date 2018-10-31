000100091019     H dftactgrp(*no) actgrp(*CALLER) bnddir('DISDIR')
000200121120     H******  PROGRAMMER c.williams
000300121120     H******   2012/11/20
000400121120     H******
000500110301     Fgenpysuml4uf   e           k disk     usropn
000600991208     **
000700000707     Daccount_fld      ds
000800000707     D payacc
000900090623     D  payarr                        1    overlay(payacc) dim(30)
001000000707     **
001100121121     dbgti_acc_fld     ds
001200121121     D bankacc                       30
001300121121     D bankarr                        1    overlay(bankacc) dim(30)
001400000707     **
001500110120     D payDS         e ds                  extname(genpysum) inz
001600100629     d                                     qualified
001700100629
001800090106     D payelcDS      e ds                  extname(payelctrn) inz
001900091013     D                                     qualified prefix('P':1)
002000091013
002100091013     D shrelcDS      e ds                  extname(shrelctrn) inz
002200091013     D                                     qualified prefix('P':1)
002300090105
002400100209     D cuselcDS      e ds                  extname(cuselctrn) inz
002500100209     D                                     qualified prefix('P':1)
002600000711     **
002700090105     Dciti_err         ds                  template
002800000711     D e_payemp                       6  0
002900121121     D e_payacc                      16
003000000711     D e_paycrd                       6  0
003100000711     D e_payname                     26
003200090105
003300000707     **
003400090105
003500121119     DGb_text_header   ds
003600121119     D Gb_head_amt                   16     inz('Amount')
003700121119     D Gb_text_comma                  1    inz(',')
003800121119     D Gb_head_Name                  30     inz('Name')
003900121119     D Gb_text_comma2                 1    inz(',')
004000121119     D Gb_head_acct                  16     inz('Account No.')
004100121119     D Gb_text_comma3                 1    inz(',')
004200121119     D Gb_head_Date                  11     inz('Value Date')
004300090105
004400121120
004500100209
004600121119     Dgb_text_detail   ds
004700121120     D gb_det_total                  13  0
004800121119     D gb_det_comma                   1    inz(',')
004900121120     D gb_det_Name                   40
005000121119     D gb_det_comma2                  1    inz(',')
005100121121     D gb_det_account                16
005200121119     D gb_det_comma3                  1    inz(',')
005300121121     D gb_det_date                   12
005400090105
005500091019
005600091019      // General PROTOTYPES
005700121119      /copy qcpysrc,GENPR
005800090105
005900090106     D DIGITS          c                   '0123456789'
006000090106     D CRLF            c                   x'0D25'
006100090106
006200090623     D xcnt            s              2  0 inz(30)
006300100210     D ycnt            s              2  0 inz(16)
006400100215     D fnumber         s              1  0 inz(0)
006500180628        Dcl-s ndate    packed(6:0) inz(0);
006600090105     D acnt            s              4  0
006700090105     D gcnt            s              4  0
006800090107     D crddate         s               d   datfmt(*jul)
006900090107     D julDate         s              7  0
007000090106     D seqcnt          s              6  0
007100121121     Daccount_wtr      s             16
007200121120     Damount_wtr       s             13  0
007300090107       // Identify BANK/FI
007400121121     D Gbtype          s              2  0 inz(5)
007500090106     D rbarr           s              8  0 dim(30)
007600100209     D comma           s              1    inz(',')
007700090107     D zeros           s              9  0 inz(*zeros)
007800090107     D year            s              4  0
007900000209     **
008000090105     D*--------------------------------------
008100090105     D* Prototype for procedure: get_bank_acc
008200090105     D*--------------------------------------
008300090105     D get_bank_acc    PR
008400090105
008500090105
008600090105     D*-----------------------------------------
008700090106     D* Prototype for procedure: prepare_batch
008800090105     D*-----------------------------------------
008900100210     D sendBatch       PR
009000090105
009100090106     D*-----------------------------------------
009200090106     D* Prototype for procedure: write_to_file
009300090106     D*-----------------------------------------
009400090106     D write_to_file   PR
009500090106     D option                         1    const
009600090106        // '1' - create/open file
009700090106        // '2' - write data
009800090106        // '3' - close file
009900090106     D data                         100    const varying options(*nopass)
010000090107
010100090107     D write_batch_detail...
010200090107     D                 PR
010300121120     D amount                        13  0 const
010400100209     D space                          1    const
010500121121     D account                       16    const
010600121119     D space2                         1    const
010700121120     D name                          40    const
010800121119     D space3                         1    const
010900121121     D paidDate                      12    const
011000090106
011100090106     D*-----------------------------------------
011200090106     D* Prototype for procedure: Main_pgm
011300090106     D*-----------------------------------------
011400121120     D  main_pgm       pr                  extpgm('GB0001R')
011500090105     D   citi_errDS                        likeds(citi_err) dim(100)
011600090105     D   err_count                    6  0
011700090105     D   eof                           n
011800090105     D   count                        6  0
011900090105     D   bnk_type                     2  0
012000091010     D   origin                       3    options(*nopass)
012100090106
012200090106     D  main_pgm       pi
012300090106     D   citi_errDS                        likeds(citi_err) dim(100)
012400090106     D   err_count                    6  0
012500090106     D   eof                           n
012600090106     D   count                        6  0
012700090106     D   bnk_type                     2  0
012800091010     D   origin                       3    options(*nopass)
012900091019     **
013000100210     D*--------------------------------------------------
013100100210     D* Procedure name: open_File
013200100210     D* Purpose:        open file, set cursor to retrieve data for sending ...
013300100210     D*                          to IFS
013400100210     D* Returns:
013500100210     D* Parameter:      fileNo
013600100210     D*--------------------------------------------------
013700100210     D open_File       PR
013800100210     D  fileNo                        1P 0
013900100210
014000100210     D*--------------------------------------------------
014100100210     D* Procedure name: closeFile
014200100210     D* Purpose:        close selected file
014300100210     D* Returns:
014400100210     D* Parameter:      fileNo
014500100210     D*--------------------------------------------------
014600100210     D closeFile       PR
014700100210     D  fileNo                        1P 0
014800100210
014900100210     D*--------------------------------------------------
015000100210     D* Procedure name: readFile
015100100210     D* Purpose:
015200100210     D* Returns:
015300100210     D* Parameter:      fileNo
015400100210     D*--------------------------------------------------
015500100210     D readFile        PR              N
015600100210     D  fileNo                        1P 0
015700121120
015800121120     D*--------------------------------------------------
015900121120     D* Procedure name: dateFormat
016000121120     D* Purpose:        set up fancy date format
016100121120     D* Returns:
016200121120     D* Parameter:      inDate => decimal date
016300121120     D*--------------------------------------------------
016400121120     D dateFormat      PR             7A
016500121120     D  inDate                        6P 0
016600121127
016700121127     D*--------------------------------------------------
016800121127     D* Procedure name: writeToholdingFile
016900121127     D* Purpose:        output the data to temp file for excel processing
017000121127     D* Returns:
017100121127     D*--------------------------------------------------
017200121127     D writeToholdingFile...
017300121127     D                 PR
017400121127      *
017500121127     D raw_date        S             12A
017600121127      *
017700121127     D raw_account     S             16A
017800121127
017900121127
018000121120
018100100210
018200100210
018300100210
018400090105      /free
018500090105              // Get Division and Bank account information from file
018600090106              if  count = 0;
018700110120                    open genpysuml4;
018800110120                    setll (origin:bnk_type) payfmt;
018900110120                    reade (origin:bnk_type) payfmt;
019000091019
019100090106
019200110120              if  count = 0;
019300091010
019400090107
019500110120               select;
019600091013                 when origin  = 'PAY';
019700091013                   exec sql
019800091013                     select * into :payelcDS from payelctrn
019900121120                       where petype = :Gbtype;
020000090106
020100100629               // get_bank_acc();
020200091010
020300091013                  // if called from shares system
020400091013                 when origin  = 'SHR';
020500091013                   exec sql
020600091013                     select * into :ShrelcDS from shrelctrn
020700121120                      where setype = :Gbtype;
020800091013
020900091013                   eval-corr payelcDS = shrelcDS;
021000100209                  // if called from shares system
021100100209                 when origin  = 'CRD';
021200100209                   exec sql
021300100209                     select * into :cuselcDS from cuselctrn
021400121120                      where cetype = :Gbtype;
021500100209
021600100209                   eval-corr payelcDS = cuselcDS;
021700100215               endsl;
021800091013
021900100209               // banks acct field to cb acct field
022000100215               payacc = %subst(payelcDS.peacct:1:16);
022100121127                 exec sql
022200121127                   delete from GBTIFile;
022300121127
022400091010              endif;
022500090105
022600090105              if   not(%eof());
022700090105                // get accounting info.
022800100215
022900090105                get_bank_acc();
023000100210                sendBatch();
023100090105
023200110120             //  select;
023300110120              // when origin = 'PAY';
023400100215                dow not(%eof());
023500091010                paysts = 'V';
023600091010                update  payfmt %fields(paysts);
023700110126                    reade (ORIGIN:bnk_type) payfmt;
023800100215                enddo;
023900090105              endif;
024000090105
024100090105              if  %eof();
024200090105                 eof = *on;
024300090105                 *inlr = *on;
024400110120                   close genpysuml4;
024500091013
024600100215
024700090105              endif;
024800100629              endif;
024900090105
025000090105             // RETURN
025100090105             return;
025200090105      /end-free
025300090105
025400090105     P*-----------------------------------------
025500090105     P* Procedure name: get_bank_acc
025600090105     P* Purpose:        get actual bank account
025700090105     P* Returns:        0
025800090105     P* Parameter:      0
025900090105     P*-----------------------------------------
026000090105     P get_bank_acc    B
026100090106     D get_bank_acc    PI
026200090105
026300090105      /free
026400121121          bankacc =  *blanks;
026500090105
026600090105          // check for number in character field
026700090105          dow  xcnt > 0;
026800090105
026900090106              if  %check(digits:payarr(xcnt)) = 0
027000090105                      and ycnt > 0;
027100090105                bankarr(ycnt) = payarr(xcnt);
027200090105                ycnt -=  1;
027300090105              endif;
027400090105
027500090105              xcnt -=  1;
027600090105          enddo;
027700090105
027800090105          // Reset array fields
027900090105          reset  xcnt;
028000090105          reset  ycnt;
028100090105
028200090105      /end-free
028300090105     P get_bank_acc    E
028400090105
028500090105
028600090105
028700090105     P*-----------------------------------------
028800090106     P* Procedure name: prepare_batch
028900090106     P* Purpose:        prepare batch
029000090105     P* Returns:        0
029100090105     P* Parameter:      0
029200090105     P*-----------------------------------------
029300100210     P sendBatch       B
029400100210     D sendBatch       PI
029500090105
029600121120
029700121120     DNAME_WTR         S             40    inz(' ')
029800121121     DShort_date       S             12    inz(' ')
029900100210
030000090105      /free
030100090105
030200090105         //  write header record;
030300090106         //  ...create/open file;
030400090107         //----------------------
030500090106         write_to_file('1');
030600121120         write_to_file('2':Gb_text_header);
030700110126
030800121120       //     exec sql
030900121120       //        select sum(payamt) into :sum_total from GENpysuml1
031000121120       //         where paysts =  'A' and paydiv = :bnk_type and payorg =
031100121120       //         :origin;
031200121120       //     cb_amount = sum_total;
031300121120       //     cb_hdr_acc = bankacc;
031400121120           //write_to_file('2':cb_hder_record);
031500090105
031600100209
031700100209
031800100209
031900090105         //  write detail records;
032000090107         //------------------------
032100110120          //select;
032200110120          //when origin = 'PAY';
032300100215            fnumber = 1;
032400110217
032500100629
032600100215             open_file(fnumber);
032700100215             dow not readFile(fnumber);
032800100210
032900110120          //select;
033000110120          //when origin = 'PAY';
033100100215              payacc = payDS.payacc;
033200121120              amount_wtr  =  payDs.payamt;
033300121120              name_wtr    =  %trim(payDs.paybname);
033400180628              ndate = paycrd;
033500121120              short_Date   =  dateFormat(paycrd);
033600121127               raw_date  = %trim(short_date);
033700121121              short_date = '" ' +%trim(short_date) + ' "';
033800100215               get_bank_acc();
033900121121              account_wtr = '" ' + %trim(bankacc)+ ' "';
034000121127              raw_account = %trim(bankacc);
034100121127
034200090107                // write detail figures
034300121120               write_batch_detail(amount_wtr:comma:account_wtr:comma:Name_wtr:
034400121120                                  comma:short_Date);
034500100215
034600100210             enddo;
034700090107
034800100215            closeFile(fnumber);
034900090107
035000090107
035100090107
035200090107
035300090106             //close file
035400090106             write_to_file('3');
035500090105
035600090105      /end-free
035700100210     P sendBatch       E
035800090106
035900090106     P*-----------------------------------------
036000090106     P* Procedure name: write_to_file
036100090106     P* Purpose:        write to file
036200090106     P* Returns:        0
036300090106     P* Parameter:      0
036400090106     P*-----------------------------------------
036500090106     P write_to_file   B
036600090106     D write_to_file   PI
036700090106     D option                         1    const
036800090106     D data                         100    const varying options(*nopass)
036900090106
037000121120     D/copy QCPYSRC,IFSPR
037100090106     D fd              S             10I 0
037200090106     D wrdata          S            100A
037300090106     D rddata          S            100A
037400090106     D flags           S             10U 0
037500090106     D mode            S             10U 0
037600090106     D Msg             S             50A
037700090106     D Len             S             10I 0
037800090106      /free
037900090106
038000090106       //*************************************************************
038100090106       // writing data to a stream file
038200090106       //****************************************************************
038300090106         select;
038400090106           when option = '1';
038500090107
038600091013               fd = unlink(payelcDS.pefilenm);
038700090107
038800090107             flags = O_WRONLY + O_CREAT + O_TRUNC +
038900090106                     O_TEXTDATA + O_CODEPAGE;
039000090106
039100090106             mode =  S_IRUSR + S_IWUSR
039200090106                         + S_IRGRP + S_IROTH;
039300090106
039400090107             // Open IFS file in ASCII format
039500091010
039600091013             fd = open(payelcDS.pefilenm: flags: mode: 819);
039700091013
039800090106             if  fd < 0;
039900090107             // Msg = 'open(): failed for writing';
040000090107             // return;
040100090106             endif;
040200090106
040300090107             // Close IFS file
040400090106             callp  close(fd);
040500090107
040600090107             // Reopen FS file
040700091013              fd = open(payelcDS.pefilenm:O_WRONLY + O_TEXTDATA);
040800090106
040900090106           // Write some data
041000090106           when option = '2';
041100090106             wrdata = data + CRLF;
041200090106             callp write(fd: %addr(wrdata): %len(%trimr(wrdata)));
041300090106
041400090106           // close the file
041500090106           when option = '3';
041600090106             callp  close(fd);
041700090106         endsl;
041800090106
041900090106      /end-free
042000090106     P write_to_file   E
042100090107
042200090107     P*-----------------------------------------
042300090107     P* Procedure name: write_batch_detail
042400090107     P* Purpose:        write batch detail lines
042500090107     P* Returns:        0
042600090107     P* Parameter:      0
042700090107     P*-----------------------------------------
042800090107     P write_batch_detail...
042900090107     P                 B
043000090107     D write_batch_detail...
043100090107     D                 PI
043200121120     D total                         13  0 const
043300121119     D space                          1    const
043400121121     D account                       16    const
043500121119     D space2                         1    const
043600121120     D name                          40    const
043700121119     D space3                         1    const
043800121121     D paidDate                      12    const
043900121120     D
044000121120
044100121120
044200090107      /free
044300121120
044400090107               //write detail lines
044500121120                gb_det_total   = total;
044600121120                gb_det_comma   = space;
044700121120                gb_det_account = account;
044800121120                gb_det_comma2  = space;
044900121120                gb_det_name    = name;
045000121120                gb_det_comma3  = space;
045100121120                gb_det_date    = paidDate;
045200121120
045300121127               //write record to file to be retrieved in excel.
045400121127               writeToHoldingFile();
045500121119               write_to_file('2':gb_text_detail);
045600090107      /end-free
045700090107     P write_batch_detail...
045800090107     P                 E
045900091019
046000100210
046100100210     P*--------------------------------------------------
046200100210     P* Procedure name: open_File
046300100210     P* Purpose:        open file, set cursor to retrieve data for sending ...
046400100210     P*                          to IFS
046500100210     P* Returns:
046600100210     P* Parameter:      fileNo
046700100210     P*--------------------------------------------------
046800100210     P open_File       B
046900100210     D open_File       PI
047000100210     D  fileNo                        1P 0
047100100210
047200100210
047300100210      /FREE
047400110120          // select;
047500110120            // when fileno = 1;
047600100210         exec sql
047700110120          declare payfile cursor for select * from genpysuml1
047800110325          where paydiv = :bnk_type and paysts = 'A' and payorg = :origin;
047900110325
048000100210        exec sql
048100100210        open payfile;
048200100210
048300100210      /END-FREE
048400100210     P open_File       E
048500100210
048600100210
048700100210     P*--------------------------------------------------
048800100210     P* Procedure name: closeFile
048900100210     P* Purpose:        close selected file
049000100210     P* Returns:
049100100210     P* Parameter:      fileNo
049200100210     P*--------------------------------------------------
049300100210     P closeFile       B
049400100210     D closeFile       PI
049500100210     D  fileNo                        1P 0
049600100210
049700100210
049800100210      /FREE
049900100210
050000110217
050100100210               exec sql
050200100210                 close payfile;
050300110217
050400100210      /END-FREE
050500100210     P closeFile       E
050600100210
050700100210
050800100210     P*--------------------------------------------------
050900100210     P* Procedure name: readFile
051000100210     P* Purpose:
051100100210     P* Returns:
051200100210     P* Parameter:      fileNo
051300100210     P*--------------------------------------------------
051400100210     P readFile        B
051500100210     D readFile        PI              N
051600100210     D  fileNo                        1P 0
051700100210
051800100210
051900100210     D* Local fields
052000100210     D retField        S               N
052100100210
052200100210      /FREE
052300100210         exec sql
052400100629          fetch next from payfile into :payds;
052500100629
052600100215            if sqlstt = '02000';
052700100215           retField = *on;
052800100215
052900100215          else;
053000100215           retField = *off;
053100100215        ENDIF;
053200100210        RETURN retField;
053300100210
053400100210      /END-FREE
053500100210     P readFile        E
053600100210
053700121120
053800121120     P*--------------------------------------------------
053900121120     P* Procedure name: dateFormat
054000121120     P* Purpose:        set up fancy date format
054100121120     P* Returns:
054200121120     P* Parameter:      inDate => decimal date
054300121120     P*--------------------------------------------------
054400121120     P dateFormat      B
054500121120     D dateFormat      PI             7A
054600121120     D  inDate                        6P 0
054700121120
054800121120
054900121120     D* Local fields
055000121120     D retField        S              7A
055100121120     D cdate           S              8A
055200121120     D Ddate           S              8  0
055300121120      * retrieve fancy date from iqdate file
055400121120     D ctypdate        S              9A
055500121120
055600121120      /FREE
055700121120
055800121120           exec sql
055900121120             select dabbr,CYMDDT into :ctypdate,:DDATE from iqdate/iqdate
056000121120               where ERODAT = :inDate;
056100121120
056200121120          cdate = %char(DDate);
056300121120               retField = %subst(cdate:7:2) + %subst(ctypdate:4:3) +
056400121120                         %subst(ctypdate:8:2);
056500121120        RETURN retField;
056600121120
056700121120      /END-FREE
056800121120     P dateFormat      E
056900121120
057000121127
057100121127     P*--------------------------------------------------
057200121127     P* Procedure name: writeToholdingFile
057300121127     P* Purpose:        output the data to temp file for excel processing
057400121127     P* Returns:
057500121127     P*--------------------------------------------------
057600121127     P writeToholdingFile...
057700121127     P                 B
057800121127     D writeToholdingFile...
057900121127     D                 PI
058000121127
058100121127
058200121127      /FREE
058300121127
058400121127         exec sql
058500121127         insert into GBTIFile (gbtsts,gbtTotal,gbtAccount,gbtName,gbtDate,
058600180628                               gbtorg,gbtndate)
058700121127                     values('A',:gb_det_total,:raw_account
058800180628                     ,:gb_det_name,:raw_date,:origin,:ndate);
058900121127
059000121127
059100121127      /END-FREE
059200121127     P writeToholdingFile...
059300121127     P                 E
059400121127
