000100100804     H dftactgrp(*no) actgrp(*CALLER) bnddir('DISDIR')
000200090106     H******  PROGRAMMER - S. BRITTON , c.williams
000300090106     H******  First Coded - 09/01/06
000400110418     Fgenpysuml4uf   e           k disk    usropn
000500991208     **
000600000707     Daccount_fld      ds
000700120319     D payacc
000800120319     D  payarr                        1    overlay(payacc) dim(30)
000900000707     **
001000110418     DScBk_acc_fld     ds
001100120416     D bankaccc                      30
001200120416     D bankarr                        1    overlay(bankaccc) dim(30)
001300000707     **
001400120416     D* rbgrangeDS    e ds                  extname(rbgrange) inz
001500090106     D payelcDS      e ds                  extname(payelctrn) inz
001600091013     D                                     qualified prefix('P':1)
001700091013
001800091013     D shrelcDS      e ds                  extname(shrelctrn) inz
001900091013     D                                     qualified prefix('P':1)
002000090105
002100100804     D cuselcDS      e ds                  extname(cuselctrn) inz
002200100804     D                                     qualified prefix('P':1)
002300000711     **
002400090105     Dciti_err         ds                  template
002500000711     D e_payemp                       6  0
002600000711     D e_payacc                      12
002700000711     D e_paycrd                       6  0
002800000711     D e_payname                     26
002900101108     ** Record Header-Scotia Bank
003000101108     Dsb_header_St     ds                  qualified  inz
003100101108     D sb_hrecID                      2    inz('02')
003200110418     D sb_hbatchNo                    4  0 inz(1)
003300110418     D sb_hbatchIND                   1    inz('C')
003400101108     D sb_htotRec                     5  0
003500120405     D sb_htotAmt                    15  2
003600101108     D sb_heffDate                    8  0
003700120416     D sb_hecrtDate                   8  0
003800110418     D sb_hdecPlace                   1  0 inz(2)
003900120202     D sb_hCMSIDace                  10    inz('BANKSDIHLD')
004000120202     D sb_hCTransit                   5  0 INZ(30775)
004100101108     D sb_hCAcctNo                    9  0
004200110418     D sb_hExprecFlg                  1    inz('*')
004300101108     *****Template for Detail Lines*****************
004400101108     Dsb_detail        ds
004500101108     D sb_drecID                      2    inz('03')
004600110418     D sb_dbatchNo                    4  0 inz(1)
004700120315     D sb_dTransit                    5  0 inz(30775)
004800120316     D sb_dCAcctNo                   17
004900120405     D sb_dDollAmt                   13  2
005000101108     D sb_dReffld                    25
005100101108     D sb_dInstruc                   15
005200120315     D sb_dDCbatchIND                 1    inz('C')
005300120202     D sb_ddecPlace                   1  0 inz(2)
005400120202     D sb_dCMSID                     10    inz('BANKSDIHLD')
005500120202     D sb_dRouterTrns                09    INZ(' ')                             LEAVE BLANK
005600120202     D sb_dBankName                  25    INZ(' ')                             LEAVE BLANK
005700101108     *********Footer Record
005800101108     Dsb_text_footer   ds
005900101108     D sb_FrecID                      2    inz('04')
006000101108     D sb_FnumRecs                    5  0
006100120405     D sb_FtotAmt                    18  2
006200101108     ***********************************************
006300101108
006400091019
006500091019      // General PROTOTYPES
006600120301      /copy qcpysrc,GENPR
006700090105
006800120412     D DIGITS          c                   '0123456789'
006900090106     D CRLF            c                   x'0D25'
007000101108     D xcnt            s              2  0 inz(30)
007100120416     D ycnt            s              2  0 inz(17)
007200101108     D totRecords      s              4  0
007300101108     D totVal          s             13  2
007400090107     D crddate         s               d   datfmt(*jul)
007500101108     D isoDateCrd      s              8  0
007600101108     D createISO       s              8  0
007700090106     D seqcnt          s              6  0
007800110419     D pacct           s              9  0
007900090107       // Identify BANK/FI
008000110418     D sbtype          s              2  0 inz(3)
008100101108     D sbarr           s              8  0 dim(30)
008200090107     D spaceholder     s              2    inz(*blanks)
008300090107     D zeros           s              9  0 inz(*zeros)
008400110209     D transit#g       s              9    inz(*blank)
008500110209     D
008600000209     **
008700090105     D*--------------------------------------
008800090105     D* Prototype for procedure: get_bank_acc
008900090105     D*--------------------------------------
009000090105     D get_bank_acc    PR
009100090105
009200090105     D*-----------------------------------------
009300090105     D* Prototype for procedure: write_to_struc
009400090105     D*-----------------------------------------
009500090105     D write_to_struc  PR
009600110209     D org                            3
009700090105
009800090105     D*-----------------------------------------
009900090106     D* Prototype for procedure: prepare_batch
010000090105     D*-----------------------------------------
010100090106     D prepare_batch   PR
010200090105
010300090106     D*-----------------------------------------
010400090106     D* Prototype for procedure: write_to_file
010500090106     D*-----------------------------------------
010600090106     D write_to_file   PR
010700090106     D option                         1    const
010800090106        // '1' - create/open file
010900090106        // '2' - write data
011000090106        // '3' - close file
011100101108     D data                         128    const varying options(*nopass)
011200090107
011300090107     D write_batch_detail...
011400090107     D                 PR
011500110209     D transit#                       9
011600110209     D space                          2
011700120316     Dcaccount                       17
011800110209     D DCtype                         1
011900110209     D total                         15  2
012000110209     D
012100110209     D
012200090106
012300090106     D*-----------------------------------------
012400090106     D* Prototype for procedure: Main_pgm
012500090106     D*-----------------------------------------
012600101108     D  main_pgm       pr                  extpgm('SB0001R')
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
014200091019     D*------------------------------------------
014300091019     D* Prototype for procedure: get_credit_date
014400091019     D*------------------------------------------
014500091019     D get_credit_date...
014600101108     D                 PR             8  0
014700091019     D dateCrd                        6  0 const
014800091019     D dateAcc                        6  0 const
014900101108     D datecrt                        8  0
015000101108
015100101108     D*--------------------------------------------------
015200101108     D* Procedure name: GetRecSummary
015300101108     D* Purpose:        get summary informationfor head record
015400101108     D* Returns:
015500101108     D* Parameter:      fileNo
015600101108     D* Parameter:      tot_records => number of records
015700101108     D* Parameter:      totValue => total value of payroll
015800101108     D*--------------------------------------------------
015900101108     D GetRecSummary   PR
016000101108     D  fileOrg                       3
016100101108     D  tot_records                   4P 0
016200101108     D  totValue                     13P 2
016300101108
016400101108     D*--------------------------------------------------
016500101108     D* Procedure name: writeFooter
016600101108     D* Purpose:
016700101108     D* Returns:
016800101108     D*--------------------------------------------------
016900101108     D writeFooter     PR
017000110209      * hold the Constant Value of C(credit) for payroll value type.
017100110209     D DCtypeC         C                   CONST('C')
017200120321      *
017300120321     D bankacc         S             17P 0
017400120412
017500120412     D*--------------------------------------------------
017600120412     D* Procedure name: getTransit#
017700120412     D* Purpose:        get account transit number based on account number
017800120412     D* Returns:
017900120412     D* Parameter:      account#
018000120412     D*--------------------------------------------------
018100120412     D getTransit#     PR             5P 0
018200120412     D  account#                     17P 0
018300120412      *
018400120412     D DEFAULTTRANSIT  C                   CONST(30775)
018500120412
018600120412
018700101108
018800101108
018900090105      /free
019000090105              // Get Division and Bank account information from file
019100090106              if  count = 0;
019200101108
019300101108       // get summary info from file
019400101108                getRecSummary(origin:totRecords:totVal);
019500101108                SB_header_St.sb_htotrec = totRecords;
019600101108                SB_header_St.sb_htotamt = totVal;
019700101108
019800101108       // footer information
019900110209                SB_fnumrecs = totRecords;
020000101108                SB_ftotamt = totVal;
020100110120                  //open general bank file
020200110120                    open genpysuml4;
020300110120                    setll (origin:bnk_type) payfmt;
020400110120                    reade (origin:bnk_type) payfmt;
020500110120                   IsoDateCrd = get_credit_date(paycrd:paydate:createISO);
020600090106              endif;
020700090105
020800101108
020900091019
021000090106
021100090105              if  count = 0;
021200091010
021300120508               //as per scotia request, effective date must be same as date being uploaded
021400101108                SB_header_St.sb_heffdate= isoDateCrd;
021500120508
021600120508                //as per scotia request, created date must be same as date being uploaded
021700120508                SB_header_St.sb_hecrtdate= isoDateCrd;
021800120508              // SB_header_St.sb_hecrtdate= createIso;
021900091010
022000090107
022100091010               select;
022200091013                 when origin  = 'PAY';
022300091013                   exec sql
022400091013                     select * into :payelcDS from payelctrn
022500101108                       where petype = :Sbtype;
022600090106
022700091010
022800091013                  // if called from shares system
022900091013                 when origin  = 'SHR';
023000091013                   exec sql
023100091013                     select * into :ShrelcDS from shrelctrn
023200101108                      where setype = :Sbtype;
023300091013
023400091013                   eval-corr payelcDS = shrelcDS;
023500100804                     when origin  = 'CRD';
023600100804
023700100804
023800100804                     exec sql
023900100804                     select * into :cuselcDS from cuselctrn
024000101108                      where cetype = :Sbtype;
024100100804
024200100804                   eval-corr payelcDS = cuselcDS;
024300091013               endsl;
024400091013
024500110419               // banks acct field to sb acct field
024600120404               pacct = %int(%subst(payelcDS.peacct:7));
024700110419                SB_header_St.sb_hCAcctno = pacct;
024800091013                // init batch sequence counter
024900091013                seqcnt = payelcDS.pelower -1;
025000101108
025100101108             //write header data to file
025200101108               prepare_batch();
025300091010              endif;
025400101108              //-------------detailed Records Section---------------------//
025500090105              if   not(%eof());
025600110418                 dow not(%eof());
025700110418
025800090105                // get accounting info.
025900090105                get_bank_acc();
026000120416              //  getTtansit#();
026100101108                // write detailed records, one at a time
026200101108                write_to_struc(origin);
026300110418
026400091010                paysts = 'V';
026500091010                update  payfmt %fields(paysts);
026600110418                       reade (ORIGIN:bnk_type) payfmt;
026700110418                enddo;
026800090105
026900090105              endif;
027000090105
027100090105              if  %eof();
027200090105                 eof = *on;
027300090105                 *inlr = *on;
027400110120                   close genpysuml4;
027500101108                 // write footer record
027600101108                   writeFooter();
027700101108                 //close file
027800101108                   write_to_file('3');
027900090105              endif;
028000090105
028100110418             // RETURN
028200110418           //  commit;
028300090105             return;
028400090105      /end-free
028500090105
028600090105     P*-----------------------------------------
028700090105     P* Procedure name: get_bank_acc
028800090105     P* Purpose:        get actual bank account
028900090105     P* Returns:        0
029000090105     P* Parameter:      0
029100090105     P*-----------------------------------------
029200090105     P get_bank_acc    B
029300090106     D get_bank_acc    PI
029400120319      *
029500120319     D accountHolder   S             17P 0
029600120319      *
029700120319
029800120319
029900090105      /free
030000120319
030100120319
030200120319
030300090105          bankacc =  *zeros;
030400120416          clear bankaccc;
030500120319              payacc = %trim(payacc);
030600090105          // check for number in character field
030700090105          dow  xcnt > 0;
030800090105
030900090106              if  %check(digits:payarr(xcnt)) = 0
031000090105                      and ycnt > 0;
031100090105                bankarr(ycnt) = payarr(xcnt);
031200090105                ycnt -=  1;
031300090105              endif;
031400090105
031500090105              xcnt -=  1;
031600090105          enddo;
031700120416          bankacc = %dec(%trim(bankaccc):17:0);
031800120418
031900120418
032000120319
032100120319
032200090105          // Reset array fields
032300090105          reset  xcnt;
032400090105          reset  ycnt;
032500090105
032600090105      /end-free
032700090105     P get_bank_acc    E
032800090105
032900120416
033000090105
033100090105     P*-----------------------------------------
033200090105     P* Procedure name: write_to_struc
033300090105     P* Purpose:        write to structure
033400090105     P* Returns:        0
033500090105     P* Parameter:      0
033600090105     P*-----------------------------------------
033700090105     P write_to_struc  B
033800090105     D write_to_struc  PI
033900101108     D origin                         3
034000101108     d
034100110209     Ddctype           S              1
034200110209     Daccount          S             17  0
034300120316     DCHARACCT         S             17
034400110209     D transit#        s              9
034500110209     D payamt#         s             15  2
034600090105      /free
034700090105
034800101108
034900101108            //  write detail records;
035000101108         //------------------------
035100110418           // acct# = %subst(%triml(payacc):1:17);
035200110418
035300110418           //   acct#2 = %dec(acct#:len(acct#):0);
035400110418            //    account = %dec(acct#:17:0);
035500120316               characct = %editc(bankacc:'X');
035600120319
035700110418                account = bankacc;
035800120418
035900120412                  sb_Dtransit = getTransit#(account);
036000110209                              dctype = dctypec;
036100110209                              payamt# = payamt;
036200120316                write_batch_detail(transit#g:spaceholder:characct:
036300120316                dctype:payamt#);
036400101108
036500090105
036600090105      /end-free
036700090105     P write_to_struc  E
036800090105
036900101108
037000090105     P*-----------------------------------------
037100090106     P* Procedure name: prepare_batch
037200090106     P* Purpose:        prepare batch
037300120319     P* Purpose:        prepare batch
037400090105     P* Returns:        0
037500090105     P* Parameter:      0
037600090105     P*-----------------------------------------
037700090106     P prepare_batch   B
037800090106     D prepare_batch   PI
037900090105
038000090106
038100090106     D CREDITCODE      c                   'C'
038200090107     D DEBITCODE       c                   'D'
038300090105      /free
038400090105
038500090105         //  write header record;
038600090106         //  ...create/open file;
038700090107         //----------------------
038800090106         write_to_file('1');
038900101108
039000101108
039100101108
039200101108         //  write header record;
039300101108         //------------------------
039400101108
039500110209         write_to_file('2':sb_header_st);
039600090105
039700101108
039800090105      /end-free
039900090106     P prepare_batch   E
040000090106
040100090106     P*-----------------------------------------
040200090106     P* Procedure name: write_to_file
040300090106     P* Purpose:        write to file
040400090106     P* Returns:        0
040500090106     P* Parameter:      0
040600090106     P*-----------------------------------------
040700090106     P write_to_file   B
040800090106     D write_to_file   PI
040900090106     D option                         1    const
041000101108     D data                         128    const varying options(*nopass)
041100090106
041200120301     D/copy qcpysrc,IFSPR
041300090106     D fd              S             10I 0
041400110419     D wrdata          S           1024A
041500090106     D rddata          S            100A
041600090106     D flags           S             10U 0
041700090106     D mode            S             10U 0
041800090106     D Msg             S             50A
041900090106     D Len             S             10I 0
042000090106      /free
042100090106
042200090106       //*************************************************************
042300090106       // writing data to a stream file
042400090106       //****************************************************************
042500090106         select;
042600090106           when option = '1';
042700090107
042800091013               fd = unlink(payelcDS.pefilenm);
042900090107
043000090107             flags = O_WRONLY + O_CREAT + O_TRUNC +
043100090106                     O_TEXTDATA + O_CODEPAGE;
043200090106
043300090106             mode =  S_IRUSR + S_IWUSR
043400090106                         + S_IRGRP + S_IROTH;
043500090106
043600090107             // Open IFS file in ASCII format
043700091010
043800091013             fd = open(payelcDS.pefilenm: flags: mode: 819);
043900091013
044000090106             if  fd < 0;
044100090107             // Msg = 'open(): failed for writing';
044200090107             // return;
044300090106             endif;
044400090106
044500090107             // Close IFS file
044600090106             callp  close(fd);
044700090107
044800090107             // Reopen FS file
044900091013              fd = open(payelcDS.pefilenm:O_WRONLY + O_TEXTDATA);
045000090106
045100090106           // Write some data
045200090106           when option = '2';
045300090106             wrdata = data + CRLF;
045400090106             callp write(fd: %addr(wrdata): %len(%trimr(wrdata)));
045500090106
045600090106           // close the file
045700090106           when option = '3';
045800090106             callp  close(fd);
045900090106         endsl;
046000090106
046100090106      /end-free
046200090106     P write_to_file   E
046300090107
046400090107     P*-----------------------------------------
046500090107     P* Procedure name: write_batch_detail
046600090107     P* Purpose:        write batch detail lines
046700090107     P* Returns:        0
046800090107     P* Parameter:      0
046900090107     P*-----------------------------------------
047000090107     P write_batch_detail...
047100101108     P                 B
047200090107     D write_batch_detail...
047300090107     D                 PI
047400110209     D transit#                       9
047500110209     D space                          2
047600120316     Dcaccount                       17
047700110209     D DCtype                         1
047800110209     D total                         15  2
047900101108     D
048000090107      /free
048100090107               //write detail lines
048200120315
048300120316               sb_dCAcctNo = caccount;
048400120405             //sb_dDollAmt = %dech(total:13:0);
048500120405                 sb_dDollAmt = total;
048600101108               sb_dReffld = space;
048700090107
048800110209               sb_dInstruc = paybname;
048900090107
049000090107
049100120202             //Exec sql
049200120202             //select dtname into :sb_dBankname from paydiv
049300120202             //  where dtno = :bnk_type;
049400120202         //Bank name is only needed when payment is being sent to another bank
049500120202                SB_DBANKNAME = *BLANKS;
049600120202               sb_dRouterTrns = *blanks;
049700110209             //  sb_dBankname  = paybname;   //payee name
049800101108
049900110209               write_to_file('2':sb_detail);
050000090107      /end-free
050100090107     P write_batch_detail...
050200090107     P                 E
050300091019
050400091019     P*-----------------------------------------
050500091019     P* Procedure name: get_credit_date
050600091019     P* Purpose:        get_credit_date
050700091019     P* Returns:        0
050800091019     P* Parameter:      0
050900091019     P*-----------------------------------------
051000091019     P get_credit_date...
051100091019     P                 B
051200091019     D get_credit_date...
051300101108     D                 PI             8  0
051400091019     D dateIn                         6  0 const
051500091019     D dateAcc                        6  0 const
051600101108     D createDate                     8  0
051700091019
051800091019     D olddateISO      s              8  0
051900101108     D newdate         s              8  0
052000091019      /free
052100091019
052200101108           // get ISO version of Date Created
052300101108             createDate = %dec(%date(dateAcc:*dmy):*iso);
052400091019             // DO NOT allow Credit date falling on a weekend
052500091019             //.... day 6 or 7
052600140911             monitor;
052700091019             olddateISO = %dec(%date(dateIn:*dmy):*iso);
052800091019             if dayofweek(olddateISO) = 6
052900091019               or dayofweek(olddateISO) = 7;
053000091019             //... etc.
053100091019             //newdate = %dec(%date(olddateISO:*dmy)+ %days(1));
053200101108               newdate = createDate;
053300091019              else;
053400101108               newdate = olddateISO;
053500101108
053600091019             endif;
053700140911             on-error;
053800140911             show_msg('Error found with '+ %char(payemp)+ ':'+ %char(dateIn));
053900140911             *inlr = *on;
054000140911             newdate = 0;
054100140911             return newdate;
054200140911             endmon;
054300091019
054400091019          return newdate;
054500091019
054600091019      /end-free
054700091019     P get_credit_date...
054800091019     P                 E
054900090107
055000101108
055100101108     P*--------------------------------------------------
055200101108     P* Procedure name: GetRecSummary
055300101108     P* Purpose:        get summary informationfor head record
055400101108     P* Returns:
055500101108     P* Parameter:      fileNo
055600101108     P* Parameter:      tot_records => number of records
055700101108     P* Parameter:      totValue => total value of payroll
055800101108     P*--------------------------------------------------
055900101108     P GetRecSummary   B
056000101108     D GetRecSummary   PI
056100101108     D  fileN                         3
056200101108     D  tot_records                   4P 0
056300101108     D  totValue                     13P 2
056400101108
056500101108
056600101108      /FREE
056700110126
056800101108                exec sql
056900101108                select sum(payamt), count(*) into :totValue,:tot_records
057000110418                  from genpysuml4 where paydiv = :bnk_type and payorg = :fileN;
057100101108
057200110126
057300101108
057400101108            return;
057500101108      /END-FREE
057600101108     P GetRecSummary   E
057700101108
057800101108
057900101108     P*--------------------------------------------------
058000101108     P* Procedure name: writeFooter
058100101108     P* Purpose:
058200101108     P* Returns:
058300101108     P*--------------------------------------------------
058400101108     P writeFooter     B
058500101108     D writeFooter     PI
058600101108
058700101108
058800101108      /FREE
058900101108
059000101108          write_to_file('2':sb_text_footer);
059100101108
059200101108      /END-FREE
059300101108     P writeFooter     E
059400101108
059500120412
059600120412     P*--------------------------------------------------
059700120412     P* Procedure name: getTransit#
059800120412     P* Purpose:        get account transit number based on account number
059900120412     P* Returns:
060000120412     P* Parameter:      account#
060100120412     P*--------------------------------------------------
060200120412     P getTransit#     B
060300120412     D getTransit#     PI             5P 0
060400120412     D  account#                     17P 0
060500120412
060600120412
060700120412     D* Local fields
060800120412     D retField        S              5P 0
060900120412
061000120412      /FREE
061100120507        // check if an exception record is located
061200120507        // if found, use that, else use standard checks
061300120507
061400120507            exec sql
061500120507            select sbttransit into :retfield from sbtexcpt
061600120507              where sbtacct = :account#;
061700120507
061800120507          // if record found, leave with it.
061900120507
062000120507          if sqlstt = '00000';
062100120507            return retField;
062200120507          ENDIF;
062300120507
062400120507         // if no exception record found, check range file
062500120412          exec sql
062600120412            select sbrtrans# into :retfield from sbgrange
062700120418              where :account# between sbrfrange and sbrtrange;
062800120416
062900120417
063000120418          if sqlstt = '02000';
063100120418
063200120412            retField = DEFAULTTRANSIT;
063300120412          ENDIF;
063400120412
063500120412        RETURN retField;
063600120412
063700120412      /END-FREE
063800120412     P getTransit#     E
063900120412
