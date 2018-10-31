000100090106     H dftactgrp(*no) actgrp(*CALLER)
000200090106     H******  PROGRAMMER - S. BRITTON , c.williams
000300090106     H******  First Coded - 09/01/06
000400090107     Fbnkpysuml4uf   e           k disk    commit
000500991208     **
000600000707     Daccount_fld      ds
000700000707     D payacc
000800000707     D  payarr                        1    overlay(payacc) dim(12)
000900000707     **
001000090106     DCiti_acc_fld     ds
001100090106     D bankacc                        7  0
001200090106     D bankarr                        1    overlay(bankacc) dim(7)
001300000707     **
001400090105     D rbgrangeDS    e ds                  extname(rbgrange) inz
001500090106     D payelcDS      e ds                  extname(payelctrn) inz
001600090105
001700000711     **
001800090105     Dciti_err         ds                  template
001900000711     D e_payemp                       6  0
002000000711     D e_payacc                      12
002100000711     D e_paycrd                       6  0
002200000711     D e_payname                     26
002300090105
002400090106     Drb_struc         ds                  qualified  dim(30) inz
002500090105     D rb_numrec                      4  0
002600090105     D rb_transdet                         likeDS(rb_data) dim(500)
002700090105     D rb_transsum                         likeDS(rb_summ)
002800090105
002900090105     D rb_summ         ds                  template
003000090105     D rb_amount                     15  2
003100090105     D rb_dbacc                       7  0
003200090105     D rb_cracc                       7  0
003300090105
003400090105     Drb_data          ds                  template
003500090105     D rb_account                     7  0
003600090105     D rb_total                      15  2
003700090105     D rb_charges                    15  2
003800090105
003900000707     **
004000090105
004100090105     Drb_text_header   ds
004200090106     D rb_head_zeros                 16  0 inz(*zeros)
004300090106     D rb_head_date                   7  0
004400090106     D rb_head_stuff                  9  0 inz(009154503)
004500090105
004600090105     Drb_text_detail   ds
004700090105     D rb_det_transit                 8  0
004800090105     D rb_det_space                   2
004900090105     D rb_det_seq#                    6  0
005000090105     D rb_det_paytext                 4    inz('PAYR')
005100090105     D rb_det_paycurr                 3    inz('GYD')
005200090105     D rb_det_account                 7  0
005300090105     D rb_det_spcacc                  3  0
005400090105     D rb_det_acctype                 1
005500090105     D rb_det_total                  15  2
005600090106     D rb_det_date1                   7  0
005700090106     D rb_det_date2                   7  0
005800090107     D rb_det_stuff                   9  0
005900090105
006000090105     Drb_text_footer   ds
006100090105     D rb_foot_trans                  8  0 inz(99999999)
006200090105     D rb_foot_space                  8
006300090105     D rb_foot_crtot                 15  2
006400090105     D rb_foot_dbtot                 15  2
006500090106     D rb_foot_crcnt                  7  0
006600090106     D rb_foot_dbcnt                  7  0
006700090105
006800090106     D DIGITS          c                   '0123456789'
006900090106     D CRLF            c                   x'0D25'
007000090106
007100000707     D xcnt            s              2  0 inz(12)
007200090106     D ycnt            s              2  0 inz(7)
007300090105     D acnt            s              4  0
007400090105     D gcnt            s              4  0
007500090107     D crddate         s               d   datfmt(*jul)
007600090107     D julDate         s              7  0
007700090106     D seqcnt          s              6  0
007800090107     D pacct           s              7  0
007900090107       // Identify BANK/FI
008000090106     D rbtype          s              2  0 inz(2)
008100090106     D rbarr           s              8  0 dim(30)
008200090107     D spaceholder     s              2    inz(*blanks)
008300090107     D zeros           s              9  0 inz(*zeros)
008400090107     D year            s              4  0
008500000209     **
008600090105     D*--------------------------------------
008700090105     D* Prototype for procedure: get_bank_acc
008800090105     D*--------------------------------------
008900090105     D get_bank_acc    PR
009000090105
009100090105     D*-----------------------------------------
009200090105     D* Prototype for procedure: get_transit_no
009300090105     D*-----------------------------------------
009400090105     D get_transit_no  PR
009500090105
009600090105     D*-----------------------------------------
009700090105     D* Prototype for procedure: write_to_struc
009800090105     D*-----------------------------------------
009900090105     D write_to_struc  PR
010000090105
010100090105     D*-----------------------------------------
010200090106     D* Prototype for procedure: prepare_batch
010300090105     D*-----------------------------------------
010400090106     D prepare_batch   PR
010500090105
010600090106     D*-----------------------------------------
010700090106     D* Prototype for procedure: write_to_file
010800090106     D*-----------------------------------------
010900090106     D write_to_file   PR
011000090106     D option                         1    const
011100090106        // '1' - create/open file
011200090106        // '2' - write data
011300090106        // '3' - close file
011400090106     D data                         100    const varying options(*nopass)
011500090107
011600090107     D write_batch_detail...
011700090107     D                 PR
011800090107     D transit#                       8  0 const
011900090107     D space                          2    const
012000090107     D seq#                           6  0 const
012100090107     D account                        7  0 const
012200090107     D specialacc                     3  0 const
012300090107     D gltype                         1    const
012400090107     D total                         15  2 const
012500090107     D date1                          7  0 const
012600090107     D date2                          7  0 const
012700090107     D stuff                          9  0 const
012800090106
012900090106     D*-----------------------------------------
013000090106     D* Prototype for procedure: Main_pgm
013100090106     D*-----------------------------------------
013200090105     D  main_pgm       pr                  extpgm('RB0001R')
013300090105     D   citi_errDS                        likeds(citi_err) dim(100)
013400090105     D   err_count                    6  0
013500090105     D   eof                           n
013600090105     D   count                        6  0
013700090105     D   bnk_type                     2  0
013800090106
013900090106     D  main_pgm       pi
014000090106     D   citi_errDS                        likeds(citi_err) dim(100)
014100090106     D   err_count                    6  0
014200090106     D   eof                           n
014300090106     D   count                        6  0
014400090106     D   bnk_type                     2  0
014500090105
014600090105      /free
014700090105              // Get Division and Bank account information from file
014800090106              if  count = 0;
014900090106                setll bnk_type payfmt;
015000090106              endif;
015100090105
015200090105              reade bnk_type payfmt;
015300090106
015400090105              if  count = 0;
015500090106                // set credit date for batch
015600090105                crddate = %date(paycrd:*dmy);
015700090107                year = %subdt(crddate:*years);
015800090106
015900090107                // save converted date in Julian date format
016000090107                //julDate = %dec(crddate:*JUL);
016100090107                julDate =
016200090107                         %int(%subst(%char(year):1:2) +
016300090107                              %editc(%dec(crddate:*JUL):'X'));
016400090107
016500090106                exec sql
016600090106                   select * into :payelcDS from payelctrn
016700090106                    where petype = :rbtype;
016800090106
016900090107               // banks acct field to rb acct field
017000090107               pacct = %int(%subst(peacct:1:7));
017100090106                // init batch sequence counter
017200090107                seqcnt = pelower -1;
017300090105              endif;
017400090105
017500090105              if   not(%eof());
017600090105                // get accounting info.
017700090105                get_bank_acc();
017800090106                get_transit_no();
017900090105                write_to_struc();
018000090105
018100090107             // paysts = 'V';
018200090105                update  payfmt;
018300090105
018400090105              endif;
018500090105
018600090105              if  %eof();
018700090105                 eof = *on;
018800090105                 *inlr = *on;
018900090105
019000090106                 prepare_batch();
019100090105              endif;
019200090105
019300090105             // RETURN
019400090105             return;
019500090105      /end-free
019600090105
019700090105     P*-----------------------------------------
019800090105     P* Procedure name: get_bank_acc
019900090105     P* Purpose:        get actual bank account
020000090105     P* Returns:        0
020100090105     P* Parameter:      0
020200090105     P*-----------------------------------------
020300090105     P get_bank_acc    B
020400090106     D get_bank_acc    PI
020500090105
020600090105      /free
020700090105          bankacc =  *zeros;
020800090105
020900090105          // check for number in character field
021000090105          dow  xcnt > 0;
021100090105
021200090106              if  %check(digits:payarr(xcnt)) = 0
021300090105                      and ycnt > 0;
021400090105                bankarr(ycnt) = payarr(xcnt);
021500090105                ycnt -=  1;
021600090105              endif;
021700090105
021800090105              xcnt -=  1;
021900090105          enddo;
022000090105
022100090105          // Reset array fields
022200090105          reset  xcnt;
022300090105          reset  ycnt;
022400090105
022500090105      /end-free
022600090105     P get_bank_acc    E
022700090105
022800090105     P*-----------------------------------------
022900090105     P* Procedure name: get_bank_acc
023000090105     P* Purpose:        get actual bank account
023100090105     P* Returns:        0
023200090105     P* Parameter:      0
023300090105     P*-----------------------------------------
023400090105     P get_transit_no  B
023500090105     D get_transit_no  PI
023600090105
023700090105      /free
023800090106         clear rbgrangeDS;
023900090105         exec sql
024000090105           select * into :rbgrangeDS from rbgrange
024100090107              where rbrfrange <= :bankacc and
024200090107                  rbrtrange >= :bankacc;
024300090107
024400090107         if rbrtrans# = *zeros;
024500090107            rbrtrans# = pedfttrn#;
024600090107            exec sql
024700090107             select * into :rbgrangeDS from rbgrange
024800090107               where rbrfrange <= :bankacc and
024900090107                   rbrtrange >= :bankacc;
025000090107         endif;
025100090105      /end-free
025200090105     P get_transit_no  E
025300090105
025400090105
025500090105     P*-----------------------------------------
025600090105     P* Procedure name: write_to_struc
025700090105     P* Purpose:        write to structure
025800090105     P* Returns:        0
025900090105     P* Parameter:      0
026000090105     P*-----------------------------------------
026100090105     P write_to_struc  B
026200090105     D write_to_struc  PI
026300090105
026400090105      /free
026500090105
026600090105             acnt = 0;
026700090106             acnt = %lookup(rbrtrans#:rbarr);
026800090105             if acnt = 0;
026900090105                gcnt += 1;
027000090106                rbarr(gcnt) = rbrtrans#;
027100090105
027200090105                acnt = gcnt;
027300090105
027400090105             endif;
027500090105
027600090105             // accumulate totals
027700090105             rb_struc(acnt).rb_numrec += 1;
027800090105             // write detail figures
027900090105             rb_struc(acnt).rb_transdet(rb_struc(acnt).rb_numrec)
028000090105                                          .rb_Account = bankacc;
028100090105             rb_struc(acnt).rb_transdet(rb_struc(acnt).rb_numrec)
028200090105                                          .rb_total = payamt;
028300090105             rb_struc(acnt).rb_transdet(rb_struc(acnt).rb_numrec)
028400090105                                          .rb_charges = pechgamt;
028500090105             // write summary figures
028600090105             rb_struc(acnt).rb_transsum.rb_amount += payamt;
028700090105             rb_struc(acnt).rb_transsum.rb_dbacc = rbrgl;
028800090105             rb_struc(acnt).rb_transsum.rb_cracc = rbrgl;
028900090105
029000090105      /end-free
029100090105     P write_to_struc  E
029200090105
029300090105
029400090105     P*-----------------------------------------
029500090106     P* Procedure name: prepare_batch
029600090106     P* Purpose:        prepare batch
029700090105     P* Returns:        0
029800090105     P* Parameter:      0
029900090105     P*-----------------------------------------
030000090106     P prepare_batch   B
030100090106     D prepare_batch   PI
030200090105
030300090105     D acnt            s              4  0
030400090105     D bcnt            s              4  0
030500090107     D numDebrec       s              7  0
030600090107     D numCrdrec       s              7  0
030700090107     D numDebamt       s             15  2
030800090107     D numCrdamt       s             15  2
030900090107     D batchtotal      s             15  2
031000090107     D batchcharges    s             15  2
031100090107     D clearingacc     s              7  0
031200090106
031300090107     D SPACTCODE       c                   300
031400090107     D SPACTCODE1      c                   301
031500090107     D SPACTCODE2      c                   051
031600090107     D SPACTCODE3      c                   075
031700090106     D CREDITCODE      c                   'C'
031800090107     D DEBITCODE       c                   'D'
031900090105      /free
032000090105
032100090105         //  write header record;
032200090106         //  ...create/open file;
032300090107         //----------------------
032400090106         write_to_file('1');
032500090107         rb_head_date = julDate;
032600090106         write_to_file('2':rb_text_header);
032700090105
032800090105         //  write detail records;
032900090107         //------------------------
033000090105          for acnt = 1 to gcnt;
033100090107
033200090105             // accumulate totals
033300090105             for bcnt = 1 to rb_struc(acnt).rb_numrec;
033400090107               // write detail figures
033500090107               rb_struc(acnt).rb_transdet(bcnt)
033600090105                                          .rb_charges = pechgamt;
033700090106               //write detail lines
033800090107               seqcnt += 1;
033900090107               numCrdrec += 1;
034000090107               batchtotal += rb_struc(acnt).rb_transdet(bcnt)
034100090107                                            .rb_total;
034200090107               batchcharges += rb_struc(acnt).rb_transdet(bcnt)
034300090107                                            .rb_charges;
034400090107
034500090107               numCrdamt  += rb_struc(acnt).rb_transdet(bcnt)
034600090107                                            .rb_total;
034700090107                     //   +  rb_struc(acnt).rb_transdet(bcnt)
034800090107                     //                     .rb_charges);
034900090106
035000090107               write_batch_detail(rbarr(acnt):
035100090107                                  spaceholder:
035200090107                                       seqcnt:
035300090107                 rb_struc(acnt).rb_transdet(bcnt).rb_Account:
035400090107                                    SPACTCODE:
035500090107                                   CREDITCODE:
035600090107                rb_struc(acnt).rb_transdet(bcnt).rb_total:
035700090107                                      julDate:
035800090107                                      julDate:
035900090107                                       zeros);
036000090107
036100090105             endfor;
036200090107
036300090107
036400090105             // write summary figures
036500090107             //-------------------------------
036600090107             // write credit summary record
036700090107             //-------------------------------
036800090107          seqcnt += 1;
036900090107
037000090107               numCrdrec += 1;
037100090107               numCrdamt  +=  rb_struc(acnt).rb_transsum.rb_amount;
037200090107
037300090107               write_batch_detail(rbarr(acnt):
037400090107                                  spaceholder:
037500090107                                       seqcnt:
037600090107                                   pedftchgac:
037700090107                                   SPACTCODE2:
037800090107                                    CREDITCODE:
037900090107          rb_struc(acnt).rb_transsum.rb_amount:
038000090107                                      julDate:
038100090107                                      julDate:
038200090107                                       zeros);
038300090107
038400090107             //-------------------------------
038500090107             // write Debit summary record
038600090107             //-------------------------------
038700090107          seqcnt += 1;
038800090107
038900090107               numDebrec += 1;
039000090107               numDebamt  +=  rb_struc(acnt).rb_transsum.rb_amount;
039100090107
039200090107               write_batch_detail(rbarr(acnt):
039300090107                                  spaceholder:
039400090107                                       seqcnt:
039500090107                                   pedftchgac:
039600090107                                   SPACTCODE3:
039700090107                                     debitCODE:
039800090107          rb_struc(acnt).rb_transsum.rb_amount:
039900090107                                      julDate:
040000090107                                      julDate:
040100090107                                       zeros);
040200090107
040300090107          // only write these records if the
040400090107          // current transit isn't the main transit
040500090107          if (rbarr(acnt)) <> pedfttrn#;
040600090107             //-------------------------------
040700090107             // write Debit transit clearing record
040800090107             //-------------------------------
040900090107          seqcnt += 1;
041000090107
041100090107               numDebrec += 1;
041200090107               numDebamt  +=  rb_struc(acnt).rb_transsum.rb_amount;
041300090107            exec sql
041400090107             select rbrgl into :clearingacc from rbgrange
041500090107               where rbrtrn# = :pedfttrn#;
041600090107
041700090107               write_batch_detail(rbarr(acnt):
041800090107                                  spaceholder:
041900090107                                       seqcnt:
042000090107                                  clearingacc:
042100090107                                   SPACTCODE3:
042200090107                                    debitCODE:
042300090107         rb_struc(acnt).rb_transsum.rb_amount:
042400090107                                      julDate:
042500090107                                      julDate:
042600090107                                       zeros);
042700090107
042800090107             //-------------------------------
042900090107             // write Credit transit clearing record
043000090107             //-------------------------------
043100090107          seqcnt += 1;
043200090107
043300090107               numCrdrec += 1;
043400090107               numCrdamt  +=  rb_struc(acnt).rb_transsum.rb_amount;
043500090107
043600090107               write_batch_detail(pedfttrn#:
043700090107                                  spaceholder:
043800090107                                       seqcnt:
043900090107       //                             9012261:
044000090107          rb_struc(acnt).rb_transsum.rb_cracc:
044100090107                                   SPACTCODE2:
044200090107                                   CREDITCODE:
044300090107         rb_struc(acnt).rb_transsum.rb_amount:
044400090107                                      julDate:
044500090107                                      julDate:
044600090107                                       zeros);
044700090107             endif;
044800090107
044900090105          endfor;
045000090107
045100090107          // write Debit Figure - TOTAL BATCH
045200090107          //---------------------------------
045300090107          seqcnt += 1;
045400090107               numDebrec += 1;
045500090107               numDebamt  +=  batchtotal;
045600090107
045700090107               write_batch_detail(pedfttrn#:
045800090107                                  spaceholder:
045900090107                                       seqcnt:
046000090107                                        pacct:
046100090107                                    SPACTCODE1:
046200090107                                     debitCODE:
046300090107                                    batchtotal:
046400090107                                      julDate:
046500090107                                      julDate:
046600090107                                       zeros);
046700090105
046800090107          // write Debit Figure - TOTAL CHARGES
046900090107          //-----------------------------------
047000090107
047100090107          seqcnt += 1;
047200090107               numDebrec += 1;
047300090107               numDebamt  +=  batchcharges;
047400090107
047500090107               write_batch_detail(pedfttrn#:
047600090107                                  spaceholder:
047700090107                                       seqcnt:
047800090107                                        pacct:
047900090107                                    SPACTCODE1:
048000090107                                     debitCODE:
048100090107                                  batchcharges:
048200090107                                       julDate:
048300090107                                       julDate:
048400090107                                        zeros);
048500090107
048600090107
048700090107          // write credit Figure - TOTAL CHARGES
048800090107          //-----------------------------------
048900090107          seqcnt += 1;
049000090107               numCrdrec += 1;
049100090107               numCrdamt  +=  batchcharges;
049200090107               write_batch_detail(pedfttrn#:
049300090107                                  spaceholder:
049400090107                                       seqcnt:
049500090107                                   pedftchgac:
049600090107                                    SPACTCODE2:
049700090107                                    CREDITCODE:
049800090107                                  batchcharges:
049900090107                                       julDate:
050000090107                                       julDate:
050100090107                                        zeros);
050200090107
050300090107
050400090105         //  write footer record;
050500090106               //write footer info
050600090106
050700090106             rb_foot_space = *blanks;
050800090107             rb_foot_crtot = numcrdamt;
050900090107             rb_foot_dbtot = numdebamt;
051000090106
051100090107             rb_foot_crcnt = numcrdRec;
051200090107             rb_foot_dbcnt = numdebRec;
051300090106             write_to_file('2':%trim(rb_text_footer));
051400090106
051500090106             //close file
051600090106             write_to_file('3');
051700090105
051800090105      /end-free
051900090106     P prepare_batch   E
052000090106
052100090106     P*-----------------------------------------
052200090106     P* Procedure name: write_to_file
052300090106     P* Purpose:        write to file
052400090106     P* Returns:        0
052500090106     P* Parameter:      0
052600090106     P*-----------------------------------------
052700090106     P write_to_file   B
052800090106     D write_to_file   PI
052900090106     D option                         1    const
053000090106     D data                         100    const varying options(*nopass)
053100090106
053200090106     D/copy IFSPR
053300090106     D fd              S             10I 0
053400090106     D wrdata          S            100A
053500090106     D rddata          S            100A
053600090106     D flags           S             10U 0
053700090106     D mode            S             10U 0
053800090106     D Msg             S             50A
053900090106     D Len             S             10I 0
054000090106      /free
054100090106
054200090106       //*************************************************************
054300090106       // writing data to a stream file
054400090106       //****************************************************************
054500090106         select;
054600090106           when option = '1';
054700090107
054800090107             fd = unlink(pefilenm);
054900090107
055000090107             flags = O_WRONLY + O_CREAT + O_TRUNC +
055100090106                     O_TEXTDATA + O_CODEPAGE;
055200090106
055300090106             mode =  S_IRUSR + S_IWUSR
055400090106                         + S_IRGRP + S_IROTH;
055500090106
055600090107             // Open IFS file in ASCII format
055700090107             fd = open(pefilenm: flags: mode: 819);
055800090107
055900090106             if  fd < 0;
056000090107             // Msg = 'open(): failed for writing';
056100090107             // return;
056200090106             endif;
056300090106
056400090107             // Close IFS file
056500090106             callp  close(fd);
056600090107
056700090107             // Reopen FS file
056800090107             fd = open(pefilenm:O_WRONLY + O_TEXTDATA);
056900090106
057000090106           // Write some data
057100090106           when option = '2';
057200090106             wrdata = data + CRLF;
057300090106             callp write(fd: %addr(wrdata): %len(%trimr(wrdata)));
057400090106
057500090106           // close the file
057600090106           when option = '3';
057700090106             callp  close(fd);
057800090106         endsl;
057900090106
058000090106      /end-free
058100090106     P write_to_file   E
058200090107
058300090107     P*-----------------------------------------
058400090107     P* Procedure name: write_batch_detail
058500090107     P* Purpose:        write batch detail lines
058600090107     P* Returns:        0
058700090107     P* Parameter:      0
058800090107     P*-----------------------------------------
058900090107     P write_batch_detail...
059000090107     P                 B
059100090107     D write_batch_detail...
059200090107     D                 PI
059300090107     D transit#                       8  0 const
059400090107     D space                          2    const
059500090107     D seq#                           6  0 const
059600090107     D account                        7  0 const
059700090107     D specialacc                     3  0 const
059800090107     D gltype                         1    const
059900090107     D total                         15  2 const
060000090107     D date1                          7  0 const
060100090107     D date2                          7  0 const
060200090107     D stuff                          9  0 const
060300090107      /free
060400090107               //write detail lines
060500090107               rb_det_transit = transit#;
060600090107               rb_det_space = space;
060700090107               rb_det_seq# = seq#;
060800090107               rb_det_account= account;
060900090107
061000090107               rb_det_spcacc = Specialacc;
061100090107               rb_det_acctype = gltype;
061200090107
061300090107               rb_det_total = total;
061400090107
061500090107               rb_det_date1 = date1;
061600090107               rb_det_date2 = date2;
061700090107               rb_det_stuff = stuff;
061800090107
061900090107               write_to_file('2':rb_text_detail);
062000090107      /end-free
062100090107     P write_batch_detail...
062200090107     P                 E
062300090107
