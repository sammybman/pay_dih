000100110131     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000200090106     H******  PROGRAMMER - S. BRITTON , c.williams
000300090106     H******  First Coded - 09/01/06
000400110121     Fgenpysuml4uf   e           k disk    commit  usropn
000700991208     **
000800000707     Daccount_fld      ds
000900000707     D payacc
001000090623     D  payarr                        1    overlay(payacc) dim(30)
001100000707     **
001200090106     DCiti_acc_fld     ds
001300090106     D bankacc                        7  0
001400090106     D bankarr                        1    overlay(bankacc) dim(7)
001500000707     **
001600090105     D rbgrangeDS    e ds                  extname(rbgrange) inz
001700090106     D payelcDS      e ds                  extname(payelctrn) inz
001800091013     D                                     qualified prefix('P':1)
001900091013
002000091013     D shrelcDS      e ds                  extname(shrelctrn) inz
002100091013     D                                     qualified prefix('P':1)
002200090105
002300100804     D cuselcDS      e ds                  extname(cuselctrn) inz
002400100804     D                                     qualified prefix('P':1)
002500000711     **
002600090105     Dciti_err         ds                  template
002700000711     D e_payemp                       6  0
002800000711     D e_payacc                      12
002900000711     D e_paycrd                       6  0
003000000711     D e_payname                     26
003100090105
003200090106     Drb_struc         ds                  qualified  dim(30) inz
003300090105     D rb_numrec                      4  0
003400091013     D rb_transdet                         likeDS(rb_data) dim(1500)
003500090105     D rb_transsum                         likeDS(rb_summ)
003600100215
003700090105     D rb_summ         ds                  template
003800090105     D rb_amount                     15  2
003900090105     D rb_dbacc                       7  0
004000090105     D rb_cracc                       7  0
004100090105
004200090105     Drb_data          ds                  template
004300090105     D rb_account                     7  0
004400090105     D rb_total                      15  2
004500090105     D rb_charges                    15  2
004600090105
004700000707     **
004800090105
004900090105     Drb_text_header   ds
005000090106     D rb_head_zeros                 16  0 inz(*zeros)
005100090106     D rb_head_date                   7  0
005200090106     D rb_head_stuff                  9  0 inz(009154503)
005300090105
005400090105     Drb_text_detail   ds
005500090105     D rb_det_transit                 8  0
005600090105     D rb_det_space                   2
005700090105     D rb_det_seq#                    6  0
005800090105     D rb_det_paytext                 4    inz('PAYR')
005900090105     D rb_det_paycurr                 3    inz('GYD')
006000090105     D rb_det_account                 7  0
006100090105     D rb_det_spcacc                  3  0
006200090105     D rb_det_acctype                 1
006300090105     D rb_det_total                  15  2
006400090106     D rb_det_date1                   7  0
006500090106     D rb_det_date2                   7  0
006600090107     D rb_det_stuff                   9  0
006700090105
006800090105     Drb_text_footer   ds
006900090105     D rb_foot_trans                  8  0 inz(99999999)
007000090105     D rb_foot_space                  8
007100090105     D rb_foot_crtot                 15  2
007200090105     D rb_foot_dbtot                 15  2
007300090106     D rb_foot_crcnt                  7  0
007400090106     D rb_foot_dbcnt                  7  0
007500091019
007600091019      // General PROTOTYPES
007700091019      /copy GENPR
007800090105
007900090106     D DIGITS          c                   '0123456789'
008000090106     D CRLF            c                   x'0D25'
008100090106
008200090623     D xcnt            s              2  0 inz(30)
008300090106     D ycnt            s              2  0 inz(7)
008400090105     D acnt            s              4  0
008500090105     D gcnt            s              4  0
008600090107     D crddate         s               d   datfmt(*jul)
008700090107     D julDate         s              7  0
008800090106     D seqcnt          s              6  0
008900090107     D pacct           s              7  0
009000090107       // Identify BANK/FI
009100090106     D rbtype          s              2  0 inz(2)
009200090106     D rbarr           s              8  0 dim(30)
009300090107     D spaceholder     s              2    inz(*blanks)
009400090107     D zeros           s              9  0 inz(*zeros)
009500090107     D year            s              4  0
009501110110     dtoday            s              8  0
009502110110     d@date            s             10
009600000209     **
009700090105     D*--------------------------------------
009800090105     D* Prototype for procedure: get_bank_acc
009900090105     D*--------------------------------------
010000090105     D get_bank_acc    PR
010100090105
010200090105     D*-----------------------------------------
010300090105     D* Prototype for procedure: get_transit_no
010400090105     D*-----------------------------------------
010500090105     D get_transit_no  PR
010600090105
010700090105     D*-----------------------------------------
010800090105     D* Prototype for procedure: write_to_struc
010900090105     D*-----------------------------------------
011000090105     D write_to_struc  PR
011100090105
011200090105     D*-----------------------------------------
011300090106     D* Prototype for procedure: prepare_batch
011400090105     D*-----------------------------------------
011500090106     D prepare_batch   PR
011600090105
011700090106     D*-----------------------------------------
011800090106     D* Prototype for procedure: write_to_file
011900090106     D*-----------------------------------------
012000090106     D write_to_file   PR
012100090106     D option                         1    const
012200090106        // '1' - create/open file
012300090106        // '2' - write data
012400090106        // '3' - close file
012500090106     D data                         100    const varying options(*nopass)
012600090107
012700090107     D write_batch_detail...
012800090107     D                 PR
012900090107     D transit#                       8  0 const
013000090107     D space                          2    const
013100090107     D seq#                           6  0 const
013200090107     D account                        7  0 const
013300090107     D specialacc                     3  0 const
013400090107     D gltype                         1    const
013500090107     D total                         15  2 const
013600090107     D date1                          7  0 const
013700090107     D date2                          7  0 const
013800090107     D stuff                          9  0 const
013900090106
014000090106     D*-----------------------------------------
014100090106     D* Prototype for procedure: Main_pgm
014200090106     D*-----------------------------------------
014300090105     D  main_pgm       pr                  extpgm('RB0001R')
014400090105     D   citi_errDS                        likeds(citi_err) dim(100)
014500090105     D   err_count                    6  0
014600090105     D   eof                           n
014700090105     D   count                        6  0
014800090105     D   bnk_type                     2  0
014900091010     D   origin                       3    options(*nopass)
015000090106
015100090106     D  main_pgm       pi
015200090106     D   citi_errDS                        likeds(citi_err) dim(100)
015300090106     D   err_count                    6  0
015400090106     D   eof                           n
015500090106     D   count                        6  0
015600090106     D   bnk_type                     2  0
015700091010     D   origin                       3    options(*nopass)
015800091019     **
015900091019     D*------------------------------------------
016000091019     D* Prototype for procedure: get_credit_date
016100091019     D*------------------------------------------
016200091019     D get_credit_date...
016300091019     D                 PR             6  0
016400091019     D dateCrd                        6  0 const
016500091019     D dateAcc                        6  0 const
016600090105
016700090105      /free
016800090105              // Get Division and Bank account information from file
016900090106              if  count = 0;
017200110121                    open genpysuml4;
017300110121                    setll (origin:bnk_type) payfmt;
018100090106              endif;
018200090105
018500110121                    reade (origin:bnk_type) payfmt;
018600091019                    paycrd = get_credit_date(paycrd:paydate);
019600091019
019700090106
019800090105              if  count = 0;
019900110110                // set credit date for batch
019901110110                    if origin = 'CRD';
019902110110
019904110110                        //if catering for Customer Discounts, add three days
019905110110                       // to current day.
019906110110
019907110110                          @DATE = get_sys_date(today);
019908110110                           crddate = %date(today:*ISO);
019909110110                           crddate = crddate + %days(3);
020000110110
020001110110                         else;
020100110110                            crddate = %date(paycrd:*dmy);
020101110110
020103110110
020204110110                    endif;
020205110110
020300090107                year = %subdt(crddate:*years);
020400090106
020500090107                // save converted date in Julian date format
020600090107                //julDate = %dec(crddate:*JUL);
020700090107                julDate =
020800090107                         %int(%subst(%char(year):1:2) +
020900090107                              %editc(%dec(crddate:*JUL):'X'));
021000090107
021100091010               select;
021200091013                 when origin  = 'PAY';
021300091013                   exec sql
021400091013                     select * into :payelcDS from payelctrn
021500091013                       where petype = :rbtype;
021600090106
021700091010
021800091013                  // if called from shares system
021900091013                 when origin  = 'SHR';
022000091013                   exec sql
022100091013                     select * into :ShrelcDS from shrelctrn
022200091013                      where setype = :rbtype;
022300091013
022400091013                   eval-corr payelcDS = shrelcDS;
022500100804                     when origin  = 'CRD';
022600100804
022700100804
022800100804                     exec sql
022900100804                     select * into :cuselcDS from cuselctrn
023000100804                      where cetype = :rbtype;
023100100804
023200100804                   eval-corr payelcDS = cuselcDS;
023300091013               endsl;
023400091013
023500091013               // banks acct field to rb acct field
023600091013               pacct = %int(%subst(payelcDS.peacct:1:7));
023700091013
023800091013                // init batch sequence counter
023900091013                seqcnt = payelcDS.pelower -1;
024000091010              endif;
024100090105              if   not(%eof());
024200090105                // get accounting info.
024300090105                get_bank_acc();
024400090106                get_transit_no();
024500090105                write_to_struc();
024800091010                paysts = 'V';
024900091010                update  payfmt %fields(paysts);
025000100908
026100090105
026200090105              endif;
026300090105
026400090105              if  %eof();
026500090105                 eof = *on;
026600090105                 *inlr = *on;
026900110121                   close genpysuml4;
027600091013
027700090106                 prepare_batch();
027800090105              endif;
027900090105
028000090105             // RETURN
028100101018             commit;
028200090105             return;
028300090105      /end-free
028400090105
028500090105     P*-----------------------------------------
028600090105     P* Procedure name: get_bank_acc
028700090105     P* Purpose:        get actual bank account
028800090105     P* Returns:        0
028900090105     P* Parameter:      0
029000090105     P*-----------------------------------------
029100090105     P get_bank_acc    B
029200090106     D get_bank_acc    PI
029300090105
029400090105      /free
029500090105          bankacc =  *zeros;
030400090105
030500090105          // check for number in character field
030600090105          dow  xcnt > 0;
030700090105
030800090106              if  %check(digits:payarr(xcnt)) = 0
030900090105                      and ycnt > 0;
031000090105                bankarr(ycnt) = payarr(xcnt);
031100090105                ycnt -=  1;
031200090105              endif;
031300090105
031400090105              xcnt -=  1;
031500090105          enddo;
031600090105
031700090105          // Reset array fields
031800090105          reset  xcnt;
031900090105          reset  ycnt;
032000090105
032100090105      /end-free
032200090105     P get_bank_acc    E
032300090105
032400090105     P*-----------------------------------------
032500090105     P* Procedure name: get_bank_acc
032600090105     P* Purpose:        get actual bank account
032700090105     P* Returns:        0
032800090105     P* Parameter:      0
032900090105     P*-----------------------------------------
033000090105     P get_transit_no  B
033100090105     D get_transit_no  PI
033200090105
033300090105      /free
033400090106         clear rbgrangeDS;
033500090105         exec sql
033600090105           select * into :rbgrangeDS from rbgrange
033700090107              where rbrfrange <= :bankacc and
033800090107                  rbrtrange >= :bankacc;
033900090107
034000090107         if rbrtrans# = *zeros;
034100091013
034200091013            rbrtrans# = payelcDS.pedfttrn#;
034300090107            exec sql
034400090107             select * into :rbgrangeDS from rbgrange
034500090107               where rbrfrange <= :bankacc and
034600090107                   rbrtrange >= :bankacc;
034700090107         endif;
034800090105      /end-free
034900090105     P get_transit_no  E
035000090105
035100090105
035200090105     P*-----------------------------------------
035300090105     P* Procedure name: write_to_struc
035400090105     P* Purpose:        write to structure
035500090105     P* Returns:        0
035600090105     P* Parameter:      0
035700090105     P*-----------------------------------------
035800090105     P write_to_struc  B
035900090105     D write_to_struc  PI
036000090105
036100090105      /free
036200090105
036300090105             acnt = 0;
036400090106             acnt = %lookup(rbrtrans#:rbarr);
036500090105             if acnt = 0;
036600090105                gcnt += 1;
036700090106                rbarr(gcnt) = rbrtrans#;
036800090105
036900090105                acnt = gcnt;
037000090105
037100090105             endif;
037200090105
037300090105             // accumulate totals
037400090105             rb_struc(acnt).rb_numrec += 1;
037500090105             // write detail figures
037600090105             rb_struc(acnt).rb_transdet(rb_struc(acnt).rb_numrec)
037700090105                                          .rb_Account = bankacc;
037800090105             rb_struc(acnt).rb_transdet(rb_struc(acnt).rb_numrec)
037900090105                                          .rb_total = payamt;
038000090105             rb_struc(acnt).rb_transdet(rb_struc(acnt).rb_numrec)
038100091013                                          .rb_charges = payelcDS.pechgamt;
038200090105             // write summary figures
038300090105             rb_struc(acnt).rb_transsum.rb_amount += payamt;
038400090105             rb_struc(acnt).rb_transsum.rb_dbacc = rbrgl;
038500090105             rb_struc(acnt).rb_transsum.rb_cracc = rbrgl;
038600090105
038700090105      /end-free
038800090105     P write_to_struc  E
038900090105
039000090105
039100090105     P*-----------------------------------------
039200090106     P* Procedure name: prepare_batch
039300090106     P* Purpose:        prepare batch
039400090105     P* Returns:        0
039500090105     P* Parameter:      0
039600090105     P*-----------------------------------------
039700090106     P prepare_batch   B
039800090106     D prepare_batch   PI
039900090105
040000090105     D acnt            s              4  0
040100090105     D bcnt            s              4  0
040200090107     D numDebrec       s              7  0
040300090107     D numCrdrec       s              7  0
040400090107     D numDebamt       s             15  2
040500090107     D numCrdamt       s             15  2
040600090107     D batchtotal      s             15  2
040700090107     D batchcharges    s             15  2
040800090107     D clearingacc     s              7  0
040900090106
041000090107     D SPACTCODE       c                   300
041100090107     D SPACTCODE1      c                   301
041200090107     D SPACTCODE2      c                   051
041300090107     D SPACTCODE3      c                   075
041400090106     D CREDITCODE      c                   'C'
041500090107     D DEBITCODE       c                   'D'
041600090105      /free
041700090105
041800090105         //  write header record;
041900090106         //  ...create/open file;
042000090107         //----------------------
042100090106         write_to_file('1');
042200090107         rb_head_date = julDate;
042300090106         write_to_file('2':rb_text_header);
042400090105
042500090105         //  write detail records;
042600090107         //------------------------
042700090105          for acnt = 1 to gcnt;
042800090107
042900090105             // accumulate totals
043000090105             for bcnt = 1 to rb_struc(acnt).rb_numrec;
043100090107                // write detail figures
043200090107                rb_struc(acnt).rb_transdet(bcnt)
043300091013                                          .rb_charges = payelcDS.pechgamt;
043400090107                //write detail lines
043500090107                seqcnt += 1;
043600090107                numCrdrec += 1;
043700090107                batchtotal += rb_struc(acnt).rb_transdet(bcnt)
043800090107                                            .rb_total;
043900090107                batchcharges += rb_struc(acnt).rb_transdet(bcnt)
044000090107                                            .rb_charges;
044100090107
044200090107                numCrdamt  += rb_struc(acnt).rb_transdet(bcnt)
044300090107                                            .rb_total;
044400090106
044500090107                write_batch_detail(rbarr(acnt):
044600090107                                   spaceholder:
044700090107                                        seqcnt:
044800090107                rb_struc(acnt).rb_transdet(bcnt).rb_Account:
044900090107                                    SPACTCODE:
045000090107                                   CREDITCODE:
045100090107                rb_struc(acnt).rb_transdet(bcnt).rb_total:
045200090107                                      julDate:
045300090107                                      julDate:
045400090107                                       zeros);
045500090107
045600090105             endfor;
045700090107
045800090107
045900090105             // write summary figures
046000090107             //-------------------------------
046100090107             // write credit summary record
046200090107             //-------------------------------
046300090107               seqcnt += 1;
046400090107
046500090107               numCrdrec += 1;
046600090107               numCrdamt  +=  rb_struc(acnt).rb_transsum.rb_amount;
046700090107
046800110121                 write_batch_detail(rbarr(acnt):
046900110121                                    spaceholder:
047000110121                                         seqcnt:
047100110121                             payelcDS.pedftclac:
047200110121                                     SPACTCODE2:
047300110121                                     CREDITCODE:
047400110121           rb_struc(acnt).rb_transsum.rb_amount:
047500110121                                        julDate:
047600110121                                        julDate:
047700110121                                         zeros);
047800090107
047900090107             //-------------------------------
048000090107             // write Debit summary record
048100090107             //-------------------------------
048200090107               seqcnt += 1;
048300090107
048400090107               numDebrec += 1;
048500090107               numDebamt  +=  rb_struc(acnt).rb_transsum.rb_amount;
048600090107
048700090107               write_batch_detail(rbarr(acnt):
048800090107                                  spaceholder:
048900090107                                       seqcnt:
049000091013                           payelcDS.pedftclac:
049100090107                                   SPACTCODE3:
049200091013                                    debitCODE:
049300091013         rb_struc(acnt).rb_transsum.rb_amount:
049400090107                                      julDate:
049500090107                                      julDate:
049600090107                                       zeros);
049700090107
049800090107          // only write these records if the
049900090107          // current transit isn't the main transit
050000091013          if (rbarr(acnt)) <> payelcDS.pedfttrn#;
050100090107
050200090107             //-------------------------------
050300090107             // write Debit transit clearing record
050400090107             //-------------------------------
050500090107             seqcnt += 1;
050600090107
050700090107               numDebrec += 1;
050800090107               numDebamt  +=  rb_struc(acnt).rb_transsum.rb_amount;
050900090107
051000090107               exec sql
051100090107                select rbrgl into :clearingacc from rbgrange
051200091013                  where rbrtrans# = :payelcDS.pedfttrn#;
051300090107
051400090107               write_batch_detail(rbarr(acnt):
051500090107                                            spaceholder:
051600090107                                                 seqcnt:
051700090107                                            clearingacc:
051800090107                                             SPACTCODE3:
051900090107                                              debitCODE:
052000090107                   rb_struc(acnt).rb_transsum.rb_amount:
052100090107                                                julDate:
052200090107                                                julDate:
052300090107                                                 zeros);
052400090107
052500090107             //-------------------------------
052600090107             // write Credit transit clearing record
052700090107             //-------------------------------
052800090107             seqcnt += 1;
052900090107
053000090107             numCrdrec += 1;
053100090107             numCrdamt  +=  rb_struc(acnt).rb_transsum.rb_amount;
053200090107
053300091013             write_batch_detail(payelcDS.pedfttrn#:
053400090107                                        spaceholder:
053500090107                                             seqcnt:
053600090107                rb_struc(acnt).rb_transsum.rb_cracc:
053700090107                                         SPACTCODE2:
053800090107                                         CREDITCODE:
053900090107               rb_struc(acnt).rb_transsum.rb_amount:
054000090107                                            julDate:
054100090107                                            julDate:
054200090107                                             zeros);
054300090107          endif;
054400090107
054500090105          endfor;
054600090107
054700090107          // write Debit Figure - TOTAL BATCH
054800090107          //---------------------------------
054900090107          seqcnt += 1;
055000090107          numDebrec += 1;
055100090107          numDebamt  +=  batchtotal;
055200090107
055300091013          write_batch_detail(payelcDS.pedfttrn#:
055400090107                                  spaceholder:
055500090107                                       seqcnt:
055600090107                                        pacct:
055700090107                                    SPACTCODE1:
055800090107                                     debitCODE:
055900090107                                    batchtotal:
056000090107                                      julDate:
056100090107                                      julDate:
056200090107                                       zeros);
056300090105
056400090107          // write Debit Figure - TOTAL CHARGES
056500090107          //-----------------------------------
056600090107
056700090107          seqcnt += 1;
056800090107          numDebrec += 1;
056900090107          numDebamt  +=  batchcharges;
057000090107
057100091013          write_batch_detail(payelcDS.pedfttrn#:
057200090107                                  spaceholder:
057300090107                                       seqcnt:
057400090107                                        pacct:
057500090107                                    SPACTCODE1:
057600090107                                     debitCODE:
057700090107                                  batchcharges:
057800090107                                       julDate:
057900090107                                       julDate:
058000090107                                        zeros);
058100090107
058200090107
058300090107          // write credit Figure - TOTAL CHARGES
058400090107          //-----------------------------------
058500090107          seqcnt += 1;
058600090107          numCrdrec += 1;
058700090107          numCrdamt  +=  batchcharges;
058800090107
058900091013          write_batch_detail(payelcDS.pedfttrn#:
059000091013                                   spaceholder:
059100091013                                        seqcnt:
059200091013                           payelcDS.pedftchgac:
059300090107                                    SPACTCODE2:
059400090107                                    CREDITCODE:
059500090107                                  batchcharges:
059600090107                                       julDate:
059700090107                                       julDate:
059800090107                                        zeros);
059900090107
060000090107
060100090107          // write footer record;
060200090106
060300090106             rb_foot_space = *blanks;
060400090107             rb_foot_crtot = numcrdamt;
060500090107             rb_foot_dbtot = numdebamt;
060600090106
060700090107             rb_foot_crcnt = numcrdRec;
060800090107             rb_foot_dbcnt = numdebRec;
060900090106             write_to_file('2':%trim(rb_text_footer));
061000090106
061100090106             //close file
061200090106             write_to_file('3');
061300090105
061400090105      /end-free
061500090106     P prepare_batch   E
061600090106
061700090106     P*-----------------------------------------
061800090106     P* Procedure name: write_to_file
061900090106     P* Purpose:        write to file
062000090106     P* Returns:        0
062100090106     P* Parameter:      0
062200090106     P*-----------------------------------------
062300090106     P write_to_file   B
062400090106     D write_to_file   PI
062500090106     D option                         1    const
062600090106     D data                         100    const varying options(*nopass)
062700090106
062800090106     D/copy IFSPR
062900090106     D fd              S             10I 0
063000090106     D wrdata          S            100A
063100090106     D rddata          S            100A
063200090106     D flags           S             10U 0
063300090106     D mode            S             10U 0
063400090106     D Msg             S             50A
063500090106     D Len             S             10I 0
063600090106      /free
063700090106
063800090106       //*************************************************************
063900090106       // writing data to a stream file
064000090106       //****************************************************************
064100090106         select;
064200090106           when option = '1';
064300090107
064400091013               fd = unlink(payelcDS.pefilenm);
064500090107
064600090107             flags = O_WRONLY + O_CREAT + O_TRUNC +
064700090106                     O_TEXTDATA + O_CODEPAGE;
064800090106
064900090106             mode =  S_IRUSR + S_IWUSR
065000090106                         + S_IRGRP + S_IROTH;
065100090106
065200090107             // Open IFS file in ASCII format
065300091010
065400091013             fd = open(payelcDS.pefilenm: flags: mode: 819);
065500091013
065600090106             if  fd < 0;
065700090107             // Msg = 'open(): failed for writing';
065800090107             // return;
065900090106             endif;
066000090106
066100090107             // Close IFS file
066200090106             callp  close(fd);
066300090107
066400090107             // Reopen FS file
066500091013              fd = open(payelcDS.pefilenm:O_WRONLY + O_TEXTDATA);
066600090106
066700090106           // Write some data
066800090106           when option = '2';
066900090106             wrdata = data + CRLF;
067000090106             callp write(fd: %addr(wrdata): %len(%trimr(wrdata)));
067100090106
067200090106           // close the file
067300090106           when option = '3';
067400090106             callp  close(fd);
067500090106         endsl;
067600090106
067700090106      /end-free
067800090106     P write_to_file   E
067900090107
068000090107     P*-----------------------------------------
068100090107     P* Procedure name: write_batch_detail
068200090107     P* Purpose:        write batch detail lines
068300090107     P* Returns:        0
068400090107     P* Parameter:      0
068500090107     P*-----------------------------------------
068600090107     P write_batch_detail...
068700090107     P                 B
068800090107     D write_batch_detail...
068900090107     D                 PI
069000090107     D transit#                       8  0 const
069100090107     D space                          2    const
069200090107     D seq#                           6  0 const
069300090107     D account                        7  0 const
069400090107     D specialacc                     3  0 const
069500090107     D gltype                         1    const
069600090107     D total                         15  2 const
069700090107     D date1                          7  0 const
069800090107     D date2                          7  0 const
069900090107     D stuff                          9  0 const
070000090107      /free
070100090107               //write detail lines
070200090107               rb_det_transit = transit#;
070300090107               rb_det_space = space;
070400090107               rb_det_seq# = seq#;
070500090107               rb_det_account= account;
070600090107
070700090107               rb_det_spcacc = Specialacc;
070800090107               rb_det_acctype = gltype;
070900090107
071000090107               rb_det_total = total;
071100090107
071200090107               rb_det_date1 = date1;
071300090107               rb_det_date2 = date2;
071400090107               rb_det_stuff = stuff;
071500090107
071600090107               write_to_file('2':rb_text_detail);
071700090107      /end-free
071800090107     P write_batch_detail...
071900090107     P                 E
072000091019
072100091019     P*-----------------------------------------
072200091019     P* Procedure name: get_credit_date
072300091019     P* Purpose:        get_credit_date
072400091019     P* Returns:        0
072500091019     P* Parameter:      0
072600091019     P*-----------------------------------------
072700091019     P get_credit_date...
072800091019     P                 B
072900091019     D get_credit_date...
073000091019     D                 PI             6  0
073100091019     D dateIn                         6  0 const
073200091019     D dateAcc                        6  0 const
073300091019
073400091019     D olddateISO      s              8  0
073500091019     D newdate         s              6  0
073600091019      /free
073700091019
073800091019             // DO NOT allow Credit date falling on a weekend
073900091019             //.... day 6 or 7
074000091019             olddateISO = %dec(%date(dateIn:*dmy):*iso);
074100091019             if dayofweek(olddateISO) = 6
074200091019               or dayofweek(olddateISO) = 7;
074300091019             //... etc.
074400091019             //newdate = %dec(%date(olddateISO:*dmy)+ %days(1));
074500091019               newdate = dateAcc;
074600091019              else;
074700091019               newdate = dateIn;
074800091019             endif;
074900091019
075000091019          return newdate;
075100091019
075200091019      /end-free
075300091019     P get_credit_date...
075400091019     P                 E
075500090107
