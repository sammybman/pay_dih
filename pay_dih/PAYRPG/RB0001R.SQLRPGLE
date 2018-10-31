000100161003     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000200090106     H******  PROGRAMMER - S. BRITTON , c.williams
000300090106     H******  First Coded - 09/01/06
000400110121     Fgenpysuml4uf   e           k disk    commit  usropn
000500991208     **
000600000707     Daccount_fld      ds
000700000707     D payacc
000800090623     D  payarr                        1    overlay(payacc) dim(30)
000900000707     **
001000090106     DCiti_acc_fld     ds
001100090106     D bankacc                        7  0
001200090106     D bankarr                        1    overlay(bankacc) dim(7)
001300000707     **
001400090105     D rbgrangeDS    e ds                  extname(rbgrange) inz
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
002900090105
003000090106     Drb_struc         ds                  qualified  dim(30) inz
003100090105     D rb_numrec                      4  0
003200091013     D rb_transdet                         likeDS(rb_data) dim(1500)
003300090105     D rb_transsum                         likeDS(rb_summ)
003400100215
003500090105     D rb_summ         ds                  template
003600090105     D rb_amount                     15  2
003700090105     D rb_dbacc                       7  0
003800090105     D rb_cracc                       7  0
003900090105
004000090105     Drb_data          ds                  template
004100090105     D rb_account                     7  0
004200090105     D rb_total                      15  2
004300090105     D rb_charges                    15  2
004301161010     D rb_emp                         6  0
004400090105
004500000707     **
004600090105
004700161003     D*rb_text_header   ds
004800161003     D* rb_head_zeros                 16  0 inz(*zeros)
004900161003     D* rb_head_date                   7  0
005000161003     D* rb_head_stuff                  9  0 inz(009154503)
005100161003     D*rb_text_header   ds
005200161003     D* rb_head_zeros                 16  0 inz(*zeros)
005300161003     D* rb_head_date                   7  0
005400161003     D* rb_head_stuff                  9  0 inz(009154503)
005401161003     D
005402161003     D
005403161003     Drb_text_header   ds
005404161003     D rb_headtype                    1    inz('H')
005406161003     D rb_head_date                   8
005407161003     D rb_head_time                   4    inz(' ')
005408161003     D rb_head_noRec                  5  0 inz(0)
005409161003     D rb_head_runNo                  2  0 inz(0)
005410161003     D rb_head_proNo                  4  0 inz(0)
005411161003     D rb_headfiller1               186    inz(*blank)
005412161003     D rb_headfiller2                 2    inz('BB')
005500090105
005600161003     D*rb_text_detail   ds
005700161003     D* rb_det_transit                 8  0
005800161003     D* rb_det_space                   2
005900161003     D* rb_det_seq#                    6  0
006000161003     D* rb_det_paytext                 4    inz('PAYR')
006100161003     D* rb_det_paycurr                 3    inz('GYD')
006200161003     D* rb_det_account                 7  0
006300161003     D* rb_det_spcacc                  3  0
006400161003     D* rb_det_acctype                 1
006500161003     D* rb_det_total                  15  2
006600161003     D* rb_det_date1                   7  0
006700161003     D* rb_det_date2                   7  0
006800161003     D* rb_det_stuff                   9  0
006801161003     D
006802161003     D
006803161003     Drb_text_detail   ds
006804161003     D rb_det_type                    1    inz('D')
006805161003     D rb_det_recNo                   7  0 inz(0)
006806161003     D rb_det_docType                 2
006807161003     D rb_det_field4                  3  0 inz(300)
006808161013     D rb_det_total                  16  2
006809161003     D rb_det_filler1                 4    inz('BBBB')
006810161003     D rb_det_filler2                 3    inz('BBB')
006811161003     D rb_det_filler3                 2    inz('BB')
006812161003     D rb_det_filler4                 4    inz('BBBB')
006813161003     D rb_det_filler5                16  0 inz(*zero)
006814161003     D rb_det_filler6                16  0 inz(*zero)
006815161003     D rb_det_cheq_no                16  0 inz(*zero)
006816161010     D rb_det_transit                13    inz(*blanks)
006817161003     D rb_det_bank                    3  0 inz(001)
006818161003     D rb_det_account                16
006819161004     D rb_det_trnscod                16
006820161003     D rb_det_text                   36    inz('    ')
006821161003     D rb_det_text2                  36    inz('    ')
006822161003     D rb_det_filler8                 2    inz('BB')
006900090105
007000161003     D*rb_text_footer   ds
007100161003     D* rb_foot_trans                  8  0 inz(99999999)
007200161003     D* rb_foot_space                  8
007300161003     D* rb_foot_crtot                 15  2
007400161003     D* rb_foot_dbtot                 15  2
007500161003     D* rb_foot_crcnt                  7  0
007600161003     D* rb_foot_dbcnt                  7  0
007601161003     D*
007602161003     Drb_text_footer   ds
007603161003     D rb_foot_type                   1    inz('T')
007604161003     D rb_footfiller1                 7    inz('EOD9999')
007605161003     D rb_foot_DetRec                 8  0 inz(0)
007606161003     D rb_footfiller2               194    inz(*blank)
007607161003     D rb_footfiller3                 2    inz('BB')
007700091019
007800091019      // General PROTOTYPES
007900140908      /copy qcpysrc,GENPR
007901161010      /copy qcpysrc,payPR
008000090105
008100090106     D DIGITS          c                   '0123456789'
008200090106     D CRLF            c                   x'0D25'
008300090106
008400090623     D xcnt            s              2  0 inz(30)
008500090106     D ycnt            s              2  0 inz(7)
008600090105     D acnt            s              4  0
008700090105     D gcnt            s              4  0
008800090107     D crddate         s               d   datfmt(*jul)
008900090107     D julDate         s              7  0
009000090106     D seqcnt          s              6  0
009100090107     D pacct           s              7  0
009200090107       // Identify BANK/FI
009300090106     D rbtype          s              2  0 inz(2)
009400090106     D rbarr           s              8  0 dim(30)
009500090107     D spaceholder     s              2    inz(*blanks)
009600090107     D zeros           s              9  0 inz(*zeros)
009700090107     D year            s              4  0
009800110110     dtoday            s              8  0
009900110110     d@date            s             10
010000000209     **
010001161010       DCL-S emp_name CHAR(40) INZ(*BLANK);
010100090105     D*--------------------------------------
010200090105     D* Prototype for procedure: get_bank_acc
010300090105     D*--------------------------------------
010400090105     D get_bank_acc    PR
010500090105
010600090105     D*-----------------------------------------
010700090105     D* Prototype for procedure: get_transit_no
010800090105     D*-----------------------------------------
010900090105     D get_transit_no  PR
011000090105
011100090105     D*-----------------------------------------
011200090105     D* Prototype for procedure: write_to_struc
011300090105     D*-----------------------------------------
011400090105     D write_to_struc  PR
011500090105
011600090105     D*-----------------------------------------
011700090106     D* Prototype for procedure: prepare_batch
011800090105     D*-----------------------------------------
011900090106     D prepare_batch   PR
012000090105
012100090106     D*-----------------------------------------
012200090106     D* Prototype for procedure: write_to_file
012300090106     D*-----------------------------------------
012400090106     D write_to_file   PR
012500090106     D option                         1    const
012600090106        // '1' - create/open file
012700090106        // '2' - write data
012800090106        // '3' - close file
012900161010     D data                         212    const varying options(*nopass)
013000090107
013100090107     D write_batch_detail...
013200090107     D                 PR
013300090107     D transit#                       8  0 const
013400090107     D space                          2    const
013500090107     D seq#                           6  0 const
013600090107     D account                        7  0 const
013700090107     D specialacc                     3  0 const
013800090107     D gltype                         1    const
013900090107     D total                         15  2 const
014000161013     D* date1                          7  0 const
014100161013     D* date2                          7  0 const
014200161013     D* stuff                          9  0 const
014300090106
014400090106     D*-----------------------------------------
014500090106     D* Prototype for procedure: Main_pgm
014600090106     D*-----------------------------------------
014700090105     D  main_pgm       pr                  extpgm('RB0001R')
014800090105     D   citi_errDS                        likeds(citi_err) dim(100)
014900090105     D   err_count                    6  0
015000090105     D   eof                           n
015100090105     D   count                        6  0
015200090105     D   bnk_type                     2  0
015300091010     D   origin                       3    options(*nopass)
015400090106
015500090106     D  main_pgm       pi
015600090106     D   citi_errDS                        likeds(citi_err) dim(100)
015700090106     D   err_count                    6  0
015800090106     D   eof                           n
015900090106     D   count                        6  0
016000090106     D   bnk_type                     2  0
016100091010     D   origin                       3    options(*nopass)
016200091019     **
016300091019     D*------------------------------------------
016400091019     D* Prototype for procedure: get_credit_date
016500091019     D*------------------------------------------
016600091019     D get_credit_date...
016700091019     D                 PR             6  0
016800091019     D dateCrd                        6  0 const
016900091019     D dateAcc                        6  0 const
017000090105
017100090105      /free
017200110210
017300110210
017400161003       //
017402161003       //
017403161003       DCL-S file_date DATE(*DMY) ;
017404161003       //
017405161003       DCL-S now_time TIME(*HMS) INZ(*SYS);
017406161010        DCL-s RBANK packed(2) inz(4);
017407161004
017412161004       //
017413161004       DCL-S zero CHAR(1) INZ('0');
017414161004       DCL-S four_zero CHAR(4) INZ('0000');
017415161004       DCL-S nine_zero CHAR(9) INZ('000000000');
017416161004
017417161010       //
017419161010
017500161004       //       // Get Division and Bank account information from file
017600161004              if  count = 0;
017601161012                 exec sql select count(*) into :rb_head_NoRec from genpysumL4
017602161013               where paydiv = :BNK_TYPE and payorg = :origin;
017700161004                    open genpysuml4;
017800161004                    setll (origin:bnk_type) payfmt;
017900161004              endif;
018000161004
018100161004                    reade (origin:bnk_type) payfmt;
018200161004                    paycrd = get_credit_date(paycrd:paydate);
018300161004
018400161004
018500161004              if  count = 0;
018600161004                // set credit date for batch
018700161004                    if origin = 'CRD';
018800161004
018900161004                        //if catering for Customer Discounts, add three days
019000161004                       // to current day.
019100161004
019200161004                          @DATE = get_sys_date(today);
019300161004                           crddate = %date(today:*ISO);
019400161004                           crddate = crddate + %days(3);
019500161004
019600161004                         else;
019700161004                            crddate = %date(paycrd:*dmy);
019800161004
019900161004
020000161004                    endif;
020001161010                        file_date =   crddate;//%date(crddate:*dmy);
020100161004
020200161004       //         year = %subdt(crddate:*years);
020400161004       ////         // save converted date in Julian date format
020500161004       ////         //julDate = %dec(crddate:*JUL);
020600161004       ////         julDate =
020700161004       ////                  %int(%subst(%char(year):1:2) +
020800161004       ////                       %editc(%dec(crddate:*JUL):'X'));
020900161004       ////
021000161004               select;
021100161004                 when origin  = 'PAY';
021200161004                   exec sql
021300161004                     select * into :payelcDS from payelctrn
021400161004                       where petype = :rbtype;
021401161010
021600161004
021700161004                  // if called from shares system
021800161004                 when origin  = 'SHR';
021900161004                   exec sql
022000161004                     select * into :ShrelcDS from shrelctrn
022100161004                      where setype = :rbtype;
022200161004
022300161004                   eval-corr payelcDS = shrelcDS;
022400161004                     when origin  = 'CRD';
022500161004
022600161004
022700161004                     exec sql
022800161004                     select * into :cuselcDS from cuselctrn
022900161004                      where cetype = :rbtype;
023000161004
023100161004                   eval-corr payelcDS = cuselcDS;
023200161004               endsl;
023300161004
023400161004               // banks acct field to rb acct field
023500161004               pacct = %int(%subst(payelcDS.peacct:1:7));
023600161004
023700161004                // init batch sequence counter
023800161004                seqcnt = payelcDS.pelower -1;
023900161004              endif;
024000161004              if   not(%eof());
024100161004                // get accounting info.
024200161004                get_bank_acc();
024300161004                get_transit_no();
024400161004                write_to_struc();
024500161004                paysts = 'V';
024600161004                update  payfmt %fields(paysts);
024700161004
024800161004
024900161004              endif;
025000161004
025100161004              if  %eof();
025200161004                 eof = *on;
025300161004                 *inlr = *on;
025400161004                   close genpysuml4;
025500161004
025600161004                 prepare_batch();
025700161004              endif;
025800161004
025900161004             // RETURN
026000161004             commit;
026100161004             return;
026200161004      /end-free
026300161004
026400161004     P**-----------------------------------------
026500161004     P** Procedure name: get_bank_acc
026600161004     P** Purpose:        get actual bank account
026700161004     P** Returns:        0
026800161004     P** Parameter:      0
026900161004     P**-----------------------------------------
027000161004     P get_bank_acc    B
027100161004     D get_bank_acc    PI
027200161004
027300161004      /free
027400161004          bankacc =  *zeros;
027500161004
027600161004          // check for number in character field
027700161004          dow  xcnt > 0;
027800161004
027900161004              if  %check(digits:payarr(xcnt)) = 0
028000161004                      and ycnt > 0;
028100161004                bankarr(ycnt) = payarr(xcnt);
028200161004                ycnt -=  1;
028300161004              endif;
028400161004
028500161004              xcnt -=  1;
028600161004          enddo;
028700161004
028800161004          // Reset array fields
028900161004          reset  xcnt;
029000161004          reset  ycnt;
029100161004
029200161004      /end-free
029300161004     P get_bank_acc    E
029400161004
029500161004     P**-----------------------------------------
029600161004     P** Procedure name: get_bank_acc
029700161004     P** Purpose:        get actual bank account
029800161004     P** Returns:        0
029900161004     P** Parameter:      0
030000161004     P**-----------------------------------------
030100161004     P get_transit_no  B
030200161004     D get_transit_no  PI
030300161004
030400161004      /free
030500161004         clear rbgrangeDS;
030600161004         exec sql
030700161004           select * into :rbgrangeDS from rbgrange
030800161004              where rbrfrange <= :bankacc and
030900161004                  rbrtrange >= :bankacc;
031000161004
031100161004         if rbrtrans# = *zeros;
031200161004
031300161004            rbrtrans# = payelcDS.pedfttrn#;
031400161004            exec sql
031500161004             select * into :rbgrangeDS from rbgrange
031600161004               where rbrfrange <= :bankacc and
031700161004                   rbrtrange >= :bankacc;
031800161004         endif;
031900161004      /end-free
032000161004     P get_transit_no  E
032100161004
032200161004      *
032300161004     P**-----------------------------------------
032400161004     P** Procedure name: write_to_struc
032500161004     P** Purpose:        write to structure
032600161004     P** Returns:        0
032700161004     P** Parameter:      0
032800161004     P**-----------------------------------------
032900161004     P write_to_struc  B
033000161004     D write_to_struc  PI
033100161004
033200161004      /free
033300161004
033400161004             acnt = 0;
033500161004             acnt = %lookup(rbrtrans#:rbarr);
033600161004             if acnt = 0;
033700161004                gcnt += 1;
033800161004                rbarr(gcnt) = rbrtrans#;
033900161004
034000161004                acnt = gcnt;
034100161004
034200161004             endif;
034400161004             // accumulate totals
034401161010
034500161004             rb_struc(acnt).rb_numrec += 1;
034600161004             // write detail figures
034700161004             rb_struc(acnt).rb_transdet(rb_struc(acnt).rb_numrec)
034800161004                                          .rb_Account = bankacc;
034900161004             rb_struc(acnt).rb_transdet(rb_struc(acnt).rb_numrec)
035000161004                                          .rb_total = payamt;
035001161010             rb_struc(acnt).rb_transdet(rb_struc(acnt).rb_numrec)
035002161010                                          .rb_emp = payemp;
035003161010
035100161004             rb_struc(acnt).rb_transdet(rb_struc(acnt).rb_numrec)
035200161004                                          .rb_charges = payelcDS.pechgamt;
035300161004             // write summary figures
035400161004             rb_struc(acnt).rb_transsum.rb_amount += payamt;
035500161004             rb_struc(acnt).rb_transsum.rb_dbacc = rbrgl;
035600161004             rb_struc(acnt).rb_transsum.rb_cracc = rbrgl;
035700161004
035800161004      /end-free
035900161004     P write_to_struc  E
036000161004
036100161004      *
036200161004     P*-----------------------------------------
036300161004     P* Procedure name: prepare_batch
036400161004     P* Purpose:        prepare batch
036500161004     P* Returns:        0
036600161004     P* Parameter:      0
036700161004     P*-----------------------------------------
036800161004     P prepare_batch   B
036900161004     D prepare_batch   PI
037000161004
037100161004     D acnt            s              4  0
037200161004     D bcnt            s              4  0
037300161004     D numDebrec       s              7  0
037400161004     D numCrdrec       s              7  0
037500161004     D numDebamt       s             15  2
037600161004     D numCrdamt       s             15  2
037700161004     D batchtotal      s             15  2
037800161004     D batchcharges    s             15  2
037900161004     D clearingacc     s              7  0
038000161004
038100161004     D SPACTCODE       c                   300
038200161004     D SPACTCODE1      c                   301
038300161004     D SPACTCODE2      c                   051
038400161004     D SPACTCODE3      c                   075
038500161004     D CREDITCODE      c                   'C'
038600161004     D DEBITCODE       c                   'D'
038700161004      /free
038800161004      *
038900161004       //  //  write header record;
039000161004       //  //  ...create/open file;
039100161004       //  //----------------------
039200161004         write_to_file('1');
039300161004         rb_head_date = %char(file_date);
039301161004
039302161004       //  //getting transaction time and putting it in hhmm formate
039303161004         now_time = %time();
039304161004         rb_head_time = %subst(%scanrpl(':':'':%subst(%char(now_time):
039305161004         1:5)):1:4);
039306161004
039307161004       //
039308161012       //   exec sql select count(*) into :rb_head_NoRec from genpysumL4
039309161012       //        where paydiv = :RBANK ;
039310161012       //
039311161013            rb_head_NoRec +=4;
039312161004           rb_head_runno = %dec(%subst(rb_head_time:1:2):2:0);
039313161004           rb_head_prono = %dec(rb_head_time:4:0);
039314161004
039400161004         write_to_file('2':rb_text_header);
039500161004
039600161004       //  //  write detail records;
039700161004       //  //------------------------
039701161004     D*rb_text_detail   ds
039702161004     D* rb_det_transit                 8  0
039703161004     D* rb_det_space                   2
039704161004     D* rb_det_seq#                    6  0
039705161004     D* rb_det_paytext                 4    inz('PAYR')
039706161004     D* rb_det_paycurr                 3    inz('GYD')
039707161004     D* rb_det_account                 7  0
039708161004     D* rb_det_spcacc                  3  0
039709161004     D* rb_det_acctype                 1
039710161004     D* rb_det_total                  15  2
039711161004     D* rb_det_date1                   7  0
039712161004     D* rb_det_date2                   7  0
039713161004     D* rb_det_stuff                   9  0
039714161010     D*
039715161010     D*
039716161004     D*rb_text_detail   ds
039717161004     D* rb_det_type                    1    inz('D')
039718161004     D* rb_det_recNo                   7  0 inz(0)
039719161004     D* rb_det_docType                 2
039720161004     D* rb_det_field4                  3  0 inz(300)
039721161004     D* rb_det_total                  16  0
039722161004     D* rb_det_filler1                 4    inz('BBBB')
039723161004     D* rb_det_filler2                 3    inz('BBB')
039724161004     D* rb_det_filler3                 2    inz('BB')
039725161004     D* rb_det_filler4                 4    inz('BBBB')
039726161004     D* rb_det_filler5                16  0 inz(*zero)
039727161004     D* rb_det_filler6                16  0 inz(*zero)
039728161004     D* rb_det_cheq_no                16  0 inz(*zero)
039729161004     D* rb_det_transit                13    inz(zero)
039730161004     D* rb_det_bank                    3  0 inz(001)
039731161004     D* rb_det_account                16
039732161004     D* rb_det_trnscod                16
039733161004     D* rb_det_text                   36    inz('    ')
039734161004     D* rb_det_text2                  36    inz('    ')
039735161004     D* rb_det_filler8                 2    inz('BB')
039800161004          for acnt = 1 to gcnt;
039900161004
040000161004             // accumulate totals
040100161004             for bcnt = 1 to rb_struc(acnt).rb_numrec;
040200161004                // write detail figures
040300161004                rb_struc(acnt).rb_transdet(bcnt)
040400161004                                          .rb_charges = payelcDS.pechgamt;
040500161004                //write detail lines
040600161004                seqcnt += 1;
040700161004                numCrdrec += 1;
040800161004                batchtotal += rb_struc(acnt).rb_transdet(bcnt)
040900161004                                            .rb_total;
041000161004                batchcharges += rb_struc(acnt).rb_transdet(bcnt)
041100161004                                            .rb_charges;
041200161004
041300161004                numCrdamt  += rb_struc(acnt).rb_transdet(bcnt)
041400161004                                            .rb_total;
041401161010
041402161012         //    if origin = 'PAY';
041403161010
041404161012         //   get_emp_info(rb_struc(acnt).rb_transdet(bcnt)
041405161012          //                                .rb_emp:*omit:'  ':emp_name);
041406161012           // rb_det_text = %trim(emp_name);
041407161012          //  endif;
041408161010
041500161004                    spaceholder  = 'CR';
041600161004                write_batch_detail(rbarr(acnt):
041700161004                                   spaceholder:
041800161004                                        seqcnt:
041900161004                rb_struc(acnt).rb_transdet(bcnt).rb_Account:
042000161004                                    SPACTCODE:
042100161004                                   CREDITCODE:
042200161013                rb_struc(acnt).rb_transdet(bcnt).rb_total);
042300161013       //                               julDate:
042400161013       //                               julDate:
042500161013       //                                zeros);
042600161004
042700161004             endfor;
042800161004
042900161004
043000161004       //      // write summary figures
043100161004       //      //-------------------------------
043200161004       //      // write credit summary record
043300161004       //      //-------------------------------
043400161010       //        seqcnt += 1;
043500161010       //
043600161010       //        numCrdrec += 1;
043700161010       //        numCrdamt  +=  rb_struc(acnt).rb_transsum.rb_amount;
043800161010       //             spaceholder = 'CR';
043900161010       //          write_batch_detail(rbarr(acnt):
044000161010       //                             spaceholder:
044100161010       //                                  seqcnt:
044200161010       //                      payelcDS.pedftclac:
044300161010       //                              SPACTCODE2:
044400161010       //                              CREDITCODE:
044500161010       //    rb_struc(acnt).rb_transsum.rb_amount:
044600161010       //                                 julDate:
044700161010       //                                 julDate:
044800161010       //                                  zeros);
044900161010       //
044901161013
044902161013
045000161004       //      //-------------------------------
045100161004       //      // write Debit summary record
045200161004       //      //-------------------------------
045300161013       //        seqcnt += 1;
045400161013       //
045500161013       //        numDebrec += 1;
045600161013       //        numDebamt  +=  rb_struc(acnt).rb_transsum.rb_amount;
045700161013       //
045800161013       //        write_batch_detail(rbarr(acnt):
045900161013       //                           spaceholder:
046000161013       //                                seqcnt:
046100161013       //                    payelcDS.pedftclac:
046200161013       //                            SPACTCODE3:
046300161013       //                             debitCODE:
046400161013       //  rb_struc(acnt).rb_transsum.rb_amount);
046500161013       ////                               julDate:
046600161013       //                               julDate:
046700161013       //                                zeros);
046800161010
046900161010          // only write these records if the
047000161010          // current transit isn't the main transit
047100161013       //   if (rbarr(acnt)) <> payelcDS.pedfttrn#;
047200161013       //
047300161013       //      //-------------------------------
047400161013       //      // write Debit transit clearing record
047500161013       //      //-------------------------------
047600161013       //      seqcnt += 1;
047700161013       //
047800161013       //        numDebrec += 1;
047900161013       //        numDebamt  +=  rb_struc(acnt).rb_transsum.rb_amount;
048000161013       //
048100161013       //        exec sql
048200161013       //         select rbrgl into :clearingacc from rbgrange
048300161013       //           where rbrtrans# = :payelcDS.pedfttrn#;
048400161013       //
048500161013       //        write_batch_detail(rbarr(acnt):
048600161013       //                                     spaceholder:
048700161013       //                                          seqcnt:
048800161013       //                                     clearingacc:
048900161013       //                                      SPACTCODE3:
049000161013       //                                       debitCODE:
049100161013       //            rb_struc(acnt).rb_transsum.rb_amount:
049200161013       //                                         julDate:
049300161013       //                                         julDate:
049400161013       //                                          zeros);
049500161013       //
049600161010             //-------------------------------
049700161010             // write Credit transit clearing record
049800161010             //-------------------------------
049900161013       //      seqcnt += 1;
050000161013       //
050100161013       //      numCrdrec += 1;
050200161013       //      numCrdamt  +=  rb_struc(acnt).rb_transsum.rb_amount;
050300161013       //
050400161013       //      write_batch_detail(payelcDS.pedfttrn#:
050500161013       //                                 spaceholder:
050600161013       //                                      seqcnt:
050700161013       //         rb_struc(acnt).rb_transsum.rb_cracc:
050800161013       //                                  SPACTCODE2:
050900161013       //                                  CREDITCODE:
051000161013       //        rb_struc(acnt).rb_transsum.rb_amount:
051100161013       //                                     julDate:
051200161013       //                                     julDate:
051300161013       //                                      zeros);
051400161013       //   endif;
051500161010
051600161004         endfor;
051700161004       //
051800161004       //   // write Debit Figure - TOTAL BATCH
051900161004       //   //---------------------------------
052000161004          seqcnt += 1;
052100161004          numDebrec += 1;
052200161004          numDebamt  +=  batchtotal;
052300161004                   spaceholder = 'DR';
052400161004          write_batch_detail(payelcDS.pedfttrn#:
052500161004                                  spaceholder:
052600161004                                       seqcnt:
052700161004                                        pacct:
052800161004                                    SPACTCODE1:
052900161004                                     debitCODE:
053000161013                                    batchtotal);
053100161013       //                               julDate:
053200161013       //                               julDate:
053300161013       //                                zeros);
053400161004
053500161013          // write Debit Figure - TOTAL CHARGES
053600161013          //-----------------------------------
053700161013
053800161013          seqcnt += 1;
053900161013          numDebrec += 1;
054000161013          numDebamt  +=  batchcharges;
054100161013
054200161013          write_batch_detail(payelcDS.pedfttrn#:
054300161013                                  spaceholder:
054400161013                                       seqcnt:
054500161013                                        pacct:
054600161013                                    SPACTCODE1:
054700161013                                     debitCODE:
054800161013                                  batchcharges);
054900161004       //                                julDate:
055000161004       //                                julDate:
055100161004       //                                 zeros);
055200161004       //
055300161013
055400161013          // write credit Figure - TOTAL CHARGES
055500161013          //-----------------------------------
055600161013          seqcnt += 1;
055700161013          numCrdrec += 1;
055800161013          numCrdamt  +=  batchcharges;
055900161013
056000161013          write_batch_detail(payelcDS.pedfttrn#:
056100161013                                   spaceholder:
056200161013                                        seqcnt:
056300161013                           payelcDS.pedftchgac:
056400161013                                    SPACTCODE2:
056500161013                                    CREDITCODE:
056600161013                                  batchcharges);
056700161004       //                                julDate:
056800161004       //                                julDate:
056900161004       //                                 zeros);
057000161004       //
057100161004       //
057200161004       //   // write footer record;
057300161004       //
057400161004       ////      rb_foot_space = *blanks;
057500161004       ////      rb_foot_crtot = numcrdamt;
057600161004       ////      rb_foot_dbtot = numdebamt;
057700161012             rb_foot_DetRec = rb_head_NoRec;
057701161012
057702161012
057704161004
057800161004       //      rb_foot_crcnt = numcrdRec;
057900161004       //      rb_foot_dbcnt = numdebRec;
057901161004     D**rb_text_footer   ds
057902161004     D** rb_foot_trans                  8  0 inz(99999999)
057903161004     D** rb_foot_space                  8
057904161004     D** rb_foot_crtot                 15  2
057905161004     D** rb_foot_dbtot                 15  2
057906161004     D** rb_foot_crcnt                  7  0
057907161004     D** rb_foot_dbcnt                  7  0
057908161004     D**
057909161004     D*rb_text_footer   ds
057910161004     D* rb_foot_type                   1    inz('T')
057911161004     D* rb_footfiller1                 7    inz('EOD9999')
057912161004     D* rb_foot_DetRec                 8  0 inz(0)
057913161004     D* rb_footfiller2               194    inz(*blank)
057914161004     D* rb_footfiller3                 2    inz('BB')
057915161004      *
058000161004             write_to_file('2':%trim(rb_text_footer));
058100161004
058200161004             //close file
058300161004             write_to_file('3');
058400161004       //
058500161004      */end-free
058600161010     P prepare_batch   E
058700161004      *
058800161004     P**-----------------------------------------
058900161004     P** Procedure name: write_to_file
059000161004     P** Purpose:        write to file
059100161004     P** Returns:        0
059200161004     P** Parameter:      0
059300161004     P**-----------------------------------------
059400161004     P write_to_file   B
059500161004     D write_to_file   PI
059600161004     D option                         1    const
059700161010     D data                         212    const varying options(*nopass)
059800161004
059900161004     D/copy qcpysrc,IFSPR
060000161004     D fd              S             10I 0
060100161010     D wrdata          S            214A
060200161010     D rddata          S            214A
060300161004     D flags           S             10U 0
060400161004     D mode            S             10U 0
060500161004     D Msg             S             50A
060600161004     D Len             S             10I 0
060700161004      /free
060800161004
060900161004       ////*************************************************************
061000161004       //// writing data to a stream file
061100161004       ////****************************************************************
061200161004         select;
061300161004           when option = '1';
061400161004
061500161004               fd = unlink(payelcDS.pefilenm);
061600161004
061700161004             flags = O_WRONLY + O_CREAT + O_TRUNC +
061800161004                     O_TEXTDATA + O_CODEPAGE;
061900161004
062000161004             mode =  S_IRUSR + S_IWUSR
062100161004                         + S_IRGRP + S_IROTH;
062200161004
062300161004             // Open IFS file in ASCII format
062400161004
062500161004             fd = open(payelcDS.pefilenm: flags: mode: 819);
062600161004
062700161004             if  fd < 0;
062800161004             // Msg = 'open(): failed for writing';
062900161004             // return;
063000161004             endif;
063100161004
063200161004             // Close IFS file
063300161004             callp  close(fd);
063400161004
063500161004             // Reopen FS file
063600161004              fd = open(payelcDS.pefilenm:O_WRONLY + O_TEXTDATA);
063700161004
063800161004           // Write some data
063900161004           when option = '2';
064000161004             wrdata = data + CRLF;
064100161004             callp write(fd: %addr(wrdata): %len(%trimr(wrdata)));
064200161004
064300161004           // close the file
064400161004           when option = '3';
064500161004             callp  close(fd);
064600161004         endsl;
064700161004
064800161004      */end-free
064900161010     P write_to_file   E
065000161004      *
065100161004     P**-----------------------------------------
065200161004     P** Procedure name: write_batch_detail
065300161004     P** Purpose:        write batch detail lines
065400161004     P** Returns:        0
065500161004     P** Parameter:      0
065600161004     P**-----------------------------------------
065700161004     P write_batch_detail...
065800161004     P                 B
065900161004     D write_batch_detail...
066000161004     D                 PI
066100161004     D transit#                       8  0 const
066200161004     D space                          2    const
066300161004     D seq#                           6  0 const
066400161004     D account                        7  0 const
066500161004     D specialacc                     3  0 const
066600161004     D gltype                         1    const
066700161004     D total                         15  2 const
066800161013     D* date1                          7  0 const
066900161013     D* date2                          7  0 const
067000161013     D* stuff                          9  0 const
067100161004      /free
067101161004        DCL-s transit_code_det char (3) inz('300');
067200161004               //write detail lines
067300161004       //        rb_det_transit = transit#;
067400161004       //        rb_det_space = space;
067500161004       //        rb_det_seq# = seq#;
067600161004       //        rb_det_account= account;
067700161004       //
067800161004       //        rb_det_spcacc = Specialacc;
067900161004       //        rb_det_acctype = gltype;
068000161004       //
068100161004       //        rb_det_total = total;
068200161004       //
068300161004       //        rb_det_date1 = date1;
068400161004       //        rb_det_date2 = date2;
068500161004       //        rb_det_stuff = stuff;
068501161013       if gltype = 'C';
068502161013         transit_code_det = '300';
068503161004        rb_det_docType = '02';
068504161004        else;
068505161013         transit_code_det = '301';
068506161004          rb_det_docType = '01';
068507161004       ENDIF;
068600161004
068601161004         rb_det_transit = four_zero+ zero + %editc(transit#:'X');
068602161004               rb_det_account= nine_zero + %editc(account:'X');
068603161004               rb_det_recNo = seq#;
068604161004               rb_det_total = total;
068605161004               rb_det_trnscod = nine_zero+four_zero+transit_code_det;
068700161004               write_to_file('2':rb_text_detail);
068800161004      /end-free
068900161004     P write_batch_detail...
069000161004     P                 E
069100161004      *
069200161004     P**-----------------------------------------
069300161004     P** Procedure name: get_credit_date
069400161004     P** Purpose:        get_credit_date
069500161004     P** Returns:        0
069600161004     P** Parameter:      0
069700161004     P**-----------------------------------------
069800161004     P get_credit_date...
069900161004     P                 B
070000161004     D get_credit_date...
070100161004     D                 PI             6  0
070200161004     D dateIn                         6  0 const
070300161004     D dateAcc                        6  0 const
070400161004
070500161004     D olddateISO      s              8  0
070600161004     D newdate         s              6  0
070700161010       //
070701161010
070702161010      /free
070800161004
070900161004             // DO NOT allow Credit date falling on a weekend
071000161004             //.... day 6 or 7
071100161004             monitor;
071200161004             olddateISO = %dec(%date(dateIn:*dmy):*iso);
071300161004             if dayofweek(olddateISO) = 6
071400161004               or dayofweek(olddateISO) = 7;
071500161004             //... etc.
071600161004             //newdate = %dec(%date(olddateISO:*dmy)+ %days(1));
071700161004               newdate = dateAcc;
071800161004              else;
071900161004               newdate = dateIn;
072000161004             endif;
072100161004             on-error;
072200161004             show_msg('Error found with '+ %char(payemp)+ ':'+ %char(dateIn));
072300161004             endmon;
072400161010
072500161004          return newdate;
072600161004
072700161004      /end-free
072800161004     P get_credit_date...
072900161004     P                 E
073000090107
