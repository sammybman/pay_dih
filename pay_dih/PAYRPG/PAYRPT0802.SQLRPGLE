000100180228     H DECEDIT('.') DATEDIT(*DMY/)
000200140312      //* LISTING PAYROLL YEAR TO DATES
000300140312      //* ADJUSTMENTS BY------T.I.BYNOE  *** R.WILLIAMS
000400140312      //*                     C.WILLIAMS   & S.BRITTON
000500021030     Fpayeytdl03if   e           k disk
000600160301     Fqsysprt3  O    f  150        printer oflind(*inof)
000700140312      // Payroll PROTOTYPES
000800140307      /copy qcpysrc,genPR
000900140307      /copy qcpysrc,empPR
001000140307      /copy qcpysrc,PAYPR
001100180131      /copy qcpysrc,salpr
001200140307      /copy qcpysrc,taxpr
001300090225     D
001400140312      //-------------------------------------
001500140312      // Prototype for procedure: getEmpData
001600140312      //-------------------------------------
001700090225     D getEmpData      PR
001800090225     D
001900140312      //
002000021030     D  ytd_list       ds                  import
002100021030     D  pcdftytdDS                   10
002200021030     D dft_list                       2  0 dim(5) overlay(pcdftytdDS)
002300140312      //
002400021030     D hdr_text        s              9    dim(99)
002500021030     D hdr_type        s              2  0 dim(99)
002600021030     D det_amt         s             11  2 dim(99)
002700021031     D tot_amt         s             13  2 dim(99)
002800160301     D
002900160301     D nis             s             13  2 inz(0)
003000160301     D paye            s             13  2 inz(0)
003100160301     D
003200020118     D pay_year        s              4  0
003300021030     D last_emp        s                   like(emp)
003400021030     D last_dept       s                   like(dept)
003500021030     D last_name       s                   like(name)
003600090220     D last_ird        s                   like(irdnum)
003700020118     D seq             s              1  0
003800021029     D sel             s              1  0
003900021030     D xcnt            s              3  0 inz(1)
004000021030     D counter         s              3  0 inz(1)
004100050525     D countr          s              5  0
004200021031     D cnt             s              3  0 inz(1)
004300021031     D cnt2            s              3  0 inz(2)
004400021031     D cnt3            s              3  0 inz(3)
004500021031     D cnt4            s              3  0 inz(4)
004600021031     D cnt5            s              3  0 inz(5)
004700021030     D ycnt            s              3  0
004800020122     D coltxt          s             37
004900090224     D name            s             65
005000160301     D
005100160301     D fname           s             20    inz(' ')
005200160301     D lname           s             20    inz(' ')
005300160301     D
005400180213     D** coname          s             30
005500020118     D @title          s             40
005600020118     D @date           s             10
005700020118     D arr             S             40    dim(1) ctdata perrcd(1)
005800090220     D seqcntr         s              4  0
005900090220     D noMonths        s              4  0
006000090224     D partEmp         s              1
006100090224     D title           s              4
006200090224     D tmp_status      s              1
006300090224     D int_yrend       s              8  0
006400090224     D int_yrstr       s              8  0
006500140312      //
006600090224     D YREND_DATE      c                   '3112'
006700090224     D YRSTR_DATE      c                   '0101'
006800090220     D LIMIT           c                   5
006900090220     D BLANK_TEXT      c                   'GROSS '
007000090220     D MONTHSINYEAR    c                   12
007100090220     D NAMCNST         c                   'EMPLOYEE NAME        &        ADDRE-
007200090220     D                                     SS'
007300140312      //
007400950930     D                SDS
007500950930     D  @PGM             *PROC
007600140307     D  @USER                358    367
007700140312      //
007800140307       dcl-s empTin    Char(20);
007900160307       dcl-s page_tot1   Packed(13:2);
008000160307       dcl-s page_tot2   Packed(13:2);
008100140310       dcl-s days_wrk    Packed(1:0);
008200140310       dcl-s weeks_work    Packed(2:0);
008300140310       dcl-s total_days_worked_in_year  Packed(3:0);
008400140311       dcl-s linechecker   Packed(2:0) inz (0);
008500140310       Dcl-C weeksINYEAR        52;
008600140311       Dcl-C tax_Code     'VAT';
008700160301       Dcl-C NOTFOUND     '02000';
008800160301      *
008900160301       Dcl-s address    char(80) INZ(*BLANK);
009000160301       // // Purpose: taxable value
009100160301       DCL-S gross PACKED(13:2) INZ(0);
009200160301       DCL-S record_cnt PACKED(6:0) INZ(0);
009300160301       // // Purpose: Small version to GROSS variable
009400160301       DCL-S smlGross PACKED(10:2) INZ(0);
009500160301       DCL-S smlNIS   PACKED(10:2) INZ(0);
009600160301       DCL-S smlPAYE  PACKED(10:2) INZ(0);
009700160301       //
009800160301       DCL-S page_tot3 LIKE(page_tot2) ;
009900170227       //
010000180213
010001180213       // --------------------------------------------------
010002180213       // Prototype for procedure: read_Employee_info
010003180213       // --------------------------------------------------
010004180213       DCL-PR read_Employee_info ;
010005180213         employee_number PACKED(6:0);
010006180213       END-PR ;
010007180213
010008180213       DCL-C MAX_DAYS_IN_WEEK CONST(5);
010100180213       //
010102180213       //
010103180213       DCL-DS gradetailDS EXTNAME('GRADETAIL');
010104180213       END-DS ;
010105180220       DCL-DS GRA_EMP_STRUCDS EXTNAME('EMPMAST') QUALIFIED;
010106180220       END-DS ;
010107180220       //DCL-S gra_emp_struc LIKE(emp_struc) inz;
010108180213       //
010109180213       DCL-S comptin CHAR(25) INZ(*BLANK);
010110180213       //
010111180213       DCL-C WEEKLY CONST('Weekly');
010112180213
010118180213
010119180213       //
010120180213       DCL-S sum_of_tax PACKED(12:0) INZ(0);
010121180213       //
010122180213       DCL-S tot_taxdeducted PACKED(12:0) ;
010123180213       //
010124180213       DCL-S year_month_char CHAR(5) INZ(*BLANK);
010125180213       //
010126180213       DCL-S year_month PACKED(4:0) INZ(0);
010127180213       //
010128180213       DCL-S numberofemp PACKED(4:0) INZ(0);
010129180213       //
010130180213       //
010131180213       DCL-S other_all PACKED(10:0) INZ(0);
010132180213       //
010133180213       DCL-S other_inx PACKED(10:0) INZ(0);
010134180213       //
010135180213       DCL-S monthSalary PACKED(10:0) INZ(0);
010136180213       //
010137180213       DCL-S onames CHAR(30) INZ(*BLANK);
010138180213       //
010139180213       DCL-S month_year PACKED(4:0) INZ(0);
010140180213       //
010141180213       DCL-S salary PACKED(10:0) INZ(0);
010142180213       //
010143180213       DCL-S coemp PACKED(6:0) INZ(0);
010144180213       //
010145180213       //
010146180213       DCL-S payfreq CHAR(10) INZ(*BLANK);
010147180213       //
010148180213       DCL-S coname CHAR(30) INZ(*BLANK);
010149180213       //
010150180213       DCL-S total_deduct PACKED(12:0) INZ(0);
010151180213       //
010152180213       DCL-S statutary_ded LIKE(total_deduct) INZ(0);
010153180213       //
010154180213       DCL-S other_inc LIKE(other_all) INZ(0);
010155180213       //
010156180213       DCL-S total_paid LIKE(statutary_ded) INZ(0);
010157180213       //
010158180213       DCL-S empdob CHAR(10) INZ(*BLANK);
010159180213       //
010160180213       //
010161180213       DCL-S coaddress VARCHAR(90) INZ(*BLANK);
010162180213       DCL-S total_NIS PACKED(12:0) INZ(0);
010163180213       //
010164180213       //
010165180213       DCL-S prvtin CHAR(25) INZ(*BLANK);
010166180213       DCL-S prevename CHAR(30) INZ(*BLANK);
010167180213       //
010168180213       DCL-S prveaddres VARCHAR(90) INZ(*BLANK);
010169180220       //
010170180220       DCL-S coname# LIKE(coname) INZ(*BLANK);
010171180220       //
010172180220       DCL-S month_year_code CHAR(2) INZ(*BLANK);
010173180220       //
010175180220       //
010176180220       DCL-DS CompanyDataDS INZ LIKEDS(companyInfoDS);
010177180220
010178180221
010191180225       // // Purpose: Statutary Deduction as calculated per individual
010192180225       DCL-S Net_Stat_Deduction LIKE(statutary_ded) INZ(0);
010193180225
010194180225       // --------------------------------------------------
010195180225       // Prototype for procedure: getTaxAllowancePrc
010196180225       // --------------------------------------------------
010197180225       DCL-PR getTaxAllowancePrc PACKED(12:0);
010198180225         period_code CHAR(2);
010199180225       END-PR ;
010200180225
010201180225       //
010202180225       DCL-C WHOLE_YEAR CONST(52);
010203180225
010204021030          exsr  aainit;
010300090220
010400090225          coltxt = namcnst;
010500021030
010600090225          exsr write_heading;
010700160301              reset record_cnt;
010800160301             exec sql
010900160301               select count(*) into :record_cnt from gradetail where grayear =
011000180221                  :pay_year and grasts = 'Y';
011100160301            // if records already exist for the reporting year,
011200160301            // delete them and re-write them.
011300160301              if record_cnt > 0 or sqlstt <> NotFound;
011400180225              exec sql  delete from gradetail where grayear = :pay_year
011401180225                AND GRASTS = 'Y'  with nc;
011500160301               endif;
011600021030
011601180225         month_year_code = 'WK';
011700021030         setll  pay_year  ytdrec;
011800021030         reade  pay_year  ytdrec;
011900021030
012000021030          exsr get_name_text;
012100021030          last_emp = emp;
012200021030          last_dept = dept;
012300021030          last_name = name;
012400090220          last_ird = irdnum;
012500090224
012600090224
012700090225          getEmpData();
012800021030
012900021030          dow not(%eof);
013000021030
013100021030             ycnt = %lookup(ytdtype:hdr_type);
013200021030
013300021030              // employee number changed
013400021030                if emp <> last_emp;
013500021030                  exsr print_details;
013600021030                  last_emp = emp;
013700021030                  last_dept = dept;
013800021030                  exsr get_name_text;
013900021031                  last_name = name;
014000090220                  last_ird = irdnum;
014100090224
014200090225                  getEmpData();
014300090224
014400021030                  clear det_amt;
014500021030                endif;
014600021030                if ycnt <> 0;
014700021030                  det_amt(ycnt) = det_amt(ycnt) + ytdamt;
014701180228       //             IF LINECHECKER > 40;
014702180228       //             DSPLY LINECHECKER;
014703180228       //          dsply seqcntr;
014704180228       //             DSPLY YTDAMT;
014705180228       //             DSPLY TOT_AMT(YCNT);
014706180228       //             DSPLY EMP;
014707180228       //             ENDIF;
014800021030                  tot_amt(ycnt) = tot_amt(ycnt) + ytdamt;
014900021030                endif;
015000090220
015100021030            reade  pay_year  ytdrec;
015200021030          enddo;
015300021030
015400021030          exsr print_details;
015500021031          exsr write_final;
015600021030          eval *inlr = *on;
015700021030          return;
015800021030
015900021030       //----------
016000021030       // Get Name
016100021030       //----------
016200021030       begsr get_name_text;
016300090224          name = surnam;
016400090224          if   christ <> *blanks;
016500090224                   name = %trim(name) + ' ,' +
016600090224                    %triml(christ);
016700160301
016800090224          endif;
016900160301          reset fname;
017000160301          reset lname;
017100160301
017200160301          fname = christ;
017300160301          lname = surnam;
017400090220
017500090224          // get title
017600090224          select;
017700090224           when sex = 'M';
017800090224             title = 'MR.';
017900090224           when sex = 'F';
018000090224             if marsta = 'M';
018100090224               title = 'MRS.';
018200090224               else;
018300090224               title = 'MS.';
018400090224             endif;
018500090224           other;
018600090224             title = 'MR.';
018700090224          endsl;
018800090224
018900090224          name = Title + ' ' + %trim(name);
019000090224
019100090224          if addone <> *blanks;
019200090224             name = %trim(name) + ' : ' + %triml(addone);
019300090224          endif;
019400090220
019500090224          if addtwo <> *blanks;
019600090224             name = %trim(name) + ',' + %triml(addtwo);
019700090224          endif;
019800090224
019900160301            address = %trim(addone) + ' ' + %triml(addtwo);
020000021030       endsr;
020100021031
020200021031       //---------------
020300021031       // Write Heading
020400021031       //---------------
020500021031       begsr write_heading;
020600021031         exsr reset_counters;
020700140311         dsystaxc= tax_code;
020800140311
020900140307         get_taxT_info('RD':dsystaxc:taxt_struc);
021000140307         empTin = txyreg#;
021100140307
021200021031         except  hedr;
021300021031         // Handle Multiple line heading
021400021031         dow cnt5 < xcnt;
021500160301             dsply  hdr_text(cnt);
021600160301               dsply  hdr_text(cnt2);
021700160301               dsply  hdr_text(cnt3);
021800021031         exsr increment_counters;
021900160301             dsply  hdr_text(cnt);
022000160301               dsply  hdr_text(cnt2);
022100160301               dsply  hdr_text(cnt3);
022200021031         except  hedrincr;
022300021031         enddo;
022400021031
022500021031         except  hedrnext;
022600021031       endsr;
022700021031
022800021031       //---------------
022900021031       // Write Detail
023000021031       //---------------
023100021031       begsr write_detail;
023200021031         exsr reset_counters;
023300160301
023400021219          if (det_amt(cnt) + det_amt(cnt2) +
023500021219             det_amt(cnt3) + det_amt(cnt4) +
023600021219             det_amt(cnt5)) <> *zeros;
023700021219            except  detail;
023800021219          endif;
023900021031         // Handle Multiple line details
024000021031         dow cnt5 < xcnt;
024100021220          exsr increment_counters;
024200021220          if (det_amt(cnt) + det_amt(cnt2) +
024300021220             det_amt(cnt3) + det_amt(cnt4) +
024400021220             det_amt(cnt5)) <> *zeros;
024500021220           except  detail;
024600021220          endif;
024700021031         enddo;
024800021031
024900021031       endsr;
025000021031
025100021031       //-------------------
025200021031       // Write Final totals
025300021031       //-------------------
025400021031       begsr write_final;
025500021031         exsr reset_counters;
025501180213           //----------------------------------------------//
025502180213           //Write last row of summary data to table.
025503180213           //----------------------------------------------//
025504180213
025505180213           seqcntr = 9999;
025507180213           fname = coname;
025508180213           lname = ' ';
025509180220                       month_year = pay_year;
025517180213           year_month_char = %char(month_year);
025518180213           NOCORRECT  = *BLANKS;
025519180213           SALARY = 0;
025520180213           empdob = ' ';
025521180213           coemp = 0;
025522180213           title = ' ';
025523180213           payfreq = ' ';
025524180213           reset onameS;
025525180213           reset monthSalary ;
025526180213           reset other_inc;
025527180213           reset other_all;
025528180213           reset  prvtin ;
025529180213           reset prevename;
025530180213           reset prveaddres;
025531180220           comptin = companyDATADS.TaxReg#;
025532180220           coaddress =  %TRIM(companyDATADS.ADDRESS1) + ' '+
025533180220                %TRIM(companyDATADS.ADDRESS2) + ' '
025534180220                       + %TRIM(companyDATADS.ADDRESS3);
025535180220           //  dsply year_month_char;
025536180213           exec sql
025537180213             select sum(gragross),sum(granis),sum(grapaye),sum(gratotpaid),
025538180221                    sum(grastsded),count(*),sum(totdeduct)
025539180213                into
025540180221                :gross,:total_NIS,:paye,:total_paid,:statutary_ded,:noMonths,
025541180221                :total_deduct
025542180213               from gradetail
025543180225               where grayear = :pay_year and grasts = 'Y';
025544180221              coname = %trim(coname) ;
025545180221              lname =%trim(lname);
025546180221              coaddress   = %trim(coaddress);
025547180213              // tot_taxdeducted = getGRASummary();
025549180225
025550180213            exec sql
025551180213               insert into gradetail (grasts,graseq,gratin,granprefix,grafname,
025552180213                 gralname,graaddress,graweeks#,gradays,graweekp,gragross,
025553180213                 granis,grapaye,gratotpaid,gradob,gratimed,grayear,nocorrect,
025554180213                 graemp,grapayfreq,graonames,grasalary,otherinc,otherall,
025555180213                 grastsded,gracotin,graconame,gracoaddr,graprvctin,graprvcnm,
025556180213                 graprvcadd,totdeduct)
025557180221                values('Y',:seqcntr,:comptin,:title,:coname,:lname
025558180221               ,:coaddress
025559180213               ,:noMonths,:days_wrk,:year_month_char,:gross,:total_NIS,:paye,
025560180213               :total_paid
025561180213                 ,:empdob,CURRENT_TIMESTAMP,:pay_year,:NOCORRECT,:coemp,
025562180213               :payfreq,:onameS ,:monthSalary,:other_inc,:other_all
025563180225               ,:statutary_ded,:prvtin ,:prevename,:prveaddres,:prvtin
025564180213               ,:prevename,:prveaddres,:total_deduct)
025565180213
025566180213
025567180213                with nc;
025568180213
025569180213
025570180213
025600021031         except  final;
025700021031         // Handle Multiple line details
025800021031         dow cnt5 < xcnt;
025900021031         exsr increment_counters;
026000021031         except  final;
026100021031         enddo;
026300021031       endsr;
026400021031
026500021031       //--------------------
026600021031       // Increment Counters
026700021031       //--------------------
026800021031       begsr increment_counters;
026900021031            cnt = cnt + 5;
027000021031            cnt2= cnt2 + 5;
027100021031            cnt3= cnt3 + 5;
027200021031            cnt4= cnt4 + 5;
027300021031            cnt5= cnt5 + 5;
027400021031       endsr;
027500021031       //----------------
027600021031       // Reset Counters
027700021031       //----------------
027800021031       begsr reset_counters;
027900021031         reset cnt;
028000021031         reset cnt2;
028100021031         reset cnt3;
028200021031         reset cnt4;
028300021031         reset cnt5;
028400021031       endsr;
028500021031
028600021030       //-----------------------
028700021030       // Print Employee datails
028800021030       //-----------------------
028900021030       begsr print_details;
029000090220
029100180228       //      if  linechecker = 45;
029200180228       //        reset linechecker;
029300180228       //        page_tot1 = tot_amt(cnt) + tot_amt(cnt3);
029400180228       //        page_tot3 = tot_amt(cnt3);
029500180228       //        page_tot2 = tot_amt(cnt2);
029600180228       //        except page_total;
029700180228       //         exsr write_heading;
029800180228       //      endif;
029900180228       //
030000090220             if %xfoot(det_amt) <> *zeros;
030100090220                 seqcntr += 1;
030200140311                 linechecker +=1;
030201180228                   if  linechecker = 46;
030202180228               reset linechecker;
030203180228               page_tot1 = tot_amt(cnt) + tot_amt(cnt3);
030204180228               page_tot3 = tot_amt(cnt3);
030205180228               page_tot2 = tot_amt(cnt2);
030206180228               except page_total;
030207180228                exsr write_heading;
030208180228                else;
030209180228                 except detailnam;
030210180228             endif;
030211180228
030300180228      //           except detailnam;
030400090220             endif;
030500090220
030600180131           //gross = det_amt(cnt)+ det_amt(cnt3);
030700180131             gross = det_amt(cnt);
030800160301             paye = det_amt(cnt2);
030900160301             NIS =  det_amt(cnt3);
030901180220              coname# = *BLANK;
031000160301             smlgross = gross;
031100160301             smlpaye = paye;
031200160301             smlnis  = nis;
031201180213             granis = nis;
031202180213             nocorrect = 'N';
031203180213             payfreq = WEEKLY;
031204180220             read_Employee_info(last_emp);
031205180225             // Get employee date of birth date-formated
031206180225             empdob = %CHAR(%DATE(GRA_EMP_STRUCDS.birth:*EUR):*EUR);
031207180225
031208180226                 empdob = %scanrpl('.':'-':empdob);
031209180225             // pro-rate statuary deduction if employee is less that a year
031210180225                        if nomonths  < WHOLE_YEAR;
031211180225                        month_year_code = 'WK';
031212180225
031213180225             statutary_ded = getTaxAllowancePrc(month_year_code);
031214180225              Net_Stat_Deduction = (statutary_ded * noMonths);
031215180225                        ELSE;
031216180225              //employed at least a year
031217180225                         month_year_code = '*Y';
031218180225             statutary_ded = getTaxAllowancePrc(month_year_code);
031219180225              Net_Stat_Deduction = (statutary_ded);
031220180225                        ENDIF;
031222180225
031224180225              total_deduct = graNIS + Net_Stat_Deduction;
031225180225                 total_paid = gross + other_inc + other_all ;
031226180225                 address = %scanrpl('"':' ':address);
031300160301             exec sql
031400180213               insert into gradetail (grasts,graseq,gratin,granprefix,grafname,
031401180213                 gralname,graaddress,graweeks#,gradays,graweekp,gragross,
031402180213                 granis,grapaye,gratotpaid,gradob,gratimed,grayear,nocorrect,
031403180213                 graemp,grapayfreq,graonames,grasalary,otherinc,otherall,
031404180213                 grastsded,gracotin,graconame,gracoaddr,graprvctin,graprvcnm,
031405180213                 graprvcadd,totdeduct)
031500180225                values('Y',:seqcntr,:last_ird,:title,:fname,:lname,:address
031600180213               ,:noMonths,:days_wrk,:partEmp,:gross,:graNIS,:paye,:total_paid
031601180213                 ,:empdob,CURRENT_TIMESTAMP,:pay_year,:NOCORRECT,
031602180220               :last_emp,:payfreq,:onameS,:monthSalary,:other_inc,:other_all
031603180225               ,:Net_Stat_Deduction,:comptin,:coname#,:coaddress,:prvtin
031604180213               ,:prevename,:prveaddres,:total_deduct)
031605180213
031700180213                with nc;
031800160301
031900160301             exsr write_detail;
032000160301
032100160301
032200160301           endsr;
032300021031
032400160301        //*************************************************
032500160301        // First initilisation routine for all programs  **
032600160301       //*************************************************
032700140312       Begsr aainit;
032800180220         coname = get_coname(*omit:CompanyDATADS);
032900140312         @date = get_sys_date;
033000140312         @title = arr(1);
033100140312         //
033200140312       Endsr;
033300140312       //***********************
033400140312       // Start-up subroutine **
033500140312       //***********************
033600140312       begsr *inzsr;
033700140312      /END-FREE
033800021030     C     *entry        plist
033900021030     C                   parm                    seq
034000021030     C                   parm                    pay_year
034100021030     C                   parm                    sel
034200021030     C
034300021030      /free
034400021030         select;
034500021030           when  sel = 1 or sel = 3;
034600021030              for counter to limit;
034700021030             ycnt = %lookup(dft_list(counter):hdr_type);
034800021030               if ycnt = 0 or ycnt = xcnt;
034900021030                   hdr_type(xcnt) = dft_list(counter);
035000021030                     if dft_list(counter) = 0;
035100021030                       evalr  hdr_text(xcnt) = blank_text;
035200021030                         else;
035300050525                        get_payty_info('RD':dft_list(counter):payty_struc);
035400021031                        evalr  hdr_text(xcnt) = %trim(ptyshrtnm);
035500021030                     endif;
035600021030                   xcnt = xcnt + 1;
035700021030               endif;
035800021030              endfor;
035900021030           when  sel = 2;
036000050525            countr = *zeros;
036100050525            dow not get_payty_info('RD':-1:payty_struc:countr);
036200021031              evalr  hdr_text(xcnt) = %trim(ptyshrtnm);
036300021030               hdr_type(xcnt) = ptyptype;
036400021030              xcnt = xcnt + 1;
036500021030            enddo;
036600021030
036700021030         endsl;
036800140312       endsr;
036900140312       //
037000140312      /END-FREE
037100160301     Oqsysprt3  E            hedr           2 04
037200021031     O                       @pgm                10
037300011004     O                       coname              72
037400011004     O                       @date              130
037500021031     O          E            hedr           2
037600021031     O                       @user               10
037700011004     O                       @title              80
037800011004     O                                          125 'Page:'
037900011004     O                       page          z    130
038000021031     O          E            hedr           2
038100011003     O                                           30 'EMPLOYEES'
038200140307     O                                           46 'CURRENT  Y.T.D.'
038300140307     O                                           68 'FIGURES  FOR  YEAR'
038400140307     O                       pay_year            73
038500140307     O                                           90 'Company T.I.N: '
038600140307     O                       EmpTIN             110
038700090220     O                                          130 'IRD. FORM 2A'
038800021030
038900021031     O          E            hedr           1
039000090224     O                                            5 'SEQ.'
039100090224     O                                           11 'T.I.N'
039200090220     O                       coltxt              57
039300140310     O                                           90 'Weeks'
039400160301     O                                          096 'Year'
039500160301     O                       hdr_text(cnt)      109
039600160301     O                       hdr_text(cnt3)     126
039700160301     O                       hdr_text(cnt2)     143
039800090220
039900090220     O          E            hedr           1
040000090220     O                                           91 'Employed'
040100160301     O*                                         100 'Type'
040200160301     O                                          096 'P/W'
040300090220
040400021031     O          E            hedrincr       1
040500160301     O                       hdr_text(cnt)      109
040600160301     O                       hdr_text(cnt3)     122
040700160301     O                       hdr_text(cnt2)     132
040800090220
040900021031     O          E            hedrnext       2
041000021030
041100020118     O          e            detailnam      0
041200090224     O                       seqcntr              5
041300090224     O                       last_ird            17
041400090224     O                       last_name           85
041500090224     O                       last_name           85
041600090220     O                       noMonths      z     90
041700090220     O                       noMonths      z     90
041800160301     O                                           91 ':'
041900160301     O                       days_wrk      z     92
042000160301     O                       partEmp             94
042100020118
042200021031     O          e            detail         1
042300140312      //
042400160301     O                       smlgross      2    109
042500160301     O                       smlNIS        2    126
042600160301     O                       smlpaye       2    143
042700021030
042800140310     O          e            page_total  1
042900140312      //
043000160301     O                       page_tot1     2    109
043100160301     O                       page_tot3     2    126
043200160301     O                       page_tot2     2    143
043300140310     O          e            final       1
043400140312      //
043500160301     O                       tot_amt(cnt)  2    109
043600160301     O                       tot_amt(cnt2) 2    126
043700160301     O                       tot_amt(cnt3) 2    143
043800090225
043900140312      //-----------------------------------------------
044000140312      // Procedure name: getStrDate
044100140312      // Purpose:  Get start date of employee earnings
044200140312      // Returns:        1 (start date)
044300140312      // Parameter:      none
044400140312      //-----------------------------------------------
044500090225     P getEmpData      B
044600090225     D getEmpData      PI
044700090225
044800140312      //strdate         s              8  0
044900090225
045000090225
045100090225          // no. of months employed
045200090225          int_yrend = %int(yrend_date + %char(pay_year));
045300090225          int_yrstr = %int(yrstr_date + %char(pay_year));
045400140310              //if employee on still on job at the end of the reporting period
045500140312
045600090225          if resign = *zeros;
045700140310            // if employee worked for at least 1 year.
045800090225            if %date(int_yrstr:*eur) > %date(emply:*eur);
045900090225             noMonths = %diff(%date(int_yrend:*eur):
046000090225                              %date(int_yrstr:*eur) - %days(1):*months);
046100090225              else;
046200140310                // part year calculation
046300090225             noMonths = %diff(%date(int_yrend:*eur):
046400090225                              %date(emply:*eur)- %days(1):*months);
046500140310
046600140310                     total_days_worked_in_year = %diff(%date(int_yrend:*eur):
046700140310                              %date(emply:*eur) - %days(1):*days);
046800140310
046900140310
047000140311                            weeks_work = %DIV(total_days_worked_in_year:7);
047100140311                            days_wrk = %rem(total_days_worked_in_year:7);
047200140310
047300090225            endif;
047400140310          //----------------------------------------------------//
047500090225             else;
047600140310             //if employee on resigned during the reporting period
047700140310             // but started working before the year started
047800090225            if %date(int_yrstr:*eur) > %date(emply:*eur);
047900140310                             noMonths = %diff(%date(resign:*eur):
048000090225                              %date(int_yrstr:*eur) - %days(1):*months);
048100170227                      if %date(int_yrstr:*eur) < %date(resign:*eur);
048200170227                   monitor;
048300170227                   total_days_worked_in_year = %diff(%date(resign:*eur):
048400170227                              %date(int_yrstr:*eur) - %days(1):*days);
048500170227                    on-error;
048600170227                      dump(a);
048700170227                   endmon;
048800170227                  weeks_work = %DIV(total_days_worked_in_year:7);
048900170227                            days_wrk = %rem(total_days_worked_in_year:7);
049000170227                  endif;
049100140310                    else;
049200140310
049300140310             //if employee on resigned during the reporting period
049400140310             // but started working during the reporting year
049500140310
049600140310                              noMonths = %diff(%date(resign:*eur):
049700090225                              %date(emply:*eur) - %days(1):*months);
049800140310
049900140310                         total_days_worked_in_year = %diff(%date(resign:*eur):
050000140310                              %date(emply:*eur) - %days(1):*days);
050100140310
050200140310
050300140311                            weeks_work = %DIV(total_days_worked_in_year:7);
050400140311                            days_wrk = %rem(total_days_worked_in_year:7);
050500140310
050600090225            endif;
050700090225          endif;
050800090225
050900090225          if  noMonths >= MONTHSINYEAR;
051000140310             noMonths = weeksINYEAR;
051100140310             partEmp ='W';
051200140310               days_wrk = 0;
051300090225            else;
051400090225             partEmp = 'P';
051500140310             noMonths = weeks_work;
051600140310
051700170227               if  days_wrk > MAX_DAYS_IN_WEEK;
051800170227                 days_wrk = MAX_DAYS_IN_WEEK;
051900170227               ENDIF;
052000090225          endif;
052100090225
052200170220
052300090225     P getEmpData      E
052400180213
052401180213       // --------------------------------------------------
052402180213       // Procedure name: read_Employee_info
052403180213       // Purpose:
052404180213       // Returns:
052405180213       // Parameter:      employee_number
052406180213       // --------------------------------------------------
052407180213       DCL-PROC read_Employee_info ;
052408180213         DCL-PI *N;
052409180213           employee_number PACKED(6:0);
052410180213         END-PI ;
052411180213       //
052413180220              get_emp_info2('R':employee_number:GRA_EMP_STRUCDS);
052414180213
052415180213
052418180213                otherinc = 0;
052419180213                otherall = 0;
052420180213                nocorrect = 'N';
052421180213
052425180213
052426180213         return ;
052427180213       END-PROC ;
052428180213
052429180213       // --------------------------------------------------
052430180213       // Procedure name: getGRASummary
052431180213       // Purpose:        get summarised data fro GRADETAIL table
052432180213       // Returns:
052433180213       // --------------------------------------------------
052434180220       DCL-PROC getTaxAllowance ;
052435180213         exec sql
052436180220               select PTXFREE into :statutary_ded from paytaxctl where PTXCYC =
052437180220                  :MONTH_year_CODE;
052438180220       //  exec sql
052439180220       //    select sum(grapaye) into :sum_of_tax from gradetail where grayear =
052440180220       //           :pay_year;
052441180220       //
052442180213         return ;
052443180213       END-PROC ;
052444180225
052445180225       // --------------------------------------------------
052446180225       // Procedure name: getTaxAllowancePrc
052447180225       // Purpose:
052448180225       // Returns:
052449180225       // Parameter:      period_code
052450180225       // --------------------------------------------------
052451180225       DCL-PROC getTaxAllowancePrc ;
052452180225         DCL-PI *N PACKED(12:0);
052453180225           period_code CHAR(2);
052454180225         END-PI ;
052455180225         // Your local fields go here
052456180225
052457180225         DCL-S retField PACKED(12:0);
052458180225          exec sql
052459180225               select PTXFREE into :retField from paytaxctl where PTXCYC =
052460180225                  :period_code;
052461180225
052462180225         return retField ;
052463180225       END-PROC ;
052464180225**
052500011003 Payroll Earnings Year-to-Date Report
