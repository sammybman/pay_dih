000100950726     H DECEDIT('.') DATEDIT(*DMY/)
000200140312      //* LISTING PAYROLL YEAR TO DATES
000300140312      //* ADJUSTMENTS BY------T.I.BYNOE  *** R.WILLIAMS
000400140312      //*                     C.WILLIAMS   & S.BRITTON
000500021030     Fpayeytdl03if   e           k disk
000600011003     Fqsysprt   O    f  132        printer oflind(*inof)
000700140312      // Payroll PROTOTYPES
000800140307      /copy qcpysrc,genPR
000900140307      /copy qcpysrc,empPR
001000140307      /copy qcpysrc,PAYPR
001001140307      /copy qcpysrc,taxpr
001100090225     D
001200140312      //-------------------------------------
001300140312      // Prototype for procedure: getEmpData
001400140312      //-------------------------------------
001500090225     D getEmpData      PR
001600090225     D
001700140312      //
001800021030     D  ytd_list       ds                  import
001900021030     D  pcdftytdDS                   10
002000021030     D dft_list                       2  0 dim(5) overlay(pcdftytdDS)
002100140312      //
002200021030     D hdr_text        s              9    dim(99)
002300021030     D hdr_type        s              2  0 dim(99)
002400021030     D det_amt         s             11  2 dim(99)
002500021031     D tot_amt         s             13  2 dim(99)
002600020118     D pay_year        s              4  0
002700021030     D last_emp        s                   like(emp)
002800021030     D last_dept       s                   like(dept)
002900021030     D last_name       s                   like(name)
003000090220     D last_ird        s                   like(irdnum)
003100020118     D seq             s              1  0
003200021029     D sel             s              1  0
003300021030     D xcnt            s              3  0 inz(1)
003400021030     D counter         s              3  0 inz(1)
003500050525     D countr          s              5  0
003600021031     D cnt             s              3  0 inz(1)
003700021031     D cnt2            s              3  0 inz(2)
003800021031     D cnt3            s              3  0 inz(3)
003900021031     D cnt4            s              3  0 inz(4)
004000021031     D cnt5            s              3  0 inz(5)
004100021030     D ycnt            s              3  0
004200020122     D coltxt          s             37
004300090224     D name            s             65
004400020118     D coname          s             30
004500020118     D @title          s             40
004600020118     D @date           s             10
004700020118     D arr             S             40    dim(1) ctdata perrcd(1)
004800090220     D seqcntr         s              4  0
004900090220     D noMonths        s              4  0
005000090224     D partEmp         s              1
005100090224     D title           s              4
005200090224     D tmp_status      s              1
005300090224     D int_yrend       s              8  0
005400090224     D int_yrstr       s              8  0
005500140312      //
005600090224     D YREND_DATE      c                   '3112'
005700090224     D YRSTR_DATE      c                   '0101'
005800090220     D LIMIT           c                   5
005900090220     D BLANK_TEXT      c                   'GROSS '
006000090220     D MONTHSINYEAR    c                   12
006100090220     D NAMCNST         c                   'EMPLOYEE NAME        &        ADDRE-
006200090220     D                                     SS'
006300140312      //
006400950930     D                SDS
006500950930     D  @PGM             *PROC
006600140307     D  @USER                358    367
006700140312      //
006800140307       dcl-s empTin    Char(20);
006801140310       dcl-s page_tot1   Packed(13:2);
006802140310       dcl-s page_tot2   Packed(13:2);
006803140310       dcl-s days_wrk    Packed(1:0);
006804140310       dcl-s weeks_work    Packed(2:0);
006805140310       dcl-s total_days_worked_in_year  Packed(3:0);
006806140311       dcl-s linechecker   Packed(2:0) inz (0);
006900140310       Dcl-C weeksINYEAR        52;
006901140311       Dcl-C tax_Code     'VAT';
006902140307
007000021030          exsr  aainit;
007100090220
007200090225          coltxt = namcnst;
007300021030
007400090225          exsr write_heading;
007500021031
007600021030
007700021030         setll  pay_year  ytdrec;
007800021030         reade  pay_year  ytdrec;
007900021030
008000021030          exsr get_name_text;
008100021030          last_emp = emp;
008200021030          last_dept = dept;
008300021030          last_name = name;
008400090220          last_ird = irdnum;
008500090224
008600090224
008700090225          getEmpData();
008800021030
008900021030          dow not(%eof);
009000021030
009100021030             ycnt = %lookup(ytdtype:hdr_type);
009200021030
009300021030              // employee number changed
009400021030                if emp <> last_emp;
009500021030                  exsr print_details;
009600021030                  last_emp = emp;
009700021030                  last_dept = dept;
009800021030                  exsr get_name_text;
009900021031                  last_name = name;
010000090220                  last_ird = irdnum;
010100090224
010200090225                  getEmpData();
010300090224
010400021030                  clear det_amt;
010500021030                endif;
010600021031
010700021030                if ycnt <> 0;
010800021030                  det_amt(ycnt) = det_amt(ycnt) + ytdamt;
010900021030                  tot_amt(ycnt) = tot_amt(ycnt) + ytdamt;
011000021030                endif;
011100090220
011200021030            reade  pay_year  ytdrec;
011300021030          enddo;
011400021030
011500021030          exsr print_details;
011600021031          exsr write_final;
011700021030          eval *inlr = *on;
011800021030          return;
011900021030
012000021030       //----------
012100021030       // Get Name
012200021030       //----------
012300021030       begsr get_name_text;
012400090224          name = surnam;
012500090224          if   christ <> *blanks;
012600090224                   name = %trim(name) + ' ,' +
012700090224                    %triml(christ);
012800090224          endif;
012900090220
013000090224          // get title
013100090224          select;
013200090224           when sex = 'M';
013300090224             title = 'MR.';
013400090224           when sex = 'F';
013500090224             if marsta = 'M';
013600090224               title = 'MRS.';
013700090224               else;
013800090224               title = 'MS.';
013900090224             endif;
014000090224           other;
014100090224             title = 'MR.';
014200090224          endsl;
014300090224
014400090224          name = Title + ' ' + %trim(name);
014500090224
014600090224          if addone <> *blanks;
014700090224             name = %trim(name) + ' : ' + %triml(addone);
014800090224          endif;
014900090220
015000090224          if addtwo <> *blanks;
015100090224             name = %trim(name) + ',' + %triml(addtwo);
015200090224          endif;
015300090224
015400021030       endsr;
015500021031
015600021031       //---------------
015700021031       // Write Heading
015800021031       //---------------
015900021031       begsr write_heading;
016000021031         exsr reset_counters;
016001140311         dsystaxc= tax_code;
016002140311
016003140307         get_taxT_info('RD':dsystaxc:taxt_struc);
016004140307         empTin = txyreg#;
016005140307
016100021031         except  hedr;
016200021031         // Handle Multiple line heading
016300021031         dow cnt5 < xcnt;
016400021031         exsr increment_counters;
016500021031         except  hedrincr;
016600021031         enddo;
016700021031
016800021031         except  hedrnext;
016900021031       endsr;
017000021031
017100021031       //---------------
017200021031       // Write Detail
017300021031       //---------------
017400021031       begsr write_detail;
017500021031         exsr reset_counters;
017600021219          if (det_amt(cnt) + det_amt(cnt2) +
017700021219             det_amt(cnt3) + det_amt(cnt4) +
017800021219             det_amt(cnt5)) <> *zeros;
017900021219            except  detail;
018000021219          endif;
018100021031         // Handle Multiple line details
018200021031         dow cnt5 < xcnt;
018300021220          exsr increment_counters;
018400021220          if (det_amt(cnt) + det_amt(cnt2) +
018500021220             det_amt(cnt3) + det_amt(cnt4) +
018600021220             det_amt(cnt5)) <> *zeros;
018700021220           except  detail;
018800021220          endif;
018900021031         enddo;
019000021031
019100021031       endsr;
019200021031
019300021031       //-------------------
019400021031       // Write Final totals
019500021031       //-------------------
019600021031       begsr write_final;
019700021031         exsr reset_counters;
019800021031         except  final;
019900021031         // Handle Multiple line details
020000021031         dow cnt5 < xcnt;
020100021031         exsr increment_counters;
020200021031         except  final;
020300021031         enddo;
020400021031
020500021031       endsr;
020600021031
020700021031       //--------------------
020800021031       // Increment Counters
020900021031       //--------------------
021000021031       begsr increment_counters;
021100021031            cnt = cnt + 5;
021200021031            cnt2= cnt2 + 5;
021300021031            cnt3= cnt3 + 5;
021400021031            cnt4= cnt4 + 5;
021500021031            cnt5= cnt5 + 5;
021600021031       endsr;
021700021031       //----------------
021800021031       // Reset Counters
021900021031       //----------------
022000021031       begsr reset_counters;
022100021031         reset cnt;
022200021031         reset cnt2;
022300021031         reset cnt3;
022400021031         reset cnt4;
022500021031         reset cnt5;
022600021031       endsr;
022700021031
022800021030       //-----------------------
022900021030       // Print Employee datails
023000021030       //-----------------------
023100021030       begsr print_details;
023200090220
023300140311             if  *inof or linechecker = 45;
023301140311               reset linechecker;
023302140310               page_tot1 = tot_amt(cnt);
023303140310               page_tot2 = tot_amt(cnt2);
023304140311               except page_total;
023400090220                exsr write_heading;
023500090220             endif;
023600011003
023700090220             if %xfoot(det_amt) <> *zeros;
023800090220                 seqcntr += 1;
023900140311                 linechecker +=1;
024000090220                 except detailnam;
024100090220             endif;
024200090220
024300090220             exsr write_detail;
024400021030
024500021030        endsr;
024600021030      /end-free
024700021031
024800140312      //*************************************************
024900140312      // First initilisation routine for all programs  **
025000140312      //*************************************************
025001140312      /FREE
025100140312       Begsr aainit;
025200140312         coname = get_coname;
025300140312         @date = get_sys_date;
025400140312         @title = arr(1);
025500140312         //
025600140312       Endsr;
025700140312       //***********************
025800140312       // Start-up subroutine **
025900140312       //***********************
026000140312       begsr *inzsr;
026001140312      /END-FREE
026100021030     C     *entry        plist
026200021030     C                   parm                    seq
026300021030     C                   parm                    pay_year
026400021030     C                   parm                    sel
026500021030     C
026600021030      /free
026700021030         select;
026800021030           when  sel = 1 or sel = 3;
026900021030              for counter to limit;
027000021030             ycnt = %lookup(dft_list(counter):hdr_type);
027100021030
027200021030               if ycnt = 0 or ycnt = xcnt;
027300021030                   hdr_type(xcnt) = dft_list(counter);
027400021030                     if dft_list(counter) = 0;
027500021030                       evalr  hdr_text(xcnt) = blank_text;
027600021030                         else;
027700050525                        get_payty_info('RD':dft_list(counter):payty_struc);
027800021031                        evalr  hdr_text(xcnt) = %trim(ptyshrtnm);
027900021030                     endif;
028000021030                   xcnt = xcnt + 1;
028100021030               endif;
028200021030              endfor;
028300021030
028400021030           when  sel = 2;
028500050525            countr = *zeros;
028600050525            dow not get_payty_info('RD':-1:payty_struc:countr);
028700021031              evalr  hdr_text(xcnt) = %trim(ptyshrtnm);
028800021030               hdr_type(xcnt) = ptyptype;
028900021030              xcnt = xcnt + 1;
029000021030            enddo;
029100021030
029200021030         endsl;
029300021030      /end-free
029301140312      /FREE
029400140312       endsr;
029500140312       //
029501140312      /END-FREE
029600021031     Oqsysprt   E            hedr           2 04
029700021031     O                       @pgm                10
029800011004     O                       coname              72
029900011004     O                       @date              130
030000021031     O          E            hedr           2
030100021031     O                       @user               10
030200011004     O                       @title              80
030300011004     O                                          125 'Page:'
030400011004     O                       page          z    130
030500021031     O          E            hedr           2
030600011003     O                                           30 'EMPLOYEES'
030700140307     O                                           46 'CURRENT  Y.T.D.'
030800140307     O                                           68 'FIGURES  FOR  YEAR'
030900140307     O                       pay_year            73
030901140307     O                                           90 'Company T.I.N: '
030902140307     O                       EmpTIN             110
031000090220     O                                          130 'IRD. FORM 2A'
031100021030
031200021031     O          E            hedr           1
031300090224     O                                            5 'SEQ.'
031400090224     O                                           11 'T.I.N'
031500090220     O                       coltxt              57
031600151021     O*                                          90 'Weeks'
031700151021     O*                                         103 'Employment'
031800090220     O                       hdr_text(cnt)      114
031900090220     O                       hdr_text(cnt2)     132
032000090220
032100090220     O          E            hedr           1
032200090220     O                                           91 'Employed'
032300151021     O*                                         100 'Type'
032400151021     O*                                         103 'P = Partial'
032500090220
032600021031     O          E            hedrincr       1
032700090220     O                       hdr_text(cnt)      114
032800090220     O                       hdr_text(cnt2)     132
032900090220
033000021031     O          E            hedrnext       2
033100021030
033200020118     O          e            detailnam      0
033300090224     O                       seqcntr              5
033400090224     O                       last_ird            17
033500090224     O                       last_name           85
033600090224     O                       last_name           85
033700151021     O*                      noMonths      z     90
033800151021     O*                      noMonths      z     90
033801151021     O*                                          92 ':'
033802151021     O*                      days_wrk      z     95
033900151021     O*                      partEmp             97
034000020118
034100021031     O          e            detail         1
034200140312      //
034300090220     O                       det_amt(cnt)  2    114
034400090220     O                       det_amt(cnt2) 2    132
034500021030
034600140310     O          e            page_total  1
034700140312      //
034800140310     O                       page_tot1     2    114
034801140310     O                       page_tot2     2   +  1
034901140310     O          e            final       1
034902140312      //
034903140310     O                       tot_amt(cnt)  2    114
034904140310     O                       tot_amt(cnt2) 2   +  1
035000090225
035100140312      //-----------------------------------------------
035200140312      // Procedure name: getStrDate
035300140312      // Purpose:  Get start date of employee earnings
035400140312      // Returns:        1 (start date)
035500140312      // Parameter:      none
035600140312      //-----------------------------------------------
035700090225     P getEmpData      B
035800090225     D getEmpData      PI
035900090225
036000140312      //strdate         s              8  0
036100090225
036200090225      /free
036300090225
036400090225          // no. of months employed
036500090225          int_yrend = %int(yrend_date + %char(pay_year));
036600090225          int_yrstr = %int(yrstr_date + %char(pay_year));
036700140310              //if employee on still on job at the end of the reporting period
036701140312
036800090225          if resign = *zeros;
036801140310            // if employee worked for at least 1 year.
036900090225            if %date(int_yrstr:*eur) > %date(emply:*eur);
037000090225             noMonths = %diff(%date(int_yrend:*eur):
037100090225                              %date(int_yrstr:*eur) - %days(1):*months);
037200090225              else;
037201140310                // part year calculation
037300090225             noMonths = %diff(%date(int_yrend:*eur):
037400090225                              %date(emply:*eur)- %days(1):*months);
037401140310
037402140310                     total_days_worked_in_year = %diff(%date(int_yrend:*eur):
037403140310                              %date(emply:*eur) - %days(1):*days);
037404140310
037405140310
037406140311                            weeks_work = %DIV(total_days_worked_in_year:7);
037407140311                            days_wrk = %rem(total_days_worked_in_year:7);
037408140310
037500090225            endif;
037600140310          //----------------------------------------------------//
037700090225             else;
037800140310             //if employee on resigned during the reporting period
037801140310             // but started working before the year started
037900090225            if %date(int_yrstr:*eur) > %date(emply:*eur);
038000140310                             noMonths = %diff(%date(resign:*eur):
038100090225                              %date(int_yrstr:*eur) - %days(1):*months);
038101140310                    else;
038102140310
038103140310             //if employee on resigned during the reporting period
038104140310             // but started working during the reporting year
038201140310
038300140310                              noMonths = %diff(%date(resign:*eur):
038400090225                              %date(emply:*eur) - %days(1):*months);
038401140310
038402140310                         total_days_worked_in_year = %diff(%date(resign:*eur):
038403140310                              %date(emply:*eur) - %days(1):*days);
038404140310
038405140310
038406140311                            weeks_work = %DIV(total_days_worked_in_year:7);
038407140311                            days_wrk = %rem(total_days_worked_in_year:7);
038408140310
038500090225            endif;
038600090225          endif;
038700090225
038800090225          if  noMonths >= MONTHSINYEAR;
038900140310             noMonths = weeksINYEAR;
039000140310             partEmp ='W';
039001140310               days_wrk = 0;
039100090225            else;
039200090225             partEmp = 'P';
039201140310             noMonths = weeks_work;
039202140310
039300090225          endif;
039400090225
039500090225      /end-free
039600090225     P getEmpData      E
039700011003**
039800011003 Payroll Earnings Year-to-Date Report
