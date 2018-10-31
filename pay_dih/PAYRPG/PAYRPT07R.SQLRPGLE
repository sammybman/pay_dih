000100010910     H DECEDIT('.') DATEDIT(*ymd/)
000200170301     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR') alwnull(*usrctl)
000300111002     H/define profoundui
000400180226      //*
000500180226      //* redone by :- C.WILLIAMS  & S.BRITTON
000600180226      //*
000700111002     Fempmast   if   e           k disk    usropn
000800111002     Fpayhisl10 if   e           k disk    usropn
000900111002     Fpaytranl05if   e           k disk    usropn
001000010924     Fpayrpt07d cf   e             workstn
001100111002     F/IF DEFINED(WEB)
001200111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001300111002     F/ENDIF
001400040207     Fqsysprt   o    f  132        printer oflind(*inof)
001500180226      //
001600111002      /copy QCPYSRC,genpr
001700111002      /copy QCPYSRC,PAYPR
001800111002      /copy QCPYSRC,autpr
001900180226      //
002000010924     D arr             S             40    DIM(3) CTDATA PERRCD(1)
002100180226      //
002200010924     Dtax_type         s                   like(ptrntcode)inz(61)
002300010924     Dunion_type       s                   like(ptrntcode)inz(65)
002400010924     Dtran_type        s                   like(ptrntcode)
002500180226      //
002600010924     Dlast_emp         s                   like(phisemp)
002700020107     Dlast_type        s                   like(phistype)
002800180226      //
002900010924     Dstart_date       s                   like(phispdate)
003000010924     Dend_date         s                   like(phispdate)
003100180226      //
003200010924     Dtotal_ded        s                   like(phisgpay)
003300010924     Dgross_inc        s                   like(phisgpay)
003400010924     Dall_total        s                   like(phisgpay)
003500180226      //
003600180226      //
003700010910     Drec_err          c                   'No Payroll Found For this Date'
003800010910     Dnorec_err        c                   'No Record Was Found for The -
003900010910     d                                      Criteria Entered'
004000020107
004100020107     Dindptr           s               *   inz(%addr(*in))
004200020107     D                 ds                  based(indptr)
004300020107     Dend_program              3      3n
004400020107
004500950930     D                SDS
004600950930     D  @PGM             *PROC
004700111002     D  @USER                358    367
004800180226      //
004900180221     D write_Sts       s              1    inz('M')
004901180221     D
004902180221     D emp_name        s             40
005000010910     D rep_title       s             40
005100180226      //
005200011001     Dlast_date        s                   like(phisndate)
005300011001     Dlast_run_date    s                   like(phisndate)
005400180226      //
005401180221         dcl-s empTin    Char(20);
005402180221       dcl-s page_tot1   Packed(13:2);
005403180221       dcl-s page_tot2   Packed(13:2);
005404180221       dcl-s days_wrk    Packed(1:0);
005405180221       dcl-s weeks_work    Packed(2:0);
005406180221       dcl-s total_days_worked_in_year  Packed(3:0);
005407180221       dcl-s linechecker   Packed(2:0) inz (0);
005408180221       Dcl-C weeksINYEAR        52;
005409180221       Dcl-C tax_Code     'VAT';
005410180221       Dcl-C NOTFOUND     '02000';
005411180226      //
005412180221       Dcl-s address    char(80) INZ(*BLANK);
005413180221       // // Purpose: taxable value
005414180221       DCL-S gross PACKED(13:2) INZ(0);
005415180221       DCL-c month_year_code '*M';
005500180221
005507180221
005508180221        DCL-S onames CHAR(30) INZ(*BLANK);
005509180221       //
005510180221       DCL-S month_year PACKED(4:0) INZ(0);
005511180221       //
005512180221       DCL-S salary PACKED(10:0) INZ(0);
005513180221       //
005514180221       DCL-S coemp PACKED(6:0) INZ(0);
005515180221       //
005516180221       //
005517180221       DCL-S payfreq CHAR(10) INZ(*BLANK);
005518180221       //
005519180221       DCL-S coname CHAR(30) INZ(*BLANK);
005520180221       //
005521180221       DCL-S total_deduct PACKED(12:0) INZ(0);
005522180221       //
005523180221       DCL-S statutary_ded LIKE(total_deduct) INZ(0);
005524180221       //
005525180221       DCL-S other_inc LIKE(other_all) INZ(0);
005526180221       //
005527180221       DCL-S total_paid LIKE(statutary_ded) INZ(0);
005528180221       //
005529180221       DCL-S empdob CHAR(10) INZ(*BLANK);
005530180221       //
005531180221       //
005532180221       DCL-S coaddress VARCHAR(90) INZ(*BLANK);
005533180221       DCL-S total_NIS PACKED(12:0) INZ(0);
005534180221       //
005535180221       //
005536180221       DCL-S prvtin CHAR(25) INZ(*BLANK);
005537180221       DCL-S prevename CHAR(30) INZ(*BLANK);
005538180221       //
005539180221       DCL-S prveaddres VARCHAR(90) INZ(*BLANK);
005540180221       //
005541180221       DCL-S coname# LIKE(coname) INZ(*BLANK);
005542180221       DCL-DS CompanyDataDS INZ LIKEDS(companyInfoDS);
005548180221
005549180226       //
005550180226       DCL-S total PACKED(4:0) INZ(0);
005551180226
005600010910     C     tran_key      klist
005700011001     C                   kfld                    phisndate
005800010925     C                   kfld                    phistype
005900010924     C                   kfld                    tran_type
006000010925     C                   kfld                    phisemp
006100010910
006200180226
006300111002         // authority check
006400111002         monitor;
006500111002           authority_check('PAY':@pgm);
006600111002         on-error;
006700111002           show_msg(get_err_msg('DIH0054'));
006800111002           *inlr = *on;
006900111002           return;
007000111002         endmon;
007100180226
007200180226
007300111002      /copy QCPYSRC,aainit
007400180226       dow 0 = 0;
007500180226         exfmt fmt1;
007600010908
007601180226
007700111002      /copy QCPYSRC,exit_copy
007800010913
007801180226
007900180226         chain pay_prno phisfmt;
008000180226         if not %found;
008100180226           *in28 = '1';
008200180226           errmsg = norec_err;
008300180226           iter;
008400180226         endif;
008500010913
008600180226         select;
008700180226         when rep_sel = 1;
008800180226           rep_title = %trim(arr(2));
008900180226           tran_type = tax_type;
009000180226           *in99 = *on;
009001180226
009002180221              resetGRARecords();
009109180221
009110180226
009200180226         when rep_sel = 2;
009300180226           rep_title = %trim(arr(3));
009400180226           tran_type = union_type;
009500180226           *in99 = *off;
009600180226         other;
009700180226           iter;
009800180226         endsl;
009900010924
010000180226         get_prd_info('*rng':*blanks:pay_prno:
010100180226             start_date:end_date);
010101180226
010102180221           coname = get_coname(*omit:CompanyDATADS);
010103180226
010200180226         leave;
010300180226       enddo;
010400180226
010500010911
010600180226       except hedr;
010700010911
010800180226          // start of pgm loop
010900011001
011000180226       chain pay_prno phisfmt;
011100011001
011200180226          // save first emp found
011300010924
011400180226       dow not(%eof(payhisl10));
011500180226         gross_inc = gross_inc + phisgpay;
011600010910
011700180226         if phisemp = last_emp and
011800180226               phisndate <> last_date
011900180226               or phisemp <> last_emp
012000180226               or phisemp = last_emp and
012100180226               phisndate = last_date  and phistype <>
012200180226               last_type;
012300180226           //
012400180226           setll tran_key ptisfmt;
012500180226           reade tran_key ptisfmt;
012600180226           dow not (%eof(paytranl05));
012700180226             exsr summ;
012800180226             reade tran_key ptisfmt;
012900180226           enddo;
013000180226           //
013100180226           if *inof;
013200180226             except hedr;
013300180226           endif;
013400011001
013500180226           last_emp = phisemp;
013600180226           last_type = phistype;
013700180226           last_date = phisndate;
013800180226           last_run_date = phisrdate;
013900180226         endif;
014000010924
014100180226         reade pay_prno phisfmt;
014200180226           //
014300180226         if phisemp <> last_emp;
014400010925
014500010924
014600180226           if total_ded >0;
014700180226             get_emp_info(last_emp:*omit
014800180226                 :*blanks:emp_name);
014900180226             chain last_emp emprec;
014901180226              total += 1;
015001180221            read_Employee_info(last_emp);
015002180221               getTaxAllowance();
015003180221              writeGraDetailsPrc(write_StS);
015100180226             except detail;
015200180226           endif;
015300010924
015400180226           gross_inc = 0;
015500180226           total_ded = 0;
015600180226         endif;
015700010924
015800180226       enddo;
015900010924
016000180226       *inlr = '1';
016102180221
016103180221
016105180221                    seqcntr = 9999;
016106180221           fname = coname;
016107180221           lname = ' ';
016108180221                       month_year = pay_year;
016109180221           year_month_char = %char(month_year);
016110180221           NOCORRECT  = *BLANKS;
016111180221           SALARY = 0;
016112180221           empdob = ' ';
016113180221           coemp = 0;
016114180221           title = ' ';
016115180221           payfreq = ' ';
016116180221           reset onameS;
016117180221           reset monthSalary ;
016118180221           reset other_inc;
016119180221           reset other_all;
016120180221           reset  prvtin ;
016121180221           reset prevename;
016122180221           reset prveaddres;
016123180221           comptin = companyDATADS.TaxReg#;
016124180221           coaddress =  %TRIM(companyDATADS.ADDRESS1) + ' '+
016125180221                %TRIM(companyDATADS.ADDRESS2) + ' '
016126180221                       + %TRIM(companyDATADS.ADDRESS3);
016127180221
016128180221                      writeGraDetailsPrc(write_StS);
016200180221                        except    fin_subtot;
016300180221
016400111002
016500111002         //*****************************
016600111002         // Session Init
016700111002         //*****************************
016800111002
016900111002          begsr *inzsr;
017000111006            get_session_info('PAY':errmsg:@pgm);
017100111002            if errmsg <> *blanks;
017200111002              show_msg(errmsg);
017300111002              *inlr = *on;
017400111002              return;
017500111002            endif;
017600111002
017700111002            open(e) empmast;
017800111002            open(e) payhisl10;
017900111002            open(e) paytranl05;
018000111002
018100111002          endsr;
018300180226      //****************************************
018400180226      //get total amount paid by each employee
018500180226      //****************************************
018600180226       begsr summ;
018700180226         //
018800020107
018900180226         total_ded = total_ded + ptrnamt;
019000180226         all_total = all_total + ptrnamt;
019100180226         //
019200180226       endsr;
019300180226       //*************************************************
019400180226       //
019401180226      /END-FREE
019500040207     Oqsysprt   E            HEDR           1 03
019600950930     O                       @PGM                10
019700010908     O                       coname              72
019800010912     O                       @date              132
019900950812     O          E            HEDR           1
020000010908     O                       @USER               10
020100010924     O                       rep_title           72
020200010924     O                                           73 '-'
020300010924     O                       end_date      Y     83
020400950513     O                                          127 'PAGE'
020500950513     O                       PAGE          Z    132
020600080111
020700080111     O          E            HEDR           1
020800010925     O                                           11 'No.'
020900010924     O                                           29 'Employee Name'
021000080111     O               99                          75 'T.I.N. /'
021100010925     O               99                          97 'Gross'
021200010924     O                                          118 'Deducted'
021300080111     O          E            HEDR           2
021400080111     O               99                          75 'I.R.D.#'
021500080111
021600010913     O          e            DETAIL         2
021700010925     O                       last_emp      Z     11
021800010924     O                       emp_name       B    56
021900080111     O               99      irdnum         b    75
022000010925     O               99      gross_inc     1     97
022100010924     O                       total_ded     1    118
022200010924     O          e            fin_subtot  1  1
022300950513     O                                           33 'TOTAL'
022400950513     O                                           49 '--'
022500010925     O               99      total         2   +  2
022600010925     O                       all_total     2    118
022700010911     O          e            fin_subtot  1
022800950513     O                                           77 '**  END  OF  REPORT  **'
022900180221
022901180221       // --------------------------------------------------
022902180221       // Procedure name: writeGraDetailsPrc
022903180221       // Purpose:
022904180221       // Returns:
022905180221       // Parameter:      GraStatusCode
022906180221       // --------------------------------------------------
022907180221       DCL-PROC writeGraDetailsPrc ;
022908180221         DCL-PI *N;
022909180221           GraStatusCode CHAR(1);
022910180221         END-PI ;
022911180221
022912180221         exec sql
022913180221               insert into gradetail (grasts,graseq,gratin,granprefix,grafname,
022914180221                 gralname,graaddress,graweeks#,gradays,graweekp,gragross,
022915180221                 granis,grapaye,gratotpaid,gradob,gratimed,grayear,nocorrect,
022916180221                 graemp,grapayfreq,graonames,grasalary,otherinc,otherall,
022917180221                 grastsded,gracotin,graconame,gracoaddr,graprvctin,graprvcnm,
022918180221                 graprvcadd,totdeduct)
022919180221           values(GraStatusCode,:seqcntr,:irdnum,:title,:fname,:lname,:address
022920180221               ,:noMonths,:days_wrk,:partEmp,:gross_inc,:graNIS,:total_ded
022921180221                ,:total_paid
022922180221                 ,:empdob,CURRENT_TIMESTAMP,:pay_year,:NOCORRECT,
022923180221               :last_emp,:payfreq,:onameS,:monthSalary,:other_inc,:other_all
022924180221               ,:statutary_ded,:comptin,:coname#,:coaddress,:prvtin
022925180221               ,:prevename,:prveaddres,:total_deduct)
022926180221
022927180221                with nc;
022928180221
022929180221         return ;
022930180221       END-PROC ;
022931180221        // --------------------------------------------------
022932180221       // Procedure name: read_Employee_info
022933180221       // Purpose:
022934180221       // Returns:
022935180221       // Parameter:      employee_number
022936180221       // --------------------------------------------------
022937180221       DCL-PROC read_Employee_info ;
022938180221         DCL-PI *N;
022939180221           employee_number PACKED(6:0);
022940180221         END-PI ;
022941180221       //
022942180221              get_emp_info2('R':employee_number:GRA_EMP_STRUCDS);
022943180221
022944180221
022945180221                otherinc = 0;
022946180221                otherall = 0;
022947180221                nocorrect = 'N';
022948180221
022949180221
022950180221         return ;
022951180221       END-PROC ;
022952180221
022953180221       // --------------------------------------------------
022954180221       // Procedure name: getGRASummary
022955180221       // Purpose:        get summarised data fro GRADETAIL table
022956180221       // Returns:
022957180221       // --------------------------------------------------
022958180221       DCL-PROC getTaxAllowance ;
022959180221         exec sql
022960180221               select PTXFREE into :statutary_ded from paytaxctl where PTXCYC =
022961180221                  :MONTH_year_CODE;
022962180221       //  exec sql
022963180221       //    select sum(grapaye) into :sum_of_tax from gradetail where grayear =
022964180221       //           :pay_year;
022965180221       //
022966180221         return ;
022967180221       END-PROC ;
022968180221
022969180221       // --------------------------------------------------
022970180221       // Procedure name: resetGRARecords
022971180221       // Purpose:
022972180221       // Returns:
022973180221       // --------------------------------------------------
022974180221       DCL-PROC resetGRARecords ;
022975180221       //
022976180221       DCL-S record_cnt PACKED(5:0) INZ(0);
022977180221              reset record_cnt;
022978180221             exec sql
022979180221               select count(*) into :record_cnt from gradetail where grayear =
022980180221                  :pay_year and grasts = 'M';
022981180221            // if records already exist for the reporting year,
022982180221            // delete them and re-write them.
022983180221              if record_cnt > 0 or sqlstt <> '02000';
022984180221              exec sql  delete from gradetail where grayear = :pay_year
022985180221                 and grasts = 'M' with nc;
022986180221               endif;
022987180221
022988180221         return ;
022998180221       END-PROC ;
022999180221**
023000020107     Tax & Union Monthly Report Listing
023100020107TAX Monthly Listing For Period Ending
023200010924Union Monthly Listing For Period Ending
