000100170209     H DECEDIT('.') DATEDIT(*ymd/) bnddir('QC2LE')  alwnull(*usrctl)
000200000000      //*
000300000000      //* EMPLOYEES PAYROLL
000400000000      //* PROGRAMMERS---C.Williams ,S.BRITTON
000500000000      //*
000600000000     Fempmast   ip   e           k disk
000700000000     Fpayeytdl01uf a e           k disk    commit
000800000000     Fpaybank   if   e           k disk
000900000000     Fpaydiv    if   e           k disk
001000000000     Fpaycon    if   e             disk    usropn
001100040621     Fpayrresl01if   e           k disk
001200000000     Fpayrtypl01if   e           k disk
001300000000     Fpayrdefl01if   e           k disk
001400000000     Fpaycomm   if   e           k disk
001500000000     Fpaysuspl01uf   e           k disk    commit
001600150817
001700000000     Fpayhis    o    e             disk    commit
001800000000     Fpaytrans  o    e             disk    commit
001900080221     F*paynrdl01 o    e             disk    commit
002000000000
002100000000     Fpayslipf  o    e             printer usropn
002200000000     Fpayslipfl o    e             printer usropn
002300070315
002400111004      /copy QCPYSRC,genPR
002500000000      // Payroll PROTOTYPES
002600111004      /copy QCPYSRC,PAYPR
002700111004      /Copy QCPYSRC,CsortPr
002800111004      /copy QCPYSRC,empPR
002900041206
003000041206     D GetAllYtd       pr
003100000000      // Prototypes for compare routines for sort
003200000000
003300000000     D SeqStruc        Pr            10I 0
003400000000     D  Element1@                      *   Value
003500000000     D  Element2@                      *   Value
003600150824
003700000000     Dpay_rec          ds                  occurs(50) based(struc@)
003800000000     D pay_prior                      2  0
003900000000     D pay_type                       2  0
004000000000     D pay_tax                        1
004100000000     D pay_code                       1
004200000000     D pay_hrs                        5  2
004300000000     D pay_amt                       11  2
004400070315
004500041206     Dpay_recP         ds                  occurs(99)
004600000000     D pay_codeP                      1
004700000000     D pay_descP                     25
004800000000     D pay_hrsP                       5  2
004900000000     D pay_amtP                      11  2
005000000000     D pay_shdsP                      9
005100000000     D pay_ytdP                      11  2
005200070315
005300000000     D                 ds
005400000000     D  pcdftytdDS                   10
005500000000     D dft_list                       2  0 dim(5) overlay(pcdftytdDS)
005600070315
005700000000     dpy_detailH       ds
005800000000     d  pay001                       10
005900000000     d  pay002                       10
006000000000     d  pay003                       10
006100000000     d  pay004                       10
006200000000     d  pay005                       10
006300000000     d  pay006                       10
006400000000     d  pay007                       10
006500000000     d  pay008                       10
006600000000     d  pay009                       10
006700070315
006800000000     dytd_detail       ds
006900000000     d  ytd001                       14
007000000000     d  ytd002                       14
007100000000     d  ytd003                       14
007200000000     d  ytd004                       14
007300000000     d  ytd005                       14
007400000000
007500000000     d ytdptr          s               *   inz(%addr(ytd_detail))
007600000000     d recptr          s               *   inz(%addr(py_detailH))
007700000000     d pprtArr         s             10    dim(09) based(recptr)
007800000000     d prate           s             10    dim(09)
007900000000     d pAmount         s             10    dim(09)
008000000000     d pytd            s             10    dim(09)
008100000000     d YprtArr         s             14    dim(05) based(ytdptr)
008200000000     d YAmount         s             14    dim(05)
008300041206     d paytypeArr      s              2  0 dim(99)
008400070315
008500000000     D strucMax        S              3P 0 Inz(50)
008600000000     D Memory          S              5P 0
008700000000     D xcnt            s              3  0 inz(0)
008800000000     D ycnt            s              3  0 inz(1)
008900040621     D total_gross     s             11  2
009000040621     D Max_Gross       s             11  2
009100000000     d typ_total       s             11  2
009200100312     d susperc#        s                   like(susperc)
009300070315
009400000000     D coname          s             30
009500000000     D @date           s             10
009600070315
009700000000     D totduc          s                   like(total_gross)
009800000000     D tax_gross       s                   like(total_gross)
009900000000     D actear          s                   like(total_gross)
010000000000     D insern          s                   like(total_gross)
010100000000     D com_contrib     s                   like(total_gross)
010200150817     D nis_deducted    s                   like(total_gross)
010300000000     D net             s                   like(total_gross)
010400000000     D trn_check       s              2  0
010500000000     D nrhrs           s              5  2
010600000000     D othrs           s              5  2
010700000000     D pay_codeHld     s              1
010800000000     D payroll_type    s              1
010900000000     D mimic_type      s              1
011000000000     D py_style        s              1  0
011100000000     D ytd_opt         s              1  0
011200000000     D rate_flag       s              1
011300000000     D bank_rtype      s              1
011400131029     D cat_flag        s              1    inz(' ')
011500000000     D origin          s              3    inz('PAY')
011600000000     D datefield       s               d   datfmt(*iso)                         TOTAL AMOUNT
011700000000     D payroll_date    s              8  0
011800000000     D system_date     s              8  0
011900000000     d saved_empcnt    s                   like(ycnt)
012000000000     D counter         s              3  0
012100000000     d HoursFound      s               n
012200050525     D countr          s              5  0
012300070315
012400000000     D gross_text      c                   'Total Gross'
012500000000     D deduc_text      c                   'Total Deductions'
012600000000     d rate_cnst       c                   'Hrs@Rate'
012700000000     d net_cnst        c                   'Net amount'
012800000000     d emol_cnst       c                   'Emoluments'
012900000000     d ded_cnst        c                   'Deductions'
013000000000     d ytd_cnst        c                   'Y.T.D.'
013100000000     D tax_code        c                   61
013200000000     D nis_code        c                   63
013300000000     d one             c                   1
013400000000     d fld_count       c                   9
013500000000     D limit           c                   5
013600170420     D blank_text      c                   'Tax.Gross'
013700000000
013800000000      // total-fields definition
013900000000
014000000000     D indptr          s               *   inz(%addr(*in))
014100070315
014200000000     D                 ds                  based(indptr)
014300000000     D ytd_notfound            1      1n
014400070315
014500000000     Iemprec
014600000000     I                                          dept          L2
014700000000
014800000000      // Set up initial memory allocation for Struc of StrucMax (50) elements
014900000000       Memory = %Size(pay_rec) * StrucMax;
015000000000     C                   Alloc     Memory        Struc@
015100150824
015200000000
015300000000      // Key to verify transaction to Process
015400000000     C     trn_key       klist
015500000000     C                   kfld                    payroll_type
015600000000     C                   kfld                    trn_check
015700000000      // Key for YTD
015800000000     C     ytd_key       klist
015900000000     C                   kfld                    emp
016000000000     C                   kfld                    prtyyear
016100000000     C                   kfld                    pay_type
016200000000     C                   kfld                    origin
016300000000
016400000000      //-------------------------------------------------------------------------
016500000000
016600000000      // Get PayPeriod Number
016700000000       chain payroll_type prtyfmt;
016800000000       if prtyprno = prtyproll#;
016900000000         prtyproll# = 0;
017000040621         prtyyear += 1;
017100000000       endif;
017200000000
017300000000       // check for suspended payment
017400040204       chain (emp:payroll_type)  susfmt;
017500070919       if  %found  and  susedate >= prtyndate
017600070919             and  sussdate <= prtyndate;
017700100315
017800100315          // using standard precord processing
017900100312         susperc# = susperc;
018000100315
018100000000         susprd = susprd - 1;
018200100317
018300000000         if susprd <= *zeros;
018400000000           delete susfmt;
018500000000         else;
018600000000           update susfmt;
018700000000         endif;
018800100317
018900000000       else;
019000100312         // if no suspend rec found, clear suspend percent field
019100100312         susperc# = *zeros;
019200100312       endif;
019300100317
019400000000         // Get Division and Bank account if found
019500000000         if mimic_type <> *blank;
019600000000           bank_rtype = mimic_type;
019700000000         else;
019800000000           bank_rtype = payroll_type;
019900000000         endif;
020000000000         //
020100040204         //  get employee bank account
020200040204         chain (emp:bank_rtype)  pbfmt;
020300000000
020400000000         // Get Employee Name
020500000000         emp_name = %trim(christ) +
020600000000             ' ' + %triml(surnam);
020700000000         //-------------------------------------------------------------------------
020800000000
020900000000         reset xcnt;
021000150824
021100030604     C                   clear     *all          pay_Rec
021200150824
021300030604         // clear *all pay_Rec;
021400041206         clear paytypeArr;
021500030604         clear *all pay_Recp;
021600000000         clear totduc;
021700000000         clear total_gross;
021800000000         clear tax_gross;
021900150824         clear nis_deducted;
022000041206
022100041206         //**********************************************
022200000000         //* GET EMPLOYEE TRANSACTIONS ***
022300041206         //**********************************************
022400150824
022500000000         if get_emp_trans(emp:%addr(pay_Rec):
022600000000               xcnt:totduc:tax_gross:
022700100312               total_gross:susperc#);
022800000000
022900041206           //********************************************                   *
023000041206           //* P.A.Y.E. CALCULATIONS  ROUTINE. ***
023100041206           //********************************************                   *
023200150817
023300150817           // get nis TO be deducted and subtract from taxable gross
023400150817           // when flag is set
023500150817           if pcnistaxo = 'Y';
023600150817             trn_check = nis_code;
023700150817             chain trn_key prdffmt;
023800150817             if %found and prdfflag = 'Y';
023900150817                 actear = tax_gross;
024000150817                 nis_deducted = calc_emp_nis(actear:insern:totduc:
024100150817                      com_contrib:birth:prtycycode:prtycycqty:
024200150817                      %addr(pay_rec):xcnt:emp:cat_flag:'N');
024300150817             endif;
024400150817            else;
024500150817             nis_deducted = *zeros;
024600150817           endif;
024700150817
024800000000           trn_check = tax_code;
024900000000           chain trn_key prdffmt;
025000000000           if %found and prdfflag = 'Y';
025100000000
025200180130           //calc_emp_tax(emp:frpay:(tax_gross - nis_deducted)
025300180130             calc_emp_tax(emp:frpay:tax_gross
025400000000                 :totduc:prtycycode
025500000000                 :prtyproll#:prtyyear
025600000000                 :prtycycqty:status
025700180130                 :%addr(pay_rec):xcnt:emply:bank_rtype
025800180130                 :nis_deducted);
025900000000           endif;
026000150814
026100000000           //****************************************************************
026200000000           //* N.I.S. CALCULATIONS ***
026300000000           //****************************************************************
026400000000           trn_check = nis_code;
026500000000           chain trn_key prdffmt;
026600000000           if %found and prdfflag = 'Y';
026700000000             if tax_gross > *zero;
026800000000               actear = tax_gross;
026900000000               calc_emp_nis(actear:insern:totduc:
027000000000                   com_contrib:birth:prtycycode:
027100000000                   prtycycqty:
027200131029                   %addr(pay_rec):xcnt:emp:cat_flag);
027300080212
027400000000             endif;
027500040622
027600040622            // Get payroll restrictions/parameters
027700040622            chain payroll_type prrefmt;
027800040622
027900040622             //* Declare earnings as being X.XX % more than insurable earnings
028000040622             if actear > insern
028100040622               and prredeclr > *zeros;
028200040622              actear = insern + ((prredeclr * insern) / 100);
028300000000             endif;
028400000000
028500000000           endif;
028600040622
028700150814           //****************************************************************
028800040622
028900040621          //   net = total_gross - totduc;
029000040622
029100040622           // Handle payroll guaranteed percentage etc.
029200040621           select;
029300040621
029400040621           when prreguar > 0;
029500040621              Max_Gross = round_up(total_gross *
029600040621                          (100 - prreguar) /100:'A');
029700040621              net = Max_gross - totduc;
029800040621            if (%abs(Max_gross) - %abs(totduc)) <
029900040621                 *zeros;
030000040621              adj_pay_credit(net:totduc:
030100040621                  %addr(pay_rec):xcnt);
030200040621            endif;
030300040621              net += total_gross - Max_gross;
030400040621
030500040621           other;
030600040621            net = total_gross - totduc;
030700040621            if (%abs(total_gross) - %abs(totduc)) <
030800040621                 *zeros;
030900040621              adj_pay_credit(net:totduc:
031000040621                  %addr(pay_rec):xcnt);
031100040621            endif;
031200040621           endsl;
031300040621
031400000000           //* Summary of employee transactions
031500000000           exsr sum_trans;
031600150824
031700000000           //* Figures
031800150824
031900000000           phisemp = emp;
032000000000           phisdept = dept;
032100000000           phisdiv = divis;
032200000000           phisloc = locat;
032300100218           phisgpay = round_up(total_gross:'A');
032400100218           phisaearn = round_up(actear:'A');
032500100218           phisnpay = round_up(net:'A');
032600070315
032700000000           phisfnight = prtyproll# + 1;
032800070315
032900100218           phisiearn = round_up(insern:'A');
033000000000           phissts = 'A';
033100000000           phisrdate = system_date;
033200000000           phispdate = payroll_date;
033300000000           phisndate = prtyndate;
033400131029           phiswcate = cat_flag;
033500000000           get_prd_info('*prd':*blanks:phisprno:
033600000000               prtyndate);
033700150205
033800150205           // adjust period for year-end change
033900150205           dow prtyyear > %int(%subst(%char(phisprno):1:4));
034000150205               phisprno = get_new_prd(phisprno:1:'A');
034100150205           enddo;
034200150205
034300000000           // Write Correct run type for pay history file
034400000000           phistype = bank_rtype;
034500070315
034600000000           write phisfmt;
034700000000           clear phisfmt;
034800000000
034900000000           // Write /Update YTD record
035000000000           //       Taxable Gross (pay_type = 0)
035100000000           pay_type = *zeros;
035200000000           ytdamt = *zeros;
035300000000           chain ytd_key ytdrec;
035400000000           *IN01 = NOT %FOUND;
035500150824
035600180131         //ytdamt += round_up((tax_gross - nis_deducted):'A');
035700180131           ytdamt += round_up((tax_gross ):'A');
035800150824           if ytd_notfound;
035900000000             year = prtyyear;
036000000000             ytdtype = pay_type;
036100000000             ytdorg = origin;
036200000000             write ytdrec;
036300000000           else;
036400000000             update ytdrec;
036500000000           endif;
036600000000
036700000000           // Print Paycheck
036800000000        select;
036900000000         when py_style = 1;
037000041206            if pcallytd = 'Y';
037100041206               getallytd();
037200041206            endif;
037300000000           exsr prnt_horizontal;
037400000000         when py_style = 2;
037500041206            if pcallytd = 'Y'
037600041206              and  ytd_opt = 3;
037700041206               getallytd();
037800041206            endif;
037900000000           exsr prnt_vertical;
038000000000         endsl;
038100041206
038200030603        // Clear fields used for ytd files  (temp work)
038300000000           actear = *zeros;
038400000000           insern = *zeros;
038500000000           clear ytdrec;
038600000000
038700000000           // end of valid transaction
038800000000         endif;
038900150814
039000000000         // end of valid selection - suspended payments
039100100312         //endif;
039200000000       //*********************************************************************
039300000000       // Payroll Output
039400000000       // output Payroll Info. to History/Y.T.D. files
039500000000       //*********************************************************************
039600000000       begsr sum_trans;
039700000000         if xcnt <> *zeros;
039800150824
039900000000           reset ycnt;
040000000000           dow ycnt <= xcnt;
040100000000      /END-FREE
040200000000     C     ycnt          occur     pay_Rec
040300000000     C     ycnt          occur     pay_Recp
040400000000      /FREE
040500050525            get_payty_info('RD':pay_type:payty_struc);
040600000000             // save values for payslip output
040700000000             pay_codeP = pay_code;
040800000000             pay_hrsP = pay_hrs;
040900000000             pay_amtP = pay_amt;
041000000000             pay_descP = ptydesc;
041100000000             pay_shdsP = ptyshrtnm;
041200041206
041300041206             paytypeArr(ycnt) = pay_type;
041400000000
041500000000             // if is emoluments
041600000000             if pay_code = 'E';
041700000000               select;
041800000000                 // Overtime
041900000000               when pay_type = 2;
042000000000                 phisothrs = phisothrs + pay_hrs;
042100000000                 // Normal time
042200000000               other;
042300000000                 phisnthrs = phisnthrs + pay_hrs;
042400000000               endsl;
042500000000
042600000000             endif;
042700000000             // Write to transaction History File
042800000000             ptrnsts = 'A';
042900000000             ptrnemp = emp;
043000000000             ptrnrdate = system_date;
043100000000             ptrnpdate = payroll_date;
043200000000             ptrnndate = prtyndate;
043300000000             ptrnhrs  = pay_hrs;
043400100218             ptrnamt  = ROUND_UP(pay_amt:'A');
043500000000             // Write Correct run type for pay transaction file
043600000000             ptrntype = bank_rtype;
043700150824
043800000000             ptrncode = pay_code;
043900000000             ptrntcode = pay_type;
044000131029             ptrnwcate = cat_flag;
044100000000             write ptisfmt;
044200000000
044300000000             ytdamt = *zeros;
044400000000             chain ytd_key ytdrec;
044500000000             *IN01 = NOT %FOUND;
044600000000             // Write /Update YTD record
044700000000             ytdamt = ytdamt + pay_amt;
044800100218             ytdamt = round_up(ytdamt:'A');
044900000000             if ytd_notfound;
045000000000               year = prtyyear;
045100000000               ytdtype = pay_type;
045200000000               ytdorg = origin;
045300000000               write ytdrec;
045400000000             else;
045500000000               update ytdrec;
045600000000             endif;
045700000000
045800000000             pay_ytdP = ytdamt;
045900000000             ycnt = ycnt + 1;
046000000000           enddo;
046100150824
046200000000         endif;
046300000000
046400000000       endsr;
046500000000       //*********************************************************************
046600000000       // Payroll Output
046700000000       // output Payroll Info. Printer File
046800000000       //*********************************************************************
046900000000       begsr prnt_vertical;
047000000000
047100000000           if  xcnt <> *zeros;
047200000000          // Sort data structure
047300000000            %occur(pay_recP) = 1;
047400000000            SortIt(%Addr(pay_recP) : xcnt :
047500000000                             %Size(pay_RecP) : %PAddr('SEQSTRUC'));
047600000000        // Get Bank name from division file for Payslip
047700000000          chain  divis divfmt;
047800000000          bank_name = %trim(dtname) + ' :';
047900000000          prtyndate# = %date(prtyndate) - %days(1);
048000000000        // employee  header record
048100000000                 write  header;
048200000000
048300000000           reset ycnt;
048400000000           pay_codeHld = 'E';
048500000000           dow  ycnt <= xcnt;
048600000000            %occur(pay_recP) = ycnt;
048700000000
048800000000        // if Paycode changes (E= emoluments,D= deductions)
048900000000               if   pay_codeP <> pay_codeHld;
049000000000                   sub_text = gross_text;
049100000000                   sub_tot  = total_gross;
049200000000        // employee transaction summary total for Emoluments
049300000000                   write  sub_tot_rc;
049400000000                   pay_codeHld = pay_codeP;
049500000000                endif;
049600000000
049700000000        // employee Hourly Rate
049800000000            exsr get_hoursRate;
049900000000
050000000000             pay_amt =  pay_amtP;
050100000000            if ytd_opt = 3;
050200000000             ytd_amt =  pay_ytdP;
050300000000             ptyshnm =  pay_shdsP;
050400000000             write  det_recd2;
050500000000              else;
050600000000             ptydesc =  pay_descP;
050700000000             write  det_recd;
050800000000            endif;
050900000000
051000000000             ycnt = ycnt + 1;
051100000000          enddo;
051200000000
051300000000             if  totduc <> *zeros;
051400000000                sub_text = deduc_text;
051500000000                sub_tot  = totduc;
051600000000          // employee transaction summary total for DEDUCTIONS
051700000000                write  sub_tot_rc;
051800000000              endif;
051900000000
052000000000          // final employee record
052100000000            write  fintotal;
052200000000          // employee Year-to-date Header record
052300000000            if ytd_opt <> 3;
052400000000            write  ytdrcd;
052500000000
052600000000           clear YPrtArr;
052700000000           clear Yamount;
052800000000           saved_empcnt = *zeros;
052900000000           select;
053000000000            when  ytd_opt = 1;
053100000000              for counter = 1 to limit;
053200000000
053300000000                     if dft_list(counter) = 0;
053400000000                       if  counter <> 1;
053500000000                         iter;
053600000000                       endif;
053700000000                       eval  ptyshrtnm = blank_text;
053800000000                         else;
053900050525                     get_payty_info('RD':dft_list(counter):payty_struc);
054000000000                     endif;
054100000000                        pay_type = dft_list(counter);
054200000000                        ytdamt = *zeros;
054300000000                        chain ytd_key ytdrec;
054400000000                     if  %found(payeytdl01);
054500000000                         saved_empcnt = saved_empcnt + 1;
054600000000                         YPrtArr(saved_empcnt) =  %trim(ptyshrtnm);
054700000000                         Yamount(saved_empcnt) = %char(ytdamt);
054800000000                     endif;
054900000000              endfor;
055000000000                   exsr print_ytd_record;
055100000000
055200000000           when  ytd_opt = 2;
055300000000            saved_empcnt = 0;
055400050525            countr = *zeros;
055500050525            dow not get_payty_info('RD':-1:payty_struc:countr);
055600000000              pay_type = ptyptype;
055700000000              ytdamt = *zeros;
055800000000              chain ytd_key ytdrec;
055900000000               if %found(payeytdl01);
056000000000                saved_empcnt = saved_empcnt + 1;
056100000000                  // if end of line reached
056200000000                  if   saved_empcnt > limit;
056300000000                   exsr print_ytd_record;
056400000000                  endif;
056500000000                YPrtArr(saved_empcnt) =  %trim(ptyshrtnm);
056600000000                Yamount(saved_empcnt) = %char(ytdamt);
056700000000               endif;
056800000000            enddo;
056900000000            exsr print_ytd_record;
057000000000
057100000000         endsl;
057200000000         endif;
057300000000
057400000000
057500030603        // Print any comments which had been set up by pay clerk
057600000000            exsr  prnt_comt;
057700000000
057800000000          endif;
057900000000
058000000000        endsr;
058100000000
058200000000       //** Payroll Output
058300000000       //** output Payroll Info. Printer File  Horizontal
058400000000       //********************************************************
058500000000       begsr prnt_horizontal;
058600000000
058700000000          if  xcnt <> *zeros;
058800000000        //** Sort data structure
058900000000            %occur(pay_recP) = 1;
059000000000            SortIt(%Addr(pay_recP) : xcnt :
059100000000                             %Size(pay_RecP) : %PAddr('SEQSTRUC'));
059200000000        // Get Bank name from division file for Payslip
059300000000          chain  divis divfmt;
059400000000          bank_name = %trim(dtname) + ' :';
059500000000          prtyndate# = %date(prtyndate) - %days(1);
059600000000          nis_num  = nisnum;
059700000000          emp_doe  = %date(emply:*eur);
059800000000          dname  = get_dept_info(dept);
059900000000
060000000000        // employee  header record
060100000000        write  headerH;
060200000000
060300000000         pay_codeHld = 'E';
060400000000         saved_empcnt = 1;
060500000000         clear pPrtArr;
060600000000         pay010 = *blanks;
060700000000
060800000000         for  ycnt  = 1 to xcnt;
060900000000
061000000000                  saved_empcnt = saved_empcnt + 1;
061100000000                  %occur(pay_RecP) = ycnt;
061200000000              // employee Hourly Rate
061300000000                  exsr get_hoursRate;
061400000000
061500000000                  // if changes from pay to deduct
061600000000          if  pay_codeP = pay_codeHld;
061700000000
061800000000                  // if end of line reached
061900000000                if   saved_empcnt > fld_count;
062000000000
062100000000                     typ_total = *zeros;
062200000000                     exsr Print_payslip_detail;
062300000000                     saved_empcnt = 2;
062400000000                     clear pPrtArr;
062500000000                     clear pytd;
062600000000                     clear pamount;
062700000000                     clear prate;
062800000000                     HoursFound = '0';
062900000000                endif;
063000000000
063100000000                  // if changes from pay to deduct
063200000000                  else;
063300000000                       saved_empcnt = 2;
063400000000                       typ_total = total_gross;
063500000000                       exsr Print_payslip_detail;
063600000000                       pay_codeHld = pay_codeP;
063700000000                       clear pPrtArr;
063800000000                       clear pytd;
063900000000                       clear pamount;
064000000000                       clear prate;
064100000000                       HoursFound = '0';
064200000000                endif;
064300000000
064400000000                 pPrtArr(saved_empcnt) = pay_shdsP;
064500000000                 prate(saved_empcnt) = %editc(pay_hrsP:'2');
064600000000                 if pay_rate <> *zeros;
064700000000                  prate(saved_empcnt) = %trim(prate(saved_empcnt))
064800000000                        + '@' + %trim(%editc(pay_rate:'2'));
064900000000                 endif;
065000000000                 pamount(saved_empcnt) = %char(pay_amtP);
065100000000                 pytd(saved_empcnt) = %char(pay_ytdP);
065200000000
065300000000         endfor;
065400000000
065500030603        // end of pay info
065600000000
065700000000                   typ_total = totduc;
065800000000                   exsr Print_payslip_detail;
065900000000
066000000000                       clear pPrtArr;
066100000000                       clear pytd;
066200000000                       clear pamount;
066300000000                       clear prate;
066400000000                       HoursFound = '0';
066500000000                       pPrtArr(fld_count) =  net_cnst;
066600000000                       pPrtArr(one) =  *blank;
066700000000
066800000000          // final employee record
066900000000                       pay010 = %char(net);
067000000000
067100000000                       write detail;
067200000000
067300000000          // Print any comments which had been set up by pay clerk
067400000000                         exsr   prnt_comt;
067500000000
067600000000                   endif;
067700000000
067800000000        endsr;
067900000000
068000030603        //*********************************
068100030603        // StartUp subroutine
068200030603        //*********************************
068300030603           // begsr *inzsr;
068400030603
068500030603     C     *inzsr        begsr
068600000000     C     *entry        plist
068700000000     C                   parm                    payroll_type
068800000000     C                   parm                    payroll_date
068900000000     C                   parm                    mimic_type
069000000000     C                   parm                    rate_flag
069100000000     C                   parm                    py_style
069200000000     C                   parm                    ytd_opt
069300030603
069400000000         // Get company name
069500000000           coname = get_coname;
069600000000           @date = get_sys_date(system_date);
069700000000           open paycon;
069800000000           read paycon;
069900000000            if not(%eof);
070000000000                pcdftytdDS = pcdftytd;
070100000000            endif;
070200000000          close paycon;
070300000000
070400000000           select;
070500000000            when py_style = 1;
070600000000              open payslipfl;
070700000000            when py_style = 2;
070800000000              open payslipf;
070900000000            endsl;
071000150824
071100030603         //*                 eval      @title = ARR(1)
071200000000       endsr;
071300030603
071400000000            begsr  prnt_comt;
071500000000                 chain payroll_type pcomfmt;
071600000000                   if not(%found(paycomm));
071700000000                     setll *loval pcomfmt;
071800000000                        read pcomfmt;
071900000000                      if %eof(paycomm)
072000000000                            or pcomtype  <> ' ';
072100000000                        leavesr;
072200000000                      endif;
072300000000                   endif;
072400000000
072500000000               if   py_style = 2;
072600000000                   if pcomdesc1 <> *blanks;
072700000000                   com_fld = pcomdesc1;
072800000000                    write blnk_rcd;
072900000000                    write com_rcd;
073000000000                 endif;
073100000000
073200000000                   if pcomdesc2 <> *blanks;
073300000000                   com_fld = pcomdesc2;
073400000000                    write com_rcd;
073500000000                 endif;
073600000000
073700000000                   if pcomdesc3 <> *blanks;
073800000000                   com_fld = pcomdesc3;
073900000000                    write com_rcd;
074000000000                 endif;
074100000000                else;
074200000000                // First Line
074300000000                   com_fldh = %trim(%subst((
074400000000                         %trim(pcomdesc1) + ' ' +
074500000000                         %trim(pcomdesc2) + ' ' +
074600000000                         %trim(pcomdesc3)):one:%size(com_fldh)));
074700000000                    if com_fldH <> *blanks;
074800000000                      write com_rcdh;
074900000000                    endif;
075000000000                // Second Line
075100000000                   com_fldh = %trim(%subst((
075200000000                         %trim(pcomdesc1) + ' ' +
075300000000                         %trim(pcomdesc2) + ' ' +
075400000000                         %trim(pcomdesc3)):%size(com_fldh)+ 1));
075500000000                    if com_fldH <> *blanks;
075600000000                      write com_rcdh;
075700000000                    endif;
075800000000               endif;
075900000000            endsr;
076000000000         //------------------------------
076100000000         // Get calculate Hours/Rate
076200000000         //------------------------------
076300000000        begsr get_hoursRate;
076400000000
076500000000          // if hours not zero, check if rates flag set.
076600000000          // If it is,print rates else blank it
076700000000                if  pay_hrsp <> *zeros;
076800000000                     HoursFound = '1';
076900000000
077000000000                    if  rate_flag = 'Y';
077100000000                        pay_rate  = pay_amtp  / pay_hrsp;
077200000000                         else;
077300000000                        pay_rate  =  0;
077400000000                    endif;
077500000000
077600000000                      else;
077700000000                     pay_rate =  0;
077800000000                  endif;
077900000000        endsr;
078000000000       //-----------------------------
078100000000       // Print payslip Detail lines
078200000000       //-----------------------------
078300000000           begsr print_payslip_detail;
078400000000                  for  counter  = 1 to 3;
078500000000                   write  detail;
078600000000                   pay010 = *blanks;
078700000000                     select;
078800000000                      when counter = 1;
078900000000                    // Print any Rates
079000000000                     if pay_codeHld = 'E' and
079100000000                           HoursFound;
079200000000                          pPrtArr = prate;
079300000000                          pPrtArr(one) = rate_cnst;
079400000000                          write  detail;
079500000000                        endif;
079600000000                       pPrtArr = pamount;
079700000000                       pay010 = %char(typ_total);
079800000000
079900000000                     if pay_codeHld = 'E';
080000000000                           pPrtArr(one) = emol_cnst;
080100000000                            else;
080200000000                           pPrtArr(one) = ded_cnst;
080300000000                     endif;
080400000000                      when counter = 2;
080500000000                       pPrtArr = pytd;
080600000000                       pPrtArr(one) = ytd_cnst;
080700000000                     endsl;
080800000000                endfor;
080900000000           endsr;
081000000000       //-----------------------------
081100000000       // Print payslip YTD Detail
081200000000       //-----------------------------
081300000000         begsr print_ytd_record;
081400000000
081500000000                  // Employee Year-to-date Detail records
081600000000                  for  counter = 1 to 2;
081700000000                     write  ytd_det;
081800000000                      if counter = 1;
081900000000                       YPrtArr = Yamount;
082000000000                      endif;
082100000000                endfor;
082200000000                     saved_empcnt = 1;
082300000000                     clear YPrtArr;
082400000000                     clear Yamount;
082500000000
082600000000                endsr;
082700000000
082800030603        //*******************************************************************
082900030603        //* Payroll SubProcedures  ***
083000030603        //*******************************************************************
083100030603        //*******************************************************************
083200030603        //**  Sequence data struc /array ** descending
083300030603        //*******************************************************************
083400000000     P SeqStruc        B
083500000000
083600000000     D                 PI            10I 0
083700000000     D  Element1@                      *   Value
083800000000     D  Element2@                      *   Value
083900000000
084000000000     D Element1        S                   Like(pay_RecP) Based(Element1@)
084100000000     D Element2        S                   Like(pay_RecP) Based(Element2@)
084200000000
084300030603      /FREE
084400030603        // Sort descending sequence
084500000000       Select;
084600000000       When Element1 > Element2;
084700000000         Return Low;
084800000000       When Element1 < Element2;
084900000000         Return High;
085000000000       Other;
085100000000         Return Equal;
085200000000       EndSl;
085300000000
085400000000      /END-FREE
085500000000     P                 E
085600041206       //************************************                               **
085700041206       // Get all payroll ytd. figures
085800041206       //************************************                               **
085900041206     P GetAllYtd       B
086000041206     D GetAllYtd       PI
086100041206
086200041206      /free
086300041206         if xcnt <> *zeros;
086400041206
086500050525            countr = *zeros;
086600050525            dow not get_payty_info('RD':-1:payty_struc:countr);
086700050525
086800041206             if  %lookup(ptyptype:paytypeArr) = *zeros;
086900041206
087000041206             // Get YTD record
087100041206               chain (emp:prtyyear:ptyptype:origin) ytdrec;
087200041206                if  %found and  ytdamt <> *zeros;
087300041214                 chain (payroll_type:ptyptype) prdffmt;
087400041216                 if %found and prdfflag = 'Y';
087500041206
087600041206                 // save values for payslip output
087700041206                  xcnt += 1;
087800041206                  %occur(pay_recp) = xcnt;
087900041206                  pay_codeP = ptyrtype;
088000041206                  pay_hrsP = *zeros;
088100041206                  pay_amtP = *zeros;
088200041206                  pay_descP = ptydesc;
088300041206                  pay_shdsP = ptyshrtnm;
088400041206                  pay_ytdP = ytdamt;
088500041206
088600041216                 endif;
088700041206                endif;
088800041206             endif;
088900041206
089000041206           enddo;
089100041206
089200041206         endif;
089300041206
089400041206      /end-free
089500041206     PGetAllYTD        E
