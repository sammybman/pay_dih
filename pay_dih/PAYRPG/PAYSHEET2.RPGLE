000100000000     H DECEDIT('.') DATEDIT(*ymd/) bnddir('QC2LE')
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
001300000000     Fpaytypl01 if   e           k disk
001400000000     Fpayrdefl01if   e           k disk
001500000000     Fpaycomm   if   e           k disk
001600000000     Fpaysuspl01uf   e           k disk    commit
001700000000      //
001800000000     Fpayhis    o    e             disk    commit
001900000000     Fpaytrans  o    e             disk    commit
002000000000
002100000000     Fpayslipf  o    e             printer usropn
002200000000     Fpayslipfl o    e             printer usropn
002300000000      //
002400131029      /copy qcpysrc,genPR
002500000000      // Payroll PROTOTYPES
002600131029      /copy qcpysrc,PAYPR
002700131029      /Copy qcpysrc,CsortPr
002800000000      // Prototypes for compare routines for sort
002900000000
003000000000     D SeqStruc        Pr            10I 0
003100000000     D  Element1@                      *   Value
003200000000     D  Element2@                      *   Value
003300000000      //
003400000000     Dpay_rec          ds                  occurs(50) based(struc@)
003500000000     D pay_prior                      2  0
003600000000     D pay_type                       2  0
003700000000     D pay_tax                        1
003800000000     D pay_code                       1
003900000000     D pay_hrs                        5  2
004000000000     D pay_amt                       11  2
004100000000      //
004200000000     Dpay_recP         ds                  occurs(50)
004300000000     D pay_codeP                      1
004400000000     D pay_descP                     25
004500000000     D pay_hrsP                       5  2
004600000000     D pay_amtP                      11  2
004700000000     D pay_shdsP                      9
004800000000     D pay_ytdP                      11  2
004900000000      //
005000000000     D                 ds
005100000000     D  pcdftytdDS                   10
005200000000     D dft_list                       2  0 dim(5) overlay(pcdftytdDS)
005300000000      //
005400000000     dpy_detailH       ds
005500000000     d  pay001                       10
005600000000     d  pay002                       10
005700000000     d  pay003                       10
005800000000     d  pay004                       10
005900000000     d  pay005                       10
006000000000     d  pay006                       10
006100000000     d  pay007                       10
006200000000     d  pay008                       10
006300000000     d  pay009                       10
006400000000      //
006500000000     dytd_detail       ds
006600000000     d  ytd001                       14
006700000000     d  ytd002                       14
006800000000     d  ytd003                       14
006900000000     d  ytd004                       14
007000000000     d  ytd005                       14
007100000000
007200000000     d ytdptr          s               *   inz(%addr(ytd_detail))
007300000000     d recptr          s               *   inz(%addr(py_detailH))
007400000000     d pprtArr         s             10    dim(09) based(recptr)
007500000000     d prate           s             10    dim(09)
007600000000     d pAmount         s             10    dim(09)
007700000000     d pytd            s             10    dim(09)
007800000000     d YprtArr         s             14    dim(05) based(ytdptr)
007900000000     d YAmount         s             14    dim(05)
008000000000      //
008100000000     D strucMax        S              3P 0 Inz(50)
008200000000     D Memory          S              5P 0
008300000000     D xcnt            s              3  0 inz(0)
008400000000     D ycnt            s              3  0 inz(1)
008500040621     D total_gross     s             11  2
008600040621     D Max_Gross       s             11  2
008700000000     d typ_total       s             11  2
008800000000      //
008900000000     D coname          s             30
009000000000     D @date           s             10
009100000000      //
009200000000     D totduc          s                   like(total_gross)
009300000000     D tax_gross       s                   like(total_gross)
009400000000     D actear          s                   like(total_gross)
009500000000     D insern          s                   like(total_gross)
009600000000     D com_contrib     s                   like(total_gross)
009700000000     D net             s                   like(total_gross)
009800000000     D trn_check       s              2  0
009900000000     D nrhrs           s              5  2
010000000000     D othrs           s              5  2
010100000000     D pay_codeHld     s              1
010200000000     D payroll_type    s              1
010300000000     D mimic_type      s              1
010400000000     D py_style        s              1  0
010500000000     D ytd_opt         s              1  0
010600000000     D rate_flag       s              1
010700000000     D bank_rtype      s              1
010800000000     D origin          s              3    inz('PAY')
010900000000     D datefield       s               d   datfmt(*iso)                         TOTAL AMOUNT
011000000000     D payroll_date    s              8  0
011100000000     D system_date     s              8  0
011200131029     D cat_flag        s              1    inz(' ')
011300000000     d saved_empcnt    s                   like(ycnt)
011400000000     D counter         s              3  0
011500000000     d HoursFound      s               n
011600000000      //
011700000000     D gross_text      c                   'Total Gross'
011800000000     D deduc_text      c                   'Total Deductions'
011900000000     d rate_cnst       c                   'Hrs@Rate'
012000000000     d net_cnst        c                   'Net amount'
012100000000     d emol_cnst       c                   'Emoluments'
012200000000     d ded_cnst        c                   'Deductions'
012300000000     d ytd_cnst        c                   'Y.T.D.'
012400000000     D tax_code        c                   61
012500000000     D nis_code        c                   63
012600000000     d one             c                   1
012700000000     d fld_count       c                   9
012800000000     D limit           c                   5
012900000000     D blank_text      c                   'Gross'
013000000000
013100000000      // total-fields definition
013200000000
013300000000     D indptr          s               *   inz(%addr(*in))
013400000000      //
013500000000     D                 ds                  based(indptr)
013600000000     D ytd_notfound            1      1n
013700000000      //
013800000000     Iemprec
013900000000     I                                          dept          L2
014000000000
014100000000      // Set up initial memory allocation for Struc of StrucMax (50) elements
014200000000      /FREE
014300000000       Memory = %Size(pay_rec) * StrucMax;
014400000000      /END-FREE
014500000000     C                   Alloc     Memory        Struc@
014600000000      //
014700000000
014800000000      // Key to verify transaction to Process
014900000000     C     trn_key       klist
015000000000     C                   kfld                    payroll_type
015100000000     C                   kfld                    trn_check
015200000000      // Key for YTD
015300000000     C     ytd_key       klist
015400000000     C                   kfld                    emp
015500000000     C                   kfld                    prtyyear
015600000000     C                   kfld                    pay_type
015700000000     C                   kfld                    origin
015800000000
015900000000      //-------------------------------------------------------------------------
016000000000
016100000000      // Get PayPeriod Number
016200000000      /FREE
016300000000       chain payroll_type prtyfmt;
016400000000       if prtyprno = prtyproll#;
016500000000         prtyproll# = 0;
016600040621         prtyyear += 1;
016700000000       endif;
016800000000
016900000000       // check for suspended payment
017000040204       chain (emp:payroll_type)  susfmt;
017100041206       if  %found  and  sussdate >= prtyndate;
017200000000
017300000000         susprd = susprd - 1;
017400000000         //
017500000000         if susprd <= *zeros;
017600000000           delete susfmt;
017700000000         else;
017800000000           update susfmt;
017900000000         endif;
018000000000         //
018100000000       else;
018200000000
018300000000         // Get Division and Bank account if found
018400000000         if mimic_type <> *blank;
018500000000           bank_rtype = mimic_type;
018600000000         else;
018700000000           bank_rtype = payroll_type;
018800000000         endif;
018900000000         //
019000040204         //  get employee bank account
019100040204         chain (emp:bank_rtype)  pbfmt;
019200000000
019300000000         // Get Employee Name
019400000000         emp_name = %trim(christ) +
019500000000             ' ' + %triml(surnam);
019600000000         //-------------------------------------------------------------------------
019700000000
019800000000         reset xcnt;
019900000000         //
020000030604      /end-free
020100030604     C                   clear     *all          pay_Rec
020200030604      /FREE
020300030604         // clear *all pay_Rec;
020400030604         clear *all pay_Recp;
020500000000         clear totduc;
020600000000         clear total_gross;
020700000000         clear tax_gross;
020800041206
020900041206         //**********************************************
021000000000         //* GET EMPLOYEE TRANSACTIONS ***
021100041206         //**********************************************
021200041206         //
021300000000         if get_emp_trans(emp:%addr(pay_Rec):
021400000000               xcnt:totduc:tax_gross:
021500000000               total_gross);
021600000000
021700041206           //********************************************                   *
021800041206           //* P.A.Y.E. CALCULATIONS  ROUTINE. ***
021900041206           //********************************************                   *
022000041206           //
022100000000           trn_check = tax_code;
022200000000           chain trn_key prdffmt;
022300000000           if %found and prdfflag = 'Y';
022400000000
022500000000             calc_emp_tax(emp:frpay:tax_gross
022600000000                 :totduc:prtycycode
022700000000                 :prtyproll#:prtyyear
022800000000                 :prtycycqty:status
022900041106                 :%addr(pay_rec):xcnt:emply:bank_rtype);
023000000000           endif;
023100000000           //****************************************************************
023200000000           //* N.I.S. CALCULATIONS ***
023300000000           //****************************************************************
023400000000           trn_check = nis_code;
023500000000           chain trn_key prdffmt;
023600000000           if %found and prdfflag = 'Y';
023700000000             if tax_gross > *zero;
023800000000               actear = tax_gross;
023900000000               calc_emp_nis(actear:insern:totduc:
024000000000                   com_contrib:birth:prtycycode:
024100000000                   prtycycqty:
024200131029                   %addr(pay_rec):xcnt:emp:cat_flag);
024300000000             endif;
024400040622
024500040622            // Get payroll restrictions/parameters
024600040622            chain payroll_type prrefmt;
024700040622
024800040622             //* Declare earnings as being X.XX % more than insurable earnings
024900040622             if actear > insern
025000040622               and prredeclr > *zeros;
025100040622              actear = insern + ((prredeclr * insern) / 100);
025200000000             endif;
025300000000
025400000000           endif;
025500040622
025600040622
025700040621          //   net = total_gross - totduc;
025800040622
025900040622           // Handle payroll guaranteed percentage etc.
026000040621           select;
026100040621
026200040621           when prreguar > 0;
026300040621              Max_Gross = round_up(total_gross *
026400040621                          (100 - prreguar) /100:'A');
026500040621              net = Max_gross - totduc;
026600040621            if (%abs(Max_gross) - %abs(totduc)) <
026700040621                 *zeros;
026800040621              adj_pay_credit(net:totduc:
026900040621                  %addr(pay_rec):xcnt);
027000040621            endif;
027100040621              net += total_gross - Max_gross;
027200040621
027300040621           other;
027400040621            net = total_gross - totduc;
027500040621            if (%abs(total_gross) - %abs(totduc)) <
027600040621                 *zeros;
027700040621              adj_pay_credit(net:totduc:
027800040621                  %addr(pay_rec):xcnt);
027900040621            endif;
028000040621           endsl;
028100040621
028200000000           //* Summary of employee transactions
028300000000           exsr sum_trans;
028400000000           //
028500000000           //* Figures
028600000000           //
028700000000           phisemp = emp;
028800000000           phisdept = dept;
028900000000           phisdiv = divis;
029000000000           phisloc = locat;
029100000000           phisgpay = total_gross;
029200000000           phisaearn = actear;
029300000000           phisnpay = net;
029400000000           //
029500000000           phisfnight = prtyproll# + 1;
029600000000           //
029700000000           phisiearn = insern;
029800000000           phissts = 'A';
029900000000           phisrdate = system_date;
030000000000           phispdate = payroll_date;
030100000000           phisndate = prtyndate;
030200131029           phiswcate = cat_flag;
030300000000           get_prd_info('*prd':*blanks:phisprno:
030400000000               prtyndate);
030500000000           // Write Correct run type for pay history file
030600000000           phistype = bank_rtype;
030700000000           //
030800000000           write phisfmt;
030900000000           clear phisfmt;
031000000000
031100000000           // Write /Update YTD record
031200000000           //       Taxable Gross (pay_type = 0)
031300000000           pay_type = *zeros;
031400000000           ytdamt = *zeros;
031500000000           chain ytd_key ytdrec;
031600000000           *IN01 = NOT %FOUND;
031700000000
031800040622           ytdamt += tax_gross;
031900000000           if ytd_notfound;
032000000000             year = prtyyear;
032100000000             ytdtype = pay_type;
032200000000             ytdorg = origin;
032300000000             write ytdrec;
032400000000           else;
032500000000             update ytdrec;
032600000000           endif;
032700000000           //
032800000000
032900000000           // Print Paycheck
033000000000        select;
033100000000         when py_style = 1;
033200000000           exsr prnt_horizontal;
033300000000         when py_style = 2;
033400000000           exsr prnt_vertical;
033500000000         endsl;
033600030603        //
033700030603        // Clear fields used for ytd files  (temp work)
033800000000           actear = *zeros;
033900000000           insern = *zeros;
034000000000           clear ytdrec;
034100000000
034200000000           // end of valid transaction
034300000000         endif;
034400000000         // end of valid selection - suspended payments
034500000000       endif;
034600000000       //*********************************************************************
034700000000       // Payroll Output
034800000000       // output Payroll Info. to History/Y.T.D. files
034900000000       //*********************************************************************
035000000000       begsr sum_trans;
035100000000         if xcnt <> *zeros;
035200000000           //
035300000000           reset ycnt;
035400000000           dow ycnt <= xcnt;
035500000000      /END-FREE
035600000000     C     ycnt          occur     pay_Rec
035700000000     C     ycnt          occur     pay_Recp
035800000000      /FREE
035900000000             chain pay_type ptyfmt;
036000000000             // save values for payslip output
036100000000             pay_codeP = pay_code;
036200000000             pay_hrsP = pay_hrs;
036300000000             pay_amtP = pay_amt;
036400000000             pay_descP = ptydesc;
036500000000             pay_shdsP = ptyshrtnm;
036600000000
036700000000             // if is emoluments
036800000000             if pay_code = 'E';
036900000000               select;
037000000000                 // Overtime
037100000000               when pay_type = 2;
037200000000                 phisothrs = phisothrs + pay_hrs;
037300000000                 // Normal time
037400000000               other;
037500000000                 phisnthrs = phisnthrs + pay_hrs;
037600000000               endsl;
037700000000
037800000000             endif;
037900000000             // Write to transaction History File
038000000000             ptrnsts = 'A';
038100000000             ptrnemp = emp;
038200000000             ptrnrdate = system_date;
038300000000             ptrnpdate = payroll_date;
038400000000             ptrnndate = prtyndate;
038500000000             ptrnhrs  = pay_hrs;
038600000000             ptrnamt  = pay_amt;
038700000000             // Write Correct run type for pay transaction file
038800000000             ptrntype = bank_rtype;
038900000000             //
039000000000             ptrncode = pay_code;
039100000000             ptrntcode = pay_type;
039200131029             ptrnwcate = cat_flag;
039300000000             write ptisfmt;
039400000000
039500000000             ytdamt = *zeros;
039600000000             chain ytd_key ytdrec;
039700000000             *IN01 = NOT %FOUND;
039800000000             // Write /Update YTD record
039900000000             ytdamt = ytdamt + pay_amt;
040000000000             if ytd_notfound;
040100000000               year = prtyyear;
040200000000               ytdtype = pay_type;
040300000000               ytdorg = origin;
040400000000               write ytdrec;
040500000000             else;
040600000000               update ytdrec;
040700000000             endif;
040800000000
040900000000             pay_ytdP = ytdamt;
041000000000             ycnt = ycnt + 1;
041100000000           enddo;
041200000000           //
041300000000           //
041400000000         endif;
041500000000
041600000000       endsr;
041700000000       //*********************************************************************
041800000000       // Payroll Output
041900000000       // output Payroll Info. Printer File
042000000000       //*********************************************************************
042100000000       begsr prnt_vertical;
042200000000
042300000000           if  xcnt <> *zeros;
042400000000          // Sort data structure
042500000000            %occur(pay_recP) = 1;
042600000000            SortIt(%Addr(pay_recP) : xcnt :
042700000000                             %Size(pay_RecP) : %PAddr('SEQSTRUC'));
042800000000        // Get Bank name from division file for Payslip
042900000000          chain  divis divfmt;
043000000000          bank_name = %trim(dtname) + ' :';
043100000000          prtyndate# = %date(prtyndate) - %days(1);
043200000000        // employee  header record
043300000000                 write  header;
043400000000
043500000000           reset ycnt;
043600000000           pay_codeHld = 'E';
043700000000           dow  ycnt <= xcnt;
043800000000            %occur(pay_recP) = ycnt;
043900000000
044000000000        // if Paycode changes (E= emoluments,D= deductions)
044100000000               if   pay_codeP <> pay_codeHld;
044200000000                   sub_text = gross_text;
044300000000                   sub_tot  = total_gross;
044400000000        // employee transaction summary total for Emoluments
044500000000                   write  sub_tot_rc;
044600000000                   pay_codeHld = pay_codeP;
044700000000                endif;
044800000000
044900000000        // employee Hourly Rate
045000000000            exsr get_hoursRate;
045100000000
045200000000             pay_amt =  pay_amtP;
045300000000            if ytd_opt = 3;
045400000000             ytd_amt =  pay_ytdP;
045500000000             ptyshnm =  pay_shdsP;
045600000000             write  det_recd2;
045700000000              else;
045800000000             ptydesc =  pay_descP;
045900000000             write  det_recd;
046000000000            endif;
046100000000
046200000000             ycnt = ycnt + 1;
046300000000          enddo;
046400000000
046500000000             if  totduc <> *zeros;
046600000000                sub_text = deduc_text;
046700000000                sub_tot  = totduc;
046800000000          // employee transaction summary total for DEDUCTIONS
046900000000                write  sub_tot_rc;
047000000000              endif;
047100000000
047200000000          // final employee record
047300000000            write  fintotal;
047400000000          // employee Year-to-date Header record
047500000000            if ytd_opt <> 3;
047600000000            write  ytdrcd;
047700000000
047800000000           clear YPrtArr;
047900000000           clear Yamount;
048000000000           saved_empcnt = *zeros;
048100000000           select;
048200000000            when  ytd_opt = 1;
048300000000              for counter = 1 to limit;
048400000000
048500000000                     if dft_list(counter) = 0;
048600000000                       if  counter <> 1;
048700000000                         iter;
048800000000                       endif;
048900000000                       eval  ptyshrtnm = blank_text;
049000000000                         else;
049100000000                        chain  dft_list(counter) ptyfmt;
049200000000                     endif;
049300000000                        pay_type = dft_list(counter);
049400000000                        ytdamt = *zeros;
049500000000                        chain ytd_key ytdrec;
049600000000                     if  %found(payeytdl01);
049700000000                         saved_empcnt = saved_empcnt + 1;
049800000000                         YPrtArr(saved_empcnt) =  %trim(ptyshrtnm);
049900000000                         Yamount(saved_empcnt) = %char(ytdamt);
050000000000                     endif;
050100000000              endfor;
050200000000                   exsr print_ytd_record;
050300000000
050400000000           when  ytd_opt = 2;
050500000000            setll *loval ptyfmt;
050600000000            saved_empcnt = 0;
050700000000            read ptyfmt;
050800000000            dow not(%eof);
050900000000              pay_type = ptyptype;
051000000000              ytdamt = *zeros;
051100000000              chain ytd_key ytdrec;
051200000000               if %found(payeytdl01);
051300000000                saved_empcnt = saved_empcnt + 1;
051400000000                  // if end of line reached
051500000000                  if   saved_empcnt > limit;
051600000000                   exsr print_ytd_record;
051700000000                  endif;
051800000000                YPrtArr(saved_empcnt) =  %trim(ptyshrtnm);
051900000000                Yamount(saved_empcnt) = %char(ytdamt);
052000000000               endif;
052100000000            read ptyfmt;
052200000000            enddo;
052300000000            exsr print_ytd_record;
052400000000
052500000000         endsl;
052600000000         endif;
052700000000
052800000000
052900030603        // Print any comments which had been set up by pay clerk
053000000000            exsr  prnt_comt;
053100000000
053200000000          endif;
053300000000
053400000000        endsr;
053500000000
053600000000       //** Payroll Output
053700000000       //** output Payroll Info. Printer File  Horizontal
053800000000       //********************************************************
053900000000       begsr prnt_horizontal;
054000000000
054100000000          if  xcnt <> *zeros;
054200000000        //** Sort data structure
054300000000            %occur(pay_recP) = 1;
054400000000            SortIt(%Addr(pay_recP) : xcnt :
054500000000                             %Size(pay_RecP) : %PAddr('SEQSTRUC'));
054600000000        // Get Bank name from division file for Payslip
054700000000          chain  divis divfmt;
054800000000          bank_name = %trim(dtname) + ' :';
054900000000          prtyndate# = %date(prtyndate) - %days(1);
055000000000          nis_num  = nisnum;
055100000000          emp_doe  = %date(emply:*eur);
055200000000          dname  = get_dept_info(dept);
055300000000
055400000000        // employee  header record
055500000000        write  headerH;
055600000000
055700000000         pay_codeHld = 'E';
055800000000         saved_empcnt = 1;
055900000000         clear pPrtArr;
056000000000         pay010 = *blanks;
056100000000
056200000000         for  ycnt  = 1 to xcnt;
056300000000
056400000000                  saved_empcnt = saved_empcnt + 1;
056500000000                  %occur(pay_RecP) = ycnt;
056600000000              // employee Hourly Rate
056700000000                  exsr get_hoursRate;
056800000000
056900000000                  // if changes from pay to deduct
057000000000          if  pay_codeP = pay_codeHld;
057100000000
057200000000                  // if end of line reached
057300000000                if   saved_empcnt > fld_count;
057400000000
057500000000                     typ_total = *zeros;
057600000000                     exsr Print_payslip_detail;
057700000000                     saved_empcnt = 2;
057800000000                     clear pPrtArr;
057900000000                     clear pytd;
058000000000                     clear pamount;
058100000000                     clear prate;
058200000000                     HoursFound = '0';
058300000000                endif;
058400000000
058500000000                  // if changes from pay to deduct
058600000000                  else;
058700000000                       saved_empcnt = 2;
058800000000                       typ_total = total_gross;
058900000000                       exsr Print_payslip_detail;
059000000000                       pay_codeHld = pay_codeP;
059100000000                       clear pPrtArr;
059200000000                       clear pytd;
059300000000                       clear pamount;
059400000000                       clear prate;
059500000000                       HoursFound = '0';
059600000000                endif;
059700000000
059800000000                 pPrtArr(saved_empcnt) = pay_shdsP;
059900000000                 prate(saved_empcnt) = %editc(pay_hrsP:'2');
060000000000                 if pay_rate <> *zeros;
060100000000                  prate(saved_empcnt) = %trim(prate(saved_empcnt))
060200000000                        + '@' + %trim(%editc(pay_rate:'2'));
060300000000                 endif;
060400000000                 pamount(saved_empcnt) = %char(pay_amtP);
060500000000                 pytd(saved_empcnt) = %char(pay_ytdP);
060600000000
060700000000         endfor;
060800000000
060900030603        // end of pay info
061000000000
061100000000                   typ_total = totduc;
061200000000                   exsr Print_payslip_detail;
061300000000
061400000000                       clear pPrtArr;
061500000000                       clear pytd;
061600000000                       clear pamount;
061700000000                       clear prate;
061800000000                       HoursFound = '0';
061900000000                       pPrtArr(fld_count) =  net_cnst;
062000000000                       pPrtArr(one) =  *blank;
062100000000
062200000000          // final employee record
062300000000                       pay010 = %char(net);
062400000000
062500000000                       write detail;
062600000000
062700000000          // Print any comments which had been set up by pay clerk
062800000000                         exsr   prnt_comt;
062900000000
063000000000                   endif;
063100000000
063200000000        endsr;
063300000000
063400030603        //*********************************
063500030603        // StartUp subroutine
063600030603        //*********************************
063700030603           // begsr *inzsr;
063800030603      /END-FREE
063900030603
064000030603     C     *inzsr        begsr
064100000000     C     *entry        plist
064200000000     C                   parm                    payroll_type
064300000000     C                   parm                    payroll_date
064400000000     C                   parm                    mimic_type
064500000000     C                   parm                    rate_flag
064600000000     C                   parm                    py_style
064700000000     C                   parm                    ytd_opt
064800030603
064900000000      /free
065000000000         // Get company name
065100000000           coname = get_coname;
065200000000           @date = get_sys_date(system_date);
065300000000           open paycon;
065400000000           read paycon;
065500000000            if not(%eof);
065600000000                pcdftytdDS = pcdftytd;
065700000000            endif;
065800000000          close paycon;
065900000000
066000000000           select;
066100000000            when py_style = 1;
066200000000              open payslipfl;
066300000000            when py_style = 2;
066400000000              open payslipf;
066500000000            endsl;
066600000000      /end-free
066700030603         //*                 eval      @title = ARR(1)
066800000000      /FREE
066900000000       endsr;
067000030603
067100000000            begsr  prnt_comt;
067200000000                 chain payroll_type pcomfmt;
067300000000                   if not(%found(paycomm));
067400000000                     setll *loval pcomfmt;
067500000000                        read pcomfmt;
067600000000                      if %eof(paycomm)
067700000000                            or pcomtype  <> ' ';
067800000000                        leavesr;
067900000000                      endif;
068000000000                   endif;
068100000000
068200000000               if   py_style = 2;
068300000000                   if pcomdesc1 <> *blanks;
068400000000                   com_fld = pcomdesc1;
068500000000                    write blnk_rcd;
068600000000                    write com_rcd;
068700000000                 endif;
068800000000
068900000000                   if pcomdesc2 <> *blanks;
069000000000                   com_fld = pcomdesc2;
069100000000                    write com_rcd;
069200000000                 endif;
069300000000
069400000000                   if pcomdesc3 <> *blanks;
069500000000                   com_fld = pcomdesc3;
069600000000                    write com_rcd;
069700000000                 endif;
069800000000                else;
069900000000                // First Line
070000000000                   com_fldh = %trim(%subst((
070100000000                         %trim(pcomdesc1) + ' ' +
070200000000                         %trim(pcomdesc2) + ' ' +
070300000000                         %trim(pcomdesc3)):one:%size(com_fldh)));
070400000000                    if com_fldH <> *blanks;
070500000000                      write com_rcdh;
070600000000                    endif;
070700000000                // Second Line
070800000000                   com_fldh = %trim(%subst((
070900000000                         %trim(pcomdesc1) + ' ' +
071000000000                         %trim(pcomdesc2) + ' ' +
071100000000                         %trim(pcomdesc3)):%size(com_fldh)+ 1));
071200000000                    if com_fldH <> *blanks;
071300000000                      write com_rcdh;
071400000000                    endif;
071500000000               endif;
071600000000            endsr;
071700000000         //------------------------------
071800000000         // Get calculate Hours/Rate
071900000000         //------------------------------
072000000000        begsr get_hoursRate;
072100000000
072200000000          // if hours not zero, check if rates flag set.
072300000000          // If it is,print rates else blank it
072400000000                if  pay_hrsp <> *zeros;
072500000000                     HoursFound = '1';
072600000000
072700000000                    if  rate_flag = 'Y';
072800000000                        pay_rate  = pay_amtp  / pay_hrsp;
072900000000                         else;
073000000000                        pay_rate  =  0;
073100000000                    endif;
073200000000
073300000000                      else;
073400000000                     pay_rate =  0;
073500000000                  endif;
073600000000        endsr;
073700000000       //-----------------------------
073800000000       // Print payslip Detail lines
073900000000       //-----------------------------
074000000000           begsr print_payslip_detail;
074100000000                  for  counter  = 1 to 3;
074200000000                   write  detail;
074300000000                   pay010 = *blanks;
074400000000                     select;
074500000000                      when counter = 1;
074600000000                    // Print any Rates
074700000000                     if pay_codeHld = 'E' and
074800000000                           HoursFound;
074900000000                          pPrtArr = prate;
075000000000                          pPrtArr(one) = rate_cnst;
075100000000                          write  detail;
075200000000                        endif;
075300000000                       pPrtArr = pamount;
075400000000                       pay010 = %char(typ_total);
075500000000
075600000000                     if pay_codeHld = 'E';
075700000000                           pPrtArr(one) = emol_cnst;
075800000000                            else;
075900000000                           pPrtArr(one) = ded_cnst;
076000000000                     endif;
076100000000                      when counter = 2;
076200000000                       pPrtArr = pytd;
076300000000                       pPrtArr(one) = ytd_cnst;
076400000000                     endsl;
076500000000                endfor;
076600000000           endsr;
076700000000       //-----------------------------
076800000000       // Print payslip YTD Detail
076900000000       //-----------------------------
077000000000         begsr print_ytd_record;
077100000000
077200000000                  // Employee Year-to-date Detail records
077300000000                  for  counter = 1 to 2;
077400000000                     write  ytd_det;
077500000000                      if counter = 1;
077600000000                       YPrtArr = Yamount;
077700000000                      endif;
077800000000                endfor;
077900000000                     saved_empcnt = 1;
078000000000                     clear YPrtArr;
078100000000                     clear Yamount;
078200000000
078300000000                endsr;
078400000000
078500030603        //
078600030603        //*******************************************************************
078700030603        //* Payroll SubProcedures  ***
078800030603        //*******************************************************************
078900030603        //*******************************************************************
079000030603        //**  Sequence data struc /array ** descending
079100030603        //*******************************************************************
079200030603      /end-free
079300000000     P SeqStruc        B
079400000000
079500000000     D                 PI            10I 0
079600000000     D  Element1@                      *   Value
079700000000     D  Element2@                      *   Value
079800000000
079900000000     D Element1        S                   Like(pay_RecP) Based(Element1@)
080000000000     D Element2        S                   Like(pay_RecP) Based(Element2@)
080100000000
080200030603      /FREE
080300030603        // Sort descending sequence
080400000000       Select;
080500000000       When Element1 > Element2;
080600000000         Return Low;
080700000000       When Element1 < Element2;
080800000000         Return High;
080900000000       Other;
081000000000         Return Equal;
081100000000       EndSl;
081200000000
081300000000      /END-FREE
081400000000     P                 E
