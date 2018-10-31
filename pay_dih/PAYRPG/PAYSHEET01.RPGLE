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
001300000000     Fpayrdefl01if   e           k disk
001400000000     Fpaycomm   if   e           k disk
001500000000     Fpaysuspl01uf   e           k disk    commit
001600000000      //
001700000000     Fpayhis    o    e             disk    commit
001800000000     Fpaytrans  o    e             disk    commit
001900080221     F*paynrdl01 o    e             disk    commit
002000000000
002100000000     Fpayslipf  o    e             printer usropn
002200000000     Fpayslipfl o    e             printer usropn
002300070315
002400000000      /copy genPR
002500000000      // Payroll PROTOTYPES
002600000000      /copy *libl/payrpg,PAYPR
002700000000      /Copy *libl/payrpg,CsortPr
002800050525      /copy empPR
002900041206
003000041206     D GetAllYtd       pr
003100000000      // Prototypes for compare routines for sort
003200000000
003300000000     D SeqStruc        Pr            10I 0
003400000000     D  Element1@                      *   Value
003500000000     D  Element2@                      *   Value
003600000000      //
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
009200070315
009300000000     D coname          s             30
009400000000     D @date           s             10
009500070315
009600000000     D totduc          s                   like(total_gross)
009700000000     D tax_gross       s                   like(total_gross)
009800000000     D actear          s                   like(total_gross)
009900000000     D insern          s                   like(total_gross)
010000000000     D com_contrib     s                   like(total_gross)
010100000000     D net             s                   like(total_gross)
010200000000     D trn_check       s              2  0
010300000000     D nrhrs           s              5  2
010400000000     D othrs           s              5  2
010500000000     D pay_codeHld     s              1
010600000000     D payroll_type    s              1
010700000000     D mimic_type      s              1
010800000000     D py_style        s              1  0
010900000000     D ytd_opt         s              1  0
011000000000     D rate_flag       s              1
011100000000     D bank_rtype      s              1
011200000000     D origin          s              3    inz('PAY')
011300000000     D datefield       s               d   datfmt(*iso)                         TOTAL AMOUNT
011400000000     D payroll_date    s              8  0
011500000000     D system_date     s              8  0
011600000000     d saved_empcnt    s                   like(ycnt)
011700000000     D counter         s              3  0
011800000000     d HoursFound      s               n
011900131029     D cat_flag        s              1    inz(' ')
012000050525     D countr          s              5  0
012100070315
012200000000     D gross_text      c                   'Total Gross'
012300000000     D deduc_text      c                   'Total Deductions'
012400000000     d rate_cnst       c                   'Hrs@Rate'
012500000000     d net_cnst        c                   'Net amount'
012600000000     d emol_cnst       c                   'Emoluments'
012700000000     d ded_cnst        c                   'Deductions'
012800000000     d ytd_cnst        c                   'Y.T.D.'
012900000000     D tax_code        c                   61
013000000000     D nis_code        c                   63
013100000000     d one             c                   1
013200000000     d fld_count       c                   9
013300000000     D limit           c                   5
013400000000     D blank_text      c                   'Gross'
013500000000
013600000000      // total-fields definition
013700000000
013800000000     D indptr          s               *   inz(%addr(*in))
013900070315
014000000000     D                 ds                  based(indptr)
014100000000     D ytd_notfound            1      1n
014200070315
014300000000     Iemprec
014400000000     I                                          dept          L2
014500000000
014600000000      // Set up initial memory allocation for Struc of StrucMax (50) elements
014700000000      /FREE
014800000000       Memory = %Size(pay_rec) * StrucMax;
014900000000      /END-FREE
015000000000     C                   Alloc     Memory        Struc@
015100000000      //
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
016700000000      /FREE
016800000000       chain payroll_type prtyfmt;
016900000000       if prtyprno = prtyproll#;
017000000000         prtyproll# = 0;
017100040621         prtyyear += 1;
017200000000       endif;
017300000000
017400000000       // check for suspended payment
017500040204       chain (emp:payroll_type)  susfmt;
017600070919       if  %found  and  susedate >= prtyndate
017700070919             and  sussdate <= prtyndate;
017800000000
017900000000         susprd = susprd - 1;
018000000000         //
018100000000         if susprd <= *zeros;
018200000000           delete susfmt;
018300000000         else;
018400000000           update susfmt;
018500000000         endif;
018600000000         //
018700000000       else;
018800000000
018900000000         // Get Division and Bank account if found
019000000000         if mimic_type <> *blank;
019100000000           bank_rtype = mimic_type;
019200000000         else;
019300000000           bank_rtype = payroll_type;
019400000000         endif;
019500000000         //
019600040204         //  get employee bank account
019700040204         chain (emp:bank_rtype)  pbfmt;
019800000000
019900000000         // Get Employee Name
020000000000         emp_name = %trim(christ) +
020100000000             ' ' + %triml(surnam);
020200000000         //-------------------------------------------------------------------------
020300000000
020400000000         reset xcnt;
020500000000         //
020600030604      /end-free
020700030604     C                   clear     *all          pay_Rec
020800030604      /FREE
020900030604         // clear *all pay_Rec;
021000041206         clear paytypeArr;
021100030604         clear *all pay_Recp;
021200000000         clear totduc;
021300000000         clear total_gross;
021400000000         clear tax_gross;
021500041206
021600041206         //**********************************************
021700000000         //* GET EMPLOYEE TRANSACTIONS ***
021800041206         //**********************************************
021900041206         //
022000000000         if get_emp_trans(emp:%addr(pay_Rec):
022100000000               xcnt:totduc:tax_gross:
022200000000               total_gross);
022300000000
022400041206           //********************************************                   *
022500041206           //* P.A.Y.E. CALCULATIONS  ROUTINE. ***
022600041206           //********************************************                   *
022700041206           //
022800000000           trn_check = tax_code;
022900000000           chain trn_key prdffmt;
023000000000           if %found and prdfflag = 'Y';
023100000000
023200000000             calc_emp_tax(emp:frpay:tax_gross
023300000000                 :totduc:prtycycode
023400000000                 :prtyproll#:prtyyear
023500000000                 :prtycycqty:status
023600041106                 :%addr(pay_rec):xcnt:emply:bank_rtype);
023700000000           endif;
023800000000           //****************************************************************
023900000000           //* N.I.S. CALCULATIONS ***
024000000000           //****************************************************************
024100000000           trn_check = nis_code;
024200000000           chain trn_key prdffmt;
024300000000           if %found and prdfflag = 'Y';
024400000000             if tax_gross > *zero;
024500000000               actear = tax_gross;
024600000000               calc_emp_nis(actear:insern:totduc:
024700000000                   com_contrib:birth:prtycycode:
024800000000                   prtycycqty:
024900131029                   %addr(pay_rec):xcnt:emp:cat_flag);
025000080212
025100080212                   // write data to company contrib file(paycomnis)
025200080221          // pnissts = 'A';
025300080221          // pnisemp = emp;
025400080221          // pnisrdate = system_date;
025500080221          // pnispdate = payroll_date;
025600080221          // pnisndate = prtyndate;
025700080221          // pnisamt  = com_contrib;
025800080212             // Write Correct run type for pay transaction file
025900080221          // pnistype = bank_rtype;
026000080212             //
026100080220            // pnrcode = pay_code;
026200080221          // pnistcode = nis_code;
026300080221          // write pnisfmt;
026400080212                  //
026500000000             endif;
026600040622
026700040622            // Get payroll restrictions/parameters
026800040622            chain payroll_type prrefmt;
026900040622
027000040622             //* Declare earnings as being X.XX % more than insurable earnings
027100040622             if actear > insern
027200040622               and prredeclr > *zeros;
027300040622              actear = insern + ((prredeclr * insern) / 100);
027400000000             endif;
027500000000
027600000000           endif;
027700040622
027800040622
027900040621          //   net = total_gross - totduc;
028000040622
028100040622           // Handle payroll guaranteed percentage etc.
028200040621           select;
028300040621
028400040621           when prreguar > 0;
028500040621              Max_Gross = round_up(total_gross *
028600040621                          (100 - prreguar) /100:'A');
028700040621              net = Max_gross - totduc;
028800040621            if (%abs(Max_gross) - %abs(totduc)) <
028900040621                 *zeros;
029000040621              adj_pay_credit(net:totduc:
029100040621                  %addr(pay_rec):xcnt);
029200040621            endif;
029300040621              net += total_gross - Max_gross;
029400040621
029500040621           other;
029600040621            net = total_gross - totduc;
029700040621            if (%abs(total_gross) - %abs(totduc)) <
029800040621                 *zeros;
029900040621              adj_pay_credit(net:totduc:
030000040621                  %addr(pay_rec):xcnt);
030100040621            endif;
030200040621           endsl;
030300040621
030400000000           //* Summary of employee transactions
030500000000           exsr sum_trans;
030600000000           //
030700000000           //* Figures
030800000000           //
030900000000           phisemp = emp;
031000000000           phisdept = dept;
031100000000           phisdiv = divis;
031200000000           phisloc = locat;
031300100218           phisgpay = round_up(total_gross:'A');
031400100218           phisaearn = round_up(actear:'A');
031500100218           phisnpay = round_up(net:'A');
031600070315
031700000000           phisfnight = prtyproll# + 1;
031800070315
031900100218           phisiearn = round_up(insern:'A');
032000000000           phissts = 'A';
032100131029           phiswcate = cat_flag;
032200000000           phisrdate = system_date;
032300000000           phispdate = payroll_date;
032400000000           phisndate = prtyndate;
032500000000           get_prd_info('*prd':*blanks:phisprno:
032600000000               prtyndate);
032700000000           // Write Correct run type for pay history file
032800000000           phistype = bank_rtype;
032900070315
033000000000           write phisfmt;
033100000000           clear phisfmt;
033200000000
033300000000           // Write /Update YTD record
033400000000           //       Taxable Gross (pay_type = 0)
033500000000           pay_type = *zeros;
033600000000           ytdamt = *zeros;
033700000000           chain ytd_key ytdrec;
033800000000           *IN01 = NOT %FOUND;
033900000000
034000100218           ytdamt += round_up(tax_gross:'A');
034100000000           if ytd_notfound;
034200000000             year = prtyyear;
034300000000             ytdtype = pay_type;
034400000000             ytdorg = origin;
034500000000             write ytdrec;
034600000000           else;
034700000000             update ytdrec;
034800000000           endif;
034900000000
035000000000           // Print Paycheck
035100000000        select;
035200000000         when py_style = 1;
035300041206            if pcallytd = 'Y';
035400041206               getallytd();
035500041206            endif;
035600000000           exsr prnt_horizontal;
035700000000         when py_style = 2;
035800041206            if pcallytd = 'Y'
035900041206              and  ytd_opt = 3;
036000041206               getallytd();
036100041206            endif;
036200000000           exsr prnt_vertical;
036300000000         endsl;
036400041206
036500030603        // Clear fields used for ytd files  (temp work)
036600000000           actear = *zeros;
036700000000           insern = *zeros;
036800000000           clear ytdrec;
036900000000
037000000000           // end of valid transaction
037100000000         endif;
037200000000         // end of valid selection - suspended payments
037300000000       endif;
037400000000       //*********************************************************************
037500000000       // Payroll Output
037600000000       // output Payroll Info. to History/Y.T.D. files
037700000000       //*********************************************************************
037800000000       begsr sum_trans;
037900000000         if xcnt <> *zeros;
038000000000           //
038100000000           reset ycnt;
038200000000           dow ycnt <= xcnt;
038300000000      /END-FREE
038400000000     C     ycnt          occur     pay_Rec
038500000000     C     ycnt          occur     pay_Recp
038600000000      /FREE
038700050525            get_payty_info('RD':pay_type:payty_struc);
038800000000             // save values for payslip output
038900000000             pay_codeP = pay_code;
039000000000             pay_hrsP = pay_hrs;
039100000000             pay_amtP = pay_amt;
039200000000             pay_descP = ptydesc;
039300000000             pay_shdsP = ptyshrtnm;
039400041206
039500041206             paytypeArr(ycnt) = pay_type;
039600000000
039700000000             // if is emoluments
039800000000             if pay_code = 'E';
039900000000               select;
040000000000                 // Overtime
040100000000               when pay_type = 2;
040200000000                 phisothrs = phisothrs + pay_hrs;
040300000000                 // Normal time
040400000000               other;
040500000000                 phisnthrs = phisnthrs + pay_hrs;
040600000000               endsl;
040700000000
040800000000             endif;
040900000000             // Write to transaction History File
041000000000             ptrnsts = 'A';
041100000000             ptrnemp = emp;
041200000000             ptrnrdate = system_date;
041300000000             ptrnpdate = payroll_date;
041400000000             ptrnndate = prtyndate;
041500000000             ptrnhrs  = pay_hrs;
041600100218             ptrnamt  = ROUND_UP(pay_amt:'A');
041700000000             // Write Correct run type for pay transaction file
041800000000             ptrntype = bank_rtype;
041900000000             //
042000000000             ptrncode = pay_code;
042100000000             ptrntcode = pay_type;
042200131029             ptrnwcate = cat_flag;
042300000000             write ptisfmt;
042400000000
042500000000             ytdamt = *zeros;
042600000000             chain ytd_key ytdrec;
042700000000             *IN01 = NOT %FOUND;
042800000000             // Write /Update YTD record
042900000000             ytdamt = ytdamt + pay_amt;
043000100218             ytdamt = round_up(ytdamt:'A');
043100000000             if ytd_notfound;
043200000000               year = prtyyear;
043300000000               ytdtype = pay_type;
043400000000               ytdorg = origin;
043500000000               write ytdrec;
043600000000             else;
043700000000               update ytdrec;
043800000000             endif;
043900000000
044000000000             pay_ytdP = ytdamt;
044100000000             ycnt = ycnt + 1;
044200000000           enddo;
044300000000           //
044400000000           //
044500000000         endif;
044600000000
044700000000       endsr;
044800000000       //*********************************************************************
044900000000       // Payroll Output
045000000000       // output Payroll Info. Printer File
045100000000       //*********************************************************************
045200000000       begsr prnt_vertical;
045300000000
045400000000           if  xcnt <> *zeros;
045500000000          // Sort data structure
045600000000            %occur(pay_recP) = 1;
045700000000            SortIt(%Addr(pay_recP) : xcnt :
045800000000                             %Size(pay_RecP) : %PAddr('SEQSTRUC'));
045900000000        // Get Bank name from division file for Payslip
046000000000          chain  divis divfmt;
046100000000          bank_name = %trim(dtname) + ' :';
046200000000          prtyndate# = %date(prtyndate) - %days(1);
046300000000        // employee  header record
046400000000                 write  header;
046500000000
046600000000           reset ycnt;
046700000000           pay_codeHld = 'E';
046800000000           dow  ycnt <= xcnt;
046900000000            %occur(pay_recP) = ycnt;
047000000000
047100000000        // if Paycode changes (E= emoluments,D= deductions)
047200000000               if   pay_codeP <> pay_codeHld;
047300000000                   sub_text = gross_text;
047400000000                   sub_tot  = total_gross;
047500000000        // employee transaction summary total for Emoluments
047600000000                   write  sub_tot_rc;
047700000000                   pay_codeHld = pay_codeP;
047800000000                endif;
047900000000
048000000000        // employee Hourly Rate
048100000000            exsr get_hoursRate;
048200000000
048300000000             pay_amt =  pay_amtP;
048400000000            if ytd_opt = 3;
048500000000             ytd_amt =  pay_ytdP;
048600000000             ptyshnm =  pay_shdsP;
048700000000             write  det_recd2;
048800000000              else;
048900000000             ptydesc =  pay_descP;
049000000000             write  det_recd;
049100000000            endif;
049200000000
049300000000             ycnt = ycnt + 1;
049400000000          enddo;
049500000000
049600000000             if  totduc <> *zeros;
049700000000                sub_text = deduc_text;
049800000000                sub_tot  = totduc;
049900000000          // employee transaction summary total for DEDUCTIONS
050000000000                write  sub_tot_rc;
050100000000              endif;
050200000000
050300000000          // final employee record
050400000000            write  fintotal;
050500000000          // employee Year-to-date Header record
050600000000            if ytd_opt <> 3;
050700000000            write  ytdrcd;
050800000000
050900000000           clear YPrtArr;
051000000000           clear Yamount;
051100000000           saved_empcnt = *zeros;
051200000000           select;
051300000000            when  ytd_opt = 1;
051400000000              for counter = 1 to limit;
051500000000
051600000000                     if dft_list(counter) = 0;
051700000000                       if  counter <> 1;
051800000000                         iter;
051900000000                       endif;
052000000000                       eval  ptyshrtnm = blank_text;
052100000000                         else;
052200050525                     get_payty_info('RD':dft_list(counter):payty_struc);
052300000000                     endif;
052400000000                        pay_type = dft_list(counter);
052500000000                        ytdamt = *zeros;
052600000000                        chain ytd_key ytdrec;
052700000000                     if  %found(payeytdl01);
052800000000                         saved_empcnt = saved_empcnt + 1;
052900000000                         YPrtArr(saved_empcnt) =  %trim(ptyshrtnm);
053000000000                         Yamount(saved_empcnt) = %char(ytdamt);
053100000000                     endif;
053200000000              endfor;
053300000000                   exsr print_ytd_record;
053400000000
053500000000           when  ytd_opt = 2;
053600000000            saved_empcnt = 0;
053700050525            countr = *zeros;
053800050525            dow not get_payty_info('RD':-1:payty_struc:countr);
053900000000              pay_type = ptyptype;
054000000000              ytdamt = *zeros;
054100000000              chain ytd_key ytdrec;
054200000000               if %found(payeytdl01);
054300000000                saved_empcnt = saved_empcnt + 1;
054400000000                  // if end of line reached
054500000000                  if   saved_empcnt > limit;
054600000000                   exsr print_ytd_record;
054700000000                  endif;
054800000000                YPrtArr(saved_empcnt) =  %trim(ptyshrtnm);
054900000000                Yamount(saved_empcnt) = %char(ytdamt);
055000000000               endif;
055100000000            enddo;
055200000000            exsr print_ytd_record;
055300000000
055400000000         endsl;
055500000000         endif;
055600000000
055700000000
055800030603        // Print any comments which had been set up by pay clerk
055900000000            exsr  prnt_comt;
056000000000
056100000000          endif;
056200000000
056300000000        endsr;
056400000000
056500000000       //** Payroll Output
056600000000       //** output Payroll Info. Printer File  Horizontal
056700000000       //********************************************************
056800000000       begsr prnt_horizontal;
056900000000
057000000000          if  xcnt <> *zeros;
057100000000        //** Sort data structure
057200000000            %occur(pay_recP) = 1;
057300000000            SortIt(%Addr(pay_recP) : xcnt :
057400000000                             %Size(pay_RecP) : %PAddr('SEQSTRUC'));
057500000000        // Get Bank name from division file for Payslip
057600000000          chain  divis divfmt;
057700000000          bank_name = %trim(dtname) + ' :';
057800000000          prtyndate# = %date(prtyndate) - %days(1);
057900000000          nis_num  = nisnum;
058000000000          emp_doe  = %date(emply:*eur);
058100000000          dname  = get_dept_info(dept);
058200000000
058300000000        // employee  header record
058400000000        write  headerH;
058500000000
058600000000         pay_codeHld = 'E';
058700000000         saved_empcnt = 1;
058800000000         clear pPrtArr;
058900000000         pay010 = *blanks;
059000000000
059100000000         for  ycnt  = 1 to xcnt;
059200000000
059300000000                  saved_empcnt = saved_empcnt + 1;
059400000000                  %occur(pay_RecP) = ycnt;
059500000000              // employee Hourly Rate
059600000000                  exsr get_hoursRate;
059700000000
059800000000                  // if changes from pay to deduct
059900000000          if  pay_codeP = pay_codeHld;
060000000000
060100000000                  // if end of line reached
060200000000                if   saved_empcnt > fld_count;
060300000000
060400000000                     typ_total = *zeros;
060500000000                     exsr Print_payslip_detail;
060600000000                     saved_empcnt = 2;
060700000000                     clear pPrtArr;
060800000000                     clear pytd;
060900000000                     clear pamount;
061000000000                     clear prate;
061100000000                     HoursFound = '0';
061200000000                endif;
061300000000
061400000000                  // if changes from pay to deduct
061500000000                  else;
061600000000                       saved_empcnt = 2;
061700000000                       typ_total = total_gross;
061800000000                       exsr Print_payslip_detail;
061900000000                       pay_codeHld = pay_codeP;
062000000000                       clear pPrtArr;
062100000000                       clear pytd;
062200000000                       clear pamount;
062300000000                       clear prate;
062400000000                       HoursFound = '0';
062500000000                endif;
062600000000
062700000000                 pPrtArr(saved_empcnt) = pay_shdsP;
062800000000                 prate(saved_empcnt) = %editc(pay_hrsP:'2');
062900000000                 if pay_rate <> *zeros;
063000000000                  prate(saved_empcnt) = %trim(prate(saved_empcnt))
063100000000                        + '@' + %trim(%editc(pay_rate:'2'));
063200000000                 endif;
063300000000                 pamount(saved_empcnt) = %char(pay_amtP);
063400000000                 pytd(saved_empcnt) = %char(pay_ytdP);
063500000000
063600000000         endfor;
063700000000
063800030603        // end of pay info
063900000000
064000000000                   typ_total = totduc;
064100000000                   exsr Print_payslip_detail;
064200000000
064300000000                       clear pPrtArr;
064400000000                       clear pytd;
064500000000                       clear pamount;
064600000000                       clear prate;
064700000000                       HoursFound = '0';
064800000000                       pPrtArr(fld_count) =  net_cnst;
064900000000                       pPrtArr(one) =  *blank;
065000000000
065100000000          // final employee record
065200000000                       pay010 = %char(net);
065300000000
065400000000                       write detail;
065500000000
065600000000          // Print any comments which had been set up by pay clerk
065700000000                         exsr   prnt_comt;
065800000000
065900000000                   endif;
066000000000
066100000000        endsr;
066200000000
066300030603        //*********************************
066400030603        // StartUp subroutine
066500030603        //*********************************
066600030603           // begsr *inzsr;
066700030603      /END-FREE
066800030603
066900030603     C     *inzsr        begsr
067000000000     C     *entry        plist
067100000000     C                   parm                    payroll_type
067200000000     C                   parm                    payroll_date
067300000000     C                   parm                    mimic_type
067400000000     C                   parm                    rate_flag
067500000000     C                   parm                    py_style
067600000000     C                   parm                    ytd_opt
067700030603
067800000000      /free
067900000000         // Get company name
068000000000           coname = get_coname;
068100000000           @date = get_sys_date(system_date);
068200000000           open paycon;
068300000000           read paycon;
068400000000            if not(%eof);
068500000000                pcdftytdDS = pcdftytd;
068600000000            endif;
068700000000          close paycon;
068800000000
068900000000           select;
069000000000            when py_style = 1;
069100000000              open payslipfl;
069200000000            when py_style = 2;
069300000000              open payslipf;
069400000000            endsl;
069500000000      /end-free
069600030603         //*                 eval      @title = ARR(1)
069700000000      /FREE
069800000000       endsr;
069900030603
070000000000            begsr  prnt_comt;
070100000000                 chain payroll_type pcomfmt;
070200000000                   if not(%found(paycomm));
070300000000                     setll *loval pcomfmt;
070400000000                        read pcomfmt;
070500000000                      if %eof(paycomm)
070600000000                            or pcomtype  <> ' ';
070700000000                        leavesr;
070800000000                      endif;
070900000000                   endif;
071000000000
071100000000               if   py_style = 2;
071200000000                   if pcomdesc1 <> *blanks;
071300000000                   com_fld = pcomdesc1;
071400000000                    write blnk_rcd;
071500000000                    write com_rcd;
071600000000                 endif;
071700000000
071800000000                   if pcomdesc2 <> *blanks;
071900000000                   com_fld = pcomdesc2;
072000000000                    write com_rcd;
072100000000                 endif;
072200000000
072300000000                   if pcomdesc3 <> *blanks;
072400000000                   com_fld = pcomdesc3;
072500000000                    write com_rcd;
072600000000                 endif;
072700000000                else;
072800000000                // First Line
072900000000                   com_fldh = %trim(%subst((
073000000000                         %trim(pcomdesc1) + ' ' +
073100000000                         %trim(pcomdesc2) + ' ' +
073200000000                         %trim(pcomdesc3)):one:%size(com_fldh)));
073300000000                    if com_fldH <> *blanks;
073400000000                      write com_rcdh;
073500000000                    endif;
073600000000                // Second Line
073700000000                   com_fldh = %trim(%subst((
073800000000                         %trim(pcomdesc1) + ' ' +
073900000000                         %trim(pcomdesc2) + ' ' +
074000000000                         %trim(pcomdesc3)):%size(com_fldh)+ 1));
074100000000                    if com_fldH <> *blanks;
074200000000                      write com_rcdh;
074300000000                    endif;
074400000000               endif;
074500000000            endsr;
074600000000         //------------------------------
074700000000         // Get calculate Hours/Rate
074800000000         //------------------------------
074900000000        begsr get_hoursRate;
075000000000
075100000000          // if hours not zero, check if rates flag set.
075200000000          // If it is,print rates else blank it
075300000000                if  pay_hrsp <> *zeros;
075400000000                     HoursFound = '1';
075500000000
075600000000                    if  rate_flag = 'Y';
075700000000                        pay_rate  = pay_amtp  / pay_hrsp;
075800000000                         else;
075900000000                        pay_rate  =  0;
076000000000                    endif;
076100000000
076200000000                      else;
076300000000                     pay_rate =  0;
076400000000                  endif;
076500000000        endsr;
076600000000       //-----------------------------
076700000000       // Print payslip Detail lines
076800000000       //-----------------------------
076900000000           begsr print_payslip_detail;
077000000000                  for  counter  = 1 to 3;
077100000000                   write  detail;
077200000000                   pay010 = *blanks;
077300000000                     select;
077400000000                      when counter = 1;
077500000000                    // Print any Rates
077600000000                     if pay_codeHld = 'E' and
077700000000                           HoursFound;
077800000000                          pPrtArr = prate;
077900000000                          pPrtArr(one) = rate_cnst;
078000000000                          write  detail;
078100000000                        endif;
078200000000                       pPrtArr = pamount;
078300000000                       pay010 = %char(typ_total);
078400000000
078500000000                     if pay_codeHld = 'E';
078600000000                           pPrtArr(one) = emol_cnst;
078700000000                            else;
078800000000                           pPrtArr(one) = ded_cnst;
078900000000                     endif;
079000000000                      when counter = 2;
079100000000                       pPrtArr = pytd;
079200000000                       pPrtArr(one) = ytd_cnst;
079300000000                     endsl;
079400000000                endfor;
079500000000           endsr;
079600000000       //-----------------------------
079700000000       // Print payslip YTD Detail
079800000000       //-----------------------------
079900000000         begsr print_ytd_record;
080000000000
080100000000                  // Employee Year-to-date Detail records
080200000000                  for  counter = 1 to 2;
080300000000                     write  ytd_det;
080400000000                      if counter = 1;
080500000000                       YPrtArr = Yamount;
080600000000                      endif;
080700000000                endfor;
080800000000                     saved_empcnt = 1;
080900000000                     clear YPrtArr;
081000000000                     clear Yamount;
081100000000
081200000000                endsr;
081300000000
081400030603        //
081500030603        //*******************************************************************
081600030603        //* Payroll SubProcedures  ***
081700030603        //*******************************************************************
081800030603        //*******************************************************************
081900030603        //**  Sequence data struc /array ** descending
082000030603        //*******************************************************************
082100030603      /end-free
082200000000     P SeqStruc        B
082300000000
082400000000     D                 PI            10I 0
082500000000     D  Element1@                      *   Value
082600000000     D  Element2@                      *   Value
082700000000
082800000000     D Element1        S                   Like(pay_RecP) Based(Element1@)
082900000000     D Element2        S                   Like(pay_RecP) Based(Element2@)
083000000000
083100030603      /FREE
083200030603        // Sort descending sequence
083300000000       Select;
083400000000       When Element1 > Element2;
083500000000         Return Low;
083600000000       When Element1 < Element2;
083700000000         Return High;
083800000000       Other;
083900000000         Return Equal;
084000000000       EndSl;
084100000000
084200000000      /END-FREE
084300000000     P                 E
084400041206       //************************************                               **
084500041206       // Get all payroll ytd. figures
084600041206       //************************************                               **
084700041206     P GetAllYtd       B
084800041206     D GetAllYtd       PI
084900041206
085000041206      /free
085100041206         if xcnt <> *zeros;
085200041206
085300050525            countr = *zeros;
085400050525            dow not get_payty_info('RD':-1:payty_struc:countr);
085500050525
085600041206             if  %lookup(ptyptype:paytypeArr) = *zeros;
085700041206
085800041206             // Get YTD record
085900041206               chain (emp:prtyyear:ptyptype:origin) ytdrec;
086000041206                if  %found and  ytdamt <> *zeros;
086100041214                 chain (payroll_type:ptyptype) prdffmt;
086200041216                 if %found and prdfflag = 'Y';
086300041206
086400041206                 // save values for payslip output
086500041206                  xcnt += 1;
086600041206                  %occur(pay_recp) = xcnt;
086700041206                  pay_codeP = ptyrtype;
086800041206                  pay_hrsP = *zeros;
086900041206                  pay_amtP = *zeros;
087000041206                  pay_descP = ptydesc;
087100041206                  pay_shdsP = ptyshrtnm;
087200041206                  pay_ytdP = ytdamt;
087300041206
087400041216                 endif;
087500041206                endif;
087600041206             endif;
087700041206
087800041206           enddo;
087900041206
088000041206         endif;
088100041206
088200041206      /end-free
088300041206     PGetAllYTD        E
