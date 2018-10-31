000100170726     H DECEDIT('.') DATEDIT(*ymd/)  alwnull(*usrctl)
000200011205     **
000300011205     ** EMPLOYEES PAYROLL
000400011205     ** PROGRAMMERS---C.Williams ,S.BRITTON
000500011205     **
000600010809     Fempmast   ip   e           k disk
000700010419     Fpaybank   if   e           k disk
000800010626     Fpaydiv    if   e           k disk
000900030228     Fpaycon    if   e             disk    usropn
001000011205     Fpayhisl03 if   e           k disk
001100011205     Fpaytranl06if   e           k disk
001200030228     Fpayhisl11 if   e           k disk    rename(phisfmt:phisfmty)
001300030228     Fpaytranl03if   e           k disk    rename(ptisfmt:ptisfmty)
001400041214     F                                     prefix(x)
001500041213     Fpayrdefl01if   e           k disk
001600011205     **
001700030227     Fpayslipf  o    e             printer usropn
001800030227     Fpayslipfl o    e             printer usropn
001900030228     **
002000131030      /copy qcpysrc,genPR
002100030228     ** Payroll PROTOTYPES
002200131030      /copy qcpysrc,PAYPR
002300131030      /copy qcpysrc,EMPPR
002400010215     **
002500030228     D                 ds
002600030228     D  pcdftytdDS                   10
002700030228     D dft_list                       2  0 dim(5) overlay(pcdftytdDS)
002800030227     **
002900030227     dpy_detailH       ds
003000030227     d  pay001                       10
003100030227     d  pay002                       10
003200030227     d  pay003                       10
003300030227     d  pay004                       10
003400030227     d  pay005                       10
003500030227     d  pay006                       10
003600030227     d  pay007                       10
003700030227     d  pay008                       10
003800030227     d  pay009                       10
003900030227     **
004000030227     dytd_detail       ds
004100030227     d  ytd001                       14
004200030227     d  ytd002                       14
004300030227     d  ytd003                       14
004400030227     d  ytd004                       14
004500030227     d  ytd005                       14
004600041211
004700041213      // Prototypes for pgm
004800041213     Dget_hoursRate    pr
004900041213     Dpaygutsv         pr
005000041213     Dpayguts          pr
005100041213     Dpayallgutsv      pr
005200041213     Dpayallguts       pr
005300041213     Dprint_slip_det   pr
005400041211
005500030228      *-- Sum ytd --*                                                       --
005600030228     DSum_ytd          pr            11  2
005700030228     D                                2  0 value
005800030228     D                                6  0 value
005900030228
006000030228     **
006100030228     D coname          s             30
006200030228     D @date           s             10
006300011205     D total_gross     s             11  2
006400030228     d typ_total       s             11  2
006500030228     D ytdamt          s             11  2
006600010420     D totduc          s                   like(total_gross)
006700030303     D pay_type        s              2  0
006800010613     D pay_codeHld     s              1
006900010725     D payroll_type    s              1
007000011126     D rate_flag       s              1
007100030227     D py_style        s              1  0
007200030227     D ytd_opt         s              1  0
007300010725     D payroll_date    s              8  0
007400030228     D system_date     s              8  0
007500030228     D ycnt            s              3  0 inz(1)
007600030228     d saved_empcnt    s                   like(ycnt)
007700020117     D log_date        s              8  0
007800011205     D datefield       s               d   datfmt(*iso)                         TOTAL AMOUNT
007900011205     D reprnt          s              8    inz(' REPRINT')                      TOTAL AMOUNT
008000030227     d ytdptr          s               *   inz(%addr(ytd_detail))
008100030227     d recptr          s               *   inz(%addr(py_detailH))
008200030227     d pprtArr         s             10    dim(09) based(recptr)
008300030227     d prate           s             10    dim(09)
008400030227     d pAmount         s             10    dim(09)
008500030227     d pytd            s             10    dim(09)
008600030227     d YprtArr         s             14    dim(05) based(ytdptr)
008700030227     d YAmount         s             14    dim(05)
008800030228     D counter         s              3  0
008900050525     D countr          s              5  0
009000030228     d HoursFound      s               n
009100011205     **
009200030228     D gross_text      c                   'Total Gross'
009300030228     D deduc_text      c                   'Total Deductions'
009400030228     d rate_cnst       c                   'Hrs@Rate'
009500030228     d net_cnst        c                   'Net amount'
009600030228     d emol_cnst       c                   'Emoluments'
009700030228     d ded_cnst        c                   'Deductions'
009800030228     d ytd_cnst        c                   'Y.T.D.'
009900030228     d one             c                   1
010000030228     d fld_count       c                   9
010100030228     D limit           c                   5
010200030228     D blank_text      c                   'Gross'
010300030228     D comment_txt     c                   'Re-printed Payslip'
010400010420
010500010511
010600010419     **
010700010418
010800011205     ** Key to select transaction records
010900010427     C     trn_key       klist
011000010427     C                   kfld                    payroll_type
011100011205     C                   kfld                    emp
011200011205     C                   kfld                    payroll_date
011300011205     ** Key to select History records
011400011205     C     trn_key2      klist
011500011205     C                   kfld                    payroll_date
011600011205     C                   kfld                    payroll_type
011700011205     C                   kfld                    emp
011800010730
011900990412     **-------------------------------------------------------------------------
012000990412     ** Key to get employee bank account
012100990412     C     bnk_key       klist
012200990412     C                   kfld                    emp
012300011205     C                   kfld                    payroll_type
012400010730
012500010730
012600010824     **
012700010809     C     bnk_key       chain     pbfmt
012800010613
012900010613     ** Get Employee Name
013000010613     C                   eval      emp_name = %trim(christ) +
013100010613     C                             ' ' + %triml(surnam)
013200990412     **-------------------------------------------------------------------------
013300010511
013400010621     **
013500011205     C                   clear                   net
013600010621     C                   clear                   total_gross
013700951207     **
013800951207     ******************************************************************
013900010420     *** GET EMPLOYEE TRANSACTIONS ***
014000951207     ******************************************************************
014100011205
014200030424      /free
014300030424        // Get gross and net pay
014400030424           setll trn_key2  phisfmt;
014500030424           reade trn_key2  phisfmt;
014600030424           if  not(%eof);
014700030424
014800030424           dow  not(%eof);
014900030424                   if  log_date = *zeros or
015000030424                          log_date = phisrdate;
015100030424                      total_gross = total_gross + phisgpay;
015200030424                      net = net + phisnpay;
015300030424                   endif;
015400030424           reade trn_key2  phisfmt;
015500030424           enddo;
015600030424          ptrnpdate = phispdate;
015700030424          totduc = total_gross - net;
015800011205
015900011205
016000030424         //*****************************************************************
016100030424         //** Payroll Output
016200030424         //** output Payroll Info. Printer File
016300030424         //*****************************************************************
016400030424         //** Print Paycheck
016500030424
016600030227        select;
016700030227         when py_style = 1;
016800030227           exsr prnt_horizontal;
016900030227         when py_style = 2;
017000030227           exsr prnt_vertical;
017100030227         endsl;
017200030424
017300030424         endif;
017400030228
017500030424       //*******************************************************************
017600030424       //** Payroll Output
017700030424       //** output Payroll Info. Printer File
017800030424       //*******************************************************************
017900030424
018000030227       begsr prnt_vertical;
018100030227
018200030228
018300030227        // Get Bank name from division file for Payslip
018400030227          chain  divis divfmt;
018500030227          bank_name = %trim(dtname) + ' :';
018600030228          prtyndate# = %date(payroll_date) - %days(1);
018700030228
018800030227        // employee  header record
018900030227                 write  header;
019000030227
019100030228        // reset ycnt;
019200030227           pay_codeHld = 'E';
019300041213
019400041213           if pcallytd = 'Y'
019500041213             and ytd_opt = 3;
019600041213             payallgutsV();
019700041213             else;
019800041213             paygutsV();
019900041213           endif;
020000030227
020100030227             if  totduc <> *zeros;
020200030227                sub_text = deduc_text;
020300030227                sub_tot  = totduc;
020400030227          // employee transaction summary total for DEDUCTIONS
020500030227                write  sub_tot_rc;
020600030227              endif;
020700030227
020800030227          // final employee record
020900030227            write  fintotal;
021000030227          // employee Year-to-date Header record
021100030227            if ytd_opt <> 3;
021200030227            write  ytdrcd;
021300030227
021400030227           clear YPrtArr;
021500030227           clear Yamount;
021600030227           saved_empcnt = *zeros;
021700030227           select;
021800030227            when  ytd_opt = 1;
021900030227              for counter = 1 to limit;
022000030227
022100071206                   pay_type = dft_list(counter);
022200071206
022300030227                     if dft_list(counter) = 0;
022400030227                       if  counter <> 1;
022500030227                         iter;
022600030227                       endif;
022700030303                         ptyshrtnm = blank_text;
022800030227                         else;
022900071206                //       pay_type = dft_list(counter);
023000050525                        get_payty_info('RD':pay_type:payty_struc);
023100030227                     endif;
023200030227                         saved_empcnt = saved_empcnt + 1;
023300030301                         YPrtArr(saved_empcnt) = %trim(ptyshrtnm);
023400030228                         Yamount(saved_empcnt) = %char(sum_ytd(
023500030303                                          pay_type:phisprno));
023600030227              endfor;
023700030227                   exsr print_ytd_record;
023800030227
023900030227           when  ytd_opt = 2;
024000050525            saved_empcnt = 0;
024100050525            countr = *zeros;
024200050525            dow not get_payty_info('RD':-1:payty_struc:countr);
024300030228           // pay_type = ptyptype;
024400030228
024500030228              Ytdamt = sum_ytd(ptyptype:phisprno);
024600030228               if Ytdamt <> *zeros;
024700030227                saved_empcnt = saved_empcnt + 1;
024800030227                  // if end of line reached
024900030227                  if   saved_empcnt > limit;
025000030227                   exsr print_ytd_record;
025100030227                  endif;
025200030227                YPrtArr(saved_empcnt) =  %trim(ptyshrtnm);
025300030227                Yamount(saved_empcnt) = %char(ytdamt);
025400030227               endif;
025500030227            enddo;
025600030227            exsr print_ytd_record;
025700030227
025800030227         endsl;
025900030227         endif;
026000030227
026100030227
026200030227       // Print any comments which had been set up by pay clerk
026300030227            exsr  prnt_comt;
026400030227
026500030227
026600030227        endsr;
026700030227
026800030228       //********************************************************
026900030227       //** Payroll Output
027000030227       //** output Payroll Info. Printer File  Horizontal
027100030227       //********************************************************
027200030227       begsr prnt_horizontal;
027300030227
027400030228
027500030227        // Get Bank name from division file for Payslip
027600030227          chain  divis divfmt;
027700030227          bank_name = %trim(dtname) + ' :';
027800030228          prtyndate# = %date(payroll_date) - %days(1);
027900030227          nis_num  = nisnum;
028000030227          emp_doe  = %date(emply:*eur);
028100030227          dname  = get_dept_info(dept);
028200030227
028300030227        // employee  header record
028400030227        write  headerH;
028500030227
028600030227         pay_codeHld = 'E';
028700030227         saved_empcnt = 1;
028800030227         clear pPrtArr;
028900030227         pay010 = *blanks;
029000041213
029100041213         if pcallytd = 'Y';
029200041213            PayAllguts();
029300041213           else;
029400041213            Payguts();
029500041213         endif;
029600030227
029700030227
029800030227       // end of pay info
029900030227
030000030227                   typ_total = totduc;
030100041213                   Print_slip_det();
030200030227
030300030227                       clear pPrtArr;
030400030227                       clear pytd;
030500030227                       clear pamount;
030600030227                       clear prate;
030700030227                       HoursFound = '0';
030800030227                       pPrtArr(fld_count) =  net_cnst;
030900030227                       pPrtArr(one) =  *blank;
031000030227
031100030227          // final employee record
031200030227                       pay010 = %char(net);
031300030227
031400030227                       write detail;
031500030227
031600030227          // Print any comments which had been set up by pay clerk
031700030227                         exsr   prnt_comt;
031800030227
031900030227
032000030227        endsr;
032100030227
032200030227      /end-free
032300030227     ***********************************
032400030227     ** StartUp subroutine
032500030227     ***********************************
032600030227     C     *inzsr        begsr
032700030227     **
032800030227     C     *entry        plist
032900030227     C                   parm                    payroll_type
033000030227     C                   parm                    payroll_date
033100030227     C                   parm                    rate_flag
033200030228     C                   parm                    log_date
033300030227     C                   parm                    py_style
033400030227     C                   parm                    ytd_opt
033500030227     **
033600030227      /free
033700030227         // Get company name
033800030227           coname = get_coname;
033900030227           @date = get_sys_date(system_date);
034000030227           open paycon;
034100030227           read paycon;
034200030227            if not(%eof);
034300030227                pcdftytdDS = pcdftytd;
034400030227            endif;
034500030227          close paycon;
034600030227
034700030227           select;
034800030227            when py_style = 1;
034900030227              open payslipfl;
035000030227            when py_style = 2;
035100030227              open payslipf;
035200030227            endsl;
035300030228
035400030228      /end-free
035500030228     C                   endsr
035600030228      /free
035700030227            begsr  prnt_comt;
035800030227
035900030228               if   py_style = 2;
036000030228                   com_fld = comment_txt;
036100030227                    write blnk_rcd;
036200030227                    write com_rcd;
036300030227
036400030227
036500030227                else;
036600030227                // First Line
036700030228                   com_fldh = comment_txt;
036800030227                    if com_fldH <> *blanks;
036900030227                      write com_rcdh;
037000030227                    endif;
037100030227               endif;
037200030227            endsr;
037300030227       //-----------------------------
037400030227       // Print payslip YTD Detail
037500030227       //-----------------------------
037600030227         begsr print_ytd_record;
037700030227
037800030227                  // Employee Year-to-date Detail records
037900030227                  for  counter = 1 to 2;
038000030227                     write  ytd_det;
038100030227                      if counter = 1;
038200030227                       YPrtArr = Yamount;
038300030227                      endif;
038400030227                endfor;
038500030227                     saved_empcnt = 1;
038600030227                     clear YPrtArr;
038700030227                     clear Yamount;
038800030227
038900030227                endsr;
039000030227      /end-free
039100030228     *********************************
039200030228     ** Summarise YTD Figures       **
039300030228     *********************************
039400030228     Psum_ytd          b
039500030228     **
039600030228     Dsum_ytd          pi            11  2
039700030228     **
039800030228     D in_type                        2  0 value
039900030228     D in_prno                        6  0 value
040000030228     **
040100030228     D str_prno        s              6  0
040200030228     D ytd_val         s             11  2
040300030228     D last_date       s              8  0
040400030304     D wrk_type        s              2  0
040500030228     **
040600030228     C                   eval      str_prno = in_prno
040700030304     C                   eval      wrk_type = in_type
040800030228     C                   move      01            str_prno
040900030228     **
041000030228     C     trn_key       klist
041100030228     C                   kfld                    emp
041200030228     C                   kfld                    str_prno
041300030228     **
041400030228     C     trn_key2      klist
041500030228     C                   kfld                    emp
041600030304     C                   kfld                    wrk_type
041700030228     C                   kfld                    phisndate
041800030228     **
041900030228     C                   eval      ytd_val = *zeros
042000030228     C     trn_key       setll     phisfmty
042100030303     C     emp           reade     phisfmty
042200030228     C                   eval      last_date = *zeros
042300030228     C                   dow       not(%eof(payhisl11))
042400030303     C                   if        phisndate > payroll_date
042500030228     C                   leave
042600030228     C                   endif
042700030228     C                   if        last_date <> phisndate
042800030304     **
042900030304     C                   select
043000030304     C                   when      in_type = *ZEROS
043100030304     C                   exsr      calc_grossytd
043200030304     C                   other
043300030304     C                   exsr      calc_ytd
043400030304     C                   endsl
043500030304     **
043600030228     C                   eval      last_date = phisndate
043700030228     C                   endif
043800030228     **
043900030303     C     emp           reade     phisfmty
044000030228     C                   enddo
044100030228     C                   return    ytd_val
044200030304
044300030304     C     calc_grossytd begsr
044400030304      /free
044500050525            countr = *zeros;
044600050525            dow not get_payty_info('RD':-1:payty_struc:countr);
044700030304             if ptyrtype = 'E'
044800030304              and ptytcode = 'Y';
044900030304              wrk_type = ptyptype;
045000030304              exsr calc_ytd;
045100030304             endif;
045200030304            enddo;
045300030304      /end-free
045400030304     C                   endsr
045500030304
045600030304     C     calc_ytd      begsr
045700030304     C     trn_key2      setll     ptisfmty
045800030304     C     trn_key2      reade     ptisfmty
045900030304     C                   dow       not(%eof(paytranl03))
046000030304     **
046100041214     C                   eval      ytd_val += xptrnamt
046200030304     C     trn_key2      reade     ptisfmty
046300030304     C                   enddo
046400030304     C                   endsr
046500030228     Psum_ytd          e
046600041211
046700041213
046800041213       //*******************************************                        **
046900041213       // Get all payroll transactions - horizontal
047000041213       //*******************************************                        **
047100041213     P Payallguts      B
047200041213     D Payallguts      PI
047300041213     D ytdateamt       s             11  2
047400041213
047500041213      /free
047600050525            countr = *zeros;
047700050525            dow not get_payty_info('RD':-1:payty_struc:countr);
047800050525
047900041213           chain  (payroll_type:ptyptype) prdffmt;
048000041216           if %found and prdfflag = 'Y';
048100041213           chain  (payroll_type:emp:payroll_date:
048200041213                   ptyrtype:ptyptype) ptisfmt;
048300041213
048400041213           if  not %found;
048500041214             ptrnhrs = *zeros;
048600041214             ptrnamt = *zeros;
048700041213           endif;
048800041213
048900041213           ytdateamt =  sum_ytd(ptyptype:phisprno);
049000041213
049100041214             if  log_date = *zeros and %found(paytranl06)
049200041214                       or  log_date <> *zeros
049300041214                  and  log_date = ptrnrdate and %found(paytranl06)
049400041214                  or  ytdateamt <> *zeros;
049500041213
049600041213                 saved_empcnt +=  1;
049700041213
049800041213              // employee Hourly Rate
049900041213                 get_hoursRate();
050000041213
050100041213            // if changes from pay to deduct
050200041213            if  ptyrtype = pay_codeHld;
050300041213
050400041213                // if end of line reached
050500041213                if   saved_empcnt > fld_count;
050600041213
050700041213                     typ_total = *zeros;
050800041213                     Print_slip_det();
050900041213                     saved_empcnt = 2;
051000041213                     clear pPrtArr;
051100041213                     clear pytd;
051200041213                     clear pamount;
051300041213                     clear prate;
051400041213                     HoursFound = '0';
051500041213                endif;
051600041213
051700041213                  // if changes from pay to deduct
051800041213                  else;
051900041213                       saved_empcnt = 2;
052000041213                       typ_total = total_gross;
052100041213                       Print_slip_det();
052200041213                       pay_codeHld = ptyrtype;
052300041213                       clear pPrtArr;
052400041213                       clear pytd;
052500041213                       clear pamount;
052600041213                       clear prate;
052700041213                       HoursFound = '0';
052800041213              endif;
052900041213
053000041213                 pPrtArr(saved_empcnt) = ptyshrtnm;
053100041213                 prate(saved_empcnt) = %editc(ptrnhrs:'2');
053200041213                 if pay_rate <> *zeros;
053300041213                  prate(saved_empcnt) = %trim(prate(saved_empcnt))
053400041213                        + '@' + %trim(%editc(pay_rate:'2'));
053500041213                 endif;
053600041213                 pamount(saved_empcnt) = %char(ptrnamt);
053700041213                 pytd(saved_empcnt) = %char(ytdateamt);
053800041213
053900041213            endif;
054000041216           endif;
054100041213
054200041213          enddo;
054300041213
054400041213      /end-free
054500041213     PPayAllGuts       E
054600041213
054700041213       //****************************************************               **
054800041213       // Get payroll transactions  for payroll - horizontal
054900041213       //****************************************************               **
055000041213     P PayGuts         B
055100041213     D PayGuts         PI
055200041213      /free
055300041213
055400041213           setll trn_key ptisfmt;
055500041213           reade trn_key ptisfmt;
055600041213
055700041213           dow  not(%eof);
055800041213             if  log_date = *zeros or
055900041213                   log_date = ptrnrdate;
056000041213
056100041213                  saved_empcnt +=  1;
056200041213
056300041213              // employee Hourly Rate
056400041213                 get_hoursRate();
056500041213
056600041213                  // if changes from pay to deduct
056700041213          if  ptrncode = pay_codeHld;
056800041213
056900041213                  // if end of line reached
057000041213                if   saved_empcnt > fld_count;
057100041213
057200041213                     typ_total = *zeros;
057300041213                     Print_slip_det();
057400041213                     saved_empcnt = 2;
057500041213                     clear pPrtArr;
057600041213                     clear pytd;
057700041213                     clear pamount;
057800041213                     clear prate;
057900041213                     HoursFound = '0';
058000041213                endif;
058100041213
058200041213                  // if changes from pay to deduct
058300041213                  else;
058400041213                       saved_empcnt = 2;
058500041213                       typ_total = total_gross;
058600041213                       Print_slip_det();
058700041213                       pay_codeHld = ptrncode;
058800041213                       clear pPrtArr;
058900041213                       clear pytd;
059000041213                       clear pamount;
059100041213                       clear prate;
059200041213                       HoursFound = '0';
059300041213                endif;
059400041213
059500050525                 get_payty_info('RD':ptrntcode:payty_struc);
059600041213                 pPrtArr(saved_empcnt) = ptyshrtnm;
059700041213                 prate(saved_empcnt) = %editc(ptrnhrs:'2');
059800041213                 if pay_rate <> *zeros;
059900041213                  prate(saved_empcnt) = %trim(prate(saved_empcnt))
060000041213                        + '@' + %trim(%editc(pay_rate:'2'));
060100041213                 endif;
060200041213                 pamount(saved_empcnt) = %char(ptrnamt);
060300041213                 pytd(saved_empcnt) = %char(sum_ytd(ptrntcode:phisprno));
060400041213
060500041213            endif;
060600041213           reade trn_key ptisfmt;
060700041213          enddo;
060800041213
060900041213      /end-free
061000041213     PPayGuts          E
061100041213
061200041213       //*****************************************                          **
061300041213       // Get all payroll transactions - vertical
061400041213       //*****************************************                          **
061500041213     P Payallgutsv     B
061600041213     D Payallgutsv     PI
061700041213     D ytdateamt       s             11  2
061800041213
061900041213      /free
062000050525            countr = *zeros;
062100050525            dow not get_payty_info('RD':-1:payty_struc:countr);
062200041213           chain  (payroll_type:ptyptype) prdffmt;
062300041216           if %found and prdfflag = 'Y';
062400041213           chain  (payroll_type:emp:payroll_date:
062500041213                   ptyrtype:ptyptype) ptisfmt;
062600041213
062700041213           if  not %found;
062800041214             ptrnhrs = *zeros;
062900041214             ptrnamt = *zeros;
063000041213             clear  det_recd2;
063100041213           endif;
063200041213
063300041213           ytdateamt =  sum_ytd(ptyptype:phisprno);
063400041214
063500041214             if  log_date = *zeros and %found(paytranl06)
063600041214                       or  log_date <> *zeros
063700041214                  and  log_date = ptrnrdate and %found(paytranl06)
063800041214                  or  ytdateamt <> *zeros;
063900041213
064000041213        // if Paycode changes (E= emoluments,D= deductions)
064100041213               if   ptyrtype <> pay_codeHld;
064200041213                   sub_text = gross_text;
064300041213                   sub_tot  = total_gross;
064400041213        // employee transaction summary total for Emoluments
064500041213                   write  sub_tot_rc;
064600041213                   pay_codeHld = ptyrtype;
064700041213                endif;
064800041213
064900041213        // employee Hourly Rate
065000041213            get_hoursRate();
065100041213
065200041213             pay_hrs = ptrnhrs;
065300071205             pay_hrsp = pay_hrs;
065400041213             pay_amt = ptrnamt;
065500041213
065600041213            if ytd_opt = 3 and ytdateamt <> *zeros;
065700041213
065800041213             ytd_amt =  ytdateamt;
065900041213             ptyshnm =  ptyshrtnm;
066000041213             write  det_recd2;
066100041213              else;
066200041213
066300041213             write  det_recd;
066400041213            endif;
066500041213
066600041213            endif;
066700041216           endif;
066800041213          enddo;
066900041213      /end-free
067000041213     PPayALLgutsV      E
067100041213
067200041213       //***************************************                            **
067300041213       // Get  payroll transactions - vertical
067400041213       //***************************************                            **
067500041213     P Paygutsv        B
067600041213     D Paygutsv        PI
067700041213     D ytdateamt       s             11  2
067800041213
067900041213      /free
068000041213           setll trn_key ptisfmt;
068100041213           reade trn_key ptisfmt;
068200041213
068300041213           dow  not(%eof);
068400041213             if  log_date = *zeros or
068500041213                    log_date = ptrnrdate;
068600041213
068700050525           get_payty_info('RD':ptrntcode:payty_struc);
068800041213        // if Paycode changes (E= emoluments,D= deductions)
068900041213               if   ptrncode <> pay_codeHld;
069000041213                   sub_text = gross_text;
069100041213                   sub_tot  = total_gross;
069200041213        // employee transaction summary total for Emoluments
069300041213                   write  sub_tot_rc;
069400041213                   pay_codeHld = ptrncode;
069500041213                endif;
069600041213
069700041213        // employee Hourly Rate
069800041213            get_hoursRate();
069900041213
070000041213             pay_hrs = ptrnhrs;
070100071206             pay_hrsp = pay_hrs;
070200041213             pay_amt = ptrnamt;
070300041213
070400041213            if ytd_opt = 3;
070500041213
070600041213             ytd_amt =  sum_ytd(ptrntcode:phisprno);
070700041213             ptyshnm =  ptyshrtnm;
070800041213             write  det_recd2;
070900041213              else;
071000041213
071100041213             write  det_recd;
071200041213            endif;
071300041213
071400041213            endif;
071500041213           reade trn_key ptisfmt;
071600041213          enddo;
071700041213      /end-free
071800041213     PPaygutsv         E
071900041213         //------------------------------
072000041213         // Get calculate Hours/Rate
072100041213         //------------------------------
072200041213     P get_hoursRate   B
072300041213     D get_hoursRate   PI
072400041213
072500041213      /free
072600041213          // if hours not zero, check if rates flag set.
072700041213          // If it is,print rates else blank it
072800041213                if  ptrnhrs <> *zeros;
072900041213                     HoursFound = '1';
073000041213
073100041213                    if  rate_flag = 'Y';
073200041213                        pay_rate  = ptrnamt  / ptrnhrs;
073300041213                         else;
073400041213                        pay_rate  =  0;
073500041213                    endif;
073600041213
073700041213                      else;
073800041213                     pay_rate =  0;
073900041213                  endif;
074000041213      /end-free
074100041213     Pget_hoursRate    E
074200041213
074300041213       //-----------------------------
074400041213       // Print payslip Detail lines
074500041213       //-----------------------------
074600041213     P print_slip_det  B
074700041213     D print_slip_det  PI
074800041213
074900041213      /free
075000041213                  for  counter  = 1 to 3;
075100041213                   write  detail;
075200041213                   pay010 = *blanks;
075300041213                     select;
075400041213                      when counter = 1;
075500041213                    // Print any Rates
075600041213                     if pay_codeHld = 'E' and
075700041213                           HoursFound;
075800041213                          pPrtArr = prate;
075900041213                          pPrtArr(one) = rate_cnst;
076000041213                          write  detail;
076100041213                        endif;
076200041213                       pPrtArr = pamount;
076300041213                       pay010 = %char(typ_total);
076400041213
076500041213                     if pay_codeHld = 'E';
076600041213                           pPrtArr(one) = emol_cnst;
076700041213                            else;
076800041213                           pPrtArr(one) = ded_cnst;
076900041213                     endif;
077000041213                      when counter = 2;
077100041213                       pPrtArr = pytd;
077200041213                       pPrtArr(one) = ytd_cnst;
077300041213                     endsl;
077400041213                endfor;
077500041213      /end-free
077600041213     Pprint_slip_det   E
077700010423
077800980330     **
077900010613
