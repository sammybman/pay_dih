000100011205     H DECEDIT('.') DATEDIT(*ymd/)
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
001900030228     **
002000030228      /copy genPR
002100030228     ** Payroll PROTOTYPES
002200030228      /copy *libl/payrpg,PAYPR
002300050525      /copy EMPPR
002400010215     **
002500030228     D                 ds
002600030228     D  pcdftytdDS                   10
002700030228     D dft_list                       2  0 dim(5) overlay(pcdftytdDS)
002800030227     **
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
017200030424
017300030424         endif;
017400030228
031900030227
032000030227      /end-free
032100030227     ***********************************
032200030227     ** StartUp subroutine
032300030227     ***********************************
032400030227     C     *inzsr        begsr
032500030227     **
032600030227     C     *entry        plist
032700030227     C                   parm                    payroll_type
032800030227     C                   parm                    payroll_date
032900030227     C                   parm                    rate_flag
033000030228     C                   parm                    log_date
033100030227     C                   parm                    py_style
033200030227     C                   parm                    ytd_opt
033300030227     **
033400030227      /free
033500030227         // Get company name
033600030227           coname = get_coname;
033700030227           @date = get_sys_date(system_date);
033800030227           open paycon;
033900030227           read paycon;
034000030227            if not(%eof);
034100030227                pcdftytdDS = pcdftytd;
034200030227            endif;
034300030227          close paycon;
034400030227
034500030227           select;
034600030227            when py_style = 1;
034700030227              open payslipfl;
034800030227            when py_style = 2;
034900030227              open payslipf;
035000030227            endsl;
035100030228
035200030228      /end-free
035300030228     C                   endsr
035400030228      /free
038800030227      /end-free
038900030228     *********************************
039000030228     ** Summarise YTD Figures       **
039100030228     *********************************
039200030228     Psum_ytd          b
039300030228     **
039400030228     Dsum_ytd          pi            11  2
039500030228     **
039600030228     D in_type                        2  0 value
039700030228     D in_prno                        6  0 value
039800030228     **
039900030228     D str_prno        s              6  0
040000030228     D ytd_val         s             11  2
040100030228     D last_date       s              8  0
040200030304     D wrk_type        s              2  0
040300030228     **
040400030228     C                   eval      str_prno = in_prno
040500030304     C                   eval      wrk_type = in_type
040600030228     C                   move      01            str_prno
040700030228     **
040800030228     C     trn_key       klist
040900030228     C                   kfld                    emp
041000030228     C                   kfld                    str_prno
041100030228     **
041200030228     C     trn_key2      klist
041300030228     C                   kfld                    emp
041400030304     C                   kfld                    wrk_type
041500030228     C                   kfld                    phisndate
041600030228     **
041700030228     C                   eval      ytd_val = *zeros
041800030228     C     trn_key       setll     phisfmty
041900030303     C     emp           reade     phisfmty
042000030228     C                   eval      last_date = *zeros
042100030228     C                   dow       not(%eof(payhisl11))
042200030303     C                   if        phisndate > payroll_date
042300030228     C                   leave
042400030228     C                   endif
042500030228     C                   if        last_date <> phisndate
042600030304     **
042700030304     C                   select
042800030304     C                   when      in_type = *ZEROS
042900030304     C                   exsr      calc_grossytd
043000030304     C                   other
043100030304     C                   exsr      calc_ytd
043200030304     C                   endsl
043300030304     **
043400030228     C                   eval      last_date = phisndate
043500030228     C                   endif
043600030228     **
043700030303     C     emp           reade     phisfmty
043800030228     C                   enddo
043900030228     C                   return    ytd_val
044000030304
044100030304     C     calc_grossytd begsr
044200030304      /free
044300050525            countr = *zeros;
044400050525            dow not get_payty_info('RD':-1:payty_struc:countr);
044500030304             if ptyrtype = 'E'
044600030304              and ptytcode = 'Y';
044700030304              wrk_type = ptyptype;
044800030304              exsr calc_ytd;
044900030304             endif;
045000030304            enddo;
045100030304      /end-free
045200030304     C                   endsr
045300030304
045400030304     C     calc_ytd      begsr
045500030304     C     trn_key2      setll     ptisfmty
045600030304     C     trn_key2      reade     ptisfmty
045700030304     C                   dow       not(%eof(paytranl03))
045800030304     **
045900041214     C                   eval      ytd_val += xptrnamt
046000030304     C     trn_key2      reade     ptisfmty
046100030304     C                   enddo
046200030304     C                   endsr
046300030228     Psum_ytd          e
046400041211
046500041213
046600041213       //*******************************************                        **
046700041213       // Get all payroll transactions - horizontal
046800041213       //*******************************************                        **
046900041213     P Payallguts      B
047000041213     D Payallguts      PI
047100041213     D ytdateamt       s             11  2
047200041213
047300041213      /free
047400050525            countr = *zeros;
047500050525            dow not get_payty_info('RD':-1:payty_struc:countr);
047600050525
047700041213           chain  (payroll_type:ptyptype) prdffmt;
047800041216           if %found and prdfflag = 'Y';
047900041213           chain  (payroll_type:emp:payroll_date:
048000041213                   ptyrtype:ptyptype) ptisfmt;
048100041213
048200041213           if  not %found;
048300041214             ptrnhrs = *zeros;
048400041214             ptrnamt = *zeros;
048500041213           endif;
048600041213
048700041213           ytdateamt =  sum_ytd(ptyptype:phisprno);
048800041213
048900041214             if  log_date = *zeros and %found(paytranl06)
049000041214                       or  log_date <> *zeros
049100041214                  and  log_date = ptrnrdate and %found(paytranl06)
049200041214                  or  ytdateamt <> *zeros;
049300041213
049400041213                 saved_empcnt +=  1;
049500041213
049600041213              // employee Hourly Rate
049700041213                 get_hoursRate();
049800041213
049900041213            // if changes from pay to deduct
050000041213            if  ptyrtype = pay_codeHld;
050100041213
050200041213                // if end of line reached
050300041213                if   saved_empcnt > fld_count;
050400041213
050500041213                     typ_total = *zeros;
050600041213                     Print_slip_det();
050700041213                     saved_empcnt = 2;
050800041213                     clear pPrtArr;
050900041213                     clear pytd;
051000041213                     clear pamount;
051100041213                     clear prate;
051200041213                     HoursFound = '0';
051300041213                endif;
051400041213
051500041213                  // if changes from pay to deduct
051600041213                  else;
051700041213                       saved_empcnt = 2;
051800041213                       typ_total = total_gross;
051900041213                       Print_slip_det();
052000041213                       pay_codeHld = ptyrtype;
052100041213                       clear pPrtArr;
052200041213                       clear pytd;
052300041213                       clear pamount;
052400041213                       clear prate;
052500041213                       HoursFound = '0';
052600041213              endif;
052700041213
052800041213                 pPrtArr(saved_empcnt) = ptyshrtnm;
052900041213                 prate(saved_empcnt) = %editc(ptrnhrs:'2');
053000041213                 if pay_rate <> *zeros;
053100041213                  prate(saved_empcnt) = %trim(prate(saved_empcnt))
053200041213                        + '@' + %trim(%editc(pay_rate:'2'));
053300041213                 endif;
053400041213                 pamount(saved_empcnt) = %char(ptrnamt);
053500041213                 pytd(saved_empcnt) = %char(ytdateamt);
053600041213
053700041213            endif;
053800041216           endif;
053900041213
054000041213          enddo;
054100041213
054200041213      /end-free
054300041213     PPayAllGuts       E
054400041213
054500041213       //****************************************************               **
054600041213       // Get payroll transactions  for payroll - horizontal
054700041213       //****************************************************               **
054800041213     P PayGuts         B
054900041213     D PayGuts         PI
055000041213      /free
055100041213
055200041213           setll trn_key ptisfmt;
055300041213           reade trn_key ptisfmt;
055400041213
055500041213           dow  not(%eof);
055600041213             if  log_date = *zeros or
055700041213                   log_date = ptrnrdate;
055800041213
055900041213                  saved_empcnt +=  1;
056000041213
056100041213              // employee Hourly Rate
056200041213                 get_hoursRate();
056300041213
056400041213                  // if changes from pay to deduct
056500041213          if  ptrncode = pay_codeHld;
056600041213
056700041213                  // if end of line reached
056800041213                if   saved_empcnt > fld_count;
056900041213
057000041213                     typ_total = *zeros;
057100041213                     Print_slip_det();
057200041213                     saved_empcnt = 2;
057300041213                     clear pPrtArr;
057400041213                     clear pytd;
057500041213                     clear pamount;
057600041213                     clear prate;
057700041213                     HoursFound = '0';
057800041213                endif;
057900041213
058000041213                  // if changes from pay to deduct
058100041213                  else;
058200041213                       saved_empcnt = 2;
058300041213                       typ_total = total_gross;
058400041213                       Print_slip_det();
058500041213                       pay_codeHld = ptrncode;
058600041213                       clear pPrtArr;
058700041213                       clear pytd;
058800041213                       clear pamount;
058900041213                       clear prate;
059000041213                       HoursFound = '0';
059100041213                endif;
059200041213
059300050525                 get_payty_info('RD':ptrntcode:payty_struc);
059400041213                 pPrtArr(saved_empcnt) = ptyshrtnm;
059500041213                 prate(saved_empcnt) = %editc(ptrnhrs:'2');
059600041213                 if pay_rate <> *zeros;
059700041213                  prate(saved_empcnt) = %trim(prate(saved_empcnt))
059800041213                        + '@' + %trim(%editc(pay_rate:'2'));
059900041213                 endif;
060000041213                 pamount(saved_empcnt) = %char(ptrnamt);
060100041213                 pytd(saved_empcnt) = %char(sum_ytd(ptrntcode:phisprno));
060200041213
060300041213            endif;
060400041213           reade trn_key ptisfmt;
060500041213          enddo;
060600041213
060700041213      /end-free
060800041213     PPayGuts          E
060900041213
061000041213       //*****************************************                          **
061100041213       // Get all payroll transactions - vertical
061200041213       //*****************************************                          **
061300041213     P Payallgutsv     B
061400041213     D Payallgutsv     PI
061500041213     D ytdateamt       s             11  2
061600041213
061700041213      /free
061800050525            countr = *zeros;
061900050525            dow not get_payty_info('RD':-1:payty_struc:countr);
062000041213           chain  (payroll_type:ptyptype) prdffmt;
062100041216           if %found and prdfflag = 'Y';
062200041213           chain  (payroll_type:emp:payroll_date:
062300041213                   ptyrtype:ptyptype) ptisfmt;
062400041213
062500041213           if  not %found;
062600041214             ptrnhrs = *zeros;
062700041214             ptrnamt = *zeros;
062800041213             clear  det_recd2;
062900041213           endif;
063000041213
063100041213           ytdateamt =  sum_ytd(ptyptype:phisprno);
063200041214
063300041214             if  log_date = *zeros and %found(paytranl06)
063400041214                       or  log_date <> *zeros
063500041214                  and  log_date = ptrnrdate and %found(paytranl06)
063600041214                  or  ytdateamt <> *zeros;
063700041213
063800041213        // if Paycode changes (E= emoluments,D= deductions)
063900041213               if   ptyrtype <> pay_codeHld;
064000041213                   sub_text = gross_text;
064100041213                   sub_tot  = total_gross;
064200041213        // employee transaction summary total for Emoluments
064300041213                   write  sub_tot_rc;
064400041213                   pay_codeHld = ptyrtype;
064500041213                endif;
064600041213
064700041213        // employee Hourly Rate
064800041213            get_hoursRate();
064900041213
065000041213             pay_hrs = ptrnhrs;
065100041213             pay_amt = ptrnamt;
065200041213
065300041213            if ytd_opt = 3 and ytdateamt <> *zeros;
065400041213
065500041213             ytd_amt =  ytdateamt;
065600041213             ptyshnm =  ptyshrtnm;
065700041213             write  det_recd2;
065800041213              else;
065900041213
066000041213             write  det_recd;
066100041213            endif;
066200041213
066300041213            endif;
066400041216           endif;
066500041213          enddo;
066600041213      /end-free
066700041213     PPayALLgutsV      E
066800041213
066900041213       //***************************************                            **
067000041213       // Get  payroll transactions - vertical
067100041213       //***************************************                            **
067200041213     P Paygutsv        B
067300041213     D Paygutsv        PI
067400041213     D ytdateamt       s             11  2
067500041213
067600041213      /free
067700041213           setll trn_key ptisfmt;
067800041213           reade trn_key ptisfmt;
067900041213
068000041213           dow  not(%eof);
068100041213             if  log_date = *zeros or
068200041213                    log_date = ptrnrdate;
068300041213
068400050525           get_payty_info('RD':ptrntcode:payty_struc);
068500041213        // if Paycode changes (E= emoluments,D= deductions)
068600041213               if   ptrncode <> pay_codeHld;
068700041213                   sub_text = gross_text;
068800041213                   sub_tot  = total_gross;
068900041213        // employee transaction summary total for Emoluments
069000041213                   write  sub_tot_rc;
069100041213                   pay_codeHld = ptrncode;
069200041213                endif;
069300041213
069400041213        // employee Hourly Rate
069500041213            get_hoursRate();
069600041213
069700041213             pay_hrs = ptrnhrs;
069800041213             pay_amt = ptrnamt;
069900041213
070000041213            if ytd_opt = 3;
070100041213
070200041213             ytd_amt =  sum_ytd(ptrntcode:phisprno);
070300041213             ptyshnm =  ptyshrtnm;
070400041213             write  det_recd2;
070500041213              else;
070600041213
070700041213             write  det_recd;
070800041213            endif;
070900041213
071000041213            endif;
071100041213           reade trn_key ptisfmt;
071200041213          enddo;
071300041213      /end-free
071400041213     PPaygutsv         E
071500041213         //------------------------------
071600041213         // Get calculate Hours/Rate
071700041213         //------------------------------
071800041213     P get_hoursRate   B
071900041213     D get_hoursRate   PI
072000041213
072100041213      /free
072200041213          // if hours not zero, check if rates flag set.
072300041213          // If it is,print rates else blank it
072400041213                if  ptrnhrs <> *zeros;
072500041213                     HoursFound = '1';
072600041213
072700041213                    if  rate_flag = 'Y';
072800041213                        pay_rate  = ptrnamt  / ptrnhrs;
072900041213                         else;
073000041213                        pay_rate  =  0;
073100041213                    endif;
073200041213
073300041213                      else;
073400041213                     pay_rate =  0;
073500041213                  endif;
073600041213      /end-free
073700041213     Pget_hoursRate    E
073800041213
073900041213       //-----------------------------
074000041213       // Print payslip Detail lines
074100041213       //-----------------------------
074200041213     P print_slip_det  B
074300041213     D print_slip_det  PI
074400041213
074500041213      /free
074600041213                  for  counter  = 1 to 3;
074700041213                   write  detail;
074800041213                   pay010 = *blanks;
074900041213                     select;
075000041213                      when counter = 1;
075100041213                    // Print any Rates
075200041213                     if pay_codeHld = 'E' and
075300041213                           HoursFound;
075400041213                          pPrtArr = prate;
075500041213                          pPrtArr(one) = rate_cnst;
075600041213                          write  detail;
075700041213                        endif;
075800041213                       pPrtArr = pamount;
075900041213                       pay010 = %char(typ_total);
076000041213
076100041213                     if pay_codeHld = 'E';
076200041213                           pPrtArr(one) = emol_cnst;
076300041213                            else;
076400041213                           pPrtArr(one) = ded_cnst;
076500041213                     endif;
076600041213                      when counter = 2;
076700041213                       pPrtArr = pytd;
076800041213                       pPrtArr(one) = ytd_cnst;
076900041213                     endsl;
077000041213                endfor;
077100041213      /end-free
077200041213     Pprint_slip_det   E
077300010423
077400980330     **
077500010613
