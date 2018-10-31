000100010902     H DECEDIT('.') DATEDIT(*ymd/) bnddir('QC2LE')
000200950330     F**
000300010626     F** EMPLOYEES PAYROLL
000400010420     F** PROGRAMMERS---C.Williams ,S.BRITTON
000500950330     F**
000600021112     Fempmast   if   e           k disk
000700021106     Fpayeytdl01if   e           k disk
000800010423     Fpayrtypl01if   e           k disk
000900010613     Fpaytypl01 if   e           k disk
001000010427     Fpayrdefl01if   e           k disk
001100021111     Fpayhisl11 if   e           k disk
001200021111     Fpaytranl02if   e           k disk
001300010215     **
001400021205     Fpayslipfl o    e             printer oflind(inof)
001500980330     **
001600021112     demp              s              6  0 inz(004804)
001700021112     dprno             s              6  0 inz(200211)
001800021112     dpay_date         s              8  0 inz(20021105)
001900021112     demp_name#        s             40
002000021108
002100021112     demp_data         ds                  inz  occurs(40)
002200021112     d  emp#                          6  0
002300021205     d  pay_code                      1
002400021112     d  pay_typ                       2  0
002500021112     d  pay_amt                      11  2
002600021205     d  pay_des                      09
002700021205     d  pay_amtytd                   11  2
002800021204
002900021115
003000021115
003100021205     dpy_detailH       ds
003200021204     d  pay001                       10
003300021204     d  pay002                       10
003400021204     d  pay003                       10
003500021204     d  pay004                       10
003600021204     d  pay005                       10
003700021204     d  pay006                       10
003800021204     d  pay007                       10
003900021204     d  pay008                       10
004000021204     d  pay009                       10
004100021205
004200021204
004300021205     drecptr           s               *   inz(%addr(py_detailH))
004400021205     d pprtArr         s             10    dim(09) based(recptr)
004500021205     d pAmount         s             10    dim(09)
004600021205     d pytd            s             10    dim(09)
004700021128     ddat_fld          s               d   inz
004800021130     dsaved_empcode    s              1
004900021201     dsaved_empcnt     s                   like(ycnt)
005000021205     dGross_tot        s             11  2
005100021125     dGross_ded        s             11  2
005200021125     dnet_total        s             11  2
005300021128     dy                s              3  0
005400021128     dcount            s              3  0
005500021115     dxcnt             s              3  0
005600021112     dycnt             s              3  0
005700021112     d@date            s             10
005800021205     d one             c                   1
005900021205     d fld_count       c                   9
006000021205     d net_cnst        c                   'Net amount'
006100021205     d emol_cnst       c                   'Emoluments'
006200021205     d ded_cnst        c                   'Deductions'
006300021205     d ytd_cnst        c                   'Y.T.D.'
006400010815      /copy genPR
006500010420     ** Payroll PROTOTYPES
006600010713      /copy paylib/payrpg,PAYPR
006700021204
006800021107     c     emp_key       klist
006900021107     c                   kfld                    emp
007000021112     c                   kfld                    prno              6 0
007100021112     c                   kfld                    pay_date          8 0
007200021111     c     trn_key       klist
007300021112     c                   kfld                    phistype
007400021111     c                   kfld                    emp
007500021111     c                   kfld                    pay_date
007600021112      /free
007700021112             chain emp_key phisfmt;
007800021112             if (%found(payhisl11));
007900021128                       get_emp_info(emp:*omit:' ':emp_name#);
008000021128                       emp_name = emp_name#;
008100021128                       chain emp  emprec;
008200021128                       nis_num  = nisnum;
008300021128          //           dat_fld  = %date(emply:*MDY);
008400021128          //           emp_doe  = dat_fld;
008500021128                       ptrnpdate = pay_date;
008600021111             write  header;
008700021112             setll trn_key ptisfmt;
008800021112             reade trn_key ptisfmt;
008900021112             dow not(%eof(paytranl02));
009000021108                     xcnt = xcnt + 1;
009100021112             %occur(emp_data) = xcnt;
009200021115             chain ptrntcode ptyfmt;
009300021112
009400021115             pay_des  = ptyshrtnm;
009500021115
009600021112             emp# = emp;
009700021205             pay_code = ptrncode;
009800021112             pay_typ = ptrntcode;
009900021112             pay_amt = ptrnamt;
010000021205             pay_amtytd = 15000.10;
010100021112             reade trn_key ptisfmt;
010200021111             enddo;
010300021112             endif;
010400021112             exsr prnt_check;
010500021112             *INLR = '1';
010600021112             RETURN;
010700021128       //*******************************************
010800021128       //** Payroll Output
010900021128       //** output Payroll Info. Printer File
011000021128       //*******************************************
011100021128           begsr prnt_check;
011200010613
011300021128
011400021205                 %occur(emp_data) = 1;
011500021205                 saved_empcode = pay_code;
011600021205                 saved_empcnt =  1;
011700021128
011800021128                  for  ycnt  = 1 to xcnt;
011900011204
012000021125
012100021205                 saved_empcnt = saved_empcnt + 1;
012200021205                    %occur(emp_data) = ycnt;
012300021205
012400021205
012500021205                  // if changes from pay to deduct
012600021205                if   pay_code = saved_empcode;
012700021205
012800021205                  // if end of line reached
012900021205                if   saved_empcnt > fld_count;
013000021205
013100021205                     typ_total = *zeros;
013200021205                     exsr Print_payslip_detail;
013300021205                     saved_empcnt = 2;
013400021205                     clear pPrtArr;
013500021205                     clear pytd;
013600021205                     clear pamount;
013700021205                endif;
013800021205
013900021205                  // if changes from pay to deduct
014000021205                  else;
014100021205                       saved_empcnt = 2;
014200021205                       typ_total = Gross_tot;
014300021205                       exsr Print_payslip_detail;
014400021205                       saved_empcode = pay_code;
014500021205                       Gross_tot = *zeros;
014600021205                       clear pPrtArr;
014700021205                       clear pytd;
014800021205                       clear pamount;
014900021128                endif;
015000021128
015100021205
015200021205                 pPrtArr(saved_empcnt) = pay_des;
015300021205                 pamount(saved_empcnt) = %char(pay_amt);
015400021205                 pytd(saved_empcnt) = %char(pay_amtytd);
015500021128
015600021205         // temp block
015700021205                       gross_tot = gross_tot + pay_amt;
015800021202
015900021205                    endfor;
016000021128
016100021128       // end of pay info
016200021128
016300021205                   typ_total = Gross_tot;
016400021205                   exsr Print_payslip_detail;
016500021125
016600021205                       clear pPrtArr;
016700021205                       clear pytd;
016800021205                       clear pamount;
016900021205                       pPrtArr(fld_count) =  net_cnst;
017000021205                       pPrtArr(one) =  *blank;
017100021205
017200021205                       typ_total = gross_tot - gross_ded;
017300021205                       pay010 = %char(typ_total);
017400021202
017500021205                       write detail;
017600021115
017700021128       endsr;
017800021205       //-----------------------------
017900021205       // Print payslip
018000021205       //-----------------------------
018100021205           begsr print_payslip_detail;
018200021205                  for  count  = 1 to 3;
018300021205                   write  detail;
018400021205                   pay010 = *blanks;
018500021205                     select ;
018600021205                      when count = 1;
018700021205                       pPrtArr = pamount;
018800021205                       pay010 = %char(typ_total);
018900021205                          if saved_empcode = 'E';
019000021205                           pPrtArr(one) = emol_cnst;
019100021205                            else;
019200021205                           pPrtArr(one) = ded_cnst;
019300021205                         endif;
019400021205                      when count = 2;
019500021205                       pPrtArr = pytd;
019600021205                       pPrtArr(one) = ytd_cnst;
019700021205                     endsl;
019800021205                endfor;
019900021205           endsr;
020000021128      /end-free
020100021202
020200010215     ***********************************
020300010215     ** StartUp subroutine
020400010215     ***********************************
020500010216     C     *inzsr        begsr
020600010216     **
020700010613     ** Get company name
020800010824     C                   eval      coname = get_coname
020900010613
021000010215     C                   endsr
021100980330     **
021200010613
