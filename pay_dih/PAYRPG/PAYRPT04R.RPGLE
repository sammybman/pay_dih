000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111002     H/define profoundui
000300111002     Fpayhisl04 If   E           k DISK    usropn
000400111002     Fpayrtyp   IF   E           K DISK    usropn
000500111002     Fpaydiv    if   e           k DISK    usropn
000600010905     Fqsysprt   O    F  132        PRINTER
000700010905     Fpayrpt04d cf   e             workstn
000800111002     F/IF DEFINED(WEB)
000900111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001000111002     F/ENDIF
001100111002      /copy QCPYSRC,genpr
001200111002      /copy QCPYSRC,autpr
001300010905     **
001400010905     D den_str         DS                  occurs(40)
001500010905     D den_desc                      30
001600010905     D den_amt                        7  0
001700010905     D den_value                     13  2
001800010905     **
001900010905
002000010905     Dsav_arr          s              7  0 dim(40)
002100010905     Dden_arr          s             30    dim(40)
002200010905     Dval_arr          s             13  2 dim(40)
002300010905
002400010906     Drep_title        s             45
002500010906     Ddenom_desc       s             30
002600010905     Ddenom_amt        s              7  0
002700010905     Ddenom_value      s             13  2
002800010905     Damount           s             13  2
002900010905     Dtot_val          s             13  2
003000010905     Dall              s             13  2
003100010905     Dstcnt            s              4  0
003200010905     Dresult           s                   like(stcnt)
003300010905     Dacnt             s                   like(stcnt)
003400010905     Dxcnt             s                   like(stcnt)
003500010905     Dtot_cnt          s                   like(stcnt)
003600010905
003700010905     ** system fields for report
003800010905     D@date            S             10
003900010905     D@title           S             40
004000010905     Dconame           S             30
004100010905     D arr             S             40    DIM(1) CTDATA PERRCD(1)
004200010905     Ddate_err         c                   'Invalid Date Entered'
004300010905     Dtype_err         c                   'Invalid Payroll Type Entered'
004400010905     Ddiv_err          c                   'A Valid Division Was Not Entered.'
004500010905     **
004600950907     D                SDS
004700950907     D  @PGM             *PROC
004800111002     D  @USER                358    367
004900010905     **
005000010905     C     den_list      plist
005100010905     C                   parm                    amount
005200010905     C                   parm                    den_str
005300010905     C                   parm      *zeros        stcnt
005400010905
005500010905     C     den_key       klist
005600010905     C                   kfld                    pay_date
005700010905     C                   kfld                    run_type
005800010905     C                   kfld                    pay_div
005900010905
006000040207      /free
006100111002         // authority check
006200111002         monitor;
006300111002           authority_check('PAY':@pgm);
006400111002         on-error;
006500111002           show_msg(get_err_msg('DIH0054'));
006600111002           *inlr = *on;
006700111002           return;
006800111002         endmon;
006900040207      /end-free
007000010905     C                   exsr      aainit
007100010905     C***************************************************************
007200010905     ***         Prepare Report Criteria                          ***
007300010905     C***************************************************************
007400010905
007500010905     C                   dow       0 = 0
007600010905     C                   exfmt     fmt1
007700010905     C                   exsr      aainit
007800010905     C                   clear                   errmsg
007900010905     C                   movea     '00'          *in(25)
008000010905     ** F3 = End Pgm
008100010905     C                   if        *in03
008200010905     C                   eval      *inlr = '1'
008300010905     C                   return
008400010905     C                   endif
008500010905
008600010905      *** F1 = Run Type enquiry
008700010905     C                   if        *in01
008800010905     C                   call      'PAYENQ02R'
008900010905     C                   parm                    run_type
009000010905     C                   iter
009100010905     C                   endif
009200010905
009300010905      *** F4 = division enquiry
009400010905     C                   if        *in04
009500010906     C                   call      'HMRENQ04R'
009600010905     C                   parm                    pay_div
009700010905     C                   iter
009800010905     C                   endif
009900010905
010000010905     ** validating date field
010100010905     C                   test(d)                 pay_date               25
010200010905     C                   if        *in25
010300010905     C                   eval      errmsg = date_err
010400010905     C                   iter
010500010905     C                   endif
010600010905
010700010905     ** validating payroll type
010800010905     C     run_type      chain     prtyfmt
010900010905     C                   if        not %found
011000010905     C                   eval      *in26 = '1'
011100010905     C                   eval      errmsg = type_err
011200010905     C                   iter
011300010905     C                   endif
011400010905
011500010905     ** validating payroll type
011600010905     C     pay_div       chain     divfmt
011700010905     C                   if        not %found
011800010905     C                   eval      *in27 = '1'
011900010905     C                   eval      errmsg = div_err
012000010905     C                   iter
012100010905     C                   endif
012200010905     C                   leave
012300010905     C                   enddo
012400010906     C                   eval      rep_title = %trim(arr(1))
012500010906     C                             + ' - ' + %trim(dtname)
012600010905     **
012700010905     C     den_key       setll     phisfmt
012800010905     C     den_key       reade     phisfmt
012900010905     C                   dow       not (%eof(payhisl04))
013000010905     C                   eval      amount = phisnpay
013100010905
013200010905     C                   callb     'PAYRPT0401'  den_list
013300010905     c
013400970107     **
013500010905     C                   do        stcnt         result
013600010905     C     result        occur     den_str
013700010905     C                   eval      acnt = 1
013800010905
013900010905     c     den_desc      lookup    den_arr(acnt)                          99
014000010905     C                   if        not *in99
014100010905     C                   eval      xcnt = xcnt + 1
014200010905     C                   eval      acnt = xcnt
014300010905     c                   eval      den_arr(acnt) = den_desc
014400010905     C                   endif
014500010905
014600010905     C                   eval      sav_arr(acnt) = sav_arr(acnt) + den_amt
014700010905     C                   eval      val_arr(acnt) = val_arr(acnt) + den_value
014800010905
014900010905     C                   Enddo
015000010905     C     den_key       reade     phisfmt
015100010905     C                   enddo
015200970107     **
015300010905     C                   exsr      report_out
015400010905     C                   eval      *inlr = '1'
015500010905     C                   return
015600111002      /free
015700111002
015800111002         //*****************************
015900111002         // Session Init
016000111002         //*****************************
016100111002
016200111002          begsr *inzsr;
016300111006            get_session_info('PAY':errmsg:@pgm);
016400111002            if errmsg <> *blanks;
016500111002              show_msg(errmsg);
016600111002              *inlr = *on;
016700111002              return;
016800111002            endif;
016900111002
017000111002            open(e) payhisl04;
017100111002            open(e) payrtyp;
017200111002            open(e) paydiv;
017300111002
017400111002          endsr;
017500111002      /end-free
017600010905     ***************************************************
017700010905     C     report_out    BEGSR
017800010905     C                   except    header
017900010905     C                   do        xcnt          tot_cnt
018000010905     C                   eval      denom_amt = sav_arr(tot_cnt)
018100010905     C                   eval      denom_desc = den_arr(tot_cnt)
018200010905     C                   eval      denom_value = val_arr(tot_cnt)
018300010905     C                   eval      tot_val = tot_val + denom_value
018400010905     C                   except    detail
018500010905     C                   enddo
018600010905     C                   except    fin_total
018700010905     C                   ENDSR
018800010905     ***************************************************
018900010905     ** First initilisation routine for all programs  **
019000010905     ***************************************************
019100010905     C     AAINIT        BEGSR
019200010905     C                   eval      @title = arr(1)
019300010905     C                   eval      coname = get_coname
019400010905     C                   eval      @date = get_sys_date
019500010905     **
019600010905     C                   ENDSR
019700950731     **
019800010905     Oqsysprt   e            header         1 02
019900950907     O                       @PGM                10
020000010905     O                       coname              75
020100010906     O                                          120 'Run Date: '
020200010906     O                       @date             +  1
020300010905     Oqsysprt   E            header         1
020400950907     O                       @USER               10
020500010906     O                       rep_title           80
020600010906     O                                          120 'Payroll Date:'
020700010906     O                       pay_date          +  1 '    /  /  '
020800010905     Oqsysprt   e            detail      2
020900010905     O                       denom_amt     Zb    40
021000010905     O                       Denom_desc     b    76
021100970107     O                                           81 '='
021200010905     o                       denom_value   1     97 '$'
021300010905     Oqsysprt   e            fin_total   2
021400010905     o                       tot_val       1     97 '$'
021500010905**
021600010906     Denomination Breakdown Listing
