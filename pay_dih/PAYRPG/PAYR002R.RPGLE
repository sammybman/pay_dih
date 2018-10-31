000100170221     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR') alwnull(*usrctl)
000200111002     H/define profoundui
000300010427     H******** PROGRAMMER - S.Britton         ************
000400010427     H********            - C.Williams        ************
000500111002     Fpayrtypl01if   e           K disk    usropn
000600111002     Fpayrdefl03if   e           K DISK    usropn
000700111002     Fpayrresl01if   e           k disk    usropn
000800010906     Fempmast   uf   e           K disk    usropn
000900111002     Ftimecrdf  uf a e           K disk    usropn
001000111002     Fpaycycd   if   e           k disk    usropn
001100010906     Fpayr002d  cf   e             workstn
001200010426     f                                     sfile(sflrec:rrn)
001300111002     F/IF DEFINED(WEB)
001400111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001500111002     F/ENDIF
001600010802     ** Payroll PROTOTYPES
001700111002      /copy QCPYSRC,PAYPR
001800111002      /copy QCPYSRC,genPR
001900111002      /copy QCPYSRC,autPR
002000111002      /copy QCPYSRC,emppr
002100010420     **
002200010906     D emp_status      s              1
002300010816     D pay_opt         s              1
002400010906     D type_sts        s              1
002500040224     D dept#           s              3s 0
002600010802     D name#           s             40
002700010802     D err#            s              7
002800010802     D rrn             s              4  0
002900010816     D arr             s             40    dim(1) CTDATA PERRCD(1)
003000010427     D start           s               n   inz(*on)
003100010427     D valemp          s               n   inz(*off)
003200010427     D calcs           s               n   inz(*off)
003300040224     D dlocat          s              2  0                                      ACTION DESCRIPT.
003400040224     D ddivis          s              2  0                                      ACTION DESCRIPT.
003500010802     **
003600010427     D msg1            c                   const('Invalid Run Type Entered')
003700010816     D msg2            c                   const('Cannot Use this Run Type')
003800010427     **
003900980209     D                SDS
004000980209     D  @PGM             *PROC
004100010420     **
004200010427     C     trn_key       klist
004300010427     C                   kfld                    run_type
004400010816     C                   kfld                    emol_typ#
004500010427     C                   kfld                    emp#
004600010427     **
004700010906     C                   move      '0'           *in
004800010427
004900010427     ***********************
005000010806     ** Get run type info **
005100010427     ***********************
005200040207      /FREE
005300111002         // authority check
005400111002         monitor;
005500111002           authority_check('PAY':@pgm);
005600111002         on-error;
005700111002           show_msg(get_err_msg('DIH0054'));
005800111002           *inlr = *on;
005900111002           return;
006000111002         endmon;
006100040207      /end-free
006200010427     C                   exsr      aainit
006300010427     C                   dow       start = *on
006400010427     **
006500010427     C                   exfmt     fmt0
006600010427     C                   clear                   errmsg
006700010427     **
006800010427     C                   if        *in03
006900010724     c                   eval      *inlr = '1'
007000010427     C                   return
007100010427     C                   endif
007200010427     **
007300010427     C     run_type      chain     prtyfmt
007400010427     ** Invalid Run type entered
007500010724     C                   if        not(%found)
007600010724     c                   eval      errmsg = msg1
007700010427     C                   iter
007800010427     C                   endif
007900010724
008000010724     ** SPECIAL Run type entered  **
008100010724     C                   if        prtyprno = 0
008200010816     C                   eval      errmsg = msg2
008300010816     C                   iter
008400010816     C                   endif
008500010806     *******************************
008600010724
008700010724     C     run_type      chain     prrefmt
008800010427     **
008900010427     C                   eval      valemp = *on
009000010427     C                   leave
009100010427     C                   enddo
009200010806
009300010427     ***********************
009400010427     ** get employee info **
009500010427     ***********************
009600010427     C                   dow       valemp = *on
009700010427     C                   exfmt     fmt1
009800010427     C                   clear                   errmsg
009900980209     **
010000010426     C                   if        *in03
010100010426     C                   eval      *inlr = '1'
010200010426     C                   return
010300980209     C                   endif
010400010427     ** go Back
010500010427     C                   if        *in02
010600010427     C                   leave
010700010427     C                   endif
010800010802     ** Validate Employee number & get employee info.
010900010906     c                   eval      emp_status = get_emp_info(emp#:dept#:
011000010906     c                                prrelist:name#:*omit:*omit:*omit:err#)
011100010802     **
011200010802     c                   if        err# <> *blanks
011300010802     ** Error message
011400010802     C                   Call      'MESSAGE'
011500010802     C                   Parm      err#          MSGID             7
011600010802     C                   Parm      *BLANKS       ERRMSG
011700010802     C                   else
011800010802     C                   eval      emp_name = name#
011900010802     C                   eval      errmsg = *blanks
012000010802     c                   endif
012100010427     **
012200010427     C                   if        errmsg <> *blanks
012300010427     C                   iter
012400010427     C                   endif
012500010802     **
012600010906     C                   if        get_cat(emp_status:*omit:pay_opt:type_sts)
012700010906     C                                     = 'A'
012800010427
012900010906     ** allow change of department for temp. staff
013000010906     C                   if        type_sts = 'T'
013100010906     C                   eval      new_dept = dept#
013200010906     C                   exsr      set_department
013300010906     C                   endif
013400010906
013500010427     ** clear subfile and reset all values
013600010427     C                   eval      *in33 = '1'
013700010427     C                   write     sflctl
013800010427     C                   eval      rrn = 0
013900010427     C                   eval      *in33 = '0'
014000010427
014100010806     ** Load subfile
014200010816     C     run_type      setll     prdffmt
014300010816     C     run_type      reade     prdffmt
014400010806     C                   dow       not(%eof)
014500050519      /free
014600050519               get_payty_info('RD':prdftype:payty_struc);
014700050519
014800050525      /end-free
014900010502     ** Show record if link request = run type
015000010502     C                   if        ptylink = *blanks
015100010502     C                             or ptylink <> *blanks
015200010502     C                             and ptylink = run_type
015300010802
015400010802     **  Protect Hours Fields for Money type transactions
015500010816     C                   if        pay_opt = 'H' and ptypaytyp = 'B'
015600010816     C                             or pay_opt = 'H' and ptypaytyp = 'H'
015700020711     C                             or pay_opt = 'M' and ptypaytyp = 'H'
015800010816     C
015900010802     **
016000010427     C                   eval      rrn = rrn + 1
016100010427     C                   eval      emol_typ# = ptyptype
016200010427     C                   eval      emol_name = ptydesc
016300010806     **
016400010816     C     trn_key       chain     timcfmt
016500010427     C                   if        %found
016600010816     C                   eval      wrkhrs# = timc_hrs
016700010427     C                   else
016800010427     C                   eval      wrkhrs# = 0
016900010427     C                   endif
017000010427     C                   write     sflrec
017100010806     C                   endif
017200010816     C                   endif
017300010806
017400010816     C     run_type      reade     prdffmt
017500010806     C                   enddo
017600010816     C                   endif
017700010816     ** ind 60 show "BOTTOM"
017800010816     C                   eval      *in60 = *on
017900010816     ** ind 75 off show message
018000010816     C                   eval      *in75 = rrn > 0
018100010427     C                   eval      calcs = *on
018200010427     C                   eval      start = *off
018300010426     C                   leave
018400010426     C                   enddo
018500010427     ************************
018600010427     ** Program calcs.
018700010427     ************************
018800010427     C                   dow       calcs = *on
018900010426
019000010426     C                   write     footer
019100010426     C                   exfmt     sflctl
019200010427     ** End Program
019300010426     C                   if        *in03
019400010426     C                   eval      *inlr = '1'
019500010426     C                   return
019600010426     C                   endif
019700010427     ** go Back
019800010427     C                   if        *in02
019900010427     C                   leave
020000010427     C                   endif
020100010816     ** ind 04 - Delete record
020200010816     C                   if        *in04
020300010816     C                   exsr      delete_rtn
020400010816     C                   leave
020500010816     C                   endif
020600010802     ** Update transactions
020700010802     C                   exsr      update_rtn
020800010427     ** f15 = Confirm
020900010427     C                   if        *in15
021000010802     C                   exsr      confirm_rtn
021100010427     C                   leave
021200010427     C                   endif
021300010426
021400010427     C                   enddo
021500111002
021600111002      /free
021700111002
021800111002         //*****************************
021900111002         // Session Init
022000111002         //*****************************
022100111002
022200111002          begsr *inzsr;
022300111006            get_session_info('PAY':errmsg:@pgm);
022400111002            if errmsg <> *blanks;
022500111002              show_msg(errmsg);
022600111002              *inlr = *on;
022700111002              return;
022800111002            endif;
022900111002
023000111002            open(e) payrtypl01;
023100111002            open(e) payrresl01;
023200111002            open(e) payrdefl03;
023300111002            open(e) timecrdf;
023400111002            open(e) paycycd;
023500111002
023600111002          endsr;
023700111002
023800111002      /end-free
023900010906     **********************************
024000010906     ** set department for Temp emp. **
024100010906     **********************************
024200010906     C     set_departmentbegsr
024300010906     C                   dow       0 = 0
024400010906     C                   exfmt     win1
024500010906     C                   eval      *in99 = *off
024600010906     ** reset department
024700010906     C                   if        new_dept = *zeros
024800010906     C                   eval      new_dept = dept#
024900010906     C                   iter
025000010906     C                   endif
025100010906     **
025200010906     C                   callp     get_dept_info(new_dept:*in99:*omit:*omit
025300010906     C                                :dlocat:ddivis)
025400010906     **
025500010906     C                   if        *in99
025600010906     C                   iter
025700010906     C                   endif
025800010906     ** do department change
025900010906     C                   if        new_dept <> dept#
026000010906     C                   open      empmast
026100010906     C     emp#          chain     emprec
026200010906     C                   if        %found
026300010906     C                   eval      dept = new_dept
026400010906     C                   eval      locat = dlocat
026500010913     C                   if        bank = *blanks
026600010913     C                   eval      divis = ddivis
026700010913     C                   endif
026800010906     C                   update    emprec
026900010906     C                   endif
027000010906     C                   close     empmast
027100010906     C                   endif
027200010906     **
027300010906     C                   leave
027400010906     C                   enddo
027500010906     C                   endsr
027600010427     **************************
027700010802     ** Confirm transactions **
027800010427     **************************
027900010802     C     confirm_rtn   begsr
028000010806     c                   z-add     rrn           x                 3 0
028100010802     c                   do        x             y                 3 0
028200010816     C     y             chain     sflrec
028300010816     C     trn_key       chain     timcfmt
028400010802     **
028500010816     C                   eval      timc_emp = emp#
028600010816     C                   eval      timc_hrs = wrkhrs#
028700010816     C                   eval      timc_ptyp = run_type
028800010816     C                   eval      timc_type  = emol_typ#
028900010802     C
029000010802     C                   if        %found
029100010816     c                   if        timc_hrs <> *zeros
029200010816     C                   update    timcfmt
029300010802     C                   Else
029400010816     C                   delete    timcfmt
029500010802     C                   Endif
029600010802     **
029700010802     C                   Else
029800010802     **
029900010802     ** Prevent from writing a record with 0 value
030000010802     C
030100010816     c                   if        timc_hrs <> *zeros
030200010816     C                   write     timcfmt
030300010802     C                   endif
030400010802     C
030500010802     C                   endif
030600010802     C
030700010802     C                   enddo
030800010802     c                   endsr
030900010816     **************************
031000010816     ** Delete transactions **
031100010816     **************************
031200010816     C     delete_rtn    begsr
031300010816     c                   z-add     rrn           x                 3 0
031400010816     c                   do        x             y                 3 0
031500010816     C     y             chain     sflrec
031600010816     C     trn_key       chain     timcfmt
031700010816     **
031800010816     C
031900010816     C                   if        %found
032000010816     C                   delete    timcfmt
032100010816     C                   endif
032200010816     C
032300010816     C                   enddo
032400010816     c                   endsr
032500010802     **************************
032600010802     ** update transactions  **
032700010802     **************************
032800010802     C     update_rtn    begsr
032900010806     c                   z-add     rrn           x                 3 0
033000010802     c                   do        x             y                 3 0
033100010806     C     y             chain     sflrec
033200010816     C                   if        %found
033300010806     **
033400010806     C**                 eval      wrkhrs# = wrkhrs# - trn_hrs
033500010806     C                   endif
033600010802     **
033700010802     C                   update    sflrec
033800010802     C                   enddo
033900010802
034000010802     c                   endsr
034100990413
034200010426     ***************************************************
034300010426     ** First initilisation routine for all programs  **
034400010426     ***************************************************
034500010426     C     AAINIT        BEGSR
034600010816     C                   eval      coname = get_coname
034700010816     C                   eval      @date = get_sys_date
034800010426     C                   time                    @TIME             6 0
034900010426     ** select header for program
035000010426     C                   movel(p)  ARR(1)        @TITLE
035100010426     **
035200010426     C                   ENDSR
035300010426     **
035400010426**  Program Title
035500010816       Payroll Hours Entry
