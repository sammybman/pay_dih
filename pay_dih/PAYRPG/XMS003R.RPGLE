000100110803c    H DECEDIT('.') DATEDIT(*DMY/)  dftactgrp(*no)
000200110803     H  bnddir('DISDIR':'PAYDIR')
000300950620     H* MONTHLY & WEEKLY XMAS BONUS CALCULATION OF NET BONUS PAYABLE
000400950725     FXMSBONL1  UP   E           K DISK
000500011122     Fdeptm     IF   E           K DISK
000600020927     Fxmasfcast if   e           k disk
000700011122     Fxmascon   if   e           k disk    usropn
000800011115
000900011119     Fpayrdefl03if   e           K DISK
001000011122     Fpaytimfl01o    e           K DISK
001100011123     Fpayrtypl01uf   e           K DISK
001200011115     Fpaycycd   if   e           K DISK
001300021204     Fqsysprt   O    F  132        PRINTER OFLIND(*inof)
001400991206     **
001500011112
001600011122     d                 ds                  inz
001700011122     dcoe_amt                        13  2
001800011122     dgros_amt                       13  2
001900011116
002000011119     D  coname         s             30
002100011115     D  hrs_dummy      s              5  2 inz(0)
002200011115     D  penamt         s              9  2
002300011112     D  coepen         s              9  2
002400011112     D  coe            s              9  2
002500011112     D  totpen         s                   like(coepen)
002600011112     D  pendif         s                   like(coepen)
002700011112     D  net            s                   like(coepen)
002800011115     D  award1         s                   like(coepen)
002900011115     D  award2         s                   like(coepen)
003000011116     D  payrun_factor  s                   like(cycyearf)
003100020304     D  payrun_qty     s              2  0
003200011119     D  run_cycle      s              2
003300011119     D  pay_amt        s             13  2
003400011123     Ddate             s              8  0
003500011123     Dcurdate          s               d   datfmt(*iso)
003600011123     dnxtdate          s               d   datfmt(*iso)
003700011123     dno_days          s              4  0
003800011112     Dtotper           s              4  1
003900011113     Dpertot           s              4  1
004000991206     **
004100011119     Dcnt              s              1  0
004200011119     Dxcnt             s              1  0
004300011122     **
004400011123     D name#           s             40
004500011123     D name            s             26
004600011123     D class#          s              3
004700020927     D xmclass#        s              3
004800011123     D err#            s              7
004900040224     D*status          s              1
005000020927     D fcast           s              1
005100011122     **
005200020927     D msg1            C                   'Forecast Report? Y = YES'
005300020927     **
005400950921     D                SDS
005500950921     D  @PGM             *PROC
005600111201     D  @USER                358    367
005700011109     **
005800110706      /copy qcpysrc,genpr
005900110706      /copy qcpysrc,emppr
006000110706      /copy qcpysrc,paypr
006100011115     D@date            s             10
006200011122     IXMSREC        01
006300950725     I                                          EMP           L1
006400950725     I                                          DEPT          L2
006500981217     **
006600020927
006700020927     C     bon_key       klist
006800020927     C                   kfld                    prdftype
006900020927     C                   kfld                    xmclass#
007000011115
007100981217     **
007200991206     **-------------------------------------------------------------------------
007300991206     ** Set payroll type to XMAS BONUS
007400011119     C                   eval      cnt = 0
007500011122     c                   callp     get_emp_info(emp:dept:*blanks:name#:
007600011122     c                                     class#:*omit:*omit:err#)
007700011122     C                   eval      name = name#
007800020927     C                   if        err# = *blanks
007900011119
008000021204     C     Dept          chain     dhrec
008100011119
008200011122     C     xcrtype       setll     prdffmt
008300011122     C     xcrtype       reade     prdffmt
008400011119     C                   dow       not(%eof(payrdefl03))
008500011116     ** get payroll type cycle
008600171024
008700050928               get_payty_info('RD0':prdftype:payty_struc);
008800050520
008900151202        if ptyrtype = 'E';
009000050520
009100151202          cnt += 1;
009200020927        select;
009300020927        when fcast = 'Y';
009400020927          xmclass# = %xlate('P':' ':class#);
009500020927          chain bon_key xmsfmt;
009600020927            if %found;
009700020927               pay_amt = xmamount;
009800020927              else;
009900020927               pay_amt = *zeros;
010000020927            endif;
010100020927        other;
010200020927          pay_amt = get_emp_pay(emp:dept:prdftype:
010300020927                        hrs_dummy:payrun_factor:
010400020927                              payrun_qty:status);
010500020927        endsl;
010600171024
010700011116
010800011119     ** if is sales dept only pay 80%
010900011122     C                   if        dtype = 'S'
011000040622     C                   eval      pay_amt *= 0.80                              SALESMEN
011100011119     C                   Endif
011200011119
011300011119     c                   if        cnt = 1
011400011119     C                   eval      gros_amt = pay_amt                           SALESMEN
011500011119     C                   else
011600011119     C                   eval      coe_amt = pay_amt                            SALESMEN
011700011119     C                   Endif
011800011119
011900011119     C                   Endif
012000011119
012100011122     C     xcrtype       reade     prdffmt
012200011119     C                   enddo
012300991206     **-------------------------------------------------------------------------
012400951211     ******************************************************************
012500951211     *** ONLY VALID EMPLOYEES ***
012600951211     ******************************************************************
012700011122     **
012800011122     C                   eval      totper = 0
012900011113     C                   eval      Pendif = 0
013000011122
013100011122     c                   if        nempflag = 'Y'
013200011122     C                   eval      gros_amt = gros_amt * percen / 100
013300011122     C                   eval      coe_amt  = coe_amt * percen / 100
013400011122     C                   endif
013500011108     **
013600011115
013700011113     C                   eval      pertot = award + percen
013800011115
013900011115
014000011122     ** XMAS BONUS AMOUNT CALCULATION ROUTINE
014100011122     ** XMAS BONUS LETTERS & PAYSLIPS ROUTINE - OTHERS
014200011122     ** PAYSLIPS **
014300011122     **
014400011113     C                   eval      totper = secper + fstper  + awolpr
014500111201     C                                    + latepr
014600011112
014700011113      **  totper < 75
014800011122     C                   if        totper < 75
014900011113     ** if percen < 100
015000011122     C                   if        percen < 100
015100011112     C
015200011122     C                   eval      Penamt  = totper * gros_amt / 100
015300011122     C                   eval      coepen  = totper * coe_amt  / 100
015400011112     C
015500011112     C                   eval      totpen = penamt +  coepen                    TOTAL PENALTIES
015600011115     C                   endif
015700011122     ** if totpen >= basamt
015800011122     C                   if        totpen >= (gros_amt * .75)
015900011122     C                   eval      pendif = totpen - (gros_amt * .75)
016000011119     C                   eval      net  = gros_amt * .25
016100011119     C                   eval      coe = coe_amt - pendif
016200011112     c                   else
016300011122     C                   eval      net = gros_amt - totpen
016400011122     C                   eval      coe  = coe_amt
016500011112     C                   endif
016600011115
016700011113     c                   else
016800011122      ** totper >= 75
016900011122     C                   eval      coe =  coe_amt * .25
017000011122     C                   eval      net  = gros_amt * .25
017100011112     C                   endif
017200011108
017300011119
017400011122     c                   if        dtype = 'S' or percen >= 100
017500011122     C                   eval      net = gros_amt * percen / 100
017600011122     C                   eval      coe  = coe_amt * percen / 100
017700011122     C                   endif
017800011119
017900011122     c                   eval      award1 = award * gros_amt / 100
018000011122     C                   eval      award2 = award  * coe_amt / 100
018100011119     c                   eval      net = net + award1
018200011119     c                   eval      coe = coe + award2
018300011119
018400011108
018500011122     ** LETTERS **
018600011122     **
018700011115
018800011115      **  totper < 75
018900011122     c                   if        totper < 75
019000011122     c                   if        percen < 100
019100011122     C                   eval      Letcoe = coe_amt - coepen
019200011122     C                   eval      Letbas = gros_amt - penamt
019300011115     c                   endif
019400011115
019500011115     c                   else
019600011119     C                   eval      Letbas = gros_amt * .25
019700011119     C                   eval      Letcoe = coe_amt * .25
019800011115     c                   endif
019900011119
020000011119     C                   eval      letbas = AWARD1 + LETBAS
020100011119     C                   eval      letcoe = AWARD2 + Letcoe
020200011115
020300011122     C                   eval      gros = gros_amt
020400011122     C                   eval      gcoe = coe_amt
020500011122     **
020600950725     C                   ADD       NET           DEPTOT            9 2
020700011119     C                   ADD       gros_amt      DEPGRO            9 2
020800950725     C                   ADD       NET           FINTOT           10 2
020900011119     C                   ADD       GROS_AMT      FINGRO           10 2
021000011119     C                   ADD       coe           DEPCOE           10 2
021100011119     C                   ADD       coe           FINCOE           10 2
021200011119     C                   ADD       coe_amt       DPGCOE           10 2
021300011119     C                   ADD       coe_amt       FIGCOE           10 2
021400020927      /free
021500020927       if fcast <> 'Y';
021600020927         Update  xmsrec;
021700020927       endif;
021800011108
021900021204
022000021204       // *******************************************************
022100011115
022200020927       if fcast <> 'Y';
022300020927                  //** Write Info to work file
022400020927                  //** net & coe are output fields
022500021204        for xcnt = 1 to cnt;
022600020927           if xcnt = 1;
022700020927             timamt = net;
022800020927             timtyp = 18;
022900020927               else;
023000020927             timtyp = 19;
023100020927             timamt = coe;
023200020927           endif;
023300011119
023400020927           if timamt > *zeros;
023500020927               timemp = emp;
023600020927               timcode = 'E';
023700020927               timrtype = xcrtype;
023800020927               timhrs = 0;
023900020927               timamt = round_up(timamt:'U');
024000020927                write timfmt;
024100020927           endif;
024200020927
024300020927          clear timfmt;
024400020927
024500020927        endfor;
024600020927       endif;
024700171024
024800011115
024900011116     C                   Else
025000011116     **
025100011119     C                   Delete    Xmsrec
025200011116     C                   Endif
025300011115
025400011123     ** Set Payroll Prep. flag
025500020927     Clr                 if        fcast <> 'Y'
025600011123     Clr                 eval      prtyflag = 'Y'
025700011123     Clr                 update    prtyfmt
025800020927     Clr                 Endif
025900011115     *********************************
026000011115     ***  initial pgm subroutine   ***
026100011115     *********************************
026200011119     C     *inzsr        begsr
026300011123     ** Get system date / company name
026400011123     C                   eval      coname = get_coname
026500011123     C                   eval      @DATE = get_sys_date(date)
026600011123     C                   move      date          curdate
026700011130
026800011130     C                   open      Xmascon
026900011130     C                   read      xmascon
027000020927     **
027100020927     C     msg1          dsply                   fcast
027200020927     **
027300020927     c                   if        fcast = 'y'
027400020927     C                   eval      fcast = 'Y'
027500020927     C                   endif
027600020927
027700020927     c                   if        fcast <> 'Y'
027800020927     **
027900020927     ** get Run-type cycle
028000020927     ** get payroll cycle code
028100011130     C     xcrtype       chain     prtyfmt
028200011123     C                   if        prtyndate > *zeros
028300011123     C                   move      prtyndate     nxtdate
028400011123     C     nxtdate       subdur    curdate       no_days:*d
028500011123     C                   if        no_days > prtyavail
028600011123     C                   call      'ESCPMSG'
028700011123     C                   parm                    @pgm
028800011123     C                   parm      'PAY0001'     msgid             7
028900011123     C                   endif
029000011123     C                   endif
029100011123
029200011123     C     prtycycode    chain     cycfmt
029300011123     C                   eval      payrun_factor = cycyearf
029400020304     C                   eval      payrun_qty  = prtycycqty
029500011122     C     *lock         in        *dtaara
029600011122     C     *dtaara       define    xmsseqchk     xmseq#            2 0
029700011122     ** first page handler
029800011122     C                   if        xmseq#  <> 3
029900011122     C                   call      'ESCPMSG'
030000011122     C                   parm                    @pgm
030100011122     C                   parm      'DIH0131'     msgid             7
030200011122     C                   endif
030300011122     **
030400011122     C                   eval      xmseq# = 4
030500011122     c                   out       xmseq#
030600011122     **
030700011122     ** CLEAR FILE BEFORE WRITING NEW INFORMATION
030800011122     **
030900011122     C                   callb     'PAYCALCCLR'
031000011122     C                   parm                    xcrtype
031100020927     C                   endif
031200011122
031300011115     C                   endsr
031400011122     ******************************************************8
031500021204     Oqsysprt   H    OFNL2                  2 04
031600950620     O         OR    L2
031700950921     O                       @PGM                10
031800011115     O                       coname              80
031900011122     O                                          118 'DATE  PREPARED'
032000011122     O                       @DATE              130
032100950620     O          H    OFNL2                  3
032200950620     O         OR    L2
032300011122     O                       @USER               10
032400011122     O                                           77 'EMPLOYEE CHRISTMAS BONUS'
032500940516     O                                          128 'PAGE'
032600940516     O                       PAGE               132
032700950620     O          H    OFNL2                  2
032800950620     O         OR    L2
032900011122     O                                           25 'Department:'
033000011122     O                       Dept          Z     30
033100011112     O                       DNAME               71
033200950620     O          H    OFNL2                  1
033300950620     O         OR    L2
033400950620     O                                            6 'EMP'
033500950620     O                                           67 'GROSS'
033600950620     O                                           80 'GROSS'
033700950620     O                                           92 '%'
033800950620     O                                          112 'NET'
033900950620     O                                          130 'NET'
034000950620     O          H    OFNL2                  2
034100950620     O         OR    L2
034200950620     O                                            6 'NO'
034300011122     O                                           23 'NAME'
034400011122     O                                           47 'GRADE'
034500011122     O                                           66 'TAXABLE'
034600011122     O                                           81 'ALLOWANCE'
034700950620     O                                           94 'PAID'
034800950620     O                                          113 'TAXABLE'
034900011122     O                                          132 'ALLOWANCE'
035000021204     Oqsysprt   D    01                     2
035100011119     O                       emp           Z      6
035200011122     O                       name           B    40
035300011122     O                       Class#              45
035400011122     O                       gros_amt      3     66
035500011119     O                       coe_amt       3     81
035600011122     O                       pertot        1     94
035700011122     O                                           95 '%'
035800011119     O                       net           L    114
035900011119     O                       coe           L    132
036000021204     Oqsysprt   T    L2                     1
036100950620     O                                           65 'DEPARTMENT TOTALS --'
036200950620     O                       DEPGRO        1B    68
036300950620     O                       DPGCOE        1B    81
036400950620     O                       DEPTOT        1B   113
036500950620     O                       DEPCOE        1B   132
036600021204     Oqsysprt   T    LR                  2  3
036700950620     O                                           53 'FINAL TOTALS --'
036800950620     O                       FINGRO        1     68
036900950620     O                       FIGCOE        1     82
037000950620     O                       FINTOT        1    113
037100950620     O                       FINCOE        1    132
