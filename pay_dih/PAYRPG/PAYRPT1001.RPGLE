000100950726     H DECEDIT('.') DATEDIT(*DMY/)
000200020129     F** TIMESHEET REPORT
000300011003     F**                     C.WILLIAMS   & S.BRITTON
000400020924     Fpaytimfl03if   e           k disk
000500020607     Fpayrtypl01if   e           K DISK
000600020503     Fpaycycd   if   e           K DISK
000700011003     Fqsysprt   O    f  132        printer oflind(*inof)
000800011003     ** Payroll PROTOTYPES
000900170207      /copy QCPYSRC,genPR
001000170207      /copy QCPYSRC,PAYPR
001100170207      /copy qcpysrc,emppr
001200011003     **
001300020129     D opt             s              1
001400020118     D seq             s              1  0
001500020129     D dhours          s              5  2
001600020607     D dept_hrs        s              5  2
001700020129     D cycle           s              2
001800020129     D error           s               n
001900040222     D*dept            s              3  0
002000020129     D dept#           s              3  0
002100020425     D name#           s             40
002200020118     D name            s             30
002300020118     D coname          s             30
002400020118     D @title          s             40
002500020118     D @date           s             10
002600020503     Dpayrun_factor    s              7  3
002700020607     Dpayroll_factor   s              7  3
002800020118     D arr             S             40    dim(1) ctdata perrcd(1)
002900020118     **
003000020425     D numcnst         c                   'EMPLOYEE NAME        &         NUMB-
003100020425     D                                     ER'
003200011003     **
003300020129     D                sds
003400950930     D  @PGM             *PROC
003500950930     D  @USER                254    263
003600011003     **
003700020118     C     *entry        plist
003800020129     C                   parm                    opt
003900020129     C                   parm                    dept#
004000020118     C                   parm                    seq
004100020118
004200020118     C                   exsr      aainit
004300020425     C**                 eval      coltxt = numcnst
004400011003     C
004500011003     C                   except    hedr
004600011003     **
004700020129     C     *loval        setll     timfmt
004800020129     C                   read      timfmt                                 lr
004900020118     **
005000020607     C                   if        not(*inlr)
005100020607     ** get payroll run type cycle
005200020607     C     timrtype      chain     prtyfmt
005300020607     C     prtycycode    chain     cycfmt
005400020607     C                   eval      payrun_factor = cycyearf
005500020607
005600011003     C                   dow       not(*inlr)
005700020129
005800020425     C                   callp     get_emp_info(timemp:dept:*blanks:name#)
005900020129     C                   callp     get_dept_info(dept:error:dhours:cycle)
006000020129
006100020129     C                   if        dept# = *zeros or dept# <> *zeros
006200020129     C                             and dept# = dept
006300020607
006400020607     C                   if        opt = 'Y'
006500020607     C                   exsr      get_dept_hrs
006600020607     C                   endif
006700011003     **
006800020129     C                   if        opt = 'Y'
006900020607     C                                 and dept_hrs < timhrs
007000020129     C                                    or opt <> 'Y'
007100020425
007200020425     C                   eval      name = name#
007300020425     ** Print Header on Overflow
007400011003     C                   if        *inof
007500011003     C                   except    hedr
007600011003     C                   endif
007700011003
007800011003     C                   except    detail
007900011003     ** get dept
008000011003     **
008100011003     C                   endif
008200020129     C                   endif
008300020129     C                   read      timfmt                                 lr
008400011003     C                   enddo
008500020607
008600020607     C                   endif
008700020607     ************************************
008800020607     ** Get Correct Department hours   **
008900020607     ************************************
009000020607     C     get_dept_hrs  begsr
009100020607     ** get payroll type cycle
009200050525      /free
009300050525                get_payty_info('RD':timtyp:payty_struc);
009400050525      /end-free
009500020607     C                   if        ptyclcode <> *blanks
009600020607     C     ptyclcode     chain     cycfmt
009700020607     C                   eval      payroll_factor = cycyearf
009800020607     C                   else
009900020607     C                   eval      payroll_factor = payrun_factor
010000020607     C                   endif
010100020607     ** get Run-type cycle
010200020607     C     cycle         chain     cycfmt
010300020607     C                   eval      dept_hrs = dhours * cycyearf /
010400020607     C                                        payroll_factor
010500020607     C                   endsr
010600011003     ***************************************************
010700011003     ** First initilisation routine for all programs  **
010800011003     ***************************************************
010900011003     C     aainit        Begsr
011000020417     **
011100011003     C                   eval      coname = get_coname
011200011003     C                   eval      @date = get_sys_date
011300020118     C                   eval      @title = arr(1)
011400011003     **
011500011003     C                   Endsr
011600020118     **
011700020417     Oqsysprt   E            hedr           2 04
011800020417     O                       @pgm                10
011900011004     O                       coname              72
012000011004     O                       @date              130
012100020417     **
012200020417     O          E            hedr           2
012300020417     O                       @user               10
012400011004     O                       @title              80
012500011004     O                                          125 'Page:'
012600011004     O                       page          z    130
012700020417     **
012800020417     O          E            hedr           2
012900020425     O                       numcnst             37
013000020425     O                                           52 'Paytype'
013100020924     O                                           60 'Dept'
013200020425     O                                           67 'Run'
013300020425     O                                           80 'Hours'
013400020425     O                                           95 'Amount'
013500020118
013600020118     O          e            detail         1
013700020118     **
013800020425     O                       name                30
013900020425     O                       timemp        z     37
014000020924     O                       timtyp        1     51
014100020924     O                       dept          1     60
014200020612     O                       timrtype            67
014300020612     O                       timhrs        1     80
014400020612     O                       timamt        1     95
014500011003**
014600020425    Payroll Time Sheet Report
