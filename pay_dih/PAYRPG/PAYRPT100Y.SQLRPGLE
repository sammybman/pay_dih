000100950726     H DECEDIT('.') DATEDIT(*DMY/)
000200020129     F** TIMESHEET REPORT
000300011003     F**                     C.WILLIAMS   & S.BRITTON
000400020925     Fpaytimf   if   e           k disk
000500020925     Fempmast   if   e           k disk
000600020607     Fpayrtypl01if   e           K DISK
000700020503     Fpaycycd   if   e           K DISK
000800020503     Fpaytypl01 if   e           K DISK
000900011003     Fqsysprt   O    f  132        printer oflind(*inof)
001000011003     ** Payroll PROTOTYPES
001100011003      /copy genPR
001200020129      /copy paylib/payrpg,PAYPR
001300011003     **
001400020129     D opt             s              1
001500020118     D seq             s              1  0
001600020129     D dhours          s              5  2
001700020607     D dept_hrs        s              5  2
001800020129     D cycle           s              2
001900020129     D error           s               n
002000020129     D dept            s              3  0
002100020129     D dept#           s              3  0
002200020425     D name#           s             40
002300020118     D name            s             30
002400020118     D coname          s             30
002500020118     D @title          s             40
002600020118     D @date           s             10
002700020503     Dpayrun_factor    s              7  3
002800020607     Dpayroll_factor   s              7  3
002900020118     D arr             S             40    dim(1) ctdata perrcd(1)
003000020118     **
003100020425     D numcnst         c                   'EMPLOYEE NAME        &         NUMB-
003200020425     D                                     ER'
003300011003     **
003400020129     D                sds
003500950930     D  @PGM             *PROC
003600950930     D  @USER                254    263
003700011003     **
003800020118     C     *entry        plist
003900020129     C                   parm                    opt
004000020129     C                   parm                    dept#
004100020118     C                   parm                    seq
004200020118
004300020118     C                   exsr      aainit
004400020425     C**                 eval      coltxt = numcnst
004500011003     C
004600011003     C                   except    hedr
004700020924     C                   exsr      readtimf
004800011003     **
004900020924     C*    *loval        setll     timfmt
005000020924     C*                  read      timfmt                                 lr
005100020118     **
005200020924     C                   if        dept <> 0
005300020924     C*                  if        not(*inlr)
005400020607     ** get payroll run type cycle
005500020607     C     timrtype      chain     prtyfmt
005600020607     C     prtycycode    chain     cycfmt
005700020607     C                   eval      payrun_factor = cycyearf
005800020607
005900011003     C                   dow       not(*inlr)
006000020129
006100020425     C                   callp     get_emp_info(timemp:dept:*blanks:name#)
006200020129     C                   callp     get_dept_info(dept:error:dhours:cycle)
006300020129
006400020129     C                   if        dept# = *zeros or dept# <> *zeros
006500020129     C                             and dept# = dept
006600020607
006700020607     C                   if        opt = 'Y'
006800020607     C                   exsr      get_dept_hrs
006900020607     C                   endif
007000011003     **
007100020129     C                   if        opt = 'Y'
007200020607     C                                 and dept_hrs < timhrs
007300020129     C                                    or opt <> 'Y'
007400020425
007500020425     C                   eval      name = name#
007600020425     ** Print Header on Overflow
007700011003     C                   if        *inof
007800011003     C                   except    hedr
007900011003     C                   endif
008000011003
008100011003     C                   except    detail
008200011003     ** get dept
008300011003     **
008400011003     C                   endif
008500020129     C                   endif
008600020924     C                   exsr      readtimf
008700020924     C**                 read      timfmt                                 lr
008800011003     C                   enddo
008900020607
009000020607     C                   endif
009100020607     ************************************
009200020607     ** Get Correct Department hours   **
009300020607     ************************************
009400020607     C     get_dept_hrs  begsr
009500020607     ** get payroll type cycle
009600020607     C     timtyp        chain     ptyfmt
009700020607     C                   if        ptyclcode <> *blanks
009800020607     C     ptyclcode     chain     cycfmt
009900020607     C                   eval      payroll_factor = cycyearf
010000020607     C                   else
010100020607     C                   eval      payroll_factor = payrun_factor
010200020607     C                   endif
010300020607     ** get Run-type cycle
010400020607     C     cycle         chain     cycfmt
010500020607     C                   eval      dept_hrs = dhours * cycyearf /
010600020607     C                                        payroll_factor
010700020607     C                   endsr
010800011003     ***************************************************
010900011003     ** First initilisation routine for all programs  **
011000011003     ***************************************************
011100011003     C     aainit        Begsr
011200020417     **
011300011003     C                   eval      coname = get_coname
011400011003     C                   eval      @date = get_sys_date
011500020118     C                   eval      @title = arr(1)
011600011003     **
011700011003     C                   Endsr
011800020924
011900020924     C     Opentimf      BEGSR
012000020924      *    ----------
012100020924      *
012200020925     C/Exec Sql
012300020925     C+  Select count(*)
012400020925     C+      into :TOTALCNT
012500020924     C+
012600020924     C+  From PAYTIMF, EMPMAST         -- From Paytimf & Empmast files
012700020924     C+
012800020924     C+  Where TIMEMP = EMP            -- Employee nos. equal
012900020924     C+
013000020924     C+  Order By DEPT                 -- Sort By Department no.
013100020924     C+
013200020924     C/End-Exec
013300020924      *
013400020925     C                   ENDSR
013500020924      *---------------------------------------------------------------------
013600020924
013700020924
013800020118     **
013900020417     Oqsysprt   E            hedr           2 04
014000020417     O                       @pgm                10
014100011004     O                       coname              72
014200011004     O                       @date              130
014300020417     **
014400020417     O          E            hedr           2
014500020417     O                       @user               10
014600011004     O                       @title              80
014700011004     O                                          125 'Page:'
014800011004     O                       page          z    130
014900020417     **
015000020417     O          E            hedr           2
015100020425     O                       numcnst             37
015200020425     O                                           52 'Paytype'
015300020924     O                                           60 'Dept'
015400020425     O                                           67 'Run'
015500020425     O                                           80 'Hours'
015600020425     O                                           95 'Amount'
015700020118
015800020118     O          e            detail         1
015900020118     **
016000020425     O                       name                30
016100020425     O                       timemp        z     37
016200020924     O                       timtyp        1     51
016300020924     O                       dept#         1     60
016400020612     O                       timrtype            67
016500020612     O                       timhrs        1     80
016600020612     O                       timamt        1     95
016700011003**
016800020425    Payroll Time Sheet Report
