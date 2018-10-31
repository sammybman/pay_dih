000100950726     H DECEDIT('.') DATEDIT(*DMY/)
000200020129     F** TIMESHEET REPORT
000300011003     F**                     C.WILLIAMS   & S.BRITTON
000400020924     F**ytimf   if   e           k disk
000500020607     Fpayrtypl01if   e           K DISK
000600020503     Fpaycycd   if   e           K DISK
000700020503     Fpaytypl01 if   e           K DISK
000800011003     Fqsysprt   O    f  132        printer oflind(*inof)
000900011003     ** Payroll PROTOTYPES
001000011003      /copy genPR
001100020129      /copy paylib/payrpg,PAYPR
001200011003     **
001300020129     D opt             s              1
001400020118     D seq             s              1  0
001500020129     D dhours          s              5  2
001600020607     D dept_hrs        s              5  2
001700020129     D cycle           s              2
001800020129     D error           s               n
001900020129     D dept            s              3  0
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
004600020924     C                   exsr      opentimf
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
012200020924     C/Exec Sql Declare TIMECARD Cursor For
012300020924     C+  Select TIMEMP,
012400020924     C+         TIMTYP,
012500020924     C+         TIMRTYPE,
012600020924     C+         TIMHRS,
012700020924     C+         TIMAMT,
012800020924     C+         DEPT
012900020924     C+
013000020924     C+  From PAYTIMF, EMPMAST         -- From Paytimf & Empmast files
013100020924     C+
013200020924     C+  Where TIMEMP = EMP            -- Employee nos. equal
013300020924     C+
013400020924     C+  Order By DEPT                 -- Sort By Department no.
013500020924     C+
013600020924     C+  For Fetch Only                -- Read Only Cursor
013700020924     C/End-Exec
013800020924      *
013900020924     C/Exec Sql
014000020924     C+  Open TIMECARD
014100020924     C/End-Exec
014200020924      *
014300020924     CSR                 ENDSR
014400020924      *---------------------------------------------------------------------
014500020924     CSR   Closetimf     BEGSR
014600020924      *    ----------
014700020924     C/Exec Sql Close TIMECARD
014800020924     C/End-Exec
014900020924      *
015000020924     C                   ENDSR
015100020924
015200020924
015300020924     C     Readtimf      BEGSR
015400020924      *    ----------
015500020924      *   Read the records ...
015600020924     C/EXEC SQL
015700020924     C+ FETCH TIMECARD
015800020924     C+   INTO :TIMEMP,
015900020924     C+        :TIMRTYPE,
016000020924     C+        :TIMTYP,
016100020924     C+        :TIMHRS,
016200020924     C+        :TIMAMT,
016300020924     C+        :DEPT
016400020924     C/END-EXEC
016500020924      *
016600020924     CSR                 ENDSR
016700020118     **
016800020417     Oqsysprt   E            hedr           2 04
016900020417     O                       @pgm                10
017000011004     O                       coname              72
017100011004     O                       @date              130
017200020417     **
017300020417     O          E            hedr           2
017400020417     O                       @user               10
017500011004     O                       @title              80
017600011004     O                                          125 'Page:'
017700011004     O                       page          z    130
017800020417     **
017900020417     O          E            hedr           2
018000020425     O                       numcnst             37
018100020425     O                                           52 'Paytype'
018200020924     O                                           60 'Dept'
018300020425     O                                           67 'Run'
018400020425     O                                           80 'Hours'
018500020425     O                                           95 'Amount'
018600020118
018700020118     O          e            detail         1
018800020118     **
018900020425     O                       name                30
019000020425     O                       timemp        z     37
019100020924     O                       timtyp        1     51
019200020924     O                       dept#         1     60
019300020612     O                       timrtype            67
019400020612     O                       timhrs        1     80
019500020612     O                       timamt        1     95
019600011003**
019700020425    Payroll Time Sheet Report
