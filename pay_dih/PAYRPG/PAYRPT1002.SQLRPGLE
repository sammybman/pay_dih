000100950726     H DECEDIT('.') DATEDIT(*DMY/)
000101170207     H dftactgrp(*no) actgrp(*new) bnddir('DISDIR':'PAYDIR')
000200170207      //* TIMESHEET REPORT
000300170207      //*                     C.WILLIAMS   & S.BRITTON
000400170207     F**paytimfl03if   e           k disk
000500020607     Fpayrtypl01if   e           K DISK
000600020503     Fpaycycd   if   e           K DISK
000700011003     Fqsysprt   O    f  132        printer oflind(*inof)
000800170207      // Payroll PROTOTYPES
000900170207      /copy QCPYSRC,genPR
001000170207      /copy QCPYSRC,PAYPR
001100170207      /copy qcpysrc,emppr
001200170207      //
001300020129     D opt             s              1
001400020118     D seq             s              1  0
001500020129     D dhours          s              5  2
001600020607     D dept_hrs        s              5  2
001700020129     D cycle           s              2
001800020129     D error           s               n
001900170207      //dept            s              3  0
002000020129     D dept#           s              3  0
002100020425     D name#           s             40
002200020118     D name            s             30
002300020118     D coname          s             30
002400170207     D run_type        s              1
002500020118     D @title          s             40
002600020118     D @date           s             10
002700020503     Dpayrun_factor    s              7  3
002800020607     Dpayroll_factor   s              7  3
002900020118     D arr             S             40    dim(1) ctdata perrcd(1)
003000170207      //
003100020425     D numcnst         c                   'EMPLOYEE NAME        &         NUMB-
003200020425     D                                     ER'
003300170207      //
003400170207     D paytimDS      e ds                  extname('PAYTIMFL03')
003401170207     D                                     qualified  inz
003500020129     D                sds
003600950930     D  @PGM             *PROC
003700170207
003701170207       // --------------------------------------------------
003702170207       // Prototype for procedure: closeFilePrc
003703170207       // --------------------------------------------------
003706170207
003707170207     D  @USER                358    367
003709170207       //
003710170207       DCL-S dept## PACKED(3:0) INZ(0);
003711170207       DCL-S timemp# PACKED(6:0) INZ(0);
003712170207       DCL-S timtyp# PACKED(2:0) INZ(0);
003713170207       DCL-S timhrs# PACKED(5:2) INZ(0);
003714170207       //
003715170207       DCL-S timrtype# CHAR(1) INZ(*BLANK);
003716170207       DCL-S timamt# PACKED(11:2) INZ(0);
003800170207      //
003900020118     C     *entry        plist
003901170207     C                   parm                    run_type
004200020118     C                   parm                    seq
004401170207     C                   parm                    opt
004402170207     C*                   parm                    dept#
004403170207     C*                   parm                    seq
004404170207     C*                   parm                    run_type
004405170207      /FREE
004500170207       exsr aainit;
004600170207       //*                 eval      coltxt = numcnst
004700170207
004800170207       except hedr;
004900170207       //
005000170207            open_file();
005100170207             dow not read_file();
005200170207       //setll *loval timfmt;
005300170207       //read timfmt;
005301170207       //   *INlr = %EOF;
005400170207       //
005401170207
005402170207       //  dow not(*inlr);
005500170207         if paytimDS.timtyp >=1 and paytimDS.timtyp <=2;
005600170207         // get payroll run type cycle
005700170207         chain run_type prtyfmt;
005800170207         chain prtycycode cycfmt;
005900170207         payrun_factor = cycyearf;
006000020607
006100170207        //    dow not(*inlr);
006102170207        timemp#  = paytimDS.timemp;
006200020129
006300170207           get_emp_info(timemp#:dept:*blanks:name#);
006400170207           get_dept_info(dept:error:dhours:cycle);
006500020129
006600170207           if dept# = *zeros or dept# <> *zeros
006700170207                 and dept# = dept;
006800020607
006900170207             if opt = 'Y';
007000170207               exsr get_dept_hrs;
007100170207             endif;
007200170207             //
007201170207             select;
007202170207               when  paytimDS.timtyp = 1;
007300170207             if opt = 'Y'
007301170207
007400170207                   and dept_hrs < paytimDS.timhrs
007500170207                   or opt <> 'Y';
007600020425
007700170207               name = name#;
007800170207               // Print Header on Overflow
007900170207               if *inof;
008000170207                 except hedr;
008100170207               endif;
008201170207                             timemp#  = paytimDS.timemp;
008205170207                            timtyp#  =paytimDS.timtyp;
008206170207                            dept##  = paytimDS.dept;
008207170207                            timrtype#  = paytimDS.timrtype;
008208170207                            timhrs#    = paytimDS.timhrs;
008209170207                           timamt#  = paytimDS.timamt;
008300170207               except detail;
008600170207             endif;
008601170207               when  paytimDS.timtyp = 2;
008602170207             if opt = 'Y'
008604170207                   and 50 < paytimDS.timhrs
008605170207                   or opt <> 'Y';
008606170207
008607170207               name = name#;
008608170207               // Print Header on Overflow
008609170207               if *inof;
008610170207                 except hedr;
008611170207               endif;
008612170207                             timemp#  = paytimDS.timemp;
008613170207                            timtyp#  =paytimDS.timtyp;
008614170207                            dept##  = paytimDS.dept;
008615170207                            timrtype#  = paytimDS.timrtype;
008616170207                            timhrs#    = paytimDS.timhrs;
008617170207                           timamt#  = paytimDS.timamt;
008618170207               except detail;
008619170207             endif;
008620170207             ENDSL;
008700170207           endif;
008701170207           endif;
008800170207        //   read timfmt;
008801170207        //   *INlr = %EOF;
008900170207         enddo;
009000170207           closeFilePrc();
009001170207           return;
009100170207       //    endif;
009200170207       //**********************************
009300170207       // Get Correct Department hours   **
009400170207       //**********************************
009500170207       begsr get_dept_hrs;
009600170207         // get payroll type cycle
009601170207
009800170207                get_payty_info('RD':paytimDS.timtyp:payty_struc);
009900170207
010000170207         if ptyclcode <> *blanks;
010100170207           chain ptyclcode cycfmt;
010200170207           payroll_factor = cycyearf;
010300170207         else;
010400170207           payroll_factor = payrun_factor;
010500170207         endif;
010600170207         // get Run-type cycle
010700170207         chain cycle cycfmt;
010800170207         dept_hrs = dhours * cycyearf /
010900170207             payroll_factor;
011000170207       endsr;
011100170207       //*************************************************
011200170207       // First initilisation routine for all programs  **
011300170207       //*************************************************
011400170207       Begsr aainit;
011500170207         //
011600170207         coname = get_coname;
011700170207         @date = get_sys_date;
011800170207         @title = arr(1);
011900170207         //
012000170207       Endsr;
012100170207       //
012101170207
012200020417     Oqsysprt   E            hedr           2 04
012300020417     O                       @pgm                10
012400011004     O                       coname              72
012500011004     O                       @date              130
012600170207      //
012700020417     O          E            hedr           2
012800020417     O                       @user               10
012900011004     O                       @title              80
013000011004     O                                          125 'Page:'
013100011004     O                       page          z    130
013200170207      //
013300020417     O          E            hedr           2
013400020425     O                       numcnst             37
013500020425     O                                           52 'Paytype'
013600020924     O                                           60 'Dept'
013700020425     O                                           67 'Run'
013800020425     O                                           80 'Hours'
013900020425     O                                           95 'Amount'
014000020118
014100020118     O          e            detail         1
014200170207      //
014300020425     O                       name                30
014400170207     O                       timemp#       z     37
014500170207     O                       timtyp#       1     51
014600170207     O                       dept##        1     60
014700170207     O                       timrtype#           67
014800170207     O                       timhrs#       1     80
014900170207     O                       timamt#       1     95
015000170207      //--------------------------------------
015100170207     P open_file       B
015200170207     D open_file       PI
015300170207
015400170207         Exec Sql
015500170207          DECLARE timeFILE CURSOR FOR SELECT * FROM paytimfl03
015600170207          where timrtype = :run_type
015700170207          order by dept,timemp,timtyp
015800170207          For Fetch Only;
015801170207          exec sql
015802170207        open timefile;
015900170207
016000170207     P open_file       E
016100170207      //**********************************
016200170207     P read_file       B
016300170207     D read_file       PI              n
016400170207
016500170207        EXEC SQL
016600170207         FETCH NEXT FROM TimeFILE INTO :paytimDS;
016700170207          if SQLSTT = '02000';
016800170207             return *on;
016900170207            else;
017000170207             return *off;
017100170207          endif;
017200170207     P read_file       E
017300170207
017301170207       // --------------------------------------------------
017302170207       // Procedure name: closeFilePrc
017303170207       // Purpose:
017304170207       // Returns:
017305170207       // --------------------------------------------------
017306170207       DCL-PROC closeFilePrc ;
017307170207         // Your local fields go here
017308170207
017309170207         // Your calculation code goes here
017310170207         exec sql
017311170207           close timefile;
017312170207
017313170207
017314170207         return ;
017315170207       END-PROC ;
017316170207**
017400020425    Payroll Time Sheet Report
