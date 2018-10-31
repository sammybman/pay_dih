000100110921     H DECEDIT('.') DATEDIT(*DMY/)
000200111115     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000300171024     H alwnull(*usrctl)
000400110630      //
000500110630      // CALCULATION OF % TO BE PAID FOR XMAS BONUS
000600110630      // S. BRITTON, c williams
000700011121
000800011121     Fmasall    ipe  e           k disk
000900021125     Fabssum    if   E           K dISK
001000011121     Fpayrresl01if   e           k disk    usropn
001100011121     Fxmascon   if   e           k disk    usropn
001200021125     Fxmsbonf   o    e             disk    usropn
001300021125     Fqsysprt   o    f  132        printer oflind(*inof)
001400111115
001500021125     D tab001          s              2  0 DIM(12) CTDATA PERRCD(12) ASCEND     NEW EMP. % ALLOC.
001600021125     D tabper          s              3  2 DIM(12) ALT(TAB001)
001700021125     D tab002          s              2  0 DIM(7) CTDATA PERRCD(7) ASCEND       NEW EMP + > 3DAYS
001800021125     D tabplu          s              3  1 DIM(7) ALT(TAB002)
001900110630      //
002000011123     D  plu            s              3  1
002100011112     D  tawol          s              4  1
002200101206     D  tlate          s              4  1
002300110630     D  deplate        s              4  1
002400021125     D  totdys         s              4  1
002500021125     D  coname         s             30
002600011112     D  diff1          s                   like(totdys)
002700011112     D  diff2          s                   like(totdys)
002800021125     D  diff3          s                   like(totdys)
002900021125     D  diff4          s                   like(totdys)
003000011112     D  persub         s                   like(totdys)
003100071031     D  fstper         s                   like(totdys)
003200071031     D  secper         s                   like(totdys)
003300021125     D  thrper         s                   like(totdys)
003400021125     D  forper         s                   like(totdys)
003500011115     D  netper         s                   like(totdys)
003600011112     D  per            s              2  0
003700011121     D  permth         s              4  0
003800011121     D  dname          s             40
003900011121     D  emply_date     s               d   datfmt(*eur)
004000011121     D  cur_date       s               d   datfmt(*iso)
004100011121     D  sixmth_emp     s               n   inz(*off)
004200011121     D  length         s             15  5
004300011121     D  ind_ptr        s               *   inz(%addr(*in))
004400041116     D  name           s             26
004500110630      //
004600021125     D cmd             c                   'CLRPFM XMSBONF'
004700110630      //
004800011121     D                 ds
004900011121     D  curdate                       8  0
005000011121     D  curyr                         4  0 overlay(curdate)
005100011121     D  curmth                        2  0 inz(11) overlay(curdate:5)
005200011123     D  curday                        2  0 inz(30) overlay(curdate:7)
005300110630      //
005400011121     D                 ds                  based(ind_ptr)
005500011121     D  new_employee           6      6n
005600110630      //
005700950921     D                SDS
005800950921     D  @PGM             *PROC
005900111115     D  @USER                358    367
006000110630      // Payroll PROTOTYPES
006100110630      /copy qcpysrc,paypr
006200110630      /copy qcpysrc,genPR
006300110630      //
006400950724     IEMPREC
006500950722     I                                          DEPT          L2
006600950724     IABSSREC
006700950620     I              TOTSIC                      SICK
006800950620     I              TOTIND                      INDACC
006900950620     I              TOTAWL                      AWL
007000950620     I              TOTAWO                      AWOL
007100950620     I              TOTJUR                      JURY
007200950620     I              TOTSUS                      SUSP
007300101206     I              TOTLAT                      LATE
007400110630      //
007500011121
007600110630      /FREE
007700110630       if %scan(status:prrelist) <> 0;
007800111115
007900110630         dname = get_dept_info(dept);
008000110630         // clear fields used in calcs
008100110630         exsr reset_sr;
008200111115
008300110630         dow 0 = 0;
008400110630           //* if is first page or overflow, print header
008500011112
008600110630           if not *in98 or *inof;
008700110630             EXCEPT HEDR;
008800110630           endif;
008900011112
009000110630           *in98 = '1';
009100111115
009200110630           // NEW EMPLOYEES ROUTINE
009300011112
009400110630           curyr = *year;
009500110630      /END-FREE
009600011121     C                   move      curdate       cur_date
009700011121     C                   move      emply         emply_date
009800011121     C     cur_date      subdur    emply_date    permth:*m
009900110630      /FREE
010000110630           select;
010100110630           when permth <= 0;
010200110630             leave;
010300110630           when permth <= 12;
010400110630             per = permth;
010500110630      /END-FREE
010600950724     C     PER           LOOKUP    TAB001        TABPER                   06
010700110630      /FREE
010800110630           Endsl;
010900011112
011000110630           //sum up absentism records
011100110630           CHAIN EMP ABSSUM;
011200110630           //
011300110630           IF %found;
011400110907             totdys = sick + indacc + awl + awol  //+ late
011500110630                 + jury + susp;
011600110630             tawol += awol;
011700110630             tlate += late;
011800110630           Endif;
011900110630           //
012000110630           if new_employee;
012100110630             sixmth_emp = per >= 6;
012200111115
012300110630             if totdys <= 3
012400110630                   and sixmth_emp;
012500110630      /END-FREE
012600011123     C     PER           LOOKUP    TAB002        TABPLU                   13
012700110630      /FREE
012800110630               plu = tabplu;
012900110630             endif;
013000110630             //
013100110630           endif;
013200011112
013300011112
013400110630           // DETERMINE TOTAL NUMBER OF DEDUCTION DAYS
013500110630           // DETERMINE WHETHER DEDUCTION DAYS EXCEED THREE OR ELEVEN DAYS
013600110630           // CALCULATE 3 %  FOR EACH DAY FROM 4 TO 10 & 5% FOR EACH DAY OVER 10
013700110630           // + 4 % PER DAY FOR EACH DAY AWOL.
013800110630           if totdys > xcfdays;
013900011112
014000110630             if totdys <= xcflvldays;
014100110630               diff1 = totdys - xcfdays;
014200110630             else;
014300110630               diff1 = xcflvldays - xcfdays;
014400110630               diff2 = totdys - xcflvldays;
014500110630             endif;
014600110630             //
014700110630           endif;
014800110630
014900110630           // NEW EMPLOYEES WITH LESS THAN 6 MONTHS SERVICE
015000110630           if new_employee and not sixmth_emp;
015100110630             // if absence <= 10 days use the amount
015200110630             if totdys <= xcflvldays;
015300110630               diff1 = TOTDYS;
015400110630             else;
015500110630               // save excess over 10 days
015600110630               diff2 = totdys - xcflvldays;
015700110630             endif;
015800110630           endif;
015900011112
016000110630           if status = 'G';
016100110630             if sick > 3;
016200110630               fstper = 0.5 * (sick - 3);
016300110630             endif;
016400110630             if (totdys - sick) > 3;
016500110630               fstper = 1.0 * ((totdys - sick) - 3)
016600110630                   + fstper;
016700110630             endif;
016800110630             //
016900110630           else;
017000110630             fstper = xcfpenalty * diff1;
017100071031        if (xcspenalty * diff2) > 999;
017200071031             secper = 999.0;
017300071031            else;
017400071031             secper = xcspenalty * diff2;
017500071031        endif;
017600110630             if tawol > 0;                                                    //AWOL %
017700110630               awolpr = tawol * 4;
017800110630             Endif;
017900111115
018000181010             if (tlate - xclatedays) > 0;                                             //late %
018100181010               latepr = (tlate - xclatedays) * xclatepen;
018200110630             Endif;
018300110630           endif;
018400110630           //
018500101206        if (fstper + secper + awolpr + latepr) > 999;
018600071101          persub = 999.0;
018700071101           else;
018800101206          persub = fstper + secper + awolpr + latepr;
018900071101        endif;
019000110630         //*****************************
019100110630         // Over xxx days absenteeism **
019200110630         //*****************************
019300110630           if totdys > xcfdays;
019400110630             // selection for New employee etc.
019500110630             select;
019600110630             when new_employee;
019700181010
019800011121     C                   MOVE      TABPER        NEWPER            3 0
019900110630             other;
020000110630               newper = 100;
020100110630             Endsl;
020200011121
020300110630             //**********************************
020400110630             // less than xxx days absenteeism **
020500110630             //**********************************
020600110630           else;
020700110630             // selection for New employee etc.
020800110630             select;
020900110630             when new_employee;
021000021125     C                   MOVE      TABPER        NEWPER
021100110630               newper = newper + plu;
021200110630               aaward = plu;
021300110630             other;
021400110630               newper = 100 + xcfaward;
021500110630               aaward = xcfaward;
021600110630             Endsl;
021700011121
021800110630           endif;
021900110630           // Calculate employee net percentage
022000110630           netper = newper - persub;
022100111115
022200110630           // if net percent < 0 it is a problem
022300011112
022400110630           *in12  = netper < 0;
022500011112
022600110630           // Ensure employee earning less than XX % gets XX% bonus
022700110630           if netper >= xcmpercent
022800110630                 or new_employee;
022900110630             percen = netper;
023000110630           else;
023100110630             percen = xcmpercent;
023200110630           endif;
023300111115
023400110630           // Summarize absent days for department summary
023500950722     C                   ADD       SICK          DEPSCK            5 1
023600950722     C                   ADD       INDACC        DEPIND            5 1
023700950722     C                   ADD       AWL           DEPAWL            5 1
023800950722     C                   ADD       AWOL          DEPAWO            5 1
023900950722     C                   ADD       JURY          DEPJRY            5 1
024000950722     C                   ADD       SUSP          DEPSUS            5 1
024100110705     C                   ADD       LATE          DEPLAT            4 1
024200011112
024300110630      // Set NEW employee flag for amount calcs
024400110705          //    deplat += late;
024500110630           if new_employee;
024600110630             nempflag = 'Y';
024700110630           else;
024800110630             nempflag = *blank;
024900110630           endif;
025000011112
025100110630
025200110630           // only write record to update file if pay percentage is > 0
025300110630           if percen > 0;
025400110630             WRITE XMSREC;
025500110630           endif;
025600011112
025700110630           // write detailed record to printer
025800110630           name = %trim(christ) + ' ' +
025900110630               %trim(surnam);
026000110630           Except;
026100110630           //
026200110630           leave;
026300110630         enddo;
026400110630         //
026500110630       endif;
026600110630       // FINAL TOTALS
026700950620     CL2                 ADD       DEPSCK        TOTSCK            5 1
026800950620     CL2                 ADD       DEPIND        TOTIND            5 1
026900950620     CL2                 ADD       DEPAWL        TOTAWL            5 1
027000950620     CL2                 ADD       DEPAWO        TOTAWO            5 1
027100950620     CL2                 ADD       DEPJRY        TOTJRY            5 1
027200950620     CL2                 ADD       DEPSUS        TOTSUS            5 1
027300110705     CL2                 ADD       DEPlat        TOTlat            5 1
027400011112     CL2                 eval      *in98 = '0'
027500110630      //*******************
027600110630      // Iniz subroutine **
027700110630      //*******************
027800110630       begsr *inzsr;
027900011121
028000110630         coname = get_coname();
028100110630         in *lock *dtaara;
028200031208     C     *dtaara       define    xmsseqchk     xmseq#            2 0
028300110630      // Check for correct run sequence of processed options
028400110630         if xmseq#  <> 1;
028500011121     C                   call      'ESCPMSG'
028600011121     C                   parm                    @pgm
028700011121     C                   parm      'DIH0131'     msgid             7
028800011121     C                   parm      'QGPL'        @lib             10
028900011121     C                   parm      'DIHMSGF'     @FILE            10
029000110630         endif;
029100110630         //
029200110630         xmseq# = 2;
029300110630         out xmseq#;
029400011121     C                   CALL      'QCMDEXC'
029500011121     C                   PARM      CMD           COMMAN           14
029600011121     C                   PARM      14            LENGTH
029700110630      //
029800110630         OPEN xmsBONF;
029900110630         OPEN xmascon;
030000110630         OPEN payrresl01;
030100110630         //
030200110630         read xmascon;
030300110630         chain xcrtype prrefmt;
030400110630       endsr;
030500110630       //*****************************
030600110630       // reset fields used in calcs.
030700110630       //*****************************
030800110630       begsr reset_sr;
030900110630         totdys = 0;
031000110630         persub = 0;
031100110630         aaward = 0;
031200110630         percen = 0;
031300110630         // clear all indicators starting from ind 06
031400011121     C                   move      '00000000'    *in(06)
031500110630         reset sixmth_emp;
031600011121     C                   Z-ADD     0             DIFF1
031700011121     C                   Z-ADD     0             DIFF2
031800011121     C                   Z-ADD     0             FSTPER
031900011121     C                   Z-ADD     0             SECPER
032000011121     C                   Z-ADD     0             NETPER
032100011121     C                   Z-ADD     0             TAWOL
032200101206     C                   Z-ADD     0             TLATE
032300011121     C                   Z-ADD     0             AWOLPR
032400111115     C                   Z-ADD     0             latepr
032500011121     C                   Z-ADD     0             PLU
032600110630       endsr;
032700110630       //***********************************
032800021125     Oqsysprt   E            HEDR           2 04
032900950921     O                       @PGM                10
033000021125     O                       coname              76
033100950724     O          E            HEDR        0  2
033200950921     O                       @USER               10
033300950724     O          E            HEDR           2
033400950620     O                                           46 'EMPLOYEES"'
033500950620     O                                           71 'XMAS BONUS % CALCULATION'
033600950620     O                       UDATE         Y     82
033700950620     O                                          127 'PAGE'
033800950620     O                       PAGE               132
033900950724     O          E            HEDR           2
034000011112     O                       DNAME               88
034100950724     O          E            HEDR           1
034200041116     O                                            9 'EMP.DPT.'
034300041116     O                                           42 'GRADE'
034400950620     O                                           61 'JOB DESCRIPTION'
034500110711     O*                                          98 '     DATE OF   SCK   I'
034600110711     O*                                         118 'ND   AWL  AWOL   JRY'
034700110711     O*                                         123 'SUSP'
034800110711     O*                                         131 'NET'
034900110711     O                                           98 '     DATE OF  LATE SCK'
035000110711     O                                          119 '  IND AWL  AWOL  JRY'
035100110711     O                                          124 'SUSP'
035200110711     O                                          131 'NET'
035300950724     O          E            HEDR           2
035400950620     O                                            8 'NO. NO.'
035500041116     O                                           30 'N    A    M    E'
035600110711     O                                           98 '      EMPLMNT       LVE'
035700110711     O                                          104 'ACC'
035800950620     O                                          118 'SRV'
035900950620     O                                          130 '%'
036000950724     O          E                           2
036100950620     O                       EMP           Z      6
036200950620     O                       DEPT          Z      9
036300041116     O                       name           B    36
036400950620     O                       CLASS          B    39
036500950620     O                       DESCRP         B    70
036600970105     O                       EMPLY          B    88 '  /  /    '
036700110711     O                       LATE          2B    93
036800110711     O                       SICK          2B    98
036900110711     O                       INDACC        2B   104
037000110711     O                       AWL           2B   109
037100110711     O                       AWOL          2B   114
037200110711     O                       JURY          2B   119
037300110711     O                       SUSP          2B   124
037400110705     O              N12      percen        1    132
037500110705     O                 N06 12                   132 'WHY?'
037600950620     O          T    L2                  2
037700111115     O                       DEPLAT        2B    93
037800111115     O                       DEPSCK        2B    98
037900111115     O*                      DEPIND        2B   104
038000111115     O                       DEPAWL        2B   109
038100111115     O                       DEPAWO        2B   115
038200111115     O                       DEPJRY        2B   119
038300111115     O                       DEPSUS        2B   125
038400110711     O*                      DEPLAT        2B   129
038500950620     O          T    LR                  1  3
038600950620     O                       TOTSCK        2     94
038700950620     O                       TOTIND        2    100
038800950620     O                       TOTAWL        2    106
038900950620     O                       TOTAWO        2    112
039000950620     O                       TOTJRY        2    118
039100950620     O                       TOTSUS        2    124
039200110705     O                       TOTlat        2    129
039300950620     O          T    LR                          01
039400950620     O                                           74 '** END OF REPORT **'
039500940516**
039600940516010090201703025040340504206050070590806709075100841109212100
039700940516** NEW EMPLOYEES WITH 6 MONTHS BUT LESS THAN 1 YEAR'S SERVICE---AWARD FOR LESS T
03980094051606035070400804509050100551106012065
