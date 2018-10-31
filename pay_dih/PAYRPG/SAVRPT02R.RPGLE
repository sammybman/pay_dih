000100950418     H DECEDIT('.') DATEDIT(*DMY/)
000200131030     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300950418     F** UPDATE MASTERS WITH SAVINGS AT MONTH END & LIST TO-DATE SAVINGS
000400010905     F** PROGRAMMER------ S.BRITTON & C.WILLIAMS
000500010920     Fsavmast   ip   e           k disk
000600021125     Fsavconf   if   e             disk
000700010920     Fliabtl07  if   e           k disk
000800011228     Fpayhisl11 if   e           k disk
000900010906     Fpaytranl03if   e           k disk
001000021125     Fqsysprt   o    f  132        printer oflind(*inof)
001100020228     **
001200131030      /copy qcpysrc,genpr
001300010905     ** Payroll PROTOTYPES
001400131030      /copy qcpysrc,PAYPR
001500010905     **
001600010905     D                sds
001700010905     D  @pgm             *proc
001800131030     D  @user                358    367
001900010905     **
002000010920     D totytd          s             11  2
002100020228     D prstrd          s              8  0
002200020228     D prendd          s              8  0
002300010921      ** Include data definition
002400131030      /copy qcpysrc,savcopy#4
002500010921
002600010921      ** Include Input specs.
002700131030      /copy qcpysrc,savcopy#2
002800010905     **
002900010905
003000010921     c                   callp     get_emp_info(savemp:*omit:*blanks:name#)
003100010921     C                   eval      emp_name = name#
003200010905     **
003300010906     C                   exsr      get_Ded_rtn
003400010920     C                   exsr      get_lib_rtn
003500010905     **
003600010920     C                   eval      withdr = withdr + wth(pmth)
003700010921     C                   eval      netsav = sav(pmth) + savpyear + prd_ded
003800010920     C                                      - withdr
003900010920     ** Final totals
004000010920     C                   eval      totytd = totytd + sav(pmth)
004100010920     C                   eval      totbfw = totbfw + savpyear
004200010921     C                   eval      totemp = totemp + prd_ded
004300010920     C                   eval      totwit = totwit + withdr
004400010921     C                   eval      totsav = totsav + netsav
004500010905     **
004600010921      ** Include Period calc sub routines
004700131030      /copy qcpysrc,savcopy#
004800010920     ** Init routine
004900010920     C     *inzsr        begsr
005000010920     C     *entry        plist
005100010920     C                   parm                    prno
005200010920     **
005300010920     C                   move      prno          cmth
005400010920     **
005500010920     **  Get Previous Month
005600010920     C                   eval      pmth = cmth - 1
005700010920     C                   if        pmth = 0
005800010920     C                   eval      pmth = 12
005900010920     C                   endif
006000020228
006100020228     C                   eval      coname = get_coname
006200020228     ** get period  dates
006300020228     C                   callp     get_prd_info('*rng':*blanks:prno:
006400020228     C                                          prstrd:prendd)
006500021125     ** read control file
006600021125     C                   read      sacfmt
006700021125     C                   eval      pay_type = sacdtype
006800021125     C                   eval      lib_type = sactype
006900020228
007000010920     C                   endsr
007100020228
007200021125     Oqsysprt   H    1P                     1 02
007300950418     O         OR    OF
007400950919     O                       @PGM                10
007500011002     O                       coname              70
007600950418     O          H    1P                     2
007700950418     O         OR    OF
007800950919     O                       @USER               10
007900950418     O          H    1P                  0  2
008000950418     O         OR    OF
008100971113     O                                          120 'Run Date: '
008200971113     O                       UDATE         Y   +  1
008300950418     O          H    1P                     1
008400950418     O         OR    OF
008500950418     O                                           69 'EMPLOYEES  SAVINGS  AS  '
008600950418     O                                           71 'AT'
008700020228     O                       prendd              81 '    /  /  '
008800950418     O                                          128 'PAGE'
008900950418     O                       PAGE               132
009000950418     O          H    1P                     2
009100950418     O         OR    OF
009200950418     O                                           69 '------------------------'
009300950418     O                                           81 '------------'
009400950418     O          H    1P                     1
009500950418     O         OR    OF
009600010925     O                                           52 'PREVIOUS YRS.'
009700010926     O                                           75 'CURRENT'
009800010926     O                                           96 'LAST MONTH''S'
009900010925     O                                          129 'NET'
010000950418     O          H    1P                     2
010100950418     O         OR    OF
010200010925     O                                            7 'EMP.'
010300010925     O                                           20 'NAME'
010400010925     O                                           50 'SAVINGS'
010500010926     O                                           77 'YRS. SAVINGS'
010600010926     O                                           94 'SAVINGS'
010700010926     O                                          112 'WITHDRAWAL'
010800010925     O                                          131 'SAVINGS'
010900010905     ** Detail Lines
011000010905     O          d                           2
011100010925     O                       savemp        z      7
011200010921     O                       emp_name            35
011300010920     O                       savpyear      a     51
011400010926     O                       sav(pmth)     a     75
011500010920     O                       prd_ded       ab    93
011600010920     O                       withdr        a    110
011700010921     O                       netsav        ab   130
011800010905     ** Total Lines
011900010905     O          t    lr                     3
012000010905     O                       totbfw        1     49
012100010926     O                                           51 '*'
012200010926     O                       totytd        1     73
012300010926     O                                           75 '*'
012400010926     O                       totemp        1     91
012500010926     O                                           93 '*'
012600010926     O                       totwit        1    109
012700010926     O                                          111 '*'
012800010926     O                       totsav        1    129
012900010926     O                                          131 '*'
013000010905     ** End of report
013100010905     O          t    lr                          01
013200950418     O                                           73 '** END OF REPORT **'
