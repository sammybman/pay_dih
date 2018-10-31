000100950418     H DECEDIT('.') DATEDIT(*DMY/)
000200170227     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR') alwnull(*usrctl)
000300950418     F** YEAR END SAVINGS REPORT AND OUTPUT MASTERS FOR SAVINGS LETTERS
000400010914     F** PROGRAMMER------ S.BRITTON
000500010920     Fsavmast   iP   E           K DISK
000600021128     Fsavyear   if   e           k DISK    rename(savrec:savfmt)
000700021127     Fsavconf   if   e             disk
000800021127     Fsavconf2  if   e           k disk
000900010920     Fempmast   if   e           k disk
001000010920     Fliabtl07  if   e           k disk
001100011228     Fpayhisl11 if   e           k disk
001200010920     Fpaytranl03if   e           k disk
001300021127     Fqsysprt   o    f  132        printer oflind(*inof)
001400010917     **
001500950911     D                SDS
001600950911     D  @PGM             *PROC
001700131030     D  @USER                358    367
001800980330     **
001900021128     D  year#          s              4  0
002000010914     ** Payroll PROTOTYPES
002100131030      /copy qcpysrc,PAYPR
002200131030      /copy qcpysrc,Genpr
002300010921      ** Include data definition
002400131030      /copy qcpysrc,savcopy#4
002500010920
002600010921      ** Include Input specs.
002700131030      /copy qcpysrc,savcopy#2
002800010920     **
002900021128     C     year_key      klist
003000021128     C                   kfld                    savemp
003100021128     C                   kfld                    year#
003200010914     **  Get employee name
003300010920     c     savemp        chain     emprec
003400010920     c**                 eval      status =  get_emp_info(savemp:*omit:
003500010920     c**                                           *blanks:name#)
003600010921     C**                 eval      emp_name = name#
003700010921     C                   eval      emp_name = %trim(christ) + ' '
003800010920     C                                      + %trim(surnam)
003900010920     ** Select transactions Switch 1 & active employees
004000010920     ** (omitting Pensioners) or not Switch 1 & Inactive employees
004100010920     C                   if        run_type <> 'A'
004200010920     C                              and get_cat(status) <> 'A'
004300010920     C                             or run_type <> 'A' and status = 'O'
004400010920     C                             or run_type = 'A' and
004500010920     C                               get_cat(status) = 'A' and
004600010920     C                               status <> 'O'
004700021128     c     year_key      chain     savfmt
004800021128     C                   if        %found(savyear)
004900021128     **
005000021128     C                   eval      savpyear = savbfw
005100021128     C                   eval      prd_ded = empsav
005200021128     C                   eval      netsav = savbfw - withdr + intrst
005300021128     C                                    + empsav + comcon
005400031007     ** Final Report totals
005500031007     C                   eval      totbfw = totbfw + savpyear
005600031007     C                   eval      totint = totint + intrst
005700031007     C                   eval      totemp = totemp + prd_ded
005800031007     C                   eval      totwit = totwit + withdr
005900031007     C                   eval      totcom = totcom + comcon
006000031007     C                   eval      totsav = totsav + netsav
006100021128     C                   else
006200021128     **
006300021128      ** Include Year-end calcs
006400131030      /copy qcpysrc,savcopy#3
006500021128     C                   endif
006600010914     C                   except
006700010914     C                   endif
006800010920     **
006900010921      ** Include Period calc sub routines
007000131030      /copy qcpysrc,savcopy#
007100010920     ** Init routine
007200010920     C     *inzsr        begsr
007300010920     C     *entry        plist
007400010920     C                   parm                    prno
007500010920     C                   parm                    run_type
007600010920     **
007700010920     C                   move      prno          cmth
007800021128     C                   movel     prno          year#
007900010920     **
008000010920     **  Get Previous Month
008100010920     C                   eval      pmth = cmth - 1
008200010920     C                   if        pmth = 0
008300010920     C                   eval      pmth = 12
008400010920     C                   endif
008500010921     C                   eval      coname = get_coname
008600021127     ** read control file
008700021127     C                   read      sacfmt
008800021127     C                   eval      pay_type = sacdtype
008900021127     C                   eval      lib_type = sactype
009000021128     ** control previous file reference
009100021128     C                   if        cmth >= sacmth
009200021128     C                   eval      year# = year# + 1
009300021128     C                   endif
009400010920     C                   endsr
009500010917
009600021127     Oqsysprt   H    1P                     1 02
009700950418     O         OR    OF
009800950911     O                       @PGM                10
009900010921     O                       coname              76
010000950418     O          H    1P                     2
010100950418     O         OR    OF
010200950911     O                       @USER               10
010300010921     O                                           73 '=========================='
010400010921     O                                        +   0 '===='
010500010920     O          H    1P                     1
010600010920     O         OR    OF
010700950418     O                                           44 'EMPLOYEES  YEAR  END  SA'
010800950418     O                                           68 'VINGS  REPORT  FOR  THE '
010900950418     O                                           92 ' FINANCIAL  YEAR  ENDING'
011000950418     O                       UDATE         Y    102
011100950418     O                                          126 'PAGE'
011200950418     O                       PAGE               130
011300010920     O          H    1P                     2
011400010920     O         OR    OF
011500950418     O                                           44 '========================'
011600950418     O                                           68 '========================'
011700950418     O                                           92 '========================'
011800950418     O                                          102 '=========='
011900950418     O          H    1P                     1
012000950418     O         OR    OF
012100010921     O                                           49 'SAVINGS'
012200010921     O                                          100 'EMPLOYEE'
012300010921     O                                          114 'COMPANY'
012400950418     O          H    1P                     2
012500950418     O         OR    OF
012600010921     O                                            8 'EMP.'
012700010921     O                                           21 'N A M E'
012800010921     O                                           49 'BFWD'
012900010921     O                                           64 'WITHDRAWALS'
013000010921     O                                           80 'INTEREST'
013100010921     O                                          100 'CONTRIB'
013200010921     O                                          114 'CONTRIB'
013300010921     O                                          129 'BALANCE'
013400010921     O          e                           2
013500010920     O                       savemp        Z      7
013600010921     O                       emp_name            35
013700010921     O                       savpyear      j     50
013800010921     O                       withdr        j     65
013900010921     O                       intrst        1     80
014000010921     O                       prd_ded       1    100
014100010921     O                       comcon        1    114
014200010920     O                       netsav        j    130
014300010921     O          t    lr                  1  1
014400010921     O                       TOTBFW        j     50
014500010921     O                       TOTINT        1     80
014600010921     O                       TOTCOM        1    114
014700010921     O          t    lr                     2
014800010921     O                       TOTWIT        j     65
014900951004     O                       TOTEMP        1    100
015000010921     O                       TOTSAV        j    130
015100010921     O          t    lr                          01
015200950418     O                                           73 '** END OF REPORT **'
015300980330     **
