000100950418     H DECEDIT('.') DATEDIT(*DMY/)
000200111013     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300950418     F** UPDATE MASTERS WITH SAVINGS AT MONTH END & LIST TO-DATE SAVINGS
000400010905     F** PROGRAMMER------ S.BRITTON & C.WILLIAMS
000500021219     Fsavmast   up   e           k disk    commit
000600021125     Fsavconf   if   e             disk
000700010914     Fliabtl07  if   e           k disk
000800011228     Fpayhisl11 if   e           k disk
000900010906     Fpaytranl03if   e           k disk
001000010905     ** Payroll PROTOTYPES
001100111013      /copy QCPYSRC,PAYPR
001200010905     **
001300010905     D                sds
001400010905     D  @pgm             *proc
001500111013     D  @user                358    367
001600010905     **
001700010921      ** Include data definition
001800111013      /copy QCPYSRC,savcopy#4
001900010921
002000010921      ** Include Input specs.
002100111013      /copy QCPYSRC,savcopy#2
002200010905
002300010905     **
002400010906     C                   exsr      get_Ded_rtn
002500010914     C                   exsr      get_lib_rtn
002600010905     **
002700010920     C                   eval      sav(cmth) = sav(pmth) + prd_ded
002800010920     C                   eval      wth(cmth) = wth(pmth) + withdr
002900021009     C                   eval      savlprno = prno
003000010905     **
003100010906     C                   update    savrec
003200010905     **
003300010921      ** Include Period calc sub routines
003400111013      /copy QCPYSRC,savcopy#
003500010920     ** Init routine
003600010920     C     *inzsr        begsr
003700010920     C     *entry        plist
003800010920     C                   parm                    prno
003900010920     **
004000010920     C                   move      prno          cmth
004100010920     **
004200010920     **  Get Previous Month
004300010920     C                   eval      pmth = cmth - 1
004400010920     C                   if        pmth = 0
004500010920     C                   eval      pmth = 12
004600010920     C                   endif
004700021125     ** read control file
004800021125     C                   read      sacfmt
004900021125     C                   eval      pay_type = sacdtype
005000021125     C                   eval      lib_type = sactype
005100010920     C                   endsr
