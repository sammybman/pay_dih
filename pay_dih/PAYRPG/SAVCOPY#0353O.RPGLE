000100010917     **
000200010920     C                   eval      comcon = *zeros
000300010920     C                   eval      intrst = *zeros
000400010920     C                   eval      netsav = *zeros
000500010914     **
000600010920     C                   MOVE      EMPLY         YREMP
000700010920
000800010920     C                   exsr      get_Ded_rtn
000900010920     C                   exsr      get_lib_rtn
001000010920
001100010920     C                   eval      withdr = withdr + wth(pmth)
001200010920     C                   eval      prd_Ded = prd_ded + sav(pmth)
001300011005
001400011005     ** Do calcs for active employees ONLY
001500011005     C                   if        run_type = 'A'
001600010920     **
001700010920     C                   if        withdr < savpyear
001800021125     ** Interest calcs.
001900010920     C                   if        (*year - yremp) < 10
002000010920     C
002100010920     C                   eval      intrst = (savpyear - withdr) * 0.04
002200010920     C                   else
002300010920     C                   eval      intrst = (savpyear - withdr) * 0.05
002400010920     C                   endif
002500010920     C                   endif
002600970105     **
002700970105     ** Clear cents from interest field
002800980330     C                   eval      intrst = round_up(intrst:'U')
002900010920     **
003000010920     **  Calculation of company contributions based on withdrawals during
003100010920     ** the current financial year (98/02/10). Withdrawals are subtracted
003200010920     ** from the previous year end balance and added to the current year
003300010920     ** savings. This total , if positive , is then doubled and
003400010920     ** reported as the companys' contribution for the year.
003500010920
003600010920     C                   if        withdr > savpyear
003700010920     C                   eval      comcon =( prd_ded - withdr + savpyear)
003800010920     C                                      * 2
003900010920     C                   if        comcon < 0
004000010920     C                   eval      comcon = *zeros
004100010920     C                   endif
004200010920     C                   else
004300010920     C                   eval      comcon = prd_ded * 2
004400010920     C                   endif
004500010920
004600010920     C                   endif
004700970105     **
004800010920     C                   eval      netsav = savpyear - withdr + intrst
004900010920     C                                    + prd_ded + comcon
005000010920     ** Final Report totals
005100010920     C                   eval      totbfw = totbfw + savpyear
005200010920     C                   eval      totint = totint + intrst
005300010920     C                   eval      totemp = totemp + prd_ded
005400010920     C                   eval      totwit = totwit + withdr
005500010920     C                   eval      totcom = totcom + comcon
005600010920     C                   eval      totsav = totsav + netsav
005700010914     **
