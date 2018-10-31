000100950617     H
000200031006     Fsavyear   o    e             disk    commit  rename(savrec:srec#)
000300010928     Dyear             s              4  0
000400031006     Dbfwd             s             13  2
000500010928     Dccon             s             11  2
000600010928     Decon             s             11  2
000700010928     Dwith             s             11  2
000800010928     Dintr             s             11  2
000900021128     Dtax              s             11  2
001000010928     **
001100010928     C     *entry        plist
001200010928     C                   parm                    emp
001300010928     C                   parm                    year
001400010928     C                   parm                    bfwd
001500010928     C                   parm                    ccon
001600010928     C                   parm                    econ
001700010928     C                   parm                    with
001800010928     C                   parm                    intr
001900021128     C                   parm                    tax
002000010928     **
002100010928     C                   eval      syear = year
002200010928     C                   eval      savbfw = bfwd
002300010928     C                   eval      withdr = with
002400010928     C                   eval      intrst = intr
002500010928     C                   eval      empsav = econ
002600010928     C                   eval      comcon = ccon
002700021128     C                   eval      savtax = tax
002800031006     C                   write     srec#
002900010928     **
003000010928     C                   return
