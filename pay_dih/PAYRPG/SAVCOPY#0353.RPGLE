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
001900021125     **          Variable on years of service
002000021125     C                   if        sacintd = 'Y'
002100021125     C     int_type      setll     sac2fmt
002200021125     C     int_type      reade     sac2fmt
002300021125     C                   dow       not(%eof)
002400021125     C                   if        (*year - yremp) <= sac2upsrv and
002500021125     C                             (*year - yremp) >= sac2lwsrv
002600021125     **  do calcs
002700021125     C                   select
002800021125     C                   when      sacintbf = 'Y'
002900021125     C                   eval      intrst = (savpyear - withdr)
003000021125     C                                    * sac2per / 100
003100021125     C                   when      sacintbcf = 'Y'
003200021125     C                   eval      intrst = (savpyear - withdr + prd_ded)
003300021125     C                                    * sac2per / 100
003400021125     C                   endsl
003500021125     C                   leave
003600021125     C                   endif
003700021125     C     int_type      reade     sac2fmt
003800021125     C                   enddo
003900021125     C                   else
004000021125     **          on fixed percentage
004100021125     C                   if        sacint > *zeros
004200021125     C                   select
004300021125     C                   when      sacintbf = 'Y'
004400021125     C                   eval      intrst = (savpyear - withdr)
004500021125     C                                    * sacint / 100
004600021125     C                   when      sacintbcf = 'Y'
004700021125     C                   eval      intrst = (savpyear - withdr + prd_ded)
004800021125     C                                    * sacint / 100
004900021125     C                   endsl
005000021125     C                   endif
005100021125     C                   endif
005200021127
005300021127     C                   endif
005400970105     **
005500970105     ** Clear cents from interest field
005600980330     C                   eval      intrst = round_up(intrst:'U')
005700010920     **
005800010920
005900021126     ** Company Contribution Calcs.
006000021126     **          Variable on years of service
006100021126     C                   if        saccond = 'Y'
006200021127     C     con_type      setll     sac2fmt
006300021127     C     con_type      reade     sac2fmt
006400021126     C                   dow       not(%eof)
006500021126     C                   if        (*year - yremp) <= sac2upsrv and
006600021126     C                             (*year - yremp) >= sac2lwsrv
006700021126     **  do calcs
006800021126     C                   select
006900021126     C                   when      sacconbf = 'Y'
007000021126     C                   eval      comcon = (savpyear - withdr + prd_ded)
007100021126     C                                    * sac2per / 100
007200021126     ** calculation on difference
007300021126     C                   when      sacconef = 'Y'
007400021126     ** calculation on difference
007500021126     C                   if        sacconef = 'Y' and
007600021126     C                                  withdr > savpyear
007700021126     C                   eval      comcon = (savpyear - withdr + prd_ded)
007800021126     C                                    * sac2per / 100
007900021126     C                   else
008000021126     C                   eval      comcon = prd_ded
008100021126     C                                    * sac2per / 100
008200021126     C                   endif
008300021126     C                   endsl
008400021126     C                   leave
008500021126     C                   endif
008600021127     C     con_type      reade     sac2fmt
008700021126     C                   enddo
008800021126
008900021126     C                   else
009000021126     **          on fixed percentage
009100021126     C                   if        saccom > *zeros
009200021126     C                   select
009300021126     C                   when      sacconbf = 'Y'
009400021126     C                   eval      comcon = (savpyear - withdr + prd_ded)
009500021126     C                                    * saccom / 100
009600021126     C                   when      sacconef = 'Y'
009700021126     ** calculation on difference
009800021126     C                   if        sacconef = 'Y' and
009900021126     C                                  withdr > savpyear
010000021126     C                   eval      comcon = (savpyear - withdr + prd_ded)
010100021126     C                                    * saccom / 100
010200021126     C                   else
010300021126     C                   eval      comcon = prd_ded
010400021126     C                                    * saccom / 100
010500021126     C                   endif
010600021126     C                   endsl
010700021126     C                   endif
010800021126     C                   endif
010900010920     C                   endif
011000021126
011100021126     C                   if        comcon < 0
011200021126     C                   eval      comcon = *zeros
011300021126     C                   endif
011400970105     **
011500010920     C                   eval      netsav = savpyear - withdr + intrst
011600010920     C                                    + prd_ded + comcon
011700010920     ** Final Report totals
011800010920     C                   eval      totbfw = totbfw + savpyear
011900010920     C                   eval      totint = totint + intrst
012000010920     C                   eval      totemp = totemp + prd_ded
012100010920     C                   eval      totwit = totwit + withdr
012200010920     C                   eval      totcom = totcom + comcon
012300010920     C                   eval      totsav = totsav + netsav
012400010914     **
