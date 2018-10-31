000100980321     *****  PROGRAMMER - SAMUEL BRITTON
000200000629     *****  Populating Citizen Bank transaction file
000300000630     **  Flag Parameter values --- *off - detail transactions
000400000630     **                        --- *on - Last detail transaction
000500000629     **                                  for batch
000600000629     Ptrans_up         b
000700000629     **-------------------------------------------------------------**
000800000629     Dtrans_up         pi
000900000630     Dciti_t#                              like(citi_t)
001000000629     Dciti_a#                              like(citi_a)
001100000630     Damount                         12  2
001200000630     Ddate_crd                        6  0
001300000630     Dcount                           4  0
001400000630     Dtrn_type                             like(winsta)
001500000630     Dlast_trn_flag                    n
001600000629     **-------------------------------------------------------------**
001700000630     Dciti_t         e ds                  extname(citibtch)
001800000630     Dciti_a         e ds                  extname(citidefn)
001900000629     Dcrdate           s               d   datfmt(*dmy)
002000000629     Dpydate           s               d   datfmt(*dmy)
002100000629     Dtrn_txt          c                   const('Salary Credit')
002200000707     Dtrn_txt2         c                   const('Salary paid')
002300000629     Dcentury          s              1
002400980321     **
002500980321     ***********************************************************************
002600000630     ****  PROCESSING BANK"S BATCH INFORMATION ***
002700980321     ***********************************************************************
002800980321     **
002900000629     **
003000000630     C                   move      citi_t#       citi_t
003100000630     C                   move      citi_a#       citi_a
003200000630     C     *dmy          move      date_crd      crdate
003300000630     C                   move      trn_type      winsta
003400000630
003500000630     ** write transaction information
003600000629     C                   select
003700000629     C                   when      trn_type = 'T'
003800000629     C                   move      '01'          winobk
003900000629     C                   z-add     *zeros        winobr
004000000629     C                   move      '01'          winbnk
004100000629
004200000629     ** get accounting info.
004300000629     C                   z-add     acmbrn        winbrn
004400000629     C                   move      acmccy        winccy
004500000629     C                   z-add     acmgln        wingln
004600000629     C                   z-add     acmccn        winccn
004700000629     C                   move      acmaty        winaty
004800000629     C                   z-add     acmcun        wincun
004900000629
005000000630     C                   z-add     acmacc        winacc
005100000629     **
005200000629     C                   extrct    crdate:*m     winvdm
005300000629     C                   extrct    crdate:*d     winvdd
005400000629     C                   extrct    crdate:*y     winvdy
005500000629     **
005600000630     C                   if        last_trn_flag
005700000630     C                   movel(p)  trn_txt2      wintds
005800000630     C                   z-add     amount        windra
005900000630     C                   z-add     *zeros        wincr1
006000000706     C                   move      'DR'          wintcd
006100000630     C                   else
006200000706     C                   move      'CR'          wintcd
006300000630     C                   movel(p)  trn_txt       wintds
006400000630     C                   z-add     amount        wincr1
006500000630     C                   endif
006600000630     **
006700000629     C                   z-add     697           winkbt
006800000629     C                   z-add     count         winksq
006900000706     C                   z-add     *zeros        winfer
007000000629     ** write batch date
007100000629     C                   extrct    crdate:*m     winbdm
007200000629     C                   extrct    crdate:*d     winbdd
007300000629     C                   extrct    crdate:*y     winbdy
007400000629     **
007500000629     C                   eval      century = (%subst(%char(*year):2:2))
007600000629     C                   move      century       winbdc
007700000629     C                   move      century       winvdc
007800000629
007900000630     ** writing header information
008000000629     C                   when      trn_type = 'B'
008100000630     **
008200000630     C                   move      'GYD'         winccy
008300000630     C                   eval      windra = amount
008400000630     C                   eval      wincr1 = amount
008500000630     **
008600000630     C                   move      *blanks       wintds
008700000630     C                   z-add     *zeros        winvdm
008800000630     C                   z-add     *zeros        winvdd
008900000630     C                   z-add     *zeros        winvdy
009000000630     C                   z-add     *zeros        winksq
009100000630     C                   move      'A'           winarf
009200000630     C                   z-add     count         winttr
009300000630     **
009400000629     C                   endsl
009500000630     **
009600000630     C                   move      citi_t        citi_t#
009700000630     C                   move      citi_a        citi_a#
009800980330     **
009900000629     Ptrans_up         e
