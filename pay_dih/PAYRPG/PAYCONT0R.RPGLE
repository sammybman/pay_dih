000100941121     H DATEDIT(*DMY)
000200111002     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000300111002     H/define profoundui
000400011017     Fpaycon    uf a e           k disk
000500011102     Fpaycont0d cf   e             workstn
000600111002     F/IF DEFINED(WEB)
000700111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000800111002     F/ENDIF
000900010301     **
001000111002      /copy QCPYSRC,genpr
001100021029     **
001200010815     D                sds
001300950711     D  @PGM             *PROC
001400010716     **
001500020611     D  err_his        c                   'Valid Values 0-999'
001600020611     D  err_ytd        c                   'Valid Values 2-999'
001700011102     D  err_auto       c                   'Valid Values are ''''Y'''' and -
001800011102     D                                     ''''N'''' '
001900021029     **
002000021029     D @date           s             10
002100041018     D arr             s             40    dim(1) CTDATA PERRCD(1)
002200021029     D err_chk         s              7    inz(*blank)
002300021029     **
002400021218     D                 ds                  inz
002500021029     D  pcdftytdDS                   10
002600021029     D  p_typeFx1                     2  0 overlay(pcdftytdDS)
002700021029     D  p_typeFx2                     2  0 overlay(pcdftytdDS:3)
002800021029     D  p_type3                       2  0 overlay(pcdftytdDS:5)
002900021029     D  p_type4                       2  0 overlay(pcdftytdDS:7)
003000021029     D  p_type5                       2  0 overlay(pcdftytdDS:9)
003100010228     **
003200010301     ** Initial Setup
003300111002      /copy QCPYSRC,aainit
003400950711     **
003500010301     *******************
003600010301     ** Program start **
003700010301     *******************
003800941123     **
003900011102     C                   read      pconfmt
004000941121     **
004100011102     C                   if        pcytd = 0
004200011102     C                   eval      *in99= '0'
004300011102     C                   eval      pchis#     = *zero
004400011102     C                   eval      pcytd#     = *zero
004500011102     C                   eval      pcautotax# = ' '
004600011102     C                   eval      pcautonis# = ' '
004700150818     C                   eval      pcnistaxo# = ' '
004800021029     C                   eval      pcdftytdDS = '0061636469'
004900041018     C                   eval      pctxonprd# = ' '
005000041025     C                   eval      pctxprate# = ' '
005100041206     C                   eval      pcallytd# = ' '
005200011102     C                   else
005300011102     C                   eval      *in99 = '1'
005400011102     C                   eval      pchis#    = pchis
005500011102     C                   eval      pcytd#    = pcytd
005600011102     C                   eval      pcautotax# = pcautotax
005700011102     C                   eval      pcautonis# = pcautonis
005800150818     C                   eval      pcnistaxo# = pcnistaxo
005900021029     C                   eval      pcdftytdDS = pcdftytd
006000041018     C                   eval      pctxonprd# = pctxonprd
006100041025     C                   eval      pctxprate# = pctxprate
006200041206     C                   eval      pcallytd# = pcallytd
006300011102     C                   endif
006400010301     ***********************
006500010301     ** Body of Program   **
006600010301     ***********************
006700010301     C                   dow       0 = 0
006800011018
006900011102     C                   exfmt     fmt1
007000011018     C                   eval      errmsg = *blanks
007100041206     C                   movea     '0000000'     *in(26)
007200010301     ** Cancel Entry
007300010301     C                   if        *in02
007400010301     C                   leave
007500010301     C                   endif
007600010301     ** End Program
007700010301     C                   if        *in03
007800010301     C                   eval      *inlr = *on
007900010419     C                   return
008000010301     C                   endif
008100011023
008200011102     C                   if        pchis# <0
008300011102     C                   eval      errmsg = err_his
008400011102     C                   Eval      *in26 = '1'
008500011102     C                   iter
008600011102     C                   endif
008700020611     C                   if        pcytd# <=1
008800011102     C                   eval      errmsg = err_ytd
008900011102     C                   Eval      *in27 = '1'
009000011102     C                   iter
009100011102     C                   endif
009200011102     C                   if        pcautotax# <> 'Y' and pcautotax# <> ' '
009300011102     C                             and pcautotax# <> 'N'
009400011102     C                   eval      errmsg = err_auto
009500011102     C                   Eval      *in28 = '1'
009600011102     C                   iter
009700011102     C                   endif
009800011102     C                   if        pcautonis# <> 'Y' and pcautonis# <> ' '
009900011102     C                             and pcautonis# <> 'N'
010000011102     C                   Eval      *in29 = '1'
010100011102     C                   eval      errmsg = err_auto
010200011102     C                   iter
010300011102     C                   endif
010400041018     C                   if        pctxonprd# <> 'Y' and pctxonprd# <> ' '
010500041018     C                             and pctxonprd# <> 'N'
010600041018     C                   Eval      *in30 = '1'
010700041018     C                   eval      errmsg = err_auto
010800041018     C                   iter
010900041018     C                   endif
011000041025     C                   if        pctxprate# <> 'Y' and pctxprate# <> ' '
011100041025     C                             and pctxprate# <> 'N'
011200041025     C                   Eval      *in31 = '1'
011300041025     C                   eval      errmsg = err_auto
011400041025     C                   iter
011500041025     C                   endif
011600041206     C                   if        pcallytd# <> 'Y' and pcallytd# <> ' '
011700041206     C                             and pcallytd# <> 'N'
011800041206     C                   Eval      *in32 = '1'
011900041206     C                   eval      errmsg = err_auto
012000041206     C                   iter
012100041206     C                   endif
012200150817     C                   if        pcnistaxo# <> 'Y' and pcnistaxo# <> ' '
012300150817     C                             and pcnistaxo# <> 'N'
012400150817     C                   Eval      *in34 = '1'
012500150817     C                   eval      errmsg = err_auto
012600150817     C                   iter
012700150817     C                   endif
012800011023     ** F15 to confirm
012900011023     c                   if        *in15
013000011023     c                   exsr      confirm_sr
013100011102     C                   eval      *inlr = '1'
013200011102     C                   return
013300011023     C                   endif
013400010301     **
013500010301     C                   enddo
013600041018
013700041018     *************************************
013800041018     ** Confirmation Routine
013900041018     *************************************
014000011023     c     confirm_sr    begsr
014100011102     C                   eval      pchis     = pchis#
014200011102     C                   eval      pcytd     = pcytd#
014300150817     C                   eval      pcautotax = pcautotax#
014400150817     C                   eval      pcnistaxo = pcnistaxo#
014500011102     C                   eval      pcautonis = pcautonis#
014600041018     C                   eval      pctxonprd = pctxonprd#
014700041025     C                   eval      pctxprate = pctxprate#
014800041206     C                   eval      pcallytd = pcallytd#
014900021029     C                   eval      pcdftytd = pcdftytdDS
015000011023     **
015100011023     C                   if        *in99
015200011023     C                   update    pconfmt
015300011023     C                   else
015400011023     C                   write     pconfmt
015500011023     C                   endif
015600011023     C                   endsr
015700010301**  Program Title
015800011023    Payroll Authority System Control
