000100141022     H DATEDIT(*DMY)   dftactgrp(*no) bnddir('DISDIR')
000200020927     Fxmasfcast uf a e           k disk
000300011105     Fempsmf    if   e           k disk
000400020927     Fxmascon   if   e             disk    usropn
000500020927     Fpayrdefl03if   e           K DISK
000600020927     Fxpaycont1dcf   e             workstn
000700181010     F/IF DEFINED(WEB)
000800181010     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000900181010     F/ENDIF
001000010301     **
001100141022      /copy qcpysrc,genpr
001200141022      /copy qcpysrc,emppr
001300010815     D                sds
001400950711     D  @PGM             *PROC
001500010716     **
001600011105     D  err_ptype      c                   'Pay Type Entered Not Found'
001700011105     D  err_class      c                   'Class Entered Not Found'
001800020927     D  err_type       c                   'Pay type entered not valid'
001900011105     D  err_amt        c                   'Default Values < *zero'
002000011105
002100010716     D arr             S             40    DIM(1) CTDATA PERRCD(1)
002200050525     D notfound        S               n
002300010228     **
002400010301     ** Initial Setup
002500141022      /copy qcpysrc,aainit
002600950711     **
002700010301     *******************
002800010301     ** Program start **
002900010301     *******************
003000941123     **
003100011105
003200011105     C     bon_key       klist
003300011105     C                   kfld                    xmptype#
003400011105     C                   kfld                    xmclass#
003500020927
003600020927     C     trn_key       klist
003700020927     C                   kfld                    xcrtype
003800020927     C                   kfld                    xmptype#
003900011105
004000011105     C                   dow       0 = 0
004100011105     C                   exfmt     fmt1
004200011105     ** End Program
004300011105     C                   if        *in03
004400011105     C                   eval      *inlr = *on
004500011105     C                   return
004600011105     C                   endif
004700011105     ** Display List
004800011105     C                   if        *in01
004900011105     C                   call      'PAYENQ01R'
005000011105     C                   parm                    xmptype#
005100011105     C                   iter
005200011105     C                   endif
005300011105
005400011105     C     xmclass#      chain     esmfmt
005500011105     C                   if        not %found
005600011105     C     'P':' '       xlate     xmclass#      xmclass#
005700011105     C     xmclass#      chain     esmfmt
005800011105     C                   endif
005900011105
006000011105     C                   if        not %found
006100011105     C                   eval      errmsg = err_class
006200011105     C                   Eval      *in28 = '1'
006300011105     C                   iter
006400011105     C                   endif
006500011105
006600050520      /free
006700050927          notfound = get_payty_info('RD0':xmptype#:payty_struc);
006800050520
006900050520      /end-free
007000050525     C                   if        notfound or not notfound
007100020927     C                             and ptyrtype <> 'E'
007200011105     C                   eval      errmsg = err_ptype
007300011105     C                   Eval      *in26 = '1'
007400011105     C                   iter
007500011105     C                   else
007600011105     C                   eval      type_name = ptydesc
007700011105     C                   endif
007800011105
007900020927     C     trn_key       chain     prdffmt
008000020927     **
008100020927     C                   if        not %found
008200020927     C                   eval      errmsg = err_type
008300020927     C                   iter
008400020927     C                   endif
008500020927
008600011105     C     bon_key       chain     xmsfmt
008700941121     **
008800011105     C                   if        not %found
008900011105     C                   eval      *in99      = '0'
009000011105     C                   eval      xmamount#  = 0
009100011102     C                   else
009200011105     C                   eval      *in99     = '1'
009300011105     C                   eval      xmptype#  = xmptype
009400011105     C                   eval      xmclass#  = xmclass
009500011105     C                   eval      xmamount# = xmamount
009600011102     C                   endif
009700011105     c                   leave
009800011105     C                   enddo
009900010301     ***********************
010000010301     ** Body of Program   **
010100010301     ***********************
010200010301     C                   dow       0 = 0
010300011018
010400011105     C                   exfmt     fmt2
010500011018     C                   eval      errmsg = *blanks
010600011102     C                   movea     '0000'        *in(26)
010700010301     ** Cancel Entry
010800010301     C                   if        *in02
010900010301     C                   leave
011000010301     C                   endif
011100010301     ** End Program
011200010301     C                   if        *in03
011300010301     C                   eval      *inlr = *on
011400010419     C                   return
011500010301     C                   endif
011600011023
011700011105     C                   if        xmamount# < 0
011800011102     C                   Eval      *in29 = '1'
011900011105     C                   eval      errmsg = err_amt
012000011102     C                   iter
012100011102     C                   endif
012200011023     ** F15 to confirm
012300011023     c                   if        *in15
012400011023     c                   exsr      confirm_sr
012500011106     C                   leave
012600011023     C                   endif
012700010301     **
012800010301     C                   enddo
012900011105
013000011105     ***********************************
013100011105     ** confirm                       **
013200011105     ***********************************
013300011023     c     confirm_sr    begsr
013400011105
013500011105     C                   eval      xmptype   = xmptype#
013600011105     C                   eval      xmclass   = xmclass#
013700011105     C                   eval      xmamount  = xmamount#
013800011023     **
013900011023     C                   if        *in99
014000011105     C                   update    xmsfmt
014100011023     C                   else
014200011105     C                   write     xmsfmt
014300011023     C                   endif
014400011105     **
014500011023     C                   endsr
014600020927     *********************************
014700020927     ***  initial pgm subroutine   ***
014800020927     *********************************
014900020927     C     *inzsr        begsr
015000020927     ** Get system date / company name
015100020927
015200020927     C                   open      Xmascon
015300020927     C                   read      xmascon
015400020927
015500020927     C                   endsr
015600010301**  Program Title
015700020927      Xmas Bonus Forecast Schedule
