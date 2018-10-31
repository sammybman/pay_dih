000100150817     H DATEDIT(*DMY)   dftactgrp(*no) bnddir('DISDIR')
000200020927     Fxmascon   uf a e             disk
000300020927     Fpayrtypl01if   e           k disk
000400011105     Fxpaycont0dcf   e             workstn
000500181010     F/IF DEFINED(WEB)
000600181010     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000700181010     F/ENDIF
000800010301     **
000900141022      /copy qcpysrc,genpr
001000141022      /copy qcpysrc,emppr
001100010815     D                sds
001200950711     D  @PGM             *PROC
001300010716     **
001400020927     D  err_rtype      c                   'Pay Run type Entered Not Found'
001500011105
001600010716     D arr             S             40    DIM(1) CTDATA PERRCD(1)
001700050525     D notfound        S               n
001800010228     **
001900010301     ** Initial Setup
002000141022      /copy qcpysrc,aainit
002100020927     C     1             setll     xcfmt
002200020927     C                   read      xcfmt
002300950711     **
002400010301     *******************
002500010301     ** Program start **
002600010301     *******************
002700941123     **
002800011105
002900011105     C                   dow       0 = 0
003000011105     C                   exfmt     fmt1
003100011105     ** End Program
003200011105     C                   if        *in03
003300011105     C                   eval      *inlr = *on
003400011105     C                   return
003500011105     C                   endif
003600020927     ** Display List (run types)
003700011105     C                   if        *in01
003800020927     C                   call      'PAYENQ02R'
003900020927     C                   parm                    xcrtype
004000011105     C                   iter
004100011105     C                   endif
004200011105
004300011105
004400011105
004500020927     C     xcrtype       chain     prtyfmt
004600011105     C                   if        not %found
004700020927     C                   eval      errmsg = err_rtype
004800011105     C                   Eval      *in26 = '1'
004900020927     C                   eval      type_name = *blanks
005000011105     C                   iter
005100011105     C                   else
005200020927     C                   eval      type_name = prtydesc
005300011105     C                   endif
005400020927
005500020927     ** if anything changed
005600020927     C                   if        *in40
005700020927     C                   iter
005800020927     C                   endif
005900020927     **
006000020927     C                   if        not(%eof(xmascon))
006100020927     C                   update    xcfmt
006200020927     C                   else
006300020927     C                   write     xcfmt
006400020927     C                   endif
006500020927
006600020927     C                   eval      *inlr = *on
006700020927     C                   return
006800011105     C                   enddo
006900011105
007000010301**  Program Title
007100011106      Xmas Bonus Payroll Control
