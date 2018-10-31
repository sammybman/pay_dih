000100010905     Fbtypel02  If   E           k DISK
000200950907     D*
000300010905     D  den_str        DS                  occurs(40)
000400010905     Dden_desc                       30
000500010905     Dden_amt                         7  0
000600010905     Dden_value                      13  2
000700010905
000800010905     Damount           s             13  2
000900010905     Dcnt              s              4  0
001000950804     **
001100010905     C     *entry        plist
001200010905     C                   parm                    amount
001300010905     C                   parm                    den_str
001400010905     C                   parm                    cnt
001500010905     c
001600010905     C     *hival        setll     vbtfmt
001700010905     C                   read      vbtfmt
001800010905     c                   dow       not (%eof)
001900010905     c
002000010905     C                   if        amount >= vbtval
002100010905     C                   eval      cnt = cnt + 1
002200010905     C     cnt           occur     den_str
002300010905     C                   eval      den_amt = amount /vbtval
002400010905     C                   eval      den_value = den_amt * vbtval
002500010905     C                   eval      amount = amount - den_value
002600010905     C                   eval      den_desc = vbtdesc
002700010905     C                   endif
002800010905
002900010905     C                   read      vbtfmt
003000010905     C                   enddo
003100970107     **
003200010905     C                   return
003300970107     **
