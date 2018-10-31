000100991204     H
000200941003     H******  PROGRAMMER - S. BRITTON
000300000619     H******  First Coded - 00/06/19
000400000707     Fbnkpysuml2uf   e           k disk
000500991208     **
000600091012     **
000700091012     D*-----------------------------------------
000800091012     D* Prototype for procedure: Main_pgm
000900091012     D*-----------------------------------------
001000091012     D  main_pgm       pr                  extpgm('CITI03R')
001100091012     D   bnk_type                     2  0
001200091012     D   ol_bnk_acc                  30
001300091012     D   nw_bnk_acc                  30
001400091012     D   origin                       3
001500091012
001600091012     D  main_pgm       pi
001700091012     D   bnk_type                     2  0
001800091012     D   ol_bnk_acc                  30
001900091012     D   nw_bnk_acc                  30
002000091012     D   origin                       3
002100091012
002200091012      /free
002300091012          select;
002400091012            when origin = 'PAY';
002500091012               exec sql
002600091012                 update bnkpysum set payacc = :nw_bnk_acc
002700091012                   where paysts = 'A' and paydiv = :bnk_type
002800091012                    and payacc = :ol_bnk_acc;
002900091012            when origin = 'SHR';
003000091012               exec sql
003100091012                 update shrpysum set sbkacc = :nw_bnk_acc
003200091012                   where sbksts = 'A' and sbkdiv = :bnk_type
003300091012                    and sbkacc = :ol_bnk_acc;
003400091012          endsl;
003500000710     **
003600000710     c     get_key       klist
003700000710     c                   kfld                    bnk_type
003800000710     c                   kfld                    ol_bnk_acc
003900000707
004000000707     **
004100000707     ** get accounting info.
004200000710     C     get_key       chain     payfmt                             lr
004300000707     **
004400000710     C                   dow       not(*inlr)
004500000707     **
004600000710     C                   move      nw_bnk_acc    payacc
004700000710     C                   update    payfmt
004800000710     C     get_key       reade     payfmt                                 lr
004900000710     C                   enddo
005000000619     **
005100000619     ** RETURN
005200000619     C                   return
