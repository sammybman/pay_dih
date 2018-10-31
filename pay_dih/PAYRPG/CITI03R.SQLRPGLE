000100991204     H
000200941003     H******  PROGRAMMER - S. BRITTON
000300000619     H******  First Coded - 00/06/19
000400091012     **
000500091012     D*-----------------------------------------
000600091012     D* Prototype for procedure: Main_pgm
000700091012     D*-----------------------------------------
000800091012     D  main_pgm       pr                  extpgm('CITI03R')
000900091012     D   bnk_type                     2  0
001000091012     D   ol_bnk_acc                  30
001100091012     D   nw_bnk_acc                  30
001200091012     D   origin                       3
001300091012
001400091012     D  main_pgm       pi
001500091012     D   bnk_type                     2  0
001600091012     D   ol_bnk_acc                  30
001700091012     D   nw_bnk_acc                  30
001800091012     D   origin                       3
001900091012
002000091012      /free
002100091012          select;
002200091012
002300091012            when origin = 'PAY';
002400091012               exec sql
002500091012                 update bnkpysum set payacc = :nw_bnk_acc
002600091012                   where paysts = 'A' and paydiv = :bnk_type
002700091016                    and payacc = :ol_bnk_acc with NC;
002800091012
002900091012            when origin = 'SHR';
003000091012               exec sql
003100091012                 update shrpysum set sbkacc = :nw_bnk_acc
003200091012                   where sbksts = 'A' and sbkdiv = :bnk_type
003300091016                    and sbkacc = :ol_bnk_acc with NC;
003400100125            when origin = 'CRD';
003500100125               exec sql
003600100125                 update cuspysum set cbkacc = :nw_bnk_acc
003700100125                   where cbksts = 'A' and cbkdiv = :bnk_type
003800100125                    and cbkacc = :ol_bnk_acc with NC;
003900091012          endsl;
004000000707
004100091012          *inlr  = *on;
004200091012          return;
