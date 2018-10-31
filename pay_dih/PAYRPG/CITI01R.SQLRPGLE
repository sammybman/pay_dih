000100110210     H DATEDIT(*DMY)       dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000200000707     F******* GET NO. OF RECORDS   --- S.BRITTON ******
000300990201     **
000400091010     D*-----------------------------------------
000500091010     D* Prototype for procedure: Main_pgm
000600091010     D*-----------------------------------------
000700091010     D  main_pgm       pr                  extpgm('CITI01R')
000800091010     D   bnk_type                     2  0
000900091010     D   bnk_rrn                      6  0
001000091010     D   origin                       3
001100091010
001200091010     D  main_pgm       pi
001300091010     D   bnk_type                     2  0
001400091010     D   bnk_rrn                      6  0
001500091010     D   origin                       3
001600091010
001700091010      /FREE
001800091010
001900091010         //using sql statement to check for record in file//
002000110121         //select;
002100121120
002300091010             exec sql
002400110121                select count(*) into :bnk_rrn from genpysum
002500091010                  where paydiv  = :bnk_type and payamt > 0
002600110121                       and paysts in ('A','V') and payorg = :origin;
002700091010
002800121120
004200091010         *inlr = *on;
004300091010         return;
004400990201
004500000630
