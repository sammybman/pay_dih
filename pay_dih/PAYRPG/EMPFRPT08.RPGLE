000100950508     H DECEDIT('.') DATEDIT(*DMY/)
000200111013     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300040818     F** UPDATE SHARES MASTER
000400040818     F** PROGRAMMER------ S.BRITTON
000500060214     Fshrmast   uf   e           k disk    commit
000600040818     F* LIABILITY MASTER FILE
000700040818     Fliabm     if   e           k disk
000800021119     Fsecrosl01 if   e           k disk
000900040818
001000111013     D  main_pgm       pr                  extpgm('EMPFRPT08')
001100040818     D period                         6  0
001200040818
001300040818     D  main_pgm       pi
001400040818     D prno                           6  0
001500040818
001600950920     D                SDS
001700950920     D  @PGM             *PROC
001800111013     D  @USER                358    367
001900040818
002000040818     D liability     e ds                  extname(liabm)
002100040818
002200040818     D mth             s              2  0
002300040818     D la              s                   dim(12) like(liamt01) based(liptr)
002400040818     D liptr           s               *   inz(%addr(liamt01))
002500040818
002600040818
002700040818      /free
002800040818        mth = %int(%subst(%char(prno):5:2));
002900040818
003000040818        setll *loval shrmrec;
003100040818        read shrmrec;
003200040818
003300040818        dow not(%eof(shrmast));
003400040818          chain shrtyp secfmt;
003500040818          if %found;
003600040818           chain (secetyp:secsubt:emp) lifmt;
003700040818           if %found;
003800040818             if la(mth) = *zeros;
003900040818               delete  shrmrec;
004000040818             endif;
004100040818           endif;
004200040818          endif;
004300040818
004400040818        read shrmrec;
004500040818        enddo;
004600040818
004700040818        *inlr = *on;
004800040818        return;
004900040818      /end-free
