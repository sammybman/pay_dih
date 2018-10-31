000100040724     H DATEDIT(*DMY) bnddir('DISDIR') dftactgrp(*NO) actgrp('QILE')
000200960315     ** PROGRAMMER  S.BRITTON
000300040724     Fempcon    if   e             disk    usropn
000400040724     Fliabm     uf   e           K disk    usropn
000500040724     Fliabt     uf   e           k disk    usropn
000600040724
000700040724      /copy genpr
000800011109     **
000900960506     D                sds
001000960506     D  @pgm             *proc
001100011109     **
001200040724     Dlistruc        e ds                  extname(liabm)
001300040724     **
001400040724     d lptr            s               *   inz(%addr(liamt01))
001500040724     d la              s                   dim(12) like(liamt01) based(lptr)
001600970219     Dstopiso          s               d   datfmt(*iso)
001700040724     Dprno             s              6  0
001800040724     D#date            s              8  0
001900040724     Dstrdate          s              8  0
002000040724     Denddate          s              8  0
002100040724
002200040724     DMAXYEARS         c                   999
002300960315
002400040724     **
002500040724      /free
002600040724
002700040724        open(e) empcon;
002800040724        setll 1 esyfmt;
002900040724        read esyfmt;
003000040724        close empcon;
003100040724
003200040724         if  esyshis = MAXYEARS;
003300040724           *inlr = *on;
003400040724           return;
003500040724         endif;
003600040724
003700040724         get_sys_date(#date);
003800040724         get_prd_info('*prd':*blanks:prno:#date);
003900040724         get_prd_info('*rng':*blanks:prno:strdate:enddate);
004000040724         stopiso  = %date(enddate) - %years(esyshis);
004100040724
004200040724           open(e) liabm;
004300040724
004400040724           read   lifmt;
004500040724         // Process File until EOF
004600040724           dow  not(%eof(liabm));
004700040724             if   %xfoot(la) = *zeros;
004800040724
004900040724                delete  lifmt;
005000040724
005100040724             endif;
005200040724             read  lifmt;
005300040724           enddo;
005400040724
005500040724           close liabm;
005600040724
005700040724           open(e) liabt;
005800040724
005900040724           read  liabt;
006000040724         // Process File until EOF
006100040724           dow  not(%eof(liabt));
006200040724             if   %date(ltdate) <= stopiso;
006300040724
006400040724                delete  ltfmt;
006500040724
006600040724             endif;
006700040724             read  ltfmt;
006800040724           enddo;
006900040724
007000040724           close liabt;
007100040724
007200040724         *inlr = *on;
007300040724         return;
