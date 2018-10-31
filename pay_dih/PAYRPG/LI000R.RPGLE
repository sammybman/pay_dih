000100051102     H DATEDIT(*DMY) dftactgrp(*no) bnddir('DISDIR')
000200980119     H* Programmer S.Britton
000300021006     F* Liability master file
000400980119     Fliabm     up   e           k disk
000500051102     F* LIABILITY TRANSACTION FILE
000600051102     Fliabtl06  if   e           k disk
000700000321     Fliabm_bal o    e             disk
000800021006     **
000900140819      /copy qcpysrc,genpr
001000980115     **
001100980117     D la              s             11  2 dim(12)
001200051102     D totamt          s             11  2
001300980115     **
001400980115     Ilifmt
001500980115     I              liamt01                     la(01)
001600980115     I              liamt02                     la(02)
001700980115     I              liamt03                     la(03)
001800980115     I              liamt04                     la(04)
001900980115     I              liamt05                     la(05)
002000980115     I              liamt06                     la(06)
002100980115     I              liamt07                     la(07)
002200980115     I              liamt08                     la(08)
002300980115     I              liamt09                     la(09)
002400980115     I              liamt10                     la(10)
002500980115     I              liamt11                     la(11)
002600980115     I              liamt12                     la(12)
002700951109     **
002800980115     C     *entry        plist
002900980117     C                   parm                    cur_prd           6 0
003000980117     **
003100980119     C                   move      cur_prd       mt_new            2 0
003200980119     C     mt_new        sub       1             mt_old            2 0
003300980331     C     mt_new        sub       7             mt_clr            2 0
003400980331     **
003500980119     C                   if        mt_old = 0
003600980119     C                   z-add     12            mt_old
003700980119     C                   endif
003800980331     **
003900980331     C                   if        mt_clr <= 0
004000980331     C                   add       12            mt_clr
004100980331     C                   endif
004200980117     **
004300051102      /free
004400051102
004500051102          totamt = *zeros;
004600051102          setll (litype:liemp:listyp:cur_prd) ltfmt;
004700051102          reade (litype:liemp:listyp:cur_prd) ltfmt;
004800051102
004900051102          // Select transactions for the NEW period
005000051102          dow  not(%eof);
005100051102
005200051102             totamt += ltamt;
005300051102
005400051102          reade (litype:liemp:listyp:cur_prd) ltfmt;
005500051102          enddo;
005600051102
005700051102          la(mt_new) = totamt + la(mt_old);
005800051102          totamt = *zeros;
005900051102          la(mt_clr) = *zeros;
006000051102
006100051102         update  lifmt;
006200051102      /end-free
006300000321     **
006400000321     ** Write start of period balances
006500000321     C                   z-add     liemp         lbemp
006600000321     C                   z-add     litype        lbtype
006700000321     C                   z-add     listyp        lbstyp
006800021006     C                   eval      lbprno = get_new_prd(cur_prd:1:'S')
006900011012     C                   z-add     la(mt_old)    lbamt
007000000321     C                   write     lbfmt
007100951109     **
