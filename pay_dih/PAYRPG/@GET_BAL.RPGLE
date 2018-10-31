000100980301     H DATEDIT(*ymd)
000200140818     H bnddir('DISDIR') dftactgrp(*no) actgrp(*new)
000300950429     H******  PROGRAMMER - SAMUEL BRITTON
000400021006     ** LIABILITY MASTER FILE
000500980301     Fliabml02  if   e           k disk
000600021006     ** LIABILITY MASTER FILE - PERIOD BALANCES
000700000321     Fliabm_bl01if   e           k disk
000800140818
000900140818      /copy qcpysrc,genpr
001000980301     **
001100980301     D la              s             11  2 dim(12)
001200021006     D prd_error       s               n
001300021006     D type            s              2  0
001400021006     D styp            s              3  0
001500021006     D emp             s              6  0
001600021006     D prno            s              6  0
001700140818     D wrk_prno        s              6  0
001800021006     D limit_prno      s              6  0
001900021006     D amt             s             11  2
002000140819     D mth             s              2  0
002100980301     **
002200980301     Ilifmt
002300980301     I              liamt01                     la(01)
002400980301     I              liamt02                     la(02)
002500980301     I              liamt03                     la(03)
002600980301     I              liamt04                     la(04)
002700980301     I              liamt05                     la(05)
002800980301     I              liamt06                     la(06)
002900980301     I              liamt07                     la(07)
003000980301     I              liamt08                     la(08)
003100980301     I              liamt09                     la(09)
003200980301     I              liamt10                     la(10)
003300980301     I              liamt11                     la(11)
003400980301     I              liamt12                     la(12)
003500980301     **
003600980324     **
003700980324     C     *entry        plist
003800021006     C                   parm                    type
003900021006     C                   parm                    emp
004000021006     C                   parm                    styp
004100021006     C                   parm                    prno
004200021006     C                   parm                    amt
004300021006     C                   parm                    prd_error
004400021006     C                   parm                    limit_prno
004500990213
004600980301     **
004700000321     C     det_key       klist
004800000321     C                   kfld                    emp
004900000321     C                   kfld                    type
005000000321     **
005100980301     C     trn_key       klist
005200000321     C                   kfld                    type
005300000321     C                   kfld                    styp
005400980301     C                   kfld                    emp
005500140818     C                   kfld                    wrk_prno
005600980301     **
005700140819          prd_error = *off;
005800140818
005900141110          //wrk_prno = get_new_prd(prno:1:'A');
006000141110          wrk_prno = prno;
006100140818
006200140819          chain  trn_key  lbfmt;
006300140818
006400140819          select;
006500140819             when  prno >= limit_prno;
006600140819
006700140819               mth = %int(%subst(%editc(prno:'X'):5:2));
006800140819               chain det_key lifmt;
006900140819               amt =  la(mth);
007000140819
007100140819             when   %found(liabm_bl01);
007200140819               amt =  lbamt;
007300140819             other;
007400140819               prd_error = *on;
007500140819
007600140819          endsl;
007700140819
007800140819          *inlr = *on;
007900140819          return;
008000140819
008100980301
