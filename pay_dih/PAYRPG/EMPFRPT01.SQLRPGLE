000100941121     H DATEDIT(*DMY)
000200111013     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300020206     F* LOAN MASTER FILE
000400051016     Fematf     up   e           k disk    commit
000500021219     Fliabml04  uf   e           k disk    commit
000600021219     Fliabm_bl01uf   e           k disk    commit
000700020206     F* LOAN TRANSACTION FILE
000800021219     Fliabt     o    e           k disk    commit
000900020206     **
001000111013      /copy QCPYSRC,PAYPR
001100111013      /copy QCPYSRC,genpr
001200111013      /copy QCPYSRC,glpr
001300051016
001400051016     d                sds
001500051016     d  @pgm             *proc
001600051016     d  @user                254    263
001700010404     **
001800051016     D escpmsg         pr                  extpgm('ESCPMSG')
001900051016     D  pgm                          10    const
002000051016     D  errid                         7    const
002100051016
002200010404     Dgl_accumd        ds                  occurs(100) inz
002300010404     D gl_prd                         6  0
002400010404     D gl_account                    25
002500010404     D gl_desc                       30
002600010404     D gl_total                      13  2
002700030703     D gl_refno                      15
002800980115     **
002900020206     D txt             c                   'INTEREST FOR'
003000980117     D la              s             11  2 dim(12)
003100020206     D gl_out          s             25                                         TABLE OF MONTHS
003200020206     D tint            s                   like(ltamt)                          TABLE OF MONTHS
003300020206     D month           s              9
003400010404     D xcnt            s              4  0 inz(0)                               TOTAL AMOUNT
003500020206     D cur_prd         s              6  0                                      TOTAL AMOUNT
003600020206     D prno            s              6  0                                      TOTAL AMOUNT
003700020206     D prstrd          s              8  0
003800020206     D prendd          s              8  0
003900111013     D esysinacct      S             25
004000031001     D errmsg          S             75
004100980115     **
004200980115     Ilifmt
004300980115     I              liamt01                     la(01)
004400980115     I              liamt02                     la(02)
004500980115     I              liamt03                     la(03)
004600980115     I              liamt04                     la(04)
004700980115     I              liamt05                     la(05)
004800980115     I              liamt06                     la(06)
004900980115     I              liamt07                     la(07)
005000980115     I              liamt08                     la(08)
005100980115     I              liamt09                     la(09)
005200980115     I              liamt10                     la(10)
005300980115     I              liamt11                     la(11)
005400980115     I              liamt12                     la(12)
005500951104     **
005600021006     **
005700021006     C     trn_key       klist
005800021006     C                   kfld                    emseltyp
005900021006     C                   kfld                    listyp
006000021006     C                   kfld                    liemp
006100021006     C                   kfld                    cur_prd
006200111013      /free
006300111013        exec sql
006400111013           select esysinacct into :esysinacct  from empcon;
006500111013      /end-free
006600021015     C                   if        emintper > *zeros
006700021015
006800021015     C                   if        emintxc = *zeros  or
006900021015     C                             emintxc = cur_prd
007000080721     0*
007100020206     C     emseltyp      setll     lifmt
007200020206     C     emseltyp      reade     lifmt
007300020206
007400020206     C                   dow       not(%eof)
007500020206     **
007600980117     C                   if        la(mt) > 0
007700980117     C                   move      'A'           ltsts
007800980117     C                   move      'SYS'         ltorg
007900980117     C                   z-add     liemp         ltemp
008000980117     C                   z-add     litype        lttype
008100980117     C                   z-add     listyp        ltstyp
008200020206     C                   eval      lttext = txt + ' ' + month + ' '
008300020206     C                                 + %subst(%char(cur_prd):1:4)
008400020206     C                   z-add     cur_prd       ltprno
008500980117     C                   z-add     prendd        ltdate
008600951109     **
008700020206     C                   eval      ltamt = la(mt) * emintper / 100
008800021015     C                                     / 12 * emmth
008900990303     *** ROUNDING CENTS TO DOLLARS
009000990303     C                   eval      ltamt = round_up(ltamt:'U')
009100980117     C                   add       ltamt         la(mt)
009200990316     C                   add       ltamt         la(mt_nxt)
009300980117     C                   sub       ltamt         tint
009400980117     **
009500981016     C                   write     ltfmt
009600980117     **
009700980117     C                   if        licode <> 'N' and lifix = 0
009800980117     C                   exsr      @calfix
009900980117     C                   endif
010000981016     C                   update    lifmt
010100021006
010200021006     C     trn_key       chain     lbfmt
010300021006     C                   if        %found
010400021006     C                   eval      lbamt = la(mt)
010500021006     C                   update    lbfmt
010600021006     C                   endif
010700980115     C                   endif
010800020206
010900020206     C     emseltyp      reade     lifmt
011000020206     C                   enddo
011100951109     **
011200020206     C                   exsr      Gl_write
011300021015     C                   eval      emintlc = cur_prd
011400021015     C                   eval      emintxc = get_new_prd(cur_prd:emmth:'A')
011500021015     C                   update    emfmt
011600021015     C                   endif
011700020206     C                   endif
011800981016
011900981016     C     Gl_write      begsr
012000981016
012100010313     ** for Imas
012200111013     C                   eval      gl_out  = esysinacct
012300010404     **
012400010404     C                   exsr      accum_gl_tot
012500010313     **
012600010404     C                   z-add     tint          gl_total
012700010404     C                   movel(p)  lttext        gl_desc
012800010315     **
012900021015     ** for GL. system
013000020206     C                   eval      gl_out = emacct
013100010404     **
013200010404     C                   exsr      accum_gl_tot
013300010315     **
013400010404     C                   z-sub     tint          gl_total
013500010313     **
013600981016
013700010404     ** call pgm to write to g/l files
013800031001      /free
013900051016       errmsg = *blanks;
014000040714       %occur(gl_accumd) = 1;
014100031001       write_to_gl(%addr(gl_accumd):xcnt:'EMT':errmsg);
014200051016
014300051016       if errmsg <> *blanks;
014400051016           escpmsg(@pgm:'DIH0149');
014500051016       endif;
014600080513
014700080513       // clear summary field
014800080721       errmsg = *blanks;
014900080513       tint = *zeros;
015000080721       clear  *all gl_accumd;
015100080721       reset xcnt;
015200051016
015300031001      /end-free
015400031001
015500981016     C                   endsr
015600980115     **
015700980117     C     @calfix       begsr
015800980117     C     la(mt)        ifle      1000.00
015900980117     C                   z-add     la(mt)        lifix
016000980117     C                   else
016100980117     C     la(mt)        ifgt      1000.00
016200980117     C     la(mt)        andlt     3000.00
016300980117     C     la(mt)        mult      .3            lifix
016400980117     C                   else
016500980117     C     la(mt)        ifge      3000.00
016600980117     C     la(mt)        andlt     7000.00
016700980117     C     la(mt)        mult      .15           lifix
016800980117     C                   else
016900980117     C     la(mt)        ifge      7000.00
017000980117     C     la(mt)        mult      .10           lifix
017100980117     C                   endif
017200980117     C                   endif
017300980117     C                   endif
017400980117     C                   endif
017500980117     C                   endsr
017600010404     ***********************************************************************
017700010404      * Search for gl.code in array . if found set occurance level and add
017800010404      * total ,if not found add new array record and Data Struct. total
017900010404     ***********************************************************************
018000010404     C     accum_gl_tot  BEGSR
018100010404     **
018200010404     C                   add       1             xcnt
018300010404     C     xcnt          occur     gl_accumd
018400010404     C                   eval      gl_account = gl_out
018500020206     C                   eval      gl_prd = cur_prd
018600010404     **
018700010404
018800010404     C                   ENDSR
018900020206     ********************************
019000020206     ** iniz
019100020206     ********************************
019200020206     C     *inzsr        begsr
019300020206     C     *entry        plist
019400020206     C                   parm                    cur_prd
019500020206     **
019600020206     ***********************************************************************
019700020206     C                   eval      prno = get_new_prd(cur_prd:1:'A')
019800020206     C
019900020206     ** get period  dates
020000020206     C                   callp     get_prd_info('*rng':*blanks:cur_prd:
020100020206     C                                          prstrd:prendd)
020200020206     C                   move      prno          mt_nxt            2 0
020300020206     C                   move      cur_prd       mt                2 0
020400020206     C                   eval      month = get_month(mt)
020500020206     C                   endsr
020600980115     **
