000100980210     H DATEDIT(*ymd)
000200111013     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300981028     F* ********************************************************
000400981028     F*              B A N K S    D I H    L T D.              *
000500990113     F*                 SAVINGS WITHDRAWAL RESET               *
000600981028     F*                                                        *
000700981028     F*                         BY                             *
000800981028     F*                                                        *
000900981028     F*             Banks DIH I.S. department                  *
001000981028     F*             P.O BOX 10194, GEORGETOWN,GUYANA           *
001100981028     F*                                                        *
001200981028     F*                                                        *
001300990510     F*       RPG PROGRAM NAME..: LI008RCLR                    *
001400981028     F*                                                        *
001500990113     F*       UPDATED BY.....: SAMUEL BRITTON                  *
001600990510     F*       DESIGN DATE....: 10/05/1999                      *
001700981028     F*       MODULES........: *NONE                           *
001800981103     F*       INDICATORS.....: 40:toggle- type field on/off.   *
001900981028     F*                                                        *
002000990510     F*  THIS PROGRAM IS DESIGNED TO HANDLE TEL/MAG/CLUB       *
002100990116     F*  CLEARING AFTER Y/END CALCULATIONS.                    *
002200981028     F*  FILES.: LIABM   (master file)                         *
002300981028     F*          LIABT   (transaction file)                    *
002400981028     F**********************************************************
002500990116     Fliabm     if   e           k disk
002600001103     fliabt     o    e           k disk    commit
002700020206     **
002800111013      /copy QCPYSRC,genpr
002900111013      /copy QCPYSRC,apppr
003000111013      /copy QCPYSRC,emppr
003100021005     **
003200031001     d  liab_Struc   e ds                  extname(liabm)
003300031001     **
003400980206     d                sds
003500980206     d  @pgm             *proc
003600980206     d
003700111013     d  @user                358    367
003800031001     d la              s                   dim(12) like(liamt01) based(lptr)
003900980206     d mth             s              2  0
004000020206     d prno            s              6  0
004100020206     d prn             s              6  0
004200050514     d date@           s              8  0
004300050514     d countr          s              5  0
004400020206     d nxt_numb        s             11  0
004500031001     d lptr            s               *   inz(%addr(liamt01))
004600980206     **
004700981030     d
004800990510     d  #text1         c                   const('Clearing Allowances')
004900981027     D
005000980216     **
005100990116     C     *entry        plist
005200020206     C                   parm                    prno
005300981027     C
005400990116     C                   z-add     prno          mth
005500021005
005600021004      /free
005700021004
005800021005        if (mth - 1) = 0;
005900021005           mth = 12;
006000021005            else;
006100021005           mth = mth - 1;
006200021005        endif;
006300021005
006400021004        // setup liability types for clearing
006500021005        get_sys_date(date@);
006600050514
006700050514        countr = *zeros;
006800050514        dow not get_ltype_info('RD':-1:ematf_struc:countr);
006900021004            if %subst(%char(emdept):1:1) = '2';
007000021004               if %subst(%char(emdept):2:2) =
007100021004                   %subst(%char(prno):5:2);
007200021005                   exsr AllS_clr;
007300021004                  else;
007400021004                   *inlr = *on;
007500021004                   return;
007600021004                endif;
007700021004            endif;
007800050514
007900021004        enddo;
008000050514
008100021005        *inlr = *on;
008200021005        return;
008300021005
008400021005
008500021005       //*****************************************************              *
008600021005       // Clear Telephone,Magazine & Club figures for new year              *
008700021005       //*****************************************************              *
008800021005           begsr AllS_clr;
008900021005            //  get prv prd
009000021005              prn = get_new_prd(prno:1:'S');
009100021005
009200021005            // update JL number
009300021005
009400021005              nxt_numb = get_next_no('JL':'U');
009500990116
009600021005        // checks to determine if all records have been
009700021005        // selected to be processed
009800021005              setll emseltyp liabm;
009900021005              reade emseltyp liabm;
010000021005
010100021005                 dow  not(%eof);
010200021005                      if la(mth) <> 0;
010300021005
010400021005        // writing record to liability transaction file
010500021005
010600021005                          ltsts = 'X';
010700021005                          ltdate = date@;
010800021005                          lttext = #text1;
010900021005                          ltemp = liemp;
011000021005                          lttype = litype;
011100021005                          ltamt = la(mth) * -1;
011200021005                          ltprno = prn;
011300021005                          ltnumb = nxt_numb;
011400051002                          ltorg = 'SYS';
011500021005                          ltstyp = listyp;
011600021005                          ltref = *blanks;
011700021005
011800021005                         write  ltfmt;
011900021005
012000021005          // Call update utility program  (Receiving)***
012100031001                         update_liab(ltnumb:ltorg:'I':prn);
012200021005
012300021219                 //      commit;
012400021005
012500021005                      endif;
012600021005              reade emseltyp liabm;
012700021005
012800021005                 enddo;
012900021005
013000021005        // end of allowances reset sub-routine
013100021005           endsr;
013200021005
013300021005      /end-free
