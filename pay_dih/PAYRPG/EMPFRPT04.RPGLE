000100980210     H DATEDIT(*ymd)
000200170216     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')  alwnull(*usrctl)
000300981028     F* ********************************************************
000400021125     F*              EMPLOYEE ACCOUNTS SYSTEM                  *
000500990113     F*                 SAVINGS WITHDRAWAL RESET               *
000600981028     F*                                                        *
000700981028     F*                         BY                             *
000800981028     F*                                                        *
000900021125     F*             Diserv Systems                             *
001000981028     F*                                                        *
001100981028     F*                                                        *
001200990113     F*       RPG PROGRAM NAME..: SAV000R                      *
001300981028     F*                                                        *
001400990113     F*       UPDATED BY.....: SAMUEL BRITTON                  *
001500990113     F*       DESIGN DATE....: 13/01/1999                      *
001600981028     F*       MODULES........: *NONE                           *
001700981103     F*       INDICATORS.....: 40:toggle- type field on/off.   *
001800981028     F*                                                        *
001900990116     F*  THIS PROGRAM IS DESIGNED TO HANDLE SAVINGS W/DRAWAL   *
002000990116     F*  CLEARING AFTER Y/END CALCULATIONS.                    *
002100981028     F*  FILES.: LIABM   (master file)                         *
002200981028     F*          LIABT   (transaction file)                    *
002300981028     F**********************************************************
002400990116     Fliabm     if   e           k disk
002500001103     fliabt     o    e           k disk    commit
002600021125     Fsavconf   if   e             disk
002700011228     ** General PROTOTYPES
002800111013      /copy QCPYSRC,GENPR
002900111013      /copy QCPYSRC,appPR
003000980206     **
003100031001     d  liab_struc   e ds                  extname(liabm)
003200031001     **
003300980206     d                sds
003400980206     d  @pgm             *proc
003500111013     d  @user                358    367
003600011228     d
003700031001     d la              s                   dim(12) like(liamt01) based(lptr)
003800980206     d mth             s              2  0
003900021125     d pmth            s              2  0
004000011228     d nxt_numb        s             11  0
004100011228     d date@           s              8  0
004200011228     d prno            s              6  0
004300011228     d prn             s                   like(prno)
004400021125     d type            s              2  0
004500031001     d lptr            s               *   inz(%addr(liamt01))
004600980206     **
004700981030     d
004800990116     d  #text1         c                   const('Clearing Savings W/drawals')
004900981027     D
005000980216     **
005100990116     C     *entry        plist
005200011228     C                   parm                    prno
005300981027     C
005400990116     C                   z-add     prno          mth
005500021125     ** Read Control file
005600021125     C                   read      sacfmt
005700990116     ** Set Saving withdrawal type
005800021125     C                   eval      type = sactype
005900981027     C
006000021125     C                   if        mth <> sacmth
006100990116     C                   seton                                        lr
006200990116     C                   return
006300990116     C                   endif
006400021125
006500021125     C                   if        (sacmth - 1) = 0
006600021125     C                   eval      pmth = 12
006700021125     C                   else
006800021125     C                   eval      pmth = sacmth - 1
006900021125     C                   endif
007000980206     **
007100990116     *** get sys date
007200011228     C                   callp     get_sys_date(date@)
007300981027     C
007400980206     **
007500990116     C                   exsr      savW_clr
007600981030     c
007700990116     C                   return
007800981029     C
007900981029     ************************************************************************
008000990116     ** Clear savings withdrawal figures for new year                      **
008100981029     ************************************************************************
008200990116     C     savW_clr      begsr
008300990116     **  get prv prd
008400011228     C                   eval      prn = get_new_prd(prno:1:'S')
008500981029     C
008600991215     **  update JL number
008700011228     C                   eval      nxt_numb = get_next_no('JL':'U')
008800990116     **
008900990116
009000981102     ** checks to determine if all records have been selected to be processed
009100990116     C     type          chain     liabm                              Lr
009200981029     C
009300990116     C                   dow       not(*inlr)
009400021125     C                   if        la(pmth) <> 0
009500981029     C
009600981029     ** writing record to liability transaction file
009700981029     C
009800981029     C                   move      'X'           ltsts
009900981029     C                   z-add     date@         ltdate
010000981029     C                   movel     #text1        lttext
010100981029     C                   z-add     liemp         ltemp
010200981029     C                   z-add     litype        lttype
010300981029     C
010400981029     C
010500021125     C                   z-sub     la(pmth)      ltamt
010600981029     C
010700991115     C                   z-add     prn           ltprno
010800991215     C                   z-add     nxt_numb      ltnumb
010900051002     C                   move      'SYS'         ltorg
011000981029     C                   z-add     *zeros        ltstyp
011100981029     C                   move      *blanks       ltref
011200981029     C
011300981029     C                   write     ltfmt
011400990113     C
011500990113     *** Call update utility program  (Receiving)***
011600031001      /free
011700031001                         update_liab(ltnumb:ltorg:'I':prn);
011800031001      /end-free
011900990113     **
012000981029     C                   endif
012100990116     C     type          reade     liabm                                  lr
012200990116     C
012300981029     C                   enddo
012400981029     C
012500981029     C
012600990113     ** end of withdrawal reset sub-routine
012700981029     C                   endsr
012800981103     C
012900981102     C
013000981102     C
013100981029     *****************************************************************
013200981027     **
