000100941121     H DATEDIT(*DMY)
000200111013     H DECEDIT('.') bnddir('DISDIR') dftactgrp(*no) actgrp(*new)
000300111013     H/define profoundui
000400021010     Fsavconf   uf a e           k disk
000500021010     Fempcont1d cf   e             workstn
000600111013     F/IF DEFINED(WEB)
000700111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000800111013     F/ENDIF
000900010301     **
001000111013      /copy QCPYSRC,genpr
001100111013      /copy QCPYSRC,emppr
001200111013      /copy QCPYSRC,PAYPR
001300010815     D@date            s             10
001400021010     Dsav_type         s              1
001500111013
001600010815     D                sds
001700950711     D  @PGM             *PROC
001800010716     **
001900021005     D  err_fin        c                   'Valid Values 1-12'
002000021128     D  msg1           c                   'Invalid Tax /Ytd combination'
002100020717     **
002200010716     D arr             S             40    DIM(1) CTDATA PERRCD(1)
002300020717     D trn_type        S              1
002400010228     **
002500010301     ** Initial Setup
002600111013      /copy QCPYSRC,aainit
002700950711     **
002800010301     *******************
002900010301     ** Program start **
003000010301     *******************
003100941123     **
003200021010     C                   read      sacfmt
003300941121     **
003400021010     C                   if        sacmth <> *zeros
003500020717     C                   eval      *in99= *on
003600011102     C                   else
003700020717     C                   eval      *in99 = *off
003800011102     C                   endif
003900020717
004000010301     ***********************
004100010301     ** Body of Program   **
004200010301     ***********************
004300010301     C                   dow       0 = 0
004400011018
004500011102     C                   exfmt     fmt1
004600011018     C                   eval      errmsg = *blanks
004700010301     ** Cancel Entry
004800010301     C                   if        *in02
004900010301     C                   leave
005000010301     C                   endif
005100010301     ** End Program
005200010301     C                   if        *in03
005300010301     C                   eval      *inlr = *on
005400010419     C                   return
005500010301     C                   endif
005600021005
005700021010     ** Check file for valid liability type
005800050513     C                   if        get_ltype_info('RD':sactype:ematf_struc)
005900021010     C                   iter
006000021010     C                   endif
006100021010     ** Check file for valid start month
006200021010     C                   if        sacmth < 1 or sacmth > 12
006300021005     C                   eval      errmsg = err_fin
006400021005     C                   iter
006500021005     C                   endif
006600021010
006700021121     ** more options
006800021121     C                   if        *in10
006900021128     C                   dow       0 = 0
007000021121     C                   exfmt     fmt2
007100021128     C                   eval      errmsg = *blanks
007200021128     C                   if        sactaxper <> *zeros
007300021128     C                             and sacytdpay = 'Y'
007400021128     C                   eval      errmsg = msg1
007500021128     C                   iter
007600021128     C                   endif
007700021128     C                   leave
007800021128     C                   enddo
007900021128     C                   if        *in02
008000021128     C                   iter
008100021128     C                   endif
008200021121     C                   endif
008300021121
008400021011     C****               if        *in10
008500021011     C                   select
008600021011     C                   when      sacintd = 'Y'
008700111013     C                   call      'EMPCONT101'
008800021010     C                   parm      'I'           sav_type
008900021011     C                   other
009000021011     C                   endsl
009100021011     C****               endif
009200021010
009300021011     C****               if        *in10
009400021011     C                   select
009500021011     C                   when      saccond = 'Y'
009600111013     C                   call      'EMPCONT101'
009700021010     C                   parm      'E'           sav_type
009800021011     C                   other
009900021011     C                   endsl
010000021011     C****               endif
010100020717
010200011023     ** F15 to confirm
010300011023     c                   if        *in15
010400011023     c                   exsr      confirm_sr
010500011102     C                   eval      *inlr = '1'
010600011102     C                   return
010700011023     C                   endif
010800010301     **
010900010301     C                   enddo
011000021005     *************************************
011100011023     **
011200021005     *************************************
011300011023     c     confirm_sr    begsr
011400011023     **
011500020717     C                   if        not(%eof)
011600021010     C                   update    sacfmt
011700011023     C                   else
011800021010     C                   write     sacfmt
011900011023     C                   endif
012000011023     C                   endsr
012100010301**  Program Title
012200021010        Savings Configuration
