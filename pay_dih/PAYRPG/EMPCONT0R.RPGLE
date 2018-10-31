000100941121     H DATEDIT(*DMY)
000200111013     H DECEDIT('.') bnddir('DISDIR') dftactgrp(*no) actgrp(*new)
000300111013     H/define profoundui
000400021005     Fempcon    uf a e           k disk
000500021005     Fpaycycd   if   e           k disk
000600021005     Fempcont0d cf   e             workstn
000700111013     F/IF DEFINED(WEB)
000800111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000900111013     F/ENDIF
001000010301     **
001100111013      /copy QCPYSRC,genpr
001200111013      /copy QCPYSRC,glpr
001300111013      /copy QCPYSRC,PAYPR
001400010815     D@date            s             10
001500010815     D                sds
001600950711     D  @PGM             *PROC
001700010716     **
001800020717     D  err_his        c                   'Valid Values 1-999'
001900021005     D  err_fin        c                   'Valid Values 1-12'
002000021005     D  err_cyc        c                   'Must enter a valid Cycle Code'
002100020717     **
002200010716     D arr             S             40    DIM(1) CTDATA PERRCD(1)
002300010228     **
002400010301     ** Initial Setup
002500111013      /copy QCPYSRC,aainit
002600950711     **
002700010301     *******************
002800010301     ** Program start **
002900010301     *******************
003000941123     **
003100021005     C                   read      esyfmt
003200941121     **
003300021005     C                   if        esysupdprd <> *zeros
003400020717     C                   eval      *in99= *on
003500011102     C                   else
003600020717     C                   eval      *in99 = *off
003700011102     C                   endif
003800020717
003900010301     ***********************
004000010301     ** Body of Program   **
004100010301     ***********************
004200010301     C                   dow       0 = 0
004300011018
004400011102     C                   exfmt     fmt1
004500011018     C                   eval      errmsg = *blanks
004600010301     ** Cancel Entry
004700010301     C                   if        *in02
004800010301     C                   leave
004900010301     C                   endif
005000010301     ** End Program
005100010301     C                   if        *in03
005200010301     C                   eval      *inlr = *on
005300010419     C                   return
005400010301     C                   endif
005500011023
005600021005     C                   if        esyshis < 1
005700011102     C                   eval      errmsg = err_his
005800011102     C                   iter
005900011102     C                   endif
006000021005
006100021005     C                   if        esysfinmth < 1 or esysfinmth > 12
006200021005     C                   eval      errmsg = err_fin
006300021005     C                   iter
006400021005     C                   endif
006500020717
006600021005     ** Validate Cycle code
006700021005     C                   if        esysdedcyc <> *blanks
006800021005     C     esysdedcyc    chain     cycfmt
006900021005     C                   if        not%found
007000021005     C                   eval      errmsg = err_cyc
007100021005     C                   iter
007200021005     C                   endif
007300021005     C                   endif
007400021005
007500020717     **  Validate Default Gl. Code (interest)
007600031001      /free
007700031001       errmsg = *blanks;
007800031001       get_gl_info(esysinacct:errmsg:'V');
007900031001      /end-free
008000020717     **
008100020717     C                   if        errmsg <> *blanks
008200020717     C                   iter
008300020717     C                   endif
008400020717     **  Validate Default Gl. Code (Write-offs)
008500031001      /free
008600031001       errmsg = *blanks;
008700031001       get_gl_info(esysceacct:errmsg:'V');
008800031001      /end-free
008900020717     **
009000020717     C                   if        errmsg <> *blanks
009100020717     C                   iter
009200020717     C                   endif
009300011023     ** F15 to confirm
009400011023     c                   if        *in15
009500011023     c                   exsr      confirm_sr
009600011102     C                   eval      *inlr = '1'
009700011102     C                   return
009800011023     C                   endif
009900010301     **
010000010301     C                   enddo
010100021005     *************************************
010200011023     **
010300021005     *************************************
010400011023     c     confirm_sr    begsr
010500011023     **
010600020717     C                   if        not(%eof)
010700021005     C                   update    esyfmt
010800011023     C                   else
010900021005     C                   eval      esysdedcyh = esysdedcyc
011000021005     C                   write     esyfmt
011100011023     C                   endif
011200011023     C                   endsr
011300010301**  Program Title
011400021005    Employee Credit System Control
