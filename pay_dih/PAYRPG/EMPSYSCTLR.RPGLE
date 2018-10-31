000100111013     H DECEDIT('.') bnddir('DISDIR') dftactgrp(*no) actgrp(*new)
000200111013     H/define profoundui
000300000313     F**********************************************************
000400010607     F*                   DISERV  SYSTEM                       *
000500021005     F*                EMPLOYEE CREDIT CONTROL SYSTEM          *
000600980616     F*                                                        *
000700980616     F*                         BY                             *
000800980616     F*                                                        *
000900020716     F*             Diserv Systems                             *
001000980616     F*             P.O BOX 10194, GEORGETOWN,GUYANA           *
001100000313     F*
001200000313     F*                                                        *
001300980616     F*                                                        *
001400021005     F*       RPG PROGRAM NAME..: EMPSYSCTLR                   *
001500980616     F*                                                        *
001600010607     F*       PROGRAMMER NAME: Samuel Britton                  *
001700010607     F*                        Christopher Williams            *
001800010716     F*       DESIGN DATE....: 2001/07/16                      *
001900021005     F*       MODULES........: EMPSYSCTLR                      *
002000021005     F*                        EMPCONT0R                       *
002100980917     F*       INDICATORS.....:                                 *
002200980917     F*                                                        *
002300980616     F*                                                        *
002400010716     F*  THIS PROGRAM IS DESIGNED  to configure  system        *
002500010716     F*                            parameters                  *
002600980616     F*                                                        *
002700980616     F**********************************************************
002800021005     Fempsysctldcf   e             workstn
002900111013     F/IF DEFINED(WEB)
003000111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
003100111013     F/ENDIF
003200010607     **
003300111013      /copy QCPYSRC,genPR
003400111013      /copy QCPYSRC,autPR
003500000406     D ARR             S             40    DIM(2) CTDATA PERRCD(1)
003600011025     D origin          S              3
003700020717     D ck_pgm          s             10
003800020717     D ck_isys         s              3
003900111013
004000111013     D ADMINUSER       c                   'DISERVADMN'
004100020717     **
004200980319     D                sds
004300980917     D  @pgm             *proc
004400111013     D  @user                358    367
004500980603     **
004600000407
004700040207      /free
004800111013         // authority check
004900111013         monitor;
005000111013             authority_check('EMT':@pgm);
005100111013         on-error;
005200111013          show_msg(get_err_msg('DIH0054'));
005300111013          *inlr = *on;
005400111013          return;
005500111013         endmon;
005600111013
005700040207      /end-free
005800981005     C
005900000310     C                   exsr      aainit
006000010607     *******************
006100010607     ** Program Start
006200010607     *******************
006300010607     C                   dow       0 = 0
006400000210     C                   exfmt     fmt1
006500981005     C
006600981005     ** end of program
006700980917     C                   if        *in03
006800010607     C                   eval      *inlr = *on
006900980917     C                   return
007000980917     C                   endif
007100980917     C
007200010716     C                   select
007300010716     C                   when      option = 1
007400111013     C                   call      'EMPCONT0R'
007500010716     C                   when      option = 2
007600011025     C                   call      'SYS000R'
007700021005     C                   parm      'EMT'         origin
007800021010     C                   when      option = 3
007900111013     C                   call      'EMPCONT1R'
008000010716     C                   endsl
008100000303     C
008200010716     C                   enddo
008300000303     C
008400111013      /free
008500111013
008600111013         //*****************************
008700111013         // Session Init
008800111013         //*****************************
008900111013
009000111013          begsr *inzsr;
009100111013            get_session_info('EMT':errmsg:@pgm);
009200111013            if errmsg <> *blanks;
009300111013              show_msg(errmsg);
009400111013              *inlr = *on;
009500111013              return;
009600111013            endif;
009700111013
009800111013          endsr;
009900111013
010000111013      /end-free
010100000330     C
010200980917     *****************************************************************
010300980917     ** Begin subroutine to print header
010400980917     *****************************************************************
010500980917     C     AAINIT        BEGSR
010600010815     C                   eval      CONAME = get_coname
010700010815     C                   eval      @DATE = get_sys_date
010800980917     C                   TIME                    @TIME             6 0
010900980917     C                   MOVEL     ARR(1)        @TITLE
011000980917     C
011100980917     C                   ENDSR
011200980917     **
011300980917**   Header to be displayed
011400010716      System Configuration
