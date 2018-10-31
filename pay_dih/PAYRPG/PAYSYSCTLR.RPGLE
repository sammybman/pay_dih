000100111002     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111002     H/define profoundui
000300000313     F**********************************************************
000400010607     F*                   DISERV  SYSTEM                       *
000500010607     F*                   PAYROLL SYSTEM                       *
000600980616     F*                                                        *
000700980616     F*                         BY                             *
000800980616     F*                                                        *
000900980616     F*             Banks DIH I.S. department                  *
001000980616     F*             P.O BOX 10194, GEORGETOWN,GUYANA           *
001100000313     F*
001200000313     F*                                                        *
001300980616     F*                                                        *
001400140924     F*       RPG PROGRAM NAME..: PAYSYSCTLR                   *
001500980616     F*                                                        *
001600010607     F*       PROGRAMMER NAME: Samuel Britton                  *
001700010607     F*                        Christopher Williams            *
001800010716     F*       DESIGN DATE....: 2001/07/16                      *
001900140616     F*       MODULES........: *none                           *
002000140616     F*                                                        *
002100140616     F*                                                        *
002200980917     F*       INDICATORS.....:                                 *
002300980917     F*                                                        *
002400980616     F*                                                        *
002500010716     F*  THIS PROGRAM IS DESIGNED  to configure  system        *
002600010716     F*                            parameters                  *
002700980616     F*                                                        *
002800980616     F**********************************************************
002900010716     Fpaysysctldcf   e             workstn
003000111002     F/IF DEFINED(WEB)
003100111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
003200111002     F/ENDIF
003300010607     **
003400111002      /copy QCPYSRC,genPR
003500111002      /copy QCPYSRC,autPR
003600111002
003700000406     D ARR             S             40    DIM(2) CTDATA PERRCD(1)
003800011025     D origin          S              3
003900111002
004000980319     D                sds
004100980917     D  @pgm             *proc
004200111002     D  @user                358    367
004300010607     **
004400141008     DMsg1             C                   'Invalid Options Entered'
004500980603     **
004600000407
004700141008
004800111002         // authority check
004900111002         monitor;
005000111002           authority_check('PAY':@pgm);
005100111002         on-error;
005200111002           show_msg(get_err_msg('DIH0054'));
005300111002           *inlr = *on;
005400111002           return;
005500111002         endmon;
005600141008
005700111002
005800000310     C                   exsr      aainit
005900010607     *******************
006000010607     ** Program Start
006100010607     *******************
006200010607     C                   dow       0 = 0
006300000210     C                   exfmt     fmt1
006400141008
006401141008              errmsg = *blanks;
006500141008             // end of program
006600980917     C                   if        *in03
006700010607     C                   eval      *inlr = *on
006800980917     C                   return
006900980917     C                   endif
007000980917     C
007100010716     C                   select
007200010716     C                   when      option = 1
007300111002     C                   call      'PAYTAXCTLR'
007400010716     C                   when      option = 2
007500111002     C                   call      'PAYNISCTLR'
007600011025     C                   when      option = 3
007700111002     C                   call      'PAYCONT0R'
007800011025     C                   when      option = 4
007900011025     C                   call      'SYS000R'
008000011025     C                   parm      'PAY'         origin
008001141007     C                   when      option = 5
008002141008     C                   call      'PAYINTCTLR'
008003141008                         other;
008004141008                          errmsg = msg1;
008005141008                          iter;
008100010716     C                   endsl
008200000303     C
008300010716     C                   enddo
008400000303     C
008500141007
008600111002
008700111002         //*****************************
008800111002         // Session Init
008900111002         //*****************************
009000111002
009100111002          begsr *inzsr;
009200111006            get_session_info('PAY':errmsg:@pgm);
009300111002            if errmsg <> *blanks;
009400111002              show_msg(errmsg);
009500111002              *inlr = *on;
009600111002              return;
009700111002            endif;
009800111002
009900111002          endsr;
010000141007
010100111002
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
