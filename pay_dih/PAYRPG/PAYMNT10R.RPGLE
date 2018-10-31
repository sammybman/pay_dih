000100950911     H* ********************************************************
000200950911     H*              B A N K S    D I H    L T D.              *
000300010808     H*                    PAYROLL SYSTEM                      *
000400950911     H*                                                        *
000500950911     H*                         BY                             *
000600950911     H*                                                        *
000700010808     H*                    DISERV SYSTEMS                      *
000800950911     H*                                                        *
000900010808     H*       RPG PROGRAM NAME..: Paymnt10r                    *
001000950911     H*                                                        *
001100951025     H*       PROGRAMMER NAME: CHRIS WILLIAMS                  *
001200010808     H*       DESIGN DATE....: 2001/08/08                      *
001300950911     H*                                                        *
001400010808     H*  PROGRAM DESCRIPTION: Holiday Maintenance              *
001500950911     H*                                                        *
001600010808     H*  THIS PROGRAM IS DESIGNED TO MAINTAIN THE Holiday      *
001700010808     H*   MASTER FILE (ADDING DATA AS WELL AS UPDATING)        *
001800950911     H*                                                        *
001900950911     H*                                                        *
002000950911     H**********************************************************
002100941121     H DATEDIT(*DMY)
002200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
002300111002     H/define profoundui
002400111002     Fholl01    uf a e           k disk    usropn
002500010808     Fpaymnt10d cf   e             workstn
002600111002     F/IF DEFINED(WEB)
002700111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002800111002     F/ENDIF
002900010813     ** Payroll/general PROTOTYPES
003000111002      /copy QCPYSRC,genpr
003100111002      /copy QCPYSRC,PAYPR
003200111002      /copy QCPYSRC,autpr
003300010808     **
003400010808     D ARR             S             40    dim(1) CTDATA PERRCD(1)
003500010813     D day             S              1s 0
003600010808     D msg1            c                   'Invalid date Entered'
003700010808     D msg2            c                   'Must enter Holiday Description'
003800010808     **
003900950911     D                SDS
004000950911     D  @PGM             *PROC
004100111002     D  @USER                358    367
004200010808
004300040207      /free
004400111002         // authority check
004500111002         monitor;
004600111002           authority_check('PAY':@pgm);
004700111002         on-error;
004800111002           show_msg(get_err_msg('DIH0054'));
004900111002           *inlr = *on;
005000111002           return;
005100111002         endmon;
005200040207      /end-free
005300950920     ** Execute subroutine to show header
005400010808     C                   exsr      aainit
005500010808
005600010808     ************************
005700010808     ** Program Start      **
005800010808     ************************
005900010808     C                   dow       0 = 0
006000950920     **
006100010808     ** Display to allow Holiday  date entry
006200010808     C                   exfmt     fmt1
006300950911     **
006400010808     C                   eval      errmsg =  *blanks
006500941122     **
006600010808     **  F3 to end program
006700010808     C                   if        *in03
006800010808     C                   eval      *inlr = *on
006900010808     C                   return
007000010808     C                   endif
007100050612      /free
007200050612
007300050612            // GET Holiday ENQUIRY
007400050612            if   *in04;
007500050612               sys_callender(*omit:hodate#);
007600050612               iter;
007700050612            endif;
007800050612      /end-free
007900010808
008000010808     ** GET Holiday ENQUIRY
008100010808     C                   if        *in05
008200010816     C                   call      'PAYENQ03R'
008300010808     C                   iter
008400010808     C                   endif
008500951130     **
008600010808     ** Check for valid date
008700010815     C     *iso          test(de)                hodate#
008800010815     C                   if        %error
008900010808     C                   eval      errmsg =  msg1
009000010808     C                   iter
009100010808     C                   endif
009200010808     **
009300010808     C     hodate#       chain     holfmt
009400010808     C                   if        %found
009500010808     C                   eval      @mode = 'AMEND '
009600010808     C                   eval      *in99 = *on
009700010808     **
009800010808     C                   else
009900010808     C                   eval      @mode = ' ADD  '
010000010815     C                   eval      hodesc = *blanks
010100010808     C                   eval      *in99 = *off
010200010808     C                   endif
010300010808     ** DAY OF WEEK
010400010815     C                   eval      day = dayofweek(hodate#:day#)
010500010808     C                   leave
010600010808     C                   enddo
010700010808
010800010808     ********************************************************************
010900010808     ** This screen display details of Holiday master and also enables
011000950920     ** you to adjust any changes necessary
011100010808     ********************************************************************
011200010808     C                   dow       0 = 0
011300010808     C                   exfmt     fmt2
011400951026     **
011500010808     C                   eval      errmsg = *blanks
011600010808     ** F2 = go back
011700010808     C                   if        *in02
011800010808     C                   leave
011900010808     C                   endif
012000010808     ** F3 = end program
012100010808     C                   if        *in03
012200010808     C                   eval      *inlr = *on
012300010808     C                   return
012400010808     C                   endif
012500010808     ** Validate Description entry
012600010808     C                   if        hodesc = *blanks
012700010808     C                   eval      errmsg = msg2
012800010808     C                   iter
012900010808     C                   endif
013000010808     ** Any entry changes
013100010808     C   40              iter
013200010808     ** Perform Updates
013300010808     C                   select
013400010808     C                   when      *in20
013500010808     C                   delete    holfmt
013600010808     C                   other
013700010808     **
013800010808     C                   if        %found
013900010808     C                   update    holfmt
014000010808     C                   else
014100010808     C                   eval      hosts = 'A'
014200010808     C                   eval      hodate = hodate#
014300010808     C                   write     holfmt
014400010808     C                   endif
014500010808     **
014600010808     C                   leave
014700010808     C                   endsl
014800941120     **
014900010808     C                   enddo
015000111002      /free
015100111002
015200111002         //*****************************
015300111002         // Session Init
015400111002         //*****************************
015500111002
015600111002          begsr *inzsr;
015700111006            get_session_info('PAY':errmsg:@pgm);
015800111002            if errmsg <> *blanks;
015900111002              show_msg(errmsg);
016000111002              *inlr = *on;
016100111002              return;
016200111002            endif;
016300111002
016400111002            open(e) holl01;
016500111002
016600111002          endsr;
016700111002      /end-free
016800010808     **
016900950920     ** The subroutine header
017000010808     C     aainit        begsr
017100010813     C                   eval      coname = get_coname
017200010815     C                   eval      @date  = get_sys_date
017300010808     **
017400950911     C                   TIME                    @TIME             6 0
017500950911     C                   MOVEL     ARR(1)        @TITLE
017600950911     C                   ENDSR
017700951025**
017800010808        Holiday Maintenance
