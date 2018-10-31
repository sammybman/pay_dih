000100941121     H DATEDIT(*DMY)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300111002     H/define profoundui
000400990209     **************************************************
000500010425     ** BANKS PAYROLL RUN TYPE MAINTENANCE PROGRAM  **
000600990209     **************************************************
000700111002     Fpayrtypl01if   e           k disk    usropn
000800020613     **
000900111002     Fpaycomm   uf a e           k disk    usropn
001000020613     **
001100021115     Fpaymnt04d cf   e             workstn
001200111002     F/IF DEFINED(WEB)
001300111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001400111002     F/ENDIF
001500010508     ** Payroll PROTOTYPES
001600111002      /copy QCPYSRC,genPR
001700111002      /copy QCPYSRC,autPR
001800990112     **
001900010815     D                sds
002000990211     D  @pgm             *proc
002100111002     D  @user                358    367
002200021115     **
002300021115     D arr             S             40    DIM(1) CTDATA PERRCD(1)
002400021115     D @date           s             10
002500021115     D date            s              8  0
002600021115     D delete_flag     s               n   inz(*off)
002700010425     **
002800021115     D  msg1           c                   'Comments cannot be Blanks'
002900021115     D  msg2           c                   'Delete Request cancelled'
003000021115     D  msg3           c                   'Deletion completed successfully'
003100021115     D  msg4           c                   'Invalid run type entered'
003200021118     D  type_text      c                   'ALL RUN TYPES'
003300950711     **
003400010424     C                   move      *blanks       #mode
003500040207      /free
003600111002         // authority check
003700111002         monitor;
003800111002           authority_check('PAY':@pgm);
003900111002         on-error;
004000111002           show_msg(get_err_msg('DIH0054'));
004100111002           *inlr = *on;
004200111002           return;
004300111002         endmon;
004400040207      /end-free
004500010424     ** Initial Setup
004600010424     C                   exsr      aainit
004700010424     ************************************
004800010424     ** First screen to request number **
004900010424     ************************************
005000010424     C                   dow       0 = 0
005100941123     **
005200010424     C                   exfmt     fmt1
005300941123     **
005400010424     C                   if        *in03
005500010424     C                   eval      *inlr = *on
005600010424     C                   return
005700010424     C                   endif
005800010424
005900021115     C                   eval      errmsg = *blanks
006000021115
006100010917     C                   if        *in01
006200010509     C                   call      'PAYENQ02R'
006300021115     C                   parm                    ptype#
006400010509     C                   iter
006500010509     C                   endif
006600021115     ** Delete routine
006700021115     C                   if        *in04
006800021115     C                   eval      delete = 'N'
006900021115     C                   eval      delete_flag = *on
007000021115     C                   exsr      delete_sr
007100021115     C                   iter
007200021115     C                   endif
007300021115
007400021115     C                   if        ptype# <> *blanks
007500021115     C     ptype#        chain     prtyfmt
007600021115     C                   if        not(%found)
007700021115     C                   eval      errmsg = msg4
007800021115     C                   iter
007900021115     C                   endif
008000021118     C                   eval      run_desc = prtydesc
008100021118     C                   else
008200021118     C                   eval      run_desc = type_text
008300021115     C                   endif
008400010424     **
008500021115     C     ptype#        chain     pcomfmt                            80
008600010424     C                   if        *in80
008700010425     C                   move      ' ADD  '      #mode
008800021115     C                   clear                   pcomfmt
008900010425     ** setup mode description
009000010424     C                   else
009100010430     C                   move      'AMEND '      #mode
009200010424     C                   endif
009300010424
009400010424     C                   leave
009500010424     C                   enddo
009600941121     **
009700010424     **********************
009800010424     ** Body of Program  **
009900010424     **********************
010000010424     C                   dow       0 = 0
010100941121     **
010200010430     C                   exfmt     fmt2
010300990209
010400010424     ** clear error Field
010500010424     C                   eval      errmsg = *blanks
010600010424     ** End Program
010700010424     C                   if        *in03
010800010424     C                   eval      *inlr = *on
010900010424     C                   return
011000010424     C                   endif
011100010424     ** Cancel selection
011200010424     C                   if        *in02
011300010424     C                   leave
011400010424     C                   endif
011500020613     ** Delete routine
011600020613     C                   if        *in04
011700020613     C                   eval      delete = 'N'
011800021115     C                   eval      delete_flag = *off
011900020613     C                   exsr      delete_sr
012000020613     C                   leave
012100020613     C                   endif
012200010430     ** Validate Description
012300021115     C                   if        pcomdesc1 = *blanks and pcomdesc2 =
012400021115     C                              *blanks and pcomdesc3 = *blanks
012500010424     C                   eval      errmsg = msg1
012600010424     C                   iter
012700010424     C                   endif
012800010425     ** Confirm record
012900010425     C                   if        *in15
013000010425     C                   exsr      confirm_sr
013100010917     C                   leave
013200010425     C                   endif
013300010424     **
013400010424     C                   enddo
013500111002      /free
013600111002
013700111002         //*****************************
013800111002         // Session Init
013900111002         //*****************************
014000111002
014100111002          begsr *inzsr;
014200111006            get_session_info('PAY':errmsg:@pgm);
014300111002            if errmsg <> *blanks;
014400111002              show_msg(errmsg);
014500111002              *inlr = *on;
014600111002              return;
014700111002            endif;
014800111002
014900111002            open(e) payrtypl01;
015000111002            open(e) paycomm;
015100111002
015200111002          endsr;
015300111002
015400111002      /end-free
015500111002
015600010425     *************************
015700010425     ** Write/Update Record **
015800010425     *************************
015900010425     C     confirm_sr    begsr
016000010425     ** Write/Update Record
016100010425     C                   if        *in80
016200010425     **
016300021115     C                   eval      pcomtype = ptype#
016400021115     C                   write     pcomfmt
016500010425     **
016600010425     C                   else
016700021115     C                   update    pcomfmt
016800010425     C                   endif
016900010425     C                   endsr
017000020613     *******************
017100020613     ** Delete Record **
017200020613     *******************
017300020613     C     delete_sr     begsr
017400020613
017500020613     ** Present Confirm Screen
017600020613     C                   exfmt     win2
017700020613     **
017800020613     C                   if        delete = 'Y'
017900020613
018000021115     C                   select
018100021115     C                   when      delete_flag
018200021115     C     *loval        setll     pcomfmt
018300021115     C                   read      pcomfmt
018400021115     C                   dow       not(%eof)
018500021115     C                   delete    pcomfmt
018600021115     C                   read      pcomfmt
018700021115     C                   enddo
018800021115     C                   other
018900021115     C                   delete    pcomfmt
019000021115     C                   endsl
019100021115
019200021115     C                   eval      errmsg = msg3
019300020613     **
019400020613     C                   else
019500021115     C                   eval      errmsg = msg2
019600020613     C                   endif
019700020613     C                   endsr
019800941120     **
019900010424     ***************************************************
020000010424     ** First initilisation routine for all programs  **
020100010424     ***************************************************
020200010424     C     AAINIT        BEGSR
020300010815     C                   eval      CONAME = get_coname
020400010815     C                   eval      @DATE = get_sys_date(date)
020500010424     C                   time                    @TIME             6 0
020600010424     ** select header for program
020700010424     C                   movel(p)  ARR(1)        @TITLE
020800010424     **
020900010424     C                   ENDSR
021000010424     **
021100010424**  Program Title
021200021115      Payslip comments
