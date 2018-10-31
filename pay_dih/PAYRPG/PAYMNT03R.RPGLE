000100941121     H DATEDIT(*DMY)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300111002     H/define profoundui
000400111002     Fpaycrosl01uf a e           k disk    usropn
000500020213     Fpaycrosl03if   e           k disk    rename(pcrsfmt:pcrschk) prefix(c)
000600111002     F                                     usropn
000700010412     Fpaymnt03d cf   e             workstn
000800111002     F/IF DEFINED(WEB)
000900111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001000111002     F/ENDIF
001100111002
001200111002      /copy QCPYSRC,genpr
001300111002      /copy QCPYSRC,emppr
001400111002      /copy QCPYSRC,autpr
001500111002
001600010815     D@date            s             10
001700111002
001800010302     D                sds
001900010412     D  @pgm             *proc
002000111002     D  @user                358    367
002100090415
002200090415     D  EMOLUMENT      c                   'Emolument'
002300090415     D  DEDUCTION      c                   'Deduction'
002400090415
002500010827     D  msg1           c                   'Invalid Payroll Type'
002600010827     D  msg2           c                   'Cannot select a Type that is not a +
002700090415     D                                      deduction or charge allowance'
002800010827     D  msg3           c                   'Invalid Liability Type'
002900010827     D  msg4           c                   'Liability Type in Use - check pay t+
003000010827     D                                      ype '
003100090415     D  msg5           c                   'Liability Type mismatch - allowance+
003200090415     D                                      = emolument, liability = deduction'
003300111002
003400010827     D arr             s             40    dim(1) ctdata perrcd(1)
003500111002
003600050525     D found_ind       s               n
003700010412     D ind_ptr         s               *   inz(%addr(*in))
003800010412     D                 ds                  based(ind_ptr)
003900111002
004000010922     Ddisplay_list             1      1n
004100010412     Dcancel_option            2      2n
004200010412     Dend_program              3      3n
004300010412     Ddelete_record            4      4n
004400010412     Drecord_changed          88     88n
004500010412     Ddelete_confrm           90     90n
004600010412     Dnew_record              99     99n
004700111002
004800040207      /free
004900111002         // authority check
005000111002         monitor;
005100111002           authority_check('PAY':@pgm);
005200111002         on-error;
005300111002           show_msg(get_err_msg('DIH0054'));
005400111002           *inlr = *on;
005500111002           return;
005600111002         endmon;
005700040207      /end-free
005800010228
005900020213     C     trn_key       klist
006000020213     C                   kfld                    emseltyp#
006100020213     C                   kfld                    emsubtyp#
006200090415      // Initial Setup
006300090415      /FREE
006400090415       exsr aainit;
006500090415       //
006600090415       //*****************
006700090415       // Program start **
006800090415       //*****************
006900090415       dow 0 = 0;
007000010301
007100090415         // First screen to request type
007200090415         //
007300090415         exfmt fmt1;
007400090415         clear errmsg;
007500090415         // End Program
007600090415         if end_program;
007700090415           *inlr = *on;
007800090415           return;
007900090415         endif;
008000090415         // Display List
008100090415         if display_list;
008200090415      /END-FREE
008300010412     C                   call      'PAYENQ01R'
008400010412     C                   parm                    ptyptype#
008500090415      /FREE
008600090415           iter;
008700090415         endif;
008800010412
008900090415         // Validate payroll type
009000050525           found_ind = get_payty_info('RD':ptyptype#:payty_struc);
009100050524
009200090415           select;
009300090415             when  not found_ind;
009400090415               if    ptyrtype = 'E' and ptypaytyp <> 'S';
009500090415                  errmsg =  msg2;
009600090415                  iter;
009700090415               endif;
009800090415
009900090415               if  ptyrtype = 'E';
010000090415                  desc# = EMOLUMENT;
010100090415                 else;
010200090415                  desc# = DEDUCTION;
010300090415               endif;
010400090415             other;
010500090415               errmsg =  msg1;
010600090415               iter;
010700090415           endsl;
010800090415
010900090415         // Check For Cross reference record
011000090415         chain ptyptype# pcrsfmt;
011100090415         if %found;
011200010412
011300090415           // Get description for Liability type
011400050513           get_ltype_info('RD':pcrstype:ematf_Struc);
011500050513           emseltyp# = pcrstype;
011600050513           emsubtyp# = pcrsstype;
011700050513
011800050513          if  get_lstype_info('RD':pcrstype:pcrsstype:eastf_Struc);
011900050513
012000090415           eadsp = *blanks;
012100090415         endif;
012200090415         //
012300090415         #mode = 'AMEND ';
012400090415         new_record = *off;
012500090415         //
012600090415       else;
012700090415         new_record = *on;
012800090415         // Clear input fields
012900090415         clear pcrsfmt;
013000090415         emseltyp# = *zeros;
013100090415         emsubtyp# = *zeros;
013200090415         emdsp = *blanks;
013300090415         #mode = ' ADD  ';
013400090415       endif;
013500010301
013600090415       leave;
013700090415       enddo;
013800090415
013900090415       //*********************
014000090415       // Body of Program   **
014100090415       //*********************
014200090415       dow 0 = 0;
014300090415         exfmt fmt2;
014400090415         clear errmsg;
014500090415
014600090415         // Cancel Entry
014700090415         if cancel_option;
014800090415           leave;
014900090415         endif;
015000090415
015100090415         // End Program
015200090415         if end_program;
015300090415           *inlr = *on;
015400090415           return;
015500090415         endif;
015600090415
015700090415         errmsg = *blanks;
015800090415
015900090415          // Validate Liability Type
016000050513          if  get_ltype_info('RD':emseltyp#:ematf_Struc);
016100050513             errmsg =  msg3;
016200050513             iter;
016300050513          endif;
016400050513
016500050513          if check_ltype(emseltyp#:emsubtyp#:*blanks:
016600050513                         *omit:errmsg);
016700050513             iter;
016800050513          endif;
016900050513
017000050513          if emsubtyp# <> *zeros;
017100050513            get_lstype_info('RD':emseltyp#:emseltyp#:eastf_Struc);
017200050513          endif;
017300090415
017400090415         // check for valid match type
017500090415         if %subst(%char(emdept):1:1) = '1' and ptyrtype <> 'D'
017600090415           or %subst(%char(emdept):1:1) = '2' and ptyrtype <> 'E';
017700090415           errmsg =  msg5;
017800090415           iter;
017900090415          endif;
018000020213
018100090415         // Check for duplicates
018200090415         chain trn_key pcrschk;
018300090415         if %found and ptyptype# <> cpcrsptype;
018400090415           errmsg =  msg4 + %char(cpcrsptype);
018500090415           iter;
018600090415         endif;
018700090415
018800090415         // Record changed
018900090415         if record_changed;
019000090415           iter;
019100090415         endif;
019200010412
019300090415         // Setup Delete Option
019400090415         if delete_record and not(delete_confrm);
019500010827
019600090415           iter;
019700090415         else;
019800090415           if not(delete_record);
019900090415             delete_confrm = *off;
020000090415           endif;
020100090415         endif;
020200010412
020300090415         exsr update_rtn;
020400090415
020500090415         leave;
020600090415
020700090415       enddo;
020800111002
020900111002         //*****************************
021000111002         // Session Init
021100111002         //*****************************
021200111002
021300111002          begsr *inzsr;
021400111006            get_session_info('PAY':errmsg:@pgm);
021500111002            if errmsg <> *blanks;
021600111002              show_msg(errmsg);
021700111002              *inlr = *on;
021800111002              return;
021900111002            endif;
022000111002
022100111002            open(e) paycrosl01;
022200111002            open(e) paycrosl03;
022300111002
022400111002          endsr;
022500090415       //*************************************************
022600090415       // First initilisation routine for all programs  **
022700090415       //*************************************************
022800090415       BEGSR AAINIT;
022900090415         coname = get_coname;
023000090415         @date = get_sys_date;
023100090415      /END-FREE
023200010409     C                   time                    @TIME             6 0
023300090415      // select header for program
023400090415      /FREE
023500090415         @title = arr(1);
023600090415         // setoff all indicators
023700090415         *in = *off;
023800090415         //
023900090415       endsr;
024000090415       //*************************************************
024100090415       // Update Transactions
024200090415       //*************************************************
024300090415       begsr update_rtn;
024400090415         //
024500090415         if new_record;
024600090415           pcrssts = 'A';
024700090415           pcrsptype = ptyptype#;
024800090415           pcrstype = emseltyp#;
024900090415           pcrsstype = emsubtyp#;
025000090415           write pcrsfmt;
025100090415           //
025200090415         else;
025300090415           //
025400090415           select;
025500090415           when delete_confrm;
025600090415             delete pcrsfmt;
025700090415           other;
025800090415             pcrstype = emseltyp#;
025900090415             pcrsstype = emsubtyp#;
026000090415             update pcrsfmt;
026100090415           endsl;
026200090415         endif;
026300090415         //
026400090415       endsr;
026500090415       //
026600090415      /END-FREE
026700010301**  Program Title
026800010412      Payroll/Liability Cross Ref. Maintenance
