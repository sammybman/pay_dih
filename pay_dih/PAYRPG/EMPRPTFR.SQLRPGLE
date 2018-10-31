000100110920     H DATEDIT(*DMY) bnddir('DISDIR') dftactgrp(*no)
000200110920     H/define profoundui
000300000313     F**********************************************************
000400010607     F*                   DISERV  SYSTEM                       *
000500111013     F*                    Liability  Control                  *
000600980616     F*                                                        *
000700980616     F*                         BY                             *
000800980616     F*                                                        *
000900030310     F*             Diserv Systems                             *
001000000313     F*
001100000313     F*                                                        *
001200980616     F*                                                        *
001300111013     F*       RPG PROGRAM NAME..: EMPRPTFR                     *
001400980616     F*                                                        *
001500010607     F*       PROGRAMMER NAME: Samuel Britton                  *
001600010607     F*                        Christopher Williams            *
001700110920     F*       DESIGN DATE....: 2011/09/21                      *
001800110920     F*       MODULES........:                                 *
001900980917     F*       INDICATORS.....:                                 *
002000980917     F*                                                        *
002100980616     F*                                                        *
002200010716     F*  THIS PROGRAM IS DESIGNED  to configure  system        *
002300010716     F*                            parameters                  *
002400980616     F*                                                        *
002500980616     F**********************************************************
002600111013     Femprptsd  cf   e             workstn
002700110920     F/IF DEFINED(WEB)
002800110920     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002900110920     F/ENDIF
003000010607     **
003100110920      /copy QCPYSRC,genPR
003200110920      /copy QCPYSRC,autPR
003300111013      /copy QCPYSRC,empPR
003400020226     **
003500110920     D arr             s             40    DIM(2) CTDATA PERRCD(1)
003600111013     D prno            s              6  0
003700111013     D date#           s              8  0
003800111013     D upd_prno        s              6  0
003900111013     D type            s              2  0
004000111013     D cmd_text        s            200
004100110920     D errmsg          s             75
004200111013
004300111013     D msg1            c                   'Cannot run Period end close now'
004400111013
004500111013     D strcmt_cmd      c                   'STRCMTCTL LCKLVL(*CHG) +
004600111013     D                                           CMTSCOPE(*JOB)'
004700111013
004800111013     D Line_command    PR                  extpgm('QCMDEXC')
004900111013     D  command                     220    const
005000111013     D  length                       15  5 const
005100110920
005200111013     D liab_int_calc   pr                  extpgm('EMPFRPT01')
005300111013     D period                         6  0 const
005400111013
005500111013     D allow_reset     pr                  extpgm('EMPFRPT02')
005600111013     D period                         6  0 const
005700111013
005800111013     D pay_ded_setup   pr                  extpgm('EMPFRPT03')
005900111013     D period                         6  0 const
006000111013     D flag                           1    const
006100111013
006200111013     D Saving_upd      pr                  extpgm('EMPFRPT04')
006300111013     D period                         6  0 const
006400111013
006500111013     D Saving_upd2     pr                  extpgm('EMPFRPT05')
006600111013     D period                         6  0 const
006700111013
006800111013     D Saving_upd3     pr                  extpgm('EMPFRPT06')
006900111013     D period                         6  0 const
007000111013
007100111013     D Saving_upd4     pr                  extpgm('EMPFRPT07')
007200111013     D period                         6  0 const
007300111013     D flag                           1    const
007400111013
007500111013     D share_scheme    pr                  extpgm('EMPFRPT08')
007600111013     D period                         6  0 const
007700111013
007800111013     D Emp_liab_upd    pr                  extpgm('EMPFRPTFIN')
007900111013     D period                         6  0 const
008000111013
008100110920
008200980319     D                sds
008300980917     D  @pgm             *proc
008400110920     D  @user                358    367
008500980603     **
008600111116
008700020226     *****************************************************************
008800111013     ** checking authority to access reports
008900020226     *****************************************************************
009000040207      /free
009100110921
009200110921          // authority check
009300110921         monitor;
009400111013            authority_check('EMT':@pgm);
009500110921          on-error;
009600110921            show_msg(get_err_msg('DIH0054'));
009700110921            *inlr = *on;
009800110921            return;
009900110921         endmon;
010000110921
010100110920
010200110920         exsr  aainit;
010300111013
010400111013         exec sql
010500111019            select esysupdprd into :upd_prno from empcon;
010600111013
010700111013         upd_prno = get_new_prd(upd_prno:1:'A');
010800111013
010900111013         if  prno <= upd_prno;
011000111013            show_msg(msg1);
011100111013            *inlr = *on;
011200111013            return;
011300111013         endif;
011400111013
011500110920         //*******************
011600110920         //** Program Start
011700110920         //*******************
011800111013            exfmt  fmt2;
011900110921
012000110920            // end of program
012100110920            if  *in03;
012200110920               *inlr = *on;
012300110920               return;
012400110920            endif;
012500110921
012600111013            // select liability Types
012700111013            if  ans <> 'YES' or not (*in21);
012800111013               ans = 'NO ';
012900111013               *inlr = *on;
013000111013               return;
013100111013            endif;
013200110920
013300111013            monitor;
013400111013
013500111013            errmsg = 'clearing payroll changes table';
013600111116            send_message(errmsg);
013700111013            // clear payroll deduction changes
013800111013            cmd_text = 'CLRPFM FILE(PAYDEDCHG)';
013900111013            callp(e) line_command(cmd_text:%size(cmd_text));
014000111013
014100111013            errmsg = 'performing Interest Calcs.';
014200111116            send_message(errmsg);
014300111013            liab_int_calc(upd_prno);
014400111013            allow_reset(upd_prno);
014500111013
014600111013            // clear payroll deduction setup file
014700111013            errmsg = 'clearing Deduction setup table';
014800111116            send_message(errmsg);
014900111013            cmd_text = 'CLRPFM FILE(PAYSETUP)';
015000111013            callp(e) line_command(cmd_text:%size(cmd_text));
015100111013
015200111013            errmsg = 'populating Deduction setup table';
015300111116            send_message(errmsg);
015400111013            pay_ded_setup(upd_prno:'U');
015500111013
015600111013            // Savings update
015700111013            errmsg = 'performing savings updates';
015800111116            send_message(errmsg);
015900111013            saving_upd(upd_prno);
016000111013            saving_upd2(upd_prno);
016100111013            saving_upd3(upd_prno);
016200111013
016300111013            // copy savings file
016400111013            errmsg = 'making copy of savings master';
016500111116            send_message(errmsg);
016600111013            cmd_text = 'CPYF FROMFILE(SAVMAST) TOFILE(PYSAVMAST) ' +
016700111013                       'MBROPT(*REPLACE) CRTFILE(*YES)';
016800111013            callp(e) line_command(cmd_text:%size(cmd_text));
016900111013
017000111013            errmsg = 'updating savings master';
017100111116            send_message(errmsg);
017200111013            saving_upd4(upd_prno:'A');
017300111013
017400111013            // Share Scheme Master Update
017500111013            errmsg = 'share scheme maintenance';
017600111116            send_message(errmsg);
017700111013            share_scheme(upd_prno);
017800111013
017900111013            // Employee Liability control file Update
018000111013            errmsg = 'performing control file update';
018100111116            send_message(errmsg);
018200111013            emp_liab_upd(upd_prno);
018300111013
018400111013            on-error;
018500111013              show_msg('Error found while ' + %trim(errmsg));
018600111013              rolbk;
018700111013              *inlr = *on;
018800111013              return;
018900111013            endmon;
019000111013
019100111013            commit;
019200110920
019300111013            *inlr = *on;
019400111013            return;
019500110920
019600110920       //*****************************
019700110920       // Session Init
019800110920       //*****************************
019900110920
020000110920        begsr *inzsr;
020100111013          get_session_info('EMT':errmsg:@pgm);
020200110920          if errmsg <> *blanks;
020300110920            show_msg(errmsg);
020400110920            *inlr = *on;
020500110920            return;
020600110920          endif;
020700110920
020800110920        endsr;
020900110920
021000110920
021100110920        //*****************************************************
021200110920        //** Begin subroutine to print header
021300110920        //*****************************************************
021400110920        begsr  AAINIT;
021500111013           CONAME = get_coname;
021600111013           @DATE = get_sys_date(date#);
021700111013           get_prd_info('*prd':*blanks:prno:date#);
021800111013           @title = arr(1);
021900110920        endsr;
022000110920
022100110920      /end-free
022200980917     **
022300980917**   Header to be displayed
022400111013      Employee Liability Period Update
