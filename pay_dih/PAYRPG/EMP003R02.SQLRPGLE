000100070308     H
000200111013     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'CCREDITDIR')
000300111013     H/define profoundui
000400111013     Femp003d02 CF   E             WORKSTN
000500111013     F/IF DEFINED(WEB)
000600111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000700111013     F/ENDIF
000800111013
000900070313     D empbkds       E DS                  extname(empbkln)
001000070308     DFOUND            S               N
001100070308     D ARR             s             40    dim(1) ctdata perrcd(1)
001200070319     DDateErr01        C                   'Incorrect Start or End Date Entered'
001300070319     DDateErr02        C                   'Incorrect Start Date Entered'
001400070319     D
001500070319     D
001600070319     Derr#             s              7
001700111013
001800070308     D                SDS
001900070308     D  @PGM             *PROC
002000111013      /copy QCPYSRC,genpr
002100111013      /copy QCPYSRC,PAYPR
002200070308      /free
002300070308
002400141117        empno = 0;
002500141117        dow 0 = 0;
002600070308         exsr aainit;
002700070308          exfmt fmt1;
002800070308           if *in03;
002900070308             *inlr= *on;
003000070308               return;
003100070308           ENDIF;
003200070313
003300070313             get_emp_info(empno:*omit:*blanks:
003400141117                 empname:*omit:*omit:*omit:err#);
003500070313              if err# <> ' ';
003600141117                 show_msg(get_err_msg(err#));
003700070313                 iter;
003800070313               ENDIF;
003801141117
003900070308           exec sql
004000070312            SELECT * into :empbkDS FROM empbkln WHERE EMPLOYEE = :empno
004100070312              and bklsts = 'A';
004200070308
004300070308             IF SQLSTT = '00000';
004400070308               FOUND = '1';
004500070313
004600070319               *in35 = *on;
004700070313              strdate = bklsdate;
004800070313              cmpldate = bkledate;
004900070308               ELSE;
005000070308               FOUND = '0';
005100070313              strdate = 0;
005200070313              cmpldate = 0;
005300070313
005400070319               *in35 = *off;
005500070308             ENDIF;
005600070313             leave;
005700070313             ENDdo;
005800070313
005900070308            DOW 0 = 0;
006000070308              if found;
006100070308                mode = 'AMEND';
006200070319
006300070308                else;
006400070308                mode = 'ENTRY';
006500070319
006600070308              ENDIF;
006700070308            exfmt fmt2;
006800070319
006900070313            //handling cancel and program end options
007000070313
007100070313            if *in02;
007200070313              leave;
007300070313            ENDIF;
007301141117
007400070313            if *in03;
007500070313              *inlr = *on;
007600070313              return;
007700070313            ENDIF;
007701141117
007800070319             test(de) *iso strdate;
007900070319            errmsg = DateErr02;
007901141117
008000070319            if %error;
008100141117              show_msg(errmsg);
008200070319              iter;
008300070319            endif;
008301141117
008400070312            if *in20;
008401141117
008402141117            //Display error if mode is new entry
008403141117            IF NOT found;
008404141117               show_msg('Option not allowed for ENTRY mode.');
008405141117               iter;
008406141117            EndIf;
008500070312
008600070319            test(de) *iso cmpldate;
008700070312            if %error;
008800070319             show_msg(errmsg);
008900070312              iter;
009000070312            endif;
009001141117
009100070319            if strdate > cmpldate;
009200070319            errmsg = DateErr01;
009300070319             show_msg(errmsg);
009400070319             dsply  DateErr01;
009500070319              iter;
009600070319            ENDIF;
009601141117
009700070312              exec sql
009800070312             UPDATE empbkln SET
009900070313              BKLSTS = 'C',END_DATE =:cmpldate
010000070313               WHERE EMPLOYEE = :empno AND bklsts = 'A';
010100070313               COMMIT;
010200070312               leave;
010300070312
010400070313            endif;
010500070308
010600070308            if *in15;
010700070313            IF found;
010900070308
011000070308              exec sql
011100070312             UPDATE empbkln SET
011200070312              EMPLOYEE = :empno, START_DATE =:strdate,END_DATE = 0
011300070313               WHERE EMPLOYEE = :empno and bklsts = 'A';
011400070308
011500070313               COMMIT;
011501141117
011600070308            ELSE;
011601141117
011700070319            test(de) *iso strdate;
011800070313            if %error or cmpldate <> 0;
011900070319
011901141117               if err# <> ' ';
011902141117                  show_msg(get_err_msg(err#));
011904141117               ENDIF;
011905141117
011906141117               if cmpldate <> 0;
011907141117                  show_msg('Complete date not allowed for new entry.');
011908141117               EndIf;
011909141117
012100070312              iter;
012101141117
012200070312            endif;
012201141117
012300070308             exec sql
012400070313             INSERT INTO empbkln (bklsts,EMPLOYEE,START_DATE,END_DATE)
012500070313             VALUES('A',:empno,:strdate,:cmpldate);
012600070313               COMMIT;
012601141117
012700070308            ENDIF;
012800070308
012900070308             LEAVE;
013000070308             ELSE;
013100070308            ITER;
013200070308            endif;
013300070308          ENDDO;
013400070308           //***************************************************
013500070308       //** First initilisation routine for all programs  **
013600070308       //***************************************************
013700070308              begsr aainit;
013800070308                coname = get_coname;
013900070308                  @date = get_sys_date;
014000070308                   @title = arr(1);
014100070308             endsr;
014200070308      /end-free
014300070308**
014400070308    Employee Bank Loan Information
