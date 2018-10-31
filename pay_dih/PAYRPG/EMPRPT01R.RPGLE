000100950821     H DECEDIT('.') DATEDIT(*DMY/)
000200091121     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'CCREDITDIR')
000300111013     H/define profoundui
000400950821     F**
000500980709     F** program to display loans query
000600990301     F** done by Leslie Doodnauth
000700950821     F**
000800020213     Femprpt01d cf   e             workstn
000900111013     F/IF DEFINED(WEB)
001000111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001100111013     F/ENDIF
001200111013
001300111013      /copy QCPYSRC,genpr
001400111013      /copy QCPYSRC,autpr
001500980709     **
001600020213     D ARR             S             40    dim(2) CTDATA PERRCD(1)
001700050727     D qry_select      s            200
001800050727     D sel#            s              1  0 inz(1)
001900050727
002000050727     D ACTIVECODE      c                   'A'
002100050727     D COMP            c                   'Report completed as requested'
002200050727
002300050727     Dgen_report       PR                  extproc('EMPRPT01C')
002400050727     d qry                          200
002500050727     d sel                            1  0
002600050727     d str_date                       8  0
002700050727     d end_date                       8  0
002800050727     d
002900020213     **
003000980709     D                SDS
003100980709     D  @PGM             *PROC
003200111013     D  @USER                358    367
003300050727      /free
003400111013         // authority check
003500111013         monitor;
003600111013           authority_check('EMT':@pgm);
003700111013         on-error;
003800111013           show_msg(get_err_msg('DIH0054'));
003900111013           *inlr = *on;
004000111013           return;
004100111013         endmon;
004200050727            exsr  AAinit;
004300050727
004400050727            dow  0 = 0;
004500050727
004600050727               exfmt  fmt1;
004700050727
004800050727               *in40 = *off;
004900050727               *in41 = *off;
005000050727               *in50 = *off;
005100050727               *in60 = *off;
005200050727               *in70 = *off;
005300050727
005400050727               //in03 = F3-end
005500050727                if  *in03;
005600050727                  *inlr = *on;
005700050727                  return;
005800050727                endif;
005900990422
006000050727               //If period is entered , skip date entry end validation
006100050727                if  prd# = *zeros;
006200050727
006300050727                  //Checking if date equal to blank
006400050727
006500050727                  if date# = *zeros;
006600050727                     *in50 = *on;
006700050727                     iter;
006800050727                  endif;
006900050727
007000050727                  //Checking for valid start date
007100050727
007200050727                  test(de) date#;
007300050727                  if  %error;
007400050727                     *in60 = *on;
007500050727                     iter;
007600050727                  endif;
007700050727
007800050727                    //Checking for valid end date
007900050727
008000050727                  if date1# = *zeros;
008100050727                     *in70 = *on;
008200050727                     iter;
008300050727                  endif;
008400050727
008500050727                  test(de) date1#;
008600050727                  if  %error;
008700050727                     *in41 = *on;
008800050727                     iter;
008900050727                  endif;
009000050727
009100050727             //checking to ensure end date is not less than start date
009200990422
009300050727                  if date1# < date#;
009400050727                     *in40 = *on;
009500050727                     iter;
009600050727                  endif;
009700050727                 else;
009800050816                //get_prd_info('*rng':' ':prd#:date#:date1#);
009900050727                endif;
010000050727             leave;
010100050727            enddo;
010200050727
010300050727            qry_select = *blanks;
010400050727
010500050727         // Active Records only
010600050727              qry_select =
010700050727                            '  jrsts = ' + ACTIVECODE;
010800050727         // start and end dates
010900050727             if date1# <> *zeros;
011000050727                exsr add_and_sr;
011100050727                qry_select = 'jrdate *ge ' +
011200050727                    %char(date#) +
011300050727                   ' & jrdate *le ' +
011400050727                    %char(date1#);
011500050727               else;
011600050727                exsr add_and_sr;
011700050727                qry_select = 'jrprno *eq ' +
011800050727                    %char(prd#);
011900050727             endif;
012000050727
012100050727           //**********************************
012200050727           //** Setting up Query Selections  **
012300050727           //**********************************
012400050727            // If selection using category
012500050727                //if cat# <> 0;
012600050727                //   exsr add_and_sr;
012700050727                //   qry_select = %trim(qry_select) +
012800050727                //          '  astcate = ' + %char(cat#);
012900050727                // endif;
013000050727
013100050727
013200050727                gen_report(qry_select:sel#:date#:date1#);
013300050727
013400050727
013500050727                errmsg = comp;
013600111013
013700111013         //*****************************
013800111013         // Session Init
013900111013         //*****************************
014000111013
014100111013          begsr *inzsr;
014200111013            get_session_info('EMT':errmsg:@pgm);
014300111013            if errmsg <> *blanks;
014400111013              show_msg(errmsg);
014500111013              *inlr = *on;
014600111013              return;
014700111013            endif;
014800111013
014900111013          endsr;
015000111013
015100050727
015200050727           //***************************************************
015300050727           //** Add 'AND' to query selection                  **
015400050727           //****************************************************
015500050727              begsr add_and_sr;
015600050727                 if qry_select <> *blanks;
015700050727                   qry_select = %trim(qry_select) + ' & ';
015800050727                 endif;
015900050727              endsr;
016000050727
016100050727         //*********************************************
016200050727         //** Begin subroutine to print header
016300050727         //*********************************************
016400050727              begsr aainit;
016500050727
016600050727                 coname = get_coname;
016700050727                 @date = get_sys_date;
016800050727                 @title = arr(1);
016900020213
017000050727              endsr;
017100050727      /end-free
017200980709**
017300050727      Employee Liability Transactions
017400980709   DDDDDDDDDDDDD
