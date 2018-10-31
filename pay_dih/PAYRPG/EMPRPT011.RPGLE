000100950821     H DECEDIT('.') DATEDIT(*DMY/)
000200950821     F**
000300050727     F** program to display employee liab. transactions
000400050727     F** done by S. Britton
000500950821     F**
000600050727     Fjrnm      if   e           k disk
000700050727     Fqsysprt   o    f  132        printer oflind(overflow) usropn
000800050727
000900150615      /copy qcpysrc,genpr
001000050727     D*--------------------------------------
001100050727     D* Prototype for procedure: PrintReport
001200050727     D*--------------------------------------
001300050727     D PrintReport     PR
001400050727
001500050727
001600050727     D Main_pgm        PR                  extproc('EMPRPT011')
001700050727     d                                8  0 const
001800050727     d                                8  0 const
001900050727     D Main_pgm        PI
002000050727     d str_date                       8  0 const
002100050727     d end_date                       8  0 const
002200980709     **
002300050727     D @date           S             10
002400050727     D @title          S             40
002500050727     D coname          S             30
002600020213     D ARR             S             40    dim(2) CTDATA PERRCD(1)
002700050727     D text            S             60
002800050727     D lasttrn#        S                   like(jrnumb)
002900050727
003000020213     **
003100980709     D                SDS
003200980709     D  @PGM             *PROC
003300980709     D  @USER                254    263
003400050727      /free
003500050727            exsr  AAinit;
003600050727
003700050727           // Print data
003800050727              PrintReport();
003900050727
004000050727         //*********************************************
004100050727         //** Begin subroutine to print header
004200050727         //*********************************************
004300050727              begsr aainit;
004400050727
004500050727                 coname = get_coname;
004600050727                 @date = get_sys_date;
004700050727                 @title = arr(1);
004800020213
004900050727              endsr;
005000050727      /end-free
005100050727
005200050727     ********************************************************
005300050727     Oqsysprt   e            hedr           1 01
005400050727     O                       @pgm                10
005500050727     O                       coname              77
005600050727     O                       @date              130
005700050727
005800050727     O          e            hedr           2
005900050727     O                       @user               10
006000050727     O                       @title              85
006100050727     O                                          126 'PAGE -'
006200050727     O                       PAGE          Z    130
006300050727
006400050727     O          e            hedr           2
006500050727     O                                            9 'Period ->'
006600050727     O                       str_date            21 '    /  /  '
006700050727     O                                         +  1 '-'
006800050727     O                       end_date            34 '    /  /  '
006900050727
007000050727
007100050727     O          e            rep_hedr       2
007200050727     O                                           10 'DATE'
007300050727     O                                           23 'TRANSACTION'
007400050727     O                                           33 'EMPLOYEE'
007500050727     O                                           46 'DESCRIPTION'
007600050727     O                                          110 'AMOUNT'
007700050727
007800050727     O          e            rep_hedr       2
007900050727     o                                           16 'NUMBER'
008000050727     O                                           29 'NUMBER'
008100050727
008200050727     ** from jrnm
008300050727     O          e            detail         1
008400050727     O                       jrdate              10 '    /  /  '
008500050727     O                       jrnumb        z     23
008600050727     O                       jremp         z     33
008700050727     O                       text           b    95
008800050727     O                       jramt         a    110
008900050727
009000050727     ** print blank line when transaction changes
009100050727     O          e            blankrec       1
009200050727     O***                    total_txt           20
009300050727
009400050727
009500050727     ** Final report totals
009600050727     O          e            fintot         2
009700050727     O                                           75 '** END OF REPORT **'
009800050727
009900050727
010000050727     P*--------------------------------------
010100050727     P* Procedure name: PrintReport
010200050727     P* Purpose:
010300050727     P*--------------------------------------
010400050727     P PrintReport     B
010500050727     D PrintReport     PI
010600050727      /free
010700050727
010800050727            open(e) qsysprt;
010900050727
011000050727
011100050727              // read Liability Transaction file
011200050727
011300050727                read jrfmt;
011400050727
011500050727                lasttrn# = jrnumb;
011600050727
011700050727
011800050727                except hedr;
011900050727                except rep_hedr;
012000050727
012100050727                dow not %eof(jrnm);
012200050727
012300050727
012400050727                if overflow;
012500050727                  except hedr;
012600050727                  except rep_hedr;
012700050727                  overflow = *off;
012800050727                endif;
012900050727
013000050727                if jrnumb <> lasttrn#;
013100050727                   except blankrec;
013200050727                   lasttrn# = jrnumb;
013300050727                endif;
013400050727
013500050727                text = %trim(jrtext) + ' ' + %triml(jrtext2);
013600050727
013700050727                except detail;
013800050727
013900050727              read jrfmt;
014000050727
014100050727            enddo;
014200050727
014300050727            *inlr = *on;
014400050727            except fintot;
014500050727
014600050727            close qsysprt;
014700050727      /end-free
014800050727
014900050727     P PrintReport     E
015000050727
015100980709**
015200050727      Employee Liability Transactions
015300980709   DDDDDDDDDDDDD
