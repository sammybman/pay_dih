000100950419     H DECEDIT('.') DATEDIT(*DMY/)
000200090415      //*
000300090415      //* CHANGING PAYROLL DEDUCTIONS
000400090415      //* PROGRAMMER--------S.BRITTON ,C.WILLIAMS
000500090415      //*
000600010628     Fpaysetl01 ip   e           k disk
000700980417     Fpaydedchg if   e           k disk
000800010418     Fpaycrosl02if   e           k disk
000900011010     Fpaytimfl01uf a e           k disk    commit
001000020607     Fpayrdefl03if   e           K DISK
001100020607     Fpayrresl01if   e           K DISK
001200021005     Fpaycycd   if   e           K DISK
001300100902
001400090415      // Payroll PROTOTYPES
001500150324      /copy qcpysrc,genPR
001600150324      /copy qcpysrc,PAYPR
001700150324      /copy qcpysrc,empPR
001800090415      //
001900010418     D type            s              2  0
002000020607     D date1           s              8  0
002100020607     D prd             s              6  0
002200021005     Drun_type         s              1
002300021005     Drun_qty          s              2  0
002400021005     Drun_cycle        s              2
002500021005     Dpayrun_factor    s              7  3
002600100819     D curbal          s             11  2                                      TOTAL AMOUNT
002700100902     D errorcode       S              7
002800090415      //
002900010502     Ipstfmt
003000010502     I                                          psttype       l1
003100090415      //
003200100819
003300090415      //
003400980417     C     DedChgKey     klist
003500010502     C                   kfld                    psttype
003600980417     C                   kfld                    pstemp
003700090415      //
003800100901      //
003900100819
004000090415      /FREE
004100090415       exsr SelTrnTyp;
004200090415       if pstcyc = *blanks;
004300090415         pstcyc = '*M';
004400090415       endif;
004500021005
004600090415       chain pstcyc cycfmt;
004700090415       pstamt = pstamt / payrun_factor /
004800100901                 run_qty * cycyearf;
004900100901       chain (run_type:type) prdffmt;
005000020607
005100090415       if %found and
005200100902              %scan(get_emp_info(pstemp:*omit:*blanks:*omit:
005300100902                     *omit:*omit:*omit:errorcode):prrelist) > 0
005400100902                         and errorcode = *blanks
005500100902                            and not(*in88);
005600020607
005700100901         chain (run_type:type:pstemp) timfmt;
005800100901
005900100831         if not %found(paytimfl01);
006000100831            chain DedChgKey pcgfmt;
006100100819
006200100831            if %found;
006300100831                pcgamt = pcgamt / payrun_factor /
006400100819                         run_qty * cycyearf;
006500100831                timamt = round_up(pcgamt:'U');
006600100831               else;
006700100831                timamt = round_up(pstamt:'U');
006800100831            endif;
006900100819
007000100831            timemp  = pstemp;
007100100831            timcode = 'D';
007200100831            timtyp  = type;
007300100831            timrtype = run_type;
007400100831            exsr Setprior;
007500100819
007600100831            exsr getbal;
007700100819
007800100831            write timfmt;
007900100831          else;
008000100831            chain DedChgKey pcgfmt;
008100100824
008200100831            if %found;
008300100831                pcgamt = pcgamt / payrun_factor /
008400100824                         run_qty * cycyearf;
008500100831                timamt += round_up(pcgamt:'U');
008600100831               else;
008700100831                timamt += round_up(pstamt:'U');
008800100831            endif;
008900100831            update timfmt;
009000100831         endif;
009100100824
009200100824       endif;
009300100819
009400090415      /END-FREE
009500010502     Cl1                 exsr      AddMissChg
009600100819
009700960612
009800090415      //-------------------------------------------------------------------**
009900090415      //* GET MISSING TRANSACTIONS
010000090415      //-------------------------------------------------------------------**
010100090415      /FREE
010200090415       begsr AddMissChg;
010300100819         setll psttype pcgfmt;
010400100819         reade psttype pcgfmt;
010500100819
010600100819         if not(%eof(paydedchg));
010700021005
010800100901           chain (run_type:type) prdffmt;
010900100901
011000100820           if %found
011100090415                 and not(*in88);
011200020607
011300100819             dow not(%eof(paydedchg));
011400100902               if  %scan(get_emp_info(pcgemp:*omit:*blanks:*omit:
011500100902                     *omit:*omit:*omit:errorcode):prrelist) > 0
011600100902                         and errorcode = *blanks;
011700100901                 chain (run_type:type:pcgemp) timfmt;
011800100819
011900100819                 if not(%found);
012000100819                   timamt = round_up(pcgamt:'U');
012100100819                   timemp  = pcgemp;
012200100819                   timcode = 'D';
012300100819                   timtyp  = type;
012400100819                   timrtype = run_type;
012500100819                   exsr Setprior;
012600100819                   exsr getbal;
012700100901                   write timfmt;
012800100819                 endif;
012900100819               endif;
013000100819
013100090415               reade psttype pcgfmt;
013200090415             enddo;
013300090415           endif;
013400090415         endif;
013500100817
013600090415       endsr;
013700090415       //**********************************************************
013800090415       //* GET CURRENT BALANCE ***
013900090415       //**********************************************************
014000100819
014100090415       begsr GetBal;
014200100817
014300100819         get_emplimbal(*zeros:psttype:
014400100819               *zeros:timemp:*zeros:prd:curbal);
014500100817
014600100819         IF timamt > curbal;
014700100819           IF curbal >= *zeros;
014800100819             timamt = round_up(curbal:'U');
014900100819            else;
015000100819             timamt = *zeros;
015100090415           endif;
015200090415         endif;
015300090415       endsr;
015400100817
015500090415       //-------------------------------------------------------------------**
015600090415       //* SELECT TRANSACTION TYPE
015700090415       //-------------------------------------------------------------------**
015800090415       begsr SelTrnTyp;
015900100819
016000090415         chain psttype pcrsfmt;
016100090415         if %found;
016200090415           type = pcrsptype;
016300090415           *in88 = *off;
016400090415         else;
016500090415           *in88 = *on;
016600090415         endif;
016700090415       endsr;
016800090415       //-------------------------------------------------------------------**
016900090415       //* Set transaction Priority
017000090415       //-------------------------------------------------------------------**
017100100819
017200090415       begsr setprior;
017300090415
017400050519          if not  get_payty_info('RD':type:payty_struc);
017500100820            timprior = ptyprior;
017600100820          endif;
017700090415       endsr;
017800100819
017900090415       BEGSR *inzsr;
018000100819
018100090415      /END-FREE
018200020607     C     *entry        plist
018300021005     C                   parm                    run_type
018400021005     C                   parm                    run_qty
018500021005     C                   parm                    run_cycle
018600020607     C
018700100819         // get period  dates
018800090415      /FREE
018900090415         get_sys_date(date1);
019000090415         get_prd_info('*prd':*blanks:prd:
019100090415             date1);
019200090415         // Set up correct period
019300020607
019400090415         chain run_type prrefmt;
019500090415         // get Run-type cycle
019600090415         chain run_cycle cycfmt;
019700090415         payrun_factor = cycyearf;
019800100901
019900090415       ENDSR;
020000090415      /END-FREE
