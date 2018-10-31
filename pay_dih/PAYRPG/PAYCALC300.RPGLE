000100950419     H DECEDIT('.') DATEDIT(*DMY/)
000200090415      //*
000300090415      //* CHANGING PAYROLL DEDUCTIONS
000400090415      //* PROGRAMMER--------T.I.BYNOE  ADJ. S.BRITTON
000500090415      //*
000600090415      //  EMOLUMENTS,DEDUCTIONS TYPE FILE
000700980417     Fpaydedchg if   e           k disk
000800010418     Fpaycrosl02if   e           k disk
000900011010     Fpaytimfl01uf a e           k disk    commit
001000020607     Fpayrdefl03if   e           K DISK
001100020607     Fpayrresl01if   e           K DISK
001200090415      // Liability Master File
001300980414     Fliabm     if   e           k disk
001400090415      //
001500090415      // Payroll PROTOTYPES
001600150324      /copy qcpysrc,genPR
001700150324      /copy qcpysrc,PAYPR
001800150324      /copy qcpysrc,empPR
001900090415      //
002000980414     D la              s             11  2 dim(12)
002100020607     D date1           s              8  0
002200020607     D prd             s              6  0
002300020607     D mth             s              2  0
002400050513     D countr          s              5  0
002500100902     D errorcode       S              7
002600090415      //
002700980414     Ilifmt
002800980414     I              liamt01                     la(01)
002900980414     I              liamt02                     la(02)
003000980414     I              liamt03                     la(03)
003100980414     I              liamt04                     la(04)
003200980414     I              liamt05                     la(05)
003300980414     I              liamt06                     la(06)
003400980414     I              liamt07                     la(07)
003500980414     I              liamt08                     la(08)
003600980414     I              liamt09                     la(09)
003700980414     I              liamt10                     la(10)
003800980414     I              liamt11                     la(11)
003900980414     I              liamt12                     la(12)
004000010502
004100090415      //
004200980414     C     trn_key       klist
004300990303     C                   kfld                    emseltyp
004400980414     C                   kfld                    styp              3 0
004500010619     C                   kfld                    pcgemp
004600100723
004700100723     C     Chk_key       klist
004800100723     C                   kfld                    run_type
004900100723     C                   kfld                    emseltyp
005000100723     C                   kfld                    pcgemp
005100090415      //
005200090415      //
005300020607     C     val_key       klist
005400020607     C                   kfld                    run_type
005500020607     C                   kfld                    timtyp
005600090415      //
005700050513      /free
005800050513           countr = *zeros;
005900050513           dow not get_ltype_info('RD':-1:ematf_struc:countr);
006000050513
006100050513              if   emautpyd = 'Y';
006200050513                exsr   AddPayDed;
006300050513              endif;
006400050513
006500050513           Enddo;
006600050513           *inlr = *on;
006700960612
006800090415        //-----------------------------------------------------------------**
006900090415        //* ADD PAYROLL DEDUCTIONS
007000090415        //-----------------------------------------------------------------**
007100090415       begsr AddPayDed;
007200100818         setll emseltyp pcgfmt;
007300100818         reade emseltyp pcgfmt;
007400100818
007500100818         dow not(%eof(paydedchg));
007600090415           exsr SelTrnTyp;
007700090415           chain val_key prdffmt;
007800020607
007900090415           if %found and
008000100902                 %scan(get_emp_info(pcgemp:*omit:*blanks:*omit:
008100100902                     *omit:*omit:*omit:errorcode):prrelist) > 0
008200100902                         and errorcode = *blanks
008300100902                          and not(*in88);
008400090415
008500090415             // Write Info to work file
008600050519            get_payty_info('RD':timtyp:payty_struc);
008700050519
008800100723               chain chk_key timfmt;
008900100723               if not %found(paytimfl01);
009000100723                 timamt = round_up(pcgamt:'U');
009100100723                 timemp = pcgemp;
009200100723                 timcode = 'D';
009300100723                 timrtype = run_type;
009400100723                 timprior = ptyprior;
009500100723                 exsr getbal;
009600100723               write timfmt;
009700100723            else;
009800100818                 timamt += round_up(pcgamt:'U');
009900100723                 exsr getbal;
010000100723               update timfmt;
010100100723           endif;
010200100723           endif;
010300090415           reade emseltyp pcgfmt;
010400090415         enddo;
010500090415         //
010600090415       endsr;
010700090415
010800090415       //**********************************************************
010900090415       //* GET CURRENT BALANCE ***
011000090415       //**********************************************************
011100090415       //
011200090415       begsr GetBal;
011300090415         //
011400090415         chain trn_key lifmt;
011500090415         *IN99 = NOT %FOUND;
011600090415         //
011700090415         if timamt > la(mth);
011800090415           if la(mth) >= *zeros;
011900090415             timamt = round_up(la(mth):'U');
012000090415           else;
012100090415             timamt = *zeros;
012200090415           endif;
012300090415         endif;
012400090415       endsr;
012500090415       //-------------------------------------------------------------------**
012600090415       //* SELECT TRANSACTION TYPE
012700090415       //-------------------------------------------------------------------**
012800090415       begsr SelTrnTyp;
012900090415         //
013000090415         chain emseltyp pcrsfmt;
013100090415         if %found;
013200090415           timtyp = pcrsptype;
013300090415           *in88 = *off;
013400090415         else;
013500090415           *in88 = *on;
013600090415         endif;
013700090415         //
013800090415       endsr;
013900090415       //
014000090415       BEGSR *INZSR;
014100090415         //
014200090415      /END-FREE
014300020607     C     *entry        plist
014400020607     C                   parm                    run_type          1
014500020607     C
014600090415      // get period  dates
014700090415      /FREE
014800090415         get_sys_date(date1);
014900090415         get_prd_info('*prd':*blanks:prd:
015000090415             date1);
015100090415         // Set up correct period
015200090415      /END-FREE
015300020607     C                   move      prd           mth
015400020607
015500090415      /FREE
015600090415         chain run_type prrefmt;
015700090415       ENDSR;
015800090415      /END-FREE
