000100010424     H DATEDIT(*ymd)
000200021115     F* PAYROLL GL. Definition Report
000300010424     F* PROGRAMMER:-S. Britton & C. Williams
000400021115     Fdeptm     if   e           k disk
000500021115     Fpaytdefl02if   e           k disk
000600021120     Fpayrtypl01if   e           k disk
000700021120     Fqsysprt   o    f  132        printer oflind(*inof)
000800010424     ** Payroll PROTOTYPES
000900021115     **copy *libl/payrpg,PAYPR
001000110729      /copy qcpysrc,genpr
001100110729      /copy qcpysrc,glpr
001200110729      /copy qcpysrc,emppr
001300990113     **
001400960130     D                SDS
001500960130     D  @PGM             *PROC
001600960130     D  @USER                254    263
001700010424     **
001800021115     D arr             s             40    dim(1) ctdata perrcd(1)
001900021115     D coname          s             30                                         TOTAL AMOUNT
002000021120     D @title          s             40                                         TOTAL AMOUNT
002100021115     D @date           s             10                                         TOTAL AMOUNT
002200021120     D gldesc          s             30                                         TOTAL AMOUNT
002300021115     D gl#             s             25                                         TOTAL AMOUNT
002400021115     D dft#            s             10                                         TOTAL AMOUNT
002500021120     D rdesc#          s             15                                         TOTAL AMOUNT
002600050525     D countr          s              5  0                                      TOTAL AMOUNT
002700021120     D dept#           s              3  0                                      TOTAL AMOUNT
002800021120     D pay_type#       s              2  0                                      TOTAL AMOUNT
002900050525     D pay_type@       s                   like(pay_type#)
003000021120     D errmsg          s             75                                         TOTAL AMOUNT
003100021115
003200021115     D dft_text        c                   '(Default)'                          TOTAL AMOUNT
003300021120     D val_text        c                   '*INVALID*'                          TOTAL AMOUNT
003400010424     **
003500021115
003600021120     C     *entry        plist
003700021120     C                   parm                    dept#
003800021120     C                   parm                    pay_type#
003900021120
004000021115     C                   exsr      aainit
004100021115     C     trn_key       klist
004200021115     C                   kfld                    dept
004300021115     C                   kfld                    ptyptype
004400990826     **
004500990826
004600021115     C                   except    hedr
004700021120
004800021120     c     dept#         setll     dhrec
004900021120     C                   if        dept# <> *zeros
005000021120     c     dept#         reade     dhrec                                  lr
005100021120     C                   else
005200021120     c                   read      dhrec                                  lr
005300021120     C                   endif
005400021115     c                   dow       not(*inlr)
005500980701     **
005600021115     C                   except    hedr2
005700990114
005800021115     *****************************
005900050525      /free
006000050525         if  pay_type# <> *zeros;
006100050525             pay_type@ = pay_type#;
006200050525            else;
006300050525             pay_type@ = -1;
006400050525         endif;
006500050525      /end-free
006600021120
006700050525      /free
006800050525            countr = *zeros;
006900050525            dow not get_payty_info('RD':pay_type@:payty_struc:countr);
007000050525      /end-free
007100021115     **  Select transactions
007200021115     C     trn_key       setll     pytfmt
007300021115     C     trn_key       reade     pytfmt
007400021115     C                   if        not(%eof)
007500021115
007600021120     ** Handling First Record in definition File not being a default
007700021120     C                   if        pytrtype <> *blanks
007800021120     c                   eval      gl# = ptydglcod
007900021120     c                   eval      dft# = dft_text
008000021120     c                   exsr      validate_gl
008100021120     c                   except    detail
008200021120     c                   endif
008300021120
008400021115     c                   dow       not(%eof)
008500021115     c                   eval      gl# = pytacode
008600021115     c                   eval      dft# = *blanks
008700021115     **  Payroll Run Type
008800021115     C                   if        pytrtype <> *blanks
008900021115     C     pytrtype      chain     prtyfmt
009000021120     C                   eval      rdesc# = prtydesc
009100021115     c                   endif
009200021120     ** Handling Overflow
009300021120     C                   if        *inof
009400021120     C                   except    hedr
009500021120     C                   endif
009600021120
009700021120     c                   exsr      validate_gl
009800021115     c                   except    detail
009900021115     C     trn_key       reade     pytfmt
010000021115     c                   enddo
010100021115
010200021115     C                   else
010300021115     ** default account code
010400021115     c                   eval      gl# = ptydglcod
010500021115     c                   eval      dft# = dft_text
010600021120     c                   exsr      validate_gl
010700021115     c                   except    detail
010800021115     C                   endif
010900021115     **
011000021115     c                   enddo
011100021115     **
011200021120     C                   if        dept# <> *zeros
011300021120     c     dept#         reade     dhrec                                  lr
011400021120     C                   else
011500021120     c                   read      dhrec                                  lr
011600021120     C                   endif
011700021115     c                   enddo
011800010423     **
011900021115     ***************************************************
012000021115     ** First initilisation routine for all programs  **
012100021115     ***************************************************
012200021115     C     aainit        Begsr
012300021115     **
012400021115     C                   eval      coname = get_coname
012500021115     C                   eval      @date = get_sys_date
012600021115     C                   eval      @title = arr(1)
012700021115     **
012800021115     C                   Endsr
012900021120
013000021120     **************************************
013100021120     ** Validate GL . - Get description  **
013200021120     **************************************
013300021120     C     validate_gl   begsr
013400031001      /free
013500031001          errmsg = *blanks;
013600031001          get_gl_info(gl#:errmsg:'V');
013700031001      /end-free
013800021120
013900021120     C                   if        errmsg <> *blanks
014000021120     c                   eval      dft# = val_text
014100021120     C                   else
014200021120
014300021120     ** get GL. Description
014400031001      /free
014500031001          errmsg = *blanks;
014600031001          get_gl_info(gl#:errmsg:'D');
014700031001      /end-free
014800021120
014900021120     c                   eval      gldesc = errmsg
015000021120     C                   endif
015100021120     C                   endsr
015200021115     **
015300021115     Oqsysprt   E            hedr           2 04
015400021115     O                       @pgm                10
015500021115     O                       coname              72
015600021115     O                       @date              130
015700021115     **
015800021115     O          E            hedr           2
015900021115     O                       @user               10
016000021115     O                       @title              80
016100021115     O                                          125 'Page:'
016200021115     O                       page          z    130
016300021115     **
016400021115     O          E            hedr           2
016500021120     O                                           40 'Employee'
016600021120     O                                           50 'Run Type'
016700021120     O                                           77 'GL. A/c / Definition'
016800021115     O          E            hedr2          2
016900021115     O                       dname               40
017000021115
017100021115     O          e            detail         1
017200021115     **
017300021120     O                       ptyptype      z      3
017400021115     O                       ptydesc             30
017500021120     O                       pytemp        z     40
017600021120     O                       rdesc#         b    57
017700021120     O                       gl#                 84
017800021115     O                       dft#              +  1
017900021120     O                       gldesc         b  +  1
018000021115**
018100021115    Payroll Gl. Definition Report
