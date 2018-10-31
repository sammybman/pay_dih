000100010424     H DATEDIT(*ymd)
000200021115     F* PAYROLL GL. Definition Report
000300010424     F* PROGRAMMER:-S. Britton & C. Williams
000400021115     Fdeptm     if   e           k disk
000500021115     Fpaytdefl02if   e           k disk
000600021120     Fpayrtypl01if   e           k disk
000700021120     Fqsysprt   o    f  132        printer oflind(*inof)
000800010424     ** Payroll PROTOTYPES
000900110729      /copy qcpysrc,genpr
001000110729      /copy qcpysrc,glpr
001100110729      /copy qcpysrc,emppr
001200990113     **
001300960130     D                SDS
001400960130     D  @PGM             *PROC
001500960130     D  @USER                254    263
001600010424     **
001700021115     D arr             s             40    dim(1) ctdata perrcd(1)
001800021115     D coname          s             30                                         TOTAL AMOUNT
001900021120     D @title          s             40                                         TOTAL AMOUNT
002000021115     D @date           s             10                                         TOTAL AMOUNT
002100021120     D gldesc          s             30                                         TOTAL AMOUNT
002200021115     D gl#             s             25                                         TOTAL AMOUNT
002300021115     D dft#            s             10                                         TOTAL AMOUNT
002400021120     D rdesc#          s             15                                         TOTAL AMOUNT
002500021120     D dname#          s             25                                         TOTAL AMOUNT
002600021120     D dept#           s              3  0                                      TOTAL AMOUNT
002700021120     D pay_type#       s              2  0                                      TOTAL AMOUNT
002800021120     D errmsg          s             75                                         TOTAL AMOUNT
002900050525     D countr          s              5  0                                      TOTAL AMOUNT
003000050525     D pay_type@       s                   like(pay_type#)                      TOTAL AMOUNT
003100021115
003200021115     D dft_text        c                   '(Default)'                          TOTAL AMOUNT
003300021120     D val_text        c                   '*INVALID*'                          TOTAL AMOUNT
003400010424     **
003500021120
003600021120     C     *entry        plist
003700021120     C                   parm                    dept#
003800021120     C                   parm                    pay_type#
003900021115
004000021115     C                   exsr      aainit
004100021115     C     trn_key       klist
004200021115     C                   kfld                    dept
004300021115     C                   kfld                    ptyptype
004400990826     **
004500990826
004600021115     C                   except    hedr
004700021120     *****************************
004800050525      /free
004900050525         if  pay_type# <> *zeros;
005000050525             pay_type@ = pay_type#;
005100050525            else;
005200050525             pay_type@ = -1;
005300050525         endif;
005400050525
005500050525            countr = *zeros;
005600050525            dow not get_payty_info('RD':pay_type@:payty_struc:countr);
005700050525      /end-free
005800980701     **
005900021115     C                   except    hedr2
006000990114
006100021115     *****************************
006200021120     c     dept#         setll     dhrec
006300021120     C                   if        dept# <> *zeros
006400021120     c     dept#         reade     dhrec                                  68
006500021120     C                   else
006600021120     c                   read      dhrec                                  68
006700021120     C                   endif
006800021120
006900021115     c                   dow       not(*in68)
007000021120
007100021120     C                   eval      dname# = dname
007200021115     **  Select transactions
007300021115     C     trn_key       setll     pytfmt
007400021115     C     trn_key       reade     pytfmt
007500021115     C                   if        not(%eof)
007600021115
007700021120     ** Handling First Record in definition File not being a default
007800021120     C                   if        pytrtype <> *blanks
007900021120     c                   eval      gl# = ptydglcod
008000021120     c                   eval      dft# = dft_text
008100021120     c                   exsr      validate_gl
008200021120     c                   except    detail
008300021120     c                   endif
008400021120
008500021115     c                   dow       not(%eof)
008600021115     c                   eval      gl# = pytacode
008700021115     c                   eval      dft# = *blanks
008800021115     **  Payroll Run Type
008900021115     C                   if        pytrtype <> *blanks
009000021115     C     pytrtype      chain     prtyfmt
009100021120     C                   eval      rdesc# = prtydesc
009200021115     c                   endif
009300021120     ** Handling Overflow
009400021120     C                   if        *inof
009500021120     C                   except    hedr
009600021120     C                   endif
009700021120
009800021120     c                   exsr      validate_gl
009900021115     c                   except    detail
010000021115     C     trn_key       reade     pytfmt
010100021115     c                   enddo
010200021115
010300021115     C                   else
010400021115     ** default account code
010500021115     c                   eval      gl# = ptydglcod
010600021115     c                   eval      dft# = dft_text
010700021120     c                   exsr      validate_gl
010800021115     c                   except    detail
010900021115     C                   endif
011000021120     **
011100021120     C                   if        dept# <> *zeros
011200021120     c     dept#         reade     dhrec                                  68
011300021120     C                   else
011400021120     c                   read      dhrec                                  68
011500021120     C                   endif
011600021115     c                   enddo
011700021115     **
011800021115     c                   enddo
011900050525      /free
012000050525          *inlr = *on;
012100050525      /end-free
012200010423     **
012300021115     ***************************************************
012400021115     ** First initilisation routine for all programs  **
012500021115     ***************************************************
012600021115     C     aainit        Begsr
012700021115     **
012800021115     C                   eval      coname = get_coname
012900021115     C                   eval      @date = get_sys_date
013000021115     C                   eval      @title = arr(1)
013100021115     **
013200021115     C                   Endsr
013300021120
013400021120     **************************************
013500021120     ** Validate GL . - Get description  **
013600021120     **************************************
013700021120     C     validate_gl   begsr
013800031001      /free
013900031001          errmsg = *blanks;
014000031001          get_gl_info(gl#:errmsg:'V');
014100031001      /end-free
014200021120
014300021120     C                   if        errmsg <> *blanks
014400021120     c                   eval      dft# = val_text
014500021120     C                   else
014600021120
014700021120     ** get GL. Description
014800031001      /free
014900031001          errmsg = *blanks;
015000031001          get_gl_info(gl#:errmsg:'D');
015100031001      /end-free
015200021120
015300021120     c                   eval      gldesc = errmsg
015400021120     C                   endif
015500021120     C                   endsr
015600021115     **
015700021115     Oqsysprt   E            hedr           2 04
015800021115     O                       @pgm                10
015900021115     O                       coname              72
016000021115     O                       @date              130
016100021115     **
016200021115     O          E            hedr           2
016300021115     O                       @user               10
016400021115     O                       @title              80
016500021115     O                                          125 'Page:'
016600021115     O                       page          z    130
016700021115     **
016800021115     O          E            hedr           2
016900021120     O                                           40 'Employee'
017000021120     O                                           50 'Run Type'
017100021120     O                                           66 'GL. A/c'
017200021115     O          E            hedr2          2
017300021120     O                       ptydesc             25
017400021115
017500021115     O          e            detail         1
017600021115     **
017700021120     O                       dept          z      3
017800021120     O                       dname#              30
017900021120     O                       pytemp        z     40
018000021120     O                       rdesc#         b    57
018100021120     O                       gl#                 84
018200021115     O                       dft#              +  1
018300021120     O                       gldesc         b  +  1
018400021115**
018500021115    Payroll Gl. Definition Report
