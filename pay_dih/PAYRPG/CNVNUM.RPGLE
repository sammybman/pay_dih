000100951213     H
000200951213     H** PROGRAMMER - S.BRITTON
000300010305     **
000400951213     D TABDIG          S              1  0 DIM(10) CTDATA PERRCD(1)
000500951213     D TABCHR          S              5    DIM(10) ALT(TABDIG)
000600951213     D TABDI2          S              1  0 DIM(9) CTDATA PERRCD(1)
000700951213     D TABCH2          S              7    DIM(9) ALT(TABDI2)
000800951213     D TABDI3          S              1  0 DIM(9) CTDATA PERRCD(1)
000900951213     D TABCH3          S              9    DIM(9) ALT(TABDI3)
001000010305     **
001100970219     D BTXT            C                   CONST('Billion')
001200960514     D MTXT            C                   CONST('Million')
001300960514     D TTXT            C                   CONST('Thousand')
001400960514     D HTXT            C                   CONST('Hundred')
001500960224     D CTXT            C                   CONST('Cents *************-
001600960224     D                                     *******************-
001700960224     D                                     *******************-
001800960224     D                                     *******************-
001900960224     D                                     ********************')
002000960224     D DTXT            C                   CONST('Dollars ***********-
002100960224     D                                     *******************-
002200960224     D                                     *******************-
002300960224     D                                     *******************-
002400960224     D                                     ********************')
002500951213     D DTXT2           C                   CONST('Dollars')
002600010305     **
002700951213     D                 DS                  INZ
002800951213     D  AMT                    1     11  2
002900951215     D  HMFLD                  1      1  0
003000951215     D  TMFLD                  2      2  0
003100951213     D  MIFLD                  3      3  0
003200951213     D  HTFLD                  4      4  0
003300951213     D  TTFLD                  5      5  0
003400951213     D  THFLD                  6      6  0
003500951213     D  HUFLD                  7      7  0
003600951213     D  TEFLD                  8      8  0
003700951213     D  SIFLD                  9      9  0
003800951213     D  DSFLD                 10     10  0
003900951213     D  DTFLD                 11     11  0
004000010305     **
004100951215     D  TMCHK                  2      3  0
004200951213     D  TTCHK                  5      6  0
004300951214     D  TECHK                  8      9  0
004400951213     D  CNCHK                 10     11
004500951215     D  DOCHK                  1      9  0
004600951215     D  MICHK                  4      9  0
004700951215     D  THCHK                  7      9  0
004800951215     D  MMCHK                  4      9  0
004900951215     ** INPUT/OUTPUT PARAMETERS
005000951213     C     *ENTRY        PLIST
005100951213     C     AMT           PARM                    AMOUNT           11 2
005200960124     C                   PARM                    TEXT            100
005300960514     C                   PARM                    K                 1 0
005400951215     **
005500951215     **  HANDLING HUNDRED MILLION FIGURE
005600951215     C     HMFLD         IFGT      *ZEROS
005700951215     C     HMFLD         LOOKUP    TABDIG        TABCHR                   77
005800951215     **
005900960514     C                   CAT(P)    TABCHR:K      TEXT
006000960514     C                   CAT(P)    HTXT:K        TEXT
006100951215     C     TMCHK         IFGT      *ZERO
006200960514     C                   CAT(P)    '&':K         TEXT
006300951215     C                   ENDIF
006400951215     **
006500951215     C                   ENDIF
006600951215     **  HANDLING TEN MILLION FIGURE
006700951215     C     TMFLD         IFGT      *ZEROS
006800951215     **
006900951215     C     TMFLD         IFEQ      1
007000951215     C     MIFLD         ANDGT     *ZERO
007100951215     C     MIFLD         LOOKUP    TABDI3        TABCH3                   77
007200960514     C                   CAT(P)    TABCH3:K      TEXT
007300960514     C                   CAT(P)    MTXT:K        TEXT
007400960514     C     MMCHK         IFNE      *ZEROS
007500960514     C                   CAT(P)    ',':K         TEXT
007600951215     C                   ENDIF
007700951215     C                   SETON                                        01
007800951215     C                   ELSE
007900951215     C     TMFLD         LOOKUP    TABDI2        TABCH2                   77
008000960514     C                   CAT(P)    TABCH2:K      TEXT
008100951215     C                   ENDIF
008200951215     **
008300951215     C                   ENDIF
008400951215     **  HANDLING MILLIONS FIGURE
008500951215     C     *IN01         IFEQ      '0'
008600951213     C     MIFLD         IFGT      *ZEROS
008700951215     C     TMFLD         ORGT      *ZEROS
008800951215     C     HMFLD         ORGT      *ZEROS
008900951213     C     MIFLD         LOOKUP    TABDIG        TABCHR                   77
009000960514     C                   CAT(P)    TABCHR:K      TEXT
009100960514     C                   CAT(P)    MTXT:K        TEXT
009200960514     C     MICHK         IFNE      *ZEROS
009300960514     C                   CAT(P)    ',':K         TEXT
009400951215     C                   ENDIF
009500951213     C                   ENDIF
009600951215     C                   ENDIF
009700951215     **  HANDLING HUNDRED THOUSAND FIGURE
009800951213     C     HTFLD         IFGT      *ZEROS
009900951213     C     HTFLD         LOOKUP    TABDIG        TABCHR                   77
010000951213     **
010100960514     C                   CAT(P)    TABCHR:K      TEXT
010200960514     C                   CAT(P)    HTXT:K        TEXT
010300951213     C     TTCHK         IFGT      *ZERO
010400960514     C                   CAT(P)    '&':K         TEXT
010500951213     C                   ENDIF
010600951213     **
010700951213     C                   ENDIF
010800951215     **  HANDLING TEN THOUSAND FIGURE
010900951213     C     TTFLD         IFGT      *ZEROS
011000951213     **
011100951213     C     TTFLD         IFEQ      1
011200951213     C     THFLD         ANDGT     *ZERO
011300951213     C     THFLD         LOOKUP    TABDI3        TABCH3                   77
011400960514     C                   CAT(P)    TABCH3:K      TEXT
011500960514     C                   CAT(P)    TTXT:K        TEXT
011600960514     C     THCHK         IFNE      *ZEROS
011700960514     C                   CAT(P)    ',':K         TEXT
011800951215     C                   ENDIF
011900951215     C                   SETON                                        02
012000951213     C                   ELSE
012100951213     C     TTFLD         LOOKUP    TABDI2        TABCH2                   77
012200960514     C                   CAT(P)    TABCH2:K      TEXT
012300951213     C                   ENDIF
012400951213     **
012500951213     C                   ENDIF
012600951215     **  HANDLING THOUSAND FIGURE
012700951215     C     *IN02         IFEQ      '0'
012800951213     C     HTFLD         IFGT      *ZEROS
012900951213     C     TTFLD         ORGT      *ZEROS
013000951213     C     THFLD         ORGT      *ZEROS
013100951213     C     THFLD         LOOKUP    TABDIG        TABCHR                   77
013200960514     C                   CAT(P)    TABCHR:K      TEXT
013300960514     C                   CAT(P)    TTXT:K        TEXT
013400960514     C     THCHK         IFNE      *ZEROS
013500960514     C                   CAT(P)    ',':K         TEXT
013600951215     C                   ENDIF
013700951213     C                   ENDIF
013800951213     C                   ENDIF
013900951215     **  HANDLING HUNDRED FIGURE
014000951213     C     HUFLD         IFGT      *ZEROS
014100951213     C     HUFLD         LOOKUP    TABDIG        TABCHR                   77
014200951213     **
014300960514     C                   CAT(P)    TABCHR:K      TEXT
014400960514     C                   CAT(P)    HTXT:K        TEXT
014500951213     C     TECHK         IFGT      *ZERO
014600960514     C                   CAT(P)    '&':K         TEXT
014700951213     C                   ENDIF
014800951215     **
014900951213     C                   ENDIF
015000951215     **  HANDLING TENS FIGURE
015100951213     C     TEFLD         IFGT      *ZEROS
015200951213     **
015300951213     C     TEFLD         IFEQ      1
015400951213     C     SIFLD         ANDGT     *ZERO
015500951213     C     SIFLD         LOOKUP    TABDI3        TABCH3                   77
015600960514     C                   CAT(P)    TABCH3:K      TEXT
015700951215     C                   SETON                                        03
015800951213     C                   ELSE
015900951213     C     TEFLD         LOOKUP    TABDI2        TABCH2                   77
016000960514     C                   CAT(P)    TABCH2:K      TEXT
016100951213     C                   ENDIF
016200951215     **
016300951213     C                   ENDIF
016400951215     **  HANDLING SINGLE DOLLAR FIGURE
016500951213     C     SIFLD         IFGT      *ZEROS
016600951215     C     *IN03         ANDEQ     '0'
016700951213     C     SIFLD         LOOKUP    TABDIG        TABCHR                   77
016800951213     C     TEFLD         IFGT      *ZEROS
016900960514     C                   CAT(P)    '-':K         TEXT
017000960514     C                   CAT(P)    TABCHR:K      TEXT
017100951213     C                   ELSE
017200960514     C                   CAT(P)    TABCHR:K      TEXT
017300951213     C                   ENDIF
017400951213     **
017500951213     C                   ENDIF
017600951215     **  HANDLING CENTS FIGURE
017700951213     C     CNCHK         IFNE      *ZEROS
017800951213     C     DOCHK         IFGT      *ZEROS
017900960514     C     '&'           CAT(P)    CNCHK:K       TEXTC           100
018000960514     C                   CAT(P)    CTXT:K        TEXTC
018100960514     C                   CAT(P)    DTXT2:K       TEXT
018200951213     C                   ELSE
018300960514     C     CNCHK         CAT(P)    CTXT:K        TEXTC
018400951213     C                   ENDIF
018500960514     C                   CAT(P)    TEXTC:K       TEXT
018600951213     C                   ELSE
018700951213     C     DOCHK         IFGT      *ZEROS
018800960514     C                   CAT(P)    DTXT:K        TEXT
018900951213     C                   ENDIF
019000951213     C                   ENDIF
019100951213     **
019200951213     C                   SETON                                        LR
019300951213**   **
0194009512130
0195009512131One
0196009512132Two
0197009512133Three
0198009512134Four
0199009512135Five
0200009512136Six
0201009512137Seven
0202009512138Eight
0203009512139Nine
020400951213**
0205009512131Ten
0206009512132Twenty
0207009512133Thirty
0208009512134Forty
0209009512135Fifty
0210009512136Sixty
0211009512137Seventy
0212009512138Eighty
0213009512139Ninety
021400951213**
0215009512131Eleven
0216009512132Twelve
0217009512133Thirteen
0218009802104Fourteen
0219009512135Fifteen
0220009512136Sixteen
0221009512137Seventeen
0222009512138Eighteen
0223009512139Nineteen
