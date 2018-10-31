000100951207     H DATEDIT(*DMY)
000200960415     H******  PROGRAMMER - S. BIRKETT
000300960416     FKRON09D   CF   E             WORKSTN
000400960416     F                                     SFILE(SFDSP:REC)
000500960416     FKRONRF    IF   E           K DISK
000600951207     D*
000700951207     D                SDS
000800951207     D  @PRM             *PARMS
000900951207     **
001000951207     C                   MOVE      '0'           *IN
001100951207     C                   Z-ADD     0             REC
001200951207     **
001300960416     C     *LIKE         DEFINE    KRST          IRST
001400951207     **
001500951207     C     *ENTRY        PLIST
001600960416     C                   PARM                    IRST
001700951207     **
001800951207     C     @PRM          IFEQ      *ZEROS
001900951207     C                   SETON                                          99
002000951207     C                   ENDIF
002100951207     **
002200960416     C     *LOVAL        SETLL     KRONRF
002300941017     C                   MOVE      '1'           *IN12
002400941017     C                   WRITE     SFCTL
002500941017     C                   MOVE      '0'           *IN12
002600951207     **
002700960416     C                   READ      KRPREC                                 13
002800951207     C     *IN13         DOWEQ     '0'
002900951207     C                   ADD       1             REC               4 0
003000960416     C                   WRITE     SFDSP                                  20
003100960416     C                   READ      KRPREC                                 13
003200941029     C                   ENDDO
003300960416     C     REC           COMP      *ZEROS                                 25
003400941017     C                   EXFMT     SFCTL
003500951207     **
003600960416     C     *IN25         IFEQ      *OFF
003700960416     C                   READC     SFDSP                                  16
003800951207     C     *IN16         IFEQ      '0'
003900951207     C     OPT           ANDEQ     'X'
004000960416     C                   MOVEL(P)  KRST          IRST
004100941028     C                   ENDIF
004200960416     C                   ENDIF
004300960416     **
004400951207     C                   SETON                                        LR
