000100960419     H DATEDIT(*DMY)
000200960419     H******  PROGRAMMER - S. BIRKETT
000300960419     FKRON11D   CF   E             WORKSTN
000400960419     FKRONACRU  UF A E           K DISK
000500960415     D*
000600960415     D                SDS
000700960415     D  @PGM             *PROC
000800960415     **
000900960415     C                   EXFMT     KROND1
001000940806     C   KC              MOVE      '1'           *INLR
001100960415     C     *IN01         IFEQ      '1'
001200960419     C                   CALL      'KRON12R'
001300960419     C                   PARM                    KACCFD
001400960415     C                   ENDIF
001500960415     **
001600960415     C                   MOVE      '0'           *IN
001700960415     **
001800960415     C     AED           CASEQ     'A'           ADD
001900960415     C     AED           CASEQ     'D'           DELETE
002000960415     C                   ENDCS
002100960415     **
002200960415     C     ADD           BEGSR
002300960419     C                   WRITE     KACRU                                05
002400960419     C                   MOVE      *BLANKS       KACCFD
002500940806     C                   ENDSR
002600960415     **
002700960415     **
002800960415     **
002900940806     C     DELETE        BEGSR
003000960419     C     KACCFD        CHAIN     KACRU                              06
003100960415     C     *IN06         IFEQ      '0'
003200960419     C                   DELETE    KACRU
003300960419     C                   MOVE      *BLANKS       KACCFD
003400940806     C                   ENDIF
003500940806     C                   ENDSR
