000100951207     H DATEDIT(*DMY)
000200941017     H******  PROGRAMMER - S. BRITTON
000300951213     FCONVTD    CF   E             WORKSTN
000400951207     D*
000500951207     D                SDS
000600951207     D  @PRM             *PARMS
000700951207     **
000800960514     C     *ENTRY        PLIST
000900960514     C                   PARM                    KA                1
001000960514     **
001100951213     C     TOP           TAG
001200951213     C                   EXFMT     FMT1
001300951213     C                   MOVE      *BLANKS       TEXT
001400960514     C                   MOVE      KA            K                 1 0
001500951213     C                   CALL      'CNVNUM'
001600951213     C                   PARM                    AMOUNT
001700951213     C                   PARM                    TEXT
001800960514     C                   PARM                    K
001900951213     C   03              GOTO      BOTTOM
002000951213     C                   GOTO      TOP
002100951213     C     BOTTOM        TAG
002200951207     C                   SETON                                        LR
