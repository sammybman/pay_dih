000100941121     H DATEDIT(*DMY)
000200980519     Fdptloc    UF A E           K DISK
000300950711     FPAY009D   CF   E             WORKSTN
000400980520     **
000500950711     D                SDS
000600950711     D  @PGM             *PROC
000700950711     D  @USER                254    263
000800950711     **
000900941118     C     *INLR         DOWNE     *ON
001000941120     ** First screen to request number
001100941121     C                   MOVE      *BLANKS       #MODE
001200950711     C                   MOVE      *BLANKS       LTNAME
001300941121     **
001400941123     **
001500941118     C                   EXFMT     FMT1
001600941123     **
001700941118     C     *IN03         IFEQ      *ON
001800941118     C                   SETON                                        LR
001900941120     C                   ELSE
002000941123     **
002100950711     C     LTNO          CHAIN     LOCFMT                             80
002200941120     C     *IN80         IFEQ      *ON
002300941120     C                   MOVE      ' ADD  '      #MODE
002400950711     C                   MOVE      *BLANKS       LTNAME
002500941120     C** Clear input fields
002600941120     C                   ELSE
002700941120     C                   MOVE      'AMEND '      #MODE
002800941120     C                   END
002900941121     **
003000941121     **
003100941121     ** CONFIRMATION SCREEN
003200941121     **
003300941121     C                   EXFMT     FMT2
003400941121     C     *IN03         IFEQ      *ON
003500941121     C                   SETON                                        LR
003600941121     C                   ELSE
003700950711     ** VALIDATE
003800941120     C     *IN80         IFEQ      *ON
003900950925     C     LTNAME        ANDNE     *BLANKS
004000980520     C                   move      'A'           ltsts
004100950711     C                   WRITE     LOCFMT
004200941120     C                   ELSE
004300951212     C     *IN80         IFEQ      *OFF
004400951212     C     LTNAME        ANDNE     *BLANKS
004500950711     C                   UPDATE    LOCFMT
004600941120     C                   END
004700951212     C                   END
004800941120     **
004900941120     C                   END
005000941123     C                   END
005100941120     C**
005200941118     C                   END
