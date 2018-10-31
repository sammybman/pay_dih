000100941121     H DATEDIT(*DMY)
000200990209     **************************************************
000300990209     ** BANKS PAYROLL DIVISION MAINTENANCE PROGRAM  **
000400990209     **************************************************
000500990112     Fdiv       uf a e           k disk
000600990209     Fdivl03    if   e           k disk    rename(divfmt:divchk) prefix(ck)
000700990201     Fbnkl02    if   e           k disk
000800990211     Fmasrell3  if   e           k disk
000900990211     Fpay008d   cf   e             WORKSTN
001000990112     **
001100990211     D                sds
001200990211     D  @pgm             *proc
001300990211     D  @user                254    263
001400950711     **
001500990209     C     *like         define    dtbnk         hld_bnk
001600941118     C     *INLR         DOWNE     *ON
001700941120     ** First screen to request number
001800941121     C                   MOVE      *BLANKS       #MODE
001900950711     C                   MOVE      *BLANKS       DTNAME
002000941121     **
002100941123     **
002200941118     C                   EXFMT     FMT1
002300941123     **
002400941118     C     *IN03         IFEQ      *ON
002500941118     C                   SETON                                        LR
002600941120     C                   ELSE
002700941123     **
002800950711     C     DTNO          CHAIN     DIVFMT                             80
002900990209     C     *IN80         ifeq      *ON
003000941120     C                   MOVE      ' ADD  '      #MODE
003100950711     C                   MOVE      *BLANKS       DTNAME
003200990209     C** setup mode description
003300990209     C                   else
003400941120     C                   MOVE      'AMEND '      #MODE
003500990209     C                   move      dtbnk         hld_bnk
003600990209     C                   endif
003700941121     **
003800941121     **
003900941121     ** CONFIRMATION SCREEN
004000941121     **
004100990112     C     restart       tag
004200941121     C                   EXFMT     FMT2
004300990209
004400990209     ** Setoff error indicators
004500990210     C                   setoff                                       101112
004600941121     C     *IN03         IFEQ      *ON
004700941121     C                   SETON                                        LR
004800941121     C                   ELSE
004900990112     ** VALIDATE BANK
005000990112     C                   if        dtbnk <> *blanks
005100990112     C     dtbnk         chain     bkfmt                              10
005200990112     C     *in10         cabeq     *on           restart
005300990209     **
005400990209     ** VALIDATE BANK - Prevent duplicate bank
005500990209     C     dtbnk         chain     divchk                             99
005600990209     C                   if        not(*in99) and dtno <> ckdtno
005700990209     C                   seton                                        11
005800990209     C                   goto      restart
005900990209     C                   endif
006000990209     C                   endif
006100990211     ** VALIDATE BANK - Prevent clearing bank field if employees are currently
006200990211     **                 using this bank
006300990211     C                   if        dtbnk = *blanks and hld_bnk <> *blanks
006400990211     C     dtno          chain     emprec                             99
006500990211     C                   if        not(*in99)
006600990210     C                   seton                                        12
006700990211     C                   goto      restart
006800990211     C                   endif
006900990210     C                   endif
007000990209     **
007100941121     ** VALIDATE OWNER
007200941120     C     *IN80         IFEQ      *ON
007300950925     C     DTNAME        ANDNE     *BLANKS
007400990112     C                   move      'A'           dtrsts
007500950711     C                   WRITE     DIVFMT
007600941120     C                   ELSE
007700951212     C     *IN80         IFEQ      *OFF
007800951212     C     DTNAME        ANDNE     *BLANKS
007900950711     C                   UPDATE    DIVFMT
008000990210     C                   endif
008100990210     C                   endif
008200941120     **
008300941120     C                   END
008400941123     C                   END
008500990210     0*
008600941118     C                   END
