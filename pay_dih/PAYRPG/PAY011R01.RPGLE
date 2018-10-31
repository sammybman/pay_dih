000100950530     H DATEDIT(*DMY)
000200971231     F* BANKS DIH Balance sheet program
001400950705     FQSYSPRT   O    F  132        PRINTER
001500950717     D*
001600950530     D                SDS
001700950530     D  @PGM             *PROC
001800950530     D  @USER                254    263
001801971231     C********************************************************************
002300950823     C     *ENTRY        PLIST
002400950823     C                   PARM                    TYPE              1
002401971231     C                   PARM                    parm3             5 0
002402971231     C                   PARM                    parm4            11 2
002403971231     C                   PARM                    parm5             5 0
002404971231     C                   PARM                    parm6            11 2
002405971231     C                   PARM                    parm7            11 2
002406971231     C                   PARM                    parm8            11 2
002407971231     C                   PARM                    parm9             9 2
002408971231     C                   PARM                    parm10            9 2
002409971231     C                   PARM                    parm11            9 2
002410971231     C                   PARM                    parm12            9 2
002411971231     C                   PARM                    parm13            9 2
002412971231     C                   PARM                    parm14            9 2
002413971231     C                   PARM                    parm15            9 2
002414971231     C                   PARM                    parm16            9 2
002415971231     C*********************************************************************
002500950823     C     TYPE          COMP      'R'                                    77
002600950823     C     TYPE          COMP      'O'                                    78
002700950823     C     TYPE          COMP      'C'                                    79
002800950823     C     TYPE          COMP      'S'                                    76
002900950823     C     TYPE          COMP      'B'                                    75
002901980212     C  N76TYPE          COMP      'X'                                    76
003001980212     C     TYPE          COMP      'A'                                    80
003100971231     c
003101971231     C     parm7         sub       parm8         result           11 2
003102971231     C     result        sub       parm9         result
003103971231     C     result        sub       parm10        result
003104971231     C     result        sub       parm11        result
003105971231     C     result        sub       parm12        result
003106971231     C     result        sub       parm13        result
003107971231     C     result        sub       parm14        result
003108971231     C     result        sub       parm15        result
003109971231     C     result        sub       parm16        result
005800950717     C                   EXCEPT    HEADER
005801971231     C                   EXCEPT    det
005802971231     C                   EXCEPT    detail
005803971231     C                   EXCEPT    repend
005804971231     c                   seton                                        lr
005805971231     c                   return
005900950629     **
013000951120     OQSYSPRT   E            HEADER           01
013100950629     O                       @PGM                10
013200950629     O                                           70 'BANKS  DIH  LIMITED'
013300951120     OQSYSPRT   E            HEADER           02
013400950629     O                       @USER               10
013500950629     O                                           65 'D"AGUIAR"S INDUSTRIES & '
013600950629     O                                         +  1 ' HOLDINGS'
014200951120     OQSYSPRT   E            HEADER           03
014400950629     O                                          120 'RUN Date: '
014500950629     O                       UDATE         Y   +  1
014600971231     OQSYSPRT   E            det         2
014700971231     O                                            5 'BEING'
014901971231     O               77                          18 'REGULAR   '
014902971231     O               78                          18 'ON LEAVE  '
014903971231     O               79                          18 'CASUAL    '
014904971231     O               76                          18 'SPECIAL   '
014905971231     O               75                          18 'CAS. BONUS'
014906980212     O               80                          18 'ALLOWANCE '
015000971231     O                                         +  1 'PAYROLL FOR F/N'
015001971231     O                       udate         Y   +  1
015002971231     O                                         +  1 'WITH'
015003971231     O                       parm3         1   +  1
015004971231     O                                         +  1 'PAYSLIPS WITH OUT'
015005971231     O                                         +  1 'A/C VALUE AND'
015007971231     O                       parm5         1   +  1
015008971231     O                                         +  1 'PAYSLIPS WITH A/C'
015009971231     O                                         +  1 'VALUE'
015010971231     OQSYSPRT   E            DETAIL      2  1
015011971231     O                                           60 'TOTAL  :=>'
015012971231     O                       parm7         1   + 10
016100971231     OQSYSPRT   E            DETAIL      2  1
016200980105     O                                           60 'LESS DEMICO'
016202971231     O                       parm8         1   + 10
016204971231     OQSYSPRT   E            DETAIL         1
016205980105     O                                           60 'LESS SECURITY'
016206980102     O                       parm9         1   + 12
016207971231     OQSYSPRT   E            DETAIL         1
016208980105     O                                           60 'LESS BERBICE'
016209980102     O                       parm10        1   + 12
016210971231     OQSYSPRT   E            DETAIL         1
016211980105     O                                           60 'LESS ESSEQUIBO'
016212980102     O                       parm11        1   + 12
016213971231     OQSYSPRT   E            DETAIL         1
016214980105     O                                           60 'LESS BARTICA'
016215980102     O                       parm12        1   + 12
016216971231     OQSYSPRT   E            DETAIL         1
016217980105     O                                           60 'LESS LINDEN'
016218980102     O                       parm13        1   + 12
016219971231     OQSYSPRT   E            DETAIL         1
016220980105     O                                           60 'LESS PENSIONERS'
016221980102     O                       parm16        1   + 12
016222971231     OQSYSPRT   E            DETAIL         1
016223980105     O                                           60 'LESS N/A QIK SERV'
016224980102     O                       parm14        1   + 12
016225971231     OQSYSPRT   E            DETAIL         1
016226980105     O                                           60 'LESS ARVIDA'
016227980102     O                       parm15        1   + 12
016228971231     OQSYSPRT   E            DETAIL      2  1
016229971231     O                                           60 'N.B.I.C :=>'
016230971231     O                       result        1   + 10
016231971231     OQSYSPRT   E            DETAIL      1  1
016232971231     O                                          115 'SIGN:_______________'
017500971231     OQSYSPRT   E            REPEND      2  1
017600971231     O                                           65 '*** END OF REPORT ***'
