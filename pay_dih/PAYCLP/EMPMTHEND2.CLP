000100951103/* ******************************************************** */
000200050527/*  EMP. MONTH-END ALLOWANCE UPDATE                         */
000300951103/*                                                          */
000400951103/* ******************************************************** */
000500050527             PGM
000600070802             DCL        VAR(&PRD) TYPE(*DEC) LEN(6 0) VALUE(200708)
000700050601             STRCMTCTL  LCKLVL(*CHG) CMTSCOPE(*JOB)
000800050527
000900970305/*                                                          */
001000050527/* ALLOWANCE UPDATE AND GL. WRITE  */
001100070802    /*       CALL       PGM(EMPMTHENDR) PARM(&PRD)  */
001200050527             COMMIT
001300050527             ENDCMTCTL
001400050527
001500951103ENDPGM:      ENDPGM
