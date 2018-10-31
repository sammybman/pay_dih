000100951103/* ******************************************************** */
000200050527/*  EMP. MONTH-END ALLOWANCE UPDATE                         */
000300951103/*                                                          */
000400951103/* ******************************************************** */
000500050527             PGM        PARM(&PRD)
000600050527             DCL        VAR(&PRD) TYPE(*DEC) LEN(6 0)
000700050527             STRCMTCTL  LCKLVL(*CHG)
000800050527
000900970305/*                                                          */
001000051026/* ADJUST TIME-BASED ALLOWANCES  */
001001051026             CALL       PGM(HMRUPD01R)
001002051026/* ALLOWANCE UPDATE AND GL. WRITE  */
001100050601             CALL       PGM(EMPMTHENDR) PARM(&PRD)
001200050527             COMMIT
001300050527             ENDCMTCTL
001400050527
001500951103ENDPGM:      ENDPGM
