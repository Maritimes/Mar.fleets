define marfSpp = 197
define marfGr = 62

    SELECT SUM(CATCH.RND_WEIGHT_KGS) RND_WEIGHT_KGS, SUM(CATCH.CNT) CNT, 
    --M.licence_type, M.licence_subtype,  M.gear, M.species , 
    M.licence_type_id, M.licence_subtype_id, M.gear_code, CATCH.GEAR_CODE, M.species_code, CATCH.SPECIES_CODE --, CATCH.YEAR1 
    FROM
        (SELECT GEAR_CODE, LICENCE_ID, SPECIES_CODE, SUM(RND_WEIGHT_KGS) RND_WEIGHT_KGS, COUNT(LICENCE_ID) CNT, EXTRACT(YEAR FROM DATE_FISHED) YEAR1 
                FROM MARFISSCI.PRO_SPC_INFO 
                WHERE LICENCE_ID IN (SELECT DISTINCT LICENCE_ID FROM MARFISSCI.marbycatch_lic M WHERE SPECIES_CODE IN (&marfSpp,199))
                AND SPECIES_CODE IN (&marfSpp,199)
                GROUP BY GEAR_CODE, LICENCE_ID,SPECIES_CODE, EXTRACT(YEAR FROM DATE_FISHED)) CATCH,
        (SELECT  DISTINCT LICENCE_ID, licence_type, licence_subtype,  gear, species , licence_type_id, licence_subtype_id, gear_code, species_code   
                FROM MARFISSCI.marbycatch_lic M 
                WHERE SPECIES_CODE IN (&marfSpp,199)) M
        WHERE CATCH.LICENCE_ID = M.LICENCE_ID
        AND CATCH.SPECIES_CODE = M.SPECIES_CODE
        AND CATCH.GEAR_CODE = M.GEAR_CODE
        GROUP BY licence_type, licence_subtype,  gear, species , licence_type_id, licence_subtype_id, M.gear_code,  M.species_code, CATCH.GEAR_CODE,CATCH.SPECIES_CODE--, YEAR1
        HAVING M.GEAR_CODE = &marfGr
    
    
    