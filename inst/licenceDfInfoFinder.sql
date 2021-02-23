--By plopping a known list of acceted VRs from a fleet into this SQL, we get the associated licence types and subtypes
SELECT distinct licence_type, licence_type_id, licence_subtype, licence_subtype_id, species_code, species FROM MARFISSCI.marbycatch_lic WHERE LICENCE_ID IN ( 
    SELECT DISTINCT LICENCE_ID FROM MARFISSCI.LICENCE_VESSELS WHERE VR_NUMBER IN ()
)
