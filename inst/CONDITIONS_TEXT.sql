CREATE TABLE CONDITIONS_TEXT_MMM1 AS SELECT 
 condition_id,
    condition_lang,
    start_date,
    end_date,
    to_lob(CONDITION_TEXT) AS CONDITION_TEXT,
    cuser,
    cdate,
    uuser,
    udate,
    licence_year,
    cond_start_date,
    cond_end_date
 FROM CONDITIONS_TEXT;
 
 CREATE TABLE CONDITIONS_TEXT_MMM AS SELECT 
 condition_id,
    condition_lang,
    start_date,
    end_date,
    CAST(SUBSTR(CONDITION_TEXT, 1, 200) AS VARCHAR2(200)) AS CONDITION_TEXT,
    cuser,
    cdate,
    uuser,
    udate,
    licence_year,
    cond_start_date,
    cond_end_date
 FROM CONDITIONS_TEXT_MMM1;