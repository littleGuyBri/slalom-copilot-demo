/*<TOAD_FILE_CHUNK>*/
CREATE OR REPLACE PACKAGE
        Ensemble_NightlyBatch

AS
/* Changed By: L.Hardy  Date 8/14/01
   Added a new section importLocation_Compid_Link to eliminate
   calls to sampson in the ValidateCommission Procedure

   Changed By L.Hardy  Date 8/21/01
   Added PurgeProcessLog Section

   Changed By G.Ghali Date 8/21/01 (commented by l.hardy)
   Changed code so that the field soc_appearance does not
   cause the relationship to automatically terminate.
   Instead, the field affects the relationship created.

   Changed by L.Hardy  Date 10/23/01
   Added importUserAttribute section for CR170.
   This section will retrieve the DepositAllowInd from
   Sampson which will determine if a dealer is allowed to
   take a deposit during an activation.

   Changed By L.Hardy  Date 11/09/01
   added
   AND   (  srE.src_effective_date < sysdate and srE.dest_effective_date < sysdate )
   to the second insert into tmp_rateplanpackages in importRatePlanPackages to eliminate
   duplicate and invalid entries from the soc_relation table.

   Changed By L.Hardy  Date 11/26/01

   Added comments to all sections.


   Changed By L.Hardy  Date 12/07/01
   Modified importPackageRelationships to include logic for cr248.  Added
   additional logic to extract soc to soc relationships from soc_eligibility
   in Samson.

   Changed By:          Laura Hardy
   Date:                        01/18/02
   Description
   Modified ImportUserAttribute to perform a lookup for the archive
   indicator.  ONYX and Dealer codes that are archived are not allowed
   to perform activations.  If a code is not found it is assumed that
   (and defaulted to) this dealer is not archived.

   Changed By:  Laura Hardy
   Date 01/24/03
   Modified for Rateplan ID stability, to not expire rateplans
   when the start date, end date or name of a rate plan changes
   in Samson.  Corrected an undetected bug that expires rateplans
   that are available for all markets.


   Changed By:    Laura Hardy
   Date:                  2/5/02
   Added default values for MTMBucket in the importRatePlan section.

   Changed By:    Laura Hardy
   Date:                                06/25/02
   Reformated the source to reduce white space and improve readability
   on a standard output screen.
   Made major logic changes to importCharges and importPackageRelationships
   for Bulk activations.  The changes will result in relationships for charges
   and packages being saved (enddate is not null), rather than deleted when
   a soc expires.  Before this change the relationships were deleted.
   Source code changes can be identified with comments of --ba for "bulk
   activation".

   Changed by:  Laura Hardy
   Date         07/17/2002
   Added ImportMobileNetworkCodes

   Changed by:  Laura Hardy
   Date   10/22/02
   Modified importRatePlans to retain tm data when expiring a rateplan if
   a similar plan is to be added...

   Changed By:    Tanuja Chawla
   Date                   12/10/02
   Added a new procedure importOfferParams - changes for Enabler - CR0659

   Changed by:  Laura Hardy
   Date   01/03/03
   Modified importRatePlans to refresh variables each time the matchrateplan
   cursor is open.  This is a bug fix.

   Changed By:  Laura Hardy
   Date 01/24/03
   Modified for Rateplan ID stability, to not expire rateplans
   when the start date, end date or name of a rate plan changes
   in Samson.  Corrected an undetected bug that expires rateplans
   that are available for all markets.

   Changed by:  Laura Hardy
   Date 03/20/03
   Modified importPackageRelationships to include logic
   for bogo and reduced socs.

   Changed by:  Laura Hardy
   Date 04/07/03
   Modified importRateplans to update the spending limit for
   smart access rateplans.

   Changed by:  Laura Hardy
   Date 04/23/03
   Modified importPackages to import the duration and
   duration_ind from the promotion_terms table.

   Changed by:  Laura Hardy
   Date 06/05/2003
   Changed rateplanpackages to evaluate optional soc relationships correctly, and
   not based on price.
   Changed by: Laura Hardy

   Date 07/17/2003
   Modified importPhoneNumbers to include duplicate code for populating the 
   pagNpanxx table to correct a problem caused by additional filtering added 
   with the modification to the cNPANXX cursor.

   */
        PROCEDURE
                main;

        PROCEDURE
                importRatePlans;

        PROCEDURE
                importCharges;

        PROCEDURE
                importPackages;

        PROCEDURE
                importRatePlanCharges;

        PROCEDURE
                importRatePlanPackages;

        PROCEDURE
                importRatePlanPrice;

        PROCEDURE
                importPackageCharges;

        PROCEDURE
                importMobileNetworkCodes;

        PROCEDURE
                importPhoneNumbers;

        PROCEDURE
                createChargeRelationship(
                        pChargeID               Charge.ChargeID%TYPE,
                        pRelatedChargeID        Charge.ChargeID%TYPE,
                        pAlt_ChargeCode         Charge.alt_ChargeCode%TYPE,
                        pAlt_RelatedChargeCode  Charge.alt_ChargeCode%TYPE,
                        pAlt_ConflictCode       Charge.alt_ConflictCode%TYPE,
                        pRelationshipCode       Relationship.RelationshipCode%TYPE
                );

        PROCEDURE
                importLocation_Compid_Link;

        PROCEDURE
                purgeProcessLog;

        PROCEDURE
                importUserAttribute;

                PROCEDURE
                importOfferParams;   -- CR0659

                PROCEDURE
                importPackageRelationShips;   -- tanuja

END Ensemble_NightlyBatch;
/

CREATE OR REPLACE PACKAGE BODY
        Ensemble_NightlyBatch

AS
 /************************************
  *       importSocEligibility       *
  ************************************/

 /* GGhali:  This code tries to determine which rows should be added to the alt_RatePlanPackages
  *  table to implement the data in the soc_elegibility table.
  *
  *  The first inner SELECT gets all regular socs which could be associated with a given RatePlan.
  *  It does so by getting all of the socs associated with any dummy RatePlan which may be used
  *  as a representative for the given normal RatePlan, based on Market, ActivationLevel, and
  *  RatePlanType (AccountType/Subtype).
  *
  *  The second inner SELECT retrieves all rows from the soc_elegibility table, and decodes
  *  the relationship into its Watson counterpart.
  *
  *  Finally, in the outer SELECT, the two tables are outer joined.  Ultimately, there should
  *  be one "E" row for every soc that doesn't exist in the soc_elegibility table, but is available
  *  on a dummy RatePlan which could be associated with the normal RatePlan.  Hence the reason for
  *  the outer join.  Any soc_elegibility rows with a relationship of "A" can be ignored, as that is
  *  the default for regular socs.  So if X is the set of rows in soc_elegibility, then in essence
  *  we are inserting ^X UNION (X AND Relationship <> "A").
  */
  PROCEDURE
     importSocEligibility

  /* Changed By L.Hardy  Date 12/12/01
     Added logic to convert O relationships to E.  O relationships
     result from Samson must not be and the front end of Watson needs
     to see these relationships as excluded, or E.
  */
   IS
CURSOR  cSocEligibility
IS      SELECT  watson.*,
                DECODE( samson.RelationshipCode,
                         NULL, 'E',
                        'X','E',
                        'O', 'E', samson.RelationshipCode )  AS RelationshipCode   -- If no soc_elegibility row available, then "Excluded"
        FROM    (
                 SELECT  DISTINCT
                         rp.alt_RatePlanCode,                  -- alt_RatePlanCode
                         tmp.alt_PackageCode                   -- alt_packagecode
                 FROM    RatePlan rp,
                         RatePlan dummy,
                         RatePlanAvailability rpa,
                         tmp_RatePlanPackages tmp
                 WHERE   rp.RatePlanID = rpa.RatePlanID
                   AND   rp.MarketID = dummy.MarketID
                   AND   rp.ActivationLevelCode  IN  ( 'A', dummy.ActivationLevelCode )
                   AND   rpa.RatePlanTypeCode = dummy.RatePlanTypeCode
                   AND   dummy.alt_RatePlanCode = tmp.alt_RatePlanCode
                   AND   tmp.RelationshipCode <> 'E'        -- Don't bother excluding if it's already excluded from the dummy
                   AND   rp.RatePlanID > 0                  -- Normal Rate Plan
                   AND   dummy.RatePlanID < 0               -- Dummy Rate Plan
                   AND    rp.StartDate < SYSDATE
                   AND   rpa.StartDate < SYSDATE
                   AND   (  rp.EndDate IS NULL  OR   rp.EndDate > SYSDATE  )
                   AND   ( rpa.EndDate IS NULL  OR  rpa.EndDate > SYSDATE  )
         ) watson,
                 (
                 SELECT  RTRIM( seE.base_soc )                  AS alt_RatePlanCode,
                         RTRIM( seE.related_soc )               AS alt_PackageCode,
                         NVL( r.RelationshipCode, 'E' )         AS RelationshipCode -- Assume "Excluded" for invalid relationships
                 FROM    soc_eligibility@Ensemble seE,
                         Relationship r
                 WHERE   seE.relation_type = r.alt_RelationshipCode (+)
                   AND   seE.effective_date < SYSDATE
                   AND   (  seE.expiration_date IS NULL  OR  seE.expiration_date > SYSDATE  )
         ) samson
         WHERE   watson.alt_RatePlanCode = samson.alt_RatePlanCode (+)
           AND   watson.alt_PackageCode  = samson.alt_PackageCode  (+);

rMatchSoc       alt_RatePlanPackages%ROWTYPE;

  BEGIN
          BEGIN
                DELETE
                FROM    alt_RatePlanPackages
                WHERE   LastModBy = 'EADB';

           FOR  rSoc  IN  cSocEligibility  LOOP

                IF( rSoc.RelationshipCode <> 'A' )  THEN
                  BEGIN
                        SELECT  *
                        INTO    rMatchSoc
                        FROM    alt_RatePlanPackages
                        WHERE   alt_RatePlanCode = rSoc.alt_RatePlanCode
                          AND   alt_PackageCode  = rSoc.alt_PackageCode;


                        IF(     rMatchSoc.LastModBy         =  'EADB' -- This is redundant
                           AND  rMatchSoc.RelationshipCode  <> rSoc.RelationshipCode)
                                                THEN

                            UPDATE  alt_RatePlanPackages
                            SET     RelationshipCode = rSoc.RelationshipCode,
                                    LastModDate      = SYSDATE
                            WHERE   alt_RatePlanCode = rMatchSoc.alt_RatePlanCode
                              AND   alt_PackageCode  = rMatchSoc.alt_PackageCode;
                        END IF;

                  EXCEPTION
                       WHEN  NO_DATA_FOUND  THEN
                             INSERT
                             INTO    alt_RatePlanPackages
                                                 (
                                                  alt_RatePlanCode,
                                                  alt_PackageCode,
                                                  RelationshipCode,
                                                  IsRuntimeRelationship,
                                                  LastModBy,
                                                  LastModDate
                                                  )
                             VALUES  (
                                                  rSoc.alt_RatePlanCode,
                                                  rSoc.alt_PackageCode,
                                                  rSoc.RelationshipCode,
                                                  'Y',
                                                  'EADB',
                                                  SYSDATE
                                                   );

                  END;

                END IF;

           END LOOP;
           COMMIT;

                  EXCEPTION
                WHEN  OTHERS  THEN
                      ROLLBACK;
     END;

   END;

   /************************************
    *                                  *
    *         importRatePlans          *
    *                                  *
    ************************************/
  PROCEDURE
            importRatePlans

/*
This section extracts all standard  Sampson price plans that are available to CSM and Watson
and assigns Watson names to various Sampson codes.  Price plans
that are available to CSM are those that meet the following criteria:
?       The SOC is valid for CSM - SOC soc_appearance flag = 'Y'
?       The SOC is available - SOC soc_status = 'A'
?       The SOC is for sale - SOC for_sale_ind = 'Y'
?       The SOC is a price plan - SOC service_type in 'P' or 'M'
?       This SOC is a service plan - rated_feature feature_code = STD
?       The SOC has a price or features associated with it - the SOC appears in rated_feature, pp_rc_rate, and SOC tables
?       SOC dates are valid
*/

  /* Changed By Tanuja  Date 12/05/02
     Added changes for Enabler - CR0659
  */

AS
CURSOR  cRatePlans
IS      SELECT  tbl.soc,
                tbl.alt_RatePlanCode,
                m.MarketID,
                NVL( rpt.RatePlanTypeCode, 'X' )   AS RatePlanTypeCode,    -- If not found, then assume general
                NVL( st.ServiceTypeCode, 'PCS' )   AS ServiceTypeCode,     -- Default is "PCS"
                NVL( al.ActivationLevelCode, 'S' ) AS ActivationLevelCode, -- Default is "Service" level
                tbl.soc_description,
                NVL( c.ContractId, 3 )             AS DefaultContractId,   -- Default is 12 months
                tbl.rate,
                tbl.sale_eff_date,
                tbl.sale_exp_date,
                tbl.min_req_ctns, --cr366b
                tbl.max_allowed_ctns,  --cr366b
                                tbl.external_id     -- CR0659
        FROM    Market          m,
                Contract        c,
                RatePlanType    rpt,
                ServiceType     st,
                ActivationLevel al,
               (
                SELECT  sE.soc,
                        RTRIM( sE.soc )                         AS alt_RatePlanCode,
                        RTRIM( sE.soc_description )             AS soc_description,
                        RTRIM( mE.sub_market )                  AS sub_market,
                        RTRIM(sE.soc_level_code)                AS soc_level_code,
                        sE.customer_type || sE.customer_subtype AS alt_RatePlanTypeCode,
                        sE.product_type || sE.product_sub_type  AS alt_ServiceTypeCode,
                        sE.minimum_no_months,
                        prE.rate,
                        sE.sale_eff_date,
                        sE.sale_exp_date,
                        sE.min_req_ctns,
                        sE.max_allowed_ctns,
                                                sE.external_id    -- CR0659
                FROM    soc@Ensemble sE,
                        soc_sub_market@Ensemble mE,
                        rated_feature@Ensemble rfE,
                        pp_rc_rate@Ensemble prE
                WHERE   sE.soc             = mE.soc (+)
                  AND   sE.effective_date  = mE.effective_date (+)
                  AND   sE.soc             = rfE.soc
                  AND   sE.effective_date  = rfE.effective_date
                  AND   sE.soc             = prE.soc
                  AND   sE.effective_date  = prE.effective_date
                  AND   rfE.soc            = prE.soc
                  AND   rfE.effective_date = prE.effective_date
                  AND   sE.effective_date  = prE.effective_date
                  and   prE.rc_type = 'R' --10/5/02 bug fix
                  AND   rfE.feature_code   = prE.feature_code
                  -- AND   rfE.feature_code   = 'STD'         -- this is the pricing feature for the rate plan
                  -- AND   prE.feature_code   = 'STD'         -- this is the pricing feature for the rate plan
                                  AND   rfE.feature_code   IN ('STD', 'DATARC')  -- this is the pricing feature for the rate plan
                  AND   prE.feature_code   IN ('STD', 'DATARC')         -- this is the pricing feature for the rate plan
                  AND   (                                  -- soc appears in CSM
                             sE.soc_appearance IS NULL
                        OR   sE.soc_appearance  IN  ( 'A', 'C' )
                        )
                  AND   sE.service_type  IN  ( 'P', 'M' )  -- soc is a price plan
                  AND   sE.soc_status    = 'A'             -- soc is available
                  AND   sE.for_sale_ind  = 'Y'             -- soc is for sale
                  AND    sE.effective_date < SYSDATE
                  AND   (sE.expiration_date  IS NULL  OR   sE.expiration_date > SYSDATE  )
                  AND   rfE.effective_date < SYSDATE
                  AND   (rfE.expiration_date IS NULL  OR  rfE.expiration_date > SYSDATE  )
                  AND   prE.effective_date < SYSDATE
                  AND   (prE.expiration_date IS NULL  OR  prE.expiration_date > SYSDATE  )
                  AND   (mE.expiration_date  IS NULL  OR   mE.expiration_date > SYSDATE  )
                  AND   (se.sale_exp_date is NULL or se.sale_exp_date > sysdate )  --ba
               ) tbl
        WHERE   (
                     tbl.sub_market = m.alt_MarketCode
                 OR  tbl.sub_market IS NULL
                )
          AND   tbl.minimum_no_months    = c.Length (+)
          AND   tbl.alt_RatePlanTypeCode = rpt.alt_RatePlanTypeCode (+)
          AND   tbl.alt_ServiceTypeCode  = st.alt_ServiceTypeCode (+)
          AND   tbl.soc_level_code       = al.alt_ActivationLevelCode (+) ;


-- lh 4/7/03 changes for smart access

CURSOR cSpendingLimit
IS
  	   SELECT rp.rateplanid,
	   		  rp.expired,
	   		  mpN.crd_limit_susp_amt as spendinglimit
	   FROM   rateplanavailability rpa,
	   		  rateplan rp,
	 		  market m,
	 		  market_policy@numbers mpN
       WHERE  rpa.rateplantypecode in ('b','i')
         AND  rpa.expired = 'N'
		 AND  rpa.rateplanid = rp.rateplanid
		 AND  rp.rateplanid > 0
		 AND  rp.marketid = m.marketid
		 AND  m.alt_marketcode = mpN.market_code
		 FOR UPDATE OF rp.RateplanID, rp.spendinglimit, rp.expired;


rMatchRatePlan         RatePlan%ROWTYPE;
vRatePlanID            RatePlan.RatePlanID%TYPE;

-- lh 10/22/02 added to correct problem with expired rate plans
vStandardBucket        RatePlan.StandardBucket%TYPE := 0;
vOtherBucket           RatePlan.OtherBucket%TYPE := 0;
vAdditionalCostPerUnit RatePlan.AdditionalCostPerUnit%TYPE := 0;
vMTMBucket             RatePlan.MTMBucket%TYPE := 0;
vStatusCode            RatePlan.StatusCode%TYPE := 'N';
vMinNumServices        RatePlan.MinNumServices%TYPE ;
vMaxNumServices        RatePlan.MaxNumServices%TYPE ;
-- end of change....

vNewRatePlan    Boolean ;
vMatchRatePlan  Boolean ;

BEGIN

   vNewRatePlan := FALSE;
   /*  First we need to expire any Rate Plans which are old.
    */

  UPDATE  RatePlan rp
   SET     EndDate     = SYSDATE,
           EXPIRED     = 'Y',  --ba
           LastModBy   = 'WATSON',
           LastModDate = SYSDATE
   WHERE   ( rp.EndDate IS NULL  OR  rp.EndDate > SYSDATE )
     and   rp.expired = 'N'  --BA
     AND   rp.RatePlanID > 0      -- Do not cancel the DUMMY rate plans
     AND   NOT EXISTS(
               SELECT  1
               FROM    Market                m,
                       soc@Ensemble            sE,
                       soc_sub_market@Ensemble mE
               WHERE   rp.MarketID         = m.MarketID
                 AND   rp.alt_RatePlanCode = RTRIM( sE.soc )
                 AND   m.alt_MarketCode    = RTRIM( mE.sub_market )
                 AND   mE.soc              = sE.soc
                 AND   sE.soc_status       = 'A'               -- soc is available
                 AND   sE.for_sale_ind     = 'Y'               -- soc is for sale
				 AND   (                                       -- soc appears in CSM
                 			  		 sE.soc_appearance IS NULL
            					OR   sE.soc_appearance  IN  ( 'A', 'C' )
            				  )  --lh 1/14/03
     					AND   sE.service_type  IN  ( 'P', 'M' )  --  1/14/03 soc is a price plan
                 AND   mE.effective_date = sE.effective_date
                 AND   mE.effective_date < SYSDATE
                 AND  (mE.expiration_date IS NULL  OR  mE.expiration_date > SYSDATE  )
                 AND   sE.effective_date < SYSDATE
                 AND  (sE.expiration_date IS NULL  OR  sE.expiration_date > SYSDATE  )
                          AND   (se.sale_exp_date is NULL or se.sale_exp_date > sysdate ) --ba
                                 AND  rp.external_id = sE.external_id        -- CR0659
                 );
/*               SELECT  1
               FROM    Market                m,
                       soc@Ensemble            sE,
                       soc_sub_market@Ensemble mE
               WHERE   rp.MarketID         = m.MarketID
                 AND   rp.alt_RatePlanCode = RTRIM( sE.soc )
                 AND   m.alt_MarketCode    = RTRIM( mE.sub_market )
                 AND   mE.soc              = sE.soc
                 AND   sE.soc_status       = 'A'               -- soc is available
                 AND   sE.for_sale_ind     = 'Y'               -- soc is for sale
                 AND   sE.soc_appearance  NOT IN  ( 'B', 'N' ) -- soc appears in CSM
                 AND   mE.effective_date = sE.effective_date
                 AND   mE.effective_date < SYSDATE
                 AND  (mE.expiration_date IS NULL  OR  mE.expiration_date > SYSDATE  )
                 AND   sE.effective_date < SYSDATE
                 AND  (sE.expiration_date IS NULL  OR  sE.expiration_date > SYSDATE  )
                          AND   (se.sale_exp_date is NULL or se.sale_exp_date > sysdate ) --ba
				 AND  rp.external_id = sE.external_id        -- CR0659
                 ); */

   COMMIT;
               /*  Here we get the next RatePlanID to be used, just in case.
                */
   SELECT  NVL( MAX( RatePlanID ), 0 )
   INTO    vRatePlanID
   FROM    RatePlan
   WHERE   RatePlanID > 0;

   FOR  rRatePlan IN cRatePlans  LOOP

          vNewRateplan := FALSE ;
                  vMatchRatePlan := FALSE ;
                  -- lh 01/03/03 added to correct bug
                  vStandardBucket        := 0;
                  vOtherBucket           := 0;
                  vAdditionalCostPerUnit := 0;
                  vMTMBucket             := 0;
                  vStatusCode            := 'N';
                  vMinNumServices        := null ;
                  vMaxNumServices                := null ;
                  -- end of change....

        BEGIN
              SELECT  *
              INTO    rMatchRatePlan
              FROM    RatePlan
              WHERE   alt_RatePlanCode = rRatePlan.alt_RatePlanCode
              AND   (EndDate is null or EndDate > sysdate) --ba
                     AND   EXPIRED = 'N'  --BA
              AND   MarketID = rRatePlan.MarketID;

              IF(   rMatchRatePlan.RatePlanTypeCode   <> rRatePlan.RatePlanTypeCode
--lh 02/24/03                 OR rMatchRatePlan.Name               <> rRatePlan.soc_description
                 OR rMatchRatePlan.DefaultContractId  <> rRatePlan.DefaultContractId
                 OR rMatchRatePlan.Price              <> rRatePlan.rate
--lh 02/24/03                 OR rMatchRatePlan.StartDate          <> rRatePlan.sale_eff_date
                                 OR NVL(rMatchRatePlan.external_id, 0) <> NVL(rRatePlan.external_id, 0)    -- CR0659
--lh 02/24/03                 OR NVL( rMatchRatePlan.EndDate, '01-JAN-60' )    <>
--lh 02/24/03                                    NVL( rRatePlan.sale_exp_date, '01-JAN-60' )
                 OR NVL( rMatchRatePlan.ServiceTypeCode, 'zzz' )  <>
                                    NVL( rRatePlan.ServiceTypeCode, 'zzz' )
                 OR NVL( rMatchRatePlan.ActivationLevelCode, 'z' )<>
                                    NVL( rRatePlan.ActivationLevelCode, 'z' ))
                THEN
                         vNewRatePlan           := TRUE;
                         vStandardBucket        := rMatchRatePlan.StandardBucket; --lh 10/22/
                         vOtherBucket           := rMatchRatePlan.OtherBucket;    --lh 10/22
                         vAdditionalCostPerUnit := rMatchRatePlan.AdditionalCostPerUnit; --lh/10/22
                         vMTMBucket             := rMatchRatePlan.MTMBucket; --lh 10/22
                         vStatusCode            := rMatchRatePlan.StatusCode; --lh 10/22
                         vMinNumServices        := rMatchRatePlan.MinNumServices ; --lh 10/22
                                 vMaxNumServices        := rMatchRatePlan.MaxNumServices; --lh 10/22
				END IF ; --lh 02/24/03


				IF (vNewRatePlan = FALSE  --lh 02/24/03
				     and (rMatchRatePlan.Name  <> rRatePlan.soc_description
                           OR rMatchRatePlan.StartDate  <> rRatePlan.sale_eff_date
                           OR NVL( rMatchRatePlan.EndDate, '01-JAN-60' )    <>
                                   NVL( rRatePlan.sale_exp_date, '01-JAN-60' )
						   )
				  )

				 THEN
			         UPDATE RatePlan
               		 SET    Name 	  =  rRatePlan.soc_description,
                       		StartDate = rRatePlan.sale_eff_date,
							EndDate   = rRatePlan.sale_exp_date,
							LastModBy = 'WATSON',
           					LastModDate = SYSDATE
              		 WHERE  RatePlanID = rMatchRatePlan.RatePlanID ;
  			    END IF;   --lh 01/03/2003

				IF (vNewRatePlan = TRUE )
				THEN


                         UPDATE RatePlan
                         SET    EndDate = SYSdate,
                                EXPIRED = 'Y'
                         WHERE  RatePlanID = rMatchRatePlan.RatePlanID ;
--ba
                         UPDATE  RatePlanAvailability rpa
                         SET     EndDate = SYSdate,
                                 expired = 'Y'
                         WHERE   RatePlanID = rMatchRatePlan.RatePlanID ;

                         COMMIT;
              END IF;

        EXCEPTION
             WHEN  NO_DATA_FOUND
             THEN
                   vNewRatePlan := TRUE;
        END ;


        IF  (vNewRatePlan and not vMatchRatePlan )
        THEN
                BEGIN

                                   vRatePlanID               := vRatePlanID + 1;
                   rMatchRatePlan.RatePlanID := vRatePlanID;

                   INSERT
                   INTO    RatePlan
                                        (
                                         RatePlanID,
                                         alt_RatePlanCode,
                                         MarketID,
                                         RatePlanTypeCode,
                                         ServiceTypeCode,
                                         ActivationLevelCode,
                                         StatusCode,
                                         Name,
                                         DefaultContractID,
                                         ZoneCode,
                                         StandardBucket,
                                         OtherBucket,
                                         AdditionalCostPerUnit,
                                         MTMBucket,   --lh 2/5/02
                                          Price,
                                          StartDate,
                                          EndDate,
                                          LastModBy,
                                          LastModDate,
                                          Expired,
                                          MinNumServices, --cr366b
                                          MaxNumServices,  --cr366b
                                                                                  external_id      -- CR0659
                                        )
                   VALUES  (
                                          vRatePlanID,
                                          rRatePlan.alt_RatePlanCode,
                                          rRatePlan.MarketID,
                                          rRatePlan.RatePlanTypeCode,
                                          rRatePlan.ServiceTypeCode,
                                          rRatePlan.ActivationLevelCode,
                                          vStatusCode,       --lh 10/22
                                          rRatePlan.soc_description,
                                          rRatePlan.DefaultContractID,
                                          'DEF',
                                           vStandardBucket,  -- lh 10/22
                                           vOtherBucket,     --lh 10/22
                                           vAdditionalCostPerUnit, --lh 10/22
                                           vMTMBucket,       --lh 10/22
                                          rRatePlan.rate,
                                          rRatePlan.sale_eff_date,
                                          rRatePlan.sale_exp_date,
                                          'WATSON',
                                           SYSDATE,
                                           'N',
                                          vMinNumServices, --lh 10/22
                                                                                  vMaxNumServices, --lh 10/22
                                                                                  rRatePlan.external_id  -- CR0659
                                                                                   --        rRatePlan.min_req_ctns,  --cr366b
                                           --        rRatePlan.max_allowed_ctns --cr366b
                                    );

--   DBMS_OUTPUT.PUT_LINE( TO_CHAR(SYSDATE,'mm/dd/yyyy hh24:mi') || ':  ' || rRatePlan.alt_RatePlanCode );

         /*  Now we expire any RatePlan types for which
                     the RatePlan is no longer available...
          */
         /*  ...and create any RatePlan types for which the RatePlan is not yet defined.
          */
                   DECLARE
                   CURSOR  cRatePlanAvailability
                   IS      SELECT  rpt.RatePlanTypeCode
                           FROM    soc_acc_type@Ensemble satE,
                                   RatePlanType rpt
                           WHERE   satE.soc = rRatePlan.soc
                             AND   satE.acc_type || satE.acc_sub_type = rpt.alt_RatePlanTypeCode
                             AND   NOT EXISTS(
                                       SELECT  1
                                       FROM    RatePlanAvailability rpa
                                       WHERE   rpa.RatePlanID = rMatchRatePlan.RatePlanID
                                         AND   rpa.RatePlanTypeCode = rpt.RatePlanTypeCode
                                         AND   ( EndDate IS NULL  OR  EndDate > SYSDATE )
                                    );

                   BEGIN

                         FOR  rRPAvailability IN cRatePlanAvailability  LOOP

                              INSERT
                                INTO    RatePlanAvailability(
                                                          RatePlanID,
                                                          RatePlanTypeCode,
                                                          StartDate,
                                                          Expired
                                                        )
                              VALUES  (
                                                          rMatchRatePlan.RatePlanID,
                                                          rRPAvailability.RatePlanTypeCode,
                                                          SYSDATE,
                                                          'N'
                                      );
                         END LOOP;
                   END;

        END;
        COMMIT;
      END IF;

   END LOOP;

   /*
     Finally, expire any rateplanavailability rows if the rateplans are expired:
   */

   UPDATE RatePlanAvailability rpa
   SET    EndDate = SYSDATE,
          Expired = 'Y'
   WHERE  (EndDate IS NULL or EndDate < SYSDATE)
     AND EXPIRED = 'N'
     AND  EXISTS
              (SELECT 1
                   FROM   RatePlan rp
                   WHERE  rp.RatePlanID = rpa.RatePlanID
                     AND  (rp.EndDate < SYSDATE AND Expired = 'Y')
                   ) ;
    COMMIT;

-- lh 4/7/03 changes for smart access

	FOR rSpendingLimit in cSpendingLimit LOOP
	    UPDATE RatePlan
	    SET    SpendingLimit=rSpendingLimit.SpendingLimit
		WHERE  Rateplanid = rSpendingLimit.Rateplanid
		AND    expired = rSpendingLimit.Expired ;

	END LOOP;
	COMMIT;


  END importRatePlans;




        /************************************
         *                                  *
         *          importPackages          *
         *                                  *
         ************************************/
        PROCEDURE
                importPackages
/*
Sampson regular SOCs that are available to CSM are packages in Watson.
The logic that extracts Watson rate plans and packages from Sampson is
similar except that a Watson package is a regular  SOC(service_type = 'R')
and may not always be available to CSM.  The package information extracted
to Watson also contains the recurring feature associated with each price plan
(for which there is only one). This criteria is evaluated as follows
?       SOC soc_appearance IS NOT evaluated so SOCs with a soc_appearance of N
        may be selected
?       This is a "regular" SOC - SOC service_type NOT in 'P' or 'M'(
        or service_type is B,O,R,S)
?       The feature code for this soc is in rated_feature and pp_rc_rate
        (this is the recurring feature and rate associated with this price plan or package).
        This would return the high level charge name for the charge that is billed each month.
        This charge may contain charges within it, but these are not billed explicitly.
        An example is DPKG (data package) which, for the soc GPRSPAY2, contains the features
        BUCKT, DD, and WAP.  In the package GRPSPAY3 the DPKG (data package) contains BUCKT, DD, WAP
        and the additional feature PREMIN.

Additionally, the following criteria, which is also applied to Watson
rate plans is applied to Watson packages
?       SOC soc_status = 'A' - this SOC is available
?       SOC for_sale_ind = 'Y' - this SOC is for sale
?       SOC dates are valid
*/

  /* Changed By Tanuja  Date 12/05/02
     Added changes for Enabler - CR0659
  */

 AS
CURSOR  cPackages
IS      SELECT  RTRIM( prE.feature_code )          AS feature_code,
                RTRIM( sE.soc )                    AS soc,
                RTRIM( sE.soc_description )        AS soc_description,
                pt.PackageTypeCode,
                NVL( st.ServiceTypeCode, 'PCS' )   AS ServiceTypeCode,     -- Default is "PCS"
                NVL( al.ActivationLevelCode, 'S' ) AS ActivationLevelCode, -- Default is "Service" level
                prE.rate,
                sE.sale_eff_date,
                sE.sale_exp_date,
                sE.external_id,         -- CR0659
				ptE.duration,  --lh 4/23/03
				ptE.duration_ind --lh 4/23/03
        FROM    soc@Ensemble           sE,
                rated_feature@Ensemble rfE,
                pp_rc_rate@Ensemble    prE,
				promotion_terms@Ensemble ptE, --lh 4/23/03
                PackageType          pt,
                ServiceType          st,
                ActivationLevel      al
        WHERE   sE.soc   = rfE.soc
          AND   sE.soc   = prE.soc
          AND   rfE.soc  = prE.soc
          AND   sE.soc = ptE.soc (+)  --lh 4/23/03
          AND   sE.effective_date  = ptE.effective_date (+)  --lh 4/23/03
          AND   sE.effective_date  = rfE.effective_date
          AND   sE.effective_date  = prE.effective_date
          AND   rfE.effective_date = prE.effective_date
          AND   rfE.feature_code   = prE.feature_code
          AND   sE.service_type    = pt.alt_PackageTypeCode
          AND   sE.product_type || sE.product_sub_type = st.alt_ServiceTypeCode (+)
          AND   sE.soc_level_code = al.alt_ActivationLevelCode (+)
          AND   sE.soc_status     = 'A'                 -- soc is available
          AND   sE.service_type  NOT IN  ( 'P', 'M' )   -- soc is not a Price Plan
          AND   sE.for_sale_ind = 'Y'                   -- soc is for sale
                  AND   (se.sale_exp_date is NULL or se.sale_exp_date > sysdate )  --ba
          AND   sE.effective_date < SYSDATE
          AND  ( sE.expiration_date IS NULL  OR   sE.expiration_date > SYSDATE  )
          AND   rfE.effective_date < SYSDATE
          AND  (rfE.expiration_date IS NULL  OR  rfE.expiration_date > SYSDATE  )
          AND   prE.effective_date < SYSDATE
          AND  (prE.expiration_date IS NULL  OR  prE.expiration_date > SYSDATE  )
 ;

CURSOR  cOldPackages
IS      SELECT  PackageID
        FROM    Package p
        WHERE   ( EndDate IS NULL  OR  EndDate > SYSDATE )
                  AND   Expired = 'N'  -- ba
          AND   NOT EXISTS(
                    SELECT  1
                    FROM    soc@Ensemble sE
                    WHERE   p.alt_PackageCode = RTRIM( sE.soc )
                      AND   sE.soc_status   = 'A'         -- soc is available
                      AND   sE.for_sale_ind = 'Y'         -- soc is for sale
                      AND   sE.service_type  NOT IN  ( 'P', 'M' )   -- soc is not a Price Plan
                  --lh 02/24/03    AND   sE.soc_appearance  NOT IN  ( 'B', 'N' ) -- soc appears in CSM
                      AND   sE.effective_date < SYSDATE
                      AND  (sE.expiration_date IS NULL  OR  sE.expiration_date > SYSDATE  )
                                      AND   (se.sale_exp_date is NULL or se.sale_exp_date > sysdate )  --ba
                                          AND  sE.external_id = p.external_id                  -- CR0659
                       )
        FOR UPDATE OF  p.EndDate;

rMatchPackage   Package%ROWTYPE;
vPackageID      Package.PackageID%TYPE;
vNewPackage     Boolean ;

BEGIN



     /*  First we need to expire any Packages which are old.
     */
     FOR  rOldPackage IN cOldPackages  LOOP

          UPDATE  Package
          SET     EndDate     = SYSDATE,
                  Expired = 'Y',
                  LastModBy   = 'WATSON',
                  LastModDate = SYSDATE
          WHERE CURRENT OF cOldPackages;
     END LOOP;
     COMMIT;

     SELECT  NVL( MAX( PackageID ), 0 )
     INTO    vPackageID
     FROM    Package;

     FOR  rPackage IN cPackages  LOOP

         vNewPackage := FALSE;  --ba

          BEGIN
              BEGIN
                SELECT  *
                INTO    rMatchPackage
                FROM    Package
                WHERE   alt_PackageCode = rPackage.soc
                and   EXPIRED = 'N' ;  -- ba

                IF(   rMatchPackage.alt_PriceChargeCode   <> rPackage.feature_code
                   OR rMatchPackage.Name                  <> rPackage.soc_description
                   OR rMatchPackage.Price                 <> rPackage.rate
                   OR rMatchPackage.StartDate             <> rPackage.sale_eff_date
                   OR NVL( rMatchPackage.EndDate, '01-JAN-60' )     <>
                      NVL( rPackage.sale_exp_date, '01-JAN-60' )
                   OR NVL( rMatchPackage.ServiceTypeCode, 'zzz' )   <>
                      NVL( rPackage.ServiceTypeCode, 'zzz' )
                   OR NVL( rMatchPackage.ActivationLevelCode, '|' ) <>
                      NVL( rPackage.ActivationLevelCode, '|' )
                                   OR NVL( rMatchPackage.external_id, 0 ) <> NVL( rPackage.external_id, 0 ) )  -- CR0659
                THEN

                    vNewPackage := TRUE;

                    UPDATE  Package
                    SET     EndDate = SysDate,
                            Expired = 'Y'
                    WHERE   PackageID = rMatchPackage.PackageID;

                END IF;

             EXCEPTION
                   WHEN  NO_DATA_FOUND
                   THEN  vNewPackage := TRUE;  --ba
             END ;
--ba
                 IF vNewPackage
                 THEN


                 vPackageID := vPackageID + 1;

                 INSERT
                 INTO    Package
                                (
                                 PackageID,
                                 alt_PackageCode,
                                 alt_PriceChargeCode,
                                 PackageTypeCode,
                                 ServiceTypeCode,
                                 ActivationLevelCode,
                                 Name,
                                 Price,
                                 StartDate,
                                 EndDate,
                                 LastModBy,
                                 LastModDate,
                                 Expired,
								 External_Id,   -- CR0659
								 Duration,  --lh 4/23/03
								 Duration_ind --lh 4/23/03
                                 )
                  VALUES  (
                                  vPackageID,
                                  rPackage.soc,
                                  rPackage.feature_code,
                                  rPackage.PackageTypeCode,
                                  rPackage.ServiceTypeCode,
                                  rPackage.ActivationLevelCode,
                                  rPackage.soc_description,
                                  rPackage.rate,
                                  rPackage.sale_eff_date,
                                  rPackage.sale_exp_date,
                                  'WATSON',
                                   SYSDATE,
                                   'N',
                                  rPackage.external_id,   -- CR0659
  							      rPackage.duration,  --lh 4/23/03
								  rPackage.duration_ind --lh 4/23/03

                            );
                     END IF;
                 END ;
                 COMMIT;

     END LOOP;
     COMMIT;

         /*  To be safe, we remove any dependent relationships on any
          *  expired packages, but we just keep all other relationships
          *  since these should be harmless.
          */

     Update  PackageRelationships
     Set     EndDate = SysDate,
             expired = 'Y' --ba
     WHERE   Expired = 'N'
     and     RelatedPackageID  IN  (
                   SELECT  PackageId
                   FROM    Package
                   WHERE    (ENDDATE < SysDate)
                   and    EXPIRED = 'Y'
            ) ;

     Update  PackageRelationships
     Set     EndDate = SysDate,
             expired = 'Y' --ba
     WHERE   Expired = 'N'
         AND     PackageID  IN  (
                   SELECT  PackageId
                   FROM    Package
                   WHERE    (ENDDATE < SysDate)
                   and    EXPIRED = 'Y'
            ) ;


        END importPackages;



    /************************************
     *                                  *
     *           importCharges          *
     *                                  *
     ************************************/
  PROCEDURE
       importCharges

/*
Watson charges are equivalent to Sampson features.
The batch evaluates features in the following ways:
?       If a feature has the csm_param_req_ind = 'Y' then this
        feature is placed in alt_charge and in charge.  Data in
        the alt_charge table is only used during the batch to get
        the feature attribute name which only apply to these charges.
        The additional screen information (Language and ESN(electronic
        serial number) ).  There are only 3 alt charges and these are VM1,
        VM2 and AMPS (voice mail, enhanced voice mail and dual mode)  and
        these require additional screen information (Language and ESN (electronic
        serial number) ).
?       If a feature has the dup_ftr_allow_ind = 'N' and the mps_feature_code
        is not null, then this feature conflicts with another.  These are still
        entered as charges, but they are also flagged as charge relationships and
        used later in the batch.
?       If the feature feature_code is ACT and the feature feature_group is OC
        (which is a charge type code of FEE)  then we will change the charge type
        to 'ACT'.  This is done to override the Watson default charge type of  FEE
        and reflect that this is an activation charge.
?       If there are free minute types (such as rolling minutes, included minutes,
        use them or loose them, first free minute) associated with this charge then
        the charge type, by default is minutes (MIN).  This is determined if the
        feature fm_type is not null.
*/

/* Changed By Tanuja  Date 12/05/02
     Added changes for Enabler - CR0659
  */

AS
CURSOR  cAltCharges
IS      SELECT  RTRIM( feature_code )                                   AS feature_code,
                DECODE( msisdn_ind, 'Y', 'MSISDN', '<NOT DETERMINED>' ) AS msisdn_ind
        FROM    feature@Ensemble fE
        WHERE   csm_param_req_ind = 'Y'
          AND   NOT EXISTS(
                    SELECT  1
                    FROM    alt_Charge Ac
                    WHERE   Ac.alt_ChargeCode = RTRIM( fE.feature_code )
                );

CURSOR  cCharges
IS      SELECT  RTRIM( fE.feature_code )                    AS feature_code,
                ct.ChargeTypeCode,          -- This may not be the "final" ChargeType
                fE.mps_rating_priority,     -- Not sure what this will be used for yet
                LTRIM(RTRIM( fE.feature_desc ))             AS feature_desc,
                ct.NumOccurrences,
                0.00                                        AS price,
                DECODE( dup_ftr_allow_ind,
                        'N', RTRIM( fE.mps_feature_code ) ) AS mps_feature_code,
                NVL( online_display_ind, 'Y' )              AS online_display_ind,
                pt.ProrateTypeCode,
                NVL( st.ServiceTypeCode, 'PCS' )            AS ServiceTypeCode, -- Default is "PCS"
                Ac.FeatureAttributeName,
                fE.fm_type,
                NVL( fE.msisdn_ind, 'N' )                   AS msisdn_ind,
                DECODE( fE.switch_act_needed,
                        'Y', fE.switch_code, NULL )          AS switch_insert,
                DECODE( fE.switch_act_needed,
                        'Y', fE.switch_code, NULL )          AS switch_delete,
                                fE.External_Ind             -- CR0659
        FROM    feature@Ensemble fE,
                ChargeType     ct,
                ProrateType    pt,
                ServiceType    st,
                alt_Charge     Ac
        WHERE   RTRIM( fE.proration_ind ) = pt.alt_ProrateTypeCode (+)
          AND   RTRIM( fE.feature_code )  = Ac.alt_ChargeCode (+)
          AND   RTRIM( fE.feature_group ) = ct.alt_ChargeTypeCode
          AND   fE.product_type || fE.product_sub_type = st.alt_ServiceTypeCode (+)
          AND   ct.IsDefault = 'Y';
                  -- AND        fE.External_Ind = 'N';   -- added by tanuja 12/13

CURSOR  cOldCharges
IS      SELECT  ChargeID
        FROM    Charge c
        WHERE   ( EndDate IS NULL  OR  EndDate > SYSDATE )
           AND   NOT EXISTS(
                     SELECT  1
                     FROM    feature@Ensemble fE
                     WHERE   c.alt_ChargeCode = RTRIM( fE.feature_code )
                  )
        FOR UPDATE OF c.EndDate;

   /*  Here we just want to get a list of all conflicts where the Package
    *  (only PackageID, not RelatedPackageID), contains the Charge given.
    */
CURSOR  cOldConflictsCursor(pChargeID   IN      Charge.ChargeID%TYPE )
IS      SELECT  pr.PackageId,
                pr.RelatedPackageId,
                pr.RelationshipCode,
                pr.DefaultRelationshipCode
        FROM    PackageRelationships pr,
                PackageCharges pc
        WHERE   pr.PackageId = pc.PackageId
          AND   pc.ChargeID = pChargeID
          AND   pr.DefaultRelationshipCode IS NOT NULL
          AND   ( pr.EndDate IS NULL  OR  pr.EndDate > SYSDATE ) --ba
          AND   ( pc.EndDate IS NULL  OR  pc.EndDate > SYSDATE )
       FOR UPDATE OF
                  pr.RelationshipCode,
                  pr.DefaultRelationshipCode;

CURSOR  cConflicts(
                   pChargeID       IN      Charge.ChargeID%TYPE,
                   pFeatureCode    IN      feature.mps_feature_code@Ensemble%TYPE
                  )
IS      SELECT  c.ChargeID,
                c.alt_ChargeCode,
                DECODE( pChargeID, c.ChargeID, 'X', '?')        AS RelationshipCode
        FROM    Charge c
        WHERE   c.alt_ConflictCode = pFeatureCode
                  and   enddate is null ; -- ba


vNewRelationshipCode    Relationship.RelationshipCode%TYPE;
rMatchCharge            Charge%ROWTYPE;
vChargeID               Charge.ChargeID%TYPE;
vOLDChargeID            Charge.ChargeID%TYPE;
vChargeType             Charge.ChargeTypeCode%TYPE;
vChargeSubType          Charge.ChargeSubTypeCode%TYPE;
vFeatureAttributeName   Charge.FeatureAttributeName%TYPE;
vIsNewConflict          boolean;
vRelationshipCode       Relationship.RelationshipCode%TYPE;

  BEGIN
     /*  First we have to determine which charges require additional
      *  information, and put them into the alt_Charge table.
      */

        DELETE
        FROM    alt_Charge Ac
        WHERE   NOT EXISTS(
                    SELECT  1
                    FROM    feature@Ensemble fE
                    WHERE   Ac.alt_ChargeCode = RTRIM( fE.feature_code )
                      AND   fe.csm_param_req_ind = 'Y'
                   );


        FOR  rAltCharge IN cAltCharges  LOOP

             INSERT
             INTO    alt_Charge(
                                alt_ChargeCode,
                                FeatureAttributeName,
                                Description,
                                LastModBy,
                                LastModDate
                               )
             VALUES  (
                                 rAltCharge.feature_code,
                                 rAltCharge.msisdn_ind,
                                'This charge requires additional information for the switch',
                                'WATSON',
                                 SYSDATE
                        );

             DBMS_OUTPUT.PUT_LINE('WARNING:  Charge '  || rAltCharge.feature_code || ' requires additional information.' );

        END LOOP;
        COMMIT;

       /*  Now we need to expire any Packages which are old.
        */
        FOR rOldCharge IN cOldCharges LOOP

             UPDATE  Charge
             SET     EndDate = SYSDATE
             WHERE CURRENT OF  cOldCharges;

        END LOOP;
        COMMIT;


        FOR rCharge IN cCharges LOOP

            vChargeSubType := NULL;
            IF  rCharge.ChargeTypeCode = 'FEE'  AND
                rCharge.feature_code LIKE 'ACT%'
            THEN
                vChargeType := 'ACT';
            ELSIF  rCharge.fm_type IS NOT NULL
            THEN
                 vChargeType := 'MIN';

                   BEGIN
                         SELECT  ChargeSubTypeCode
                         INTO    vChargeSubType
                         FROM    ChargeSubType
                         WHERE   alt_ChargeSubTypeCode = rCharge.fm_type;
                   EXCEPTION
                        WHEN  NO_DATA_FOUND  THEN
                              vChargeSubType := '*';
                   END;

            ELSE
                   vChargeType := rCharge.ChargeTypeCode;
            END IF;

            SELECT  NVL( rCharge.FeatureAttributeName,
                                    DECODE( rCharge.msisdn_ind, 'Y', 'MSISDN' ) )
            INTO    vFeatureAttributeName
            FROM    dual;

            vIsNewConflict := FALSE;

            BEGIN
                   SELECT  *
                   INTO    rMatchCharge
                   FROM    Charge
                   WHERE   alt_ChargeCode = rCharge.feature_code
                     AND   (enddate > sysdate or enddate is null );  --ba

                   vChargeID := rMatchCharge.ChargeID;

                   IF  NVL( rMatchCharge.alt_ConflictCode, 'ggg' )  <>
                       NVL( rCharge.mps_feature_code, 'ggg' )
                   THEN

               /*  If the alt_ConflictCode has changed, then we need to remove all old
                *  conflicts with this charge.  Later, we can create the new ones, if any.
                */
                        vIsNewConflict := TRUE;
--ba
                        vOldChargeID := vChargeID ;

                        UPDATE ChargeRelationships
                                SET    enddate = sysdate,
                                lastmodby = 'WATSON'
                        WHERE   ( ChargeId = vChargeID  OR  RelatedChargeID = vChargeID )
                          AND   DefaultRelationshipCode IS NOT NULL;

                    END IF;

                    IF(   vIsNewConflict
                       OR rMatchCharge.ChargeTypeCode    <> vChargeType
                       OR rMatchCharge.ChargeSubTypeCode <> vChargeSubType
                       OR rMatchCharge.Priority          <> rCharge.mps_rating_priority
                       OR rMatchCharge.Description       <> INITCAP( rCharge.feature_desc )
                       OR rMatchCharge.NumOccurrences    <> rCharge.NumOccurrences
                       OR rMatchCharge.DefaultPrice      <> rCharge.price
                       OR rMatchCharge.IsDisplayedOnBill <> rCharge.online_display_ind
                                           OR NVL (rMatchCharge.External_Ind, 'g') <> NVL (rCharge.External_Ind, 'g')    -- CR0659
                       OR NVL( rMatchCharge.ProrateTypeCode, 'g' ) <>
                          NVL( rCharge.ProrateTypeCode, 'g' )
                       OR rMatchCharge.RequiresNewPhoneNumber <> rCharge.msisdn_ind
                       OR rMatchCharge.SwitchAddCommand       <> rCharge.switch_insert
                       OR rMatchCharge.SwitchDeleteCommand    <> rCharge.switch_delete
                       OR NVL( rMatchCharge.FeatureAttributeName, 'ggg' ) <>
                          NVL( vFeatureAttributeName, 'ggg' )
                       OR rMatchCharge.EndDate   IS NOT NULL  )
                    THEN                           -- new ba

                                              SELECT  NVL( rCharge.FeatureAttributeName,
                                    DECODE( rCharge.msisdn_ind, 'Y', 'MSISDN' ) )
                                  INTO    vFeatureAttributeName
                                                  FROM    dual;

                          UPDATE  Charge
                          SET     enddate = sysdate
                          WHERE   ChargeID = rMatchCharge.ChargeID ;

                          SELECT  seq_Charge.NextVal
                          INTO    vChargeID
                          FROM    dual;

                          INSERT
                          INTO    Charge(
                                      ChargeID,
                                      ChargeTypeCode,
                                      ChargeSubTypeCode,
                                      ServiceTypeCode,
                                      Priority,
                                      Name,
                                      Description,
                                      alt_ChargeCode,
                                      alt_ConflictCode,
                                      Frequency,
                                      NumOccurrences,
                                      DefaultPrice,
                                      IsDisplayedOnBill,
                                      ProrateTypeCode,
                                      FeatureAttributeName,
                                      RequiresNewPhoneNumber,
                                      SwitchAddCommand,
                                      SwitchDeleteCommand,
                                      StartDate,
                                      EndDate,
                                      LastModBy,
                                      LastModDate,
                                                                          External_Ind    -- CR0659
                                  )
                           VALUES  (
                                      vChargeID,
                                      vChargeType,
                                      vChargeSubType,
                                      rCharge.ServiceTypeCode,
                                      rCharge.mps_rating_priority,
                                      SUBSTR( rCharge.feature_desc, 1, 40 ),
                                      INITCAP( rCharge.feature_desc ),
                                      rCharge.feature_code,
                                      rCharge.mps_feature_code,
                                      1,
                                      rCharge.NumOccurrences,
                                      rCharge.price,
                                      rCharge.online_display_ind,
                                      rCharge.ProrateTypeCode,
                                      vFeatureAttributeName,
                                      rCharge.msisdn_ind,
                                      rCharge.switch_insert,
                                      rCharge.switch_delete,
                                      SYSDATE,
                                      NULL,
                                      'WATSON',
                                      SYSDATE,
                                                                          rCharge.External_Ind   -- CR0659
                                );

                    END IF;

            EXCEPTION
                 WHEN  NO_DATA_FOUND  THEN
                       IF  rCharge.mps_feature_code IS NOT NULL  THEN
                           vIsNewConflict := TRUE;
                       END IF;

                       SELECT  seq_Charge.NextVal
                       INTO    vChargeID
                       FROM    dual;

                                       INSERT
                       INTO    Charge(
                                      ChargeID,
                                      ChargeTypeCode,
                                      ChargeSubTypeCode,
                                      ServiceTypeCode,
                                      Priority,
                                      Name,
                                      Description,
                                      alt_ChargeCode,
                                      alt_ConflictCode,
                                      Frequency,
                                      NumOccurrences,
                                      DefaultPrice,
                                      IsDisplayedOnBill,
                                      ProrateTypeCode,
                                      FeatureAttributeName,
                                      RequiresNewPhoneNumber,
                                      SwitchAddCommand,
                                      SwitchDeleteCommand,
                                      StartDate,
                                      EndDate,
                                      LastModBy,
                                      LastModDate,
                                                                          External_Ind     -- CR0659
                                  )
                       VALUES  (
                                      vChargeID,
                                      vChargeType,
                                      vChargeSubType,
                                      rCharge.ServiceTypeCode,
                                      rCharge.mps_rating_priority,
                                      SUBSTR( rCharge.feature_desc, 1, 40 ),
                                      INITCAP( rCharge.feature_desc ),
                                      rCharge.feature_code,
                                      rCharge.mps_feature_code,
                                      1,
                                      rCharge.NumOccurrences,
                                      rCharge.price,
                                      rCharge.online_display_ind,
                                      rCharge.ProrateTypeCode,
                                      vFeatureAttributeName,
                                      rCharge.msisdn_ind,
                                      rCharge.switch_insert,
                                      rCharge.switch_delete,
                                      SYSDATE,
                                      NULL,
                                      'WATSON',
                                      SYSDATE,
                                                                          rCharge.External_Ind       -- CR0659
                                );
                        DBMS_OUTPUT.PUT_LINE('New Charge #'  || vChargeID || ' (' || rCharge.feature_code || ') created of type ' || vChargeType );
            END;


            /*  Now that we've created/modified the charge, we need to create
             *  any dependencies for that charge.
             */
            IF  vIsNewConflict  THEN

                 /*  For each conflict Package which contained the old Charge, we
                  *  need to determine whether or not the conflict still exists.
                  *  This can be based upon another Charge in the Package, or
                  *  upon the same Charge with it's new conflict value.
                  */
                  FOR  rOldConflict  IN  cOldConflictsCursor(vOldChargeID)  LOOP

                      /*  We need the new relationship based on the updated
                       *  ChargeRelationships values.
                       */
                        SELECT  MAX(cr.RelationshipCode)
                        INTO    vRelationShipCode
                        FROM    ChargeRelationships cr,
                                PackageCharges      pc1,
                                PackageCharges      pc2
                        WHERE   cr.ChargeID        = pc1.ChargeID
                          AND   cr.RelatedChargeID = pc2.ChargeID
                          AND   pc1.PackageID      = rOldConflict.PackageId
                          AND   pc2.PackageID      = rOldConflict.RelatedPackageId
                          AND   ( pc1.EndDate IS NULL  OR  pc1.EndDate > SYSDATE )
                          AND   ( cr.EndDate  IS NULL  OR  cr.EndDate  > SYSDATE ) --ba
                          AND   ( pc2.EndDate IS NULL  OR  pc2.EndDate > SYSDATE );


                       /*  If the conflict no longer exists then we need to delete it and
                        *  its reflexive relationship.  Otherwise, we update the relationship
                        *  with the new value.
                        */

                        IF  vRelationshipCode IS NULL
                        THEN
                            UPDATE PackageRelationships
                             SET    enddate = sysdate,
                                                                        expired = 'Y',
                                    lastmodby = 'WATSON'
                             WHERE CURRENT OF cOLDConflictsCursor;

                             UPDATE PackageRelationships
                             SET    enddate = sysdate,
                                                                        expired = 'Y',
                                    lastmodby = 'WATSON'
                             WHERE  PackageID = rOldConflict.RelatedPackageID
                             AND    RelatedPackageID = rOldConflict.packageID;



                            /*  We need to delete both sides of the reflexive relationship
                             */

                         ELSE
                              SELECT  DECODE( rOldConflict.RelationshipCode,
                                      rOldConflict.DefaultRelationshipCode, vRelationshipCode,
                                      rOldConflict.RelationshipCode )
                              INTO    vNewRelationshipCode
                              FROM    dual;

-- new ba

                            UPDATE PackageRelationships
                             SET    enddate = sysdate,
                                                                        expired = 'Y',
                                    lastmodby = 'WATSON'
                             WHERE CURRENT OF cOLDConflictsCursor;

                             UPDATE PackageRelationships
                             SET    enddate = sysdate,
                                                                        EXPIRED = 'Y',
                                    lastmodby = 'WATSON'
                             WHERE
                                    PackageID = rOldConflict.RelatedPackageID
                                     AND    RelatedPackageID = rOldConflict.packageID;



                         END IF;
                  END LOOP;


                  IF  rCharge.mps_feature_code IS NOT NULL
                  THEN

                     dbms_output.put_line('new Conflict');
                     FOR  rConflicts  IN  cConflicts(vChargeID,rCharge.mps_feature_code)  LOOP

                          createChargeRelationship(
                                                   vChargeID,    --  First the charge which this depends upon...
                                                   rConflicts.ChargeID,
                                                   rCharge.feature_code,
                                                   rConflicts.alt_ChargeCode,
                                                   rCharge.mps_feature_code,
                                                   rConflicts.RelationshipCode
                                               );
                           /*  And now we create any Package inter-relationships which are based
                            *  upon the Charge relationship above.
                            */

                          createChargeRelationship(
                                                    rConflicts.ChargeID,
                                                    vChargeID,
                                                    rConflicts.alt_ChargeCode,
                                                    rCharge.feature_code,                                                    rCharge.mps_feature_code,
                                                    rConflicts.RelationshipCode
                                           );

                    END LOOP;

                  END IF;

            END IF;
            COMMIT;

        END LOOP;


     /*  To be safe, we remove any dependent relationships on any
      *  expired charges, but we just keep all other relationships
      *  since these should be harmless.
      */

-- ba
        UPDATE  ChargeRelationships
        SET     enddate = sysdate
        WHERE   RelatedChargeID  IN  (
                                    SELECT  ChargeId
                                    FROM    Charge
                                    WHERE   EndDate <= SYSDATE
                                ) ;

        UPDATE  ChargeRelationships
        SET     enddate = sysdate
        WHERE   ChargeID  IN  (
                                    SELECT  ChargeId
                                    FROM    Charge
                                    WHERE   EndDate <= SYSDATE
                                ) ;

   END importCharges;


   /************************************
    *                                  *
    *      importOfferParams           *
    *                                  *
    ************************************/

        PROCEDURE
            importOfferParams

/*
Import Offer Params procedure is a new procedure to import offer
parameters for all the unexpired Enabler Socs from Enabler
published csm_offer_param table - CR0659  12/08/02
*/
    AS

        /* CURSOR  cOfferParams
    IS      SELECT distinct RTRIM( copE.soc_cd )  AS soc_cd,
            RTRIM( copE.item_cd ) AS item_cd,
            RTRIM( copE.param_name ) AS param_name,
            RTRIM( copE.description ) AS description,
                        copE.populate_level AS populate_level,
            RTRIM( copE.param_category ) AS param_category,
            RTRIM( copE.param_type ) AS param_type
    FROM    soc@Ensemble           sE,
            csm_offer_param@enabler  copE
    WHERE   sE.external_id  = copE.soc_cd
    AND     sE.soc_status   = 'A'
    AND     sE.for_sale_ind = 'Y'
    AND     (se.sale_exp_date is NULL or se.sale_exp_date > sysdate )
    AND     sE.effective_date < SYSDATE
    AND     ( sE.expiration_date IS NULL
                                 OR sE.expiration_date > SYSDATE); */

        CURSOR  cOfferParams
    IS      SELECT RTRIM( copE.soc_cd )  AS soc_cd,
            RTRIM( copE.item_cd ) AS item_cd,
            RTRIM( copE.param_name ) AS param_name,
            RTRIM( copE.description ) AS description,
                        copE.populate_level AS populate_level,
            RTRIM( copE.param_category ) AS param_category,
            RTRIM( copE.param_type ) AS param_type
    FROM    csm_offer_param@enabler  copE;

   BEGIN

     /*  First we have to delete all records from csm_offer_param
      *  table in Watson database
      */
        DELETE
        FROM    CSM_OFFER_PARAM ;

       FOR rOfferParams IN cOfferParams  LOOP

             INSERT
             INTO  csm_offer_param
             (
                     soc_cd,
                     item_cd,
                     param_name,
                     description,
                         populate_level,
                     param_category,
                     param_type
             )
             VALUES
             (
                     rOfferParams.soc_cd,
                     rOfferParams.item_cd,
                     rOfferParams.param_name,
                     rOfferParams.description,
                         rOfferParams.populate_level,
                     rOfferParams.param_category,
                     rOfferParams.param_type
             );

       END LOOP;


        END importOfferParams;


        /************************************
         *                                  *
         *      importRatePlanPackages      *
         *                                  *
         ************************************/
        /* Changed By L.Hardy  Date 11/09/01
           added
           AND   (  srE.src_effective_date < sysdate and srE.dest_effective_date < sysdate )
           to the second insert into tmp_rateplanpackages in importRatePlanPackages to eliminate
           duplicate and invalid entries from the soc_relation table.

*/
        PROCEDURE
                importRatePlanPackages

/*
Import Rate Plan Packages has 2 sections.  The first creates rate plan
and package associations for dummy rate plans and the second creates the
same associations for real rate plans.  Dummy rate plans are those with a
rate plan id < 0.  Dummy rate plans are used to create an association between
regular SOCs and rate plans.  Regular SOCs are only associated with a market
and without dummy rate plans there would be no way to tie them together.   The
logic for building the dummy rate plan association is to select  all regular
SOCs (soc.service_type = 'R') with dummy rate plans (rate plan id < 0) even
though they may not have any features or recurring charges (rows in
rated_feature or pp_rc_rate) associated with them.  The first step is to
populate tmp_RatePlanPackages.  The logic for populating this is:
?       Select all dummy rate plans, which  are identified with a rate plan id < 0
?       Select all SOCs that are for sale (SOC for_sale_ind = 'Y')
?       Select all SOCs that are available (SOC soc_status = 'A')
?       Select all SOCs that are regular (SOC service_type = 'R')
?       Select all SOCs that are standard (package PackageTypeCode = 'STD')
?       Select all SOCs that have been assigned an account type (soc_acc_type.soc
        = soc.soc)
?       Select all SOCs with valid dates
?       For all SOCs meeting the above criteria create an association with all
        dummy rate plans for matching markets

The logic for building the non-dummy rate plan and package association is to
select  all non dummy rate plans (rate plan id > 0) that have related packages
even though they may not have any features or recurring charges associated
with them.  These rows differ from the dummy rate plan rows above in that
these rows must have an a relationship in the soc_relation table.  The first
step is to populate tmp_RatePlanPackages using the following logic:
?       Find all related packages and rate plans - If the rate plan code equals
        the soc_relation soc_soc and the package code equals the soc_relation
        soc_dest then this package is related to this rate plan.
?       If the package price is 0 then this package is included in this rate plan
        otherwise it may be selected for an additional charge.

Override the rate plan relationship
?       Overrides from Table Maintenance are kept in the alt_RatePlanPackages table.
        For Each matching rate plan and package in tmp_RatePlanPackages and
        alt_RatePlanPackages, update the tmp_RatePlanPackages relationship
        with that listed in the alternate table.
?       Assign the relationships in RatePlanPackages using the relationship in
        tmp_RatePlanPackages.

*/
 AS

CURSOR  cRPPRelationship
IS      SELECT  Arpp.RelationshipCode
        FROM    alt_RatePlanPackages Arpp,
                tmp_RatePlanPackages Trpp
        WHERE   Arpp.alt_RatePlanCode = Trpp.alt_RatePlanCode
          AND   Arpp.alt_PackageCode = Trpp.alt_PackageCode
          AND   Arpp.IsRuntimeRelationship = 'N'
        FOR UPDATE OF Trpp.RelationshipCode;

CURSOR  cOldRatePlanPackages
IS      SELECT  rpp.RatePlanID,
                rpp.PackageID,
                rpp.RelationshipCode,
                rpp.EndDate
        FROM    RatePlan rp,
                RatePlanPackages rpp
        WHERE   rp.RatePlanID = rpp.RatePlanID
          AND   rp.StartDate < SYSDATE
          AND   rpp.StartDate < SYSDATE
          AND   (  rp.EndDate IS NULL  OR   rp.EndDate > SYSDATE )
          AND   ( rpp.EndDate IS NULL  OR  rpp.EndDate > SYSDATE )
        FOR UPDATE OF  rpp.EndDate;

CURSOR  cNewRatePlanPackages
IS      SELECT  RatePlanID,
                PackageID,
                StartDate,
                EndDate,
                RelationshipCode,
                Price,
				MandatoryInd
        FROM    tmp_RatePlanPackages tmp
        WHERE   tmp.RelationshipCode <> 'E'
          AND   NOT EXISTS(
                    SELECT  1
                    FROM    RatePlanPackages rpc
                    WHERE   rpc.RatePlanID = tmp.RatePlanID
                      AND   rpc.PackageID = tmp.PackageID
                      AND   ( rpc.EndDate IS NULL  OR  rpc.EndDate > SYSDATE ));

-- ba
CURSOR  cExpiredRatePlanPackages
IS      SELECT  rpp.RatePlanID,
                rpp.PackageID,
                rpp.RelationshipCode,
                rpp.EndDate
        FROM    RatePlan rp,
                RatePlanPackages rpp
        WHERE   rp.RatePlanID = rpp.RatePlanID
          AND   rp.StartDate < SYSDATE
          AND   rpp.StartDate < SYSDATE
          AND   (  rp.EndDate < SYSDATE )
                  and   (  rp.expired = 'Y')
          AND   ( rpp.EndDate IS NULL  OR  rpp.EndDate > SYSDATE )
        FOR UPDATE OF  rpp.EndDate;



vRelationshipCode       alt_RatePlanPackages.RelationshipCode%TYPE;
vStartDate              RatePlanPackages.StartDate%TYPE;
vEndDate                RatePlanPackages.EndDate%TYPE;
vPriority               PackageType.Priority%TYPE;

   BEGIN
        DELETE
        FROM    tmp_RatePlanPackages;


         SELECT  Priority
         INTO    vPriority
         FROM    PackageType
         WHERE   PackageTypeCode = 'STD';


         /*  First we need to explicitly associate the regular socs with their
          *  corresponding dummy Rate Plan.
          */
         INSERT
         INTO    tmp_RatePlanPackages(
                             RatePlanID,
                             PackageID,
                             alt_RatePlanCode,
                             alt_PackageCode,
                             Price,
                             RelationshipCode,
                             Priority,
                             StartDate,
                             EndDate,
							 MandatoryInd
                           )
         SELECT  rp.RatePlanID,
                 p.PackageID,
                 rp.alt_RatePlanCode,
                 p.alt_PackageCode,
                 p.Price,
                 DECODE( tbl.soc_appearance, -- If it's not visible in CSM, then make the
                         'B', 'W',           --    relationship "Web Only".  We still need
                         'N', 'W',           --    to import it so it is available for the
                         'A' ),              --    EAI and conflict resolution. GG 8/21
                 vPriority,
                 tbl.sale_eff_date,
                 tbl.sale_exp_date,
				 'N'
         FROM    RatePlan rp,
                 RatePlanType rpt,
                 Package p,
                 Market m,
                (
                 SELECT  RTRIM( sE.soc )                             AS soc,
                         RTRIM( mE.sub_market )                      AS sub_market,
                         RTRIM( satE.acc_type || satE.acc_sub_type ) AS alt_RatePlanTypeCode,
                         NVL( al.ActivationLevelCode, 'S' )          AS ActivationLevelCode,
                         sE.soc_appearance,
                         sE.sale_eff_date,
                         sE.sale_exp_date
                 FROM    soc@Ensemble sE,
                         soc_sub_market@Ensemble mE,
                         soc_acc_type@Ensemble satE,
                         ActivationLevel al
                 WHERE   sE.soc   = satE.soc
                   AND   sE.soc   = mE.soc (+)
                   AND   sE.effective_date = mE.effective_date (+)
                   AND   sE.soc_status     = 'A'         -- soc is available
                   AND   sE.service_type   = 'R'         -- soc is a Regular soc
                   AND   sE.for_sale_ind   = 'Y'         -- soc is for sale
                   AND   sE.soc_level_code = al.alt_ActivationLevelCode (+)
                   AND   sE.effective_date < SYSDATE
                                   AND  (  sE.expiration_date IS NULL  OR  sE.expiration_date > SYSDATE  )
                                   AND  (  sE.sale_exp_date IS NULL  OR  sE.sale_exp_date > SYSDATE  )  --ba
                   AND   (  mE.expiration_date IS NULL  OR  mE.expiration_date > SYSDATE  )

                  ) tbl
         WHERE   rp.RatePlanID < 0                            -- only dummy rate plans are associated with regular socs
           AND   rp.RatePlanTypeCode = rpt.RatePlanTypeCode   -- we can do this because we are only dealing with dummy rate plans
           AND   rp.ActivationLevelCode = tbl.ActivationLevelCode
           AND   rp.MarketID = m.MarketID
           AND   p.alt_PackageCode = tbl.soc
           AND   p.PackageTypeCode = 'STD'                    -- regular soc
           AND   rpt.alt_RatePlanTypeCode = tbl.alt_RatePlanTypeCode
           AND   (
                     tbl.sub_market = m.alt_MarketCode
                  OR tbl.sub_market IS NULL
                   )
           AND   ( rp.EndDate IS NULL  OR  rp.EndDate > SYSDATE )
           AND   (  p.EndDate IS NULL  OR   p.EndDate > SYSDATE );

         COMMIT;

         /*  Now we need to associate the normal RatePlans with their optional socs.
          */
/*         INSERT
         INTO    tmp_RatePlanPackages(
                                     RatePlanID,
                                     PackageID,
                                     alt_RatePlanCode,
                                     alt_PackageCode,
                                     Price,
                                     RelationshipCode,
                                     Priority,
                                     StartDate,
                                     EndDate
                            )
         SELECT  rp.RatePlanID,
                 p.PackageID,
                 rp.alt_RatePlanCode,
                 p.alt_PackageCode,
                 p.Price,
-- future fix for  Remedy Ticket: 673588 decode(rtrim(sre.relation_type)||rtrim(sre.must_incl_ind),
--                                         'OY','I',
--                                                      DECODE( p.Price, 0, 'I', 'A' )),
                 DECODE( p.Price, 0, 'I', 'A' ),
                 pt.Priority,
                 maxDate( SYSDATE, p.StartDate ),
                 minDate( srE.expiration_date, p.EndDate )
         FROM    RatePlan rp,
                 Package p,
                 PackageType pt,
                 soc_relation@Ensemble srE
         WHERE   rp.RatePlanID > 0    -- only real rate plans have optional socs
           AND   rp.alt_RatePlanCode = RTRIM( srE.soc_src )
           AND   p.alt_PackageCode = RTRIM( srE.soc_dest )
           AND   p.PackageTypeCode = pt.PackageTypeCode
           AND   ( rp.EndDate IS NULL  OR  rp.EndDate > SYSDATE )
           AND   (  p.EndDate IS NULL  OR   p.EndDate > SYSDATE )
           AND   (  srE.expiration_date IS NULL  OR  srE.expiration_date > SYSDATE  )
           AND   (  srE.src_effective_date < sysdate and srE.dest_effective_date < sysdate );  -- LH 11/09

         COMMIT;

*/
         INSERT
         INTO    tmp_RatePlanPackages(
                                     RatePlanID,
                                     PackageID,
                                     alt_RatePlanCode,
                                     alt_PackageCode,
                                     Price,
                                     RelationshipCode,
                                     Priority,
                                     StartDate,
                                     EndDate,
									 MandatoryInd
                            )
         SELECT  rp.RatePlanID,
                 p.PackageID,
                 rp.alt_RatePlanCode,
                 p.alt_PackageCode,
                 p.Price,
                 DECODE( nvl(srE.must_incl_ind,'N'),
				 		 'N', 'A',
						 'Y','I'),
                 pt.Priority,
                 maxDate( SYSDATE, p.StartDate ),
                 minDate( srE.expiration_date, p.EndDate ),
				 srE.must_incl_ind
         FROM    RatePlan rp,
                 Package p,
                 PackageType pt,
                 soc_relation@Ensemble srE
         WHERE   rp.RatePlanID > 0    -- only real rate plans have optional socs
           AND   rp.alt_RatePlanCode = RTRIM( srE.soc_src )
           AND   p.alt_PackageCode = RTRIM( srE.soc_dest )
           AND   p.PackageTypeCode = pt.PackageTypeCode
           AND   ( rp.EndDate IS NULL  OR  rp.EndDate > SYSDATE )
           AND   (  p.EndDate IS NULL  OR   p.EndDate > SYSDATE )
           AND   (  srE.expiration_date IS NULL  OR  srE.expiration_date > SYSDATE  )
           AND   (  srE.src_effective_date < sysdate and srE.dest_effective_date < sysdate );  -- LH 11/09

         COMMIT;



         FOR  rOverrideRelationship  IN  cRPPRelationship  LOOP

              UPDATE  tmp_RatePlanPackages
              SET     RelationshipCode = rOverrideRelationship.RelationshipCode
              WHERE CURRENT OF    cRPPRelationship;

         END LOOP;



        /* GGhali:  Placed the call to importSocEligibility here.  At this point, we know that the
         *  tmp_RatePlanPackages table is completely built, but the RatePlanPackages table has
         *  not been modified yet.
         */
         importSocEligibility;



         FOR  rOldRPP IN cOldRatePlanPackages  LOOP
              vEndDate := NULL;

              BEGIN
                    SELECT  EndDate
                    INTO    vEndDate
                    FROM    tmp_RatePlanPackages
                    WHERE   RatePlanID = rOldRPP.RatePlanID
                      AND   PackageID = rOldRPP.PackageID
                      AND   RelationshipCode = rOldRPP.RelationshipCode
                      AND   RelationshipCode <> 'E';

              EXCEPTION
                   WHEN  NO_DATA_FOUND  THEN
                         vEndDate := SYSDATE;  -- If it doesn't exist we need to terminate immediately
              END;

              IF  NVL( vEndDate, '01-JAN-60' )  <>
                              NVL( rOldRPP.EndDate, '01-JAN-60' )
              THEN

                  UPDATE  RatePlanPackages
                  SET     EndDate = vEndDate
                  WHERE CURRENT OF  cOldRatePlanPackages;

               END IF;
         END LOOP;
         COMMIT;


         FOR  rNewRPP IN cNewRatePlanPackages  LOOP

           /*  First we make sure there isn't another row with exactly the same
            *  StartDate.  If there is, we just use it.
            */
               UPDATE  RatePlanPackages
               SET     RelationshipCode = rNewRPP.RelationshipCode, --GG 8/21
                       EndDate = rNewRPP.EndDate
               WHERE   RatePlanID = rNewRPP.RatePlanID
                 AND   PackageID = rNewRPP.PackageID
                 AND   StartDate = rNewRPP.StartDate;

               IF  SQL%ROWCOUNT = 0  THEN

                  /*  Now we know it's a new relationship, so we can safely create it.
                   */
                    INSERT
                    INTO    RatePlanPackages(
                                           RatePlanID,
                                           PackageID,
                                           StartDate,
                                           EndDate,
                                           RelationshipCode,
                                           Price,
										   MandatoryInd
                                            )
                     VALUES  (
                                           rNewRPP.RatePlanID,
                                           rNewRPP.PackageID,
                                           rNewRPP.StartDate,
                                           rNewRPP.EndDate,
                                           rNewRPP.RelationshipCode,
                                           rNewRPP.Price,
										   rNewRPP.MandatoryInd
                              );
               END IF;
               COMMIT;

         END LOOP;

--
                 FOR rExpiredRatePlanPackages in cExpiredRatePlanPackages LOOP
               UPDATE  RatePlanPackages
                 SET   EndDate = sysdate
               WHERE   RatePlanID = rExpiredRatePlanPackages.RatePlanID
                 AND   PackageID = rExpiredRatePlanPackages.PackageID ;

                 END  LOOP;




     END importRatePlanPackages;

        /************************************
         *                                  *
         *       importRatePlanPrice        *
         *                                  *
         ************************************/

PROCEDURE importRatePlanPrice
AS
CURSOR  cRatePlanPrice
IS 		SELECT rp.RatePlanID , nvl(SUM(p.Price),0)  as AdditionalCost
	   	FROM rateplanpackages rpp,
		  	 package p,
			 RatePlan rp
		WHERE (rpp.EndDate > sysdate or rpp.EndDate is null)
		AND (p.EndDate > sysdate or p.EndDate is null)
		AND rp.RatePlanID = rpp.RatePlanID
		AND p.PackageID = rpp.PackageID
		AND rpp.RelationshipCode = 'I'
		AND p.PackageTypeCode = 'OPT'
		AND (rp.EndDate IS NULL OR rp.EndDate > SYSDATE)
		AND rp.RatePlanID >= 0
		GROUP BY rp.RatePlanID;


       BEGIN

                FOR  rRatePlanPrice IN cRatePlanPrice  LOOP

				  UPDATE Rateplan rp
				  SET    TotalRecurringCharge = rp.Price + rRatePlanPrice.AdditionalCost
				  WHERE  rp.RatePlanID = rRatePlanPrice.RatePlanID
				  AND    (rp.EndDate > SYSDATE or rp.EndDate IS NULL);

				END LOOP;
		       COMMIT;


 END importRatePlanPrice;

        /************************************
         *                                  *
         *       importRatePlanCharges      *
         *                                  *
         ************************************/
        PROCEDURE
                importRatePlanCharges
/*
Extract features from Sampson associated with each Watson rate plan and
apply any override relationships that have been created through Table
Maintenance(TM).  The logic is:
?       Find all rated_feature rows for each soc/rate plan.
?       Find the override relationship code from TM in alt_RatePlanCharges.
?       Create charge relationship if there is no TM override, using the
        following rule:
        ?       Relationship code = '$' if this is the service for this rate plan
                (if the feature code in rated_feature = 'STD')
        ?       Relationship code = '-' if this feature is Custom Call (feature code
                in rated_feature = 'CPKG')
        ?       Relationship code = the relationship code in alt_RatePlanCharges
                table if the feature code is not STD or CPKG
        ?       If the feature code is not STD, CPKG and is not being overridden
                then it is assigned to 'I', for included.

*/
AS
CURSOR  cRatePlanCharges
IS      SELECT  m.alt_MarketCode,
                rp.alt_RatePlanCode,
                rp.RatePlanID,
                RTRIM( rfE.feature_code )       AS alt_ChargeCode,
                rfE.expiration_date                     AS sale_exp_date
        FROM    rated_feature@Ensemble rfE,
                RatePlan rp,
                Market m
        WHERE   rp.alt_RatePlanCode = RTRIM( rfE.soc )
          AND   rp.MarketID = m.MarketID
                  and   rp.expired = 'N' --ba
          AND   rfE.effective_date < SYSDATE
          AND  (  rfE.expiration_date IS NULL  OR  rfE.expiration_date > SYSDATE  );

vRelationshipCode       alt_RatePlanCharges.RelationshipCode%TYPE;

        BEGIN

                DELETE
                FROM    tmp_RatePlanCharges;

                FOR  rRatePlanCharge IN cRatePlanCharges  LOOP

                        BEGIN
                                SELECT  RelationshipCode
                                INTO    vRelationshipCode
                                FROM    alt_RatePlanCharges
                                WHERE   alt_RatePlanCode = rRatePlanCharge.alt_RatePlanCode
                                  AND   alt_ChargeCode = rRatePlanCharge.alt_ChargeCode;

                        EXCEPTION
                                WHEN  NO_DATA_FOUND  THEN
                                        vRelationshipCode := 'I';

                        END;


                        INSERT
                        INTO    tmp_RatePlanCharges(
                                                RatePlanID,
                                                ChargeID,
                                                alt_RatePlanCode,
                                                alt_ChargeCode,
                                                ChargeTypeCode,
                                                RelationshipCode,
                                                Price,
                                                StartDate,
                                                EndDate
                                        )
                        SELECT  rRatePlanCharge.RatePlanID,
                                ChargeID,
                                rRatePlanCharge.alt_RatePlanCode,
                                rRatePlanCharge.alt_ChargeCode,
                                ChargeTypeCode,
                                DECODE( rRatePlanCharge.alt_ChargeCode,
                                           'STD', '$',
                                           'CPKG', '-', vRelationshipCode ),
                                0.00,
                                SYSDATE,
                                rRatePlanCharge.sale_exp_date
                        FROM    Charge
                        WHERE   alt_ChargeCode = rRatePlanCharge.alt_ChargeCode
                          AND   ( EndDate IS NULL  OR  EndDate > SYSDATE );


                END LOOP;

                COMMIT;



                /*  First we terminate any charges which are no longer available
                 *  for the given Rate Plan, but only if the Rate Plan is still
                 *  active.
                 */
                UPDATE  RatePlanCharges rpc
                SET             EndDate = SYSDATE
                WHERE   ( EndDate IS NULL  OR  EndDate > SYSDATE )
                  AND   EXISTS(
                                        SELECT  1
                                        FROM    RatePlan rp
                                        WHERE   rpc.RatePlanID = rp.RatePlanID
                                          AND   rp.StartDate < SYSDATE
                                          AND   ( rp.EndDate IS NULL  OR  rp.EndDate > SYSDATE )
                                )
                  AND   NOT EXISTS(
                                        SELECT  1
                                        FROM    tmp_RatePlanCharges tmp
                                        WHERE   rpc.RatePlanID = tmp.RatePlanID
                                          AND   rpc.ChargeID = tmp.ChargeID
                                          AND   rpc.RelationshipCode = tmp.RelationshipCode
                                );

                COMMIT;



                /*  Now we get all the charges which are new
                 */
                INSERT
                INTO    RatePlanCharges(
                                        RatePlanID,
                                        ChargeID,
                                        StartDate,
                                        EndDate,
                                        RelationshipCode,
                                        Price
                                )
                SELECT  RatePlanID,
                                ChargeID,
                                StartDate,
                                EndDate,
                                RelationshipCode,
                                Price
                FROM    tmp_RatePlanCharges tmp
                WHERE   RelationshipCode <> 'X'         -- exclude "Excluded" charges
                  AND   NOT EXISTS(
                                        SELECT  1
                                        FROM    RatePlanCharges rpc
                                        WHERE   rpc.RatePlanID = tmp.RatePlanID
                                          AND   rpc.ChargeID = tmp.ChargeID
                                          AND   ( rpc.EndDate IS NULL  OR  rpc.EndDate > SYSDATE )
                                );

                COMMIT;

        END importRatePlanCharges;




        /************************************
         *                                  *
         *       importPackageCharges       *
         *                                  *
         ************************************/
        PROCEDURE
                importPackageCharges
/*
Assign default pricing for package features  (all SOCs with a service_type
not in 'P', or 'M').  All Samson SOCs that are not price plans are packages.
For each package, evaluate the feature code associated with it and identify if
this feature (or charge) is a service, a custom call package or included.  The
logic for implementing this is
?       Select all matching SOCs in SOC and rated_feature where the SOC
        service_type <> 'P' or 'M' and the dates are valid.  This will retrieve
        all features for this package, not just the high level feature (which may
        include other features that are not charged directly for).
?       If there is a row associated with this SOC in pp_rc_rate then there is an
        additional charge for this feature
?       If the rated_feature feature_code = 'CPKG' then this feature is available
        with the Custom Call package
?       If the feature code is not CPKG or is not in pp_rc_rate then this feature
        is included
*/
AS

CURSOR  cPackageCharges
IS      SELECT  RTRIM( sE.soc )                 AS soc,
                RTRIM( rfE.feature_code )       AS feature_code,
                rfE.expiration_date             AS sale_exp_date
        FROM    soc@Ensemble sE,
                rated_feature@Ensemble rfE
        WHERE   sE.soc = rfE.soc
          AND   sE.effective_date = rfE.effective_date
          AND   sE.service_type  NOT IN  ( 'P', 'M' )   -- soc is not a Price Plan
          AND   sE.effective_date < SYSDATE
          AND  (   sE.expiration_date IS NULL  OR   sE.expiration_date > SYSDATE  )
          AND   rfE.effective_date < SYSDATE
          AND  (  rfE.expiration_date IS NULL  OR  rfE.expiration_date > SYSDATE  );

   BEGIN

        DELETE
        FROM    tmp_PackageCharges;

        FOR  rPackageCharge IN cPackageCharges  LOOP

             INSERT
             INTO    tmp_PackageCharges(
                                       PackageID,
                                       ChargeID,
                                       alt_PackageCode,
                                       alt_ChargeCode,
                                       RelationshipCode,
                                       Price,
                                       StartDate,
                                       EndDate
                                       )
             SELECT  p.PackageID,
                     c.ChargeID,
                     rPackageCharge.soc,
                     rPackageCharge.feature_code,
                     DECODE( rPackageCharge.feature_code,
                                p.alt_PriceChargeCode, '$',
                                'CPKG', '-',
                                'I' ),
                     0.00,
                     maxDate( SYSDATE, p.StartDate ),
                     minDate( rPackageCharge.sale_exp_date, p.EndDate )
             FROM    Charge c,
                     Package p
             WHERE   p.alt_PackageCode = rPackageCharge.soc
               AND   c.alt_ChargeCode = rPackageCharge.feature_code
               AND   ( p.EndDate IS NULL  OR  p.EndDate > SYSDATE )
               AND   ( c.EndDate IS NULL  OR  c.EndDate > SYSDATE );
        END LOOP;
        COMMIT;

-- ba

        /*  First we terminate any charges which are no longer
         *  available for the given rate plan.
         */
        /*  First we terminate any charges which are no longer
         *  available for the given rate plan.
         */
        UPDATE  PackageCharges pc
        SET     EndDate = SYSDATE
        WHERE   ( EndDate IS NULL  OR  EndDate > SYSDATE )
           AND  NOT EXISTS(
                    SELECT  1
                    FROM    tmp_PackageCharges tmp
                    WHERE   pc.PackageID = tmp.PackageID
                      AND   pc.ChargeID = tmp.ChargeID
                      AND   pc.RelationshipCode = tmp.RelationshipCode
                      );

        COMMIT;

                /*  Now we get all the charges which are new
                 */
        COMMIT;
                INSERT
        INTO    PackageCharges(
                               PackageID,
                               ChargeID,
                               StartDate,
                               EndDate,
                               RelationshipCode,
                               Price
                               )
         SELECT  PackageID,
                 ChargeID,
                 StartDate,
                 EndDate,
                 RelationshipCode,
                 Price
         FROM    tmp_PackageCharges tmp
         WHERE   NOT EXISTS(
                     SELECT  1
                     FROM    PackageCharges pc
                     WHERE   pc.PackageID = tmp.PackageID
                     AND   pc.ChargeID = tmp.ChargeID
                     AND   ( pc.EndDate IS NULL  OR  pc.EndDate > SYSDATE )
                    );

          COMMIT;


        END importPackageCharges;




        /************************************
         *                                  *
         *    importPackageRelationships    *
         *                                  *
         ************************************/
        PROCEDURE
                importPackageRelationships
/*
Assign a relationship code and a default relationship code for each Watson
charge (or Sampson feature) associated with a package.  The basic logic is:
?       Populate the tmp_PackageRelationships table with all packages, related
        packages and charges associated with each  using the package, package
        charges and charge relationships tables.
?       Insert package relationships from the tmp_PackageRelationships
?       Update the relationships that have changed for any existing packages

 Changed by: L.Hardy  Date 12/07/01
         Modified logic to bring in soc to soc relationships from soc_eligibility
         in Samson.  The basic logic now assumes the following:
         -  Relationships created via TM take precedence over all other
            relationships
         -  Relationships created in Samson take second precedence.  These are
            relationships listed in soc_eligibility.
         -  Conflict relationships created in this batch (based on conflicting
            features take the lowest precedence and are really used just to keep
                conflicting features from being selected on the front end.

*/


AS
CURSOR  cRelationships
IS      SELECT  *
        FROM    tmp_PackageRelationships;

--lh 2/26/02
CURSOR  cSocToSoc
IS      SELECT
                PackageID,
                RelatedPackageID,
                alt_PackageCode,
                alt_RelatedPackageCode,
                NVL(decode(RelationshipCode,
                                       'I','N',RelationshipCode),'X') as RelationshipCode
        FROM    tmp_PackageRelationships;

CURSOR ctemp1
IS     SELECT
              p1.PackageID                    as PackageID,
              p2.PackageID                    as RelatedPackageID,
              RTRIM(seE.base_soc)             as alt_PackageCode,
              RTRIM(seE.related_soc)          as alt_RelatedPackageCode,
              NVL( r.RelationshipCode, 'X' )  AS RelationshipCode -- Assume "Excluded" for invalid relationships
       FROM   soc_eligibility@Ensemble seE,
              Relationship r,
              package      p1,
              package      p2
       WHERE  seE.relation_type      = r.alt_RelationshipCode (+)
       AND    RTRIM(seE.base_soc)    = p1.alt_packagecode
       AND    RTRIM(seE.related_soc) = p2.alt_packagecode
       AND    p1.StartDate < sysdate
       AND    (p1.EndDate is null or p1.EndDate > sysdate )
       AND    p2.StartDate < sysdate
       AND    (p2.EndDate is null or p2.EndDate > sysdate )
       AND    seE.effective_date < SYSDATE
       AND    (seE.expiration_date IS NULL  OR  seE.expiration_date > SYSDATE  ) ;

CURSOR ctemp2
IS     SELECT
              p1.PackageID               as PackageID,
              p2.PackageID               as RelatedPackageID,
              p1.alt_PackageCode         as alt_PackageCode,
              p2.alt_PackageCode         as alt_RelatedPackageCode,
              nvl(MAX( cr.RelationshipCode ),'X') as RelationshipCode
       FROM   ChargeRelationships cr,
              PackageCharges      pc1,
              PackageCharges      pc2,
              Package             p1,
              Package             p2
       WHERE   cr.ChargeID        = pc1.ChargeID
         AND   cr.RelatedChargeID = pc2.ChargeID
         AND   pc1.PackageID      = p1.PackageID
         AND   pc2.PackageID      = p2.PackageID
         and   (p1.enddate is null or p1.enddate > sysdate ) -- ba
         and   (p2.enddate is null or p2.enddate > sysdate)  -- ba
         AND   ( pc1.EndDate IS NULL  OR  pc1.EndDate > SYSDATE )
         AND   ( pc2.EndDate IS NULL  OR  pc2.EndDate > SYSDATE )
       GROUP
          BY   p1.PackageID,
               p2.PackageID,
               p1.alt_PackageCode,
               p2.alt_PackageCode;

CURSOR cReducedSoc IS
SELECT
       p2.PackageID                as PackageID,
       p1.PackageID                as RelatedPackageID,
       RTRIM(srE.soc_src)          as alt_PackageCode,
       RTRIM(srE.soc_dest)         as alt_RelatedPackageCode,
           'R'                         as Defaultrelationshipcode,
           'R'                                             as RelationshipCode,
           'WATSON'                                        as LastModBy,
           sysdate                                         as StartDate,
           'N'                                             as Expired
FROM   soc_relation@Ensemble srE,
       package      p1,
       package      p2
WHERE  p1.packageTypeCode= 'RED'
AND    p1.alt_packageCode = RTRIM(srE.soc_src)
and        p2.alt_packageCode = RTRIM(srE.soc_dest)
and    srE.relation_type = 'F'
AND    p1.StartDate < sysdate
AND    (p1.EndDate is null or p1.EndDate > sysdate )
AND    p2.StartDate < sysdate
AND    (p2.EndDate is null or p2.EndDate > sysdate )
AND    (srE.expiration_date IS NULL  OR  srE.expiration_date > SYSDATE  )
AND    (srE.src_effective_date < sysdate and srE.dest_effective_date < sysdate ) ;

CURSOR cBogoSoc IS
SELECT
       p1.PackageID                as PackageID,
       p2.PackageID                as RelatedPackageID,
       RTRIM(srE.soc_src)          as alt_PackageCode,
       RTRIM(srE.soc_dest)         as alt_RelatedPackageCode,
           'B'                         as Defaultrelationshipcode,
           'B'                                             as RelationshipCode,
           'WATSON'                                        as LastModBy,
           sysdate                                         as StartDate,
           'N'                                             as Expired
FROM   soc_relation@Ensemble srE,
       package      p1,
       package      p2
WHERE  p2.packageTypeCode= 'PRO'
AND    p1.alt_packageCode = RTRIM(srE.soc_src)
and        p2.alt_packageCode = RTRIM(srE.soc_dest)
and    srE.relation_type = 'A'
AND    p1.StartDate < sysdate
AND    (p1.EndDate is null or p1.EndDate > sysdate )
AND    p2.StartDate < sysdate
AND    (p2.EndDate is null or p2.EndDate > sysdate )
AND    (srE.expiration_date IS NULL  OR  srE.expiration_date > SYSDATE  )
AND    (srE.src_effective_date < sysdate and srE.dest_effective_date < sysdate ) ;


vOldRelationshipCode    Relationship.RelationshipCode%TYPE;
vIsNewRelationship      boolean;
vDummy                  INTEGER;
rMatchPR                PackageRelationships%ROWTYPE;

BEGIN

/* Clean up the tmp_PackageRelationship table and the PackageRelationships
   table.  Any data in PackageRelationshis with a non null default value
   is populated via the batch and should be deleted before the start.
   After the cleanup is completed populate the temp table with data from
   Samson.
*/
    DELETE
    FROM    tmp_PackageRelationships;
    COMMIT;

       FOR rtemp1 in ctemp1 LOOP
          BEGIN
              INSERT
              INTO tmp_PackageRelationships(
                                            PackageID,
                                            RelatedPackageID,
                                            alt_PackageCode,
                                            alt_RelatedPackageCode,
                                            RelationshipCode
                                            )
               VALUES  (
                                             rtemp1.PackageID,
                                             rtemp1.RelatedPackageID,
                                             rtemp1.alt_PackageCode,
                                             rtemp1.alt_RelatedPackageCode,
                                             rtemp1.RelationshipCode
                         );

           EXCEPTION
               WHEN  DUP_VAL_ON_INDEX  THEN
                     vDummy := 1 ;
           END ;

      END LOOP;
      COMMIT;

--        DBMS_OUTPUT.PUT_LINE( 'start of job') ;

    FOR rtemp2 in ctemp2 LOOP
        BEGIN
            INSERT
            INTO tmp_PackageRelationships(
                                        PackageID,
                                        RelatedPackageID,
                                        alt_PackageCode,
                                        alt_RelatedPackageCode,
                                        RelationshipCode
                                                )
            VALUES  (
                                         rtemp2.PackageID,
                                         rtemp2.RelatedPackageID,
                                         rtemp2.alt_PackageCode,
                                         rtemp2.alt_RelatedPackageCode,
                                         rtemp2.RelationshipCode
                        );

            EXCEPTION
                 WHEN  DUP_VAL_ON_INDEX  THEN
                       vDummy := 1 ;


            END ;
    END LOOP;
    COMMIT;
  FOR rReducedSoc in cReducedSoc LOOP
          BEGIN
              INSERT
              INTO tmp_PackageRelationships(
                                            PackageID,
                                            RelatedPackageID,
                                            alt_PackageCode,
                                            alt_RelatedPackageCode,
                                            RelationshipCode
                                            )
               VALUES  (
                                             rReducedSoc.PackageID,
                                             rReducedSoc.RelatedPackageID,
                                             rReducedSoc.alt_PackageCode,
                                             rReducedSoc.alt_RelatedPackageCode,
                                             rReducedSoc.RelationshipCode
                         );

           EXCEPTION
               WHEN  DUP_VAL_ON_INDEX  THEN
                     vDummy := 1 ;
           END ;

      END LOOP;
      COMMIT;
       FOR rBOGOsoc in cBOGOsoc LOOP
          BEGIN
              INSERT
              INTO tmp_PackageRelationships(
                                            PackageID,
                                            RelatedPackageID,
                                            alt_PackageCode,
                                            alt_RelatedPackageCode,
                                            RelationshipCode
                                            )
               VALUES  (
                                             rBOGOsoc.PackageID,
                                             rBOGOsoc.RelatedPackageID,
                                             rBOGOsoc.alt_PackageCode,
                                             rBOGOsoc.alt_RelatedPackageCode,
                                             rBOGOsoc.RelationshipCode
                         );

           EXCEPTION
               WHEN  DUP_VAL_ON_INDEX  THEN
                     vDummy := 1 ;
           END ;

      END LOOP;
      COMMIT;

-- new query ba

-- expire any samson relationships (non tm ones) that no longer
-- exist in samson

    UPDATE  PackageRelationships pr
    set     enddate = sysdate,
                        expired = 'Y',
            lastmodby = 'WATSON'
    WHERE   DefaultRelationshipCode IS NOT NULL
      and   DefaultRelationshipCode <> RelationshipCode
          AND   NOT EXISTS(
                SELECT  1
                FROM    tmp_PackageRelationships Tpr
                WHERE   pr.PackageID = Tpr.PackageID
                  AND   pr.RelatedPackageID = Tpr.RelatedPackageID
             );

     COMMIT;



/*
   Insert the Samson data.  If it exists it is because there is a user
   override and we don't want to insert or update this data, just bypass
   the row.
*/
  FOR rSocToSoc in cSocToSoc LOOP
        BEGIN
           INSERT
           INTO PackageRelationships(
                                     PackageID,
                                     RelatedPackageID,
                                     alt_PackageCode,
                                     alt_RelatedPackageCode,
                                     DefaultRelationshipCode,
                                     RelationshipCode,
                                     LastModBy,
                                     LastModDate,
                                     StartDate,
                                                                         Expired
                                                                         )
           VALUES  (
                                      rSocToSoc.PackageID,
                                      rSocToSoc.RelatedPackageID,
                                      rSocToSoc.alt_PackageCode,
                                      rSocToSoc.alt_RelatedPackageCode,
                                      rSocToSoc.RelationshipCode,
                                      rSocToSoc.RelationshipCode,
                                      'WATSON',
                                       SYSDATE,
                                       SysDate,
                                                                           'N'
                     );

        EXCEPTION
                  WHEN  DUP_VAL_ON_INDEX  THEN
                        vDummy := 1 ;
        END ;
    END LOOP;
    COMMIT;

/*
   Once the Samson soc data is input create the default relationships for
   conflicts.  Again, if this data exists in Watson it is because the row
   was put there by a user or Samson.  In either case we do not want to
   override this so we'll just bypass the insert error and continue.
*/

                /*  Insert any default conflicting relationships.

                 */
    FOR  rRelationship  IN  cRelationships  LOOP
         BEGIN

            SELECT
                   *
            INTO   rMatchPR
            FROM   PackageRelationships pr
            WHERE  packageid = rRelationship.Packageid
            AND    RelatedPackageID = rRelationship.RelatedPackageID
            AND    (pr.enddate is null) ;

            IF    NVL(rMatchPR.alt_PackageCode,'z')        <>
                  NVL(rRelationship.alt_PackageCode,'z')
               or NVL(rMatchPR.alt_RelatedPackageCode,'z') <>
                  NVL(rRelationship.alt_RelatedPackageCode,'z')
               or NVL(rMatchPR.RelationshipCode,'z')       <>
                  NVL(rRelationship.RelationshipCode,'z')
            THEN

--  expire the relationship
                  UPDATE  PackageRelationships pr
                  set     enddate   = sysdate,
                                                  EXPIRED = 'Y',
                          lastmodby = 'WATSON'
                   WHERE  pr.PackageID        = rRelationship.PackageID
                   AND    pr.RelatedPackageID = rRelationship.RelatedPackageID
                   AND    enddate is null;

-- add the new relationship
                   INSERT
                   INTO    PackageRelationships(
                                    PackageID,
                                    RelatedPackageID,
                                    alt_PackageCode,
                                    alt_RelatedPackageCode,
                                    DefaultRelationshipcode, --ba
                                    RelationshipCode,
                                    LastModBy,
                                    LastModDate,
                                    StartDate,  -- ba
                                    EndDate,     --ba
                                                                        expired
                                     )
                   VALUES  (
                                    rRelationship.PackageID,
                                    rRelationship.RelatedPackageID,
                                    rRelationship.alt_PackageCode,
                                    rRelationship.alt_RelatedPackageCode,
                                    rRelationship.RelationshipCode, --ba
                                    rRelationship.RelationshipCode,
                                    'WATSON',
                                    SYSDATE,
                                    sysdate,  --ba
                                    null,      --ba
                                                                        'N'
                              );
            END IF;
         EXCEPTION
                WHEN  NO_DATA_FOUND  THEN
                      INSERT
                      INTO    PackageRelationships(
                                        PackageID,
                                        RelatedPackageID,
                                        alt_PackageCode,
                                        alt_RelatedPackageCode,
                                        DefaultRelationshipCode, --ba
                                        RelationshipCode,
                                        LastModBy,
                                        LastModDate,
                                        StartDate,  -- ba
                                        EndDate,     --ba
                                                                                EXPIRED
                                        )
                      VALUES  (
                                        rRelationship.PackageID,
                                        rRelationship.RelatedPackageID,
                                        rRelationship.alt_PackageCode,
                                        rRelationship.alt_RelatedPackageCode,
                                        rRelationship.RelationshipCode, --ba
                                        rRelationship.RelationshipCode,
                                        'WATSON',
                                        SYSDATE,
                                        sysdate,  --ba
                                        null,      --ba
                                                                                'N'
                                   );
         END;
         COMMIT;
         END LOOP;

        END importPackageRelationships;




        /************************************
         *                                  *
         *     createChargeRelationship     *
         *                                  *
         ************************************/
        PROCEDURE
                createChargeRelationship(
                        pChargeID               Charge.ChargeID%TYPE,
                        pRelatedChargeID        Charge.ChargeID%TYPE,
                        pAlt_ChargeCode         Charge.alt_ChargeCode%TYPE,
                        pAlt_RelatedChargeCode  Charge.alt_ChargeCode%TYPE,
                        pAlt_ConflictCode       Charge.alt_ConflictCode%TYPE,
                        pRelationshipCode       Relationship.RelationshipCode%TYPE
                )

/*
A charge relationship exists when any 2 features belong to the same
feature grouping or mps_feature_code.  Examples of this are feature_codes
DATA, GDATAM, and DATAF which both have the mps_feature_code of DATA and
therefore cannot all  be assigned together.  This is a conflict and is
represented with a relationship code of  "?".  Other examples of conflicts are:
?       FAXand GFAX which both have mps_feature_codes of FAX
?       INTLRM(international roaming), NATLRM(national roaming) and
        NOROAM(no roaming) which have mps_feature_codes of ROAM.  In this
        obvious example you cannot assign national or international roaming
        and no roaming....

Any feature that is not explicitly in conflict with another is assigned a
default relationship of 'X' or excluded.

*/

IS
vDummy  integer;

        BEGIN

                BEGIN

                        INSERT
                        INTO    ChargeRelationships(
                                                ChargeID,
                                                RelatedChargeID,
                                                alt_ChargeCode,
                                                alt_RelatedChargeCode,
                                                alt_ConflictCode,
                                                DefaultRelationshipCode,
                                                RelationshipCode,
                                                LastModBy,
                                                LastModDate,
                                                startdate,
                                                enddate
                                        )
                        VALUES  (
                                                pChargeID,
                                                pRelatedChargeID,
                                                pAlt_ChargeCode,
                                                pAlt_RelatedChargeCode,
                                                pAlt_ConflictCode,
                                                pRelationshipCode,
                                                pRelationshipCode,
                                                'WATSON',
                                                SYSDATE,
                                                sysdate,
                                                null
                                        );

                EXCEPTION
                        WHEN  DUP_VAL_ON_INDEX  THEN
                                vDummy := vDummy + 1;
                END;


        END createChargeRelationship;

        /************************************
         *                                  *
         *        importMobileNetworkCodes  *
         *                                  *
         ************************************/
        PROCEDURE
                importMobileNetworkCodes
/*
  Added on 7/18/2002
  Laura Hardy

  This logic is included for the rare occasion when Samson adds a
  Mobile network code.  This is about once a year.....

*/

AS
BEGIN

     BEGIN
          INSERT INTO MobileNetwork
                         (MNC,
                                  Name,
                                  Description )
          SELECT  gen_code,
                  gen_desc,
                  gen_desc
          FROM    generic_codes@Ensemble
          WHERE   gen_type = 'SIM MNC'
            AND   gen_code NOT IN
                 (SELECT MNC FROM MobileNetwork) ;


         EXCEPTION
               WHEN OTHERS THEN
                    DBMS_OUTPUT.PUT_LINE( 'Error on insert of MNC');
                    ROLLBACK;
         END;

    COMMIT;

    END importMobileNetworkCodes;

        /************************************
         *                                  *
         *        importPhoneNumbers        *
         *                                  *
         ************************************/
        PROCEDURE
                importPhoneNumbers
/*
Import Phone Numbers populates PhoneNumberLocations and NPANXX.
The Sampson number_group table is used to extract all the phone number
prefixes for each Watson market.  The Sampson npa_nxx_lr and ctn_inv
tables along with the Watson Phone Number Location table is queried
calculate a count of available phone numbers within a phone number
location (area code and prefix).  This count is used by the application
to determine the largest pool of number to select from when selecting a
MSISDN (Mobile Station ISDN) .
*/

AS
CURSOR  cNPANXXs
IS      SELECT  tbl.npa,
                tbl.nxx,
                tbl.npa || tbl.nxx              AS      npanxx,
                pnl.MarketID,
                pnl.Description,
                pnl.PhoneNumberLocationCode,
                tbl.NumAvailable
        FROM    (
                 SELECT  SUBSTR( ctnE.ctn, 1, 3 )     AS npa,
                         SUBSTR( ctnE.ctn, 4, 3 )     AS nxx,
                         MIN( ngp )                   AS ngp,
                         COUNT(
                         DECODE( ctn_status,
                                 'AA', 1, NULL ) )    AS NumAvailable
                 FROM ctn_inv@Numbers ctnE
             --    WHERE  RTRIM(NL) in ('GNRL','PREAA') --lh 08/14/02
                 WHERE  RTRIM(NL) = 'GNRL'--lh 03/21/03 to fix bulk problem
                 GROUP
                   BY   SUBSTR( ctnE.ctn, 1, 3 ),
                        SUBSTR( ctnE.ctn, 4, 3 )
                 ) tbl,
                 PhoneNumberLocation pnl
        WHERE   tbl.ngp = pnl.alt_PhoneNumberLocationCode;

-- added on 7/17/03 for pag problem
CURSOR  cPAGNPANXXs
IS      SELECT  tbl.npa,
                tbl.nxx,
                tbl.npa || tbl.nxx              AS      npanxx,
                pnl.MarketID,
                pnl.Description,
                pnl.PhoneNumberLocationCode,
                tbl.NumAvailable
        FROM    (
                 SELECT  SUBSTR( ctnE.ctn, 1, 3 )     AS npa,
                         SUBSTR( ctnE.ctn, 4, 3 )     AS nxx,
                         MIN( ngp )                   AS ngp,
                         COUNT(
                         DECODE( ctn_status,
                                 'AA', 1, NULL ) )    AS NumAvailable
                 FROM  ctn_inv@Numbers ctnE
                 GROUP
                   BY   SUBSTR( ctnE.ctn, 1, 3 ),
                        SUBSTR( ctnE.ctn, 4, 3 )
                 ) tbl,
                 PhoneNumberLocation pnl
        WHERE   tbl.ngp = pnl.alt_PhoneNumberLocationCode;
-- end of block

vNumAvailable   number;
vStartDate      date;
vEndDate        date;

    BEGIN
         BEGIN
               DELETE
               FROM    PhoneNumberLocation;

               INSERT
               INTO    PhoneNumberLocation(
                                      PhoneNumberLocationCode,
                                      alt_PhoneNumberLocationCode,
                                      MarketID,
                                      Description
                                        )
                        SELECT  ngE.ngp_id,
                                ngE.ngp_id,
                                m.MarketID,
                                SUBSTR( ngE.ngp_dsc, 5, 200 )
                        FROM    number_group@Ensemble ngE, 
                                Market m
                        WHERE   SUBSTR( ngE.ngp_dsc, 1, 3 ) = m.Alt_MarketCode (+);

         EXCEPTION
               WHEN OTHERS THEN
                    DBMS_OUTPUT.PUT_LINE( 'ERROR:  Phone Number Locations *not* imported.');
                    ROLLBACK;
         END;
         COMMIT;


         BEGIN
               UPDATE  NPANXX
               SET     LastModBy = DECODE( NumAvailable, -1, LastModBy, 'WATSON' ),
                       LastModDate = DECODE( NumAvailable, -1, LastModDate, SYSDATE ),
                       NumAvailable = -1;

               FOR rNPANXX IN cNPANXXs LOOP

                   BEGIN
 /* Modified by: lhardy., Uncommented out expiration_date logic in the select */
                        SELECT  MIN( effective_date ),
                                MAX( expiration_date )
                        INTO    vStartDate,
                                vEndDate
                        FROM    npa_nxx_lr@Ensemble 
                        WHERE   npa = rNPANXX.npa
                          AND   nxx = rNPANXX.nxx
                          AND   (  expiration_date IS NULL  OR  expiration_date > SYSDATE  );

                        IF  vStartDate IS NULL  THEN
                            vStartDate := SYSDATE;
                            vEndDate := SYSDATE;
                            DBMS_OUTPUT.PUT_LINE( 'WARNING:  NPA/NXX ' || rNPANXX.NPA || '/' || rNPANXX.NXX || ' not found in npa_nxx_lr table.');
                        END IF;

                        UPDATE  NPANXX
                        SET     MarketID                = rNPANXX.MarketID,
                                Description             = rNPANXX.Description,
                                PhoneNumberLocationCode = rNPANXX.PhoneNumberLocationCode,
                                NumAvailable            = rNPANXX.NumAvailable,
                                StartDate               = vStartDate,
                                EndDate                 = vEndDate,
                                LastModBy               = 'WATSON',
                                LastModDate             = SYSDATE
                        WHERE   NPANXX = rNPANXX.npanxx;

                        IF  SQL%ROWCOUNT = 0  THEN
                            INSERT
                            INTO    NPANXX(
                                           NPANXX,
                                           MarketID,
                                           Description,
                                           PhoneNumberLocationCode,
                                           NumAvailable,
                                           StartDate,
                                           EndDate,
                                           LastModBy,
                                           LastModDate
                                          )
                             VALUES  (
                                            rNPANXX.npanxx,
                                            rNPANXX.MarketID,
                                            rNPANXX.Description,
                                            rNPANXX.PhoneNumberLocationCode,
                                            rNPANXX.NumAvailable,
                                            vStartDate,
                                            vEndDate,
                                            'WATSON',
                                            SYSDATE
                                      );
                        END IF;

                   EXCEPTION
                        WHEN  NO_DATA_FOUND  THEN
                              DBMS_OUTPUT.PUT_LINE( 'ERROR:  NPA/NXX ' || rNPANXX.NPA || '/' || rNPANXX.NXX || ' has no valid line ranges defined.');
                        WHEN  OTHERS  THEN
                              DBMS_OUTPUT.PUT_LINE( 'ERROR:  processing NPA/NXX ' || rNPANXX.NPA || '/' || rNPANXX.NXX || '.');

                   END;

               END LOOP;
               COMMIT;
			   
			   
-- block added on 07/17/03 for pag problem
        BEGIN
               UPDATE  pagNPANXX
               SET     LastModBy = DECODE( NumAvailable, -1, LastModBy, 'WATSON' ),
                       LastModDate = DECODE( NumAvailable, -1, LastModDate, SYSDATE ),
                       NumAvailable = -1;

               FOR rpagNPANXX IN cpagNPANXXs LOOP

                   BEGIN
 /* Modified by: lhardy., Uncommented out expiration_date logic in the select */
                        SELECT  MIN( effective_date ),
                                MAX( expiration_date )
                        INTO    vStartDate,
                                vEndDate
                        FROM    npa_nxx_lr@Ensemble  
                        WHERE   npa = rpagNPANXX.npa
                          AND   nxx = rpagNPANXX.nxx
                          AND   (  expiration_date IS NULL  OR  expiration_date > SYSDATE  );

                        IF  vStartDate IS NULL  THEN
                            vStartDate := SYSDATE;
                            vEndDate := SYSDATE;
                            DBMS_OUTPUT.PUT_LINE( 'WARNING:  pagNPA/NXX ' || rpagNPANXX.NPA || '/' || rpagNPANXX.NXX || ' not found in npa_nxx_lr table.');
                        END IF;

                        UPDATE  pagNPANXX
                        SET     MarketID                = rpagNPANXX.MarketID,
                                Description             = rpagNPANXX.Description,
                                PhoneNumberLocationCode = rpagNPANXX.PhoneNumberLocationCode,
                                NumAvailable            = rpagNPANXX.NumAvailable,
                                StartDate               = vStartDate,
                                EndDate                 = vEndDate,
                                LastModBy               = 'WATSON',
                                LastModDate             = SYSDATE
                        WHERE   NPANXX = rpagNPANXX.npanxx;

                        IF  SQL%ROWCOUNT = 0  THEN
                            INSERT
                            INTO    pagNPANXX(
                                           NPANXX,
                                           MarketID,
                                           Description,
                                           PhoneNumberLocationCode,
                                           NumAvailable,
                                           StartDate,
                                           EndDate,
                                           LastModBy,
                                           LastModDate
                                          )
                             VALUES  (
                                            rpagNPANXX.npanxx,
                                            rpagNPANXX.MarketID,
                                            rpagNPANXX.Description,
                                            rpagNPANXX.PhoneNumberLocationCode,
                                            rpagNPANXX.NumAvailable,
                                            vStartDate,
                                            vEndDate,
                                            'WATSON',
                                            SYSDATE
                                      );
                        END IF;

                   EXCEPTION
                        WHEN  NO_DATA_FOUND  THEN
                              DBMS_OUTPUT.PUT_LINE( 'ERROR:  pagNPA/NXX ' || rpagNPANXX.NPA || '/' || rpagNPANXX.NXX || ' has no valid line ranges defined.');
                        WHEN  OTHERS  THEN
                              DBMS_OUTPUT.PUT_LINE( 'ERROR:  processing pagNPA/NXX ' || rpagNPANXX.NPA || '/' || rpagNPANXX.NXX || '.');

                   END;

               END LOOP;
               COMMIT;
-- end of added block
   	   	  		end ;
			   
         END;

    END importPhoneNumbers;


        /************************************
         *                                  *
         *  import Location_Compid_Link     *
         *                                  *
         ************************************/
        PROCEDURE
                importLocation_Compid_Link
/*
The import location comp id link section of code was added to eliminate
the need to dynamically link to Sampson for dealer location validation.
This section of code imports a portion of  Sampson location_compid_link
table into a mirror table in Watson.  the location id and comp id (dealer
compensation id) are retrieved.  The Validate Commission stored procedure
uses this table to ensure that a dealer comp id is valid for this location.
*/

        AS



                BEGIN
                        DELETE
                        FROM    Location_Compid_Link;


                        INSERT
                        INTO    Location_Compid_Link(
                                Location_id,
                                Comp_id
                                        )
                        SELECT  lcE.location_id,
                                lcE.comp_id
                        FROM    Location_Compid_Link@Ensemble lcE;

                EXCEPTION
                        WHEN OTHERS THEN
                                DBMS_OUTPUT.PUT_LINE( 'ERROR:  In ImportLocation_Compid_Link');
                                ROLLBACK;


        END importLocation_Compid_Link;


        /************************************
         *                                  *
         *   purge ProcessLog               *
         *                                  *
         ************************************/
        PROCEDURE
                purgeProcessLog
/*
The process log table contains Java page performance information.
Start and end times from random Java pages are written to the process
log table daily and this can be used to determine if performance of the
Watson application has changed.  This information is retained for 14 days
and is purged daily through the nightly batch in this section of code.  The
purge criteria is to delete all rows with a Server Start Date older than 14
days.
*/

        AS


                BEGIN
                        DELETE
                        FROM    ProcessLog
                        WHERE   trunc(ServerStartDate) < trunc(SYSDATE) - 14 ;


                EXCEPTION
                        WHEN OTHERS THEN
                                DBMS_OUTPUT.PUT_LINE( 'ERROR:  In purgeProcessLog');


        END purgeProcessLog;



        /************************************
         *                                  *
         *        importUserAttribute       *
         *                                  *
         ************************************/

/* Changed by L.Hardy  Date 10/23/01   for CR170

   This section will retrieve the deposit allow ind for all Watson
   dealers that are in Sampson.  Watson users with a system id of
   400 are Sampson recognized dealers and the only users that would
   show up in Sampson.

   The Watson field Identifier is the equivalent of the Sampson
   operator id.  This field is a character in Watson, and a number in
   sampson, therefore it is converted in the query.  If there is no
   deposit allow ind in watson the default is that the dealer can accept
   deposits.

   Changed By:          Laura Hardy
   Date:                        01/18/02
   Description
   Modified ImportUserAttribute to perform a lookup for the archive
   indicator.  ONYX and Dealer codes that are archived are not allowed
   to perform activations.  If a code is not found it is assumed that
   (and defaulted to) this dealer is not archived.

*/

PROCEDURE
        importUserAttribute

AS

CURSOR cValidateUser
IS
select
        uas.username                    as UserName,
        uas.fieldname                   as FieldName,
        uas.identifier                  as Identifier,
        decode(max(dte.deposit_allow_ind),
                   'Y','Y',
                   'N','N',
                   null,'Y',
                   'N')                 as deposit_allow_ind
from
        UserAlternateSystems  uas,
        dealer_type@Ensemble  dte,
        dealer_profile@Ensemble dpe
where
      dpe.dealer = uas.identifier
  and dpe.sales_channel = dte.dlr_tp_cd
  and uas.SystemID = 400
group
   by   username,
        fieldname,
        identifier ;

-- lh 01/18/02
CURSOR cArchiveIndicator
IS
select
        uas.username                    as UserName,
        uas.fieldname                   as FieldName,
        uas.identifier                  as Identifier,
        decode(dpe.archive_ind,
                   'Y','Y',
                   'N','N',
                   null,'N',
                   'N')                 as archive_ind
from
        UserAlternateSystems  uas,
        dealer_profile@Ensemble dpe
where
          dpe.dealer = uas.identifier ;


BEGIN

         BEGIN

                FOR rValidateUser IN cValidateUser LOOP

                BEGIN

                         UPDATE UserAlternateSystems
                         SET
                                 DepositAllowInd = rValidateUser.Deposit_allow_ind
                         WHERE
                               SystemID          = 400
                         and   UserName          = rValidateUser.UserName
                         and   FieldName         = rValidateUser.fieldName
                         and   Identifier        = rValidateUser.identifier ;

            END ;

            END LOOP;

-- lh 01/18/02
         BEGIN

                FOR rArchiveIndicator IN cArchiveIndicator LOOP

                BEGIN

                         UPDATE UserAlternateSystems
                         SET    ArchiveInd = rArchiveIndicator.archive_ind
                         WHERE
                               UserName          = rArchiveIndicator.UserName
                         and   FieldName         = rArchiveIndicator.fieldName
                         and   Identifier        = rArchiveIndicator.identifier ;

            END ;

            END LOOP;

                END ;
                COMMIT;

END;

END importUserAttribute;

        /************************************
         *                                  *
         *               MAIN               *
         *                                  *
         ************************************/

                 /* Changed By Tanuja  Date 12/08/02
            Added changes for Enabler - CR0659
         */

        PROCEDURE
                Main
        AS
                vBeginTime      DATE := SYSDATE;
                vStartTime      DATE;


        BEGIN

                DBMS_OUTPUT.PUT_LINE( 'STARTED PROCESS (' || TO_CHAR(SYSDATE,'mm/dd/yyyy hh24:mi') || ')' );
                DBMS_OUTPUT.PUT_LINE(' ');

                vStartTime := SYSDATE;
                importRatePlans;
                ShowElapsedTime( vStartTime, 'importRatePlans' );

                vStartTime := SYSDATE;
                importPackages;
                ShowElapsedTime( vStartTime, 'importPackages' );

                vStartTime := SYSDATE;
                importCharges;
                ShowElapsedTime( vStartTime, 'importCharges' );

                                vStartTime := SYSDATE;
                importOfferParams;
                ShowElapsedTime( vStartTime, 'importOfferParams' );

                vStartTime := SYSDATE;
                importRatePlanPackages;
                ShowElapsedTime( vStartTime, 'importRatePlanPackages' );

                vStartTime := SYSDATE;
                importRatePlanPrice;
                ShowElapsedTime( vStartTime, 'importRatePlanPrice' );

                vStartTime := SYSDATE;
                importRatePlanCharges;
                ShowElapsedTime( vStartTime, 'importRatePlanCharges' );

                vStartTime := SYSDATE;
                importPackageCharges;
                ShowElapsedTime( vStartTime, 'importPackageCharges' );

                vStartTime := SYSDATE;
                importPackageRelationships;
                ShowElapsedTime( vStartTime, 'importPackageRelationships' );

                vStartTime := SYSDATE;
                importMobileNetworkCodes;
                ShowElapsedTime( vStartTime, 'importMobileNetworkCodes' );

                vStartTime := SYSDATE;
                importPhoneNumbers;
                ShowElapsedTime( vStartTime, 'importPhoneNumbers' );

                        vStartTime := SYSDATE;
                importLocation_Compid_Link;
                        ShowElapsedTime( vStartTime, 'importLocation_Compid_Link' );

                        vStartTime := SYSDATE;
                purgeProcessLog;
                        ShowElapsedTime( vStartTime, 'purgeProcessLog' );

                        vStartTime := SYSDATE;
                importUserAttribute;
                        ShowElapsedTime( vStartTime, 'importUserAttribute' );

                DBMS_OUTPUT.PUT_LINE(' ');
                ShowElapsedTime( vBeginTime, 'PROCESS (' || TO_CHAR(SYSDATE,'mm/dd/yyyy hh24:mi') || ')' );

        END Main;

END Ensemble_NightlyBatch;
/
