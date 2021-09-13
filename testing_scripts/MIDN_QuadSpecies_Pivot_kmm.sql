SELECT 
Network, ParkUnit, ParkSubUnit, PlotTypeCode, PlotCode, IsAbandoned, PanelCode, StartYear, IsQAQC, PIVSQ.EventID, PlotID, SQQuadSum, 
A2_SQ, A5_SQ, A8_SQ, AA_SQ, B2_SQ, B5_SQ, B8_SQ, BB_SQ, C2_SQ, C5_SQ, C8_SQ, CC_SQ, TSN, 

IIF(SQQuadSum = 12 AND ScientificName_tmp IS NULL, 'None present', ScientificName_tmp) AS ScientificName,

IIF(A2_cd IS NULL AND A2_SQ = 'NP', 0, A2_cd) AS A2, 
IIF(A5_cd IS NULL AND A5_SQ = 'NP', 0, A5_cd) AS A5, 
IIF(A8_cd IS NULL AND A8_SQ = 'NP', 0, A8_cd) AS A8, 
IIF(AA_cd IS NULL AND AA_SQ = 'NP', 0, AA_cd) AS AA,
IIF(B2_cd IS NULL AND B2_SQ = 'NP', 0, B2_cd) AS B2, 
IIF(B5_cd IS NULL AND B5_SQ = 'NP', 0, B5_cd) AS B5, 
IIF(B8_cd IS NULL AND B8_SQ = 'NP', 0, B8_cd) AS B8,
IIF(BB_cd IS NULL AND BB_SQ = 'NP', 0, BB_cd) AS BB, 
IIF(C2_cd IS NULL AND C2_SQ = 'NP', 0, C2_cd) AS C2, 
IIF(C5_cd IS NULL AND C5_SQ = 'NP', 0, C5_cd) AS C5, 
IIF(C8_cd IS NULL AND C8_SQ = 'NP', 0, C8_cd) AS C8,
IIF(CC_cd IS NULL AND CC_SQ = 'NP', 0, CC_cd) AS CC,

IIF(A2_txt IS NULL AND A2_SQ = 'NP', '0%', A2_txt) AS A2_txt, 
IIF(A5_txt IS NULL AND A5_SQ = 'NP', '0%', A5_txt) AS A5_txt, 
IIF(A8_txt IS NULL AND A8_SQ = 'NP', '0%', A8_txt) AS A8_txt, 
IIF(AA_txt IS NULL AND AA_SQ = 'NP', '0%', AA_txt) AS AA_txt,
IIF(B2_txt IS NULL AND B2_SQ = 'NP', '0%', B2_txt) AS B2_txt, 
IIF(B5_txt IS NULL AND B5_SQ = 'NP', '0%', B5_txt) AS B5_txt, 
IIF(B8_txt IS NULL AND B8_SQ = 'NP', '0%', B8_txt) AS B8_txt,
IIF(BB_txt IS NULL AND BB_SQ = 'NP', '0%', BB_txt) AS BB_txt, 
IIF(C2_txt IS NULL AND C2_SQ = 'NP', '0%', C2_txt) AS C2_txt, 
IIF(C5_txt IS NULL AND C5_SQ = 'NP', '0%', C5_txt) AS C5_txt, 
IIF(C8_txt IS NULL AND C8_SQ = 'NP', '0%', C8_txt) AS C8_txt,
IIF(CC_txt IS NULL AND CC_SQ = 'NP', '0%', CC_txt) AS CC_txt, 

ConfidenceClassCode, IsCollected, 
QuadSppNote, TRACode, ProtectedStatusCode, DPLCode
/* Remove SQQuadSum for final version. It's a temporary column for the WHERE statement that drops null TSN/ScientificNames when
>1 but <12 (ie all) quadrats are NS. 
*/
FROM
-- Query 1: Compile SQs for each quadrat and join with temp table that sums num of quads sampled
(SELECT Network, ParkUnit, ParkSubUnit, PlotTypeCode, PlotCode, IsAbandoned, PanelCode, 
       StartYear, IsQAQC, QuadData, SQ.EventID, PlotID, DataType, SQQuadSum, ExportDate
FROM
-- Query 1A: Compile SQs
(SELECT
  tluCOMN.Park.Network AS 'Network',
	tluCOMN.Park.Unit AS 'ParkUnit',
	tluCOMN.Park.SubUnit AS 'ParkSubUnit',
	tluCOMN.PlotType.Code AS 'PlotTypeCode',
	tblCOMN.Plot.Code AS 'PlotCode',
	tblCOMN.Plot.IsAbandoned AS 'IsAbandoned',
	tluCOMN.Panel.Code AS 'PanelCode',
	tblCOMN.[Event].StartDate AS 'StartDate',
	tblCOMN.[Event].IsQAQC AS 'IsQAQC',
	tblCOMN.[Event].StartYear AS 'StartYear',
	tluCOMN.SpeciesSampleQualifier.Code AS 'QuadData',
	tblCOMN.[Event].ID AS 'EventID',
    tblCOMN.Plot.ID AS 'PlotID',
	tluCOMN.Quadrat.Code+'_'+'SQ' AS DataType,
	GETDATE() AS 'ExportDate'

FROM tluCOMN.Park
    INNER JOIN tblCOMN.Plot ON tluCOMN.Park.ID = tblCOMN.Plot.ParkID
	INNER JOIN tluCOMN.Panel ON tblCOMN.Plot.PanelID = tluCOMN.Panel.ID
	INNER JOIN tluCOMN.PlotType ON tblCOMN.Plot.PlotTypeID = tluCOMN.PlotType.ID
    INNER JOIN tblCOMN.[Event] ON tblCOMN.[Event].PlotID = tblCOMN.Plot.ID
    LEFT JOIN tblCOMN.QuadratEvent ON tblCOMN.[Event].ID = tblCOMN.QuadratEvent.EventID
	LEFT JOIN tluCOMN.Quadrat ON tblCOMN.QuadratEvent.QuadratID = tluCOMN.Quadrat.ID
	LEFT JOIN tluCOMN.SpeciesSampleQualifier ON tblCOMN.QuadratEvent.SpeciesSampleQualifierID = tluCOMN.SpeciesSampleQualifier.ID) AS SQ

LEFT JOIN
-- Query 1B: Calculate number of quads sampled and join with Q1A.
(SELECT DISTINCT
	tblCOMN.[Event].ID AS 'EventID',
	SUM(CASE 
	  WHEN tluCOMN.SpeciesSampleQualifier.Code IN ('NS', 'PM') THEN 0
	  WHEN tluCOMN.SpeciesSampleQualifier.Code IN ('SS', 'NP') THEN 1
	END) AS SQQuadSum
	
FROM tblCOMN.[Event]
    LEFT JOIN tblCOMN.QuadratEvent ON tblCOMN.[Event].ID = tblCOMN.QuadratEvent.EventID
	LEFT JOIN tluCOMN.SpeciesSampleQualifier ON tblCOMN.QuadratEvent.SpeciesSampleQualifierID = tluCOMN.SpeciesSampleQualifier.ID 

GROUP BY tblCOMN.Event.ID) AS SQquadsum

ON SQ.EventID = SQquadsum.EventID) AS SQ 

-- Pivot Query 1 wide on SQ
PIVOT (MAX(SQ.QuadData) FOR DataType IN 
(A2_SQ, A5_SQ, A8_SQ, AA_SQ, B2_SQ, B5_SQ, B8_SQ, BB_SQ, C2_SQ, C5_SQ, C8_SQ, CC_SQ)) AS PIVSQ

-- Join wide SQ table with wide quadrat species cover data
LEFT JOIN
(SELECT * FROM
-- Query 2: Compile QuadSpp by cover class code and label
-- Query 2A: Cover Class Code
(SELECT
	tblCOMN.[Event].ID AS 'EventID',
	tluCOMN.Taxon.TSN AS 'TSN',
	tluCOMN.Taxon.ScientificName AS 'ScientificName_tmp',
	tluCOMN.ConfidenceClass.Code AS 'ConfidenceClassCode',
	tluCOMN.CoverClass.Code AS 'QuadData',
	tblMIDN.QuadratEventSpeciesCover.IsCollected AS 'IsCollected',
	tblMIDN.QuadratEventSpeciesCover.Note AS 'QuadSppNote',
	tluCOMN.Quadrat.Code+'_'+'cd' AS DataType,
	tluCOMN.TaxonomicReferenceAuthority_Identification.Code AS TRACode,
    tluCOMN.ProtectedStatus.Code AS ProtectedStatusCode,
	tluCOMN.DataProcessingLevel.Code AS DPLCode

FROM tblCOMN.[Event]
    LEFT JOIN tblCOMN.QuadratEvent ON tblCOMN.[Event].ID = tblCOMN.QuadratEvent.EventID
	LEFT JOIN tluCOMN.Quadrat ON tblCOMN.QuadratEvent.QuadratID = tluCOMN.Quadrat.ID
	LEFT JOIN tblMIDN.QuadratEventSpeciesCover ON tblCOMN.QuadratEvent.ID = tblMIDN.QuadratEventSpeciesCover.QuadratEventID
	LEFT JOIN tluCOMN.CoverClass ON tblMIDN.QuadratEventSpeciesCover.CoverClassID = tluCOMN.CoverClass.ID
	LEFT JOIN tluCOMN.ConfidenceClass ON tblMIDN.QuadratEventSpeciesCover.ConfidenceClassID = tluCOMN.ConfidenceClass.ID
	LEFT JOIN tluCOMN.Taxon ON tblMIDN.QuadratEventSpeciesCover.TaxonID = tluCOMN.Taxon.ID
	LEFT OUTER JOIN tluCOMN.TaxonomicReferenceAuthority_Identification ON
	  tblMIDN.QuadratEventSpeciesCover.TaxonomicReferenceAuthority_IdentificationID = tluCOMN.TaxonomicReferenceAuthority_Identification.ID
	LEFT OUTER JOIN tluCOMN.ProtectedStatus ON
	  tblMIDN.QuadratEventSpeciesCover.ProtectedStatusID = tluCOMN.ProtectedStatus.ID
	LEFT OUTER JOIN tluCOMN.DataProcessingLevel ON
	  tblCOMN.QuadratEvent.DataProcessingLevelID = tluCOMN.DataProcessingLevel.ID

UNION
-- Query 2B: Cover Class Label
SELECT
	tblCOMN.[Event].ID AS 'EventID',
	tluCOMN.Taxon.TSN AS 'TSN',
	tluCOMN.Taxon.ScientificName AS 'ScientificName_tmp',
	tluCOMN.ConfidenceClass.Code AS 'ConfidenceClassCode',
	tluCOMN.CoverClass.Label AS 'QuadData',
	tblMIDN.QuadratEventSpeciesCover.IsCollected AS 'IsCollected',
	tblMIDN.QuadratEventSpeciesCover.Note AS 'QuadSppNote',
	tluCOMN.Quadrat.Code+'_'+'txt' AS DataType,
	tluCOMN.TaxonomicReferenceAuthority_Identification.Code AS TRACode,
    tluCOMN.ProtectedStatus.Code AS ProtectedStatusCode,
	tluCOMN.DataProcessingLevel.Code AS DPLCode

FROM tblCOMN.[Event]
    LEFT JOIN tblCOMN.QuadratEvent ON tblCOMN.[Event].ID = tblCOMN.QuadratEvent.EventID
	LEFT JOIN tluCOMN.Quadrat ON tblCOMN.QuadratEvent.QuadratID = tluCOMN.Quadrat.ID
	LEFT JOIN tblMIDN.QuadratEventSpeciesCover ON tblCOMN.QuadratEvent.ID = tblMIDN.QuadratEventSpeciesCover.QuadratEventID
	LEFT JOIN tluCOMN.CoverClass ON tblMIDN.QuadratEventSpeciesCover.CoverClassID = tluCOMN.CoverClass.ID
	LEFT JOIN tluCOMN.ConfidenceClass ON tblMIDN.QuadratEventSpeciesCover.ConfidenceClassID = tluCOMN.ConfidenceClass.ID
	LEFT JOIN tluCOMN.Taxon ON tblMIDN.QuadratEventSpeciesCover.TaxonID = tluCOMN.Taxon.ID
	LEFT OUTER JOIN tluCOMN.TaxonomicReferenceAuthority_Identification ON
	  tblMIDN.QuadratEventSpeciesCover.TaxonomicReferenceAuthority_IdentificationID = tluCOMN.TaxonomicReferenceAuthority_Identification.ID
	LEFT OUTER JOIN tluCOMN.ProtectedStatus ON
	  tblMIDN.QuadratEventSpeciesCover.ProtectedStatusID = tluCOMN.ProtectedStatus.ID
	LEFT OUTER JOIN tluCOMN.DataProcessingLevel ON
	  tblCOMN.QuadratEvent.DataProcessingLevelID = tluCOMN.DataProcessingLevel.ID
	) AS SPP

-- Pivot Query 2 wide on Cover Code and Cover Label
PIVOT (MAX(SPP.QuadData) FOR DataType IN
  (A2_cd, A5_cd, A8_cd, AA_cd, B2_cd, B5_cd, B8_cd, BB_cd, C2_cd, C5_cd, C8_cd, CC_cd,
   A2_txt, A5_txt, A8_txt, AA_txt, B2_txt, B5_txt, B8_txt, BB_txt, C2_txt, C5_txt, C8_txt, CC_txt
   )) AS PIVSPP1) AS PIVSPP

ON PIVSQ.EventID = PIVSPP.EventID

-- Filter out unwanted NULLs
WHERE (SQQuadSum > 0 AND SQQuadSum < 12 AND TSN IS NOT NULL AND ScientificName_tmp IS NOT NULL ) -- drops the extra null record
OR (SQQuadSum = 0) -- keeps null TSN and ScientificName if all quads were not sampled
OR (SQQuadSum = 12) -- keeps all plots with all quads sampled 