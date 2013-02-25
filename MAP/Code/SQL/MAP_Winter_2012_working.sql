SELECT  f12.*,
        w13.Winter13_RIT,
        w13.Winter13_Pctl,
        w13.Winter13_Quartile
FROM (
SELECT  t.StudentID AS ID,
		t.`StudentFirstName`,
		t.`StudentLastName`,
		t.`SchoolName`,
		t.`Grade`,	
		t.`ClassName`,
		t.MeasurementScale AS Subject,
		t.GrowthMeasureYN AS Fall12_GM,
		t.TestType AS  Fall12_TT, 
		t.TestRITScore AS Fall12_RIT,
		t.TestPercentile AS Fall12_Pctl,
		n.t42 as TypicalFallToSpringGrowth,
		n.r42 as ReportedFallToSpringGrowth,
		n.s42 as SDFallToSpringGrowth,
		n.t41 as TypicalFallToWinterGrowth,
		n.r41 as ReportedFallToWingerGrowth,
		n.s41 as SDFallToWinterGrowth,
		CASE
			WHEN TestPercentile >= 75 THEN 4
			WHEN TestPercentile < 75 AND TestPercentile>=50 THEN 3
			WHEN TestPercentile < 50 AND TestPercentile>=25 THEN 2
			ELSE 1
		END AS Quartile

FROM 	(
		SELECT 	a.*,
				c.ClassName
		FROM `tblClassAssignmentsFall12` as c
		JOIN (
			Select 	r.*, 
					s.DistrictName,
					s.`StudentDateOfBirth`,
					s.`StudentEthnicGroup`,
					s.`StudentLastName`,
					s.`StudentFirstName`,
					s.`StudentMI`,
					s.`Grade`
			FROM	tblAssessmentResultsFall12 as r
	    	JOIN	tblStudentBySchoolFall12 as s
			ON		r.`StudentID`=s.StudentID
			) as a
		ON a.StudentID=c.StudentID
		) as t
LEFT OUTER JOIN `viewNorms2011_Growth_Kinder_0` as n
ON 		t.`TestRITScore`=n.`StartRIT`
AND		t.`Grade`=n.`StartGrade2`
AND		t.`MeasurementScale`=n.`MeasurementScale`
WHERE GrowthMeasureYN='True' 
  AND
 	(TestType='Survey with Goals'
		OR 
		TestType='Survey'
		)
  ) as f12
    JOIN
   (
--Get Winter Scores
SELECT  t.StudentID AS ID,
		t.`StudentFirstName`,
		t.`StudentLastName`,
		t.`SchoolName`,
		t.`Grade`,	
		t.`ClassName`,
		t.MeasurementScale AS Subject,
		t.GrowthMeasureYN AS Winter13_GM,
		t.TestType AS  Winter13_TT,
		t.TestRITScore AS Winter13_RIT,
		t.TestPercentile AS Winter13_Pctl,
		CASE
			WHEN TestPercentile >= 75 THEN 4
			WHEN TestPercentile < 75 AND TestPercentile>=50 THEN 3
			WHEN TestPercentile < 50 AND TestPercentile>=25 THEN 2
			ELSE 1
		END AS Winter13_Quartile

FROM 	(
		SELECT 	a.*,
				c.ClassName
		FROM `tblClassAssignmentsWinter13` as c
		JOIN (
			Select 	r.*, 
					s.DistrictName,
					s.`StudentDateOfBirth`,
					s.`StudentEthnicGroup`,
					s.`StudentLastName`,
					s.`StudentFirstName`,
					s.`StudentMI`,
					s.`Grade`
			FROM	tblAssessmentResultsWinter13 as r
	    	JOIN	tblStudentBySchoolWinter13 as s
			ON		r.`StudentID`=s.StudentID
			) as a
		ON a.StudentID=c.StudentID
		) as t
WHERE GrowthMeasureYN='True' 
  AND
 	(TestType='Survey with Goals'
		OR 
		TestType='Survey'
		)
  ) as w13
    ON f12.ID=w13.ID
    AND f12.Subject=w13.Subject
    ;
