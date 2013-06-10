DELIMITER ;;
DROP PROCEDURE IF EXISTS `GetMAPResultsFromToByName`;;


CREATE PROCEDURE `GetMAPResultsFromToByName`(Period1 VARCHAR(50), 
                                       Period2 VARCHAR(50))
BEGIN

SET @sql =	 CONCAT("SELECT  test1.*,
        test2.Grade AS ",Period2,"_Grade,
        test2.ClassName  AS ",Period2,"_Classname,
        test2.",Period2,"_RIT,
        test2.",Period2,"_Pctl,
        test2.",Period2,"_Quartile
FROM (
SELECT  t.StudentID AS ID,
    t.`StudentFirstName`,
  	t.`StudentLastName`,
  	t.`StudentDateOfBirth`,
		t.`SchoolName`,
		t.`Grade` AS ",Period1,"_Grade,	
		t.`ClassName` AS ", Period1,"_ClassName,
		t.MeasurementScale AS Subject,
		t.GrowthMeasureYN AS ",Period1,"_GM,
		t.TestType AS  ",Period1,"_TT, 
		t.TestRITScore AS ",Period1,"_RIT,
		t.TestPercentile AS ",Period1,"_Pctl,
		n.t42 as TypicalFallToSpringGrowth,
		n.r42 as ReportedFallToSpringGrowth,
		n.s42 as SDFallToSpringGrowth,
		n.t41 as TypicalFallToWinterGrowth,
		n.r41 as ReportedFallToWinterGrowth,
		n.s41 as SDFallToWinterGrowth,
  	n.t44 as TypicalFallToFallGrowth,
		n.r44 as ReportedFallToFallGrowth,
		n.s44 as SDFallToFallGrowth,
    n.t22 as TypicalSpringToSpringGrowth,
		n.r22 as ReportedSpringToSpringGrowth,
		n.s22 as SDSpringToSpringGrowth,
  	n.t12 as TypicalWinterToSpringGrowth,
		n.r12 as ReportedWinterToSpringGrowth,
		n.s12 as SDWinterToSpringGrowth,
		CASE
			WHEN TestPercentile >= 75 THEN 4
			WHEN TestPercentile < 75 AND TestPercentile>=50 THEN 3
			WHEN TestPercentile < 50 AND TestPercentile>=25 THEN 2
			ELSE 1
		END AS ",Period1,"_Quartile

FROM 	(
		SELECT 	a.*,
				c.ClassName
		FROM `tblClassAssignments",Period1,"` as c
		JOIN (
			Select 	r.*, 
					s.DistrictName,
					s.`StudentDateOfBirth`,
					s.`StudentEthnicGroup`,
					s.`StudentLastName`,
					s.`StudentFirstName`,
					s.`StudentMI`,
					s.`Grade`
			FROM	tblAssessmentResults",Period1," as r
	    	JOIN	tblStudentBySchool",Period1," as s
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
  ) as test1
    JOIN
   (
  SELECT  t.StudentID AS ID,
		t.`StudentFirstName`,
		t.`StudentLastName`,
		t.`StudentDateOfBirth`,
		t.`SchoolName`,
		t.`Grade`,	
		t.`ClassName`,
		t.MeasurementScale AS Subject,
		t.GrowthMeasureYN AS ",Period2,"_GM,
		t.TestType AS  ",Period2,"_TT,
		t.TestRITScore AS ",Period2,"_RIT,
		t.TestPercentile AS ",Period2,"_Pctl,
		CASE
			WHEN TestPercentile >= 75 THEN 4
			WHEN TestPercentile < 75 AND TestPercentile>=50 THEN 3
			WHEN TestPercentile < 50 AND TestPercentile>=25 THEN 2
			ELSE 1
		END AS ",Period2,"_Quartile

FROM 	(
		SELECT 	a.*,
				c.ClassName
		FROM `tblClassAssignments",Period2,"` as c
		JOIN (
			Select 	r.*, 
					s.DistrictName,
					s.`StudentDateOfBirth`,
					s.`StudentEthnicGroup`,
					s.`StudentLastName`,
					s.`StudentFirstName`,
					s.`StudentMI`,
					s.`Grade`
			FROM	tblAssessmentResults",Period2," as r
	    	JOIN	tblStudentBySchool",Period2," as s
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
  ) as test2
    ON test1.StudentLastName=test2.StudentLastName
    AND test1.StudentFirstName=test2.StudentFirstName
    AND test1.StudentDateOfBirth=test2.StudentDateOfBirth
    AND test1.Subject=test2.Subject
    ;");

PREPARE	sql_ex FROM @sql;

EXECUTE	sql_ex;
END;;
DELIMITER ;
