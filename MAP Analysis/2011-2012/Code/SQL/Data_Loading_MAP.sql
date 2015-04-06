-- This file loads all Assesment Results, Class Assignments, and Student By School csv files from NWEA's Comprehensive Data File
-- The orginal CDF's csv must be processed by map_cdf_prep.sh prior to loading (included in this directory), otherwise errors will probably ensue. 

############################
## 2013 - 2014            ##
############################

-- FALL 2012 MAP Comprehensive Data File
-- Load AssessmentResults
DROP TABLE IF EXISTS tblAssessmentResultsFall13;
CREATE TABLE tblAssessmentResultsFall13 (
	TermName VARCHAR(30), 
	StudentID INT, 
	SchoolName VARCHAR(100), 
	MeasurementScale VARCHAR(30),
	Discipline VARCHAR(11),
	GrowthMeasureYN VARCHAR(5), 
	TestType VARCHAR(20),
	TestName VARCHAR(100),
	TestID INT,
	TestStartDate VARCHAR(10),
	TestDurationInMinutes INT,
	TestRITScore SMALLINT(3),
	TestStandardError REAL(3,1),
	TestPercentile INT,
	TypicalFallToFallGrowth INT,
	TypicalSpringToSpringGrowth INT,
	TypicalFallToSpringGrowth INT,
	TypicalFallToWinterGrowth INT,
  RITtoReadingScore INT,
	RITtoReadingMin INT,
	RITtoReadingMax INT,
	Goal1Name VARCHAR(50),
	Goal1RitScore INT,
	Goal1StdErr REAL(3,1),
	Goal1Range VARCHAR(7),
	Goal1Adjective VARCHAR(2),
	Goal2Name VARCHAR(50),
	Goal2RitScore INT,
	Goal2StdErr REAL(3,1),
	Goal2Range VARCHAR(7),
	Goal2Adjective VARCHAR(2),
	Goal3Name VARCHAR(50),
	Goal3RitScore INT,
	Goal3StdErr REAL(3,1),
	Goal3Range VARCHAR(7),
	Goal3Adjective VARCHAR(2),
	Goal4Name VARCHAR(50),
	Goal4RitScore INT,
	Goal4StdErr REAL(3,1),
	Goal4Range VARCHAR(7),
	Goal4Adjective VARCHAR(2),
	Goal5Name VARCHAR(50),
	Goal5RitScore INT,
	Goal5StdErr REAL(3,1),
	Goal5Range VARCHAR(7),
	Goal5Adjective VARCHAR(2),
	Goal6Name VARCHAR(50),
	Goal6RitScore INT,
	Goal6StdErr REAL(3,1),
	Goal6Range VARCHAR(7),
	Goal6Adjective VARCHAR(2),
	Goal7Name VARCHAR(50),
	Goal7RitScore INT,
	Goal7StdErr REAL(3,1),
	Goal7Range VARCHAR(7),
	Goal7Adjective VARCHAR(2),
	Goal8Name VARCHAR(50),
	Goal8RitScore INT,
	Goal8StdErr REAL(3,1),
	Goal8Range VARCHAR(7),
	Goal8Adjective VARCHAR(2),
	TestStartTime TIME,
	PercentCorrect INT,
	ProjectedProficiency VARCHAR(17)
	);
	
	

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2013/AssessmentResults_loaddata.csv'
	INTO TABLE tblAssessmentResultsFall13
	FIELDS TERMINATED BY ','
	;



-- Load Class Assignments
DROP TABLE IF EXISTS tblClassAssignmentsFall13;	
CREATE TABLE tblClassAssignmentsFall13 (
	TermName VARCHAR(30),
	StudentID INT,
	SchoolName VARCHAR(50),
	ClassName VARCHAR(50),
	TeacherName VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2013/ClassAssignments_loaddata.csv'
	INTO TABLE tblClassAssignmentsFall13
	FIELDS TERMINATED BY ','
	;


-- Load StudentBySchool	
DROP TABLE IF EXISTS tblStudentBySchoolFall13;
CREATE TABLE tblStudentBySchoolFall13 (
	TermName VARCHAR(30),
	DistrictName VARCHAR(50),
	SchoolName VARCHAR(50),
	StudentLastName VARCHAR(50),
	StudentFirstName VARCHAR(50),
	StudentMI VARCHAR(2),
	StudentID INT,
	StudentDateOfBirth VARCHAR(10),
	StudentEthnicGroup VARCHAR(20),
	StudentGender VARCHAR(1),
	Grade INT);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2013/StudentsBySchool_loaddata.csv'
	INTO TABLE tblStudentBySchoolFall13
	FIELDS TERMINATED BY ','
	;

-- Load Program Assignments (i.e. Special Education)
DROP TABLE IF EXISTS tblProgramAssignmentsFall13;	
CREATE TABLE tblProgramAssignmentsFall13 (
	TermName VARCHAR(30),
	StudentID INT,
	Program VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2013/ProgramAssignments_loaddata.csv'
	INTO TABLE tblProgramAssignmentsFall13
	FIELDS TERMINATED BY ','
	;
--Need delete an extra, erroneous record from Class Assignments for every Primary class caused by Bass (Special Ed teacher)
--having access to each class.
DELETE FROM tblClassAssignmentsFall13
WHERE TeacherName="Bass"
AND SchoolName="KIPP Ascend Primary";






#############################
## 2012 - 2013             ##
#############################
-- SPRING 2013 MAP Comprehensive Data File
-- Load AssessmentResults
DROP TABLE IF EXISTS tblAssessmentResultsSpring13;
CREATE TABLE tblAssessmentResultsSpring13 (
	TermName VARCHAR(30), 
	StudentID INT, 
	SchoolName VARCHAR(100), 
	MeasurementScale VARCHAR(30),
	Discipline VARCHAR(11),
	GrowthMeasureYN VARCHAR(5), 
	TestType VARCHAR(20),
	TestName VARCHAR(100),
	TestID INT,
	TestStartDate VARCHAR(10),
	TestDurationInMinutes INT,
	TestRITScore SMALLINT(3),
	TestStandardError REAL(3,1),
	TestPercentile INT,
	TypicalFallToFallGrowth INT,
	TypicalSpringToSpringGrowth INT,
	TypicalFallToSpringGrowth INT,
	RITtoReadingScore INT,
	RITtoReadingMin INT,
	RITtoReadingMax INT,
	Goal1Name VARCHAR(50),
	Goal1RitScore INT,
	Goal1StdErr REAL(3,1),
	Goal1Range VARCHAR(7),
	Goal1Adjective VARCHAR(2),
	Goal2Name VARCHAR(50),
	Goal2RitScore INT,
	Goal2StdErr REAL(3,1),
	Goal2Range VARCHAR(7),
	Goal2Adjective VARCHAR(2),
	Goal3Name VARCHAR(50),
	Goal3RitScore INT,
	Goal3StdErr REAL(3,1),
	Goal3Range VARCHAR(7),
	Goal3Adjective VARCHAR(2),
	Goal4Name VARCHAR(50),
	Goal4RitScore INT,
	Goal4StdErr REAL(3,1),
	Goal4Range VARCHAR(7),
	Goal4Adjective VARCHAR(2),
	Goal5Name VARCHAR(50),
	Goal5RitScore INT,
	Goal5StdErr REAL(3,1),
	Goal5Range VARCHAR(7),
	Goal5Adjective VARCHAR(2),
	Goal6Name VARCHAR(50),
	Goal6RitScore INT,
	Goal6StdErr REAL(3,1),
	Goal6Range VARCHAR(7),
	Goal6Adjective VARCHAR(2),
	Goal7Name VARCHAR(50),
	Goal7RitScore INT,
	Goal7StdErr REAL(3,1),
	Goal7Range VARCHAR(7),
	Goal7Adjective VARCHAR(2),
	Goal8Name VARCHAR(50),
	Goal8RitScore INT,
	Goal8StdErr REAL(3,1),
	Goal8Range VARCHAR(7),
	Goal8Adjective VARCHAR(2),
	TestStartTime TIME,
	PercentCorrect INT,
	ProjectedProficiency VARCHAR(17)
	);
	
	

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2013/AssessmentResults_loaddata.csv'
	INTO TABLE tblAssessmentResultsSpring13
	FIELDS TERMINATED BY ','
	;



-- Load Class Assignments
DROP TABLE IF EXISTS tblClassAssignmentsSpring13;	
CREATE TABLE tblClassAssignmentsSpring13 (
	TermName VARCHAR(30),
	StudentID INT,
	SchoolName VARCHAR(50),
	ClassName VARCHAR(50),
	TeacherName VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2013/ClassAssignments_loaddata.csv'
	INTO TABLE tblClassAssignmentsSpring13
	FIELDS TERMINATED BY ','
	;


-- Load StudentBySchool	
DROP TABLE IF EXISTS tblStudentBySchoolSpring13;
CREATE TABLE tblStudentBySchoolSpring13 (
	TermName VARCHAR(30),
	DistrictName VARCHAR(50),
	SchoolName VARCHAR(50),
	StudentLastName VARCHAR(50),
	StudentFirstName VARCHAR(50),
	StudentMI VARCHAR(2),
	StudentID INT,
	StudentDateOfBirth VARCHAR(10),
	StudentEthnicGroup VARCHAR(20),
	StudentGender VARCHAR(1),
	Grade INT);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2013/StudentsBySchool_loaddata.csv'
	INTO TABLE tblStudentBySchoolSpring13
	FIELDS TERMINATED BY ','
	;

-- Load Program Assignments (i.e. Special Education)
DROP TABLE IF EXISTS tblProgramAssignmentsSpring13;	
CREATE TABLE tblProgramAssignmentsSpring13 (
	TermName VARCHAR(30),
	StudentID INT,
	Program VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2013/ProgramAssignments_loaddata.csv'
	INTO TABLE tblProgramAssignmentsSpring13
	FIELDS TERMINATED BY ','
	;


-- WINTER 2013 MAP Comprehensive Data File
-- Load AssessmentResults
DROP TABLE IF EXISTS tblAssessmentResultsWinter13;
CREATE TABLE tblAssessmentResultsWinter13 (
	TermName VARCHAR(30), 
	StudentID INT, 
	SchoolName VARCHAR(100), 
	MeasurementScale VARCHAR(30),
	Discipline VARCHAR(11),
	GrowthMeasureYN VARCHAR(5), 
	TestType VARCHAR(20),
	TestName VARCHAR(100),
	TestID INT,
	TestStartDate VARCHAR(10),
	TestDurationInMinutes INT,
	TestRITScore SMALLINT(3),
	TestStandardError REAL(3,1),
	TestPercentile INT,
	TypicalFallToFallGrowth INT,
	TypicalSpringToSpringGrowth INT,
	TypicalFallToSpringGrowth INT,
	RITtoReadingScore INT,
	RITtoReadingMin INT,
	RITtoReadingMax INT,
	Goal1Name VARCHAR(50),
	Goal1RitScore INT,
	Goal1StdErr REAL(3,1),
	Goal1Range VARCHAR(7),
	Goal1Adjective VARCHAR(2),
	Goal2Name VARCHAR(50),
	Goal2RitScore INT,
	Goal2StdErr REAL(3,1),
	Goal2Range VARCHAR(7),
	Goal2Adjective VARCHAR(2),
	Goal3Name VARCHAR(50),
	Goal3RitScore INT,
	Goal3StdErr REAL(3,1),
	Goal3Range VARCHAR(7),
	Goal3Adjective VARCHAR(2),
	Goal4Name VARCHAR(50),
	Goal4RitScore INT,
	Goal4StdErr REAL(3,1),
	Goal4Range VARCHAR(7),
	Goal4Adjective VARCHAR(2),
	Goal5Name VARCHAR(50),
	Goal5RitScore INT,
	Goal5StdErr REAL(3,1),
	Goal5Range VARCHAR(7),
	Goal5Adjective VARCHAR(2),
	Goal6Name VARCHAR(50),
	Goal6RitScore INT,
	Goal6StdErr REAL(3,1),
	Goal6Range VARCHAR(7),
	Goal6Adjective VARCHAR(2),
	Goal7Name VARCHAR(50),
	Goal7RitScore INT,
	Goal7StdErr REAL(3,1),
	Goal7Range VARCHAR(7),
	Goal7Adjective VARCHAR(2),
	Goal8Name VARCHAR(50),
	Goal8RitScore INT,
	Goal8StdErr REAL(3,1),
	Goal8Range VARCHAR(7),
	Goal8Adjective VARCHAR(2),
	TestStartTime TIME,
	PercentCorrect INT,
	ProjectedProficiency VARCHAR(17)
	);
	
	

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Winter_2013/AssessmentResults_loaddata.csv'
	INTO TABLE tblAssessmentResultsWinter13
	FIELDS TERMINATED BY ','
	;



-- Load Class Assignments
DROP TABLE IF EXISTS tblClassAssignmentsWinter13;	
CREATE TABLE tblClassAssignmentsWinter13 (
	TermName VARCHAR(30),
	StudentID INT,
	SchoolName VARCHAR(50),
	ClassName VARCHAR(50),
	TeacherName VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Winter_2013/ClassAssignments_loaddata.csv'
	INTO TABLE tblClassAssignmentsWinter13
	FIELDS TERMINATED BY ','
	;


-- Load StudentBySchool	
DROP TABLE IF EXISTS tblStudentBySchoolWinter13;
CREATE TABLE tblStudentBySchoolWinter13 (
	TermName VARCHAR(30),
	DistrictName VARCHAR(50),
	SchoolName VARCHAR(50),
	StudentLastName VARCHAR(50),
	StudentFirstName VARCHAR(50),
	StudentMI VARCHAR(2),
	StudentID INT,
	StudentDateOfBirth VARCHAR(10),
	StudentEthnicGroup VARCHAR(20),
	StudentGender VARCHAR(1),
	Grade INT);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Winter_2013/StudentsBySchool_loaddata.csv'
	INTO TABLE tblStudentBySchoolWinter13
	FIELDS TERMINATED BY ','
	;

-- Load Program Assignments (i.e. Special Education)
DROP TABLE IF EXISTS tblProgramAssignmentsWinter13;	
CREATE TABLE tblProgramAssignmentsWinter13 (
	TermName VARCHAR(30),
	StudentID INT,
	Program VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Winter_2013/ProgramAssignments_loaddata.csv'
	INTO TABLE tblProgramAssignmentsWinter13
	FIELDS TERMINATED BY ','
	;

-- FALL 2012 MAP Comprehensive Data File
-- Load AssessmentResults
DROP TABLE IF EXISTS tblAssessmentResultsFall12;
CREATE TABLE tblAssessmentResultsFall12 (
	TermName VARCHAR(30), 
	StudentID INT, 
	SchoolName VARCHAR(100), 
	MeasurementScale VARCHAR(30),
	Discipline VARCHAR(11),
	GrowthMeasureYN VARCHAR(5), 
	TestType VARCHAR(20),
	TestName VARCHAR(100),
	TestID INT,
	TestStartDate VARCHAR(10),
	TestDurationInMinutes INT,
	TestRITScore SMALLINT(3),
	TestStandardError REAL(3,1),
	TestPercentile INT,
	TypicalFallToFallGrowth INT,
	TypicalSpringToSpringGrowth INT,
	TypicalFallToSpringGrowth INT,
	RITtoReadingScore INT,
	RITtoReadingMin INT,
	RITtoReadingMax INT,
	Goal1Name VARCHAR(50),
	Goal1RitScore INT,
	Goal1StdErr REAL(3,1),
	Goal1Range VARCHAR(7),
	Goal1Adjective VARCHAR(2),
	Goal2Name VARCHAR(50),
	Goal2RitScore INT,
	Goal2StdErr REAL(3,1),
	Goal2Range VARCHAR(7),
	Goal2Adjective VARCHAR(2),
	Goal3Name VARCHAR(50),
	Goal3RitScore INT,
	Goal3StdErr REAL(3,1),
	Goal3Range VARCHAR(7),
	Goal3Adjective VARCHAR(2),
	Goal4Name VARCHAR(50),
	Goal4RitScore INT,
	Goal4StdErr REAL(3,1),
	Goal4Range VARCHAR(7),
	Goal4Adjective VARCHAR(2),
	Goal5Name VARCHAR(50),
	Goal5RitScore INT,
	Goal5StdErr REAL(3,1),
	Goal5Range VARCHAR(7),
	Goal5Adjective VARCHAR(2),
	Goal6Name VARCHAR(50),
	Goal6RitScore INT,
	Goal6StdErr REAL(3,1),
	Goal6Range VARCHAR(7),
	Goal6Adjective VARCHAR(2),
	Goal7Name VARCHAR(50),
	Goal7RitScore INT,
	Goal7StdErr REAL(3,1),
	Goal7Range VARCHAR(7),
	Goal7Adjective VARCHAR(2),
	Goal8Name VARCHAR(50),
	Goal8RitScore INT,
	Goal8StdErr REAL(3,1),
	Goal8Range VARCHAR(7),
	Goal8Adjective VARCHAR(2),
	TestStartTime TIME,
	PercentCorrect INT,
	ProjectedProficiency VARCHAR(17)
	);
	
	

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2012/AssessmentResults_loaddata.csv'
	INTO TABLE tblAssessmentResultsFall12
	FIELDS TERMINATED BY ','
	;



-- Load Class Assignments
DROP TABLE IF EXISTS tblClassAssignmentsFall12;	
CREATE TABLE tblClassAssignmentsFall12 (
	TermName VARCHAR(30),
	StudentID INT,
	SchoolName VARCHAR(50),
	ClassName VARCHAR(50),
	TeacherName VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2012/ClassAssignments_loaddata.csv'
	INTO TABLE tblClassAssignmentsFall12
	FIELDS TERMINATED BY ','
	;


-- Load StudentBySchool	
DROP TABLE IF EXISTS tblStudentBySchoolFall12;
CREATE TABLE tblStudentBySchoolFall12 (
	TermName VARCHAR(30),
	DistrictName VARCHAR(50),
	SchoolName VARCHAR(50),
	StudentLastName VARCHAR(50),
	StudentFirstName VARCHAR(50),
	StudentMI VARCHAR(2),
	StudentID INT,
	StudentDateOfBirth VARCHAR(10),
	StudentEthnicGroup VARCHAR(20),
	StudentGender VARCHAR(1),
	Grade INT);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2012/StudentsBySchool_loaddata.csv'
	INTO TABLE tblStudentBySchoolFall12
	FIELDS TERMINATED BY ','
	;

-- Load Program Assignments (i.e. Special Education)
DROP TABLE IF EXISTS tblProgramAssignmentsFall12;	
CREATE TABLE tblProgramAssignmentsFall12 (
	TermName VARCHAR(30),
	StudentID INT,
	Program VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2012/ProgramAssignments_loaddata.csv'
	INTO TABLE tblProgramAssignmentsFall12
	FIELDS TERMINATED BY ','
	;
--Need delete an extra, erroneous record from Class Assignments for every Primary class caused by Bass (Special Ed teacher)
--having access to each class.
DELETE FROM tblClassAssignmentsFall12
WHERE TeacherName="Bass"
AND SchoolName="KIPP Ascend Primary";

###########################
### 2011-12              ##
###########################

-- WINTER 2012 MAP Comprehensive Data File
-- Load AssessmentResults
DROP TABLE IF EXISTS tblAssessmentResultsWinter12;
CREATE TABLE tblAssessmentResultsWinter12 (
	TermName VARCHAR(30), 
	StudentID INT, 
	SchoolName VARCHAR(100), 
	MeasurementScale VARCHAR(30),
	Discipline VARCHAR(11),
	GrowthMeasureYN VARCHAR(5), 
	TestType VARCHAR(20),
	TestName VARCHAR(100),
	TestID INT,
	TestStartDate VARCHAR(10),
	TestDurationInMinutes INT,
	TestRITScore SMALLINT(3),
	TestStandardError REAL(3,1),
	TestPercentile INT,
	TypicalFallToFallGrowth INT,
	TypicalSpringToSpringGrowth INT,
	TypicalFallToSpringGrowth INT,
	RITtoReadingScore INT,
	RITtoReadingMin INT,
	RITtoReadingMax INT,
	Goal1Name VARCHAR(50),
	Goal1RitScore INT,
	Goal1StdErr REAL(3,1),
	Goal1Range VARCHAR(7),
	Goal1Adjective VARCHAR(2),
	Goal2Name VARCHAR(50),
	Goal2RitScore INT,
	Goal2StdErr REAL(3,1),
	Goal2Range VARCHAR(7),
	Goal2Adjective VARCHAR(2),
	Goal3Name VARCHAR(50),
	Goal3RitScore INT,
	Goal3StdErr REAL(3,1),
	Goal3Range VARCHAR(7),
	Goal3Adjective VARCHAR(2),
	Goal4Name VARCHAR(50),
	Goal4RitScore INT,
	Goal4StdErr REAL(3,1),
	Goal4Range VARCHAR(7),
	Goal4Adjective VARCHAR(2),
	Goal5Name VARCHAR(50),
	Goal5RitScore INT,
	Goal5StdErr REAL(3,1),
	Goal5Range VARCHAR(7),
	Goal5Adjective VARCHAR(2),
	Goal6Name VARCHAR(50),
	Goal6RitScore INT,
	Goal6StdErr REAL(3,1),
	Goal6Range VARCHAR(7),
	Goal6Adjective VARCHAR(2),
	Goal7Name VARCHAR(50),
	Goal7RitScore INT,
	Goal7StdErr REAL(3,1),
	Goal7Range VARCHAR(7),
	Goal7Adjective VARCHAR(2),
	Goal8Name VARCHAR(50),
	Goal8RitScore INT,
	Goal8StdErr REAL(3,1),
	Goal8Range VARCHAR(7),
	Goal8Adjective VARCHAR(2),
	TestStartTime TIME,
	PercentCorrect INT,
	ProjectedProficiency VARCHAR(17)
	);
	
	

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Winter_2012/AssessmentResults_loaddata.csv'
	INTO TABLE tblAssessmentResultsWinter12
	FIELDS TERMINATED BY ','
	;



-- Load Class Assignments
DROP TABLE IF EXISTS tblClassAssignmentsWinter12;	
CREATE TABLE tblClassAssignmentsWinter12 (
	TermName VARCHAR(30),
	StudentID INT,
	SchoolName VARCHAR(50),
	ClassName VARCHAR(50),
	TeacherName VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Winter_2012/ClassAssignments_loaddata.csv'
	INTO TABLE tblClassAssignmentsWinter12
	FIELDS TERMINATED BY ','
	;


-- Load StudentBySchool	
DROP TABLE IF EXISTS tblStudentBySchoolWinter12;
CREATE TABLE tblStudentBySchoolWinter12 (
	TermName VARCHAR(30),
	DistrictName VARCHAR(50),
	SchoolName VARCHAR(50),
	StudentLastName VARCHAR(50),
	StudentFirstName VARCHAR(50),
	StudentMI VARCHAR(2),
	StudentID INT,
	StudentDateOfBirth VARCHAR(10),
	StudentEthnicGroup VARCHAR(20),
	StudentGender VARCHAR(1),
	Grade INT);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Winter_2012/StudentsBySchool_loaddata.csv'
	INTO TABLE tblStudentBySchoolWinter12
	FIELDS TERMINATED BY ','
	;

-- Load Program Assignments (i.e. Special Education)
DROP TABLE IF EXISTS tblProgramAssignmentsWinter12;	
CREATE TABLE tblProgramAssignmentsWinter12 (
	TermName VARCHAR(30),
	StudentID INT,
	Program VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Winter_2012/ProgramAssignments_loaddata.csv'
	INTO TABLE tblProgramAssignmentsWinter12
	FIELDS TERMINATED BY ','
	;


-- FALL 2011 MAP Comprehensive Data File
-- Load AssessmentResults
DROP TABLE IF EXISTS tblAssessmentResultsFall11;
CREATE TABLE tblAssessmentResultsFall11 (
	TermName VARCHAR(30), 
	StudentID INT, 
	SchoolName VARCHAR(100), 
	MeasurementScale VARCHAR(30),
	Discipline VARCHAR(11),
	GrowthMeasureYN VARCHAR(5), 
	TestType VARCHAR(20),
	TestName VARCHAR(100),
	TestStartDate VARCHAR(10),
	TestDurationInMinutes INT,
	TestRITScore SMALLINT(3),
	TestStandardError REAL(3,1),
	TestPercentile INT,
	TypicalFallToFallGrowth INT,
	TypicalSpringToSpringGrowth INT,
	TypicalFallToSpringGrowth INT,
	RITtoReadingScore INT,
	RITtoReadingMin INT,
	RITtoReadingMax INT,
	Goal1Name VARCHAR(50),
	Goal1RitScore INT,
	Goal1StdErr REAL(3,1),
	Goal1Range VARCHAR(7),
	Goal1Adjective VARCHAR(2),
	Goal2Name VARCHAR(50),
	Goal2RitScore INT,
	Goal2StdErr REAL(3,1),
	Goal2Range VARCHAR(7),
	Goal2Adjective VARCHAR(2),
	Goal3Name VARCHAR(50),
	Goal3RitScore INT,
	Goal3StdErr REAL(3,1),
	Goal3Range VARCHAR(7),
	Goal3Adjective VARCHAR(2),
	Goal4Name VARCHAR(50),
	Goal4RitScore INT,
	Goal4StdErr REAL(3,1),
	Goal4Range VARCHAR(7),
	Goal4Adjective VARCHAR(2),
	Goal5Name VARCHAR(50),
	Goal5RitScore INT,
	Goal5StdErr REAL(3,1),
	Goal5Range VARCHAR(7),
	Goal5Adjective VARCHAR(2),
	Goal6Name VARCHAR(50),
	Goal6RitScore INT,
	Goal6StdErr REAL(3,1),
	Goal6Range VARCHAR(7),
	Goal6Adjective VARCHAR(2),
	Goal7Name VARCHAR(50),
	Goal7RitScore INT,
	Goal7StdErr REAL(3,1),
	Goal7Range VARCHAR(7),
	Goal7Adjective VARCHAR(2),
	Goal8Name VARCHAR(50),
	Goal8RitScore INT,
	Goal8StdErr REAL(3,1),
	Goal8Range VARCHAR(7),
	Goal8Adjective VARCHAR(2),
	TestStartTime TIME,
	PercentCorrect INT,
	ProjectedProficiency VARCHAR(17)
	);
	
	

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2011/AssessmentResults_loaddata.csv'
	INTO TABLE tblAssessmentResultsFall11
	FIELDS TERMINATED BY ','
	;



-- Load Class Assignments
DROP TABLE IF EXISTS tblClassAssignmentsFall11;	
CREATE TABLE tblClassAssignmentsFall11 (
	TermName VARCHAR(30),
	StudentID INT,
	SchoolName VARCHAR(50),
	ClassName VARCHAR(50),
	TeacherName VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2012/ClassAssignments_loaddata.csv'
	INTO TABLE tblClassAssignmentsFall11
	FIELDS TERMINATED BY ','
	;


-- Load StudentBySchool	
DROP TABLE IF EXISTS tblStudentBySchoolFall11;
CREATE TABLE tblStudentBySchoolFall11 (
	TermName VARCHAR(30),
	DistrictName VARCHAR(50),
	SchoolName VARCHAR(50),
	StudentLastName VARCHAR(50),
	StudentFirstName VARCHAR(50),
	StudentMI VARCHAR(2),
	StudentID INT,
	StudentDateOfBirth VARCHAR(10),
	StudentEthnicGroup VARCHAR(20),
	StudentGender VARCHAR(1),
	Grade INT);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2011/StudentBySchool_loaddata.csv'
	INTO TABLE tblStudentBySchoolFall11
	FIELDS TERMINATED BY ','
	;



-- SPRING 2011 MAP Comprehensive Data File
-- Load AssessmentResults
CREATE TABLE tblAssessmentResultsSpring12 (
	TermName VARCHAR(30), 
	StudentID INT, 
	SchoolName VARCHAR(100), 
	MeasurementScale VARCHAR(30),
	Discipline VARCHAR(11),
	GrowthMeasureYN VARCHAR(5), 
	TestType VARCHAR(20),
	TestName VARCHAR(100),
	TestStartDate VARCHAR(10),
	TestDurationInMinutes INT,
	TestRITScore SMALLINT(3),
	TestStandardError REAL(3,1),
	TestPercentile INT,
	TypicalFallToFallGrowth INT,
	TypicalSpringToSpringGrowth INT,
	TypicalFallToSpringGrowth INT,
	RITtoReadingScore INT,
	RITtoReadingMin INT,
	RITtoReadingMax INT,
	Goal1Name VARCHAR(50),
	Goal1RitScore INT,
	Goal1StdErr REAL(3,1),
	Goal1Range VARCHAR(7),
	Goal1Adjective VARCHAR(2),
	Goal2Name VARCHAR(50),
	Goal2RitScore INT,
	Goal2StdErr REAL(3,1),
	Goal2Range VARCHAR(7),
	Goal2Adjective VARCHAR(2),
	Goal3Name VARCHAR(50),
	Goal3RitScore INT,
	Goal3StdErr REAL(3,1),
	Goal3Range VARCHAR(7),
	Goal3Adjective VARCHAR(2),
	Goal4Name VARCHAR(50),
	Goal4RitScore INT,
	Goal4StdErr REAL(3,1),
	Goal4Range VARCHAR(7),
	Goal4Adjective VARCHAR(2),
	Goal5Name VARCHAR(50),
	Goal5RitScore INT,
	Goal5StdErr REAL(3,1),
	Goal5Range VARCHAR(7),
	Goal5Adjective VARCHAR(2),
	Goal6Name VARCHAR(50),
	Goal6RitScore INT,
	Goal6StdErr REAL(3,1),
	Goal6Range VARCHAR(7),
	Goal6Adjective VARCHAR(2),
	Goal7Name VARCHAR(50),
	Goal7RitScore INT,
	Goal7StdErr REAL(3,1),
	Goal7Range VARCHAR(7),
	Goal7Adjective VARCHAR(2),
	Goal8Name VARCHAR(50),
	Goal8RitScore INT,
	Goal8StdErr REAL(3,1),
	Goal8Range VARCHAR(7),
	Goal8Adjective VARCHAR(2),
	TestStartTime TIME,
	PercentCorrect INT,
	ProjectedProficiency VARCHAR(17)
	);



LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2012/AssessmentResults_loaddata.csv'
	INTO TABLE tblAssessmentResultsSpring12
	FIELDS TERMINATED BY ','
	;



-- Load Class Assignments	

CREATE TABLE tblClassAssignmentsSpring12 (
	TermName VARCHAR(30),
	StudentID INT,
	SchoolName VARCHAR(50),
	ClassName VARCHAR(50),
	TeacherName VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2012/ClassAssignments_loaddata.csv'
	INTO TABLE tblClassAssignmentsSpring12
	FIELDS TERMINATED BY ','
	;


-- Load StudentBySchool	
CREATE TABLE tblStudentBySchoolSpring12 (
	TermName VARCHAR(30),
	DistrictName VARCHAR(50),
	SchoolName VARCHAR(50),
	StudentLastName VARCHAR(50),
	StudentFirstName VARCHAR(50),
	StudentMI VARCHAR(2),
	StudentID INT,
	StudentDateOfBirth VARCHAR(10),
	StudentEthnicGroup VARCHAR(20),
	StudentGender VARCHAR(1),
	Grade INT);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2012/StudentBySchool_loaddata.csv'
	INTO TABLE tblStudentBySchoolSpring12
	FIELDS TERMINATED BY ','
	;


###############
# 2010-11    ##
################

-- SPRING 2011 MAP Comprehensive Data File
-- Load AssessmentResults
CREATE TABLE tblAssessmentResultsSpring11 (
	TermName VARCHAR(30), 
	StudentID INT, 
	SchoolName VARCHAR(100), 
	MeasurementScale VARCHAR(30),
	Discipline VARCHAR(11),
	GrowthMeasureYN VARCHAR(5), 
	TestType VARCHAR(20),
	TestName VARCHAR(100),
	TestStartDate VARCHAR(10),
	TestDurationInMinutes INT,
	TestRITScore SMALLINT(3),
	TestStandardError REAL(3,1),
	TestPercentile INT,
	TypicalFallToFallGrowth INT,
	TypicalSpringToSpringGrowth INT,
	TypicalFallToSpringGrowth INT,
	RITtoReadingScore INT,
	RITtoReadingMin INT,
	RITtoReadingMax INT,
	Goal1Name VARCHAR(50),
	Goal1RitScore INT,
	Goal1StdErr REAL(3,1),
	Goal1Range VARCHAR(7),
	Goal1Adjective VARCHAR(2),
	Goal2Name VARCHAR(50),
	Goal2RitScore INT,
	Goal2StdErr REAL(3,1),
	Goal2Range VARCHAR(7),
	Goal2Adjective VARCHAR(2),
	Goal3Name VARCHAR(50),
	Goal3RitScore INT,
	Goal3StdErr REAL(3,1),
	Goal3Range VARCHAR(7),
	Goal3Adjective VARCHAR(2),
	Goal4Name VARCHAR(50),
	Goal4RitScore INT,
	Goal4StdErr REAL(3,1),
	Goal4Range VARCHAR(7),
	Goal4Adjective VARCHAR(2),
	Goal5Name VARCHAR(50),
	Goal5RitScore INT,
	Goal5StdErr REAL(3,1),
	Goal5Range VARCHAR(7),
	Goal5Adjective VARCHAR(2),
	Goal6Name VARCHAR(50),
	Goal6RitScore INT,
	Goal6StdErr REAL(3,1),
	Goal6Range VARCHAR(7),
	Goal6Adjective VARCHAR(2),
	Goal7Name VARCHAR(50),
	Goal7RitScore INT,
	Goal7StdErr REAL(3,1),
	Goal7Range VARCHAR(7),
	Goal7Adjective VARCHAR(2),
	Goal8Name VARCHAR(50),
	Goal8RitScore INT,
	Goal8StdErr REAL(3,1),
	Goal8Range VARCHAR(7),
	Goal8Adjective VARCHAR(2),
	TestStartTime TIME,
	PercentCorrect INT,
	ProjectedProficiency VARCHAR(17)
	);



LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2011_Ascend/AssessmentResults_loaddata.csv'
	INTO TABLE tblAssessmentResultsSpring11
	FIELDS TERMINATED BY ','
	;



-- Load Class Assignments	

CREATE TABLE tblClassAssignmentsSpring11 (
	TermName VARCHAR(30),
	StudentID INT,
	SchoolName VARCHAR(50),
	ClassName VARCHAR(50),
	TeacherName VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2011_Ascend/ClassAssignments_loaddata.csv'
	INTO TABLE tblClassAssignmentsSpring11
	FIELDS TERMINATED BY ','
	;


-- Load StudentBySchool	
CREATE TABLE tblStudentBySchoolSpring11 (
	TermName VARCHAR(30),
	DistrictName VARCHAR(50),
	SchoolName VARCHAR(50),
	StudentLastName VARCHAR(50),
	StudentFirstName VARCHAR(50),
	StudentMI VARCHAR(2),
	StudentID INT,
	StudentDateOfBirth VARCHAR(10),
	StudentEthnicGroup VARCHAR(20),
	StudentGender VARCHAR(1),
	Grade INT);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2011_Ascend/StudentBySchool_loaddata.csv'
	INTO TABLE tblStudentBySchoolSpring11
	FIELDS TERMINATED BY ','
	;

	
#################################	
### 2009-10 Academic year data ##
#################################

-- FALL 2009 MAP Comprehensive Data File
-- Load AssessmentResults
CREATE TABLE tblAssessmentResultsFall09 (
	TermName VARCHAR(30), 
	StudentID INT, 
	SchoolName VARCHAR(100), 
	MeasurementScale VARCHAR(30),
	Discipline VARCHAR(11),
	GrowthMeasureYN VARCHAR(5), 
	TestType VARCHAR(20),
	TestName VARCHAR(100),
	TestStartDate VARCHAR(10),
	TestDurationInMinutes INT,
	TestRITScore SMALLINT(3),
	TestStandardError REAL(3,1),
	TestPercentile INT,
	TypicalFallToFallGrowth INT,
	TypicalSpringToSpringGrowth INT,
	TypicalFallToSpringGrowth INT,
	RITtoReadingScore INT,
	RITtoReadingMin INT,
	RITtoReadingMax INT,
	Goal1Name VARCHAR(50),
	Goal1RitScore INT,
	Goal1StdErr REAL(3,1),
	Goal1Range VARCHAR(7),
	Goal1Adjective VARCHAR(2),
	Goal2Name VARCHAR(50),
	Goal2RitScore INT,
	Goal2StdErr REAL(3,1),
	Goal2Range VARCHAR(7),
	Goal2Adjective VARCHAR(2),
	Goal3Name VARCHAR(50),
	Goal3RitScore INT,
	Goal3StdErr REAL(3,1),
	Goal3Range VARCHAR(7),
	Goal3Adjective VARCHAR(2),
	Goal4Name VARCHAR(50),
	Goal4RitScore INT,
	Goal4StdErr REAL(3,1),
	Goal4Range VARCHAR(7),
	Goal4Adjective VARCHAR(2),
	Goal5Name VARCHAR(50),
	Goal5RitScore INT,
	Goal5StdErr REAL(3,1),
	Goal5Range VARCHAR(7),
	Goal5Adjective VARCHAR(2),
	Goal6Name VARCHAR(50),
	Goal6RitScore INT,
	Goal6StdErr REAL(3,1),
	Goal6Range VARCHAR(7),
	Goal6Adjective VARCHAR(2),
	Goal7Name VARCHAR(50),
	Goal7RitScore INT,
	Goal7StdErr REAL(3,1),
	Goal7Range VARCHAR(7),
	Goal7Adjective VARCHAR(2),
	Goal8Name VARCHAR(50),
	Goal8RitScore INT,
	Goal8StdErr REAL(3,1),
	Goal8Range VARCHAR(7),
	Goal8Adjective VARCHAR(2),
	TestStartTime TIME,
	PercentCorrect INT,
	ProjectedProficiency VARCHAR(17)
	);
	
	

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2009_Ascend/AssessmentResults_loaddata.csv'
	INTO TABLE tblAssessmentResultsFall09
	FIELDS TERMINATED BY ','
	;



-- Load Class Assignments	
CREATE TABLE tblClassAssignmentsFall09 (
	TermName VARCHAR(30),
	StudentID INT,
	SchoolName VARCHAR(50),
	ClassName VARCHAR(50),
	TeacherName VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2009_Ascend/ClassAssignments_loaddata.csv'
	INTO TABLE tblClassAssignmentsFall09
	FIELDS TERMINATED BY ','
	;


-- Load StudentBySchool	
CREATE TABLE tblStudentBySchoolFall09 (
	TermName VARCHAR(30),
	DistrictName VARCHAR(50),
	SchoolName VARCHAR(50),
	StudentLastName VARCHAR(50),
	StudentFirstName VARCHAR(50),
	StudentMI VARCHAR(2),
	StudentID INT,
	StudentDateOfBirth VARCHAR(10),
	StudentEthnicGroup VARCHAR(20),
	StudentGender VARCHAR(1),
	Grade INT);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2009_Ascend/StudentBySchool_loaddata.csv'
	INTO TABLE tblStudentBySchoolFall09
	FIELDS TERMINATED BY ','
	;



-- SPRING 2010 MAP Comprehensive Data File
-- Load AssessmentResults
CREATE TABLE tblAssessmentResultsSpring10 (
	TermName VARCHAR(30), 
	StudentID INT, 
	SchoolName VARCHAR(100), 
	MeasurementScale VARCHAR(30),
	Discipline VARCHAR(11),
	GrowthMeasureYN VARCHAR(5), 
	TestType VARCHAR(20),
	TestName VARCHAR(100),
	TestStartDate VARCHAR(10),
	TestDurationInMinutes INT,
	TestRITScore SMALLINT(3),
	TestStandardError REAL(3,1),
	TestPercentile INT,
	TypicalFallToFallGrowth INT,
	TypicalSpringToSpringGrowth INT,
	TypicalFallToSpringGrowth INT,
	RITtoReadingScore INT,
	RITtoReadingMin INT,
	RITtoReadingMax INT,
	Goal1Name VARCHAR(50),
	Goal1RitScore INT,
	Goal1StdErr REAL(3,1),
	Goal1Range VARCHAR(7),
	Goal1Adjective VARCHAR(2),
	Goal2Name VARCHAR(50),
	Goal2RitScore INT,
	Goal2StdErr REAL(3,1),
	Goal2Range VARCHAR(7),
	Goal2Adjective VARCHAR(2),
	Goal3Name VARCHAR(50),
	Goal3RitScore INT,
	Goal3StdErr REAL(3,1),
	Goal3Range VARCHAR(7),
	Goal3Adjective VARCHAR(2),
	Goal4Name VARCHAR(50),
	Goal4RitScore INT,
	Goal4StdErr REAL(3,1),
	Goal4Range VARCHAR(7),
	Goal4Adjective VARCHAR(2),
	Goal5Name VARCHAR(50),
	Goal5RitScore INT,
	Goal5StdErr REAL(3,1),
	Goal5Range VARCHAR(7),
	Goal5Adjective VARCHAR(2),
	Goal6Name VARCHAR(50),
	Goal6RitScore INT,
	Goal6StdErr REAL(3,1),
	Goal6Range VARCHAR(7),
	Goal6Adjective VARCHAR(2),
	Goal7Name VARCHAR(50),
	Goal7RitScore INT,
	Goal7StdErr REAL(3,1),
	Goal7Range VARCHAR(7),
	Goal7Adjective VARCHAR(2),
	Goal8Name VARCHAR(50),
	Goal8RitScore INT,
	Goal8StdErr REAL(3,1),
	Goal8Range VARCHAR(7),
	Goal8Adjective VARCHAR(2),
	TestStartTime TIME,
	PercentCorrect INT,
	ProjectedProficiency VARCHAR(17)
	);



LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2010_Ascend/AssessmentResults_loaddata.csv'
	INTO TABLE tblAssessmentResultsSpring10
	FIELDS TERMINATED BY ','
	;



-- Load Class Assignments	

CREATE TABLE tblClassAssignmentsSpring10 (
	TermName VARCHAR(30),
	StudentID INT,
	SchoolName VARCHAR(50),
	ClassName VARCHAR(50),
	TeacherName VARCHAR(50)
	);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2010_Ascend/ClassAssignments_loaddata.csv'
	INTO TABLE tblClassAssignmentsSpring10
	FIELDS TERMINATED BY ','
	;


-- Load StudentBySchool	
CREATE TABLE tblStudentBySchoolSpring10 (
	TermName VARCHAR(30),
	DistrictName VARCHAR(50),
	SchoolName VARCHAR(50),
	StudentLastName VARCHAR(50),
	StudentFirstName VARCHAR(50),
	StudentMI VARCHAR(2),
	StudentID INT,
	StudentDateOfBirth VARCHAR(10),
	StudentEthnicGroup VARCHAR(20),
	StudentGender VARCHAR(1),
	Grade INT);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Spring_2010_Ascend/StudentBySchool_loaddata.csv'
	INTO TABLE tblStudentBySchoolSpring10
	FIELDS TERMINATED BY ','
	;

--Update all of Kinder = Grade 13 to Kinder = Grade 0 (this change in 2011-12 school year/testing cycle
UPDATE `tblStudentBySchoolFall08`  set Grade=0 where Grade=13;
UPDATE `tblStudentBySchoolSpring09`  set Grade=0 where Grade=13;

UPDATE `tblStudentBySchoolFall09`  set Grade=0 where Grade=13;
UPDATE `tblStudentBySchoolSpring10`  set Grade=0 where Grade=13;

UPDATE `tblStudentBySchoolFall10`  set Grade=0 where Grade=13;
UPDATE `tblStudentBySchoolSpring11`  set Grade=0 where Grade=13;

UPDATE `tblStudentBySchoolFall11`  set Grade=0 where Grade=13;
UPDATE `tblStudentBySchoolWinter12`  set Grade=0 where Grade=13;
UPDATE `tblStudentBySchoolSpring12`  set Grade=0 where Grade=13;

########################
## 2010-11 Special Ed ##
########################
CREATE TABLE tblSpecialEd1011 (
	StudentID INT,
	Sped INT);

LOAD DATA LOCAL INFILE '/Users/christopher.haid/Documents/Dropbox/Consulting/KIPP Ascend/MAP Data/Ascend Data/Sped201011.csv'
	INTO TABLE tblSpecialEd1011
	FIELDS TERMINATED BY ','
	;
	

########################
## 2011-12 Special Ed ##
########################
CREATE TABLE tblSpecialEd1112 (
	StudentID INT,
	Sped INT);

LOAD DATA LOCAL INFILE '/Users/christopher.haid/Documents/Dropbox/Consulting/KIPP Ascend/MAP Data/Ascend Data/Sped201112.csv'
	INTO TABLE tblSpecialEd1112
	FIELDS TERMINATED BY ','
	LINES TERMINATED BY '\r'
	;


##############################################	
## Add 2011 Growth Norms Look-up table 		##
##############################################


drop table if exists `tblNorms2011_Growth`, `temp_loading`;	
CREATE TABLE  temp_loading (
	Subj REAL,
	StartGrade  INT,
	StartRIT  INT,
	T41	REAL,
	T42 REAL,
	T44 REAL,
	T22 REAL,
	T12	REAL, 
	R41	REAL,
	R42 REAL,
	R44 REAL, 
	R22 REAL,
	R12	REAL, 
	S41 REAL,
	S42	REAL, 
	S44	REAL, 
	S22 REAL,
	S12 REAL
	);

LOAD DATA LOCAL INFILE '/Users/chaid/Dropbox/Consulting/KIPP Ascend/Testing Analysis/MAP/Results/NWEA Comprehensive Data Files/Norm_Table_Download_2011_v1212/Norms2011_Growth.csv'
	INTO TABLE temp_loading
	FIELDS TERMINATED BY ','
	LINES TERMINATED BY '\r'
	IGNORE 1 LINES
	;

#######################################
## NWEAs 2011 Growth Norms for MAP  ##
#######################################

CREATE TABLE tblNorms2011_Growth
SELECT 	*,
		CASE
			WHEN Subj = 1 THEN 'Mathematics'
			WHEN Subj = 2 THEN 'Reading'
			WHEN Subj = 3 THEN 'Language Usage'
			WHEN Subj = 4 THEN 'General Science'
			WHEN Subj = 5 THEN 'Concepts and Processes'
		END AS MeasurementScale
FROM 	temp_loading
;					

drop table if exists `temp_loading`;	


# We create a view to create a column that replaces Grade 13 (Kinder before Fall 2012) to Grade 0 (Kinder after Fall 2012)

CREATE VIEW viewNorms2011_Growth_Kinder_0
AS
SELECT *,
		Replace(StartGrade, 13,0) AS StartGrade2
FROM `tblNorms2011_Growth`;

##############################################	
## Add 9 Week adjusted Growth Target Tables ##
##############################################


CREATE TABLE tbl9WeekAdjGrowthTargets (
	Subject_Grade_RIT_ID  VARCHAR(20),
	Subj  VARCHAR(20),
	Grade  INT,
	RIT  INT,
	Est  REAL(3,1),
	SD  REAL,
	Rpt  INT);

LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Growth_Goal_Adj_9_Weeks.csv'
	INTO TABLE tbl9WeekAdjGrowthTargets
	FIELDS TERMINATED BY ','
	LINES TERMINATED BY '\r'
	;	
	
#########################################################	
## LOAD 2010-11 Preliminary ISAT (per student) results ##
#########################################################

CREATE TABLE tblISATPrelim1011 (
	Grade  INT,
	Room  INT,
	StudentID  INT,
	StudentFirstName  VARCHAR(50),	
	StudentLastName	 VARCHAR(50),
	ELLBilPro  INT,
	ReadingPerfLevel  VARCHAR(9),
	ReadingStdScore  INT,
	ReadingPercentile  INT,
	ReadingStanine  INT,	
	ReadingExtRespPoints  INT,	
	ReadingPctCorrectVocabDevelopment  REAL(4,0),
	ReadingPctCorrectReadingStrategies  REAL(4,0),
	ReadingPctCorrectReadingComprehension  REAL(4,0),
	ReadingPctCorrectLit  REAL(4,0),	
	MathPerfLevel  VARCHAR(9),	
	MathStdScore  INT,
	MathPercentile  INT,
	MathStanine  INT,
	MathExtRespPointsMathKnowl  INT,
	MathExtRespPointsStratKnowl  INT,
	MathExtRespPointsExplanation  INT,
	MathShortResp1  INT,	
	MathShortResp2  INT,	
	MathPctCorrectNumberSense  REAL(4,0),	
	MathPctCorrectMeasurement  REAL(4,0),	
	MathPctCorrectAlgebra  REAL(4,0),
	MathPctCorrectGeometry  REAL(4,0),
	MathPctCorrectDataAnalysisStats  REAL(4,0),
	SciencePerfLevel  VARCHAR(9),
	ScienceStdScore  INT,
	SciencePercentile  INT,
	ScienceStanine  INT,
	SciencePctCorrectSciInquiryandTechDesign  REAL(4,0),
	SciencePctCorrectLifeandEnviromentalScience  REAL(4,0),
	SciencePctCorrectMatterEnergyandForce  REAL(4,0),
	SciencePctCorrectEarthandSpaceScience  REAL(4,0),
	SciencePctCorrectSafetySciSocietyandMeasurement  REAL(4,0),
	WritingPerfLevel  VARCHAR(9),
	WritingStdScore INT
	);	
	
LOAD DATA LOCAL INFILE '/Users/christopher.haid/Documents/Dropbox/Consulting/KIPP Ascend/MAP Data/ISAT Results/PreliminaryISATResults201011.csv'
	INTO TABLE tblISATPrelim1011
	FIELDS TERMINATED BY ','
	LINES TERMINATED BY '\r'
	;

#########################################################	
## LOAD 2011-12 ISAT (per student) results             ##
#########################################################
DROP TABLE tblISATPrelim1112;
CREATE TABLE tblISATPrelim1112 (
	Grade  INT,
	Room  INT,
	StudentID  INT,
	StudentFirstName  VARCHAR(50),	
	StudentLastName	 VARCHAR(50),
	ELLBilPro  INT,
	ReadingPerfLevel  VARCHAR(9),
	ReadingStdScore  INT,
	ReadingPercentile  INT,
	ReadingStanine  INT,	
	ReadingExtRespPoints  INT,	
	ReadingPctCorrectVocabDevelopment  REAL(4,0),
	ReadingPctCorrectReadingStrategies  REAL(4,0),
	ReadingPctCorrectReadingComprehension  REAL(4,0),
	ReadingPctCorrectLit  REAL(4,0),	
	MathPerfLevel  VARCHAR(9),	
	MathStdScore  INT,
	MathPercentile  INT,
	MathStanine  INT,
	MathExtRespPointsMathKnowl  INT,
	MathExtRespPointsStratKnowl  INT,
	MathExtRespPointsExplanation  INT,
	MathShortResp1  INT,	
	MathShortResp2  INT,	
	MathPctCorrectNumberSense  REAL(4,0),	
	MathPctCorrectMeasurement  REAL(4,0),	
	MathPctCorrectAlgebra  REAL(4,0),
	MathPctCorrectGeometry  REAL(4,0),
	MathPctCorrectDataAnalysisStats  REAL(4,0),
	SciencePerfLevel  VARCHAR(9),
	ScienceStdScore  INT,
	SciencePercentile  INT,
	ScienceStanine  INT,
	SciencePctCorrectSciInquiryandTechDesign  REAL(4,0),
	SciencePctCorrectLifeandEnviromentalScience  REAL(4,0),
	SciencePctCorrectMatterEnergyandForce  REAL(4,0),
	SciencePctCorrectEarthandSpaceScience  REAL(4,0),
	SciencePctCorrectSafetySciSocietyandMeasurement  REAL(4,0),
	WritingPerfLevel  VARCHAR(9),
	WritingStdScore INT
	);	
	
LOAD DATA LOCAL INFILE '/Users/christopher.haid/Documents/Dropbox/Consulting/KIPP Ascend/MAP Data/ISAT Results/PreliminaryISATResults201112.csv'
	INTO TABLE tblISATPrelim1112
	FIELDS TERMINATED BY ','
	LINES TERMINATED BY '\r'
	;	
	
