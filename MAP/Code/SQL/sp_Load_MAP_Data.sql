DELIMITER ;;
DROP PROCEDURE IF EXISTS spCreateMAPCDFTables;;
CREATE PROCEDURE `spCreateMAPCDFTables`(IN season VARCHAR(50))
BEGIN

--Drop and Add Assessment  Results
SET @drop_assign = 	CONCAT("
DROP TABLE IF EXISTS tblAssessmentResults", season,";	
"
);



SET @create_asses = 	CONCAT("
CREATE TABLE tblAssessmentResults", season," (
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
"
);


PREPARE	sql_ex FROM @drop_asses;
EXECUTE	sql_ex;

PREPARE	sql_ex FROM @create_asses;
EXECUTE	sql_ex;



-- Drop and Add Class Assignements
SET @drop_class = 	CONCAT("
DROP TABLE IF EXISTS tblClassAssignments", season,";
"
);



SET @create_class = 	CONCAT("
CREATE TABLE tblClassAssignments", season," (
	TermName VARCHAR(30),
	StudentID INT,
	SchoolName VARCHAR(50),
	ClassName VARCHAR(50),
	TeacherName VARCHAR(50)
	);
"
);


PREPARE	sql_ex FROM @drop_class;
EXECUTE	sql_ex;

PREPARE	sql_ex FROM @create_class;
EXECUTE	sql_ex;


-- Drop and Add Students by School
SET @drop_school = 	CONCAT("
DROP TABLE IF EXISTS tblStudentBySchool", season,";
"
);



SET @create_school = 	CONCAT("
CREATE TABLE tblStudentBySchool", season," (
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
"
);


PREPARE	sql_ex FROM @drop_school;
EXECUTE	sql_ex;

PREPARE	sql_ex FROM @create_school;
EXECUTE	sql_ex;

-- Drop and Add Program Assignements
SET @drop_assign = 	CONCAT("
DROP TABLE IF EXISTS tblProgramAssignments", season,";
"
);



SET @create_assign = 	CONCAT("
CREATE TABLE tblProgramAssignments", season, "(
	TermName VARCHAR(30),
	StudentID INT,
	Program VARCHAR(50)
	);
"
);


PREPARE	sql_ex FROM @drop_assign;
EXECUTE	sql_ex;

PREPARE	sql_ex FROM @create_assign;
EXECUTE	sql_ex;


-- Now load the data

-- Assessment
SET @load_assess = 	CONCAT("
LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2013/AssessmentResults_loaddata.csv'
	INTO TABLE tblAssessmentResults", season,"
	FIELDS TERMINATED BY ','
	;
"
);

PREPARE	sql_ex FROM @load_assess;
EXECUTE	sql_ex;

-- Class Assignments
SET @load_assign = 	CONCAT("
LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2013/ClassAssignments_loaddata.csv'
	INTO TABLE tblClassAssignments", season,"
	FIELDS TERMINATED BY ','
	;
"
);

PREPARE	sql_ex FROM @load_assign;
EXECUTE	sql_ex;


-- Student By School
SET @load_student = 	CONCAT("
LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2013/StudentsBySchool_loaddata.csv'
	INTO TABLE tblStudentBySchool", season,"
	FIELDS TERMINATED BY ','
	;
"
);

PREPARE	sql_ex FROM @load_student;
EXECUTE	sql_ex;

-- Program Assignements 
LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2013/StudentsBySchool_loaddata.csv'
	INTO TABLE tblStudentBySchool", season,"
	FIELDS TERMINATED BY ','
	;
SET @load_student = 	CONCAT("
LOAD DATA LOCAL INFILE '~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Input/NWEA Comprehensive Data Files/MAP_Fall_2013/ProgramAssignments_loaddata.csv'
	INTO TABLE tblProgramAssignments", season,"
	FIELDS TERMINATED BY ','
	;
"
);

PREPARE	sql_ex FROM @load_student;
EXECUTE	sql_ex;

END;;
DELIMITER ;
