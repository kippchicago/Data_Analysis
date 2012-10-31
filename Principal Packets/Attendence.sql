SELECT att.schoolid,
s.lastfirst, 
att.Att_Date,
attc.Att_Code,
attc.Description,
attc.Presence_Status_CD,
s.Enroll_Status
FROM Attendance att
  INNER JOIN Attendance_Code attc ON att.Attendance_CodeID = attc.ID
 LEFT JOIN students s ON att.StudentID = s.id
WHERE att.Att_Mode_Code = 'ATT_ModeDaily'
  AND att.Att_Date = '08-OCT-2012'
  AND s.Enroll_Status=0  
  AND att.schoolid=400146
  AND attc.att_code = 'A'
ORDER BY s.lastfirst;

--Selects students with enexcused absences by school, by day (or by date range)
SELECT att.schoolid,
s.lastfirst, 
s.GRADE_LEVEL,
att.Att_Date,
attc.Att_Code,
attc.Description,
attc.Presence_Status_CD,
s.Enroll_Status,
s.ExitDate
FROM Attendance att
  INNER JOIN Attendance_Code attc ON att.Attendance_CodeID = attc.ID
 LEFT JOIN students s ON att.StudentID = s.id
WHERE 
att.Att_Mode_Code = 'ATT_ModeDaily'
  AND att.Att_Date >= '27-AUG-2012'
  AND att.Att_Date <= '31-AUG-2012'
 -- AND s.Enroll_Status=0  
AND att.schoolid=7810
AND (attc.att_code = 'A' OR attc.att_code = 'S')
ORDER BY s.grade_level, att.Att_Date, s.lastfirst, att_code;

--Counts by Grade
Select Grade_Level, count(*) as Number_Absent
FROM (
SELECT att.schoolid,
s.lastfirst, 
s.GRADE_LEVEL,
att.Att_Date,
attc.Att_Code,
attc.Description,
attc.Presence_Status_CD,
s.Enroll_Status,
s.ExitDate
FROM Attendance att
  INNER JOIN Attendance_Code attc ON att.Attendance_CodeID = attc.ID
 LEFT JOIN students s ON att.StudentID = s.id
WHERE 
att.Att_Mode_Code = 'ATT_ModeDaily'
  AND att.Att_Date >= '27-AUG-2012'
  AND att.Att_Date <= '30-AUG-2012'
 -- AND s.Enroll_Status=0  
AND att.schoolid=7810
AND (attc.att_code = 'A' OR attc.att_code = 'S')
)
GROUP BY grade_level
ORDER BY Grade_level;



--Selects enrollment by school, by grade, by date (or date range by adding an extra AND to the WHERE clause).
SELECT SchoolID, grade_level, Count(studentid) as Enrollment
from PS_Membership_Defaults
Where calendardate = '10-OCT-12'
Group By schoolid, grade_level
ORDER BY schoolid, grade_level;

--Selects ALL students enrolled on a given date as well as their entry and exit data.
SELECT 
	m.SchoolID, 	
	m.studentid,
	s.first_name,
  	s.middle_name,
     s.last_name,
	m.grade_level, 
 	s.ethnicity AS Race_ID,
  	s.gender,
  	s.dob,
	s.entrydate,
	s.SCHOOLENTRYDATE,
	s.DISTRICTENTRYDATE,
	s.EXITDATE,
	s.exitcode,
	s.EXITCOMMENT
FROM PS_Membership_Defaults m
	JOIN STUDENTS s
	ON m.studentid = s.id
WHERE m.calendardate = '3-OCT-11'
ORDER BY schoolid, grade_level

--Calculate counts and percentages by exit reason (this is for pulling data for KIPP 
--Foundation's HSR data request
SELECT 
  ExitCode, 
  COUNT(*) as ExitCount, 
  count(*)/sum( count(*)) over()  as Pct
FROM (
SELECT 
	m.SchoolID, 	
	m.studentid,
	s.first_name,
  	s.middle_name,
     s.last_name,
	m.grade_level, 
 	s.ethnicity AS Race_ID,
  	s.gender,
  	s.dob,
	s.entrydate,
	s.SCHOOLENTRYDATE,
	s.DISTRICTENTRYDATE,
	s.EXITDATE,
	s.exitcode,
	s.EXITCOMMENT
FROM PS_Membership_Defaults m
	JOIN STUDENTS s
	ON m.studentid = s.id
WHERE m.calendardate = '3-OCT-11'
ORDER BY schoolid, grade_level
)
GROUP BY exitcode
ORDER BY exitcode;
;

--Get each enrolled student and their attendence status by date for a given date range
SELECT 
	m.schoolid, 
	m.grade_level,
	m.calendardate, 
	m.studentid, 
	m.Enrolled,
	a.Att_Code,
	a.Description as AttDescr,
	CASE 
		WHEN	a.Presence_Status_CD = 'Absent' THEN 1 
		ELSE 0 
	END as boolAbsent
FROM (
	SELECT
		SchoolID,  
		grade_level, 
		calendardate, studentid, 
		1 as Enrolled  
	FROM PS_Membership_Defaults 
	Where 	calendardate >= '11-OCT-12'
		AND  calendardate <= '16-OCT-12'
LEFT JOIN (
) m
	SELECT 
		att.schoolid,
		s.lastfirst, 
		s.id as StudentID,
		s.GRADE_LEVEL,
		att.Att_Date,
		attc.Att_Code,
		attc.Description,
		attc.Presence_Status_CD
	FROM Attendance att
  	INNER JOIN Attendance_Code attc ON att.Attendance_CodeID = attc.ID
 	LEFT JOIN students s ON att.StudentID = s.id
	WHERE 
		att.Att_Mode_Code = 'ATT_ModeDaily'
  		AND att.Att_Date >= '11-OCT-12'
  		AND att.Att_Date <= '16-OCT-12'
		AND (attc.att_code = 'A' OR attc.att_code = 'S')
) a
ON m.STUDENTID = a.studentid AND m.calendardate =a.Att_Date AND m.schoolID = a.schoolid
ORDER BY schoolid, grade_level, calendardate;


