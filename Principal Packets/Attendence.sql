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
  AND att.Att_Date = '10-OCT-2012'
  AND s.Enroll_Status=0  
AND att.schoolid=400146
AND attc.att_code = 'A'
ORDER BY s.lastfirst, att_code;

--Selects enrollment by school, by grade, by date (or date range by adding an extra AND to the WHERE clause).
SELECT SchoolID, grade_level, Count(studentid) as Enrollment
from PS_Membership_Defaults
Where calendardate = '10-OCT-12'
Group By schoolid, grade_level
ORDER BY schoolid, grade_level;

