/*����������*/
SELECT si.id, d.name as "���� ������", ti.name as "�����", g.name as "������", c.name as "�������� ����������", p.name as "����� ���������", te.name as "�������������" FROM schedule_items si
 INNER JOIN courses c ON si.course_id = c.id
 INNER JOIN teachers te ON si.teacher_id = te.id
 INNER JOIN places p ON si.place_id = p.id
 INNER JOIN times ti ON si.time_id = ti.id
 INNER JOIN days d ON si.day_id = d.id
 INNER JOIN groups g ON si.group_id = g.id
 ORDER BY d.id, ti.id, g.id
;
/*���������� ���������*/
SELECT g.id, g.name, COUNT(s.id) as student_count FROM groups g
 INNER JOIN students s ON s.group_id = g.id 
 GROUP BY g.id, g.name
 /*HAVING COUNT(s.id) > 5*/ /*�� ���������� �� student_count*/
 ORDER BY student_count DESC
;
/*���������� � �������*/
SELECT c.name, LIST(m.mark), AVG(m.mark) FROM marks m
 INNER JOIN students s ON s.id = m.student_id
 INNER JOIN courses c ON c.id = m.course_id 
 GROUP BY c.name
;