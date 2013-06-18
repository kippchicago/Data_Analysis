# Preprossing.minging script for recreateing school level "arrow" charts

MAP.Network.School.Level.data <- copy(MAP.Network.School.Level.data[Growth_Academic_Year==2012])


MAP.Network.School.Level.data [,diff:=Spring.RIT - Fall.RIT]
MAP.Network.School.Level.data [,posdiff:=diff>0]