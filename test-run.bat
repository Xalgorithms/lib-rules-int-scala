@ECHO OFF

:: Provide rule folder as windows path like\this

set rule-folder-ms=%1
set rule-folder-unix=%rule-folder-ms:\=/%
echo Running on %rule-folder-ms%

call bundle exec ruby compile.rb %rule-folder%
if errorlevel 1 (
    echo Compile failure.
    exit /b 1
)

:: Interpret compiled rule
call sbt "runMain org.xalgorithms.rules.Runner %rule-folder%"

:: Remove compiled rules and tables
call del %rule-folder-ms%\*.rule.json
call del %rule-folder-ms%\*.table.json