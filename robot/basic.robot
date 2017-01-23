*** Settings ***
Library  Process

*** Test Cases ***
Simple1 Test Passes
    Run Test  simple1r
    Result Should Contain  Passed :\ \ 1
    Result Should Match    *Simple*PASS*

Fail1 Test Fails
		Run Test  fail1r
    Result Should Contain  Failed :\ \ 1
    Result Should Match    *Fail*DOES NOT WORK*FAIL*

*** Keywords ***
Run Test
    [Arguments]  ${testname}
    ${result}=  Run Process  test_sources/${testname}
    Set Test Variable  ${TEST_OUTPUT}  ${result.stdout}

Result Should Contain
    [Arguments]  ${testresult}
    Should Contain  ${TEST_OUTPUT}  ${testresult}

Result Should Match
    [Arguments]  ${testresult}
    Should Match  ${TEST_OUTPUT}  ${testresult}
