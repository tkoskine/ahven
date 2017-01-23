*** Settings ***
Library  Process

*** Test Cases ***
Simple1 Test Passes
    Run Test  simple1
    Result Should Contain  Passed :\ \ 1
    Result Should Match    *Simple*PASS*

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
