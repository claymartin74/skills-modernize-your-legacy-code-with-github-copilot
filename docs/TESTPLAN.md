# Student Account Management System - Test Plan

## Overview

This test plan covers all business logic and functionality of the Student Account Management System. It is designed to validate the current COBOL implementation and will serve as the basis for unit and integration tests in the Node.js migration.

## Test Environment

- **Current System:** COBOL application compiled with GnuCOBOL
- **Target System:** Node.js application
- **Initial Balance:** $1,000.00 (default)
- **Balance Format:** 6 digits + 2 decimal places (max: $999,999.99)

---

## Test Cases

### 1. Menu Navigation Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-MENU-001 | Display main menu on application start | Application compiled successfully | 1. Launch the application | Menu displays with options 1-4: View Balance, Credit Account, Debit Account, Exit | | | |
| TC-MENU-002 | Valid menu option selection (1-4) | Application is running, menu is displayed | 1. Enter option "1" | Application processes View Balance operation | | | |
| TC-MENU-003 | Invalid menu option - letter input | Application is running, menu is displayed | 1. Enter "a" | Display "Invalid choice, please select 1-4." and return to menu | | | |
| TC-MENU-004 | Invalid menu option - number out of range | Application is running, menu is displayed | 1. Enter "5" | Display "Invalid choice, please select 1-4." and return to menu | | | |
| TC-MENU-005 | Invalid menu option - negative number | Application is running, menu is displayed | 1. Enter "-1" | Display "Invalid choice, please select 1-4." and return to menu | | | |
| TC-MENU-006 | Exit application | Application is running, menu is displayed | 1. Enter "4" | Display "Exiting the program. Goodbye!" and terminate | | | |
| TC-MENU-007 | Menu loop continues after operation | Application is running | 1. Enter "1" to view balance 2. Observe after operation completes | Menu is displayed again after operation | | | |

---

### 2. View Balance Tests (Option 1 - TOTAL Operation)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-BAL-001 | View initial default balance | Fresh application start, no prior transactions | 1. Select option "1" | Display "Current balance: 001000.00" | | | Default balance is $1,000.00 |
| TC-BAL-002 | View balance after credit | Balance has been credited | 1. Credit $500.00 2. Select option "1" | Display "Current balance: 001500.00" | | | |
| TC-BAL-003 | View balance after debit | Balance has been debited | 1. Debit $200.00 2. Select option "1" | Display "Current balance: 000800.00" | | | |
| TC-BAL-004 | View zero balance | Balance is exactly $0.00 | 1. Debit entire balance 2. Select option "1" | Display "Current balance: 000000.00" | | | |
| TC-BAL-005 | View balance multiple times consecutively | Application is running | 1. Select option "1" 2. Select option "1" again | Same balance displayed both times | | | Balance should not change on view |

---

### 3. Credit Account Tests (Option 2 - CREDIT Operation)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-CRD-001 | Credit with whole number amount | Initial balance $1,000.00 | 1. Select option "2" 2. Enter "500" | Display "Amount credited. New balance: 001500.00" | | | |
| TC-CRD-002 | Credit with decimal amount | Initial balance $1,000.00 | 1. Select option "2" 2. Enter "250.50" | Display "Amount credited. New balance: 001250.50" | | | |
| TC-CRD-003 | Credit with zero amount | Initial balance $1,000.00 | 1. Select option "2" 2. Enter "0" | Display "Amount credited. New balance: 001000.00" | | | Balance unchanged |
| TC-CRD-004 | Credit small amount (cents only) | Initial balance $1,000.00 | 1. Select option "2" 2. Enter "0.01" | Display "Amount credited. New balance: 001000.01" | | | Minimum credit amount |
| TC-CRD-005 | Credit large amount | Initial balance $1,000.00 | 1. Select option "2" 2. Enter "998999.99" | Display "Amount credited. New balance: 999999.99" | | | Maximum balance reached |
| TC-CRD-006 | Multiple consecutive credits | Initial balance $1,000.00 | 1. Credit $100.00 2. Credit $200.00 3. Credit $300.00 | Final balance should be $1,600.00 | | | Verify cumulative credits |
| TC-CRD-007 | Credit after debit | Balance reduced by debit | 1. Debit $500.00 2. Credit $250.00 | Balance should be $750.00 | | | |
| TC-CRD-008 | Credit to zero balance | Balance is $0.00 | 1. Debit entire balance 2. Credit $100.00 | Display "Amount credited. New balance: 000100.00" | | | |

---

### 4. Debit Account Tests (Option 3 - DEBIT Operation)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-DEB-001 | Debit with whole number amount (sufficient funds) | Initial balance $1,000.00 | 1. Select option "3" 2. Enter "500" | Display "Amount debited. New balance: 000500.00" | | | |
| TC-DEB-002 | Debit with decimal amount (sufficient funds) | Initial balance $1,000.00 | 1. Select option "3" 2. Enter "250.50" | Display "Amount debited. New balance: 000749.50" | | | |
| TC-DEB-003 | Debit exact balance amount | Initial balance $1,000.00 | 1. Select option "3" 2. Enter "1000" | Display "Amount debited. New balance: 000000.00" | | | Balance becomes zero |
| TC-DEB-004 | Debit with zero amount | Initial balance $1,000.00 | 1. Select option "3" 2. Enter "0" | Display "Amount debited. New balance: 001000.00" | | | Balance unchanged |
| TC-DEB-005 | Debit small amount (cents only) | Initial balance $1,000.00 | 1. Select option "3" 2. Enter "0.01" | Display "Amount debited. New balance: 000999.99" | | | Minimum debit amount |
| TC-DEB-006 | Debit exceeding balance (insufficient funds) | Initial balance $1,000.00 | 1. Select option "3" 2. Enter "1500" | Display "Insufficient funds for this debit." Balance remains $1,000.00 | | | **Critical business rule** |
| TC-DEB-007 | Debit by $0.01 more than balance | Initial balance $1,000.00 | 1. Select option "3" 2. Enter "1000.01" | Display "Insufficient funds for this debit." Balance remains $1,000.00 | | | Edge case for insufficient funds |
| TC-DEB-008 | Multiple consecutive debits (sufficient funds) | Initial balance $1,000.00 | 1. Debit $100.00 2. Debit $200.00 3. Debit $300.00 | Final balance should be $400.00 | | | Verify cumulative debits |
| TC-DEB-009 | Debit after credit | Balance increased by credit | 1. Credit $500.00 2. Debit $750.00 | Balance should be $750.00 | | | |
| TC-DEB-010 | Debit from zero balance | Balance is $0.00 | 1. Debit entire balance 2. Attempt debit $1.00 | Display "Insufficient funds for this debit." Balance remains $0.00 | | | |
| TC-DEB-011 | Consecutive debit attempts after insufficient funds | Initial balance $1,000.00 | 1. Attempt debit $2000.00 (fails) 2. Debit $500.00 | First fails with "Insufficient funds", second succeeds with balance $500.00 | | | System recovers after failed transaction |

---

### 5. Data Persistence Tests (DataProgram)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-DAT-001 | Balance persists across operations | Application running | 1. Credit $500.00 2. View balance 3. Debit $200.00 4. View balance | Balance correctly reflects all operations ($1,300.00) | | | |
| TC-DAT-002 | Balance resets on application restart | Previous session had modified balance | 1. Exit application 2. Restart application 3. View balance | Balance returns to default $1,000.00 | | | In-memory storage limitation |
| TC-DAT-003 | Read operation returns current balance | Balance modified | 1. Credit $100.00 2. Internal READ operation | Returns updated balance value | | | Unit test for DataProgram |
| TC-DAT-004 | Write operation updates stored balance | Application running | 1. Internal WRITE with new balance | STORAGE-BALANCE updated to new value | | | Unit test for DataProgram |

---

### 6. Boundary Value Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-BND-001 | Maximum balance value | Balance near maximum | 1. Set balance to $999,999.98 2. Credit $0.01 | Balance becomes $999,999.99 | | | Maximum representable value |
| TC-BND-002 | Minimum balance value | Balance is $0.01 | 1. Debit $0.01 | Balance becomes $0.00 | | | Minimum balance is zero |
| TC-BND-003 | Maximum transaction amount | Initial balance $1,000.00 | 1. Credit $999,999.99 | System handles large transaction | | | Test numeric overflow handling |
| TC-BND-004 | Precision with decimal values | Initial balance $1,000.00 | 1. Credit $0.01 2. Credit $0.01 3. Credit $0.01 | Balance should be $1,000.03 | | | Verify decimal precision |

---

### 7. Integration Tests (Program Communication)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-INT-001 | MainProgram calls Operations correctly | Application compiled | 1. Select menu option 2. Verify Operations receives correct operation code | Operations program receives 'TOTAL', 'CREDIT', or 'DEBIT' | | | |
| TC-INT-002 | Operations calls DataProgram for READ | Operations processing | 1. Trigger view balance | DataProgram receives 'READ' and returns current balance | | | |
| TC-INT-003 | Operations calls DataProgram for WRITE | Operations processing credit/debit | 1. Complete credit operation | DataProgram receives 'WRITE' and updates stored balance | | | |
| TC-INT-004 | Full transaction flow - Credit | Application running | 1. Select Credit 2. Enter amount 3. Verify all programs interact correctly | MainProgram → Operations → DataProgram (READ) → Operations → DataProgram (WRITE) → Display | | | End-to-end flow |
| TC-INT-005 | Full transaction flow - Debit (success) | Sufficient balance | 1. Select Debit 2. Enter valid amount | Complete flow with balance update | | | |
| TC-INT-006 | Full transaction flow - Debit (failure) | Insufficient balance | 1. Select Debit 2. Enter amount exceeding balance | Flow stops at validation, no WRITE operation | | | |

---

### 8. Error Handling Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-ERR-001 | Invalid menu input recovery | Application running | 1. Enter invalid option 2. Enter valid option | System recovers and processes valid option | | | |
| TC-ERR-002 | Insufficient funds recovery | Failed debit attempt | 1. Attempt debit exceeding balance 2. Perform valid operation | System continues normally after failed debit | | | |
| TC-ERR-003 | Non-numeric amount input | Credit/Debit prompt | 1. Enter "abc" as amount | System should handle gracefully | | | Behavior may vary |

---

## Test Summary

| Category | Total Tests | Passed | Failed | Blocked |
|----------|-------------|--------|--------|---------|
| Menu Navigation | 7 | | | |
| View Balance | 5 | | | |
| Credit Account | 8 | | | |
| Debit Account | 11 | | | |
| Data Persistence | 4 | | | |
| Boundary Value | 4 | | | |
| Integration | 6 | | | |
| Error Handling | 3 | | | |
| **TOTAL** | **48** | | | |

---

## Notes for Node.js Migration

### Unit Test Considerations

1. **DataProgram Functions** → Create separate module with `read()` and `write()` functions
2. **Operations Functions** → Create module with `viewBalance()`, `credit(amount)`, `debit(amount)` functions
3. **Validation Logic** → Extract insufficient funds check into testable function

### Integration Test Considerations

1. Test API endpoints if building REST API
2. Test database operations if adding persistence
3. Test complete transaction flows end-to-end

### Suggested Test Framework

- **Unit Tests:** Jest or Mocha with Chai
- **Integration Tests:** Supertest for API testing
- **Coverage:** Istanbul/nyc for code coverage

---

## Approval

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Business Stakeholder | | | |
| QA Lead | | | |
| Development Lead | | | |
