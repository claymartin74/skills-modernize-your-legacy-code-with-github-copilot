/**
 * Student Account Management System - Unit Tests
 * 
 * These tests mirror the test scenarios defined in the COBOL test plan (TESTPLAN.md)
 * to ensure the Node.js implementation maintains the same business logic and behavior.
 */

const { DataProgram, Operations, MainProgram } = require('./index');

// Reset balance before each test to ensure test isolation
beforeEach(() => {
    DataProgram.reset();
});

// ============================================================================
// 2. View Balance Tests (Option 1 - TOTAL Operation)
// ============================================================================

describe('View Balance Tests (TOTAL Operation)', () => {
    // TC-BAL-001: View initial default balance
    test('TC-BAL-001: View initial default balance', () => {
        const result = Operations.execute('TOTAL');
        expect(result.success).toBe(true);
        expect(result.balance).toBe(1000.00);
        expect(result.message).toContain('1000.00');
    });

    // TC-BAL-002: View balance after credit
    test('TC-BAL-002: View balance after credit', () => {
        Operations.execute('CREDIT', 500.00);
        const result = Operations.execute('TOTAL');
        expect(result.success).toBe(true);
        expect(result.balance).toBe(1500.00);
    });

    // TC-BAL-003: View balance after debit
    test('TC-BAL-003: View balance after debit', () => {
        Operations.execute('DEBIT', 200.00);
        const result = Operations.execute('TOTAL');
        expect(result.success).toBe(true);
        expect(result.balance).toBe(800.00);
    });

    // TC-BAL-004: View zero balance
    test('TC-BAL-004: View zero balance', () => {
        Operations.execute('DEBIT', 1000.00);
        const result = Operations.execute('TOTAL');
        expect(result.success).toBe(true);
        expect(result.balance).toBe(0.00);
        expect(result.message).toContain('0.00');
    });

    // TC-BAL-005: View balance multiple times consecutively
    test('TC-BAL-005: View balance multiple times consecutively', () => {
        const result1 = Operations.execute('TOTAL');
        const result2 = Operations.execute('TOTAL');
        expect(result1.balance).toBe(result2.balance);
        expect(result1.balance).toBe(1000.00);
    });
});

// ============================================================================
// 3. Credit Account Tests (Option 2 - CREDIT Operation)
// ============================================================================

describe('Credit Account Tests (CREDIT Operation)', () => {
    // TC-CRD-001: Credit with whole number amount
    test('TC-CRD-001: Credit with whole number amount', () => {
        const result = Operations.execute('CREDIT', 500);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(1500.00);
        expect(result.message).toContain('Amount credited');
        expect(result.message).toContain('1500.00');
    });

    // TC-CRD-002: Credit with decimal amount
    test('TC-CRD-002: Credit with decimal amount', () => {
        const result = Operations.execute('CREDIT', 250.50);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(1250.50);
        expect(result.message).toContain('1250.50');
    });

    // TC-CRD-003: Credit with zero amount
    test('TC-CRD-003: Credit with zero amount', () => {
        const result = Operations.execute('CREDIT', 0);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(1000.00);
    });

    // TC-CRD-004: Credit small amount (cents only)
    test('TC-CRD-004: Credit small amount (cents only)', () => {
        const result = Operations.execute('CREDIT', 0.01);
        expect(result.success).toBe(true);
        expect(result.balance).toBeCloseTo(1000.01, 2);
    });

    // TC-CRD-005: Credit large amount
    test('TC-CRD-005: Credit large amount to reach maximum balance', () => {
        const result = Operations.execute('CREDIT', 998999.99);
        expect(result.success).toBe(true);
        expect(result.balance).toBeCloseTo(999999.99, 2);
    });

    // TC-CRD-006: Multiple consecutive credits
    test('TC-CRD-006: Multiple consecutive credits', () => {
        Operations.execute('CREDIT', 100.00);
        Operations.execute('CREDIT', 200.00);
        const result = Operations.execute('CREDIT', 300.00);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(1600.00);
    });

    // TC-CRD-007: Credit after debit
    test('TC-CRD-007: Credit after debit', () => {
        Operations.execute('DEBIT', 500.00);
        const result = Operations.execute('CREDIT', 250.00);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(750.00);
    });

    // TC-CRD-008: Credit to zero balance
    test('TC-CRD-008: Credit to zero balance', () => {
        Operations.execute('DEBIT', 1000.00);
        const result = Operations.execute('CREDIT', 100.00);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(100.00);
        expect(result.message).toContain('100.00');
    });
});

// ============================================================================
// 4. Debit Account Tests (Option 3 - DEBIT Operation)
// ============================================================================

describe('Debit Account Tests (DEBIT Operation)', () => {
    // TC-DEB-001: Debit with whole number amount (sufficient funds)
    test('TC-DEB-001: Debit with whole number amount (sufficient funds)', () => {
        const result = Operations.execute('DEBIT', 500);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(500.00);
        expect(result.message).toContain('Amount debited');
        expect(result.message).toContain('500.00');
    });

    // TC-DEB-002: Debit with decimal amount (sufficient funds)
    test('TC-DEB-002: Debit with decimal amount (sufficient funds)', () => {
        const result = Operations.execute('DEBIT', 250.50);
        expect(result.success).toBe(true);
        expect(result.balance).toBeCloseTo(749.50, 2);
    });

    // TC-DEB-003: Debit exact balance amount
    test('TC-DEB-003: Debit exact balance amount', () => {
        const result = Operations.execute('DEBIT', 1000.00);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(0.00);
        expect(result.message).toContain('0.00');
    });

    // TC-DEB-004: Debit with zero amount
    test('TC-DEB-004: Debit with zero amount', () => {
        const result = Operations.execute('DEBIT', 0);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(1000.00);
    });

    // TC-DEB-005: Debit small amount (cents only)
    test('TC-DEB-005: Debit small amount (cents only)', () => {
        const result = Operations.execute('DEBIT', 0.01);
        expect(result.success).toBe(true);
        expect(result.balance).toBeCloseTo(999.99, 2);
    });

    // TC-DEB-006: Debit exceeding balance (insufficient funds)
    test('TC-DEB-006: Debit exceeding balance (insufficient funds)', () => {
        const result = Operations.execute('DEBIT', 1500);
        expect(result.success).toBe(false);
        expect(result.balance).toBe(1000.00);
        expect(result.message).toContain('Insufficient funds');
    });

    // TC-DEB-007: Debit by $0.01 more than balance
    test('TC-DEB-007: Debit by $0.01 more than balance', () => {
        const result = Operations.execute('DEBIT', 1000.01);
        expect(result.success).toBe(false);
        expect(result.balance).toBe(1000.00);
        expect(result.message).toContain('Insufficient funds');
    });

    // TC-DEB-008: Multiple consecutive debits (sufficient funds)
    test('TC-DEB-008: Multiple consecutive debits (sufficient funds)', () => {
        Operations.execute('DEBIT', 100.00);
        Operations.execute('DEBIT', 200.00);
        const result = Operations.execute('DEBIT', 300.00);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(400.00);
    });

    // TC-DEB-009: Debit after credit
    test('TC-DEB-009: Debit after credit', () => {
        Operations.execute('CREDIT', 500.00);
        const result = Operations.execute('DEBIT', 750.00);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(750.00);
    });

    // TC-DEB-010: Debit from zero balance
    test('TC-DEB-010: Debit from zero balance', () => {
        Operations.execute('DEBIT', 1000.00);
        const result = Operations.execute('DEBIT', 1.00);
        expect(result.success).toBe(false);
        expect(result.balance).toBe(0.00);
        expect(result.message).toContain('Insufficient funds');
    });

    // TC-DEB-011: Consecutive debit attempts after insufficient funds
    test('TC-DEB-011: Consecutive debit attempts after insufficient funds', () => {
        const failedResult = Operations.execute('DEBIT', 2000.00);
        expect(failedResult.success).toBe(false);
        expect(failedResult.message).toContain('Insufficient funds');
        
        const successResult = Operations.execute('DEBIT', 500.00);
        expect(successResult.success).toBe(true);
        expect(successResult.balance).toBe(500.00);
    });
});

// ============================================================================
// 5. Data Persistence Tests (DataProgram)
// ============================================================================

describe('Data Persistence Tests (DataProgram)', () => {
    // TC-DAT-001: Balance persists across operations
    test('TC-DAT-001: Balance persists across operations', () => {
        Operations.execute('CREDIT', 500.00);
        let result = Operations.execute('TOTAL');
        expect(result.balance).toBe(1500.00);
        
        Operations.execute('DEBIT', 200.00);
        result = Operations.execute('TOTAL');
        expect(result.balance).toBe(1300.00);
    });

    // TC-DAT-002: Balance resets on application restart (simulated with reset)
    test('TC-DAT-002: Balance resets on reset (simulates restart)', () => {
        Operations.execute('CREDIT', 500.00);
        expect(DataProgram.execute('READ')).toBe(1500.00);
        
        DataProgram.reset();
        expect(DataProgram.execute('READ')).toBe(1000.00);
    });

    // TC-DAT-003: Read operation returns current balance
    test('TC-DAT-003: Read operation returns current balance', () => {
        Operations.execute('CREDIT', 100.00);
        const balance = DataProgram.execute('READ');
        expect(balance).toBe(1100.00);
    });

    // TC-DAT-004: Write operation updates stored balance
    test('TC-DAT-004: Write operation updates stored balance', () => {
        DataProgram.execute('WRITE', 2500.00);
        const balance = DataProgram.execute('READ');
        expect(balance).toBe(2500.00);
    });
});

// ============================================================================
// 6. Boundary Value Tests
// ============================================================================

describe('Boundary Value Tests', () => {
    // TC-BND-001: Maximum balance value
    test('TC-BND-001: Maximum balance value', () => {
        DataProgram.execute('WRITE', 999999.98);
        const result = Operations.execute('CREDIT', 0.01);
        expect(result.success).toBe(true);
        expect(result.balance).toBeCloseTo(999999.99, 2);
    });

    // TC-BND-002: Minimum balance value
    test('TC-BND-002: Minimum balance value', () => {
        DataProgram.execute('WRITE', 0.01);
        const result = Operations.execute('DEBIT', 0.01);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(0.00);
    });

    // TC-BND-003: Maximum transaction amount
    test('TC-BND-003: Maximum transaction amount (credit)', () => {
        const result = Operations.execute('CREDIT', 999999.99);
        // This would exceed maximum, so it should fail
        // Initial balance is 1000.00 + 999999.99 > 999999.99
        expect(result.success).toBe(false);
        expect(result.message).toContain('exceed maximum balance limit');
    });

    // TC-BND-004: Precision with decimal values
    test('TC-BND-004: Precision with decimal values', () => {
        Operations.execute('CREDIT', 0.01);
        Operations.execute('CREDIT', 0.01);
        const result = Operations.execute('CREDIT', 0.01);
        expect(result.balance).toBeCloseTo(1000.03, 2);
    });
});

// ============================================================================
// 7. Integration Tests (Program Communication)
// ============================================================================

describe('Integration Tests (Program Communication)', () => {
    // TC-INT-001: Operations receives correct operation code
    test('TC-INT-001: Operations receives correct operation code for TOTAL', () => {
        const result = Operations.execute('TOTAL');
        expect(result.success).toBe(true);
        expect(result.message).toContain('Current balance');
    });

    // TC-INT-002: Operations calls DataProgram for READ
    test('TC-INT-002: Operations calls DataProgram for READ (view balance)', () => {
        // Modify balance directly to verify Operations reads from DataProgram
        DataProgram.execute('WRITE', 5000.00);
        const result = Operations.execute('TOTAL');
        expect(result.balance).toBe(5000.00);
    });

    // TC-INT-003: Operations calls DataProgram for WRITE
    test('TC-INT-003: Operations calls DataProgram for WRITE after credit', () => {
        Operations.execute('CREDIT', 100.00);
        const balance = DataProgram.execute('READ');
        expect(balance).toBe(1100.00);
    });

    // TC-INT-004: Full transaction flow - Credit
    test('TC-INT-004: Full transaction flow - Credit', () => {
        const initialBalance = DataProgram.execute('READ');
        expect(initialBalance).toBe(1000.00);
        
        const result = Operations.execute('CREDIT', 500.00);
        expect(result.success).toBe(true);
        
        const finalBalance = DataProgram.execute('READ');
        expect(finalBalance).toBe(1500.00);
    });

    // TC-INT-005: Full transaction flow - Debit (success)
    test('TC-INT-005: Full transaction flow - Debit (success)', () => {
        const initialBalance = DataProgram.execute('READ');
        expect(initialBalance).toBe(1000.00);
        
        const result = Operations.execute('DEBIT', 300.00);
        expect(result.success).toBe(true);
        
        const finalBalance = DataProgram.execute('READ');
        expect(finalBalance).toBe(700.00);
    });

    // TC-INT-006: Full transaction flow - Debit (failure)
    test('TC-INT-006: Full transaction flow - Debit (failure, no WRITE)', () => {
        const initialBalance = DataProgram.execute('READ');
        expect(initialBalance).toBe(1000.00);
        
        const result = Operations.execute('DEBIT', 2000.00);
        expect(result.success).toBe(false);
        
        // Balance should remain unchanged
        const finalBalance = DataProgram.execute('READ');
        expect(finalBalance).toBe(1000.00);
    });
});

// ============================================================================
// 8. Error Handling Tests
// ============================================================================

describe('Error Handling Tests', () => {
    // TC-ERR-001: Invalid menu input recovery (simulated with invalid operation)
    test('TC-ERR-001: Invalid operation recovery', () => {
        const invalidResult = Operations.execute('INVALID');
        expect(invalidResult.success).toBe(false);
        
        // System should continue to work after invalid operation
        const validResult = Operations.execute('TOTAL');
        expect(validResult.success).toBe(true);
        expect(validResult.balance).toBe(1000.00);
    });

    // TC-ERR-002: Insufficient funds recovery
    test('TC-ERR-002: Insufficient funds recovery', () => {
        const failedDebit = Operations.execute('DEBIT', 5000.00);
        expect(failedDebit.success).toBe(false);
        
        // Valid operation should work after failed debit
        const validCredit = Operations.execute('CREDIT', 100.00);
        expect(validCredit.success).toBe(true);
        expect(validCredit.balance).toBe(1100.00);
    });

    // TC-ERR-003: Negative amount input
    test('TC-ERR-003: Negative amount input for credit', () => {
        const result = Operations.execute('CREDIT', -100);
        expect(result.success).toBe(false);
        expect(result.message).toContain('Invalid amount');
        expect(result.balance).toBe(1000.00);
    });

    // TC-ERR-003: Negative amount input for debit
    test('TC-ERR-003: Negative amount input for debit', () => {
        const result = Operations.execute('DEBIT', -50);
        expect(result.success).toBe(false);
        expect(result.message).toContain('Invalid amount');
        expect(result.balance).toBe(1000.00);
    });

    // TC-ERR-003: NaN amount input
    test('TC-ERR-003: NaN amount input', () => {
        const result = Operations.execute('CREDIT', NaN);
        expect(result.success).toBe(false);
        expect(result.message).toContain('Invalid amount');
    });
});

// ============================================================================
// Additional Utility Tests
// ============================================================================

describe('Utility Function Tests', () => {
    test('formatBalance formats to 2 decimal places', () => {
        expect(Operations.formatBalance(1000)).toBe('1000.00');
        expect(Operations.formatBalance(1234.5)).toBe('1234.50');
        expect(Operations.formatBalance(0)).toBe('0.00');
        expect(Operations.formatBalance(999999.99)).toBe('999999.99');
    });

    test('Operation types are case-insensitive', () => {
        DataProgram.reset();
        const result1 = Operations.execute('total');
        expect(result1.success).toBe(true);
        
        DataProgram.reset();
        const result2 = Operations.execute('TOTAL');
        expect(result2.success).toBe(true);
        
        DataProgram.reset();
        const result3 = Operations.execute('Total');
        expect(result3.success).toBe(true);
    });

    test('Operation types handle whitespace', () => {
        const result = Operations.execute('  TOTAL  ');
        expect(result.success).toBe(true);
    });
});
