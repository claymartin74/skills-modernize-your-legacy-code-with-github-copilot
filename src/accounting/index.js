/**
 * Student Account Management System
 * Modernized from COBOL legacy application
 * 
 * This application preserves the original business logic, data integrity,
 * and menu options from the legacy COBOL system.
 * 
 * Architecture (Three-tier design):
 * - Main Program: Entry point and user interface
 * - Operations: Business logic for account operations
 * - Data Program: Data storage and persistence layer
 */

const readlineSync = require('readline-sync');

// ============================================================================
// DATA PROGRAM (Equivalent to data.cob / DataProgram)
// Acts as the data access layer, managing the persistent storage of the account balance
// ============================================================================

const DataProgram = (() => {
    // WORKING-STORAGE SECTION
    // STORAGE-BALANCE PIC 9(6)V99 VALUE 1000.00
    // Persistent balance storage (default: 1000.00)
    let storageBalance = 1000.00;

    /**
     * Data access operations for reading and writing balance
     * @param {string} operation - 'READ' or 'WRITE'
     * @param {number} balance - Balance value for WRITE operations
     * @returns {number} Current balance for READ operations
     */
    function execute(operation, balance = 0) {
        const operationType = operation.trim().toUpperCase();

        if (operationType === 'READ') {
            return storageBalance;
        } else if (operationType === 'WRITE') {
            storageBalance = balance;
            return storageBalance;
        }
        
        return storageBalance;
    }

    /**
     * Reset balance to initial value (for testing purposes)
     */
    function reset() {
        storageBalance = 1000.00;
    }

    return { execute, reset };
})();

// ============================================================================
// OPERATIONS (Equivalent to operations.cob / Operations)
// Contains the business logic for all account operations
// ============================================================================

const Operations = (() => {
    /**
     * Execute account operations based on operation type
     * @param {string} passedOperation - 'TOTAL', 'CREDIT', or 'DEBIT'
     * @param {number|null} amount - Optional amount for CREDIT/DEBIT operations (for testing)
     * @returns {object} Result object with success status, message, and balance
     */
    function execute(passedOperation, amount = null) {
        const operationType = passedOperation.trim().toUpperCase();

        if (operationType === 'TOTAL') {
            // View Balance operation
            const finalBalance = DataProgram.execute('READ');
            const message = `Current balance: ${formatBalance(finalBalance)}`;
            console.log(message);
            return { success: true, message, balance: finalBalance };

        } else if (operationType === 'CREDIT') {
            // Credit Account operation
            let creditAmount = amount;
            if (creditAmount === null) {
                const amountStr = readlineSync.question('Enter credit amount: ');
                creditAmount = parseFloat(amountStr);
            }

            if (isNaN(creditAmount) || creditAmount < 0) {
                const message = 'Invalid amount entered.';
                console.log(message);
                return { success: false, message, balance: DataProgram.execute('READ') };
            }

            let finalBalance = DataProgram.execute('READ');
            finalBalance = finalBalance + creditAmount;
            
            // Validate maximum balance (PIC 9(6)V99 = max 999999.99)
            if (finalBalance > 999999.99) {
                const message = 'Credit would exceed maximum balance limit.';
                console.log(message);
                return { success: false, message, balance: DataProgram.execute('READ') };
            }
            
            DataProgram.execute('WRITE', finalBalance);
            const message = `Amount credited. New balance: ${formatBalance(finalBalance)}`;
            console.log(message);
            return { success: true, message, balance: finalBalance };

        } else if (operationType === 'DEBIT') {
            // Debit Account operation
            let debitAmount = amount;
            if (debitAmount === null) {
                const amountStr = readlineSync.question('Enter debit amount: ');
                debitAmount = parseFloat(amountStr);
            }

            if (isNaN(debitAmount) || debitAmount < 0) {
                const message = 'Invalid amount entered.';
                console.log(message);
                return { success: false, message, balance: DataProgram.execute('READ') };
            }

            let finalBalance = DataProgram.execute('READ');
            
            // Business Rule: Insufficient Funds Check
            // Debits cannot exceed the current balance
            if (finalBalance >= debitAmount) {
                finalBalance = finalBalance - debitAmount;
                DataProgram.execute('WRITE', finalBalance);
                const message = `Amount debited. New balance: ${formatBalance(finalBalance)}`;
                console.log(message);
                return { success: true, message, balance: finalBalance };
            } else {
                const message = 'Insufficient funds for this debit.';
                console.log(message);
                return { success: false, message, balance: finalBalance };
            }
        }

        return { success: false, message: 'Unknown operation', balance: DataProgram.execute('READ') };
    }

    /**
     * Format balance for display (2 decimal places)
     * @param {number} balance - Balance to format
     * @returns {string} Formatted balance string
     */
    function formatBalance(balance) {
        return balance.toFixed(2);
    }

    return { execute, formatBalance };
})();

// ============================================================================
// MAIN PROGRAM (Equivalent to main.cob / MainProgram)
// Entry point and user interface
// ============================================================================

const MainProgram = (() => {
    // WORKING-STORAGE SECTION
    // USER-CHOICE PIC 9 VALUE 0
    // CONTINUE-FLAG PIC X(3) VALUE 'YES'
    let continueFlag = 'YES';

    /**
     * Display the main menu
     */
    function displayMenu() {
        console.log('--------------------------------');
        console.log('Account Management System');
        console.log('1. View Balance');
        console.log('2. Credit Account');
        console.log('3. Debit Account');
        console.log('4. Exit');
        console.log('--------------------------------');
    }

    /**
     * Main program logic - runs the menu loop
     */
    function run() {
        // PERFORM UNTIL CONTINUE-FLAG = 'NO'
        while (continueFlag === 'YES') {
            displayMenu();
            const userChoice = readlineSync.question('Enter your choice (1-4): ');
            const choice = parseInt(userChoice, 10);

            // EVALUATE USER-CHOICE
            switch (choice) {
                case 1:
                    // CALL 'Operations' USING 'TOTAL '
                    Operations.execute('TOTAL');
                    break;
                case 2:
                    // CALL 'Operations' USING 'CREDIT'
                    Operations.execute('CREDIT');
                    break;
                case 3:
                    // CALL 'Operations' USING 'DEBIT '
                    Operations.execute('DEBIT');
                    break;
                case 4:
                    // MOVE 'NO' TO CONTINUE-FLAG
                    continueFlag = 'NO';
                    break;
                default:
                    // WHEN OTHER
                    console.log('Invalid choice, please select 1-4.');
            }
        }

        // DISPLAY "Exiting the program. Goodbye!"
        console.log('Exiting the program. Goodbye!');
    }

    return { run };
})();

// ============================================================================
// Program Entry Point
// ============================================================================

// Only run the main program if this file is executed directly (not required as a module)
if (require.main === module) {
    MainProgram.run();
}

// Export modules for testing
module.exports = { DataProgram, Operations, MainProgram };
