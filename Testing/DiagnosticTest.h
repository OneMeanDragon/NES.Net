#pragma once

#include <iostream>
#include <iomanip>
#include <string>

class DiagnosticTest {
private:
    int passed;
    int failed;
    std::string currentTest;

public:
    DiagnosticTest() : passed(0), failed(0) {}

    void StartTest(const std::string& name) {
        currentTest = name;
        std::cout << "\n=== " << name << " ===" << std::endl;
    }

    void Assert(bool condition, const std::string& message) {
        if (condition) {
            passed++;
            std::cout << "  [PASS] " << message << std::endl;
        }
        else {
            failed++;
            std::cout << "  [FAIL] " << message << std::endl;
        }
    }

    void AssertEquals(uint8_t expected, uint8_t actual, const std::string& message) {
        if (expected == actual) {
            passed++;
            std::cout << "  [PASS] " << message << std::endl;
        }
        else {
            failed++;
            std::cout << "  [FAIL] " << message
                << " (Expected: 0x" << std::hex << std::setw(2) << std::setfill('0') << (int)expected
                << ", Got: 0x" << std::setw(2) << std::setfill('0') << (int)actual << std::dec << ")" << std::endl;
        }
    }

    void AssertEquals(uint16_t expected, uint16_t actual, const std::string& message) {
        if (expected == actual) {
            passed++;
            std::cout << "  [PASS] " << message << std::endl;
        }
        else {
            failed++;
            std::cout << "  [FAIL] " << message
                << " (Expected: 0x" << std::hex << std::setw(4) << std::setfill('0') << expected
                << ", Got: 0x" << std::setw(4) << std::setfill('0') << actual << std::dec << ")" << std::endl;
        }
    }

    void Info(const std::string& message) {
        std::cout << "  [INFO] " << message << std::endl;
    }

    void PrintSummary() {
        std::cout << "\n========================================" << std::endl;
        std::cout << "TEST SUMMARY" << std::endl;
        std::cout << "========================================" << std::endl;
        std::cout << "Total Passed: " << passed << std::endl;
        std::cout << "Total Failed: " << failed << std::endl;
        std::cout << "Total Tests:  " << (passed + failed) << std::endl;
        if (passed + failed > 0) {
            std::cout << "Success Rate: " << std::fixed << std::setprecision(2)
                << (100.0 * passed / (passed + failed)) << "%" << std::endl;
        }
    }

    int GetPassed() const { return passed; }
    int GetFailed() const { return failed; }
};