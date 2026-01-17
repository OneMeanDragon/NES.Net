#include "DiagnosticHelpers.h"

#include <string>
#include <format>

void InvalidPointer(DiagnosticLogCallback callback, int line, const char* file, const char* msg) {
	if (callback) {
		callback(std::format("Error: Invalid Pointer at line: {} in file {} (\"{}\")", line, file, msg).c_str());
	}
}