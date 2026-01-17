#pragma once

typedef void(__stdcall* DiagnosticLogCallback)(const char* message);

void InvalidPointer(DiagnosticLogCallback callback, int line, const char* file, const char* msg);