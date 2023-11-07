BEGIN{
  print "#include <stdio.h>";
  print
  print "#ifdef WINDOWS_MESSAGEBOX";
  print "#include <windows.h>";
  print "#include <string.h>";
  print "#include <malloc.h>";
  print "#define LICENSE_PRINT(a) strcat(licenseText,a)";
  print "#else";
  print "#define LICENSE_PRINT(a) printf(a)";
  print "#endif";
  print
  print "void license() {";
  print "#ifdef WINDOWS_MESSAGEBOX";
  print "  char *licenseText = malloc(2000);";
  print
  print "  if (licenseText) {";
  print "    licenseText[0] = '\\0';";
  print "#endif";
  print
}

{
  print "  LICENSE_PRINT(\"" $0 "\\n\");"
}

END{
  print "#ifdef WINDOWS_MESSAGEBOX";
  print "  MessageBox(NULL,licenseText,\"License\",MB_OK | MB_ICONINFORMATION);";
  print "  free(licenseText);";
  print "  }";
  print "#endif";
  print "}"
}
