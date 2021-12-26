import os, system

when isMainModule:
  if paramCount() < 1:
    stderr.writeLine("Authentication file is required to run!")
    quit(1)
  elif paramCount() > 1:
    stderr.writeLine("Too many arguments! Need only authentication file!")
    quit(1)

  let contents = readFile(paramStr(1))
  echo "Contents: ", contents
