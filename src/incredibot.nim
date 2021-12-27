import os, system, dimscord, asyncdispatch, options, regex, std/strutils, std/sequtils

when isMainModule:
  if paramCount() < 1:
    stderr.writeLine("Authentication file is required to run!")
    quit(1)
  elif paramCount() > 2:
    stderr.writeLine("Too many arguments! Need only authentication file!")
    quit(1)

  let contents = readFile(paramStr(1)).split(Whitespace)

  let discord = newDiscordClient(contents[0])

proc subredditlink(s: openArray[string]): seq[string] =
  var toReturn = newSeq[string](0)
  for rawLink in s:
      let slightlyLessRawLink = rawLink.strip().toLower
      let subname = slightlyLessRawLink.split(sep = "r/")[1]
      toReturn.add("https://www.reddit.com/r/" & subname)
  return toReturn

const handlers = [(re(r"(?im)(^|[^\S\r\n])\/?(r\/\w+)"), subredditlink)]

proc messageCreate(s: Shard, m: Message) {.event(discord).} =
  if m.author.bot: return
  for (reg, callback) in handlers:
    let t = m.content.findAndCaptureAll(reg)
    if len(t) > 0:
      for msg in callback(t):
        discard await discord.api.sendMessage(m.channel_id, msg)

# Connect to Discord and run the bot.
waitFor discord.startSession()
