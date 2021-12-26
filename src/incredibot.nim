import os, system, dimscord, asyncdispatch, times, options, regex

proc subredditlink(s: openArray[string]): string =
  discard

when isMainModule:
  if paramCount() < 1:
    stderr.writeLine("Authentication file is required to run!")
    quit(1)
  elif paramCount() > 1:
    stderr.writeLine("Too many arguments! Need only authentication file!")
    quit(1)

  let contents = readFile(paramStr(1))

  let discord = newDiscordClient(contents)

const handlers = [(re("\\s\\/?(r\\/\\w*)"), subredditlink)]

proc messageCreate(s: Shard, m: Message) {.event(discord).} =
  if m.author.bot: return
  for (reg, callback) in handlers:
    let t = m.content.matches(reg)
    if len(t) > 0:
      discard await discord.api.sendMessage(m.channel_id, callback(matches))


