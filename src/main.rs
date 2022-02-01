use std::{collections::HashMap, env, fs::read_to_string};

use fancy_regex::Regex;
use lazy_static::lazy_static;
use serenity::{
    async_trait,
    client::Context,
    framework::StandardFramework,
    model::{channel::Message, gateway::Ready},
    prelude::{EventHandler, RwLock},
    Client,
};
use tokio::time::Instant;

type Ret<T> = Result<T, String>;
type RwList<K, V> = RwLock<Vec<(K, V)>>;
type RwMap<K, V> = RwLock<HashMap<K, V>>;

#[allow(dead_code)]
enum StrongAction {
    Print(String),
    AddCommand,
    SetPrefix,
}

#[allow(dead_code)]
// TODO: Add a way to parse regexes and add custom commands?
enum WeakAction {
    Reddit,
}

#[allow(dead_code)]
struct Handler {
    prefix: RwLock<String>,
    strong_commands: RwMap<String, StrongAction>, // Stop checking as soon as one of these commands is found.
    weak_commands: RwList<Regex, WeakAction>,
}

impl Handler {
    pub fn new(
        prefix: String,
        strong_commands: Vec<(String, StrongAction)>,
        weak_commands: Vec<(Regex, WeakAction)>,
    ) -> Handler {
        Handler {
            prefix: RwLock::new(prefix),
            strong_commands: RwLock::new(strong_commands.into_iter().collect::<HashMap<_, _>>()),
            weak_commands: RwLock::new(weak_commands),
        }
    }
}

impl Default for Handler {
    fn default() -> Self {
        let strongs = vec![
            ("prefix".to_string(), StrongAction::SetPrefix),
            ("learn".to_string(), StrongAction::AddCommand),
        ];
        let weaks = vec![(
            Regex::new("(?im)(^|[^\\S\\r\\n])\\/?(r\\/\\w+)").unwrap(),
            WeakAction::Reddit,
        )];
        Self::new("bob!".to_string(), strongs, weaks)
    }
}

#[async_trait]
impl EventHandler for Handler {
    async fn message(&self, ctx: Context, msg: Message) {
        let t = Instant::now();
        if !from_bot(&msg) {
            if msg.author.id.0 == CHANNEL.user_id
                && msg.channel_id.0 != CHANNEL.channel_allowed
                && CHANNEL.regex.is_match(&msg.content).unwrap()
            {
                msg.author
                    .dm(&ctx.http, |m| {
                        m.content(format!(
                            "Your message was deleted for not posting in the right channel!"
                        ))
                    })
                    .await
                    .unwrap();
                msg.delete(&ctx.http).await.unwrap();
            }
            if let Some(input) = msg.content.strip_prefix(&*self.prefix.read().await) {
                for (com, act) in self.strong_commands.read().await.iter() {
                    if let Some(_arg) = input.strip_prefix(com.as_str()) {
                        match act {
                            StrongAction::Print(s) => {
                                say_resolved(&msg, &ctx, s).await;
                            }
                            StrongAction::AddCommand => {
                                say_resolved(
                                    &msg,
                                    &ctx,
                                    "Adding commands is not currently supported!",
                                )
                                .await;
                            }
                            StrongAction::SetPrefix => {
                                say_resolved(
                                    &msg,
                                    &ctx,
                                    "Changing the prefix is not currently supported!",
                                )
                                .await;
                                /* *self.prefix.write().await = arg.to_string();
                                say_resolved(&msg, &ctx, &format!("Prefix successfully changed to `{}`!", arg)).await; */
                            }
                        }
                        return;
                    }
                }
            }
            for (reg, act) in self.weak_commands.read().await.iter() {
                for m in reg.captures_iter(&msg.content) {
                    if let Ok(m) = m {
                        match act {
                            WeakAction::Reddit => {
                                let raw = m.get(0).unwrap().as_str().trim();
                                let to_say = raw.strip_prefix("/").unwrap_or(raw);
                                say_resolved(
                                    &msg,
                                    &ctx,
                                    &format!("https://www.reddit.com/{}", to_say),
                                )
                                .await;
                            }
                        }
                    }
                }
            }
        }
        println!(
            "{} millis elapsed in message handling",
            t.elapsed().as_millis()
        );
    }
}

async fn say_resolved(src: &Message, ctx: &Context, to_send: &str) {
    let t = Instant::now();
    // Stolen from https://github.com/serenity-rs/serenity/blob/7b89775858d92a1c8be05f213b92fbe72b083980/examples/e01_basic_ping_bot/src/main.rs#L24
    if let Err(why) = src.channel_id.say(&ctx.http, to_send).await {
        println!("Error sending message: {:?}", why);
    }
    println!(
        "{} millis elapsed while sending the message",
        t.elapsed().as_millis()
    );
}

fn from_bot(m: &Message) -> bool {
    m.author.bot
}

#[tokio::main]
async fn main() -> Ret<()> {
    let token = get_auth_token()?;
    let handler = Handler::default();
    let mut client = Client::builder(&token)
        .event_handler(handler)
        .framework(StandardFramework::new())
        .await
        .expect("Err creating client"); // Stolen from https://github.com/serenity-rs/serenity/blob/7b89775858d92a1c8be05f213b92fbe72b083980/examples/e01_basic_ping_bot/src/main.rs#L49
    println!("{}", CHANNEL.channel_allowed); // Initializing it
    if let Err(why) = client.start().await {
        // Stolen from https://github.com/serenity-rs/serenity/blob/7b89775858d92a1c8be05f213b92fbe72b083980/examples/e01_basic_ping_bot/src/main.rs#L56
        println!("Client error: {:?}", why);
    }
    Ok(())
}

fn get_auth_token() -> Ret<String> {
    let mut args = env::args();
    args.next(); // Get rid of binary name, will always exist

    if let Some(fpath) = args.next() {
        let raw = read_to_string(&fpath);
        match raw {
            Ok(contents) => Ok(contents
                .split('\n')
                .next()
                .ok_or(format!("File {} is empty!", fpath))?
                .trim()
                .to_string()),
            Err(_) => Err(format!("Could not read from file {}!", fpath)),
        }
    } else {
        Err("A path to a file containing the authentication token is required to run!".to_string())
    }
}

// This is a quick hack to resolve drama on a personal discord server, it won't stay
fn get_only_one_details() -> Ret<OnlyOneChannel> {
    let mut args = env::args();
    args.next(); // Get rid of binary name, will always exist

    if let Some(fpath) = args.next() {
        let raw = read_to_string(&fpath);
        match raw {
            Ok(contents) => {
                let mut ls = contents.split('\n');
                ls.next();
                let raw_user_id = ls
                    .next()
                    .ok_or("Could not get user id!")
                    .map_err(|e| format!("{}", e))?
                    .trim()
                    .parse::<u64>()
                    .map_err(|e| format!("{}", e))?;
                let raw_channel_id = ls
                    .next()
                    .ok_or("Could not get channel id!")
                    .map_err(|e| format!("{}", e))?
                    .trim()
                    .parse::<u64>()
                    .map_err(|e| format!("{}", e))?;
                let reg = Regex::new(ls.next().ok_or("Could not find regex!")?)
                    .map_err(|e| format!("{}", e))?;

                Ok(OnlyOneChannel {
                    user_id: raw_user_id,
                    channel_allowed: raw_channel_id,
                    regex: reg,
                })
            }

            Err(_) => Err(format!("Could not read from file {}!", fpath)),
        }
    } else {
        Err("A path to a file containing the authentication token is required to run!".to_string())
    }
}

lazy_static! {
    static ref CHANNEL: OnlyOneChannel = get_only_one_details().unwrap();
}

#[derive(Debug)]
struct OnlyOneChannel {
    user_id: u64,
    channel_allowed: u64,
    regex: Regex,
}
