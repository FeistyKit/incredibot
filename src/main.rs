use std::{env, fs::read_to_string, collections::HashMap, time::Instant};

use fancy_regex::Regex;
use serenity::{prelude::{RwLock, EventHandler}, async_trait, model::{channel::Message, gateway::Ready}, client::Context, Client, framework::StandardFramework};
use log::{debug, error};
use simple_logger::SimpleLogger;

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
    Reddit
}

#[allow(dead_code)]
struct Handler {
    prefix: RwLock<String>,
    strong_commands: RwMap<String, StrongAction>, // Stop checking as soon as one of these commands is found.
    weak_commands: RwList<Regex, WeakAction>
}

impl Handler {
    pub fn new(prefix: String, strong_commands: Vec<(String, StrongAction)>, weak_commands: Vec<(Regex, WeakAction)>) -> Handler {
        Handler {
            prefix: RwLock::new(prefix),
            strong_commands: RwLock::new(strong_commands.into_iter().collect::<HashMap<_, _>>()),
            weak_commands: RwLock::new(weak_commands)
        }
    }
}

impl Default for Handler {
    fn default() -> Self {
        let strongs = vec![("prefix".to_string(), StrongAction::SetPrefix), ("learn".to_string(), StrongAction::AddCommand)];
        let weaks = vec![(Regex::new("(?im)(^|[^\\S\\r\\n])\\/?(r\\/\\w+)").unwrap(), WeakAction::Reddit)];
        Self::new("bob!".to_string(), strongs, weaks)
    }
}

#[async_trait]
impl EventHandler for Handler {
    async fn message(&self, ctx: Context, msg: Message) {
        if !from_bot(&msg) {
            let inst = Instant::now();
            if let Some(input) = msg.content.strip_prefix(&*self.prefix.read().await) {
                debug!("Took {} miliseconds to access prefix!", inst.elapsed().as_millis());
                let inst = Instant::now();
                let coms = self.strong_commands.read().await;
                debug!("Took {} miliseconds to access strong commands!", inst.elapsed().as_millis());
                for (com, act) in coms.iter() {
                    if let Some(_arg) = input.strip_prefix(com.as_str()) {
                        match act {
                            StrongAction::Print(s) => {
                                say_resolved(&msg, &ctx, s).await;
                            },
                            StrongAction::AddCommand => {
                                say_resolved(&msg, &ctx, "Adding commands is not currently supported!").await;
                            },
                            StrongAction::SetPrefix => {
                                say_resolved(&msg, &ctx, "Changing the prefix is not currently supported!").await;
                                /* *self.prefix.write().await = arg.to_string();
                                say_resolved(&msg, &ctx, &format!("Prefix successfully changed to `{}`!", arg)).await; */
                            },
                        }
                        return;
                    }
                }
            }
            let inst = Instant::now();
            let weaks = self.weak_commands.read().await;
            debug!("Took {} miliseconds to access weak commands!", inst.elapsed().as_millis());
            for (reg, act) in weaks.iter() {
                for m in reg.captures_iter(&msg.content) {
                    if let Ok(m) = m {
                        match act {
                            WeakAction::Reddit => {
                                let raw = m.get(0).unwrap().as_str().trim();
                                let to_say = raw.strip_prefix("/").unwrap_or(raw);
                                say_resolved(&msg, &ctx, &format!("https://www.reddit.com/{}", to_say)).await;
                            }
                        }
                    }
                }
            }
        }
    }
}

async fn say_resolved(src: &Message, ctx: &Context, to_send: &str) {
    // Stolen from https://github.com/serenity-rs/serenity/blob/7b89775858d92a1c8be05f213b92fbe72b083980/examples/e01_basic_ping_bot/src/main.rs#L24
    if let Err(why) = src.channel_id.say(&ctx.http, to_send).await {
        error!("Error sending message: {:?}", why);
    }
}

fn from_bot(m: &Message) -> bool {
    m.author.bot
}

#[tokio::main]
async fn main() -> Ret<()> {
    SimpleLogger::new().with_level(log::LevelFilter::Debug).init().unwrap();
    let token = get_auth_token()?;
    let handler = Handler::default();
    let mut client = Client::builder(&token).event_handler(handler).framework(StandardFramework::new()).await.expect("Err creating client"); // Stolen from https://github.com/serenity-rs/serenity/blob/7b89775858d92a1c8be05f213b92fbe72b083980/examples/e01_basic_ping_bot/src/main.rs#L49
    if let Err(why) = client.start().await { // Stolen from https://github.com/serenity-rs/serenity/blob/7b89775858d92a1c8be05f213b92fbe72b083980/examples/e01_basic_ping_bot/src/main.rs#L56
        error!("Client error: {:?}", why);
    }
    Ok(())
}

fn get_auth_token() -> Ret<String> {
    let mut args = env::args();
    args.next(); // Get rid of binary name, will always exist

    if let Some(fpath) = args.next() {
        let raw = read_to_string(&fpath);
        match raw {
            Ok(contents) => Ok(contents.split('\n').next().ok_or(format!("File {} is empty!", fpath))?.trim().to_string()),
            Err(_) => Err(format!("Could not read from file {}!", fpath)),
        }
    } else {
        Err("A path to a file containing the authentication token is required to run!".to_string())
    }
}
