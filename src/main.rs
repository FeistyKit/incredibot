

type RetErr<T> = Result<T, String>;


fn main() -> RetErr<()> {
    Ok(())
}

fn get_auth_token() -> RetErr<String> {

}
