use tera::{Result, Tera};

pub fn load_templates(pattern: &str) -> Result<Tera> {
    let mut templates = Tera::new(pattern)?;
    templates.autoescape_on(vec![]);

    // Implemented another way, but kept if we want to reference it in the future.
    // templates.register_function("has_revision", |args: &HashMap<String, Value>| {
    //     let commit = match args.get("latest_commit") {
    //         None => return Ok(Value::Bool(false)),
    //         Some(x) => x,
    //     };
    //     let created = args
    //         .get("created")
    //         .ok_or("must provide `created` to `has_revision`")?;
    //     let commit_dt =
    //         DateTime::parse_from_rfc3339(commit.get("dt").unwrap().as_str().unwrap()).unwrap();
    //     let created_dt = DateTime::parse_from_rfc3339(created.as_str().unwrap()).unwrap();
    //
    //     Ok(Value::Bool(commit_dt.date() != created_dt.date()))
    // });

    Ok(templates)
}
