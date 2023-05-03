use std::cell::RefCell;
use std::collections::hash_map::HashMap;


#[allow(dead_code)]
#[allow(unused_variables)]
pub fn too() {
    let rc = RefCell::new(42);

    let rrc = rc.try_borrow();

    match rrc {
        Ok(rref) => {},
        Err(err) => eprintln!("Error while borrow: {err}")
    }
}

pub fn update_or_insert(hm: &mut HashMap<String, String>, key: &str, val: &String) {

    if let Some(old_v) = hm.get(key) {
        let mut new_v = String::from(val);
        new_v.push_str(old_v);
        hm.insert(key.to_string(), new_v.to_string().clone());
    }
}
