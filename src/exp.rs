use std::cell::RefCell;

enum Tree<T> {
    Node { value: T, left: Box<Tree<T>>, right: Box<Tree<T>> },
    Empty,
}

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
