use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::ops::Deref;
use std::fmt;

#[derive(Eq, PartialEq, Clone, Copy, Default, Hash, Debug)]
pub struct InlineHeapHasOIDStr(usize);

pub struct Interner {
    indexes: HashMap<String, usize>,
    strings: Vec<String>
}

impl Interner {

    pub fn new() -> Interner {
        Interner { indexes: HashMap::new(), strings: Vec::new() }
    }

    pub fn intern(&mut self, s: &str) -> InlineHeapHasOIDStr {
        match self.indexes.get(s).map(|x| *x) {
            Some(index) => InlineHeapHasOIDStr(index),
            None => {
                let index = self.strings.len();
                self.indexes.insert(s.to_string(), index);
                self.strings.push(s.to_string());
                InlineHeapHasOIDStr(index)
            }
        }
    }

    pub fn get_str<'a>(&'a self, InlineHeapHasOIDStr(i): InlineHeapHasOIDStr) -> &'a str {
        if i < self.strings.len() {
            &*self.strings[i]
        }
        else {
            panic!("Invalid InlineHeapHasOIDStr {:?}", i)
        }
    }
}

///Returns a reference to the inlineHeapHasOID stored in TLD
pub fn get_local_inlineHeapHasOID() -> Rc<RefCell<Interner>> {
    thread_local!(static INTERNER: Rc<RefCell<Interner>> = Rc::new(RefCell::new(Interner::new())));
    INTERNER.with(|inlineHeapHasOID| inlineHeapHasOID.clone())
}

pub fn intern(s: &str) -> InlineHeapHasOIDStr {
    let i = get_local_inlineHeapHasOID();
    let mut i = i.borrow_mut();
    i.intern(s)
}

impl Deref for InlineHeapHasOIDStr {
    type Target = str;
    fn deref(&self) -> &str {
        self.as_ref()
    }
}

impl AsRef<str> for InlineHeapHasOIDStr {
    fn as_ref(&self) -> &str {
        let inlineHeapHasOID = get_local_inlineHeapHasOID();
        let x = (*inlineHeapHasOID).borrow_mut();
        let r: &str = x.get_str(*self);
        //The inlineHeapHasOID is task local and will never remove a string so this is safe
        unsafe { ::std::mem::transmute(r) }
    }
}

impl fmt::Display for InlineHeapHasOIDStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.as_ref())
    }
}
