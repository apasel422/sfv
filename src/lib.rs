/*!
`sfv` crate is an implementation of IETF draft [Structured Field Values for HTTP](https://httpwg.org/http-extensions/draft-ietf-httpbis-header-structure.html)
for parsing and serializing structured HTTP field values.
It also exposes a set of types that might be useful for defining new structured fields.

# Data Structures

There are three types of structured fields:

- `Item` - can be an `Integer`, `Decimal`, `String`, `Token`, `Byte Sequence`, or `Boolean`. It can have associated `Parameters`.
- `List` - array of zero or more members, each of which can be an `Item` or an `InnerList`, both of which can be `Parameterized`.
- `Dictionary` - ordered map of name-value pairs, where the names are short textual strings and the values are `Items` or arrays of `Items` (represented with `InnerList`), both of which can be `Parameterized`. There can be zero or more members, and their names are unique in the scope of the `Dictionary` they occur within.

There's a number of primitive types used to construct structured field values:
- `BareItem` used as `Item`'s value or as a parameter value in `Parameters`.
- `Parameters` are an ordered map of key-value pairs that are associated with an `Item` or `InnerList`. The keys are unique within the scope the `Parameters` they occur within, and the values are `BareItem`.
- `InnerList` is an array of zero or more `Items`. Can have `Parameters`.
- `ListEntry` represents either `Item` or `InnerList` as a member of `List` or as member-value in `Dictionary`.

# Examples

### Parsing

```
use sfv::Parser;

// Parsing structured field value of Item type
let item_header_input = "12.445;foo=bar";
let item = Parser::parse_item(item_header_input.as_bytes());
assert!(item.is_ok());
println!("{:#?}", item);

// Parsing structured field value of List type
let list_header_input = "1;a=tok, (\"foo\" \"bar\");baz, ()";
let list = Parser::parse_list(list_header_input.as_bytes());
assert!(list.is_ok());
println!("{:#?}", list);

// Parsing structured field value of Dictionary type
let dict_header_input = "a=?0, b, c; foo=bar, rating=1.5, fruits=(apple pear)";
let dict = Parser::parse_dictionary(dict_header_input.as_bytes());
assert!(dict.is_ok());
println!("{:#?}", dict);

```

### Value Creation and Serialization
Create `Item` with empty parameters:
```
use sfv::{Item, BareItem, SerializeValue};

let str_item = Item::new(BareItem::String(String::from("foo")));
assert_eq!(str_item.serialize_value().unwrap(), "\"foo\"");
```


Create `Item` field value with parameters:
```
use sfv::{Item, BareItem, SerializeValue, Parameters, Decimal, FromPrimitive};

let mut params = Parameters::new();
let decimal = Decimal::from_f64(13.45655).unwrap();
params.insert("key".into(), decimal.into());
let int_item = Item::with_params(99.into(), params);
assert_eq!(int_item.serialize_value().unwrap(), "99;key=13.457");
```

Create `List` field value with `Item` and parametrized `InnerList` as members:
```
use sfv::{Item, BareItem, InnerList, List, SerializeValue, Parameters};

// Create Item
let tok_item = BareItem::Token("tok".into());

// Create InnerList members
let str_item = Item::new(BareItem::String(String::from("foo")));

let mut int_item_params = Parameters::new();
int_item_params.insert("key".into(), BareItem::Boolean(false));
let int_item = Item::with_params(99.into(), int_item_params);

// Create InnerList
let mut inner_list_params = Parameters::new();
inner_list_params.insert("bar".into(), BareItem::Boolean(true));
let inner_list = InnerList::with_params(vec![int_item, str_item], inner_list_params);


let list: List = vec![Item::new(tok_item).into(), inner_list.into()];
assert_eq!(
    list.serialize_value().unwrap(),
    "tok, (99;key=?0 \"foo\");bar"
);
```

Create `Dictionary` field value:
```
use sfv::{Parser, Item, BareItem, SerializeValue, ParseValue, Dictionary};

let member_value1 = Item::new(BareItem::String(String::from("apple")));
let member_value2 = Item::new(BareItem::Boolean(true));
let member_value3 = Item::new(BareItem::Boolean(false));

let mut dict = Dictionary::new();
dict.insert("key1".into(), member_value1.into());
dict.insert("key2".into(), member_value2.into());
dict.insert("key3".into(), member_value3.into());

assert_eq!(
    dict.serialize_value().unwrap(),
    "key1=\"apple\", key2, key3=?0"
);

```
*/

mod parser;
mod serializer;
mod utils;

#[cfg(test)]
mod test_parser;
#[cfg(test)]
mod test_serializer;
use indexmap::IndexMap;

pub use rust_decimal::{
    prelude::{FromPrimitive, FromStr},
    Decimal,
};

pub use parser::{ParseMore, ParseValue, Parser};
pub use serializer::SerializeValue;

type SFVResult<T> = std::result::Result<T, &'static str>;

/// Represents `Item` type structured field value.
/// Can be used as a member of `List` or `Dictionary`.
// sf-item   = bare-item parameters
// bare-item = sf-integer / sf-decimal / sf-string / sf-token
//             / sf-binary / sf-boolean
#[derive(Debug, PartialEq, Clone)]
pub struct Item {
    /// Value of `Item`.
    pub bare_item: BareItem,
    /// `Item`'s associated parameters. Can be empty.
    pub params: Parameters,
}

impl Item {
    /// Returns new `Item` with empty `Parameters`.
    pub fn new(bare_item: BareItem) -> Item {
        Item {
            bare_item,
            params: Parameters::new(),
        }
    }
    /// Returns new `Item` with specified `Parameters`.
    pub fn with_params(bare_item: BareItem, params: Parameters) -> Item {
        Item { bare_item, params }
    }
}

/// Represents `Dictionary` type structured field value.
// sf-dictionary  = dict-member *( OWS "," OWS dict-member )
// dict-member    = member-name [ "=" member-value ]
// member-name    = key
// member-value   = sf-item / inner-list
pub type Dictionary = IndexMap<String, ListEntry>;

/// Represents `List` type structured field value.
// sf-list       = list-member *( OWS "," OWS list-member )
// list-member   = sf-item / inner-list
pub type List = Vec<ListEntry>;

/// Parameters of `Item` or `InnerList`.
// parameters    = *( ";" *SP parameter )
// parameter     = param-name [ "=" param-value ]
// param-name    = key
// key           = ( lcalpha / "*" )
//                 *( lcalpha / DIGIT / "_" / "-" / "." / "*" )
// lcalpha       = %x61-7A ; a-z
// param-value   = bare-item
pub type Parameters = IndexMap<String, BareItem>;

/// Represents a member of `List` or `Dictionary` structured field value.
#[derive(Debug, PartialEq, Clone)]
pub enum ListEntry {
    /// Member of `Item` type.
    Item(Item),
    /// Member of `InnerList` (array of `Items`) type.
    InnerList(InnerList),
}

impl From<Item> for ListEntry {
    fn from(item: Item) -> Self {
        ListEntry::Item(item)
    }
}

impl From<InnerList> for ListEntry {
    fn from(item: InnerList) -> Self {
        ListEntry::InnerList(item)
    }
}

/// Array of `Items` with associated `Parameters`.
// inner-list    = "(" *SP [ sf-item *( 1*SP sf-item ) *SP ] ")"
//                 parameters
#[derive(Debug, PartialEq, Clone)]
pub struct InnerList {
    /// `Items` that `InnerList` contains. Can be empty
    pub items: Vec<Item>,
    /// `InnerList`'s associated parameters. Can be empty.
    pub params: Parameters,
}

impl InnerList {
    /// Returns new `InnerList` with empty `Parameters`.
    pub fn new(items: Vec<Item>) -> InnerList {
        InnerList {
            items,
            params: Parameters::new(),
        }
    }

    /// Returns new `InnerList` with specified `Parameters`.
    pub fn with_params(items: Vec<Item>, params: Parameters) -> InnerList {
        InnerList { items, params }
    }
}

/// `BareItem` type is used to construct `Items` or `Parameters` values.
#[derive(Debug, PartialEq, Clone)]
pub enum BareItem {
    // Either Integer, or Decimal.
    Number(Num),
    // sf-string = DQUOTE *chr DQUOTE
    // chr       = unescaped / escaped
    // unescaped = %x20-21 / %x23-5B / %x5D-7E
    // escaped   = "\" ( DQUOTE / "\" )
    String(String),
    // ":" *(base64) ":"
    // base64    = ALPHA / DIGIT / "+" / "/" / "="
    ByteSeq(Vec<u8>),
    // sf-boolean = "?" boolean
    // boolean    = "0" / "1"
    Boolean(bool),
    // sf-token = ( ALPHA / "*" ) *( tchar / ":" / "/" )
    Token(String),
}

impl From<i64> for BareItem {
    fn from(item: i64) -> Self {
        BareItem::Number(Num::Integer(item))
    }
}

impl From<Decimal> for BareItem {
    fn from(item: Decimal) -> Self {
        BareItem::Number(Num::Decimal(item))
    }
}

/// Used to represent either `Decimal` or `Integer` as `Numeric` variant of `BareItem`.
#[derive(Debug, PartialEq, Clone)]
pub enum Num {
    /// Decimal number
    // sf-decimal  = ["-"] 1*12DIGIT "." 1*3DIGIT
    Decimal(Decimal),
    /// Integer number
    // sf-integer = ["-"] 1*15DIGIT
    Integer(i64),
}
