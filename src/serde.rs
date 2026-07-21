#![allow(missing_docs)]

use std::{borrow::Cow, marker::PhantomData, string::String as StdString};

use serde_core::de;

use crate::{
    error, utils, Date, Decimal, Integer, Key, KeyRef, String, StringRef, Token, TokenRef, Version,
};

impl de::Error for error::Error {
    fn custom<T: std::fmt::Display>(msg: T) -> Self {
        error::Repr::Visit(msg.to_string().into()).into()
    }
}

// Workaround for lack https://github.com/rust-lang/rust/issues/99697.
trait MakeDeserializer: 'static {
    fn make_deserializer<'a, 'de>(
        parser: &'a mut Parser<'de>,
    ) -> impl de::Deserializer<'de, Error = error::Error> + 'a;
}

fn item_deserializer<'a, 'de>(
    parser: &'a mut Parser<'de>,
) -> impl de::Deserializer<'de, Error = error::Error> + 'a {
    // https://httpwg.org/specs/rfc9651.html#parse-item

    struct Maker;

    impl MakeDeserializer for Maker {
        fn make_deserializer<'a, 'de>(
            parser: &'a mut Parser<'de>,
        ) -> impl de::Deserializer<'de, Error = error::Error> + 'a {
            parser.bare_item_deserializer()
        }
    }

    parser.parameterized_deserializer::<Maker>()
}

struct CommaSeparated<'a, 'de> {
    parser: &'a mut Parser<'de>,
}

impl<'de> de::SeqAccess<'de> for CommaSeparated<'_, 'de> {
    type Error = error::Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        if self.parser.peek().is_none() {
            return Ok(None);
        }

        let value = seed.deserialize(self.parser.list_entry_deserializer())?;
        self.consume_suffix()?;
        Ok(Some(value))
    }
}

impl<'de> de::MapAccess<'de> for CommaSeparated<'_, 'de> {
    type Error = error::Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.parser.peek().is_none() {
            return Ok(None);
        }

        seed.deserialize(de::value::BorrowedStrDeserializer::new(
            self.parser.parse_key()?.as_str(),
        ))
        .map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let value = if let Some(b'=') = self.parser.peek() {
            self.parser.next();
            seed.deserialize(self.parser.list_entry_deserializer())
        } else {
            struct Maker;

            impl MakeDeserializer for Maker {
                fn make_deserializer<'a, 'de>(
                    _: &'a mut Parser<'de>,
                ) -> impl de::Deserializer<'de, Error = error::Error> + 'a {
                    de::value::BoolDeserializer::new(true)
                }
            }

            seed.deserialize(self.parser.parameterized_deserializer::<Maker>())
        }?;

        self.consume_suffix()?;
        Ok(value)
    }
}

impl CommaSeparated<'_, '_> {
    fn consume_suffix(&mut self) -> Result<(), error::Repr> {
        self.parser.consume_ows_chars();

        match self.parser.peek() {
            None => return Ok(()),
            Some(b',') => {}
            Some(_) => {
                return Err(error::Repr::TrailingCharactersAfterMember(
                    self.parser.index,
                ))
            }
        }

        let comma_index = self.parser.index;
        self.parser.next();

        self.parser.consume_ows_chars();

        if self.parser.peek().is_none() {
            // Report the error at the position of the comma itself, rather
            // than at the end of input.
            Err(error::Repr::TrailingComma(comma_index))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, PartialEq)]
enum Num {
    Decimal(Decimal),
    Integer(Integer),
}

/// Exposes methods for parsing input into a structured field value.
#[derive(Debug)]
#[must_use]
pub struct Parser<'de> {
    input: &'de [u8],
    index: usize,
    version: Version,
}

impl<'de> Parser<'de> {
    /// Creates a parser from the given input with [`Version::Rfc9651`].
    pub fn new(input: &'de (impl ?Sized + AsRef<[u8]>)) -> Self {
        Self {
            input: input.as_ref(),
            index: 0,
            version: Version::Rfc9651,
        }
    }

    /// Sets the parser's version and returns it.
    pub fn with_version(mut self, version: Version) -> Self {
        self.version = version;
        self
    }

    #[must_use]
    pub fn into_item_deserializer(self) -> impl de::Deserializer<'de, Error = error::Error> {
        struct Maker;

        impl MakeDeserializer for Maker {
            fn make_deserializer<'a, 'de>(
                parser: &'a mut Parser<'de>,
            ) -> impl de::Deserializer<'de, Error = error::Error> + 'a {
                item_deserializer(parser)
            }
        }

        self.top_level_deserializer::<Maker>()
    }

    #[must_use]
    pub fn into_list_deserializer(self) -> impl de::Deserializer<'de, Error = error::Error> {
        struct Maker;

        impl MakeDeserializer for Maker {
            fn make_deserializer<'a, 'de>(
                parser: &'a mut Parser<'de>,
            ) -> impl de::Deserializer<'de, Error = error::Error> + 'a {
                de::value::SeqAccessDeserializer::new(CommaSeparated { parser })
            }
        }

        self.top_level_deserializer::<Maker>()
    }

    #[must_use]
    pub fn into_dictionary_deserializer(self) -> impl de::Deserializer<'de, Error = error::Error> {
        struct Maker;

        impl MakeDeserializer for Maker {
            fn make_deserializer<'a, 'de>(
                parser: &'a mut Parser<'de>,
            ) -> impl de::Deserializer<'de, Error = error::Error> + 'a {
                de::value::MapAccessDeserializer::new(CommaSeparated { parser })
            }
        }

        self.top_level_deserializer::<Maker>()
    }

    fn peek(&self) -> Option<u8> {
        self.input.get(self.index).copied()
    }

    fn next(&mut self) -> Option<u8> {
        self.peek().inspect(|_| self.index += 1)
    }

    fn top_level_deserializer<M>(self) -> impl de::Deserializer<'de, Error = error::Error>
    where
        M: MakeDeserializer,
    {
        // https://httpwg.org/specs/rfc9651.html#text-parse

        struct Deserializer<'de, M> {
            parser: Parser<'de>,
            marker: PhantomData<M>,
        }

        impl<'de, M> de::Deserializer<'de> for Deserializer<'de, M>
        where
            M: MakeDeserializer,
        {
            type Error = error::Error;

            fn deserialize_any<V>(mut self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.parser.consume_sp_chars();

                let value = M::make_deserializer(&mut self.parser).deserialize_any(visitor)?;

                self.parser.consume_sp_chars();

                if self.parser.peek().is_some() {
                    return Err(
                        error::Repr::TrailingCharactersAfterParsedValue(self.parser.index).into(),
                    );
                }

                Ok(value)
            }

            serde_core::forward_to_deserialize_any! {
                bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
                bytes byte_buf option unit unit_struct newtype_struct seq tuple
                tuple_struct map struct enum identifier ignored_any
            }
        }

        Deserializer::<M> {
            parser: self,
            marker: PhantomData,
        }
    }

    fn list_entry_deserializer(&mut self) -> impl de::Deserializer<'de, Error = error::Error> + '_ {
        // https://httpwg.org/specs/rfc9651.html#parse-item-or-list
        // ListEntry represents a tuple (item_or_inner_list, parameters)

        struct Deserializer<'a, 'de> {
            parser: &'a mut Parser<'de>,
        }

        impl<'de> de::Deserializer<'de> for Deserializer<'_, 'de> {
            type Error = error::Error;

            fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                if let Some(b'(') = self.parser.peek() {
                    self.parser
                        .inner_list_deserializer()?
                        .deserialize_any(visitor)
                } else {
                    item_deserializer(self.parser).deserialize_any(visitor)
                }
            }

            serde_core::forward_to_deserialize_any! {
                bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
                bytes byte_buf option unit unit_struct newtype_struct seq tuple
                tuple_struct map struct enum identifier ignored_any
            }
        }

        Deserializer { parser: self }
    }

    fn inner_list_deserializer(
        &mut self,
    ) -> Result<impl de::Deserializer<'de, Error = error::Error> + '_, error::Repr> {
        // https://httpwg.org/specs/rfc9651.html#parse-innerlist

        struct SeqAccess<'a, 'de> {
            parser: &'a mut Parser<'de>,
        }

        impl<'de> de::SeqAccess<'de> for SeqAccess<'_, 'de> {
            type Error = error::Error;

            fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
            where
                T: de::DeserializeSeed<'de>,
            {
                if self.parser.peek().is_some() {
                    self.parser.consume_sp_chars();

                    if let Some(b')') = self.parser.peek() {
                        self.parser.next();
                        return Ok(None);
                    }

                    let value = seed.deserialize(item_deserializer(self.parser))?;

                    if let Some(c) = self.parser.peek() {
                        if c != b' ' && c != b')' {
                            Err(error::Repr::ExpectedInnerListDelimiter(self.parser.index))?;
                        }
                    }

                    return Ok(Some(value));
                }

                Err(error::Repr::UnterminatedInnerList(self.parser.index))?
            }
        }

        struct Maker;

        impl MakeDeserializer for Maker {
            fn make_deserializer<'a, 'de>(
                parser: &'a mut Parser<'de>,
            ) -> impl de::Deserializer<'de, Error = error::Error> + 'a {
                de::value::SeqAccessDeserializer::new(SeqAccess { parser })
            }
        }

        let Some(b'(') = self.peek() else {
            return Err(error::Repr::ExpectedStartOfInnerList(self.index));
        };

        self.next();

        Ok(self.parameterized_deserializer::<Maker>())
    }

    fn bare_item_deserializer(&mut self) -> impl de::Deserializer<'de, Error = error::Error> + '_ {
        // https://httpwg.org/specs/rfc9651.html#parse-bare-item

        struct Deserializer<'a, 'de> {
            parser: &'a mut Parser<'de>,
        }

        impl<'de> de::Deserializer<'de> for Deserializer<'_, 'de> {
            type Error = error::Error;

            fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                match self.parser.peek() {
                    Some(b'?') => self.deserialize_bool(visitor),
                    Some(b'"') => visitor.visit_map(KeyedAccess::new(
                        STRING_KEY,
                        de::value::CowStrDeserializer::new(match self.parser.parse_string()? {
                            Cow::Borrowed(v) => Cow::Borrowed(v.as_str()),
                            Cow::Owned(v) => Cow::Owned(v.into()),
                        }),
                    )),
                    Some(b':') => self.deserialize_byte_buf(visitor),
                    Some(b'@') => visitor.visit_map(KeyedAccess::new(
                        DATE_KEY,
                        de::value::I64Deserializer::new(
                            self.parser.parse_date()?.unix_seconds().into(),
                        ),
                    )),
                    Some(b'%') => visitor.visit_map(KeyedAccess::new(
                        DISPLAY_STRING_KEY,
                        de::value::CowStrDeserializer::new(self.parser.parse_display_string()?),
                    )),
                    Some(c) if utils::is_allowed_start_token_char(c) => {
                        visitor.visit_map(KeyedAccess::new(
                            TOKEN_KEY,
                            de::value::BorrowedStrDeserializer::new(
                                self.parser.parse_token()?.as_str(),
                            ),
                        ))
                    }
                    Some(b'-' | b'0'..=b'9') => match self.parser.parse_number()? {
                        Num::Integer(v) => visitor.visit_i64(v.into()),
                        Num::Decimal(v) => visitor.visit_f64(v.into()),
                    },
                    _ => Err(error::Repr::ExpectedStartOfBareItem(self.parser.index))?,
                }
            }

            fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                visitor.visit_some(self)
            }

            fn deserialize_newtype_struct<V>(
                self,
                _name: &'static str,
                visitor: V,
            ) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                visitor.visit_newtype_struct(self)
            }

            fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                visitor.visit_bool(self.parser.parse_bool()?)
            }

            fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.deserialize_byte_buf(visitor)
            }

            fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                visitor.visit_byte_buf(self.parser.parse_byte_sequence()?)
            }

            fn deserialize_enum<V>(
                self,
                _name: &'static str,
                _variants: &'static [&'static str],
                visitor: V,
            ) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                // Only token bare items can be directly deserialized into an enum.
                visitor.visit_enum(de::value::BorrowedStrDeserializer::new(
                    self.parser.parse_token()?.as_str(),
                ))
            }

            serde_core::forward_to_deserialize_any! {
                i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
                unit unit_struct seq tuple
                tuple_struct map struct identifier ignored_any
            }
        }

        Deserializer { parser: self }
    }

    fn parse_bool(&mut self) -> Result<bool, error::Repr> {
        // https://httpwg.org/specs/rfc9651.html#parse-boolean

        if self.peek() != Some(b'?') {
            return Err(error::Repr::ExpectedStartOfBoolean(self.index));
        }

        self.next();

        match self.peek() {
            Some(b'0') => {
                self.next();
                Ok(false)
            }
            Some(b'1') => {
                self.next();
                Ok(true)
            }
            _ => Err(error::Repr::ExpectedBoolean(self.index)),
        }
    }

    fn parse_string(&mut self) -> Result<Cow<'de, StringRef>, error::Repr> {
        // https://httpwg.org/specs/rfc9651.html#parse-string

        if self.peek() != Some(b'"') {
            return Err(error::Repr::ExpectedStartOfString(self.index));
        }

        self.next();

        let start = self.index;
        let mut output = Vec::new();

        while let Some(curr_char) = self.peek() {
            match curr_char {
                b'"' => {
                    let end = self.index;
                    self.next();
                    // TODO: The UTF-8 validation is redundant with the preceding character checks, but
                    // its removal is only possible with unsafe code.
                    return Ok(if output.is_empty() {
                        let slice = &self.input[start..end];
                        let output = std::str::from_utf8(slice).unwrap();
                        Cow::Borrowed(StringRef::from_validated_str(output))
                    } else {
                        let output = StdString::from_utf8(output).unwrap();
                        Cow::Owned(String::from_validated_string(output))
                    });
                }
                0x00..=0x1f | 0x7f..=0xff => {
                    return Err(error::Repr::InvalidStringCharacter(self.index));
                }
                b'\\' => {
                    let escape_index = self.index;
                    self.next();
                    match self.peek() {
                        Some(c @ (b'\\' | b'"')) => {
                            self.next();
                            if output.is_empty() {
                                output = self.input[start..escape_index].to_vec();
                            }
                            output.push(c);
                        }
                        None => return Err(error::Repr::UnterminatedEscapeSequence(self.index)),
                        Some(_) => return Err(error::Repr::InvalidEscapeSequence(self.index)),
                    }
                }
                _ => {
                    self.next();
                    if !output.is_empty() {
                        output.push(curr_char);
                    }
                }
            }
        }
        Err(error::Repr::UnterminatedString(self.index))
    }

    fn parse_non_empty_str(
        &mut self,
        is_allowed_start_char: impl FnOnce(u8) -> bool,
        is_allowed_inner_char: impl Fn(u8) -> bool,
    ) -> Option<&'de str> {
        let start = self.index;

        if !self.peek().is_some_and(is_allowed_start_char) {
            return None;
        }

        self.next();

        while self.peek().is_some_and(&is_allowed_inner_char) {
            self.next();
        }

        // TODO: The UTF-8 validation is redundant with the preceding character checks, but
        // its removal is only possible with unsafe code.
        Some(std::str::from_utf8(&self.input[start..self.index]).unwrap())
    }

    fn parse_token(&mut self) -> Result<&'de TokenRef, error::Repr> {
        // https://httpwg.org/specs/rfc9651.html#parse-token

        match self.parse_non_empty_str(
            utils::is_allowed_start_token_char,
            utils::is_allowed_inner_token_char,
        ) {
            None => Err(error::Repr::ExpectedStartOfToken(self.index)),
            Some(str) => Ok(TokenRef::from_validated_str(str)),
        }
    }

    fn parse_byte_sequence(&mut self) -> Result<Vec<u8>, error::Repr> {
        // https://httpwg.org/specs/rfc9651.html#parse-binary

        if self.peek() != Some(b':') {
            return Err(error::Repr::ExpectedStartOfByteSequence(self.index));
        }

        self.next();
        let start = self.index;

        loop {
            match self.next() {
                Some(b':') => break,
                Some(_) => {}
                None => return Err(error::Repr::UnterminatedByteSequence(self.index)),
            }
        }

        let colon_index = self.index - 1;

        match base64::Engine::decode(&utils::BASE64, &self.input[start..colon_index]) {
            Ok(content) => Ok(content),
            Err(err) => {
                let index = match err {
                    base64::DecodeError::InvalidByte(offset, _)
                    | base64::DecodeError::InvalidLastSymbol(offset, _) => start + offset,
                    // Report these two at the position of the last base64
                    // character, since they correspond to errors in the input
                    // as a whole.
                    base64::DecodeError::InvalidLength(_) | base64::DecodeError::InvalidPadding => {
                        colon_index - 1
                    }
                };

                Err(error::Repr::InvalidByteSequence(index))
            }
        }
    }

    fn parse_number(&mut self) -> Result<Num, error::Repr> {
        // https://httpwg.org/specs/rfc9651.html#parse-number

        fn char_to_i64(c: u8) -> i64 {
            i64::from(c - b'0')
        }

        let sign = if let Some(b'-') = self.peek() {
            self.next();
            -1
        } else {
            1
        };

        let mut magnitude = if let Some(c @ b'0'..=b'9') = self.peek() {
            self.next();
            char_to_i64(c)
        } else {
            return Err(error::Repr::ExpectedDigit(self.index));
        };

        let mut digits = 1;

        loop {
            match self.peek() {
                Some(b'.') => {
                    if digits > 12 {
                        return Err(error::Repr::TooManyDigitsBeforeDecimalPoint(self.index));
                    }
                    self.next();
                    break;
                }
                Some(c @ b'0'..=b'9') => {
                    digits += 1;
                    if digits > 15 {
                        return Err(error::Repr::TooManyDigits(self.index));
                    }
                    self.next();
                    magnitude = magnitude * 10 + char_to_i64(c);
                }
                _ => return Ok(Num::Integer(Integer::from_validated_i64(sign * magnitude))),
            }
        }

        magnitude *= 1000;
        let mut scale = 100;

        while let Some(c @ b'0'..=b'9') = self.peek() {
            if scale == 0 {
                return Err(error::Repr::TooManyDigitsAfterDecimalPoint(self.index));
            }

            self.next();
            magnitude += char_to_i64(c) * scale;
            scale /= 10;
        }

        if scale == 100 {
            // Report the error at the position of the decimal itself, rather
            // than the next position.
            Err(error::Repr::TrailingDecimalPoint(self.index - 1))
        } else {
            Ok(Num::Decimal(Decimal::from_integer_scaled_1000(
                Integer::from_validated_i64(sign * magnitude),
            )))
        }
    }

    fn parse_date(&mut self) -> Result<Date, error::Repr> {
        // https://httpwg.org/specs/rfc9651.html#parse-date

        if self.peek() != Some(b'@') {
            return Err(error::Repr::ExpectedStartOfDate(self.index));
        }

        match self.version {
            Version::Rfc8941 => return Err(error::Repr::Rfc8941Date(self.index)),
            Version::Rfc9651 => {}
        }

        let start = self.index;
        self.next();

        match self.parse_number()? {
            Num::Integer(seconds) => Ok(Date::from_unix_seconds(seconds)),
            Num::Decimal(_) => Err(error::Repr::NonIntegerDate(start)),
        }
    }

    fn parse_display_string(&mut self) -> Result<Cow<'de, str>, error::Repr> {
        // https://httpwg.org/specs/rfc9651.html#parse-display

        if self.peek() != Some(b'%') {
            return Err(error::Repr::ExpectedStartOfDisplayString(self.index));
        }

        match self.version {
            Version::Rfc8941 => return Err(error::Repr::Rfc8941DisplayString(self.index)),
            Version::Rfc9651 => {}
        }

        self.next();

        if self.peek() != Some(b'"') {
            return Err(error::Repr::ExpectedQuote(self.index));
        }

        self.next();

        let start = self.index;
        let mut output = Vec::new();

        while let Some(curr_char) = self.peek() {
            match curr_char {
                b'"' => {
                    let end = self.index;
                    self.next();
                    return if output.is_empty() {
                        let slice = &self.input[start..end];
                        match std::str::from_utf8(slice) {
                            Ok(output) => Ok(Cow::Borrowed(output)),
                            Err(err) => Err(error::Repr::InvalidUtf8InDisplayString(
                                start + err.valid_up_to(),
                            )),
                        }
                    } else {
                        match StdString::from_utf8(output) {
                            Ok(output) => Ok(Cow::Owned(output)),
                            Err(err) => Err(error::Repr::InvalidUtf8InDisplayString(
                                start + err.utf8_error().valid_up_to(),
                            )),
                        }
                    };
                }
                0x00..=0x1f | 0x7f..=0xff => {
                    return Err(error::Repr::InvalidDisplayStringCharacter(self.index));
                }
                b'%' => {
                    let escape_index = self.index;
                    self.next();

                    let mut octet = 0;

                    for _ in 0..2 {
                        octet = (octet << 4)
                            + match self.peek() {
                                Some(c @ b'0'..=b'9') => {
                                    self.next();
                                    c - b'0'
                                }
                                Some(c @ b'a'..=b'f') => {
                                    self.next();
                                    c - b'a' + 10
                                }
                                None => {
                                    return Err(error::Repr::UnterminatedEscapeSequence(self.index))
                                }
                                Some(_) => {
                                    return Err(error::Repr::InvalidEscapeSequence(self.index))
                                }
                            };
                    }

                    if output.is_empty() {
                        output = self.input[start..escape_index].to_vec();
                    }
                    output.push(octet);
                }
                _ => {
                    self.next();
                    if !output.is_empty() {
                        output.push(curr_char);
                    }
                }
            }
        }
        Err(error::Repr::UnterminatedDisplayString(self.index))
    }

    fn parameterized_deserializer<M>(
        &mut self,
    ) -> impl de::Deserializer<'de, Error = error::Error> + '_
    where
        M: MakeDeserializer,
    {
        // https://httpwg.org/specs/rfc9651.html#parse-param

        struct Deserializer<'a, 'de, M> {
            parser: &'a mut Parser<'de>,
            in_params: bool,
            marker: PhantomData<M>,
        }

        impl<'de, M> de::MapAccess<'de> for Deserializer<'_, 'de, M>
        where
            M: MakeDeserializer,
        {
            type Error = error::Error;

            fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
            where
                K: de::DeserializeSeed<'de>,
            {
                let key = if self.in_params {
                    let Some(b';') = self.parser.peek() else {
                        return Ok(None);
                    };

                    self.parser.next();
                    self.parser.consume_sp_chars();
                    // Note: It is up to the visitor to properly handle duplicate keys.
                    self.parser.parse_key()?.as_str()
                } else {
                    ""
                };

                seed.deserialize(de::value::BorrowedStrDeserializer::new(key))
                    .map(Some)
            }

            fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
            where
                V: de::DeserializeSeed<'de>,
            {
                if self.in_params {
                    if let Some(b'=') = self.parser.peek() {
                        self.parser.next();
                        seed.deserialize(self.parser.bare_item_deserializer())
                    } else {
                        seed.deserialize(de::value::BoolDeserializer::new(true))
                    }
                } else {
                    self.in_params = true;
                    seed.deserialize(M::make_deserializer(self.parser))
                }
            }
        }

        de::value::MapAccessDeserializer::new(Deserializer::<M> {
            parser: self,
            in_params: false,
            marker: PhantomData,
        })
    }

    fn parse_key(&mut self) -> Result<&'de KeyRef, error::Repr> {
        // https://httpwg.org/specs/rfc9651.html#parse-key

        match self.parse_non_empty_str(
            utils::is_allowed_start_key_char,
            utils::is_allowed_inner_key_char,
        ) {
            None => Err(error::Repr::ExpectedStartOfKey(self.index)),
            Some(str) => Ok(KeyRef::from_validated_str(str)),
        }
    }

    fn consume_ows_chars(&mut self) {
        while let Some(b' ' | b'\t') = self.peek() {
            self.next();
        }
    }

    fn consume_sp_chars(&mut self) {
        while let Some(b' ') = self.peek() {
            self.next();
        }
    }
}

const DATE_KEY: &str = "$sfv::private::Date";
const DISPLAY_STRING_KEY: &str = "$sfv::private::DisplayString";
const STRING_KEY: &str = "$sfv::private::String";
const TOKEN_KEY: &str = "$sfv::private::Token";

struct KeyedAccess<T> {
    key: &'static str,
    value: Option<T>,
}

impl<T> KeyedAccess<T> {
    fn new(key: &'static str, value: T) -> Self {
        Self {
            key,
            value: Some(value),
        }
    }
}

impl<'de, T> de::MapAccess<'de> for KeyedAccess<T>
where
    T: de::Deserializer<'de, Error = error::Error>,
{
    type Error = error::Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        match self.value {
            None => Ok(None),
            Some(_) => seed
                .deserialize(de::value::BorrowedStrDeserializer::new(self.key))
                .map(Some),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        seed.deserialize(self.value.take().unwrap())
    }
}

struct KeyedVisitor<T> {
    key: &'static str,
    expecting: &'static str,
    marker: PhantomData<fn() -> T>,
}

impl<T> KeyedVisitor<T> {
    fn new(key: &'static str, expecting: &'static str) -> Self {
        Self {
            key,
            expecting,
            marker: PhantomData,
        }
    }

    fn keyed_value<'de, A>(self, mut acc: A) -> Result<T, A::Error>
    where
        A: de::MapAccess<'de>,
        T: de::Deserialize<'de>,
    {
        if acc.next_key::<&str>()? == Some(self.key) {
            acc.next_value()
        } else {
            Err(<A::Error as de::Error>::invalid_type(
                de::Unexpected::Map,
                &self.expecting,
            ))
        }
    }
}

impl<'de> de::Visitor<'de> for KeyedVisitor<i64> {
    type Value = i64;

    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.expecting)
    }

    fn visit_map<A>(self, acc: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        self.keyed_value(acc)
    }
}

impl<'de> de::Visitor<'de> for KeyedVisitor<&'de str> {
    type Value = &'de str;

    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.expecting)
    }

    fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(v)
    }

    fn visit_map<A>(self, acc: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        self.keyed_value(acc)
    }
}

impl<'de> de::Visitor<'de> for KeyedVisitor<StdString> {
    type Value = StdString;

    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.expecting)
    }

    fn visit_string<E>(self, v: StdString) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(v)
    }

    fn visit_map<A>(self, acc: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        self.keyed_value(acc)
    }
}

impl<'de: 'a, 'a> de::Deserialize<'de> for &'a TokenRef {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer
            .deserialize_str(KeyedVisitor::<&str>::new(
                TOKEN_KEY,
                "a structured-header token",
            ))?
            .try_into()
            .map_err(de::Error::custom)
    }
}

impl<'de> de::Deserialize<'de> for Token {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer
            .deserialize_string(KeyedVisitor::<StdString>::new(
                TOKEN_KEY,
                "a structured-header token",
            ))?
            .try_into()
            .map_err(de::Error::custom)
    }
}

impl<'de: 'a, 'a> de::Deserialize<'de> for &'a StringRef {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer
            .deserialize_str(KeyedVisitor::<&str>::new(
                STRING_KEY,
                "a structured-header string",
            ))?
            .try_into()
            .map_err(de::Error::custom)
    }
}

impl<'de> de::Deserialize<'de> for String {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer
            .deserialize_string(KeyedVisitor::<StdString>::new(
                STRING_KEY,
                "a structured-header string",
            ))?
            .try_into()
            .map_err(de::Error::custom)
    }
}

impl<'de> de::Deserialize<'de> for Date {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        Ok(Date::from_unix_seconds(
            Integer::try_from(deserializer.deserialize_map(KeyedVisitor::<i64>::new(
                DATE_KEY,
                "a structured-header date",
            ))?)
            .map_err(de::Error::custom)?,
        ))
    }
}

impl<'de> de::Deserialize<'de> for Integer {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct Visitor;

        impl de::Visitor<'_> for Visitor {
            type Value = Integer;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str("a structured-header integer")
            }

            fn visit_i8<E>(self, v: i8) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(v.into())
            }

            fn visit_i16<E>(self, v: i16) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(v.into())
            }

            fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(v.into())
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                v.try_into().map_err(de::Error::custom)
            }

            fn visit_i128<E>(self, v: i128) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                v.try_into().map_err(de::Error::custom)
            }

            fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(v.into())
            }

            fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(v.into())
            }

            fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(v.into())
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                v.try_into().map_err(de::Error::custom)
            }

            fn visit_u128<E>(self, v: u128) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                v.try_into().map_err(de::Error::custom)
            }
        }

        deserializer.deserialize_i64(Visitor)
    }
}

impl<'de> de::Deserialize<'de> for Decimal {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct Visitor;

        impl de::Visitor<'_> for Visitor {
            type Value = Decimal;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str("a structured-header decimal")
            }

            fn visit_f32<E>(self, v: f32) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                v.try_into().map_err(de::Error::custom)
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                v.try_into().map_err(de::Error::custom)
            }
        }

        deserializer.deserialize_f64(Visitor)
    }
}

impl<'de: 'a, 'a> de::Deserialize<'de> for &'a KeyRef {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        <&str>::deserialize(deserializer)?
            .try_into()
            .map_err(de::Error::custom)
    }
}

impl<'de> de::Deserialize<'de> for Key {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        StdString::deserialize(deserializer)?
            .try_into()
            .map_err(de::Error::custom)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct FromDisplayString<T>(pub T);

impl<T> From<T> for FromDisplayString<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<'de: 'a, 'a> de::Deserialize<'de> for FromDisplayString<&'a str> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer
            .deserialize_str(KeyedVisitor::<&str>::new(
                DISPLAY_STRING_KEY,
                "a structured-header display string",
            ))
            .map(Self)
    }
}

impl<'de> de::Deserialize<'de> for FromDisplayString<StdString> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer
            .deserialize_string(KeyedVisitor::<StdString>::new(
                DISPLAY_STRING_KEY,
                "a structured-header display string",
            ))
            .map(Self)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct IgnoringParameters<T>(pub T);

impl<T> From<T> for IgnoringParameters<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<'de, T> de::Deserialize<'de> for IgnoringParameters<T>
where
    T: de::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct Visitor<T>(PhantomData<fn() -> T>);

        impl<'de, T> de::Visitor<'de> for Visitor<T>
        where
            T: de::Deserialize<'de>,
        {
            type Value = T;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str("a structured-header item")
            }

            fn visit_map<A>(self, mut acc: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let Some("") = acc.next_key()? else {
                    return Err(<A::Error as de::Error>::invalid_type(
                        de::Unexpected::Map,
                        &self,
                    ));
                };

                let value = acc.next_value()?;

                while acc.next_key::<de::IgnoredAny>()?.is_some() {
                    acc.next_value::<de::IgnoredAny>()?;
                }

                Ok(value)
            }
        }

        deserializer
            .deserialize_map(Visitor::<T>(PhantomData))
            .map(Self)
    }
}

#[test]
fn test_deserialize() {
    use serde::Deserialize;

    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(rename_all = "kebab-case")]
    enum Color {
        R,
        G,
        B,
    }

    #[derive(Debug, Default, PartialEq, Deserialize)]
    #[serde(rename_all = "kebab-case")]
    enum Dir {
        #[default]
        N,
        E,
        S,
        W,
    }

    #[derive(Debug, PartialEq, Deserialize)]
    struct Widget<'a> {
        #[serde(rename = "", borrow)]
        label: Cow<'a, StringRef>,
        #[serde(rename = "c")]
        color: Option<Color>,
        #[serde(rename = "d", default)]
        dir: Dir,
        #[serde(rename = "t", default)]
        time: Date,
    }

    let actual: Widget =
        Deserialize::deserialize(Parser::new(r#" "hello";d=w;c=g "#).into_item_deserializer())
            .unwrap();
    assert_eq!(
        actual,
        Widget {
            label: Cow::Borrowed(crate::string_ref("hello")),
            color: Some(Color::G),
            dir: Dir::W,
            time: Date::default(),
        }
    );
}
