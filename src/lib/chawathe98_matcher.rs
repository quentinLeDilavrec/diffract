// Copyright (c) 2017 King's College London
// created by the Software Development Team <http://soft-dev.org/>
//
// The Universal Permissive License (UPL), Version 1.0
//
// Subject to the condition set forth below, permission is hereby granted to any
// person obtaining a copy of this software, associated documentation and/or
// data (collectively the "Software"), free of charge and under any and all
// copyright rights in the Software, and any and all patent rights owned or
// freely licensable by each licensor hereunder covering either (i) the
// unmodified Software as contributed to or provided by such licensor, or (ii)
// the Larger Works (as defined below), to deal in both
//
// (a) the Software, and
// (b) any piece of software and/or hardware listed in the lrgrwrks.txt file
// if one is included with the Software (each a "Larger Work" to which the Software
// is contributed by such licensors),
//
// without restriction, including without limitation the rights to copy, create
// derivative works of, display, perform, and distribute the Software and make,
// use, sell, offer for sale, import, export, have made, and have sold the
// Software and the Larger Work(s), and to sublicense the foregoing rights on
// either these or other terms.
//
// This license is subject to the following condition: The above copyright
// notice and either this complete permission notice or at a minimum a reference
// to the UPL must be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#![warn(missing_docs)]

///
/// This matcher is based on the paper Meaningful Change Detection in Structured Data.
/// Written by Sudarshan S. Chawathe and Hector Garcia-Molina.
/// This matcher would use the additional operation which are GLUE and COPY.
/// In addition to Insert, Delete, Move, Update.
/// This is supposed to be an alternative matcher to chawathe96.

use std::fmt::Debug;

use ast::Arena;
use matchers::{MappingStore, MatchTrees};

/// The Chawathe98Config matcher needs no configuration.
#[derive(Debug, Clone, PartialEq)]
pub struct Chawathe98Config  {}

impl Default for Chawathe98Config  {
    fn default() -> Chawathe98Config  {
        Chawathe98Config  {}
    }
}

impl Chawathe98Config  {
    /// Create a new configuration object, with default values.
    pub fn new() -> Chawathe98Config  {
        Default::default()
    }
}

impl<T: Clone + Debug + Eq + 'static> MatchTrees<T> for Chawathe98Config  {
    /// Describe this matcher for the user.
    fn describe(&self) -> String {
        let desc = "This matcher performs no matching operations.";
        String::from(desc)
    }

    /// Perform no matching operations.
    fn match_trees(&self, base: Arena<T>, diff: Arena<T>) -> MappingStore<T> {
        MappingStore::new(base, diff)
    }
}