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

use std::cmp::max;
use std::collections::HashSet;
use std::fmt::Debug;

use action::{ApplyAction, Delete, EditScript, Insert, Move, Update};
use ast::{ArenaError, NodeId};
use matchers::{EditScriptResult, MappingStore, TemporaryMappingStore, MappingType};


/// Given a matching between two ASTs, generate a complete edit script.
///
/// This trait should usually be implemented on configuration objects that
/// define thresholds and weights for a given algorithm.
pub trait EditScriptGenerator<T: Clone + Debug + Eq + 'static> {
    /// Given a matching between two ASTs, generate a complete edit script.
    fn generate_script(&self, store: &mut MappingStore<T>) -> EditScriptResult<T>;
}


#[derive(Debug, Clone, PartialEq)]
/// The Chawathe et al. (1996) algorithm does not require any configuration.
pub struct Chawathe98Config {}

impl Default for Chawathe98Config {
    fn default() -> Chawathe98Config {
        Chawathe98Config {}
    }
}
impl Chawathe98Config {
    /// Create a new configuration object, with default values.
    pub fn new() -> Chawathe98Config {
        Default::default()
    }
}


impl<T: Clone + Debug + Eq + 'static> EditScriptGenerator<T> for Chawathe98Config {
    /// This function should generate the edit script for GLUE and COPY
    fn generate_script(&self, _store: &mut MappingStore<T>) -> EditScriptResult<T> {
        Ok(EditScript::new())
    }
}

#[derive(Debug, Clone, PartialEq)]
/// The Chawathe et al. (1996) algorithm does not require any configuration.
pub struct Chawathe96Config {}

impl Default for Chawathe96Config {
    fn default() -> Chawathe96Config {
        Chawathe96Config {}
    }
}

impl Chawathe96Config {
    /// Create a new configuration object, with default values.
    pub fn new() -> Chawathe96Config {
        Default::default()
    }

    fn align_children<T: Clone + Debug + Eq + 'static>(
        &self,
        store: &mut MappingStore<T>,
        w: NodeId,
        x: NodeId,
        script: &mut EditScript<T>,
        new_mappings: &TemporaryMappingStore,
        from_in_order: &mut HashSet<NodeId>,
        to_in_order: &mut HashSet<NodeId>,
    ) -> Result<(), ArenaError> {
        // Variable key:
        // a, w in store.from_arena (T_1 in paper).
        // b, x in store.to_arena (T_2 in paper).
        debug!("align_children({}, {})", w, x);
        if w.is_leaf(&store.from_arena) && x.is_leaf(&store.to_arena) {
            debug!("{} and {} both leaf nodes.", w, x);
            return Ok(());
        }
        // 1. Mark all children of w and all children of x "out of order".
        for child in w.children(&store.from_arena) {
            from_in_order.remove(&child);
        }
        for child in x.children(&store.to_arena) {
            to_in_order.remove(&child);
        }
        // 2. Let s1 be the sequence of children of w whose partners are
        // children of x and let s2 be the sequence of children of x whose
        // partners are children of w.
        let mut s1: Vec<NodeId> = vec![];
        for child in w.children(&store.from_arena) {
            if new_mappings.contains_from(&child) {
                let mapped = new_mappings.get_to(&child).unwrap();
                if x.children(&store.to_arena).any(|n| n == mapped) {
                    s1.push(child);
                }
            }
        }
        let mut s2: Vec<NodeId> = vec![];
        for child in x.children(&store.to_arena) {
            if new_mappings.contains_to(&child) {
                let mapped = new_mappings.get_from(&child).unwrap();
                if w.children(&store.from_arena).any(|n| n == mapped) {
                    s2.push(child);
                }
            }
        }
        // 3. Let equal(a, b) iff (a, b) in store.
        // 4. Let lcs be lcss(s1, s2, equal).
        let lcs = self.lcss(new_mappings, &s1, &s2);
        // 5. For each (a, b) in lcs mark nodes a and b "in order".
        for &(a, b) in &lcs {
            from_in_order.insert(a);
            to_in_order.insert(b);
        }
        // 6. For each a in s1, b in s2 such that (a, b) in new_mappings but
        // (a, b) not in lcs: (i) let k = find_pos(b); (ii) append MOV(a, w, k)
        // to the script and apply MOV(a, w, k) to T_1; (iii) Mark a, b as "in
        // order".
        for a in &s1 {
            for b in &s2 {
                if store.is_mapped(a, b) && !lcs.contains(&(*a, *b)) {
                    let k = self.find_pos(store, *b, new_mappings, from_in_order, to_in_order);
                    let mut mov = Move::new(*a, w, k);
                    debug!(
                        "Edit script align_children: MOV {} {:?} Parent: {} {:?} Child: {}",
                        store.from_arena[*a].label,
                        store.from_arena[*a].value,
                        store.to_arena[w].label,
                        store.to_arena[w].value,
                        k
                    );
                    script.push(mov);
                    mov.apply(&mut store.from_arena)?;
                    from_in_order.insert(*a);
                    to_in_order.insert(*b);
                }
            }
        }
        Ok(())
    }

    /// Find the position of node x in to_arena.
    fn find_pos<T: Clone + Debug + Eq + 'static>(
        &self,
        store: &MappingStore<T>,
        x: NodeId,
        new_mappings: &TemporaryMappingStore,
        from_in_order: &HashSet<NodeId>,
        to_in_order: &HashSet<NodeId>,
    ) -> u16 {
        // 1. Let y=p(x) in T_2 and let w be the partner of x in T_1.
        // N.B. w seems to be unused in the algorithm.
        let y = store.to_arena[x].parent().unwrap();
        // 2. If x is the leftmost child of y that is marked "in order" return
        // 0 (the paper says 1 but we count child nodes from 0).
        let siblings = y.children(&store.to_arena).collect::<Vec<NodeId>>();
        for child in &siblings {
            if to_in_order.contains(child) {
                if x == *child {
                    debug!("find_pos({}) <- 0. Node in order.", x);
                    return 0;
                } else {
                    break;
                }
            }
        }
        let x_pos = x.get_child_position(&store.to_arena).unwrap();
        // 3. Find v in T_2, where v is the rightmost sibling of x that is to
        // the left of x and is marked "in order".
        let v = x.children(&store.to_arena)
                 .take(x_pos)
                 .filter(|c| to_in_order.contains(&c))
                 .last();
        if v.is_none() {
            debug!("find_pos({}) <- 0. No right-most sibling in order.", x);
            return 0;
        }
        // 4. Let u be the partner of v in T_1.
        let u_opt = new_mappings.get_from(&v.unwrap());
        assert!(u_opt.is_some(), "find_pos() could not find a partner for v");
        let u = u_opt.unwrap();
        // 5. Suppose u is the i'th child of its parent that is marked
        // "in order". Return i + 1.
        let u_p = store.from_arena[u].parent().unwrap();
        let u_pos = u.get_child_position(&store.from_arena).unwrap();
        let ret = u_p.children(&store.from_arena)
                     .take(u_pos)
                     .filter(|c| from_in_order.contains(&c))
                     .count() as u16 + 1;
        debug!("find_pos({}) <- {}", x, ret);
        ret
    }

    fn lcss(
        &self,
        store: &TemporaryMappingStore,
        seq1: &[NodeId],
        seq2: &[NodeId],
    ) -> Vec<(NodeId, NodeId)> {
        let mut lcss: Vec<(NodeId, NodeId)> = vec![];
        if seq1.is_empty() || seq2.is_empty() {
            return lcss;
        }
        let mut grid = vec![];
        for _ in 0..seq1.len() + 1 {
            grid.push(vec![0; seq2.len() + 1]);
        }
        debug_assert_eq!(seq1.len() + 1, grid.len());
        debug_assert_eq!(seq2.len() + 1, grid[0].len());
        for (i, n1) in seq1.iter().enumerate() {
            for (j, n2) in seq2.iter().enumerate() {
                if store.is_mapped(n1, n2) {
                    grid[i + 1][j + 1] = 1 + grid[i][j];
                } else {
                    grid[i + 1][j + 1] = max(grid[i + 1][j], grid[i][j + 1]);
                }
            }
        }
        let mut i = seq1.len();
        let mut j = seq2.len();
        while i != 0 && j != 0 {
            if grid[i][j] == grid[i - 1][j] {
                i -= 1;
            } else if grid[i][j] == grid[i][j - 1] {
                j -= 1;
            } else {
                lcss.push((seq1[i - 1], seq2[j - 1]));
                i -= 1;
                j -= 1;
            }
        }
        lcss.reverse();
        lcss
    }
}

impl<T: Clone + Debug + Eq + 'static> EditScriptGenerator<T> for Chawathe96Config {
    /// This function implements the optimal algorithm of Chawathe et al. (1996).
    /// Variable names as in Figures 8 and 9 of the paper.
    fn generate_script(&self, store: &mut MappingStore<T>) -> EditScriptResult<T> {
        // 1. E <- e, M' <- M.
        let mut script: EditScript<T> = EditScript::new();
        let mut from_in_order: HashSet<NodeId> = HashSet::new();
        let mut to_in_order: HashSet<NodeId> = HashSet::new();
        let mut new_mappings = TemporaryMappingStore::new();
        // Copy current mappings over to new_mappings.
        for (key, value) in &store.from {
            new_mappings.push(*key, value.0);
        }
        // Combined update, insert, align and move phases.
        // 2. Visit the nodes of T_2 in breadth-first order.
        let tmp_to_arena = store.to_arena.clone();
        let root = tmp_to_arena.root().unwrap_or_else(|| NodeId::new(0));
        // (a) Let x be the current node in the breadth-first search of T_2
        // and let y be the parent of x. Let z be the partner of y in M'.
        for x in root.breadth_first_traversal(&tmp_to_arena) {
            let mut w = root; // Overwritten later.
            // Insertion phase.
            if !new_mappings.contains_to(&x) && x.is_root(&store.to_arena) {
                // Handle root nodes separately. This branch is most likely to
                // be used when the "from" and "to" ASTs have been parsed
                // into different grammars.
                let k = self.find_pos(store, x, &new_mappings, &from_in_order, &to_in_order);
                debug!(
                    "Edit script: INS {} {:?} No parent",
                    store.to_arena[x].label,
                    store.to_arena[x].value
                );
                w = store.from_arena.new_node(
                    store.to_arena[x].value.clone(),
                    store.to_arena[x].label.clone(),
                    store.to_arena[x].indent,
                    store.to_arena[x].col_no,
                    store.to_arena[x].line_no,
                    store.to_arena[x].char_no,
                    store.to_arena[x].token_len,
                );
                new_mappings.push(w, x);
                let mut ins = Insert::new(w, None, k);
                ins.apply(&mut store.from_arena)?;
                script.push(ins);
            } else if !new_mappings.contains_to(&x) {
                let y = store.to_arena[x].parent().unwrap();
                let z = new_mappings.get_from(&y).unwrap();
                // (b) if x has no partner in M': i. let k<-find_pos(x),
                let k = self.find_pos(store, x, &new_mappings, &from_in_order, &to_in_order);
                debug!(
                    "Edit script: INS {} {:?} Parent: {} {:?}",
                    store.to_arena[x].label,
                    store.to_arena[x].value,
                    store.from_arena[z].label,
                    store.from_arena[z].value
                );
                w = store.from_arena.new_node(
                    store.to_arena[x].value.clone(),
                    store.to_arena[x].label.clone(),
                    store.to_arena[x].indent,
                    store.to_arena[x].col_no,
                    store.to_arena[x].line_no,
                    store.to_arena[x].char_no,
                    store.to_arena[x].token_len,
                );
                // iii. Add (w, x) to M' and apply INS((w, a, v(x)), z, k) to T_1.
                new_mappings.push(w, x);
                // ii. Append INS((w, a, v(x)), z, k) to E for new identifier w
                let mut ins = Insert::new(w, Some(z), k);
                ins.apply(&mut store.from_arena)?;
                script.push(ins);
            } else if !x.is_root(&store.to_arena) {
                // Insertion and update phases.
                // (c) else if x is not a root (x has a partner in M').
                // i. Let w be the partner of x in M' and let v be the parent
                // of w in T_1.
                w = new_mappings.get_from(&x).unwrap();
                let v = store.from_arena[w].parent().unwrap();
                // ii. if value_of(w) != value_of(x):
                if store.from_arena[w].value != store.to_arena[x].value {
                    debug!(
                        "Edit script: UPD {} {:?} -> {} {:?}",
                        store.from_arena[w].label,
                        store.from_arena[w].value,
                        store.to_arena[x].label,
                        store.to_arena[x].value
                    );
                    let mut upd = Update::new(
                        w,
                        store.to_arena[x].value.clone(),
                        store.to_arena[x].label.clone(),
                    );
                    // B. Apply UPD(w, v(x)) to T_1.
                    upd.apply(&mut store.from_arena)?;
                    // A. Append UPD(w, v(x)) to E.
                    script.push(upd.clone());
                }
                // MOVE phase.
                // iii. If (y, v) not in M':
                let y = store.to_arena[x].parent().unwrap();
                // A. Let z be the partner of y in M'.
                let z = new_mappings.get_from(&y).unwrap();
                if z != v {
                    // B. Let k<-find_pos(x).
                    let k = self.find_pos(store, x, &new_mappings, &from_in_order, &to_in_order);
                    let mut mov = Move::new(w, z, k);
                    debug!(
                        "Edit script: MOV {} {:?} Parent: {} {:?} Child: {}",
                        store.from_arena[w].label,
                        store.from_arena[w].value,
                        store.from_arena[z].label,
                        store.from_arena[z].value,
                        k
                    );
                    // D. Apply MOV(w, z, k) to T_1.
                    mov.apply(&mut store.from_arena)?;
                    // C. Append MOV(w, z, k) to E.
                    script.push(mov);
                }
            }
            // (d) align_children(w, x).
            self.align_children(
                store,
                w,
                x,
                &mut script,
                &new_mappings,
                &mut from_in_order,
                &mut to_in_order,
            )?;
        }
        // Delete phase.
        // 3. Perform a post-order traversal of T_1.
        let mut actions = EditScript::new();
        // (a) Let w be the current node in the post-order traversal of T_1.
        for w in root.post_order_traversal(&store.from_arena) {
            // (b) If w has no partner in M' then append DEL(w) to E and apply
            // DEL(w) to T_1.
            if !new_mappings.contains_from(&w) {
                debug!(
                    "Edit script: DEL {} {:?}",
                    store.from_arena[w].label,
                    store.from_arena[w].value
                );
                let del = Delete::new(w);
                script.push(del);
                actions.push(del);
            }
        }
        actions.apply(&mut store.from_arena)?;
        // Add new mappings to the existing store.
        debug!("Append new mappings to store.");
        for (from, to) in new_mappings.from {
            if !store.contains_from(&from) {
                debug!("New mapping due to edit script: {} -> {}", from, to);
                store.push(from, to, MappingType::EDIT);
            }
        }
        Ok(script)
    }
}
