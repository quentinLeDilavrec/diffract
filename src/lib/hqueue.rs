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
// if one is included with the Software (each a “Larger Work” to which the Software
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

use std::cmp::Ordering;
use std::fmt;

use ast::{Arena, NodeId};

#[derive(Copy, Clone, Eq, PartialEq)]
/// A `PriorityNodeId` wraps the height of a node with its id.
///
/// This type should be completely opaque to clients of this module.
/// Client code should construct a `HeightQueue` and call its methods,
/// which will return `NodeId`s directly, rather than the `PriorityNodeId`
/// wrapper.
struct PriorityNodeId {
    index: NodeId,
    height: u32,
}

impl PriorityNodeId {
    fn new(index: NodeId, height: u32) -> PriorityNodeId {
        PriorityNodeId {
            index: index,
            height: height,
        }
    }

    fn id(&self) -> NodeId {
        self.index
    }

    fn height(&self) -> u32 {
        self.height
    }
}

impl Ord for PriorityNodeId {
    fn cmp(&self, other: &PriorityNodeId) -> Ordering {
        self.height.cmp(&other.height)
    }
}

impl PartialOrd for PriorityNodeId {
    fn partial_cmp(&self, other: &PriorityNodeId) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Eq, PartialEq)]
/// A queue of `NodeId`s sorted on the height of their respective nodes.
pub struct HeightQueue {
    queue: Vec<PriorityNodeId>, // Use Vec so we can call `sort()`.
}

impl fmt::Debug for HeightQueue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[ ")?;
        for item in &self.queue {
            write!(f, "({:?}, {:?}) ", item.id(), item.height())?;
        }
        write!(f, "]")
    }
}

impl HeightQueue {
    /// Create empty priority queue.
    pub fn new() -> HeightQueue {
        HeightQueue { queue: Vec::new() }
    }

    /// Remove (and discard) all items in this queue, leaving it empty.
    pub fn clear(&mut self) {
        self.queue.clear();
    }

    /// `true` if this queue is empty, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    /// Get the id of the `Node` with the greatest height in the current queue.
    pub fn peek_max(&self) -> Option<NodeId> {
        if self.queue.is_empty() {
            return None;
        }
        return Some(self.queue[self.queue.len() - 1].index);
    }

    /// Remove information about the tallest node and return its `NodeId`.
    pub fn pop(&mut self) -> Option<NodeId> {
        if self.is_empty() {
            return None;
        }
        Some(self.queue.pop().unwrap().index)
    }

    /// Push a new node into this priority queue, keeping the queue sorted.
    ///
    /// This method has no effect if the new node is already in the queue.
    pub fn push<T: Clone>(&mut self, index: NodeId, arena: &Arena<T>) {
        let height = index.height(arena);
        let new_node = PriorityNodeId::new(index, height);
        if self.queue.contains(&new_node) {
            // Case 1: new node is already in the queue.
            return;
        } else if self.is_empty() || height <= self.queue[0].height() {
            // Case 2: new node is the shortest in the queue.
            self.queue.insert(0, new_node);
        } else if height >= self.queue[self.queue.len() - 1].height() {
            // Case 3: new node is the tallest in the queue.
            self.queue.push(new_node);
        } else {
            // Case 4: new node needs to be somewhere in the middle of the queue.
            for index in 0..self.queue.len() - 1 {
                if self.queue[index].height() <= height && self.queue[index + 1].height() > height {
                    self.queue.insert(index + 1, new_node);
                    return;
                }
            }
        }
    }

    /// Insert all the children of `parent` into this queue, keeping it sorted.
    pub fn open<T: Clone>(&mut self, parent: &NodeId, arena: Arena<T>) {
        let children = parent.children(&arena).collect::<Vec<NodeId>>();
        for child in children {
            self.push(child, &arena);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    fn create_arena() -> Arena<String> {
        let mut arena = Arena::new();
        let root = arena.new_node(String::from("+"), String::from("Expr"), 0);
        let n1 = arena.new_node(String::from("1"), String::from("INT"), 2);
        arena.make_child_of(n1, root).unwrap();
        let n2 = arena.new_node(String::from("*"), String::from("Expr"), 2);
        arena.make_child_of(n2, root).unwrap();
        let n3 = arena.new_node(String::from("3"), String::from("INT"), 4);
        arena.make_child_of(n3, n2).unwrap();
        let n4 = arena.new_node(String::from("4"), String::from("INT"), 4);
        arena.make_child_of(n4, n2).unwrap();
        let format1 = "Expr +
  INT 1
  Expr *
    INT 3
    INT 4
";
        assert_eq!(format1, format!("{}", arena));
        arena
    }

    // Assert that `queue` is in sorted order and has the same size `arena`.
    fn assert_sorted<T: Clone>(queue: &HeightQueue, arena: &Arena<T>) {
        let mut expected = arena.size();
        if expected == 0 {
            assert!(queue.is_empty());
            return;
        }
        let mut clone = queue.clone();
        let mut tallest = clone.pop().unwrap();
        expected -= 1;
        while !clone.is_empty() {
            assert!(expected > 0);
            assert!(tallest.height(arena) >= clone.peek_max().unwrap().height(arena));
            tallest = clone.pop().unwrap();
            expected -= 1;
        }
        assert_eq!(0, expected);
    }

    #[test]
    fn clear() {
        let arena = create_arena();
        let mut queue = arena.get_priority_queue();
        assert!(!queue.is_empty());
        queue.clear();
        assert!(queue.is_empty());
    }

    #[test]
    fn cmp_priority_node() {
        let p0 = PriorityNodeId::new(NodeId::new(0), 0);
        let p1 = PriorityNodeId::new(NodeId::new(0), 1);
        let p2 = PriorityNodeId::new(NodeId::new(0), 2);
        let p3 = PriorityNodeId::new(NodeId::new(0), 2);
        assert!(p0 < p1);
        assert!(p1 < p2);
        assert!(p2 == p3);
        assert!(p0 < p3);
        assert!(p3 > p1);
        assert!(p2 > p1);
        assert!(p3 > p0);
    }

    #[test]
    fn fmt_debug() {
        let arena = create_arena();
        let queue = arena.get_priority_queue();
        let s = format!("{:?}", queue);
        // Three leaves in this arena can be placed in the queue in any order,
        // so we don't check the whole string, we just check the start of the
        // formatted string and the branch nodes at the end.
        let expected = " (NodeId { index: 2 }, 2) (NodeId { index: 0 }, 3) ]";
        assert_eq!("[ (NodeId { index:", s[..18].to_string());
        assert_eq!(expected, s[76..].to_string());
        assert_eq!(128, s.len());
    }

    #[test]
    fn new() {
        assert!(HeightQueue::new().is_empty());
    }

    #[test]
    fn open() {
        let arena = create_arena();
        let mut queue = HeightQueue::new();
        queue.open(&NodeId::new(0), arena);
        assert_eq!(NodeId::new(2), queue.pop().unwrap()); // Expr *
        assert_eq!(NodeId::new(1), queue.pop().unwrap()); // INT 1
    }

    #[test]
    fn peek_max() {
        let arena = create_arena();
        let queue = arena.get_priority_queue();
        let root = queue.peek_max().unwrap();
        assert_eq!(0, root.id());
        assert_eq!(3, root.height(&arena));
    }

    #[test]
    fn pop() {
        let arena = create_arena();
        let mut queue = arena.get_priority_queue();
        assert_eq!(0, queue.pop().unwrap().id());
        assert_eq!(2, queue.pop().unwrap().id());
        // Nodes 1, 3, 4 have the same height, and so may be stored in any order.
        let leaves = vec![1, 3, 4];
        assert!(leaves.contains(&queue.pop().unwrap().id()));
        assert!(leaves.contains(&queue.pop().unwrap().id()));
        assert!(leaves.contains(&queue.pop().unwrap().id()));
    }

    #[test]
    fn push() {
        let arena = create_arena();
        let queue = arena.get_priority_queue();
        assert_sorted(&queue, &arena);
    }

    #[test]
    fn push_identical_nodes() {
        let arena = create_arena();
        let mut queue = HeightQueue::new();
        queue.push(NodeId::new(0), &arena);
        let formatted = format!("{:?}", queue);
        let expected = "[ (NodeId { index: 0 }, 3) ]";
        assert_eq!(expected, formatted);
        queue.push(NodeId::new(0), &arena); // Should have no effect.
        assert_eq!(expected, formatted);
    }

    const BENCH_ITER: usize = 10000;

    #[bench]
    fn bench_push(bencher: &mut Bencher) {
        let mut arena: Arena<String> = Arena::new();
        for _ in 0..BENCH_ITER {
            arena.new_node(String::from(""), String::from(""), 0);
        }
        let mut queue = HeightQueue::new();
        // Because `HeightQueues` are sets, each iteration of this
        // microbenchmark must push a distinct `NodeId` to the queue, to avoid
        // the optimisation that does not attempt to push an existing value to
        // the structure.
        bencher.iter(|| for id in 0..BENCH_ITER {
                         queue.push(NodeId::new(id), &arena);
                         queue.clear();
                     });
    }
}
