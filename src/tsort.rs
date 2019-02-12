use std::collections::VecDeque;
use std::fmt;
use std::error::Error;
use std::hash::Hash;
use std::result::Result;
use std::vec::Vec;
use std::collections::{HashMap, HashSet};


#[derive(Debug)]
enum ErrorKind<'a, Node> {
    UnknownCycle,
    Cycle(Vec<&'a Node>)
}


#[derive(Debug)]
pub(crate) struct SortingError<'a, Node> {
    kind: ErrorKind<'a, Node>
}


impl<'a, N> Error for SortingError<'a, N> where N: fmt::Debug + fmt::Display { }


impl<'a, N> fmt::Display for SortingError<'a, N> where N: fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match &self.kind {
            ErrorKind::UnknownCycle => write!(f, "there is at least one cycle"),
            ErrorKind::Cycle(path) => {
                write!(f, "there is at least one cycle [")?;
                for (i, n) in path.iter().enumerate() {
                    if i != path.len() {
                        write!(f, "{}->", n)?;
                    } else {
                        write!(f, "{}", n)?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}


impl<'a, N> From<ErrorKind<'a, N>> for SortingError<'a, N> {
    fn from(error: ErrorKind<'a, N>) -> Self {
        SortingError { kind: error }
    }
}

/// Topological sort of a directed graph.
///
/// The graph is defined as a list of edges.
/// The function return an error if the graph contains a cycle.
pub(crate) fn topological_sort<Node>(edges: &[(Node, Node)]) -> Result<Vec<&Node>, SortingError<Node>>
where
    Node: Clone + Hash + Eq + fmt::Debug + fmt::Display,
{

    // It is easier to work with st and ts than directly with edges list.
    let mut st: HashMap<&Node, HashSet<&Node>> = HashMap::new();
    let mut ts: HashMap<&Node, HashSet<&Node>> = HashMap::new();

    for (frm, to) in edges {
        st.entry(frm)
            .and_modify(|set| {
                set.insert(to);
            })
            .or_insert_with(|| {
                let mut set = HashSet::new();
                set.insert(to);
                set
            });

        ts.entry(to)
            .and_modify(|set| {
                set.insert(frm);
            })
            .or_insert_with(|| {
                let mut set = HashSet::new();
                set.insert(frm);
                set
            });
    }

    let mut zero_in_degs: HashSet<&Node> = {
        let st_keys: HashSet<&Node> = st.keys().cloned().collect();
        let ts_keys: HashSet<&Node> = ts.keys().cloned().collect();
        st_keys.difference(&ts_keys).map(|&x| x).collect()
    };

    // topologically sorted list of nodes
    let mut sorted: Vec<&Node> = Vec::new();

    while let Some(frm) = zero_in_degs.iter().find_map(|x| Some(x.clone())) {

        zero_in_degs.remove(frm);
        sorted.push(frm);

        if let Some(st_set) = st.get(frm).map(|set| set.clone()) {
            for to in st_set {
                st.entry(frm).and_modify(|x| {
                    x.remove(to);
                });

                ts.entry(to).and_modify(|x| {
                    x.remove(frm);
                });

                // the to-node does not have incoming edges, thus is can be added to zero_in_degree set
                let to_is_zero_in_degree = ts.get(&to).map_or(true, |x| x.is_empty());

                if to_is_zero_in_degree {
                    zero_in_degs.insert(to);
                }
            }
        }
    }

    let graph_non_empty = st.values().any(|x| !x.is_empty());
    if graph_non_empty {

        let mut path: Vec<&Node> = Vec::new();
        let mut list: VecDeque<&&Node> = st.keys().collect();

        loop {
            match list.pop_front() {
                Some(n) => {
                    if path.contains(n) {
                        return Err(ErrorKind::Cycle(path).into())
                    } else {
                        path.push(n);

                        if let Some(set) = st.get(n) {
                            for i in set {
                                list.push_front(i);
                            }
                        }
                    }
                },
                None => {
                    return Err(ErrorKind::UnknownCycle.into())
                }
            }
        }
    } else {
        Ok(sorted)
    }
}


#[cfg(test)]
mod tests {

    use super::*;


    #[test]
    fn empty_graph() {
        let edges: Vec<(char, char)> = Vec::new();
        let sorted = topological_sort(&edges[..]);

        assert!(sorted.is_ok());
    }

    #[test]
    fn no_cycles_single_root() {
        let edges = vec![
            ('a', 'b'),
            ('b', 'c'),
            ('c', 'd')
        ];
        let sorted = topological_sort(&edges[..]);
        assert!(sorted.is_ok(), "must topologically sort");

        let nodes: Vec<char> = sorted.unwrap().iter().map(|c| *c.clone()).collect();
        assert_eq!(nodes, vec!['a', 'b', 'c', 'd']);
    }

    #[test]
    fn no_cycles_multiple_root() {
        let edges = vec![
            ('c', 'd'),
            ('a', 'b'),
            ('b', 'd')
        ];
        let sorted = topological_sort(&edges[..]);
        assert!(sorted.is_ok(), "must topologically sort");

        let nodes: Vec<char> = sorted.unwrap().iter().map(|c| *c.clone()).collect();

        assert!(
            nodes == vec!['c', 'a', 'b', 'd'] ||
                nodes == vec!['a', 'c', 'b', 'd'] ||
                nodes == vec!['a', 'b', 'c', 'd']
        );
    }

    #[test]
    fn detect_cycles() {
        let edges = vec![
            ('a', 'b'),
            ('b', 'c'),
            ('c', 'd'),
            ('d', 'a')
        ];
        let sorted = topological_sort(&edges[..]);
        assert!(sorted.is_err());
    }
}
