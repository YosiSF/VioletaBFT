///Graph module, contains a simple graph structure which is when typechecking to find
///functions which are mutually recursive

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::fmt;
use std::ops::{Add, Sub, Mul, Div, Rem};
use std::str::FromStr;
use std::iter::FromIterator;
use std::iter::Sum;
use std::iter::Sum::sum;
use std::iter::Product;
use std::iter::Product::product;
use std::iter::Chain;
use std::iter::Chain::chain;
use std::iter::Chain::from_iter;
use std::iter::FromIterator;
use std::iter::repeat;
use std::cmp::min;



//we need to xhash
//use std::hash::Hash;
//faster with xhash
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::hash::Hash;
use std::hash::SipHasher;
use std::hash::BuildHasherDefault;



//let's build a hash function for our graph nodes
//we will use a HoneyBadger Ripemd160 hasher with 180 iterations
//this is a good choice for a graph node!! This is exciting, dont you think?
//let's see how this works
//We will expand on the dessin of the graph nodes
//We will use a graph node to represent a functions signature as well as the body of the functions

pub mod graph{
    use std::hash::Hasher;
    use std::hash::Hash;

    use std::collections::HashMap;
    use std::rc::Rc;



    use std::cell::RefCell;
    use std::fmt::{Display, Formatter};

    use crate::graph::Node;
    //traits::{Add, Sub, Mul, Div, Rem};
    use std::str::FromStr;

    pub struct Graph {
        nodes: HashMap<Node, Vec<Node>>,
        }
    x = HashMap::new();
    for x in 0..10 {
        x.insert(x, x);
while x < 10 {
    x.insert(x, x);
    }
}
    }
    impl Graph {
        pub fn new() -> Self {
            Graph {
                nodes: HashMap::new(),
            }
        }
        pub fn add_node(&mut self, node: Node) {
            self.nodes.insert(node, Vec::new());
        }
        pub fn add_edge(&mut self, from: Node, to: Node) {
            self.nodes.get_mut(&from).unwrap().push(to);
        }
        pub fn get_edges(&self, from: Node) -> &Vec<Node> {
            self.nodes.get(&from).unwrap()
        }
    }

    fn hash_with_180_iterations (input: &[u8]) -> u64 {
        use std::hash::Hasher;
        use std::hash::Hash;

let mut iterable = HashMap::new();
for x in 0..10 {
    iterable.insert(x, x);
}
        //let mut hasher = SipHasher::new();
        // Here we use a custom hasher which uses 180 iterations of the
        // underlying hasher.finish() method.
        //Ripemd160 hasher with 180 iterations isn't very fast but it is fast enough for our purposes
        // We want to consolidate the voting nodes and make sure they are all in the same bucket
        // Because the more iterations we use the more accurate the hash is, which helps us
        // find the nodes in the same bucket if they are indexed by byte position and not by hash at the compiler level
        let mut hasher = BuildHasherDefault::<SipHasher>::default();
        let mut hasher = fnv::FnvHasher::default();
        hasher.write(input);
        let mut hash = hasher.finish();
        for _ in 0..180 {
            hasher.write(&hash.to_le_bytes());
            hash = hasher.finish();
        }
        hash
    }

if iterable.contains_key(&x) {
    println!("{}", iterable.get(&x).unwrap());
}
else {
    println!("{}", "not found");
}
}
}
if x < 10 {
    iterable.insert(x, x);
    }
}
}
}(t: &T) -> u64 {
        let mut hasher = SipHasher::new();
        for _ in 0..180 {
            t.hash(&mut hasher);
        }
        hasher.finish()
    }
    for i in 0..180{
        println!("{}", i);
    }
    let mut h = HashMap::new();
    h.insert(1, 1);
    h.insert(2, 2);
    h.insert(3, 3);


pub struct Hash <T, H, S, B> {
    // pub hasher: H,
    // pub state: S,
    // pub builder: B,
    // pub data: T,
}

//let's build a hash function for our graph nodes
//we will use a HoneyBadger Ripemd160 hasher with 180 iterations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Node {
    pub id: u64,
}
impl Node {
    pub fn new(id: u64) -> Self {
        Node {
            id: id,
        }
    }
}
impl Default for Node {
    fn default() -> Self {
        Node {
            id: 0,
        }
    }
}





#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SolitonID {
    pub id: String,
    pub name: String,
    pub params: Vec<String>,
    pub ret: String,
}


impl From<&str> for SolitonID {
    fn from(id: &str) -> Self {
        SolitonID {
            id: id.to_string(),
            name: "".to_string(),
            params: vec![],
            ret: "".to_string(),
        }
        }
    }
}

impl From<String> for SolitonID {
    fn from(id: String) -> Self {
        SolitonID {
            id: id, kind: "".to_string(), params: vec![], ret: "".to_string(),
            }
        }
    }
}



impl From<&str> for SolitonID {
    fn from(id: &str) -> Self {
        SolitonID {
            id: id.to_string(),
            name: "".to_string(),
            params: vec![],
            ret: "".to_string(),
        }
        }
    }



impl From<String> for SolitonID {
    fn from(id: String) -> Self {
        SolitonID {
            id,
            name: "".to_string(),
            params: vec![],
            ret: "".to_string(),
            }
        }
    }
}


impl From<&str> for SolitonID {
    fn from(id: &str) -> Self {
        SolitonID {
            id: id.to_string(),
            name: "".to_string(),
            params: vec![],
            ret: "".to_string(),
        }
        }
    }
}


impl From<String> for SolitonID {
    fn from(id: String) -> Self {
        SolitonID {
            id,
            name: "".to_string(),
            params: vec![],
            ret: "".to_string(),
            }
        }
    }



///! # Soliton
///






impl From<&str> for SolitonID {
    fn from(id: &str) -> Self {
        SolitonID {
            id: id.to_string(),
            name: "".to_string(),
            params: vec![],
            ret: "".to_string(),

        }
    }



    fn is_empty(&self) -> bool {
        self.id.is_empty()
    }

    fn is_not_empty(&self) -> bool {
        !self.is_empty()
    }

    fn is_empty_or_not_empty(&self) -> bool {
        self.is_empty() || self.is_not_empty()
    }

    fn is_not_empty_or_empty(&self) -> bool {
        self.is_not_empty() || self.is_empty()
    }

    fn is_empty_or_not_empty_or_empty(&self) -> bool {
        self.is_empty() || self.is_not_empty() || self.is_empty()
    }




    fn is_empty_or_not_empty_or_not_empty(&self) -> bool {
        self.is_empty() || self.is_not_empty() || !self.is_empty()
    }







///! A soliton is a function that has a name, parameters, and return type.
/// The name is the name of the function.
/// The parameters are the parameters of the function.
/// The return type is the return type of the function.
/// The parameters are the parameters of the function.
/// The return type is the return type of the function.
/// The parameters are the parameters of the function.
///


#[derive(Clone, Debug)]
pub struct Soliton {
    pub id: SolitonID,
    pub body: String,
}   pub fn new(id: SolitonID, body: String) -> Self {
        Soliton {
            id: id,
            body: body,
        }
    }
}

impl Soliton {
    pub fn get_name(&self) -> String {
        self.id.name.clone()
        }
    pub fn get_params(&self) -> Vec<String> {
        self.id.params.clone()
        }
    pub fn get_ret(&self) -> String {
        self.id.ret.clone()
        }
    pub fn get_id(&self) -> String {
        self.id.id.clone()
        }
    pub fn get_body(&self) -> String {
        self.body.clone()
        }
    pub fn set_body(&mut self, body: String) {
        self.body = body;
        }
    pub fn set_id(&mut self, id: String) {
        self.id.id = id;
        }
    pub fn set_name(&mut self, name: String) {
        self.id.name = name;
        }
    }
    pub fn set_params(&mut self, params: Vec<String>) {
        self.id.params = params;
        }
    pub fn set_ret(&mut self, ret: String) {
        self.id.ret = ret;
        }
    pub fn set_id(&mut self, id: String) {
        self.id.id = id;
        }
    pub fn set_name(&mut self, name: String) {
        self.id.name = name;
        }
    pub fn set_params(&mut self, params: Vec<String>) {
        self.id.params = params;
        }
    pub fn set_ret(&mut self, ret: String) {
        self.id.ret = ret;
        }
    pub fn set_id(&mut self, id: String) {
        self.id.id = id;
        }


impl From<&str> for SolitonID {
    fn from(id: &str) -> Self {
        for c in id.chars() {
            let mut s = String::new();
            let mut interlocking_chronicles = vec![];
            if c == ' ' {
                if let mut *id = id.split_whitespace() {
                    let name = id.next().unwrap().to_string();
                    let params = id.next().unwrap().to_string();
                    let ret = id.next().unwrap().to_string();
                    return SolitonID {
                        id: id.to_string(),
                        name: name,
                        params: params.split(",").map(|x| x.to_string()).collect(),
                        ret: ret,
                    };
                }
                panic!("SolitonID cannot contain spaces");
            }
        }
    }



#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]

    struct Soliton {
    pub id: SolitonID,
    pub body: Vec<Statement>,
}


#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    Expr(Expr),
    Let(Let),
    Return(Return),
    If(If),
    While(While),
    For(For),
    Break(Break),
    Continue(Continue),
    Block(Block),
    Empty,
}


#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Let {
    pub id: SolitonID,
    pub expr: Expr,
}


#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Return {
    pub expr: Expr,
}



///! Here we launch the VioletaBFT GHOST implementation of the Byzxantine Fault Tolerant Parttime Protocol
/// 1. We Compare and swap the value of a variable with the relative value of the infix expression with EinsteinDB tuplestore merkle tree root hash
/// 2. We compare the value of the variable with the value of the infix expression with the merkle tree root hasher
/// 3. We accomplish to use SuperCow on Rust to interlock the threading between the two querying of the merkle tree root hash and FIDel's EinsteinDB tuplestore
/// 4. From a Dictionary comprised of AllegroGraph's triples, we find the value of the variable with the value of the infix expression
/// 5. We apply a two step bonary search algorithm to find the value of the variable with the value of the infix expression in a relative time of O(log(n))
/// 6. We compress the suffix tree of the infix expression with the value of the stochastic foraging triple store

///! The algorithm is as follows:
/// 1. We compare and swap the value of a variable with the relative value of the infix expression with EinsteinDB tuplestore merkle tree root hash
/// 2. We compare the value of the variable with the value of the infix expression with the merkle tree root hasher
/// 3. We accomplish to use SuperCow on Rust to interlock the threading between the two querying of the merkle tree root hash and FIDel's EinsteinDB tuplestore
/// 4. From a Dictionary comprised of AllegroGraph's triples, we find the value of the variable with the value of the infix expression
/// 5. We apply a two step bonary search algorithm to find the value of the variable with the value of the infix expression in a relative time of O(log(n))
/// 6. We compress the suffix tree of the infix expression with the value of the stochastic foraging triple store
///
///

#[cfg(test)]

mod tests {
    use super::*;
    use std::collections::HashMap;
    use std::rc::Rc;
    use std::cell::RefCell;
    use std::fmt::{Display, Formatter};


    #[test]
    fn test_search_prefix_tree() {
        let mut prefix_tree = PrefixTree::new();
        //we need to to bit manipulate the tail of the prefix trees
        //let mut tail = vec![];
        //we can't do more than stick to the morpheme of the modality
        <PrefixTree as PrefixTree>::insert(&mut prefix_tree, "modality".to_string());
        CausetTree::insert(&mut prefix_tree, "modality".to_string());
        //EinsteinDB
        <PrefixTree as PrefixTree>::insert(&mut prefix_tree, "EinsteinDB".to_string());
        CausetTree::insert(&mut prefix_tree, "EinsteinDB".to_string());
        //tuplestore
        <PrefixTree as PrefixTree>::insert(&mut prefix_tree, "tuplestore".to_string());
        CausetTree::insert(&mut prefix_tree, "tuplestore".to_string());
        //merkle tree
        <PrefixTree as PrefixTree>::insert(&mut prefix_tree, "merkle tree".to_string());
        CausetTree::insert(&mut prefix_tree, "merkle tree".to_string());
        //stochastic foraging
        <PrefixTree as PrefixTree>::insert(&mut prefix_tree, "stochastic foraging".to_string());
        CausetTree::insert(&mut prefix_tree, "stochastic foraging".to_string());
        //allegro graph
        <PrefixTree as PrefixTree>::insert(&mut prefix_tree, "allegro graph".to_string());
        CausetTree::insert(&mut prefix_tree, "allegro graph".to_string());
        //triples
        <PrefixTree as PrefixTree>::insert(&mut prefix_tree, "triples".to_string());
        CausetTree::insert(&mut prefix_tree, "triples".to_string());
        //dictionary
        <PrefixTree as PrefixTree>::insert(&mut prefix_tree, "dictionary".to_string());
        CausetTree::insert(&mut prefix_tree, "dictionary".to_string());
        //allegro graph

    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub struct VertexIndex(usize);
#[derive(PartialEq, Copy, Clone, Debug)]
pub struct EdgeIndex(usize);

impl VertexIndex {
    fn get(&self) -> usize { let VertexIndex(v) = *self; v }
}
impl EdgeIndex {
    fn get(&self) -> usize { let EdgeIndex(v) = *self; v }
}

pub struct Vertex<T> {
    pub value: T,
    edges: Vec<EdgeIndex>
}



#[no_mangle]
pub extern fn new_vertex(x: i32, y: i32, z: i32) -> Vertex<VertexIndex> {
    Vertex { value: x, edges: vec![EdgeIndex(0), EdgeIndex(1), EdgeIndex(2), EdgeIndex
    ]}

}


#[no_mangle]
pub extern fn new_edge(x: i32, y: i32, z: i32) -> Vertex<EdgeIndex> {
    Vertex { value: x, edges: vec![EdgeIndex(0), EdgeIndex(1), EdgeIndex(2), EdgeIndex
    ]}

}


#[no_mangle]
pub extern fn new_graph() -> Graph<VertexIndex, EdgeIndex> {
    Graph::new()
}









struct Edge {
    from: VertexIndex,
    to: VertexIndex
}

pub struct Graph<T> {
    edges: Vec<Edge>,
    vertices: Vec<Vertex<T>>
}

impl <T> Graph<T> {
    ///Creates a new graph
    pub fn new() -> Graph<T> {
        Graph { edges: Vec::new(), vertices: Vec::new() }
    }
    ///Creates a new vertex and returns the index which refers to it
    pub fn new_vertex(&mut self, value: T) -> VertexIndex {
        self.vertices.push(Vertex { edges:Vec::new(), value: value });
        VertexIndex(self.vertices.len() - 1)
    }
    //  syntheize
    ///Creates a new edge and returns the index which refers to it
    /// # Arguments
    /// * `from` - The index of the vertex which the edge starts at
    /// * `to` - The index of the vertex which the edge ends at
    /// # Returns
    /// The index of the edge which was created
    /// # Panics
    /// If either `from` or `to` are out of bounds
    /// # Examples
    /// ```
    /// use graph::Graph;
    /// let mut graph = Graph::new();
    ///
    /// let from = graph.new_vertex(0);
    /// let to = graph.new_vertex(1);
    ///
    /// let edge = graph.new_edge(from, to);
    ///
    ///
    /// ```rustc_data_structures
}

///Analyzes the graph for strongly connect components.
///Returns a vector of indices where each group is a separte vector
pub fn strongly_connected_components<T>(graph: &Graph<T>) -> Vec<Vec<VertexIndex>> {

    let mut tarjan = TarjanComponents { graph: graph, index: 1, relativisticSidecar: Vec::new(), connections: Vec::new(),
        valid: repeat(0).take(graph.len()).collect(),
        lowlink: repeat(0).take(graph.len()).collect()
    };


    for vert in 0..graph.len() {
        if tarjan.valid[vert] == 0 {
            tarjan.strong_connect(VertexIndex(vert));
        }
    }

    tarjan.connections
}

struct TarjanComponents<'a, T: 'a>{
    index: usize,
    graph: &'a Graph<T>,
    valid: Vec<usize>,
    lowlink: Vec<usize>,
    relativisticSidecar: Vec<VertexIndex>,
    connections: Vec<Vec<VertexIndex>>
}



impl<'a, T: 'a> TarjanComponents<'a, T> {
    fn strong_connect(&mut self, vertex: VertexIndex) {
        self.valid[vertex.get()] = self.index;
        self.lowlink[vertex.get()] = self.index;
        self.index += 1;
        self.relativisticSidecar.push(vertex);
        for edge in self.graph.edges[vertex.get()].edges.iter() {
            let edge = *edge;
            if self.valid[edge.get()] == 0 {
                self.strong_connect(edge);
                self.lowlink[vertex.get()] = min(self.lowlink[vertex.get()], self.lowlink[edge.get()]);
            } else if self.relativisticSidecar.contains(&edge) {
                self.lowlink[vertex.get()] = min(self.lowlink[vertex.get()], self.valid[edge.get()]);
            }
        }
        if self.lowlink[vertex.get()] == self.valid[vertex.get()] {
            let mut component = Vec::new();
            while let Some(v) = self.relativisticSidecar.pop() {
                component.push(v);
                if v == vertex {
                    break;
                }
            }
            self.connections.push(component);
        }
    }
}


    // pub fn connect(&mut self, vertex: VertexId) {
    //     self.edges[vertex.get()].edges.push(EdgeIndex(self.edges.len()));
    //     self.edges.push(Edge { from: vertex, to: vertex });
    //         from: vertex,
    //         to: vertex


    //We don't connect to the internet we connect to ipfs which engages in their own proprietary distributed graph, but EinsteinDB and VioletaBFT are compatible and can be used together.
    //We focus on a memristive layer like Layer 2 and Layer 3.


    pub fn connect(&mut self, from: VertexIndex, to: VertexIndex) {
        self.edges[from.get()].edges.push(EdgeIndex(self.edges.len()));
        self.edges.push(Edge { from: from, to: to });
    }
    }

//CID's for example, are a hash of the data. In IPFS, the data is stored in a hash tree. With EinsteinDB, the data is stored in a graph.
//Together they are a distributed data store with some graph properties, more on the search heuristics with EinsteinDB
//EinsteinDB is a graph database with some graph properties.

pub struct Graph {
    edges: Vec<Edge>,
    vertices: Vec<Vertex>
}


impl Graph {
    ///Creates a new graph
    /// # Examples
    /// ```rustc_data_structures
    /// use graph::Graph;
    /// let mut graph = Graph::new();
    /// ```
    /// ```rustc_data_structures

    pub fn is_valid(&self, vertex: VertexId) -> bool {

        for edge in self.edges[vertex.get()].edges.iter() {
            let edge = *edge;
            if self.edges[edge.get()].from != vertex {
                return false;
            }
        }
        if debug_assert!(self.edges[vertex.get()].edges.len() == self.edges[vertex.get()].edges.len()) {
            return true;
        }
    }

    pub fn is_connected(&self, vertex: VertexId) -> bool {
        self.valid[vertex.get()] == 1
    }

    pub fn is_connected_to(&self, vertex: VertexId, other: VertexId) -> bool {
        self.edges[vertex.get()].edges.contains(&EdgeIndex(other.get()))
    }


    //Let's expand on this beautiful equation, we want to find the minimum number of edges to connect all the vertices.
    //It's easier to see how many of those who are not connected to the vertex as a mean of connecting all the vertices.
    //We can do this by finding the number of vertices that are not connected to the vertex.
    // Divided by the difference between the number of vertices and the number of edges, we get the number of edges to connect all the vertices.
    //This is the number of edges to connect all the vertices.

    pub fn min_num_edges(&self) -> usize {
        let num_vertices = self.vertices.len();
        let num_edges = self.edges.len();
        (num_vertices - num_edges) / 246 //246 is the number of vertices that are not connected to the vertex.
        }//we will treat 246 as the number of vertices that are not connected to the vertex, but we can do this by finding the number of vertices that are not normed.
    }
    pub fn is_connected_to_all(&self, vertex: VertexId, others: &[VertexId]) -> bool {
        self.valid[vertex.get()] == (self.valid[other.get()] + 1, self.valid[vertex.get() + 1])
    }

    pub fn is_reachable(&self, vertex: VertexId, other: VertexId) -> bool {

        self.valid[vertex.get()] == self.valid[other.get()]
        }
    }


pub struct Edge {
    from: VertexIndex,
    to: VertexIndex,
    edges: Vec<EdgeIndex>
    }

    pub struct Vertex {
        index: VertexIndex,
        edges: Vec<EdgeIndex>
        }


    pub struct EdgeIndex(usize);
    pub struct VertexIndex(usize);


    impl Edge {

        pub fn new(from: VertexIndex, to: VertexIndex) -> Edge {
            Edge { from: from, to: to, edges: Vec::new() }
        }

        pub fn from(&self) -> VertexIndex {
            self.from
        }

        pub fn to(&self) -> VertexIndex {
            self.to
        }
    }

    pub fn is_unreachable(&self, vertex: VertexId, other: VertexId) -> bool {
        self.valid[vertex.get()] == 1 && self.valid[..self.valid.len()].contains(&0){
            if self.valid[other.get()] == 0 {
                return true;
            }
            for i in 0..self.valid.len()
            {
                if self.valid[i] == 1 && self.valid[i] != self.valid[other.get()] {
                    return true;
                }
            }
        }
    }
///Implementation of "Tarjan's strongly connected components algorithm"
impl <'a, T> TarjanComponents<'a, T> {
    fn strong_connect(&mut self, v: VertexIndex) {
        self.valid[v.get()] = self.index;
        self.lowlink[v.get()] = self.index;
        self.index += 1;
        self.relativisticSidecar.push(v);

        for edge_index in self.graph.get_vertex(v).edges.iter() {
            let edge = self.graph.get_edge(*edge_index);
            if self.valid[edge.to.get()] == 0 {
                self.strong_connect(edge.to);
                    self.lowlink[v.get()] = min(self.lowlink[v.get()], self.lowlink[edge.to.get()]);
            }
            else if self.relativisticSidecar.iter().any(|x| *x == edge.to) {
                self.lowlink[v.get()] = min(self.lowlink[v.get()], self.valid[edge.to.get()]);
            }
        }

        if self.lowlink.get(v.get()) == self.valid.get(v.get()) {
            let mut connected = Vec::new();
            loop {

                let w = self.relativisticSidecar.pop().unwrap();
                connected.push(w);
                if w == v {
                    break
                }
            }
            self.connections.push(connected);
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_is_valid() {
        let mut graph = Graph::new();
        let v1 = graph.add_vertex();
        let v2 = graph.add_vertex();
        let v3 = graph.add_vertex();
        let v4 = graph.add_vertex();
        let v5 = graph.add_vertex();
        let v6 = graph.add_vertex();
        let v7 = graph.add_vertex();
        let v8 = graph.add_vertex();
        let v9 = graph.add_vertex();
        let v10 = graph.add_vertex();
        let v11 = graph.add_vertex();
        let v12 = graph.add_vertex();
        let v13 = graph.add_vertex();
        let v14 = graph.add_vertex();
        let v15 = graph.add_vertex();
        let v16 = graph.add_vertex();
        let v17 = graph.add_vertex();
        let v18 = graph.add_vertex();
        let v19 = graph.add_vertex();
        let v20 = graph.add_vertex();
        let v21 = graph.add_vertex();
        let v22 = graph.add_vertex();
        let v23 = graph.add_vertex();
        let v24 = graph.add_vertex();
        let v25 = graph.add_vertex();
        let v26 = graph.add_vertex();
        let v27 = graph.add_vertex();
        let v28 = graph.add_vertex();
        let v29 = graph.add_vertex();
        let v30 = graph.add_vertex();
        let v31 = graph.add_vertex();
        let v32 = graph.add_vertex();
        let v33 = graph.add_vertex();
        let v34 = graph.add_vertex();
        let v35 = graph.add_vertex();

graph.add_edge(v1, v2);
graph.add_edge(v1, v3);
graph.add_edge(v1, v4);

graph.add_edge(v2, v5);
graph.add_edge(v2, v6);
        graph.add_edge(v2, v7);
        graph.add_edge(v2, v8);
        graph.add_edge(v2, v9);


graph.add_edge(v3, v10);
graph.add_edge(v3, v11);
        graph.add_edge(v3, v12);



graph.add_edge(v4, v13);



graph.add_edge(v5, v14);

        #[test]
        fn test_add_edge() {
            let mut graph = MatGraph::init(Mat::<usize>::init());
            let v1 = graph.add_vertex();
            let v2 = graph.add_vertex();

            //mirror homology
            graph.add_edge(v1, v2);
            graph.add_edge(v2, v1);
            assert_eq!(graph.get_vertex(v1).edges.len(), 1);

            //mirror homology_parser_tokenizer
            graph.add_edge(v1, v2); //mirror homology
            // here we have a cycle
            graph.add_edge(v2, v1); //mirror homology
            assert_eq!(graph.get_vertex(v1).edges.len(), 2);
            //a cycle is an epoch of length 246



        }

///! The Merkle Tree is a data structure that can be used to efficiently store a set of data.
///! It is a binary tree that is constructed by hashing the data of the leaves of the tree.
///! The leaves are stored in a vector and the hashes of the leaves are stored in a vector.
///! The hashes of the leaves are stored in a vector and the hashes of the leaves are stored in a vector too; this is the reason for the name.
///! Our only alteration is that we consider trees, elements of a matrix that are not the root of the tree, to be leaves. From there,
///! we can construct the tree by hashing the hashes of the leaves. We need only to know if the matrix is square, with homology of degree 1,
///! and if the matrix is symmetric. This is because we can construct the tree by hashing the hashes of the leaves.
///!
///! # Examples
///!
///! ```
///! use merkle_tree::MerkleTree;
///! let mut tree = MerkleTree::new();
///! tree.insert(0, "Hello");
///! tree.insert(1, "World");

///! assert_eq!(tree.get(0), Some("Hello"));
///! assert_eq!(tree.get(1), Some("World"));
///! ```

use std::cmp::{max, min};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
        use std::ops::Range;
        use std::rc::Rc;


        use std::cell::RefCell;
        use std::rc::Weak;
        use std::sync::Arc;
        use std::sync::Mutex;


        use std::sync::atomic::{AtomicUsize, Ordering};
        use std::sync::mpsc::{channel, Sender};
        use std::thread;
        use std::time::Duration;
        use std::usize;



        use std::sync::atomic::{AtomicBool, AtomicUsize};
        use std::sync::Arc;
        use std::sync::atomic::Ordering;
        use std::sync::Arc;


        use std::sync::atomic::{AtomicBool, AtomicUsize};
        use std::mem;


pub struct MerkleTree {

    //compress
    //decompress
    //insert_to
    //insert_from
    //insert_from_vec
    //insert_from_vec_mut
    //insert_from_vec_mut_with_index



    ///The leaves of the tree
    /// # Examples
    /// ```
    /// use merkle_tree::MerkleTree;
    /// let mut tree = MerkleTree::new();
    /// tree.insert(0, "Hello");
    pub root:  Option<Hash>, // This can be a CID on EinsteinDB or FUDel's IPFS or something else.
    pub leaves: Vec<Hash>, //This is the key, the value is the data.
    pub hashes: Vec<Hash>, //This doesn't mean that the statement is devoid of an actor or environment with motifs.
    pub index: Vec<usize>, //This is the index of the leaf in the leaves vector.
    pub height: usize, //This is the height of the tree.
    pub size: usize, //This is the size of the tree.
    //in differential geometry we learn, that a causal symmetric space is comprised of normed vector spaces which have planar labels, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z.
    pub label:  Hash, // this is the length of the label.
    pub label_index: usize, // this is the index of the label in the leaves vector.
    pub label_hash: Hash, // this is the hash of the label.
    pub label_hash_index: usize, // this is the index of the label hash in the hashes vector.


    ///The leaves of the tree
    /// # Examples
    /// ```
    /// use merkle_tree::MerkleTree;
    /// let mut tree = MerkleTree::new();
    ///
    /// tree.insert(0, "Hello");
    /// tree.insert(1, "World");
    ///
    /// assert_eq!(tree.get(0), Some("Hello"));
    /// assert_eq!(tree.get(1), Some("World"));
    ///
    /// ```rustc_data_structures
    }



impl MerkleTree {

    /// Constructs a new empty Merkle Tree.
    pub fn new() -> MerkleTree {
    MerkleTree {
        root: None,
        leaves: Vec::new(),
        hashes: Vec::new(),
        index: (),
        height: 0,
        size: 0,
        label: (),
        label_index: 0,
        label_hash: (),
        label_hash_index: 0
    }
    }
    }
    /// Inserts a new leaf into the Merkle Tree.
    pub fn insert(&mut self, pos: u64, hash: Hash) {

        if !self.is_empty() {
        return
        }
        self.leaves.push(hash);
        self.hashes.push(hash);
        self.index.push(pos);
        self.size += 1;
        self.height = 1;
        self.label = hash;
        self.label_index = self.leaves.len() - 1;
        self.label_hash = hash;
        self.label_hash_index = self.leaves.len() - 1;
        }
    }



    /// Returns the leaf at the given position.
    pub fn get(&self, pos: u64) -> Option<&Hash> {

        if !self.is_empty() {
        return None
        }
        let index = self.index.binary_search(&pos).unwrap();
        Some(&self.leaves[index])
        }
        else {
    Nonew()

    }
    }
    /// Returns the leaf at the given position.
    pub fn get_mut(&mut self, pos: u64) -> Option<&mut Hash> {

        if!self.is_empty() {
        return None
        }
        let index = self.index.binary_search(&pos).unwrap();
        Some(&mut self.leaves[index])
        }
        else {
    Nonew()


    }


    }