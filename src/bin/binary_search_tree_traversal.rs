use itertools::Itertools;
use std::collections::VecDeque;
use std::fmt::{Debug, Display, Formatter};
use std::io::BufRead;
use std::str::FromStr;

#[derive(Debug)]
struct NodeData<T>
where
	T: Debug + Eq + Ord,
{
	data: T,
	left: Node<T>,
	right: Node<T>,
}

impl<T> NodeData<T>
where
	T: Debug + Eq + Ord,
{
	fn new(data: T, left: Node<T>, right: Node<T>) -> Self {
		Self { data, left, right }
	}

	fn just_data(data: T) -> Self {
		Self::new(data, Node::root(), Node::root())
	}
}

#[derive(Copy, Clone)]
enum TraversalMethod {
	PreOrder,
	InOrder,
	PostOrder,
	LevelOrder,
}

#[derive(Debug)]
enum Node<T>
where
	T: Debug + Eq + Ord,
{
	Empty,
	WithData(Box<NodeData<T>>),
}

impl<T> Node<T>
where
	T: Debug + Eq + Ord,
{
	fn root() -> Self {
		Self::Empty
	}

	fn with_data(data: T) -> Self {
		Self::WithData(Box::new(NodeData::just_data(data)))
	}

	fn get_data(&self) -> Option<&NodeData<T>> {
		match self {
			Self::Empty => None,
			Self::WithData(node_data) => Some(node_data.as_ref()),
		}
	}

	fn insert(self, data: T) -> Self {
		match self {
			Self::Empty => Self::with_data(data),
			Self::WithData(mut node_data) => {
				if data < node_data.data {
					node_data.left = node_data.left.insert(data);
				} else {
					node_data.right = node_data.right.insert(data);
				}

				Self::WithData(node_data)
			}
		}
	}

	fn traverse(&self, method: TraversalMethod, visitor: &mut impl FnMut(&T) -> ()) {
		if let Some(node_data) = self.get_data() {
			match method {
				TraversalMethod::PreOrder => {
					visitor(&node_data.data);
					node_data.left.traverse(method, visitor);
					node_data.right.traverse(method, visitor);
				}
				TraversalMethod::InOrder => {
					node_data.left.traverse(method, visitor);
					visitor(&node_data.data);
					node_data.right.traverse(method, visitor);
				}
				TraversalMethod::PostOrder => {
					node_data.left.traverse(method, visitor);
					node_data.right.traverse(method, visitor);
					visitor(&node_data.data);
				}
				TraversalMethod::LevelOrder => {
					self.level_traverse(visitor);
				}
			}
		}
	}

	fn level_traverse(&self, visitor: &mut impl FnMut(&T) -> ()) {
		let mut queue = VecDeque::from([self]);

		while let Some(node) = queue.pop_front() {
			if let Some(node_data) = node.get_data() {
				visitor(&node_data.data);
				queue.push_back(&node_data.left);
				queue.push_back(&node_data.right);
			}
		}
	}
}

fn read_from_stdin<T>() -> T
where
	T: FromStr,
{
	let mut buffer = String::new();
	std::io::stdin().lock().read_line(&mut buffer).unwrap();

	T::from_str(buffer.trim_end())
		.ok()
		.expect("Parse from stdin")
}

struct Collector<T>
where
	T: Clone,
{
	vec: Vec<T>,
}

impl<T> Collector<T>
where
	T: Clone,
{
	fn new() -> Self {
		Self { vec: Vec::new() }
	}

	fn collect(&mut self, elem: &T) {
		self.vec.push(elem.clone())
	}
}

impl<T> Display for Collector<T>
where
	T: Display + Clone,
{
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.vec.iter().join(" "))
	}
}

fn main() {
	let mut root: Node<i32> = Node::root();

	let _n: u32 = read_from_stdin();
	let values: String = read_from_stdin();
	let values = values
		.split(" ")
		.map(|raw_number| i32::from_str(raw_number).unwrap());

	for value in values {
		root = root.insert(value);
	}

	for method in [
		TraversalMethod::PreOrder,
		TraversalMethod::InOrder,
		TraversalMethod::PostOrder,
		TraversalMethod::LevelOrder,
	] {
		let mut collector: Collector<i32> = Collector::new();

		root.traverse(method, &mut |v| collector.collect(v));

		println!("{collector}");
	}
}
