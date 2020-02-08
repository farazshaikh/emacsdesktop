use std::sync::{Arc, Mutex};
use std::thread;

type Hub = Vec<Node>;
type NodeArr = Arc<Mutex<Hub>>;

trait Transport {
    fn on_message(&self, from: u16, message: u16);
    fn send(&self, to_node: u16, message: u16, hub: &Hub);
}

#[derive(Debug)]
struct Node {
    node_id: u16,
    cookie: u16,
    arr: NodeArr,
}

impl Transport for Node {
    fn on_message(&self, from_node: u16, message: u16) {
        println!(
            "node {} recieved {} from node {}",
            self.node_id, message, from_node
        );
    }
    fn send(&self, to_node: u16, message: u16, hub: &Hub) {
        println!("Node {} sent {} to Node {}", self.node_id, message, to_node);
        hub[to_node as usize].on_message(self.node_id, message);
    }
}

fn threadfn(i: u16, num: u16, node_arr: NodeArr) {
    let inner = node_arr.lock().unwrap();
    inner[i as usize].send((i + 1) % num, i * 100, &inner)
}

#[warn(unused_variables)]
#[warn(dead_code)]
fn main() {
    let arr: NodeArr = Default::default();
    for id in 0..10 {
        let mut v = arr.lock().unwrap();
        v.push(Node {
            node_id: id,
            cookie: id,
            arr: arr.clone(),
        })
    }

    let mut jh = Vec::new();
    for id in 0..10 {
        let perthread = arr.clone();
        jh.push(thread::spawn(move || {
            threadfn(id, 10, perthread);
        }));
    }

    for j in jh {
        j.join().unwrap();
    }
    println!("Hello, world!");
}
