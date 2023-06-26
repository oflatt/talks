let start = "(/ x 2)".parse().unwrap();
let rules: &[Rewrite<SymbolLang, ()>] = &[
    rw!("factor"; "(/ ?x ?y)"
                  => "(* ?x (/ 1 ?y))"),
];
let mut runner = Runner::default()
                        .with_expr(&start)
                        .run(rules);




#[derive(Default)]
struct ConstantFolding;
impl Analysis<SimpleMath> for ConstantFolding {
    type Data = Option<i32>;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        egg::merge_max(to, from)
    }

    fn make(egraph: &EGraph<SimpleMath, Self>, enode: &SimpleMath) -> Self::Data {
        let x = |i: &Id| egraph[*i].data;
        match enode {
            SimpleMath::Num(n) => Some(*n),
            SimpleMath::Add([a, b]) => Some(x(a)? + x(b)?),
            SimpleMath::Mul([a, b]) => Some(x(a)? * x(b)?),
            _ => None,
        }
    }

    fn modify(egraph: &mut EGraph<SimpleMath, Self>, id: Id) {
        if let Some(i) = egraph[id].data {
            let added = egraph.add(SimpleMath::Num(i));
            egraph.union(id, added);
        }
    }
}




let multipattern =
   "?v1 = (f ?x ?y), ?v2 = (g ?x ?y)".parse();

