mod semantic_generated;
mod types;

use std::cell::RefCell; 
use std::rc::Rc; 

use crate::ast; 

use crate::semantic; 
use crate::semantic::walk;
use semantic_generated::fbsemantic;
use flatbuffers::{UnionWIPOffset, WIPOffset}; 

pub fn serialize(ast_pkg: &ast::Package) -> Result<(Vec<u8>, usize), String> {    
    let v = new_serializing_visitor_with_capacity(1024);
    walk::walk(&v, walk::Node::Package(ast_pkg));
    v.finish()
}

fn new_serializing_visitor_with_capacity<'a>(capacity: usize) -> SerializingVisitor<'a> {
    SerializingVisitor {
        inner: Rc::new(RefCell::new(SerializingVisitorState::new_with_capacity(
            1024,
        ))),
    }
}

struct SerializingVisitor<'a> {
    inner: Rc<RefCell<SerializingVisitorState<'a>>>,
}

impl<'a> semantic::walk::Visitor<'a> for SerializingVisitor<'a> {
    fn visit(&self, _node: Rc<walk::Node<'a>>) -> Option<Self> {
        let v = self.inner.borrow();
        if let Some(_) = &v.err {
            return None;
        }
        Some(SerializingVisitor {
            inner: Rc::clone(&self.inner),
        })
    }

    fn done(&self, node: Rc<walk::Node<'a>>) {
        let mut v = &mut *self.inner.borrow_mut();
        if let Some(_) = &v.err {
            return;
        }
        let node = node.as_ref();
        let loc = v.create_loc(node.loc()); 
        match node {
            walk::Node::IntegerLit(int) => {
                let int = fbsemantic::IntegerLiteral::create(
                    &mut v.builder, 
                    &fbsemantic::IntegerLiteralArgs {
                        loc, 
                        value: int.value, 
                    }, 
                ); 
                v.expr_stack.push((int.as_union_value(), fbsemantic::Expression::IntegerLiteral))
            }
            walk::Node::UintLit(uint) => {
                let uint = fbsemantic::UnsignedIntegerLiteral::create(
                    &mut v.builder, 
                    &fbsemantic::UnsignedIntegerLiteralArgs {
                        loc, 
                        value: uint.value, 
                    }, 
                ); 
                v.expr_stack.push((uint.as_union_value(), fbsemantic::Expression::UnsignedIntegerLiteral))
            }
            walk::Node::FloatLit(float) => {
                let float = fbsemantic::FloatLiteral::create(
                    &mut v.builder, 
                    &fbsemantic::FloatLiteralArgs {
                        loc, 
                        value: float.value, 
                    }, 
                ); 
                v.expr_stack.push((float.as_union_value(), fbsemantic::Expression::FloatLiteral))
            }
            walk::Node::RegexpLit(regex) => {
                let regex_val = v.create_string(&regex.value);
                let regex = fbsemantic::RegexpLiteral::create(
                    &mut v.builder, 
                    &fbsemantic::RegexpLiteralArgs {
                        loc, 
                        value: regex_val, 
                    }, 
                ); 
                v.expr_stack.push((regex.as_union_value(), fbsemantic::Expression::RegexpLiteral))
            }
            walk::Node::StringLit(string) => {
                let string_val = v.create_string(&string.value);
                let string = fbsemantic::StringLiteral::create(
                    &mut v.builder, 
                    &fbsemantic::StringLiteralArgs {
                        loc, 
                        value: string_val, 
                    }, 
                ); 
                v.expr_stack.push((string.as_union_value(), fbsemantic::Expression::StringLiteral))
            }
            walk::Node::DurationLit(dur_lit) => {
                let mut dur_vec: Vec<WIPOffset<fbsemantic::Duration>> =
                    Vec::with_capacity(dur_lit.values.len()); 
            }
        }
    }
}

impl<'a> SerializingVisitor<'a> {
    fn finish(self) -> Result<(Vec<u8>, usize), String> {
        let v = match Rc::try_unwrap(self.inner) {
            Ok(sv) => sv,
            Err(_) => return Err(String::from("error unwrapping rc")),
        };
        let mut v = v.into_inner();
        if let Some(e) = v.err {
            return Err(e);
        };
        let pkg = match v.package {
            None => return Err(String::from("missing serialized package")),
            Some(pkg) => pkg,
        };
        v.builder.finish(pkg, None);

        // Collapse releases ownership of the byte vector and returns it to caller.
        Ok(v.builder.collapse())
    }
}



struct SerializingVisitorState<'a> {
    // Any error that occurred during serialization, returned by the visitor's finish method.
    err: Option<String>,

    builder: flatbuffers::FlatBufferBuilder<'a>,

    package: Option<WIPOffset<fbsemantic::Package<'a>>>,
    package_clause: Option<WIPOffset<fbsemantic::PackageClause<'a>>>,
    import_decls: Vec<WIPOffset<fbsemantic::ImportDeclaration<'a>>>,
    files: Vec<WIPOffset<fbsemantic::File<'a>>>,
    blocks: Vec<WIPOffset<fbsemantic::Block<'a>>>,
    stmts: Vec<(WIPOffset<UnionWIPOffset>, fbsemantic::Statement)>,

    expr_stack: Vec<(WIPOffset<UnionWIPOffset>, fbsemantic::Expression)>,
    properties: Vec<WIPOffset<fbsemantic::Property<'a>>>,
    string_expr_parts: Vec<WIPOffset<fbsemantic::StringExpressionPart<'a>>>,
    member_assign: Option<WIPOffset<UnionWIPOffset>>,
}

impl<'a> SerializingVisitorState<'a> {
    fn new_with_capacity(capacity: usize) -> SerializingVisitorState<'a> {
        SerializingVisitorState {
            err: None,
            builder: flatbuffers::FlatBufferBuilder::new_with_capacity(capacity),
            package: None,
            package_clause: None,
            import_decls: Vec::new(),
            files: Vec::new(),
            blocks: Vec::new(),
            stmts: Vec::new(),
            expr_stack: Vec::new(),
            properties: Vec::new(),
            string_expr_parts: Vec::new(),
            member_assign: None,
        }
    }

    fn pop_expr(&mut self) -> (Option<WIPOffset<UnionWIPOffset>>, fbsemantic::Expression) {
        match self.expr_stack.pop() {
            None => {
                self.err = Some(String::from("Tried popping empty expression stack"));
                return (None, fbsemantic::Expression::NONE);
            }
            Some((o, e)) => (Some(o), e),
        }
    }

    fn pop_expr_with_kind<T>(&mut self, kind: fbsemantic::Expression) -> Option<WIPOffset<T>> {
        match self.expr_stack.pop() {
            Some((wipo, e)) => {
                if e == kind {
                    Some(WIPOffset::new(wipo.value()))
                } else {
                    self.err = Some(String::from(format!(
                        "expected {} on expr stack, got {}",
                        fbsemantic::enum_name_expression(kind),
                        fbsemantic::enum_name_expression(e)
                    )));
                    return None;
                }
            }
            None => {
                self.err = Some(String::from("Tried popping empty expression stack"));
                return None;
            }
        }
    }

    fn create_string(&mut self, str: &String) -> Option<WIPOffset<&'a str>> {
        Some(self.builder.create_string(str.as_str()))
    }

    fn create_opt_string(&mut self, str: &Option<String>) -> Option<WIPOffset<&'a str>> {
        match str {
            None => None,
            Some(str) => Some(self.builder.create_string(str.as_str())),
        }
    }

    fn create_stmt_vector(&mut self, num_of_stmts: usize) -> Vec<WIPOffset<fbsemantic::WrappedStatement<'a>>> {
        let start = self.stmts.len() - num_of_stmts;
        let union_stmts = &self.stmts.as_slice()[start..];
        let mut wrapped_stmts: Vec<WIPOffset<fbsemantic::WrappedStatement>> =
            Vec::with_capacity(num_of_stmts);

        for (stmt, stmt_type) in union_stmts {
            let wrapped_st = fbsemantic::WrappedStatement::create(
                &mut self.builder,
                &fbsemantic::WrappedStatementArgs {
                    statement_type: *stmt_type,
                    statement: Some(*stmt),
                },
            );
            wrapped_stmts.push(wrapped_st);
        }

        self.stmts.truncate(start);

        wrapped_stmts
    }

    fn create_property_vector(&mut self, n_props: usize) -> Vec<WIPOffset<fbsemantic::Property<'a>>> {
        let start = self.properties.len() - n_props;
        self.properties.split_off(start)
    }

    fn pop_property_key(&mut self) -> (Option<WIPOffset<UnionWIPOffset>>, fbsemantic::PropertyKey) {
        match self.pop_expr() {
            (offset, fbsemantic::Expression::IdentifierExpression) => (offset, fbsemantic::PropertyKey::Identifier),
            (offset, fbsemantic::Expression::StringLiteral) => {
                (offset, fbsemantic::PropertyKey::StringLiteral)
            }
            _ => {
                self.err = Some(String::from(
                    "unexpected expression on stack for property key",
                ));
                (None, fbsemantic::PropertyKey::NONE)
            }
        }
    }

    // fn pop_assignment_stmt(&mut self) -> (Option<WIPOffset<UnionWIPOffset>>, fbsemantic::Assignment) {
    //     match self.stmts.pop() {
    //         Some((va, fbsemantic::Statement::)) => {
    //             (Some(va), fbsemantic::Assignment::VariableAssignment)
    //         }
    //         None => {
    //             self.err = Some(String::from("Tried popping empty statement stack. Expected assignment on top of stack."));
    //             (None, fbsemantic::Assignment::NONE)
    //         }
    //         Some(_) => {
    //             self.err = Some(String::from("Expected assignment on top of stack statement stack."));
    //             (None, fbsemantic::Assignment::NONE)
    //         }
    //     }
    // }

    fn create_loc(&mut self, loc: &ast::SourceLocation) -> Option<WIPOffset<fbsemantic::SourceLocation<'a>>> {
        let file = self.create_opt_string(&loc.file);
        let source = self.create_opt_string(&loc.source);
        
        Some(fbsemantic::SourceLocation::create(
            &mut self.builder,
            &fbsemantic::SourceLocationArgs {
                file,
                start: Some(&fbsemantic::Position::new(
                    loc.start.line as i32,
                    loc.start.column as i32,
                )),
                end: Some(&fbsemantic::Position::new(
                    loc.end.line as i32,
                    loc.end.column as i32,
                )),
                source,
            },
        ))
    }
}
