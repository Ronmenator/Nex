use nexc_diag::Span;

pub type Name = String;

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub path: String,
    pub span: Span,
    pub items: Vec<Item>,
}

impl Default for SourceFile {
    fn default() -> Self {
        Self {
            path: String::new(),
            span: Span::new(0, 0),
            items: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Internal,
    Public,
}

#[derive(Debug, Clone)]
pub enum ImportKind {
    Module,
    From(Vec<String>),
}

#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub path: Vec<String>,
    pub alias: Option<String>,
    pub kind: ImportKind,
    pub span: Span,
    pub visibility: Visibility,
    pub synthetic: bool,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: String,
    pub args: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeExpr {
    pub span: Span,
    pub kind: TypeExprKind,
}

#[derive(Debug, Clone)]
pub enum TypeExprKind {
    Named(String),
    Generic(String, Vec<TypeExpr>),
    Var,
    Unit,
    Nullable(Box<TypeExpr>),
    Function(Vec<TypeExpr>, Box<TypeExpr>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    String(String),
    Null,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Name,
    pub inferred_type: Option<TypeExpr>,
    pub explicit_type: Option<TypeExpr>,
    pub initializer: Option<Expr>,
    pub is_dynamic: bool,
    pub visibility: Visibility,
    pub attributes: Vec<Attribute>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ParamDecl {
    pub name: Name,
    pub type_hint: Option<TypeExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: Name,
    pub type_params: Vec<Name>,
    pub params: Vec<ParamDecl>,
    pub return_type: Option<TypeExpr>,
    pub is_public: bool,
    pub is_virtual: bool,
    pub is_override: bool,
    pub is_static: bool,
    pub is_async: bool,
    pub operator: Option<String>,
    pub body: Option<Expr>,
    pub span: Span,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct FieldDecl {
    pub name: Name,
    pub ty: Option<TypeExpr>,
    pub initializer: Option<Expr>,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BaseSpec {
    pub name: Name,
    pub shared: bool,
    pub ctor_args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub name: Name,
    pub is_partial: bool,
    pub visibility: Visibility,
    pub type_params: Vec<Name>,
    pub base_specs: Vec<BaseSpec>,
    pub fields: Vec<FieldDecl>,
    pub methods: Vec<FunctionDecl>,
    pub attributes: Vec<Attribute>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct InterfaceDecl {
    pub name: Name,
    pub visibility: Visibility,
    pub type_params: Vec<Name>,
    pub methods: Vec<FunctionDecl>,
    pub attributes: Vec<Attribute>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: Name,
    pub visibility: Visibility,
    pub type_params: Vec<Name>,
    pub interfaces: Vec<Name>,
    pub fields: Vec<FieldDecl>,
    pub methods: Vec<FunctionDecl>,
    pub attributes: Vec<Attribute>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: Name,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: Name,
    pub visibility: Visibility,
    pub variants: Vec<EnumVariant>,
    pub attributes: Vec<Attribute>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UsingDecl {
    pub variable_name: Name,
    pub expr: Expr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CatchClause {
    pub variable_name: Option<Name>,
    pub variable_type: Option<TypeExpr>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub init: Option<Expr>,
    pub condition: Option<Expr>,
    pub step: Option<Expr>,
    pub body: Box<Stmt>,
    pub span: Span,
    /// For-each: `for (varName in iterable) { body }`
    pub for_each: Option<(String, Box<Expr>)>,
}

#[derive(Debug, Clone)]
pub struct TryStmt {
    pub body: Block,
    pub catches: Vec<CatchClause>,
    pub finally: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MemberQualifier {
    pub base: String,
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShlAssign,
    ShrAssign,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Or,
    And,
    BitOr,
    BitXor,
    BitAnd,
    EqEq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,
    Neg,
    BitNot,
}

#[derive(Debug, Clone)]
pub enum StringInterpPart {
    Literal(String),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Literal(Literal, Span),
    EnumVariant {
        enum_name: String,
        variant: String,
        span: Span,
    },
    Wildcard(Span),
    Binding(String, Span),
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Box<Expr>>,
    pub body: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Identifier {
        name: String,
        span: Span,
    },
    Literal {
        value: Literal,
        span: Span,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
        span: Span,
    },
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
        op: AssignOp,
        span: Span,
    },
    Call {
        callee: Box<Expr>,
        type_args: Vec<TypeExpr>,
        args: Vec<Expr>,
        span: Span,
    },
    MemberAccess {
        receiver: Box<Expr>,
        name: String,
        span: Span,
        qualifier: Option<String>,
    },
    Lambda {
        params: Vec<ParamDecl>,
        return_type: Option<TypeExpr>,
        body: Box<Expr>,
        span: Span,
    },
    Await {
        expr: Box<Expr>,
        span: Span,
    },
    StringInterp {
        parts: Vec<StringInterpPart>,
        span: Span,
    },
    Ternary {
        then_expr: Box<Expr>,
        condition: Box<Expr>,
        else_expr: Box<Expr>,
        span: Span,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
        span: Span,
    },
    Block(Block),
    Unsupported {
        raw: String,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Return(Option<Expr>, Span),
    Throw(Expr, Span),
    VarDecl(VarDecl),
    Using(UsingDecl),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Try(TryStmt),
    Block(Block),
    Continue(Span),
    Break(Span),
}

#[derive(Debug, Clone)]
pub enum Item {
    Import(ImportDecl),
    Function(FunctionDecl),
    Class(ClassDecl),
    Interface(InterfaceDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
    Variable(VarDecl),
    Using(UsingDecl),
    Statement(Stmt),
}
