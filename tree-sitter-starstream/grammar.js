module.exports = grammar({
  name: 'starstream',

  extras: $ => [
    $._whitespace,
    $.comment,
    $.commentLine
  ],

  reserved: {
    global: $ => ["fn", "let", "mut"]
  },

  rules: {
    Program: $ => seq(
      repeat(choice($.Utxo, $.Script, $.Token, $.TypeDef, $.Constant, $.Abi))
    ),

    Utxo: $ => seq(
      'utxo',
      $.Type,
      '{',
      repeat(choice($.Main, $.Impl, $.Storage)),
      '}'
    ),

    Script: $ => seq(
      'script',
      '{',
      repeat($.FnDef),
      '}'
    ),

    Token: $ => seq(
      'token',
      $.ident,
      '{',
      repeat(choice($.Bind, $.Unbind, $.Mint)),
      '}'
    ),

    TypeDef: $ => seq (
      'typedef',
      $.ident,
      '=',
      $.Type
    ),

    Constant: $ => seq (
      'const',
      $.ident,
      '=',
      $.Expr,
      ';'
    ),

    Abi: $ => seq(
      'abi',
      $.Type,
      '{',
      repeat(seq(choice($.FnSig, $.EffectSig), ';')),
      '}'
    ),

    Impl: $ => seq(
      'impl',
      $.Type,
      '{',
      repeat($.FnDef),
      '}'
    ),

    Main: $ => seq(
      'main',
      optional(seq('(', $.TypedBindings, ')')),
      $.Block
    ),

    Storage: $ => seq(
      'storage',
      '{',
      repeat(seq($.TypedBinding, ';')),
      '}'
    ),

    Bind: $ => seq(
      'bind',
      $.Block
    ),

    Unbind: $ => seq(
      'unbind',
      $.Block
    ),

    Mint: $ => seq(
      'mint',
      $.Block
    ),

    TypedBinding: $ => seq(
      $.ident,
      optional(seq(':', $.Type))
    ),

    TypedBindings: $ =>
      seq(
        $.TypedBinding,
        repeat(seq(',', $.TypedBinding)),
        optional(',')
      )
    ,

    EffectSet: $ => seq(
      '{',
      optional(
        seq(
          $.ident,
          repeat(
            seq(
            ',',
            $.ident
            )
          )
        )
      ),
      '}'
    ),

    FnDef: $ => seq(
      'fn',
      $.ident,
      '(',
      optional($.TypedBindings),
      ')',
      optional(seq(':', $.Type)),
      optional(seq( '/', $.EffectSet )),
      $.Block
    ),

    EffectBlock: $ => seq($.Effect, $.Block),

    Statement: $ => choice(
      $.BindVar,
      seq($.returnLike, optional($.Expr), ';'),
      seq($.Assign, ';'),
      seq('try', $.Block, repeat1(seq('with', $.EffectBlock))),
      seq('while', '(', $.Expr, ')', $.LoopBody),
      seq('loop', $.LoopBody)
    ),

    Assign: $ => seq(
      $.Expr,
      '=',
      $.Expr
    ),

    Var: $ => $.ident,

    BindVar: $ => seq(
      seq('let', optional('mut')),
      $.Var,
      optional(seq(':', $.Type)),
      '=',
      $.Expr,
      ';'
    ),

    LoopBody: $ => choice(
      $.Statement,
      seq($.Block),
      seq($.Expr, ';')
    ),

    Effect: $ => seq(
      repeat(
        seq($.Type, '::')
      ),
      $.ident,
      '(',
      $.TypedBindings,
      ')'
    ),

    Block: $ => 
      seq(
        '{',
        seq(repeat(choice($.Statement, seq($.IfExpr, optional(';')), seq($.Expr, ';'))), optional($.Expr)),
        '}'
    ),

    Expr: $ => prec.left(1, choice(
      seq($.PrimaryExpr, optional($.InfixOpTail)),
      seq('!', $.Expr),
      seq($.yield, optional($.Expr))
    )),

    InfixOpTail: $ => seq(
      $.InfixOp,
      $.Expr
    ),

    PrimaryExpr: $ => prec.left(1, choice(
      $.number,
      $.bool,
      $.stringLiteral,
      seq(repeat(seq($.Type, '::')), $.ident, optional($.Arguments)),
      seq('(', $.Expr, ')'),
      $.Block,
      $.IfExpr,
      $.ObjectLiteral,
    )),

    ObjectLiteral: $ => seq(
      $.Type,
      '{',
        seq(
          seq($.ident, ':', $.Expr),
          repeat(seq(',', $.ident, ':', $.Expr)),
          optional(',')
        ),
      '}'
     ),

    IfExpr: $ => prec.left(1, seq(
      'if',
      '(',
      $.Expr,
      ')',
      $.Block,
      optional(seq('else', $.Block))
    )),

    Arguments: $ => seq(
      '(',
      optional(seq($.Expr, repeat(seq(',', $.Expr)))),
      ')'
    ),

    InfixOp: $ => choice(
      '+', '*', '==', '<', '<=', '>=', '!=', '&&', '||', '.', '-'
    ),

    Sig: $ => seq(
      $.ident,
      '(',
      optional(seq($.Type, repeat(seq(',', $.Type)))),
      ')',
      optional(seq(':', $.Type))
    ),

    FnSig: $ => seq(
      'fn',
      $.Sig
    ),

    EffectSigType: $ => seq(
      $.Type,
      '(',
      optional(seq($.Type, repeat(seq(',', $.Type)))),
      ')',
      optional(seq(':', $.Type))
    ),

    EffectSig: $ => seq(
      choice('effect', 'event', 'error'),
      $.EffectSigType
    ),

    Type: $ => choice(
      seq('(', $.TypedBindings, ')', optional(seq('->', $.Type))),
      seq('&', $.Type),
      seq('{', $.TypedBindings, '}'),
      seq($.ident, optional(seq('<', $.Type, repeat(seq(',', $.Type)), '>')))
    ),

    // Terminals
    ident: $ => {
      const identifier = /[a-zA-Z]([a-zA-Z0-9]|_)+|[a-zA-Z]/;
      return token(prec(-1, new RegExp(identifier.source)));
    },
    _whitespace: $ => /[\t\n\r ]+/,
    number: $ => /0|-?[1-9]([0-9]|_)*(\.[0-9]*)?/,
    stringLiteral: $ => /"[^"]*"/,
    yield: $ => choice('yield', 'raise', 'fail'),
    returnLike: $ => choice('resume', 'return'),
    bool: $ => choice($.true, $.false),
    true: $ => 'true',
    false: $ => 'false',
    comment: $ => /\/\*[^*]*\*+([^/*][^*]*\*+)*\//,
    commentLine: $ => /\/\/[^\n\r]*/
  }
});
