let emit = console.log;
let test = (name: string, callback: () => void) => callback();

interface AST {
  equals(other: AST): boolean;
}

class Number implements AST {
  constructor(public value: number) {}

  equals(other: AST) {...}
}

class Id implements AST {
  constructor(public value: string) {}

  equals(other: AST) {...}
}

class Not implements AST {
  constructor(public term: AST) {}

  equals(other: AST) {...}
}

class Equal implements AST {
  constructor(public left: AST, public right: AST) {}

  equals(other: AST) {...}
}

class NotEqual implements AST {
  constructor(public left: AST, public right: AST) {}

  equals(other: AST) {...}
}

class Add implements AST {
  constructor(public left: AST, public right: AST) {}

  equals(other: AST) {...}
}

class Subtract implements AST {
  constructor(public left: AST, public right: AST) {}

  equals(other: AST) {...}
}

class Multiply implements AST {
  constructor(public left: AST, public right: AST) {}

  equals(other: AST) {...}
}

class Divide implements AST {
  constructor(public left: AST, public right: AST) {}

  equals(other: AST) {...}
}

class Call implements AST {
  constructor(public callee: string, 
	      public args: Array<AST>) {}

  equals(other: AST) {
    return other instanceof Call && 
      this.callee === other.callee &&
      this.args.length === other.args.length &&
      this.args.every((arg, i) =>
        arg.equals(other.args[i]));
  }
}

class Return implements AST {
  constructor(public term: AST) {}

  equals(other: AST) {...}
}

class Block implements AST {
  constructor(public statements: array<AST>) {}

  equals(other: AST) {...}
}

class If implements AST {
  constructor(public conditional: AST, 
	      public consequence: AST,
              public alternative: AST) {}

  equals(other: AST) {...}
}

class Function implements AST {
  constructor(public name: string, 
              public parameters: Array<string>,
              public body: AST) {}

  equals(other: AST) {...}
}

class Var implements AST {
  constructor(public name: string, public value: AST) {}

  equals(other: AST) {...}
}

class Assign implements AST {
  constructor(public name: string, 
              public value: AST) {}

  equals(other: AST) {...}
}

class While implements AST {
  constructor(public conditional: AST,     
              public body: AST) {}

  equals(other: AST) {...}
}

interface Parser<T> {
  parse(s: Source): ParseResult<T> | null;
}

class Source {
  constructor(public string: string,
              public index: number) {}

  match(regexp: RegExp): (ParseResult<string> | null) {
    console.assert(regexp.sticky);
    regexp.lastIndex = this.index;
    let match = this.string.match(regexp);
    if (match) {
      let value = match[0];
      let newIndex = this.index + value.length;
      let source = new Source(this.string, newIndex);
      return new ParseResult(value, source);
    }
    return null;
}

class ParseResult<T> {
  constructor(public value: T,
              public source: Source) {}
}

class Parser<T> {
  constructor(
    public parse: (s: Source) => (ParseResult<T> | null)
  ) {}

  static regexp(regexp: RegExp): Parser<string> {
    return new Parser(source => source.match(regexp));
  }

  static constant<U>(value: U): Parser<U> {
    return new Parser(source =>
      new ParseResult(value, source));
  }

  static error<U>(message: string): Parser<U> {
    return new Parser(source => {
      throw Error(message);
    });
  }

  or(parser: Parser<T>): Parser<T> {
    return new Parser((source) => {
      let result = this.parse(source);
      if (result)
        return result;
      else 
        return parser.parse(source));
    });
  }

  static zeroOrMore<U>(
    parser: Parser<U>,
  ): Parser<Array<U>> {
    return new Parser(source => {
      let results = [];
      let item = null;
      while (item = parser.parse(source)) {
        source = item.source;
        results.push(item.value);
      }
      return new ParseResult(results, source);
    });
  }

  bind<U>(
    callback: (value: T) => Parser<U>,
  ): Parser<U> {
    return new Parser((source => {
      let result = this.parse(source);
      if (result) {
        let value = result.value;
        let source = result.source;
        return callback(value).parse(source);
      } else {
        return null;
      }
    });
  }

  and<U>(parser: Parser<U>): Parser<U> {
    return this.bind((_) => parser);
  }

  map<U>(callback: (t: T) => U): Parser<U> {
    return this.bind((value) => 
      constant(callback(value)));
  }

  static maybe<U>(
    parser: Parser<U | null>,
  ): Parser<U | null> {
    return parser.or(constant(null));
  }
  
  parseStringToCompletion(string: string): T {
    let source = new Source(string, 0);

    let result = this.parse(source);
    if (!result)
      throw Error("Parse error at index 0");

    let index = result.source.index;
    if (index != result.source.string.length)
      throw Error("Parse error at index " + index);

    return result.value;
  }
}

let {regexp, constant, maybe, zeroOrMore, error} = Parser;

let whitespace = regexp(/[ \n\r\t]+/y);
let comments = regexp(/[/][/].*/y).or(regexp(/[/][*].*[*][/]/sy));
let ignored = zeroOrMore(whitespace.or(comments));

let token = (pattern: RegExp) =>
  Parser.regexp(pattern).bind((value) =>
    ignored.and(constant(value)));

// Keywords
let FUNCTION = token(/function\b/y);
let IF = token(/if\b/y);
let ELSE = token(/else\b/y);
let RETURN = token(/return\b/y);
let VAR = token(/var\b/y);
let WHILE = token(/while\b/y);

let COMMA = token(/[,]/y);
let SEMICOLON = token(/;/y);
let LEFT_PAREN = token(/[(]/y);
let RIGHT_PAREN = token(/[)]/y);
let LEFT_BRACE = token(/[{]/y);
let RIGHT_BRACE = token(/[}]/y);

let NUMBER = 
  token(/[0-9]+/y).map((digits) =>
    new Number(parseInt(digits, 10)));

let ID = token(/[a-zA-Z_][a-zA-Z0-9_]*/y);

let id = ID.map((x) => new Id(x));

// Operators
let NOT = token(/!/y).map((_) => Not);
let EQUAL = token(/==/y).map((_) => EQUAL);
let NOT_EQUAL = token(/!=/y).map((_) => NotEqual);
let PLUS = token(/[+]/y).map((_) => ADD);
let MINUS = token(/[-]/y).map((_) => Subtract);
let STAR = token(/[*]/y).map((_) => Multiply);
let SLASH = token(/[/]/y).map((_) => Divide);
let ASSIGN = token(/=/y).map((_) => Assign);

let expression: Parser<AST> = 
  Parser.error(
    "Expression parser used before definition");

// expression <- comparison 
expression.parse = comparison.parse;

// args <- (expression (COMMA expression)*)?
let args: Parser<Array<AST>>> = 
  expression.bind((arg) =>
    zeroOrMore(COMMA.and(expression)).bind((args) =>
      constant([arg, ...args]))).or(constant([]))

// call <- ID LEFT_PAREN args RIGHT_PAREN
let call = Parser<AST> =
  ID.bind((callee) => 
    LEFT_PAREN.and(args.bind((args) =>
      RIGHT_PAREN.and(constant(
        callee === 'assert'
          ? new Asssert(args[0])
          : new Call(callee, args))))));

// atom <- call / ID / NUMBER 
	/ LEFT_PAREN expression RIGHT_PAREN
let atom: Parser<AST> = 
  call.or(id).or(NUMBER).or(
    LEFT_PAREN.and(expression).bind((e) =>
      RIGHT_PAREN.and(constant(e))));

// unary <- NOT? atom
let unary: Parser<AST> = 
  maybe(NOT).bind((not) =>
    atom.map((term) => not ? new Not(term) : term));

let infix = (operatorParser, termParser) =>
  termParser.bind((term) =>
    zeroOrMore(operatorParser.bind((operator) =>
      termParser.bind((term) =>
        constant({operator, term})))).map((operatorTerms) =>
          oepratorTerms.reduce((left, {operator, term}) =>
            new operator(left, term), term)));

// product <- unary ((STAR / SLASH) unary)*
let product = infix(STAR.or(SLASH).and(unary);

// sum <- product ((PLUS / MINUS) product)*
let sum = infix(PLUS.or(MINUS), product);

// comparison <- sum ((EQUAL / NOT_EQUAL) sum)*
let comparison = infix(EQUAL.or(NOT_EQUAL), sum);

// expression <- comparison
expression.parse = comparison.parse;

let statement: Parser<AST> =
  Parser.error(
    "Statement parser used before definition");

// returnStatement <- RETURN expression SEMICOLON
let returnStatement: Parser<AST> =
  RETURN.and(exression).bind((term) =>
    SEMICOLON.and(constant(new Return(term))));

// expressionStatement <- expression SEMICOLON
let expressioinStatement: Parser<AST> =
  expression.bind((term) =>
    SEMICOLON.and(constant(term)));

// ifStatement <- 
//   IF LEFT_PAREN expression RIGHT_PAREN statement ELSE statement
let ifStatement: Parser<AST> = 
  IF.and(LEFT_PAREN).and(expression).bind(
    (conditional) =>
      RIGHT_PAREN.and(statement).bind((consequence) =>
        ELSE.and(statement).bind((alternative) =>
          constant(new If(conditional,
                          consequence,
 			  alternative)))));

// whileStatement <-
//  WHILE LEFT_PAREN expression RIGHT_PAREN statement
let whileStatement: Parser<AST> =
  WHILE.and(LEFT_PAREN).and(expression).bind(
    (conditional) =>
      RIGHT_PAREN.and(statement).bind((body) =>
        constant(new While(conditional, body))));

// varStatement <- VAR ID ASSIGN expression SEMICOLON
let varStatement: Parser<AST> =
  VAR.and(ID).bind((name) =>
    ASSSIGN.and(expression).bind((value) =>
      SEMICOLON.and(constant(new Var(name, value)))));

// assignmentStatement <- ID ASSIGN EXPRESSION SEMICOLON
let assignmentStatement: Parser<AST> =
  ID.bind((name) =>
    ASSIGN.and(expression).bind((value) =>
      SEMICOLON.and(constant(new Assign(name, value)))));

// blockStatement <- LEFT_BRACE statement* RIGHT_BRACE
let blockStatement: Parser<AST> =
  LEFT_BRACE.and(zeroOrMore(statement)).bind(
    (statements) =>
      RIGHT_BRACE.and(constant(new Block(statements))));

// parameters <- (ID (COMMA ID)*)?
let parameters: Parser<Array<string>> =
  ID.bind((param) =>
    zeroOrMore(COMMA.and(ID)).bind((params) =>
      constant([param, ...params]))).or(constant([]))

// functionStatement <-
//   FUNCTION ID LEFT_PAREN parameters RIGHT_PAREN blockStatement
let functionStatement: Parser<AST> =
  FUNCTION.and(ID).bind((name) =>
    LEFT_PAREN.and(parameters).bind((parameters) =>
      RIGHT_PAREN.and(blockStatement).bind((block) =>
        constant(
          name === 'main'
            ? new Main(block.statements)
              : new Function(name, parameters, block)))));

// statement <- returnStatement
//	      / ifStatement
//	      / whileStatement
//	      / varStatement
//	      / assignmentStatement
//	      / blockStatement
//	      / functionStatement
//	      / expressionStatement
let statementParser: Parser<AST> =
  returnStatement
    .or(functionStatement)
    .or(ifStatement)
    .or(whileStatement)
    .or(varStatement)
    .or(assignmentStatement)
    .or(blockStatement)
    .or(expressionStatement)

statement.parse = statementParser.parse;

let parser: Parser<AST> = 
  ignored.and(zeroOrMore(statement)).map((statements) =>
    new Block(statements));

test("Parser integration test", () => {
  let source = `
    function factorial(n) {
      var result = 1;
      while (n != 1) {
        result = result * n;
        n = n - 1;
      }
      return result;
    }
  `;

  let expected = new Block([
    new Function("factorial", ["n"], new Block([
      new Var("result", new Number(1)),
      new While(new NotEqual(new Id("n"), new Number(1)), new Block([
        new Assign("result", new Multiply(new Id("result"), new Id("n"))),
        new Assign("n", new Subtract(new Id("n"), new Number(1))),
      ])),
      new Return(new Id("result")),
    ])),
  ]);

  let result = parser.parseStringToCompletion(source);

  console.assert(result.equals(expected));
});

interface AST {
  emit(): void;
  equals(other:AST): boolean;
}

class Main implements AST {
  constructor(public statements: Array<AST>) {}

  emit(env: Environment) {
    emit(`.global main`);
    emit(`main:`);
    emit(`  push {fp, lr}`);
    this.statements.forEach((statement) =>
      statement.emit(env)
    );
    emit(`  mov r0, #0`);
    emit(`  pop {fp, pc}`);
  }

  equals(other: AST): boolean {
    return other instanceof Main &&
      this.statements.length === other.statements.length &&
      this.statements.every((statement, i) =>
        statement.equals(other.statements[i]));
  }
}

class Assert imlements AST {
  constructor(public condition: AST) {}

  emit(env: Environment) {
    this.condition.emit(env);
    emit(`   cmp r0, #1`);
    emit(`   moveq r0, #'.'`); 
    emit(`   movne r0, #'F'`); 
    emit(`   bl putchar`);
  }

  equals(other: AST): boolean {
    return other instanceof Assert &&
    this.condition.equals(other.condition);
  }
}

class Number implements AST {
  constructor(public value: number) {}

  emit(env: Environment) {
    emit(`  ldr r0, =${this.value}`);
  }

  equals(other: AST): boolean {
    return other instanceof Number &&
      this.value == other.value;
  }
} 

class Not implements AST {
  constructor(public term: AST) {}

  emit(env: Environment) {
    this.term.emit(env);
    emit(`  cmp r0, #0`);
    emit(`  moveq r0, #1`);
    emit(`  movne r0, #0`);
  }

  equals(other: AST): boolean {
    return other instanceof Not &&
      this.term.equals(other.term);
  }
}

class Equal imlements AST {
  constructor(public left: AST, public right: AST) {}

  emit(env: Environment) {
    this.left.emit(env);
    emit(`  push {r0, ip}`);
    this.right.emit(env);
    emit(`  pop {r1, ip}`);
    emit(`  cmp r0, r1`);
    emit(`  moveq r0, #1`);
    emit(`  movne r0, #0`);
  }

  equals(other: AST): boolean {
    return other instanceof Equal &&
      this.left.equals(other.left) &&
      this.right.equals(other.right);
  }
}

class Block implements AST {
  constructor(public statements: Array<AST>) {}

  emit(env: Environment) {
    this.statements.forEach((statement) =>
      statement.emit(env);
    );
  }

  equals(other: AST): boolean {
    return other instanceof Block &&
      this.statements.length === other.statements.length &&
      this.statements.every((statement, i) =>
        statement.equals(other.statements[i]));
  }
}

class Call implements AST {
  constructor(public callee: string, public args: Array<AST>) {}

  emit(env: Environment) {
    let count = this.arg.length;
    if (count === 0) {
      emit(`  bl ${this.callee}`);
    } else if (count === 1) {
      this.args[0].emit(env);
      emit(`  bl ${this.callee}`);
    } else if (count >= 2 && count <= 4) {
      emit(`  sub sp, sp, #16`);
      this.args.forEach((arg, i) => {
        arg.emit(env);
        emit(`  str r0, [sp, #${4 * i}]`);
      });
      emit(`  pop {r0, r1, r2, r3}`);
      emit(`  bl ${this.callee}`);
    } else {
      throw Error("More than 4 arguments are not supported");
    }
  }

  equals(other: AST): boolean {
    return other instanceof Call &&
      this.callee === other.callee &&
      this.args.length === other.args.length &&
      this.args.every((arg, i) => arg.equals(other.args[i]));
  }
}

class Label {
  static counter = 0;
  value: number;

  constructor() {
    this.value = Label.counter++;
  }

  toString() {
    return `.L${this.value}`;
  }
}

class If implements AST {
  constructor(public conditional: AST,
              public consequence: AST,
  	      public alternative: AST) {}

  emit(env: Environment) {
    let ifFalseLabel = new Label();
    let endIfLabel = new Label();
    this.conditional.emit(env);
    emit(`  cmp r0, #0`);
    emit(`  beq ${ifFalseLabel}`);
    this.consequence.emit(env);
    emit(`  b ${endIfLabel}:`);
    this.alternative.emit(env);
    emit(`${endIfLabel}:`);
  }

  equals(other: AST): boolean {
    return other instanceof If &&
      this.conditional.equals(other.conditional) &&
      this.consequence.equals(other.consequence) &&
      this.alternative.equals(other.alternative);
  }
}
