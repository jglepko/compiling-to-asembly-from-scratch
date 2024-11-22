let emit = console.log;
let test = (name: string, callback: () => void) => callback();

class Number implements AST {
  constructor(public value: number) {}

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
  
  emit(env: environment) {
    this.left.emit(env);
    emit(`  push {r0, ip}`);
    this.right.emit(env);
    emit(`  pop {r1, ip}`);
    emit(`  cmp r0, r1`);
    emit(`  movne r0, #1`);
    emit(`  moveq r0, #0`);
  }

  equals(other: AST): boolean {
    return other instanceof NotEqual &&
      this.left.equals(other.left) &&
      this.right.equals(other.right);
  }
}

class Add implements AST {
  constructor(public left: AST, public right: AST) {}

   emit(env: environment) {
    this.left.emit(env);
    emit(`  push {r0, ip}`);
    this.right.emit(env);
    emit(`  pop {r1, ip}`);
    emit(`  add r0, r1, r0`);
  }

  equals(other: AST): boolean {
    return other instanceof Add &&
      this.left.equals(other.left) &&
      this.right.equals(other.right);
  }
}

class Subtract implements AST {
  constructor(public left: AST, public right: AST) {}

  emit(env: environment) {
    this.left.emit(env);
    emit(`  push {r0, ip}`);
    this.right.emit(env);
    emit(`  pop {r1, ip}`);
    emit(`  sub r0, r1, r0`);
  }

  equals(other: AST): boolean {
    return other instanceof Subtract &&
      this.left.equals(other.left) &&
      this.right.equals(other.right);
  }
}

class Multiply implements AST {
  constructor(public left: AST, public right: AST) {}

  emit(env: environment) {
    this.left.emit(env);
    emit(`  push {r0, ip}`);
    this.right.emit(env); 
    emit(`  pop {r1, ip}`);
    emit(`  mul r0, r1, r0`);
  }

  equals(other: AST): boolean {
    return other instanceof Multiply &&
      this.left.equals(other.left) &&
      this.right.equals(other.right);
  }
}

class Divide implements AST {
  constructor(public left: AST, public right: AST) {}

  emit(env: environment) {
    this.left.emit(env);
    emit(`  push {r0, ip}`);
    this.right.emit(env); 
    emit(`  pop {r1, ip}`);
    emit(`  udiv r0, r1, r0`);
  }

  equals(other: AST): boolean {
    return other instanceof Divide &&
      this.left.equals(other.left) &&
      this.right.equals(other.right);
  }
}

class Exit implements AST {
  constructor(public term: AST) {}

  emit(env: Environment) {
    let syscallNumber = 1;
    emit(`  mov r0, #0`);
    emit(`  bl fflush`);
    this.term.emit(env);
    emit(`  mov r7, #${syscallNumber}`);
    emit(`  swi #0`);
  }

  equals(other: AST): boolean {
    return other instanceof Exit &&
      this.term.equals(other.term);
  }
}

interface Parser<T> {
  parse(s: Source): ParseResult<T> | null;
}

class Source {
  constructor(public string: string,
              public index: number) {}

  match(regexp: RegExp): (ParseResult<string> | null) {
    console.assert(regexp['sticky']);
    regexp.lastIndex = this.index;
    let match = this.string.match(regexp);
    if (match) {
      let value = match[0];
      let source = new Source(this.string, this.index + value.length);
      return new ParseResult(value, source);
    }
    return null;
  }
}

test("Source matching is idempotent", () => {
  let s = new Source('  let', 2);
  let result1 = s.match(/let/y);
  console.assert(result1 !== null && result1.value === 'let');
  console.assert(result1 !== null && result1.source.index == 5);
  let result2 = s.match(/let/y);
  console.assert(result2 !== null && result2.value === 'let');
  console.assert(result2 !== null && result2.source.index == 5);
});

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

  static zeroOrMore<U>(parser: Parser<U>): Parser<Array<U>> {
    return new Parser(source => {
      let results = [];
      let item;
      while (item = parser.parse(source)) {
        source = item.source;
        results.push(item.value);
      }
      return new ParseResult(results, source);
    });
  }

  bind<U>(callback: (t: T) => Parser<U>): Parser<U> {
    return new Parser((source => {
      let result = this.parse(source);
      if (result) {
        return callback(result.value).parse(result.source);
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

  static maybe<U>(parser: Parser<U | null>): Parser<U | null> {
    return parser.or(constant(null));
  }
  
  parseStringToCompletion(string: string): T {
    let source = new Source(string, 0);

    let result = this.parse(source);
    if (!result)
      throw Error("Parse error: could not parse anything at all");

    let index = result.source.index;
    if (index != result.source.string.length)
      throw Error("Parse error at index " + index);

    return result.value;
  }
}

let {regexp, constant, maybe, zeroOrMore, error} = Parser;

test("Parsing alternatives with `or`", () => {
  let parser = regexp(/by/y).or(regexp(/hai/y));
  let result = parser.parseStringToCompletion('hai');
  console.assert(result == 'hai');
});

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
  Parser.error("Expression parser used before definition");

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
          operatorTerms.reduce((left, {operator, term}) =>
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
let expressionStatement: Parser<AST> =
  expression.bind((term) => SEMICOLON.and(constant(term)));

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
          name === '__main'
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

interface AST {
  emit(env: Environment): void;
  equals(node: AST): boolean;
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

class Environment {
  constructor(public locals: Map<string, number> = new Map(),
	      public nextLocalOffset: number = 0) {}
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

class Function implements AST {
  constructor(public name: string,
              public parameters: Array<string>,
              public body: AST) {}

  emit(_: Environment) {
    if (this.parameters.length > 4)
      throw Error("More than 4 params is not supported");

    emit(``);
    emit(`.global ${this.name}`);
    emit(`${this.name}:`);

    this.emitPrologue();
    let env = this.setUpEnvironment();
    this.body.emit(env);
    this.emitEpilogue();
  }

  emitPrologue() {
    emit(`  push {fp, lr}`);
    emit(`  mov fp, sp`);
    emit(`  push {r0, r1, r2, r3}`);
    // Alternatively: 
    // emit(`  push {r0, r1, r2, r3, fp, lr}`);
    // emit(`  add fp, sp, $16`);
  }

  setUpEnvironment() {
    let env = new Environment();
    this.parameters.forEach((parameter, i) => {
      env.locals.set(parameter, 4 * i - 16);
    });
    env.nextLocalOffset = -20;
    return env;
  }

  emitPrologue() {
    emit(`  mov sp, fp`);
    emit(`  mov ro, #0`);
    emit(`  pop {fp, pc}`);
  }

  equals(other: AST): boolean {
    return other instanceof Function &&
      this.name === other.name &&
      this.parameters.length === other.parameters.length &&
      this.parameters.every((parameter, i) =>
        parameter === other.parameters[i]) &&
      this.body.equals(other.body);
  }
} 

class Id implements AST {
  constructor(public value: string) {}

  emit(env: Environment) {
    let offset = env.locals.get(this.value);
    if (offset) {
      emit(`  ldr r0, [fp, #${offset}]`);
    } else {
      throw Error(`Undefined variable: ${this.value}`);
    }
  }

  equals(other: AST): boolean {
    return other instanceof Id &&
      this.value === other.value;
  }
}

class Return implements AST {
  constructor(public term: AST) {}

  emit(env: Environment) {
    this.term.emit(env);
    emit(`  mov sp, fp`);
    emit(`  pop {fp, pc}`);
  }

  equals(other: AST): boolean {
    return other instanceof Return &&
      this.term.equals((other.term);
  }
}

class Assign implements AST {
  constructor(public name: string, public value: AST) {}

  emit(env: Environment) {
    this.value.emit(env);
    let offset = env.locals.get(this.name);
    if (offset) {
      emit(`  str r0, [fp, #${offset}]`);
    } else {
      throw Error(`Undefined variable: ${this.name}`);
    }
  }

  equals(other: AST): boolean {
    return other instanceof Assign &&
      this.name === other.name &&
      this.value.equals(other.value);
  }
}

class While implements AST {
  constructor(public conditional: AST, public body: AST) {}

  emit(env: Environment) {
    let loopStart = new Label();
    let loopEnd = new Label();

    emit(`${loopStart}:`);
    this.conditional.emit(env);
    emit(`  cmp r0, #0`);
    emit(`  beq ${loopEnd}`);
    this.body.emit(env);
    emit(`  b ${loopStart}`);
    emit(`${loopEnd}:`);
  } 

  equals(other: AST): boolean {
    return other instanceof While &&
      this.conditional.equals(other.conditional) &&
      this.body.equals(other.body);
  }
}


class Var implements AST {
  constructor(public name: string, public value: AST) {}

  emit(env: Environment) {
    this.value.emit(env);
    emit(`  push {r0, ip}`);
    env.locals.set(this.name, env.nextLocalOffset - 4);
    env.nextLocalOffset -= 8;
  }

  equals(other: AST): boolean {
    return other instanceof Var &&
      this.name === other.name &&
      this.value.equals(other.value);
  }
}

test("Expression parser", () => {
  let [x, y, z] = [new Id('x'), new id('y'), new Id('z')];
  let parse = (s: string) => expression.parseStringToCompletion(s);

  console.assert(parse('x + y + z').equals(new Add(new Add(x, y), z)));
  console.assert(parse('x + y * z').equals(new Add(x, new Multiply(y, z))));
  console.assert(parse('x * y + z').equals(new Add(new Multiply(x, y), z)));
  console.assert(parse('(x + y) * z').equals(new Multiply(new Add(x, y), z)));
  console.assert(parse('x == y + z').equals(new Equal(x, new Add(y, z))));
  console.assert(parse('x + y == z').equals(new Equal(new Add(x, y), z)));

  console.assert(parse('f()').equals(new Call('f', [])));
  console.assert(parse('f(x)').equals(new Call('f', [x])));
  console.assert(parse('f(x, y, z)').equals(new Call('f', [x, y, z])));
});

test("Statement parser", () => {
  let [x, y, z] = [new Id('x'), new id('y'), new Id('z')];
  let parse = (s: string) => expression.parseStringToCompletion(s);

  console.assert(parse('return x;').equals(new Return(x)));
  console.assert(parse('returnx;').equals(new Id('returnx')));
  console.assert(parse('x + y;').equals(new Add(x, y)));

  console.assert(parse('if (x) return y; else return z;').equals(
    new If(x, new Return(y), new Return(z))));

  console.assert(parse('{}').equals(new Block([])));
  console.assert(parse('{ x; y; }').equals(new Block([x, y])));

  console.assert(parse('if (x) { return y; } else { return z; }').equals(
    new If(x, new Block([new Return(y)]), new Block([new Return(z)]))));

  console.assert(parse('function id(x) { return x; }').equals(
    new Function('id', ['x'], new Block([new Return(x)]))));
});

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

test("End-to-end test", () => {
  let source = `
    function main() {
      // Test Number
      assert(1);

      // Test Not
      assert(!0);
      assert(!(!1));
      
      putchar(46);

      // Test Equal
      assert(42 == 42);
      assert(!(0 == 42));
      
      // Test NotEqual
      assert(!(42 != 42));
      assert(0 != 42);

      // Test infix operators
      assert(42 == 4 + 2 * (12 - 2) + 3 * (5 + 1));

      // Test Call with no parameters
      assert(return42() == 42);
      assert(!returnNothing());

      // Test multiple parameters
      assert42(42);
      assert1234(1, 2, 3, 4);

      //assert(rand() != 42);
      //assert(putchar() != 1);

      //while (1) {
      //  assert(1);
      //}

      // Test If
      if (1)
        assert(1);
      else
        assert(0);

      if (0) {
        assert(0);
      } else {
        assert(1);
      }

      assert(factorial(5) == 120);

      var x = 4 + 2 * (12 - 2);
      var y = 3 * (5 + 1);
      var z = x + y;
      assert(z == 42);

      var a = 1;
      assert(a == 1);
      a = 0;
      assert(a == 0);

      // Test while loops
      var i = 0;
      while (i != 3) {
        i = i + 1;
      }
      assert(i == 3);

      assert(factorial2(5) == 120);

      putchar(10); // Newline
    }
 
    function return42() { return 42; }
    function returnNothing() {}
    function assert42(x) {
      assert(x == 42);
    }
    function assert1234(a, b, c, d) {
      assert(a == 1);
      assert(b == 2);
      assert(c == 3);
      assert(d == 4);
    }

    function assert(x) {
      if (x) {
        putchar(46);
      } else {
        putchar(70);
      }
    }

    function factorial(n) {
      if (n == 0) {
        return 1;
      } else {
        return n * factorial(n - 1);
      }
    }

    function factorial2(n) {
      var result = 1;
      while (n != 1) {
        result = result * n;
        n = n - 1;
      }
      return result;
    }
  `;

  let ast = parser.parseStringToCompletion(source);
 
  ast.emit(new Environment());
});

export {}
