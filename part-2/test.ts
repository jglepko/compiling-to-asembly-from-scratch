import { 
  Type, BooleanType, NumberType, VoidType, ArrayType, FunctionType, } from "/.types"

import {
  AST, Main, Assert, Length, Number, Boolean, Undefined, Not, Equal, NotEqual,
  Add, Subtract, Multiply, Divide, Call, ArrayLiteral, ArrayLookup, Exit, Block, If,
  Function, Id, Return, While, Assign, Var, Visitor,
} from "./ast;

import { TypeChecker } from "./type-checker"
import { CodeGenerator, CodeGeneratorDynamicTyping } from "./code-generator"
import { ASTTraversal } from "./ast-traversal"
import { Optimizer } from "./optimizer"
import { ParseResult, Source, Parser } from "./parser-combinators"
import { statement, expression, parser } from "./parser"

let test = (name: string, callback: () => void) => callback();

test("Source matching is idempotent", () => {
  let s = new Source('  let', 2);
  let result1 = s.match(/let/y);
  console.assert(result1 !== null && result1.value === 'let');
  console.assert(result1 !== null && result1.source.index == 5);
  let result2 = s.match(/let/y);
  console.assert(result2 !== null && result2.value === 'let');
  console.assert(result2 !== null && result2.source.index == 5);
});

let {regexp, constant, maybe, zeroOrMore, error} = Parser;

test("Parsing alternatives with `or`", () => {
  let parser = regexp(/by/y).or(regexp(/hai/y));
  let result = parser.parseStringToCompletion('hai');
  console.assert(result == 'hai');
});

test("Parsing with bindings", () => {
  let parser = regexp(/[a-z]+/y).bind((word) =>
    regexp(/[0-9]+/y).bind((digits) =>
      constant(`first ${word}, then ${digits}`)));
  let result = parser.parseStringToCompletion(`hai123');
  console.assert(result == 'first hai, then 123');
});

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

//  console.assert(parse('function id(x) { return x; }').equals(
//    new Function('id', ['x'], new Block([new Return(x)]))));
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

let parse = (s: string) => parser.parseStringToCompletion(s);


