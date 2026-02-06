declare module "*.grammar" {
  import { Parser } from "@lezer/common";

  export const parser: Parser;
}

declare module "*squirrel_lezer.grammar.terms" {
  export const insertSemi: number;
  export const LineComment: number;
  export const BlockComment: number;
  export const spaces: number;
  export const newline: number;
}
