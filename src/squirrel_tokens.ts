import {ContextTracker, ExternalTokenizer} from '@lezer/lr'

import {insertSemi, LineComment, BlockComment, spaces, newline} from './squirrel_lezer.grammar.terms'

const braceR = 125

export const trackNewline = new ContextTracker({
  start: false,
  shift(context, term) {
    return term == LineComment || term == BlockComment || term == spaces ? context : term == newline
  },
  strict: false
})

export const insertSemicolon = new ExternalTokenizer((input, stack) => {
  let {next} = input
  if (next == braceR || next == -1 || stack.context)
    input.acceptToken(insertSemi)
}, {contextual: true, fallback: true})
