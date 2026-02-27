import { Change } from '@codemirror/merge'

/*
Begin copied from https://github.com/codemirror/merge/blob/3bb0627b0c4cbab0f1eaac1513d8d15ab024acf7/src/diff.ts
 */

function findDiff(a: string, fromA: number, toA: number, b: string, fromB: number, toB: number): Change[] {
  if (a == b) return []

  // Remove identical prefix and suffix
  let prefix = commonPrefix(a, fromA, toA, b, fromB, toB)
  let suffix = commonSuffix(a, fromA + prefix, toA, b, fromB + prefix, toB)
  fromA += prefix; toA -= suffix
  fromB += prefix; toB -= suffix
  let lenA = toA - fromA, lenB = toB - fromB
  // Nothing left in one of them
  if (!lenA || !lenB) return [new Change(fromA, toA, fromB, toB)]

  // Try to find one string in the other to cover cases with just 2
  // deletions/insertions.
  if (lenA > lenB) {
    let found = a.slice(fromA, toA).indexOf(b.slice(fromB, toB))
    if (found > -1) return [
      new Change(fromA, fromA + found, fromB, fromB),
      new Change(fromA + found + lenB, toA, toB, toB)
    ]
  } else if (lenB > lenA) {
    let found = b.slice(fromB, toB).indexOf(a.slice(fromA, toA))
    if (found > -1) return [
      new Change(fromA, fromA, fromB, fromB + found),
      new Change(toA, toA, fromB + found + lenA, toB)
    ]
  }

  // Only one character left on one side, does not occur in other
  // string.
  if (lenA == 1 || lenB == 1) return [new Change(fromA, toA, fromB, toB)]

  // Try to split the problem in two by finding a substring of one of
  // the strings in the other.
  let half = halfMatch(a, fromA, toA, b, fromB, toB)
  if (half) {
    let [sharedA, sharedB, sharedLen] = half
    return findDiff(a, fromA, sharedA, b, fromB, sharedB)
      .concat(findDiff(a, sharedA + sharedLen, toA, b, sharedB + sharedLen, toB))
  }

  // Fall back to more expensive general search for a shared
  // subsequence.
  return findSnake(a, fromA, toA, b, fromB, toB)
}

let scanLimit = 1e9
let timeout = 0

// Implementation of Myers 1986 "An O(ND) Difference Algorithm and Its Variations"
function findSnake(a: string, fromA: number, toA: number, b: string, fromB: number, toB: number): Change[] {
  let lenA = toA - fromA, lenB = toB - fromB
  if (scanLimit < 1e9 && Math.min(lenA, lenB) > scanLimit * 16 ||
    timeout > 0 && Date.now() > timeout) {
    if (Math.min(lenA, lenB) > scanLimit * 64) return [new Change(fromA, toA, fromB, toB)]
    return crudeMatch(a, fromA, toA, b, fromB, toB)
  }
  let off = Math.ceil((lenA + lenB) / 2)
  frontier1.reset(off)
  frontier2.reset(off)
  let match1 = (x: number, y: number) => a.charCodeAt(fromA + x) == b.charCodeAt(fromB + y)
  let match2 = (x: number, y: number) => a.charCodeAt(toA - x - 1) == b.charCodeAt(toB - y - 1)
  let test1 = (lenA - lenB) % 2 != 0 ? frontier2 : null, test2 = test1 ? null : frontier1
  for (let depth = 0; depth < off; depth++) {
    if (depth > scanLimit || timeout > 0 && !(depth & 63) && Date.now() > timeout)
      return crudeMatch(a, fromA, toA, b, fromB, toB)
    let done = frontier1.advance(depth, lenA, lenB, off, test1, false, match1) ||
      frontier2.advance(depth, lenA, lenB, off, test2, true, match2)
    if (done) return bisect(a, fromA, toA, fromA + done[0], b, fromB, toB, fromB + done[1])
  }
  // No commonality at all.
  return [new Change(fromA, toA, fromB, toB)]
}

class Frontier {
  vec: number[] = []
  declare len: number
  declare start: number
  declare end: number

  reset(off: number) {
    this.len = off << 1
    for (let i = 0; i < this.len; i++) this.vec[i] = -1
    this.vec[off + 1] = 0
    this.start = this.end = 0
  }

  advance(depth: number, lenX: number, lenY: number, vOff: number, other: Frontier | null,
          fromBack: boolean, match: (a: number, b: number) => boolean) {
    for (let k = -depth + this.start; k <= depth - this.end; k += 2) {
      let off = vOff + k
      let x = k == -depth || (k != depth && this.vec[off - 1] < this.vec[off + 1])
        ? this.vec[off + 1] : this.vec[off - 1] + 1
      let y = x - k
      while (x < lenX && y < lenY && match(x, y)) { x++; y++ }
      this.vec[off] = x
      if (x > lenX) {
        this.end += 2
      } else if (y > lenY) {
        this.start += 2
      } else if (other) {
        let offOther = vOff + (lenX - lenY) - k
        if (offOther >= 0 && offOther < this.len && other.vec[offOther] != -1) {
          if (!fromBack) {
            let xOther = lenX - other.vec[offOther]
            if (x >= xOther) return [x, y]
          } else {
            let xOther = other.vec[offOther]
            if (xOther >= lenX - x) return [xOther, vOff + xOther - offOther]
          }
        }
      }
    }
    return null
  }
}

// Reused across calls to avoid growing the vectors again and again
const frontier1 = new Frontier, frontier2 = new Frontier

// Given a position in both strings, recursively call `findDiff` with
// the sub-problems before and after that position. Make sure cut
// points lie on character boundaries.
function bisect(a: string, fromA: number, toA: number, splitA: number,
                b: string, fromB: number, toB: number, splitB: number) {
  let stop = false
  if (!validIndex(a, splitA) && ++splitA == toA) stop = true
  if (!validIndex(b, splitB) && ++splitB == toB) stop = true
  if (stop) return [new Change(fromA, toA, fromB, toB)]
  return findDiff(a, fromA, splitA, b, fromB, splitB).concat(findDiff(a, splitA, toA, b, splitB, toB))
}

function chunkSize(lenA: number, lenB: number) {
  let size = 1, max = Math.min(lenA, lenB)
  while (size < max) size = size << 1
  return size
}

// Common prefix length of the given ranges. Because string comparison
// is so much faster than a JavaScript by-character loop, this
// compares whole chunks at a time.
function commonPrefix(a: string, fromA: number, toA: number, b: string, fromB: number, toB: number): number {
  if (fromA == toA || fromA == toB || a.charCodeAt(fromA) != b.charCodeAt(fromB)) return 0
  let chunk = chunkSize(toA - fromA, toB - fromB)
  for (let pA = fromA, pB = fromB;;) {
    let endA = pA + chunk, endB = pB + chunk
    if (endA > toA || endB > toB || a.slice(pA, endA) != b.slice(pB, endB)) {
      if (chunk == 1) return pA - fromA - (validIndex(a, pA) ? 0 : 1)
      chunk = chunk >> 1
    } else if (endA == toA || endB == toB) {
      return endA - fromA
    } else {
      pA = endA; pB = endB
    }
  }
}

// Common suffix length
function commonSuffix(a: string, fromA: number, toA: number, b: string, fromB: number, toB: number): number {
  if (fromA == toA || fromB == toB || a.charCodeAt(toA - 1) != b.charCodeAt(toB - 1)) return 0
  let chunk = chunkSize(toA - fromA, toB - fromB)
  for (let pA = toA, pB = toB;;) {
    let sA = pA - chunk, sB = pB - chunk
    if (sA < fromA || sB < fromB || a.slice(sA, pA) != b.slice(sB, pB)) {
      if (chunk == 1) return toA - pA - (validIndex(a, pA) ? 0 : 1)
      chunk = chunk >> 1
    } else if (sA == fromA || sB == fromB) {
      return toA - sA
    } else {
      pA = sA; pB = sB
    }
  }
}

// a assumed to be be longer than b
function findMatch(
  a: string, fromA: number, toA: number, b: string, fromB: number, toB: number,
  size: number, divideTo: number
): [number, number, number] | null {
  let rangeB = b.slice(fromB, toB)

  // Try some substrings of A of length `size` and see if they exist
  // in B.
  let best: [number, number, number] | null = null
  for (;;) {
    if (best || size < divideTo) return best
    for (let start = fromA + size;;) {
      if (!validIndex(a, start)) start++
      let end = start + size
      if (!validIndex(a, end)) end += end == start + 1 ? 1 : -1
      if (end >= toA) break
      let seed = a.slice(start, end)
      let found = -1
      while ((found = rangeB.indexOf(seed, found + 1)) != -1) {
        let prefixAfter = commonPrefix(a, end, toA, b, fromB + found + seed.length, toB)
        let suffixBefore = commonSuffix(a, fromA, start, b, fromB, fromB + found)
        let length = seed.length + prefixAfter + suffixBefore
        if (!best || best[2] < length) best = [start - suffixBefore, fromB + found - suffixBefore, length]
      }
      start = end
    }
    if (divideTo < 0) return best
    size = size >> 1
  }
}

// Find a shared substring that is at least half the length of the
// longer range. Returns an array describing the substring [startA,
// startB, len], or null.
function halfMatch(
  a: string, fromA: number, toA: number, b: string, fromB: number, toB: number
): [number, number, number] | null {
  let lenA = toA - fromA, lenB = toB - fromB
  if (lenA < lenB) {
    let result = halfMatch(b, fromB, toB, a, fromA, toA)
    return result && [result[1], result[0], result[2]]
  }
  // From here a is known to be at least as long as b
  if (lenA < 4 || lenB * 2 < lenA) return null
  return findMatch(a, fromA, toA, b, fromB, toB, Math.floor(lenA / 4), -1)
}

function crudeMatch(
  a: string, fromA: number, toA: number, b: string, fromB: number, toB: number
): Change[] {
  let lenA = toA - fromA, lenB = toB - fromB
  let result
  if (lenA < lenB) {
    let inv = findMatch(b, fromB, toB, a, fromA, toA, Math.floor(lenA / 6), 50)
    result = inv && [inv[1], inv[0], inv[2]]
  } else {
    result = findMatch(a, fromA, toA, b, fromB, toB, Math.floor(lenB / 6), 50)
  }
  if (!result) return [new Change(fromA, toA, fromB, toB)]
  let [sharedA, sharedB, sharedLen] = result
  return findDiff(a, fromA, sharedA, b, fromB, sharedB)
    .concat(findDiff(a, sharedA + sharedLen, toA, b, sharedB + sharedLen, toB))
}

function mergeAdjacent(changes: Change[], minGap: number) {
  for (let i = 1; i < changes.length; i++) {
    let prev = changes[i - 1], cur = changes[i]
    if (prev.toA > cur.fromA - minGap && prev.toB > cur.fromB - minGap) {
      changes[i - 1] = new Change(prev.fromA, cur.toA, prev.fromB, cur.toB)
      changes.splice(i--, 1)
    }
  }
}

// Reorder and merge changes
function normalize(a: string, b: string, changes: Change[]) {
  for (;;) {
    mergeAdjacent(changes, 1)
    let moved = false
    // Move unchanged ranges that can be fully moved across an
    // adjacent insertion/deletion, to simplify the diff.
    for (let i = 0; i < changes.length; i++) {
      let ch = changes[i], pre, post
      // The half-match heuristic sometimes produces non-minimal
      // diffs. Strip matching pre- and post-fixes again here.
      if (pre = commonPrefix(a, ch.fromA, ch.toA, b, ch.fromB, ch.toB))
        ch = changes[i] = new Change(ch.fromA + pre, ch.toA, ch.fromB + pre, ch.toB)
      if (post = commonSuffix(a, ch.fromA, ch.toA, b, ch.fromB, ch.toB))
        ch = changes[i] = new Change(ch.fromA, ch.toA - post, ch.fromB, ch.toB - post)
      let lenA = ch.toA - ch.fromA, lenB = ch.toB - ch.fromB
      // Only look at plain insertions/deletions
      if (lenA && lenB) continue
      let beforeLen = ch.fromA - (i ? changes[i - 1].toA : 0)
      let afterLen = (i < changes.length - 1 ? changes[i + 1].fromA : a.length) - ch.toA
      if (!beforeLen || !afterLen) continue
      let text = lenA ? a.slice(ch.fromA, ch.toA) : b.slice(ch.fromB, ch.toB)
      if (beforeLen <= text.length &&
        a.slice(ch.fromA - beforeLen, ch.fromA) == text.slice(text.length - beforeLen)) {
        // Text before matches the end of the change
        changes[i] = new Change(ch.fromA - beforeLen, ch.toA - beforeLen, ch.fromB - beforeLen, ch.toB - beforeLen)
        moved = true
      } else if (afterLen <= text.length &&
        a.slice(ch.toA, ch.toA + afterLen) == text.slice(0, afterLen)) {
        // Text after matches the start of the change
        changes[i] = new Change(ch.fromA + afterLen, ch.toA + afterLen, ch.fromB + afterLen, ch.toB + afterLen)
        moved = true
      }
    }
    if (!moved) break
  }
  return changes
}

const isSurrogate1 = (code: number) => code >= 0xD800 && code <= 0xDBFF
const isSurrogate2 = (code: number) => code >= 0xDC00 && code <= 0xDFFF

// Returns false if index looks like it is in the middle of a
// surrogate pair.
function validIndex(s: string, index: number) {
  return !index || index == s.length || !isSurrogate1(s.charCodeAt(index - 1)) || !isSurrogate2(s.charCodeAt(index))
}

/*
End copied from https://github.com/codemirror/merge/blob/3bb0627b0c4cbab0f1eaac1513d8d15ab024acf7/src/diff.ts
 */

/*
 * High level alg:
 *
 * When we see a multiline constructor `Foo(\n` we expect the fields inside it to match
 * This constructor will always end at the same indent it started with a `)`
 *
 * When we see a multiline array `[\n`
 * - If it is in the `functions` field, we run the diff on every element
 * - Otherwise, we just run the diffing algorithm within the lines of the array
 */

const constructorBeginRegex = () => /^( *)(\w+)\(\n/gm
const constructorEndRegex = (indent: number) => new RegExp(`^ {${indent}}\\),?$`, 'gm')

const fieldRegex = (indent: number) => new RegExp(`^ {${indent}}(\\w+) = (\\[|[^\\n]+)\\n`, 'gm')
const arrayFieldEndRegex = (indent: number) => new RegExp(`^ {${indent}}],?$`, 'gm')

const funcprotoBeginRegex = (indent: number) => new RegExp(`^ {${indent}}FunctionProto\\(`, 'gm')

function funcprotos_diff(constructorIndent: number, a: string, fromA: number, toA: number, b: string, fromB: number, toB: number): Change[] {
  // we find functionprotos until we're past the search range

  const searchA = funcprotoBeginRegex(constructorIndent)
  searchA.lastIndex = fromA
  const searchB = funcprotoBeginRegex(constructorIndent)
  searchB.lastIndex = fromB

  let changes: Change[] = []
  let lastAEnd = -1
  let lastBEnd = -1
  while (searchA.lastIndex < toA && searchB.lastIndex < toB) {
    const matchA = searchA.exec(a)
    const matchB = searchB.exec(b)

    if(matchA != null && matchB != null) {
      const searchEndA = constructorEndRegex(constructorIndent)
      searchEndA.lastIndex = searchA.lastIndex
      const matchEndA = searchEndA.exec(a)

      const searchEndB = constructorEndRegex(constructorIndent)
      searchEndB.lastIndex = searchB.lastIndex
      const matchEndB = searchEndB.exec(b)

      if(matchEndA !== null && matchEndB !== null) {
        changes.push(...constructor_diff(a, matchA.index, matchEndA.index + matchEndA.length, b, matchB.index, matchEndB.index + matchEndB.length))
      } else {
        return findDiff(a, fromA, toA, b, fromB, toB)
      }
    } else {
      break
    }
  }

  // if there's one not past, we have a mismatch, so we need to register it all as a change
  if(searchA.lastIndex < toA || searchB.lastIndex < toB) {
    changes.push(new Change(lastAEnd, toA, lastBEnd, toB))
  }

  return changes
}

function fields_diff(fieldIndent: number, a: string, fromA: number, toA: number, b: string, fromB: number, toB: number): Change[] {
  // if we see fieldName = [ we need to check if fieldName is "functions"
  // - if it is run a special alg
  // - if it's not just run the normal diff alg on the lines
  // otherwise just diff the lines straight up

  const searchA = fieldRegex(fieldIndent)
  searchA.lastIndex = fromA
  const searchB = fieldRegex(fieldIndent)
  searchB.lastIndex = fromB

  let changes: Change[] = []
  while(searchA.lastIndex < toA && searchB.lastIndex < toB) {
    const matchA = searchA.exec(a)
    const matchB = searchB.exec(b)

    if(matchA != null && matchB != null) {
      const fieldNameA = matchA[1]
      const fieldNameB = matchB[1]
      const fieldTypeA = matchA[2]
      const fieldTypeB = matchB[2]

      if(fieldTypeA === "[" && fieldTypeB === "[") {
        const searchEndA = arrayFieldEndRegex(fieldIndent)
        searchEndA.lastIndex = searchA.lastIndex
        const matchEndA = searchEndA.exec(a)

        const searchEndB = arrayFieldEndRegex(fieldIndent)
        searchEndB.lastIndex = searchB.lastIndex
        const matchEndB = searchEndB.exec(b)

        if(matchEndA !== null && matchEndB !== null) {
          if(fieldNameA === "functions" && fieldNameB === "functions") {
            changes.push(...funcprotos_diff(2 + fieldIndent, a, matchA.index + matchA[0].length, matchEndA.index - 1, b, matchB.index + matchB[0].length, matchEndB.index - 1))
          } else {
            changes.push(...findDiff(a, matchA.index + matchA[0].length, matchEndA.index - 1, b, matchB.index + matchB[0].length, matchEndB.index - 1))
          }
        } else {
          return findDiff(a, fromA, toA, b, fromB, toB)
        }
      } else if(fieldTypeA.endsWith("(") && fieldTypeB.endsWith("(")) {
        const searchEndA = constructorEndRegex(fieldIndent)
        searchEndA.lastIndex = searchA.lastIndex
        const matchEndA = searchEndA.exec(a)

        const searchEndB = constructorEndRegex(fieldIndent)
        searchEndB.lastIndex = searchB.lastIndex
        const matchEndB = searchEndB.exec(b)

        if(matchEndA !== null && matchEndB !== null) {
          changes.push(...fields_diff(2 + fieldIndent, a, matchA.index + matchA[0].length, matchEndA.index - 1, b, matchB.index + matchB[0].length, matchEndB.index - 1))
        } else {
          return findDiff(a, fromA, toA, b, fromB, toB)
        }
      } else {
        // in this case, the field should only be a single line
        changes.push(...findDiff(a, matchA.index, matchA.index + matchA[0].length, b, matchB.index, matchB.index + matchB[0].length))
      }
    } else {
      break
    }
  }

  return changes
}

function constructor_diff(a: string, fromA: number, toA: number, b: string, fromB: number, toB: number): Change[] {
  // if A and B are not both FunctionProtos, it's fine to just run the regular diff algorithm on them

  const searchA = constructorBeginRegex()
  searchA.lastIndex = fromA
  const matchA = searchA.exec(a)

  const searchB = constructorBeginRegex()
  searchB.lastIndex = fromB
  const matchB = searchB.exec(b)

  if(matchA !== null && matchB !== null) {
    const aIndent = matchA[1]
    const aIdentifier = matchA[2]
    const bIndent = matchB[1]
    const bIdentifier = matchB[2]

    if(aIdentifier === "FunctionProto" && bIdentifier === "FunctionProto" || aIndent == bIndent) {
      const searchEndA = constructorEndRegex(aIndent.length)
      searchEndA.lastIndex = searchA.lastIndex
      const matchEndA = searchEndA.exec(a)

      const searchEndB = constructorEndRegex(bIndent.length)
      searchEndB.lastIndex = searchB.lastIndex
      const matchEndB = searchEndB.exec(b)

      if(matchEndA !== null && matchEndB !== null) {
        const indexAfterLinebreakA = matchA.index + matchA[0].length
        const indexAfterLinebreakB = matchB.index + matchB[0].length
        const indexBeforeLinebreakA = matchEndA.index - 1
        const indexBeforeLinebreakB = matchEndB.index - 1

        return fields_diff(aIndent.length + 2, a, indexAfterLinebreakA, indexBeforeLinebreakA, b, indexAfterLinebreakB, indexBeforeLinebreakB)
      } else {
        return findDiff(a, fromA, toA, b, fromB, toB)
      }
    } else {
      return findDiff(a, fromA, toA, b, fromB, toB)
    }
  } else {
    return findDiff(a, fromA, toA, b, fromB, toB)
  }
}

export function diff_cnut(a: string, b: string): readonly Change[] {
  // we expect both texts to either be empty or start with Closure(
  // FIXME the above is not true
  if(a.length === 0 && b.length == 0) {
    return []
  } else if (a.length === 0 || b.length == 0) {
    return [new Change(0, a.length, 0, b.length)]
  } else {
    return normalize(a, b, constructor_diff(a, 0, a.length, b, 0, b.length))
  }
}
