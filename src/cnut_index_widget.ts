import {Decoration, DecorationSet, EditorView, ViewPlugin, ViewUpdate, WidgetType} from "@codemirror/view";
import {syntaxTree} from "@codemirror/language";
import { Range } from "@codemirror/state";

class IndexWidget extends WidgetType {
  constructor(readonly idx: number, readonly maxidx: number) {
    super();
  }

  toDOM(): HTMLElement {
    const code = document.createElement('code')
    code.classList.add('cm-se-indexWidget')
    code.innerText = this.idx.toString().padStart(this.maxidx.toString().length, ' ')
    return code
  }

  eq(other: IndexWidget): boolean {
    return this.idx == other.idx && this.maxidx == other.maxidx
  }
}

const indices = (view: EditorView) => {
  const accum: Range<Decoration>[] = []
  for(const {from, to} of view.visibleRanges) {
    syntaxTree(view.state).iterate({
      from, to,
      enter: (node) => {
        if(node.name === "Array") {
          const stable = node.node
          let idx = 0
          let posIdxPairs: { pos: number, idx: number }[] = []
          for(let child = stable.firstChild; child !== null; child = child?.nextSibling) {
            posIdxPairs.push({
              pos: child.from,
              idx: idx
            })
            idx++
          }
          const maxIdx = idx - 1
          for (const {pos, idx} of posIdxPairs) {
            const deco = Decoration.widget({
              widget: new IndexWidget(idx, maxIdx),
              side: -1
            })
            accum.push(deco.range(pos))
          }
        }
      },
    })
  }
  accum.sort((a, b) => a.from - b.from)
  return Decoration.set(accum)
}

export const indexWidgetPlugin = ViewPlugin.fromClass(class {
  decorations: DecorationSet

  constructor(view: EditorView) {
    this.decorations = indices(view)
  }

  update(update: ViewUpdate) {
    if (update.docChanged || update.viewportChanged ||
      syntaxTree(update.startState) != syntaxTree(update.state))
      this.decorations = indices(update.view)
  }
}, {
  decorations: v => v.decorations
})
