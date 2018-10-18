
PR.registerLangHandler(
    PR.createSimpleLexer(
        [
         // Whitespace
         [PR.PR_PLAIN,       /^[\t\n\r \xA0]+/, null, '\t\n\r \xA0'],
         // A double or single quoted, possibly multi-line, string.
         [PR.PR_STRING,      /^(?:\"(?:[^\"])*(?:\"|$)|\'(?:[^\'])*(?:\'|$))/, null, '"\'']
        ],
        [
         // A comment is either a line comment that starts with two dashes, or
         // two dashes preceding a long bracketed block.
         [PR.PR_COMMENT, /^;[^\r\n]*/],
         // A long bracketed block not preceded by -- is a string.
         [PR.PR_KEYWORD, /^(?:ADC|AND|ASL|BCC|BCS|BEQ|BIT|BMI|BNE|BPL|BRK|BVC|BVS|CLC|CLD|CLI|CLV|CMP|CPX|CPY|DEC|DEX|DEY|EOR|INC|INX|INY|JMP|JSR|LDA|LDX|LDY|LSR|NOP|ORA|PHA|PHP|PLA|PLP|ROL|ROR|RTI|RTS|SBC|SEC|SED|SEI|STA|STX|STY|TAX|TAY|TSX|TXA|TXS|TYA|a|x|y|ADD|SUB|BGE|BLT|BGT|BLE|BNZ|BZE)\b/, null],
         [PR.PR_TYPE, /^(?:\.\w+)\b/i, null],
         [PR.PR_DECLARATION, /[@?]?[a-zA-Z0-9_]+\s*\:(?!\:)/],
         [PR.PR_DECLARATION, /^def(word|var|const|c?val)\w*\b/],
         // A number is a hex integer literal, a decimal literal, or a binary literal.
         [PR.PR_LITERAL,
          /^(?:(?:\%[01]+)|(?:\$[0-9a-f]+)|(?:[0-9]+))/i],
         // An identifier
         [PR.PR_PLAIN, /^[a-z_]\w*/i],
         // A run of punctuation
         [PR.PR_PUNCTUATION, /^[^\w\t\n\r \xA0][^\w\t\n\r \xA0\"\'\-\+=]*/]
        ]),
['inc', 's']);

// Comments are nested with (), so fake nesting by expanding this regex
// a bunch of times.
var comment = "\\(\\s[^)]*\\)"
for (let i = 0; i < 10; i++) {
  comment = comment.replace("[^)]", "(?:[^()]|\\([^)]*\\))");
}

PR.registerLangHandler(
  PR.createSimpleLexer(
    [],
    [
     [PR.PR_COMMENT, /^\s\\[^\r\n]*/, '\\'],
     [PR.PR_COMMENT, new RegExp(comment)],
     [PR.PR_TYPE, /^(?:\s\:\s+[^\s]+)/],
     [PR.PR_TYPE, /^(?:dup|drop|c?\!|c?\@)(?=\s|$)/],
     [PR.PR_KEYWORD, /^(?:to|\')\s+[^\s]+/i],
     [PR.PR_KEYWORD, /^(?:(?:if|else|then|begin|until)\b)/],
     // A number is a hex integer literal, a decimal literal, or a binary literal.
     [PR.PR_LITERAL,
      /^(?:(?:\%[01]+)|(?:[0-9a-f]+(?=\s))|(?:[0-9]+))/i],
     // An identifier
     [PR.PR_PLAIN, /^[a-z_]\w*/i],
     // A run of punctuation
     // [PR.PR_PUNCTUATION, /^[^\w\t\n\r \xA0][^\w\t\n\r \xA0\"\'\-\+=]*/]
    ]),
  ['f']);

window.addEventListener('load', function() {
  const SCALE = 4;
  const colors = ["#000000", "#333333", "#666666", "#FFFFFF"];
  const elts = Array.from(document.querySelectorAll("script[type='application/russellsprouts-chr']"));
  for (const elt of elts) {
    fetch(elt.getAttribute('src')).then(response => {
      return response.blob()
    }).then(blob => {
      const reader = new FileReader();
      reader.onload = function() {
        const buffer = reader.result;
        console.log(reader.result);
        const canvas = document.createElement('canvas');
        const ctx = canvas.getContext('2d');
        canvas.width = 16*8*SCALE;
        canvas.height = Math.floor(buffer.length / 16 / 16) * 8 * SCALE;
        elt.parentNode.insertBefore(canvas, elt);

        for (let tile = 0; tile < Math.floor(buffer.length / 16); tile++) {
          const tileX = (tile % 16) * 8;
          const tileY = Math.floor(tile / 16) * 8;
          for (let y = 0; y < 8; y++) {
            let plane0 = buffer.charCodeAt(tile * 16 + y);
            let plane1 = buffer.charCodeAt(tile * 16 + y + 8);

            for (let x = 0; x < 8; x++) {
              const color = ((plane0 & 0x80) >> 7) | ((plane1 & 0x80) >> 6);
              ctx.fillStyle = colors[color];
              ctx.fillRect((tileX + x) * SCALE, (tileY + y) * SCALE, SCALE, SCALE);
              plane0 <<= 1;
              plane1 <<= 1;
            }
          }
        }
      };
      reader.readAsBinaryString(blob);
    });
  }
});
