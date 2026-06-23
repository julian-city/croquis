// Undo / redo keyboard shortcuts
// Ctrl+Z (or Cmd+Z on Mac) triggers undo
// Ctrl+Shift+Z / Ctrl+Y (or Cmd+Shift+Z / Cmd+Y) triggers redo
// Shortcuts are suppressed when the user is typing in a form field
// so that browser-native text undo still works in inputs.
document.addEventListener('keydown', function(e) {
  var tag = document.activeElement.tagName.toLowerCase();
  if (tag === 'input' || tag === 'textarea' || tag === 'select') return;
  if (document.activeElement.isContentEditable) return;

  var ctrl = e.ctrlKey || e.metaKey;

  // Undo: Ctrl+Z (without Shift)
  if (ctrl && e.key === 'z' && !e.shiftKey) {
    e.preventDefault();
    Shiny.setInputValue('undo_click', Math.random(), {priority: 'event'});
    return;
  }

  // Redo: Ctrl+Shift+Z or Ctrl+Y
  if (ctrl && ((e.key === 'z' && e.shiftKey) || e.key === 'y')) {
    e.preventDefault();
    Shiny.setInputValue('redo_click', Math.random(), {priority: 'event'});
  }
});