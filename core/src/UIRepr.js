module.exports.UIRepr = UI;

function UI(screen, name, args, typ, value, parentQuery) {
  this.screen = screen;
  this.name = name;
  this.args = args;
  this.typ = typ;
  this.value = value;
  this.parentQuery = parentQuery;
}

UI.prototype.inspect = function() {
  return 'UI(' + this.name + ')';
};
