module.exports.UIRepr = function UI(name, args, typ, value, query) {
  this.name = name;
  this.args = args;
  this.typ = typ;
  this.query = query;
  this.value = value;
}

UI.prototype.inspect = function() {
  return 'UI(' + this.name + ')';
};
