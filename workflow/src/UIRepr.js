module.exports.UIRepr = UI;

function UI(name, args, typ, value, query, outQuery) {
  this.name = name;
  this.args = args;
  this.typ = typ;
  this.value = value;
  this.query = query;
  this.outQuery = outQuery;
}

UI.prototype.inspect = function() {
  return 'UI(' + this.name + ')';
};
