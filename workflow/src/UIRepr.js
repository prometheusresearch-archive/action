module.exports.UIRepr = class UI {

  constructor(name, args, typ, value, query) {
    this.name = name;
    this.args = args;
    this.typ = typ;
    this.query = query;
    this.value = value;
  }
  inspect() {
    return `UI(${this.name})`;
  }
}
