module.exports.UIRepr = class UI {

  constructor(name, typ, query) {
    this.name = name;
    this.typ = typ;
    this.query = query;
  }
  inspect() {
    return `UI(${this.name})`;
  }
}
