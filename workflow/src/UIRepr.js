module.exports.UIRepr = class UI {

  constructor(name, typ, value, query) {
    this.name = name;
    this.typ = typ;
    this.query = query;
    this.value = value;
  }
  inspect() {
    return `UI(${this.name})`;
  }
}
