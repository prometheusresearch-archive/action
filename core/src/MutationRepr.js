module.exports.MutationRepr = Mutation;

function Mutation(execute, kind) {
  this.execute = execute;
  this.kind = kind;
}

Mutation.prototype.inspect = function() {
  return 'Mutation()';
};
