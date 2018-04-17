module.exports.MutationRepr = Mutation;

function Mutation(execute) {
  this.execute = execute;
}

Mutation.prototype.inspect = function() {
  return 'Mutation()';
};
