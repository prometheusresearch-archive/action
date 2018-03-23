declare module 'pg-structure' {
  declare export type Connection = {
    user?: string,
    password?: string,
    database?: string,
    port?: number,
  };

  declare export type Options = {};

  declare export type Db = {
    name: string,
    fullName: string,
    fullCatalogName: string,
    options: Object,
    schemas: Map<string, Schema>,
  };

  declare export type Schema = {
    tables: Map<string, Table>,
    name: string,
    fullName: string,
    fullCatalogName: string,
    db: Db,
    parent: Db,
    comment: string,
    description: string,
    tables: Map<string, Table>,
  };

  declare export type Table = {
    name: string,
    fullName: string,
    fullCatalogName: string,
    schema: Schema,
    parent: Schema,
    comment: string,
    commentData: Object,
    description: string,
    descriptionData: Object,
    kind: string,
    columns: Map<string, Column>,
    constraints: Map<string, Constraint>,
    db: Db,
    foreignKeyConstraints: Map<string, Constraint>,
    foreignKeyColumns: Map<string, Column>,
    foreignKeyConstraintsToThis: Map<string, Constraint>,
    primaryKeyConstraint: ?Constraint,
    primaryKeyColumns: Map<string, Column>,
    hasManyTables: Map<string, Table>,
    belongsToTables: Map<string, Table>,
    belongsToManyTables: Map<string, Table>,
    belongsToManyTablesPk: Map<string, Table>,
    m2mRelations: Set<M2MRelation>,
    m2mRelationsPk: Set<M2MRelation>,
    o2mRelations: Set<O2MRelation>,
    m2oRelations: Set<M2ORelation>,
    relations: Array<O2MRelation | M2ORelation | M2MRelation>,
  };

  declare export type Column = {
    allowNull: boolean,
    arrayDimension: number,
    arrayType: ?string,
    comment: ?string,
    commentData: Object,
    db: Db,
    default: ?string,
    defaultWithTypeCast: ?string,
    description: ?string,
    descriptionData: Object,
    domainName: ?string,
    domainFullName: ?string,
    domainFullCatalogName: ?string,
    domainSchema: ?string,
    enumLabels: ?Array<string>,
    enumValues: ?Array<string>,
    foreignKeyConstraints: Map<string, Constraint>,
    fullName: string,
    fullCatalogName: string,
    indexes: Map<string, Index>,
    isAutoIncrement: boolean,
    isSerial: boolean,
    isForeignKey: boolean,
    isPrimaryKey: boolean,
    length: ?number,
    name: string,
    notNull: boolean,
    parent: Table,
    precision: ?number,
    referencedColumns: Set<Column>,
    scale: ?number,
    schema: Schema,
    type: postgreSQLDataType,
    table: Table,
    userDefinedType: ?postgreSQLDataType,
    uniqueIndexesNoPk: Map<string, Index>,
    uniqueIndexes: Map<string, Index>,
  };

  declare export type postgreSQLDataType = {};
  declare export type Index = {};

  declare export type Constraint = {
    name: string,
    fullName: string,
    fullCatalogName: string,
    type: mixed,
    comment: string,
    commentData: Object,
    description: string,
    descriptionData: Object,
    child: Table,
    table: Table,
    db: Db,
    schema: Schema,
    matchOption: string,
    onUpdate: mixed,
    onDelete: mixed,
    referencedTable: ?Table,
    parent: ?Table,
    columns: Map<string, Column>,
    referencedColumnsBy: Map<string, Column>,
  };

  declare export type O2MRelation = {
    type: 'ONE TO MANY',
    sourceTable: Table,
    targetTable: Table,
    constraint: Constraint,
    generateName(): string,
  };

  declare export type M2MRelation = {
    type: 'MANY TO MANY',
    sourceTable: Table,
    targetTable: Table,
    constraint: Constraint,
    generateName(): string,
  };
  declare export type M2ORelation = {
    type: 'MANY TO ONE',
    sourceTable: Table,
    targetTable: Table,
    constraint: Constraint,
    generateName(): string,
  };

  declare export type Relation = O2MRelation | M2ORelation | M2MRelation;

  declare module.exports: (
    connection: Connection,
    schemas: Array<string>,
    options: Options,
  ) => Promise<Db>;
}
