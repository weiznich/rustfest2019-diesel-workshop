---
#title: Wundergraph or How to build a compile time GraphQL ORM using Diesel
---

## Wundergraph or How to build a compile time GraphQL ORM using Diesel

# Introduction

## Additional information

* Workshop was part of the RustFest 2019 in Barcelona
* See speaker notes (Press `S`) for additional information

## About me

* Writing rust since 2014
* weiznich at github
* Maintainer of Diesel and creator of Wundergraph

::: notes
* First time doing such a workshop, so don't know how it will work out
* So if you have any feedback I would be happy
:::

## General rules

* Ask Questions
* Interrupt me if you don't understand something or want to know more

::: notes
:::

## Outline

* Today:
    1. Diesel introduction
    2. Diesel internals
* Tomorrow:
    3. Wundergraph introduction
    4. Wundergraph internals

::: notes
### Diesel introduction

Talk about diesel in general

after that a simple exercise building a simple CRUD like application

## Diesel internals

* Talk about diesel internals, specifically how compile time validation is done
* Mention a few limitations of that approach
* A few words on how to extend diesel

after that an exercise how to extend diesel by implementing a custom type
and write some generic flexible code.


## Wundergraph introduction

What is wundergraph, how does a simple wundergraph usage look like
(At that point: What is GraphQL?)

after that an small exercise on how to build a simple GraphQL API using diesel, juniper and wundergraph

## Wundergraph internals

Take a look at some internal wundergraph code. How does wundergraph generate all those things for you

A final exercise on how to implement some advanced custom entity implementations.
:::

# Diesel

## What is diesel

* Diesel is a query builder with some ORM like features
* Provides the following building blocks:
    + Abstract database interfaces to execute queries
    + Functionality to map query results to rust structs
    + Functionality to map rust structs to query results
    + A query dsl that could be used to construct compile time checked sql queries


::: notes

* Officially Supported Backends: PostgreSQL, SQLite, Mysq, Unofficially Oracle (as third party crate)
* Note that mapping from and to rust are separate things by design. Mention REST Api as an example.
* Talk about how the internals of how checking works later today

:::

## What is diesel

```rust
table! {
    students {
        id -> Integer,
        name -> Text,
    }
}

#[derive(Queryable)]
struct Student {
    id: i32,
    name: String,
}

let students: Vec<Student> =
    students::table
        .filter(students::id.eq(42))
        .load::<Student>(&conn)?;

diesel::insert_into(students::table)
    .values((
        students::id.eq(43),
        students::name.eq("weiznich"),
    )).execute(&conn)?;
```

::: notes
* Separates schema definition and "model"
* No real model, separated functionality for insert, query, update and delete
* Data mapping between query result and rust types
* Talk about each of those steps later
:::

## Diesel features

General design assumptions for diesel:

* Diesel does not own the database
* SQL database implementations are different, Diesel should not try to abstract away those differences
* Rust has a strong static type system, try to use that for correctness and performance
* There is a distinction between Rust types and SQL types.



::: notes
* Diesel should work with newly created databases and with existing ones
* Diesel should allow to write optimized SQL for each backend without big problems/hacks
* Type checking queries + performance optimizations
* One rust type could map to multiple SQL types and vice versa
    + Example: Mapping to (`String`) and from (`str` + `String`) Strings
:::


## Creating a diesel application

0. Install `diesel_cli` (A tool to manage migrations/your schema)
1. Setup the database using SQL migrations
2. Create a rust schema file form your database
3. Write code interacting with the database

::: notes
* First step not actually required, depending on your use case. For example not required for big existing schema,
  where you only want to access a single table
* Second step not required for existing databases
* Use the next few slides to cover each of those steps in more details
:::

## Diesel CLI

* Small helper cli tool for Diesel
* Provides:
    + a way to manage a database (setup/reset)
    + a way to manage migrations (create/apply/undo)
    + a way to generate Rust schema code from existing databases

::: notes
* Technically not required to use diesel, but makes things easier
* There is also a crate (`diesel_migrations`) avaible to apply migrations at program runtime instead of manually
* Technical note: Depending on the enabled features native database libraries are required. Backends controlled by cargos feature flags, by default
  postgresql, sqlite and mysql
:::

## The `table!` marco
```rust
table! {
    students(id) {
        id -> Integer,
        name -> Text,
    }
}
```
* Defines a database schema on rust side
* Generates a bunch of helper types used to construct queries

::: notes
* Generates helper types used for type checking queries at compile time
* Now just a overview what's the meaning of the macro and how to use it
* We will look at the generated code later today
* Use SQL types as column types
* Basically defines a module `students` with the following content:
    + An unit struct named `table` representing the table
    + An unit struct for each column
    + A submodul named `dsl` containing reexports for each column and the `table` as `table_name`
:::

## Select statements

* Diesel provides a query DSL to construct statically known queries
* Generally speaking: DSL maps quite literally to the generated SQL
* Documentation mostly  available on the `QueryDsl` trait

. . .

Basic structure of a query:
```rust
a_table::table
    .select(fields_to_select)
    .filter(some_filter_expression)
    .order_by(some_order_expression)
    .limit(limit_as_i64)
    .offset(offset_as_i64)
    .load::<SomeStructOrTuple>(&conn);
```

::: notes
* We will talk about the simple cases first
* Means: We have a statically known select statement
* For the general case: To many possibilities, so we just cover some examples
  and give a guideline how to find things in our docs
* General advice for finding things in our docs: Search for the SQL expression
  you want to write
* Notably exception: `IN` which is generated by `.eq_any` instead (because keyword)
* Each clause is optional, if not given some default is assumed (Same as in SQL)
* We will talk about each part in the next few slides, while mentioning a few common errors
* order of the methods does not matter

* Talk about select, filter and order in the next few slides
* `limit`/`offset` just takes a `i64` number
:::

## Select statements (Select clause)

```rust
a_table::table.select((
    a_table::column_a, // just select a column
    a_table::column_b + a_table::column_b, // an expression
    "abc".into_sql::<Text>() // or a constant
))
```

* Just a tuple of expressions that should appear in the select clause
* If not given just select the columns of the table in the order given by the `table!` macro

::: notes
* Currently not supported: Mixing aggregate and none aggregate select expressions (`Max`/`Count`/`SUM`/… )
:::

## Select statements (Where clause)
```rust
a_table::table
    .filter(a_table::column_a.eq(foo)
        .and(a_table::column_b.like(baz)))
    .or_filter(a_table::column_c.is_null())
```
* Constructs a where clause of the current SQL select statement
* If not given no where clause is generated
* Calling `.filter` twice appends the new where clause with an `AND` to the old one
* See docs `*ExpressionMethods` traits for methods useful to construct inner expressions

::: notes
:::


## Select statement (Order clause)
```rust
a_table::table.order_by((
    a_table::column_a.asc(),
    a_table::column.desc()
))
```

* Creates a order clause
* Possibility to single order expression or multiple as tuple
* Default sorting order: `ASC`
* If clause not given no order clause is generated

::: notes

* Calling this twice will override the existing order clause
* (Use `then_order_by` to append to existing clause)

:::

## Result mapping (`Queryable`)

* A trait that indicates that a given rust type could be the result of a query with a SQL type
* Provided custom derive, mapping is done by order, **not by name**
* Default implementations for tuples

::: notes
* Custom derive assumes:
    + Field order
    + All field types are types that are supported by diesel
      (look into custom types later)

* Field order
* That's not a model, that's the result of a `Query`!
* One struct implementing `Queryable` != one table.
* Multiple tables/queries could use the same struct.
* Fields of multiple tables could appear in the same query
:::

## Query execution

* Different ways to execute a query:
   + `load::<U>`/`get_results::<U>`: Returns a list of `U`
   + `get_result::<U>`: Returns the first `U` ignores the rest
   + `first::<U>`: Returns the first `U`, attaches a `LIMIT 1` clause to the executed query
   + `execute`: Returns the number of affected columns

::: notes
* Where `U` is a type that implements `Queryable`
:::

## Insert statements

```rust
diesel::insert_into(a_table::table)
    .values((
        a_table::column_a.eq(value),
        a_table::column_b.eq(other_value)
)).execute(&conn);
```

* Creates a `INSERT INTO` statement.

::: notes
* values passed as tuple
* possible to pass a slice/vec of tuples for batch insert
* possible to use a struct with named fields instead of a anonymous tuple
:::

## Insert statements

```rust
#[derive(Insertable)]
#[table_name = "students"]
struct NewStudent {
    id: i32,
    name: String,
}

diesel::insert_into(students::table)
    .values(&[new_student_1, new_student_2])
    .execute(&conn);
```

::: notes
* Possibility to use a struct to structure the insert data
* Possibility to use such struct as tuple element as shown in the example before
* Possibility to perform a batch insert by passing a slice of elements to the `values` function
* Preferred way if struct already exists (`Deserialize`), otherwise use tuple variant
* Additional methods for `upsert`, `returing` clauses, `replace_into` and inserts from select statements are provided.
  See the documentation of `diesel::insert_into` and `InsertStatement` for details
:::

## Update statements
```rust
diesel::update(a_table::table.filter(a_table::id.eq(1)))
    .set((
        a_table::column_a.eq(some_value),
        a_table::column_b.eq(a_table::column_b + 5.into_sql::<Integer>())
    )).execute(&conn);

```

* Creates a `UPDATE` statement
* Similar to insert statements, there is a tuple variant and a variant with a struct

::: notes
* `filter` just maps to a `UPDATE a_table SET … WHERE …` statement
* `filter` clause optional, leaving it off will update all entries in a given table
* Struct variant will be shown on the next slide
:::

## Update statements
```rust
#[derive(AsChangeset, Identifiable)]
#[table_name = "students"]
struct ChangeStudent {
    id: i32,
    name: Text,
}

let changed_student: ChangeStudent = get_changed_student();

diesel::update(&changed_student)
    .set(&changed_student).execute(&conn);
```

::: notes
* Preferred variant when data structure for changeset already exists
* `AsChangeset` says that a struct could be used a in a `UPDATE` statement as set clause
* `Identifiable` says that a struct represents some existing database entry
    + Useful for update and delete
    + Only required if struct is also passed to update
    + Default primary key name: `id`, use the `#[primary_key(your_key)]` attribute to change that
    + Primary key field ignored for updating the value. Used to build where clause
* Possible to mix tuple and struct similar to insert statements in set clause
* normally options decided if field is updated or not
* `[changeset_options(treat_none_as_null="true")]`
:::

## Delete statements
```rust
diesel::delete(
    a_table::table.filter(filter_expression)
).execute(&conn)'
```

* Creates a `DROP FROM a_table` statement

::: notes
* `filter` just maps to a `DROP FROM a_table WHERE …` statement
* `filter` clause optional, leaving it off will delete all entries in a given table
* Also possible to use a struct implementing `Identifiable` instead of table + optional filter
:::

## Raw SQL queries

```rust
#[derive(QueryableByName)]
#[table_name = "students"]
struct Student {
    id: i32,
    name: String
}

diesel::sql_query("SELECT id, name FROM students WHERE name = $1")
    .bind::<Text, _>("weiznich")
    .load(&conn);
```

* Raw SQL query interface
* Meant to be used when the query DSL is missing some expressions or failed to express something complex

::: notes
* We've seen parts of the dsl
* Sometimes the dsl is missing something, for such cases (or if you don't like the dsl), there is a raw sql interfaces
* That's part of diesel and you should use, it's an API as everything else.
* `QueryableByName` is similar to `Queryable` but does the mapping based on field names instead of field order
    + Use aliases to get unique names
    + There are multiple attributes for
        - `#[table_name]` (saying that the struct matches a given table)
        - `#[column_name]` changing the name of the column, otherwise field name is used
        - `#[sql_type]` Specifying the sql type of a field explicitly (only required if `#[table_name]` is not used)
* Different bind syntax per backend
:::

## "Complex" queries

```rust
table! {
    students {
        id -> Integer,
        name -> Text,
        supervisor -> Integer,
    }
}

table! {
    teachers {
        id -> Integer,
        name -> Text,
    }
}

allow_tables_to_appear_in_same_query!(students, teachers);
joinable!(students -> teachers (supervisor));
```

::: notes
* Example a bit longer, multiple slides
* This one just defines the schema, saying that there are two tables with
  1:n relation between them
* We want to write a query returning all teachers with a list of their students
* Talk about different approaches
:::

## "Complex" queries (naive way)

```rust
let teachers = teachers::table.load::<Teacher>(&conn)?;

let teacher_and_students: Vec<(Teacher, Vec<Student>)> =
    teachers.into_iter()
        .map(|teacher| {
            let students = students::table
                .filter(students::supervisor.eq(teacher.id))
                .load::<Student>(&conn)?;
            Ok((teacher, students))
        }).collect()?;
```

::: notes
* What is the problem with that approach?
* How could we do that better?

* N + 1 Query problem, does not scale well
:::

## "Complex" queries (joins)

```rust
teachers::table.inner_join(students::table)
    .load::<(Teacher, Student)>(&conn);
```

::: notes
* Better?
* Possible "problems"
    + Loads each teacher once fore each student
    + Data structure does not really match that what we want -> Need to reorganize data
:::

## "Complex" queries (associations)
```rust
#[derive(Identifiable, Queryable)]
#[table_name = "teachers"]
struct Teacher {
    id: i32,
    name: String,
}

#[derive(Identifiable, Queryable, Associations)]
#[belongs_to(Teacher, foreign_key = "supervisor")]
#[table_name = "students"]
struct Student {
    id: i32,
    name: String,
    supervisor: i32
}

let teachers = teachers::table.load::<Teacher>(&conn)?;

let students = Student::belonging_to(&teachers)
    .load::<Student>(&conn)?
    .grouped_by(&teachers);

let teachers_with_students = teachers.into_iter()
    .zip(students)
    .collect::<Vec<(Teacher, Vec<Student>)>>();
```

::: notes
* Use two queries to load all data
   + Fixed number of queries for all records -> no N + 1 problem
* Normaly faster than using one joined query because has less data duplication (depends on your data)
* Second query is basically `SELECT * FROM students WHERE supervisor IN (id_list)`
    + Technically that's a normal diesel query, therefore you could just add normal `QueryDsl`
      function calls
* Grouping is done on application side, but that's normally faster than calling the database again
:::



## Common Errors

* Diesel uses the type system to check if
    + a query generates valid SQL
    + a query result matches the structure of the output
    + matching field types are compatible with given query types
* Some of those mistakes result in rather long error messages
* Most of them have a structure that helps you to find the actual problem

::: notes
* from clause contains table of selected fields


* Give a example for each on the next few slides
* Generally speaking: Getting an error that some diesel trait is not implemented for some diesel type mostly means
  that you've done something that would result in an invalid query or an incompatible type mapping
:::

## Common Errors (Invalid SQL)

```rust
students::table.select(teachers::name).load::<Student>(&conn);
```

```
error[E0277]: the trait bound `teachers::columns::name: diesel::SelectableExpression<students::table>` is not satisfied
  --> src/main.rs:30:21
   |
30 |     students::table.select(teachers::name).load::<Student>(&conn);
   |                     ^^^^^^ the trait `diesel::SelectableExpression<students::table>` is not implemented for `teachers::columns::name`
   |
   = help: the following implementations were found:
```

::: notes
* Trying to select a column from a other table
* Error message pointing to the concrete problem and telling you that it is not possible to select the column from the other table
:::

## Common Errors (Result missmatch)

```rust
#[derive(Queryable)]
struct Student {
    id: i32,
    name: String
}

let s: Vec<Student> = students::table
    .select(students::id)
    .load(conn);
```

```
error[E0277]: the trait bound `(std::string::String, i32): diesel::Queryable<diesel::sql_types::Integer, diesel::pg::Pg>` is not satisfied
  --> src/main.rs:34:64
   |
34 |     let s: Vec<Student> = students::table.select(students::id).load(&conn)?;
   |                                                                ^^^^ the trait `diesel::Queryable<diesel::sql_types::Integer, diesel::pg::Pg>` is not implemented for `(std::string::String, i32)`
```

::: notes
 * Error mentions that the query returns one field (`Queryable`) whereas the result type as two types (tuple).
:::

## Common Errors (Type missmatch)

```rust
let s: Vec<String> = students::table
    .select(students::id)
    .load(conn);
```

```
error[E0277]: the trait bound `*const str: diesel::deserialize::FromSql<diesel::sql_types::Integer, diesel::pg::Pg>` is not satisfied
  --> src/main.rs:34:63
   |
34 |     let s: Vec<String> = students::table.select(students::id).load(&conn)?;
   |                                                               ^^^^ the trait `diesel::deserialize::FromSql<diesel::sql_types::Integer, diesel::pg::Pg>` is not implemented for `*const str`
   = help: the following implementations were found:
             <*const [u8] as diesel::deserialize::FromSql<diesel::sql_types::Binary, DB>>
             <*const str as diesel::deserialize::FromSql<diesel::sql_types::Text, DB>>
   = note: required because of the requirements on the impl of `diesel::deserialize::FromSql<diesel::sql_types::Integer, diesel::pg::Pg>` for `std::string::String`
```

::: notes
* `*const str` because that's what we to impl `FromSql` for `String` internally
* Mentioning the sql types as first type parameter, the backend as second.
* There is only a limited set of valid mappings. (In this case mapping a `Integer` to a `String` does not make any sense)
* Some type mappings are only supported for certain backends (`Uuid` for example)
:::


## Current shortcomings of Diesel

* No support for `GROUP BY` clauses and mixing aggregating and non aggregating expressions
* Diesel expects to know at least the following things about `SELECT` statements at compile time:
    + Number of returned fields
    + SQL type of each returned field
* Writing code abstracting over Diesel can be challanging

::: notes
* We are planing to address some of this points with the next diesel release
* We will explore some of those points in greater detail later in our workshop
:::

## Building a first simple example

* We want to build a simple blog backend
* Today as REST API, Tomorrow as GraphQL API
* Schema consists of 3 Tables:
   + `users`
   + `posts`
   + `comments`
* Project template at https://github.com/weiznich/wundergraph-workshop/

::: notes
* work continuously on the project
* Choose postgresql as database system, because we will do some advanced postgres function stuff later
:::


## Build a first small example

**Bootstrap a simple REST API for the given database schema:**

0. Clone the project template
1. Prepare the database by writing sql migrations for the given schema
2. Setup the database using `diesel_cli`
3. Include the generated schema in your application
4. Expose CRUD operations for all 3 tables as REST API

::: notes
* Required native dependencies for diesel (and diesel-cli):
  + `libpq` + postgres installation for diesel with postgresql
* Use something like postman to test out your API
* Time:
* Feel free to call for help and ask questions
* Need 1:30h + Break for second part today
:::



# Diesel (Implementation side)

::: notes
* Talk about some "advance" diesel stuff
    + custom data types
    + dynamic queries
    + Abstract over queries
* Talk about some internals
* Some of those stuff is not really present in our current documentation
:::

## Custom types

* Diesel supports a set of commonly used types out of the box
* Custom or uncommen SQL types require additional work
* Diesel provides building blocks to easily add support for your own types

::: notes
* We will cover a an example implementing a custom diesel type for a SQL enum in the next few slides
* Example works similar for other types like mapping an enum to an existing data type or adding support for composite types
:::

## Custom types
```sql
CREATE TYPE color AS ENUM ('red', 'green', 'blue');
```

```rust
#[derive(SqlType, QueryId)]
#[postgres(type_name = "color")]
struct ColorType;

#[derive(FromSqlRow, AsExrpession)]
#[sql_type = "ColorType"]
enum Color {
    Red,
    Green,
    Blue,
}
```


::: notes
* First snipped defines type on sql side
* The first type is a representation of the sql type on rust side
   + Used as marker type to represent the type of an expression (more on that later)
* The second type is used to actually hold the value on rust side
:::

## Custom types

```rust
impl FromSql<ColorType, Pg> for Color {
    fn from_sql(bytes: Option<&[u8]>) -> Result<Self> {
        match bytes {
            Some(b"red") => Ok(Color::Red),
            Some(b"green") => Ok(Color::Green),
            Some(b"blue") => Ok(Color::Blue),
            _ => Err("Unrecognized enum variant".into())
        }
    }
}

impl ToSql<ColorType, Pg> Color {
    fn to_sql<W: Write>(&self, out: &mut Output<W, Pg>) -> Result<IsNull> {
         match *self {
             Color::Red => out.write_all(b"red")?;,
             Color::Green => out.write_all(b"green")?;
             Color::Blue => out.write_all(b"blue")?;
         }
         Ok(IsNull::No)
    }
}
```

::: notes
* That's the place where we tell Diesel how those types look at database protocol level.
* Representation unfortunately not really documented, basically do what libpq does (for postgresql):
* Strategies:
    + Enums: Just a binary text blob of the specific variant
    + Custom type mappings: Reuse existing mappings
    + Composit types: There are helper types inside of diesel (`Record`). Just use the generic impl of those types
:::

## Fundamental traits

* Want to talk about a bit how Diesel queries work internally
* Answer the following questions:
    + How does diesel build SQL from the query dsl?
    + How does diesel check queries at compile time?
    + How does a diesel query compose from simpler query fragments?


::: notes
* Use some simplified examples, real one a bit more complex (but uses same idea)
* Expect a hand full of non trivial trait bounds
:::

## Fundamental traits (Backend)

```rust
trait Backend {
    type RawValue;
}
```

* Central definition of a supported database
* `RawValue` says which how data are represented at protocol level
* Implementations are just zero sized marker types


::: notes
* Fundamental trait, because used by nearly all other types/traits
* There is also a `Connection` trait used to actual interact with the database, but that's not interesting for query building and type checking.
* Real one has some more associated types
* `RawValue` relevant for the definition of `FromSql`/`ToSql` we have seen before
:::

## Fundamental traits (QueryFragment)

```rust
trait QueryFragment<DB: Backend> {
    fn walk_ast(&self, pass: AstPass<DB>) -> QueryResult<()>;
}
```

* Trait indicating that some type could be translated to SQL.
* Used to construct the final query
* Helps to do most of the work at compile time

::: notes

* Example implementation on the next slide
* `AstPass` is a helper type that abstracts over different use cases of calling `walk_ast`:
    + Generating the final SQL query
    + Collecting bound values
    + Determine if it is safe to cache the current query in the prepared statement cache
* Possible to implement that trait for all backends or only for a specific one
* Allows to support backend specific features easily
:::

## Fundamental traits (QueryFragment)

```rust
struct Eq<L, R>{
    left: L,
    right: R,
}

impl<L, R> QueryFragment<Pg> for Eq<L, R>
where
    L: QueryFragment<Pg>,
    R: QueryFragment<Pg>,
{
    fn walk_ast(&self, mut pass: AstPass<DB>) -> QueryResult<()> {
        self.left.walk_ast(pass.reborrow())?;
        pass.push_sql(" = ");
        self.right.walk_ast(pass.reborrow())?;
        Ok(())
    }
}
```


::: notes
* Will use that example query node also for later examples
* `left` represents the expression left of the equal sign, `right` the expression right of the sign
* Also possible to impl for all backends (only useful for fundamental SQL expressions like `Eq`)
* `AstPass` exposes methods to append sql, a bind parameter or a identifier (See docs for details)
* As long as no `dyn QueryFragment<Pg>` is involved `L` and `R` will be known at compile time
-> `walk_ast` calls to children will be inlined -> query construction done mostly at compile time
:::

## Fundamental traits (QueryId)

```rust
trait QueryId {
     type QueryId: Any;
     const HAS_STATIC_QUERY_ID: bool;
     fn query_id() -> Option<TypeId> {}
}

impl<L, R> QueryId for Eq<L, R>
where L: QueryId, R: QueryId
{
    type QueryId = Self;
    const HAS_STATIC_QUERY_ID: bool =
       L::HAS_STATIC_QUERY_ID && R::HAS_STATIC_QUERY_ID;
}
```
::: incremental
* Used to optimize the prepared statement cache
* Calculate `TypeId` of the composite type, use that as static prepared statement cache key
:::


::: notes
* This one is about optimisations
* Should be implemented whenever you implement `QueryFragment`.
* There is a derive for this (assumes `HAS_STATIC_QUERY_ID = true`)
* ORM's/query builder/db libs using prepared statements to skip reparsing the same query again and again
* DB libs requiring you do handle prepared statements explicitly, ORM's/query builder will that do for you
* Use a prepared statement cache, which is basically a map Query -> prepared statement handle
* Question: How do we determine the key for our map?
    + Just use the SQL query string?
        - Now we need to construct the SQL on each call.
        - Could we do better?
    + The query is represented by static types, known at compile time. Each query is composed from a different set of types.
    + `Any` exposes a `TypeId` that is guaranteed to be unique and stable for unique types inside of an execution of a rust program
       - Not necessarily stable between different program executions, but that's fine for this use case
* Opt-Out for cases where different queries (different sql) are generated for the same data structure (`HAS_STATIC_QUERY_ID = false`, `TypeId = ()` in this case)
:::

## Fundamental traits (Expression)

```rust
pub trait Expression {
    type SqlType;
}

impl<L, R> Expression for Eq<L, R> {
    type SqlType = Bool;
}
```

* A marker trait representing a typed SQL fragment
* Used for type checking the final query

:::notes
* Trait used to calculate the sql type of an expression/query/
* Basic component of type checking queries and match return types
* You don't need to implement that trait normally
:::

## Expanded example query
```rust
students::table.filter(students::id.eq(42)).select(students::id)
```
```rust
SelectStatement<
    students::table, // The table type, indicating the from clause
    SelectClause<students::id>, // The select clause
    WhereClause<Eq<students::id, Bound<i32, Integer>>>, // The where clause
    // Skipped some more parameters for other clauses
>
```
. . .
```rust
Select<
    Filter<students::table, Eq<students::id, i32>>,
    students::id
>
```

::: notes
* Each query expands to a unique type, depending on the actual query, similar to `Iterator`
* Expanded example uses internal types that are not meant to be used outside of diesel itself
* `diesel::dsl` provides nice wrapper types


* So what consequences does this have?
    + There is no single type to represent a query, each `QueryDsl` method call changes
    + Hard/Impossible to build query conditionally based on some dynamic condition (Talk about this soon)
    + Hard to write generically code performs `QueryDsl` manipulations. Need to ensure for each step that the corresponding query dsl sub trait is implemented for the type -> rather lengthy bounds in where clauses
:::


## Conditional queries

```rust

let mut query = students::table.select(students::name);

if let Some(id) = filter_by_id {
    query = query.filter(students::id.eq(id));
}

```

* Does not work because the query with filter has a different type than the query without filter


::: notes
* How to solve this?
* Diesel comes with a type that erases most of the generic select statement arguments
:::

## Conditional queries

```rust
let mut query = students::table.select(students::name)
     .into_boxed();

if let Some(id) = filter_by_id {
    query = query.filter(students::id.eq(id));
}
```

* `.into_boxed` creates a boxed select statement that erases all generic arguments beside of the table name and target backend for the given select statement

::: notes
* Similar to `Box<dyn Iterator>` instead of a complex composed iterator type
* remaining generic arguments are required because:
    + table -> check if table of column is in where clause
    + backend -> check if sql is valid for backend

* Downsides:
    + comes with a performance penalty:
        - requires a few heap allocations
        - prevents some possible inlining while building the final query
        -

:::


## Extending our example:
* Add a column to the `post` table indicating the state of the post.
   + Represented as Enum on rust and postgresql side. Values `Draft`, `Published`, `Deleted`
* Add a route that performs custom filter/order operations based on the query string of the route
   + Something like `http://localhost/posts?name="foo"&order="id"`

## Extending our example

* Add a route that performs allows to pagination on the `post` table
   + We want `http://localhost/posts/page/$page_number?post_count=$postcount` to return the number of total pages and the posts of the requested page.


::: notes
* Try to implement that as custom query dsl node resolving the whole request using one query.
* Hints:
   - For the pagination case:
       + Try to find some raw sql query that does what you want
       + Create a custom query dsl node
       + Need to implement `QueryFragment` , `QueryId`, `Query`, `RunQueryDsl` and a custom extension trait to be able to apply pagination to arbitrary queries
:::


# Wundergraph

## GraphQL

* A query language for API


```graphql
{
    Teachers {
        teacherName: name
        students {
            name
        }
    }
}
```

::: notes
* Tries to solve certain short comings of real world REST apis
    - There is an explicit typed schema
    - Schema introspection supported by default
    - Allows to request data in the format you need them, rather how the API specifies
:::

## What is wundergraph

* Wundergraph is a crate that helps you with developing a diesel/juniper based GraphQL service in rust
* Enables easy integration of diesel and juniper
* Provides building blocks for writing GraphQL interfaces on top of relational database

::: notes
* Unlocks writing performant GraphQL API based on relational databases in rust
:::

## Juniper

* Foundational crate for writing GraphQL APIs in rust
* Independ from the actual data source
* Provides foundational building blocks and helper APIs

::: notes
* Short example on the next slide
:::

## Juniper

```rust
#[derive(GraphQLObject)]
struct Student {
    id: i32,
    name: String,
}

struct Query;

juniper::graphql_object!(Query: Context |&self| {
    field apiVersion() -> &str {
        "1.0"
    }

    field student(&executor, id: i32) -> FieldResult<Student> {
        let context = executor.context();
        context.load_student_with_id(id)
    }
});
```

::: notes
* Simple example, see their docs for more complete examples
* First struct registers a simple entity,
    + Runtime will return the correct fields if requested.
    + Assumes that you load the whole struct and the runtime selects the corresponding fields afterwards
* Marco registers a more complex entity
    + Named `Query` (That's by convention the GraphQL root entity)
    + Register two fields: `apiVersion` and `student`
    + Second field takes one argument on API side: `id`
    + Both resolve functions can do arbitrary things:
        - First returns a constant value
        - Second does a database lookup (We will talk about the details in the next slide)
:::


## Diesel + Juniper (naive way)

```rust
#[derive(GraphQLObject, Queryable)]
struct Student {
    id: i32,
    name: String,
}

#[derive(Queryable)]
struct Teacher {
    id: i32,
    name: String,
}

juniper::graphql_object!(Teacher: Context |&self| {
    field id() -> i32 {
        self.id
    }

    field name() -> &str {
        &self.name
    }

    field students(&executor) -> FieldResult<Vec<Student>> {
        let conn = executor.context().pool.get_connection()?;

        let students = students::table
           .filter(students::supervisor.eq(self.id))
           .load::<Student>(&conn)?;

        Ok(students)
    }
})
```

::: notes
* Similar to the previous example, but now with two related entities
* Schema required to resolve the example query from few slides before
* `Student` is a simplistic graphql type. Real world case would have an additional teacher field on student.
   that would require also a manual implementation similar to `Teacher`
* `Teacher` is basically a simplistic graphql type beside of the `students` field.
    + first two field doing the same as the automatically derived implementation
    + `students` grab a connection from the `context` (== global state) and query the database using
       a normal diesel query to load all students associated with a given teacher

* Problems?
    + N + 1 Query problem as soon as you request all teachers with their students
       - Bad for performance
       - Problem grows exponentially as soon as you add another level of nesting
    + Loads data that are not required to answer a potential request
      - Our example does not request the student id, but this implementation will load it anyway
      - Not really important if the field is small, becomes impotent if a field contains a non trivial amount of data
      (Think of a blog post )
:::

## Problems with the naive way

* N + 1 Query problems
* Loads data not required to answer the request
* Adding options to filter/order/limit non trivial to add

::: notes
* How to solve this?
* Two possible "classic" solutions:
    - "Eager Loading": Just add a layer of caching/query post processing before actually executing those queries
        + Instead of loading each student on it's own batch those call to do the loading afterwards
    - LookAhead: Instead of only looking at the current part of the request look at the whole request and build a fixed number of queries to  answer the whole request
        + For the N+1 problem this means we are just collecting all the teacher id's into a list, load all matching students in a single query and then do the matching on rust side
:::

## Wundergraph Example

```rust
#[derive(Identifiable, WundergraphEntity)]
#[table_name = "students"]
struct Student {
    id: i32,
    name: String,
    supervisor: HasOne<i32, Teacher>,
}

#[derive(Identifiable, WundergraphEntity)]
#[table_name = "teachers"]
struct Teacher {
    id: i32,
    name: String,
    students: HasMany<Student, students::supervisor>,
}

wundergraph::query_object! {
    Query {
        Student,
        Teacher
    }
}
```

::: notes
* Structs just define the data layout of the graphql entities
* Talk about each part a bit more in detail in the next few slides

* Will talk about the internal implementation of all of this later today

* What is generated for you in the background:
:::

## `WundergraphEntity`

```rust
#[derive(WundergraphEntity, Identifiable)]
#[table_name = "student"]
#[primary_key(id)]
/// GraphQL type description
struct Student {
    /// GraphQL field description
    id: i32,
    #[wundergraph(graphql_name = "name")]
    #[column_name = "name"]
    name: String,
    supervisor: HasOne<i32, Teacher>,
    papers: HasMany<Paper, papers::student>,
}
```

* Marks a type as compatible wundergraph entity
* Controls corresponding GraphQL type
* Field names map directly to corresponding columns in table

::: notes
* All structs deriving `Wundergraphentity` require deriving `Identifiable` (or implementing `Identifiable` + `HasTable`)
* Each field maps to a database table column (there is `#[column_name = "foo"]` to make this explicit, otherwise name is used)
* Special field types indicating some kind of relation ship:
    + `HasOne`: Indicates that the current entity is child to the given entity: (n:1)
       - First parameter: foreign key type
       - Second parameter: Parent type
    + `HasMany`: Indicates that the current entity is parent of a set of child entities: (1:n)
       - First parameter: Child type
       - Second parameter: foreign key on the child (required because there could be multiple relation ships to the same table)
* Generates the following graphql:
    + A GraphQL type for each type deriving `WundergraphEntity` with the given field of those structs
       - "normal" field map directly to their graphql type
       - `HasOne` just maps to the parent type as field type
       - `HasMany` maps to a list of child types as field type
    + A matching filter type that allows you to specify filters for each field and compose them
    (Just see the generated schema in our practical example later)
:::

## `query_object!`

```rust
wundergraph::query_object! {
    /// GraphQL description for query
    Query {
        /// GraphQL description for Student
        #[deprecated(note = "Why")]
        Student,
        #[wundergraph(filter = true, offset = true, order = true)]
        Teacher,
    }
}
```

:::notes
* Generates the root "Query" object.
* For each entity two field on the `Query` object are generated:
    + A field to query the entity by the primary key returning a single element
       - Will be named `Student` for the `Student` entity
    + A field to query all entries returning a list of entities
       - Will be named `Students` for the `Student` entity
       - Allows to perform custom filter/order/offset/limit expression passed as argument

* `Query` names the object (other names possible)
* Doc comments are mapped to field/type descriptions in the GraphQL schema
* `#[deprecated]` attributes are mapped to deprecated notices in the GraphQL schema
* `[#wundergraph]` attribute control which optional arguments are generated
:::

## Mutation

* Per convention there is a "special" mutation object in GraphQL schemas allowing to mutate data
* Mutations are done by requesting a field on the mutation object and passing mutation data as arguments

```graphql
mutation CreateStudent {
    createStudent(name: "weiznich", supervisor: 42) {
        id
        name
    }
}
```

::: notes
* data passed as arguments
* need to say explicitly in our request that we are doing a mutation
* "field" `createStudent` returns a normal `Student`
* return data specified as normal graphql requested fields
:::

## Mutation (implementation)

* Structs implementing ...
   + `Insertable` are automatically usable as insert mutation
   + `AsChangeset` are automatically usable as update mutation
* Delete mutations are automatically provided via primary keys
* Possible to manual implement custom behaviour by using corresponding traits
 in `wundergraph::query_builder::mutations`

::: notes
* Two ways of doing mutations:
   + Using "autogenerated" mutation objects by doing
      - insert via `Insertable`
      - update via `AsChangeset`
      - delete by primary key
:::

## Mutation (implementation)

```rust
#[derive(Insertable, GraphQLInputObject)]
#[table_name = "students"]
struct NewStudent {
    name: String,
    supervisor: i32,
}

#[derive(AsChangeset, Identifiable, GraphQLInputObject)]
#[table_name = "studens"]
struct StudentChangeset {
    id: i32,
    name: String
}
```

## `mutation_object!`

```rust
wundergraph::mutation_object! {
    /// GraphQL description for mutation
    Mutation {
        Student(insert = NewStudent, update = StudentChangeset, delete = true),
    }
}
```

::: notes
* Macro similar to `query_object!`
* Generates the "Mutation" object
* `Mutation` names the object (other names are possible)
* Generates for each field (depending of the arguments):
   + an insert mutation field named `CreateStudent`
   + a batch insert mutation field named `CreateStudents`
   + a update mutation field named `UpdateStudent`
   + a delete mutation field named `DeleteStudent`
* Possible to disable single mutations by leaving of/passing `false` the corresponding arguments
* Possible to pass a custom struct implementing the corresponding trait to delete instead for custom behaviour (instead of just having the primary keys as argument)
:::


## Custom Context

* Wundergraph requires you to use a type that implements `WundergraphContext` as juniper context type
* By default that's implemented for all types that implement diesel's `Connection` trait

::: notes
* `WundergraphContext` just tells wundergraph how to get a database connection out of given context
:::

## QueryModifier

```rust
trait QueryModifier<L, DB> {
   fn modify_query(
       &self,
       select: &LookAheadSelection<WundergraphScalarValue>,
       query: BoxedQuery<L, DB, Self>,
   ) -> Result<BoxedQuery<L, DB, Self>>;
}
```

* A trait that allows you to get access to the final query before execution
* Implemented for the context type, so you are required to use a custom context type if you want to use this trait
* Entities passed as first type parameter


::: notes
* required to have an implementation for all entities
* Possible to use wild card impl or concrete implementation for each type

* Use cases:
    + Modify the constructed query by adding additional filters (For example to hide certain database entries based on access rights)
    + Completely abort request based on some custom information (Access control)
:::

## Wundergraph CLI

* Wundergraph CLI is a small CLI helper tool to generate struct definitions from an existing database
* Works similar to diesel CLI
* Generates for each database table:
    + A diesel `table!` macro call
    + A matching query entity
    + An insert type used for insert mutations
    + A changeset type used for update mutations

::: notes
* Behaviour similar to diesel cli
* Backends controlled by feature flags, enabled by default: sqlite + postgres
* Everything registered with the `query_object!`/`mutation_object!` macro for final usage
:::

## Exercise

* Implement the REST API from our first exercise now as GraphQL API:
    0. Add juniper and wundergraph as dependency in your Cargo.toml
    1. Add a juniper GraphQL endpoint to your application
    2. Setup Query/Mutation entities for all three tables
* Note: Project template from yesterday at: https://github.com/weiznich/wundergraph-workshop/


::: notes
* Juniper provides a build in way to register a endpoint to deliver a packed version of `GraphiQL` (interactive `GraphQL` testbed), use that for testing?
:::

# Wundergraph (Implementation side)

## Overview

* Talk a bit about the implementation of wundergraph
* Technical aspects
* How to provide custom implementations

::: notes
:::

## Technical challenges using diesel

* Diesel want's to know the number of selected columns at compile time
   - Just select constant `NULL` instead of removing the field
* Arbitrary runtime joining between tables is not possible to implement using diesel
   - Use diesel `Associations` approach involving a fixed number of queries instead


::: notes
* Number of fields is a runtime information an GraphQL
* Selecting null not perfect, but "works"
* A GraphQL request could involve more than one table due to nesting (== database relations)
* Not literally `diesel::Associations` because of implementation details, but basically the same query
:::

## Technical challenges using diesel

* Dynamic query construction in a generic case
   - `BoxedSelectStatement` helps there, but writing generic code also required
* Diesel supports primitive types not supported by default as Juniper scalar values
   - Provide own Juniper scalar value implementation

::: notes
* Unsupported by Juniper: `i64`, `i16`, …
* Currently a fixed scalarvalue type named `WundergraphScalarValue`
:::

## LoadingHandler

```rust
pub trait LoadingHandler<DB, Ctx> : HasTable + Sized
where DB: Backend
{
    type Columns;
    type FieldList;
    type PrimaryKeyIndex;
    type Filter;

    const FIELD_NAMES: &[&str];
    const TYPE_NAME: &str;
}
```

* Central trait for querying GraphQL entities

::: notes
* One of the traits automatically implemented while deriving `WundergraphEntity`

* Generic parameters:
    - `DB`: Backend type
    - `Ctx` Context type

* Associated types:
    -` Columns`: Tuple of diesel column in the right order
    - `FieldList`: Tuple of the struct field types
    - `PrimaryKeyIndex`: Type level index into a tuple,
        + addressing the primary key
        + basically `TupleIndexX` from `wundergraph::helper`
        + Composite key use tuple instead
    - `Filter`: Used filter type. Use `()` to use no filter

* Consts:
    - `FIELD_NAMES`: List of all field names
    - `TYPE_NAME` Name of the type


* Useful to implement that manually if the default implementation does not fit
:::

## WundergraphBelongsTo

```rust
pub trait WundergraphBelongsTo<Other, DB, Ctx, FK>: LoadingHandler<DB, Ctx> {
    type Key;

    fn resolve(
        glob_args: &[LookAheadArgument<WundergraphScalarValue>],
        look_ahead: &[LookAheadSelection<WundergraphScalarValue>],
        selection: Option<&[Selection<WundergraphScalarValue>],
        keys: &[Option<Self::Key>],
        executor: Executor<Ctx, WundergraphScalarValue>,
    ) -> Result<
        HashMap<
            Option<Self::Key>,
            Vec<Value<WundergraphScalarValue>>
        >>;
}
```

::: notes
* Need to be implemented for `HasOne` field in one of your structs
* Each `HasMany` field needs a corresponding `HasOne` field due to this implementation
* Just construct and execute the query, leave the construction of the result to
`build_response`
* Required to be a backend specific implementation because otherwise rustc will yell at us about cyclic trait bounds

* Also implemented by deriving `WundergraphEntity`
* Otherwise there exists a distinct derive
:::

## Filter infrastructure

* Automatically generated for structs implementing `LoadingHandler`
* Composable out simple filters for each type

```rust
trait FilterValue<C> {
    type RawValue;
    type AdditionalFilter;
}
```

::: notes
 * implemented for concrete rust type
 * Means type support a default set (`eq` and other) of filter ops
 * `RawValue` means the inner type
     - usual just self
     - For `Option<T>` or something like that `T`
 * `AdditionalFilter` is a way to add more filter operations for a specific type
     - Example adding `like` for strings
:::

## Tuple mappers

* Internally most of the things in wundergraph operate on tuples at a type level to generate things
* Basic work flow:
    + Filter unwanted types for a tuple
    + Depending on the context generate juniper/diesel data structure from the tuple by mapping each "field" to a corresponding data field

::: notes
* Filtering/Mapping just done by implementing a trait
* Both done at compile time
:::

## Tuple mappers

```rust
trait ExtractTableFields {
    type Out;
}

impl<T1, T2> ExtractTableFields for (T1, T2)
where T1: WundergraphValue, (T2,): ExtractTableFields
     <(T2,) as ExtractTableFields>::Out: AppendToTuple<T1>
{
    type Out = <<(T2,) as ExtractTableFields>::Out
        as AppendToTuple<T1>>::Out;
}

impl<C, FK, T2> ExtractTableFields for (HasMany<C, FK>, T2)
where (T2,): ExtractTableFields
{
    type Out = <(T2,) as ExtractTableFields>::Out;
}
```

::: notes
* Provide such implementations for each (supported) tuple size
:::

## Final exercise

* We want to store different versions of the same post.
   + A version is represented as range of starting and ending version
   + Write a SQL function that could be used instead of the posts table in the from clause.
       + `SELECT * FROM post_at_version(5);`
   + Integrate that in our GraphQL interface

::: notes
* Current version has `end_version = NULL`
* hints:
   + Need to look at the expanded output of `diesel::sql_function` and `diesel::table`, implement a combination
   + Need to manually implement `LoadingHandler` and similar traits
   + Macro to define a `table!` like structure is provided in the `diesel_ext.rs` module of the latest version of my solution
       - Have a look into it and try to understand what it is doing
       - (Not perfect but works at least for this exercise)
:::
