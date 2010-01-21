## General Usage

### parse_transform compile directive

You can force the erlang compiler to execute a parse_transform 
by either including a -compile({parse_transform}, mymodule) attribute
in your module or by using the +"{parse_transform, mymodule}" flag
with erlc.

	-module(foo).
	-compile(export_all).
	-compile({parse_transform, custom_guards}).
	
--or--

	erlc -o ebin +"{parse_transform, custom_guards}" foo.erl

## custom_guards.erl

### USAGE
adding a parse_transform compile directive to your module
for custom_guards makes the following guards available:

is_member(Key, Values)
	Key = term()
	Values = [term()]

is_required(Terms)
is_required(Terms, Opts)
	Terms = [any()] %% a list of variables to check
	Opts = [ %% a proplist of options
		{mode, active | passive}
		{reject, [undefined]}
	]
	
### Modes
__active__:  This is the default. Active mode will throw
an error if a required field does not have
a value. Errors are of the format:
{error, {field_cannot_be_undefined, FieldName::atom()}}
__passive__: Passive mode adds guards to the function clause to
ensure that execution will not enter that function
if one of the required fields does not have a value.

### Reject
The reject list specifies values to be considered equivalent
to the 'undefined' atom. For instance, passing in a reject 
option of {reject, ["", undefined, 0]} will cause input
parameters with values of "" or 0, as well as undefined, to
be considered not populated.  The default reject list is
[undefined].

### EXAMPLE

	-module(test).
	-compile(export_all).

	-compile({parse_transform, custom_guards}). %% <-- like so

	-record(bar, {baz}).
    
	foo1(A,B,C,D) when is_integer(D), is_required([A,B,C]) ->
	    ok.
    
	foo2(Bar) when is_required([Bar#bar.baz]) ->
	    ok.
    
	foo3(#bar{baz=Baz}) when is_required([Baz]) ->
	    ok.

	foo4(A) when is_member(A, [a,b,c]) ->
		ok;
	foo4(_) ->
		nok.

### TEST OUTPUT

	1> test:foo1(1,2,3,4).
	ok
	2> test:foo1(1,undefined,3,4).
	** exception exit: {error,{field_cannot_be_undefined,'B'}}
	     in function  test:foo1/4
	3> test:foo2({bar, 55}).
	ok
	4> test:foo2({bar, undefined}).
	** exception exit: {error,{field_cannot_be_undefined,baz}}
	     in function  test:foo2/1
	5> test:foo3({bar, 55}).       
	ok
	6> test:foo3({bar, undefined}).
	** exception exit: {error,{field_cannot_be_undefined,'Baz'}}
	     in function  test:foo3/1
	7> test:foo4(a).
	ok
	8> test:foo4(e).
	nok