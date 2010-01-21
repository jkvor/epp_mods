%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%%
%% Traverse the list of forms and accumulate a list of record 
%% definitions.  Once the end of the attributes section has been 
%% reached, insert the expanded_record_fields/1 function which 
%% takes a record name as an argument and returns a list of field 
%% names.
%%
%% EXAMPLE USAGE:
%% 
%%%% -module(foo).
%%%% -compile({parse_transform, expand_records}).
%%%% -record(person, {age, weight, name}).
%%%% -record(pizza, {diameter, toppings, price}).
%%%% 
%%%% encode_record(Rec) ->
%%%% 	[RecName|Fields] = tuple_to_list(Rec),
%%%% 	FieldNames = expanded_record_fields(RecName),
%%%% 	lists:zip(FieldNames, Fields).
-module(expand_records).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    handle_form(Forms, [], []).
		
%% base case: all forms have been processed
handle_form([], Acc, _Attrs) -> lists:reverse(Acc);

%% encountered a record definition attribute
handle_form([{attribute,_,record,_}=Attr|Rest], Acc, Attrs) ->
	%% add record definition to list of records and continue
    handle_form(Rest, Acc, [Attr|Attrs]);

%% encountered a non-record attribute
handle_form([{attribute,_,_,_}=Attr|Rest], Acc, Attrs) ->
	%% add to accumulator and continue
	handle_form(Rest, [Attr|Acc], Attrs);

%% encountered a non-attribute form
%% but our list of attribute forms is
%% still empty, so we continue looking
%% for them
handle_form([Form|Rest], Acc, []) -> 
	handle_form(Rest, [Form|Acc], []);

%% a non-attribute form has been encountered. Insert 
%% expanded_record_fields/1 function def here.
handle_form([Form|Rest], Acc, Attrs) ->
    Fun = get_record_info_fun(Attrs),
    handle_form(Rest, [Form, Fun | Attrs ++ Acc], []).

%% create a clause for each record type that returns
%% its field names.
get_record_info_fun(Attrs) ->
    {function,0,expanded_record_fields,1,[
        {clause,0,
             [{atom,0,RecordName}],
             [],
             [lists:foldr(
                fun ({record_field,_,{atom,_,FieldName}}, Cons) -> {cons,0,{atom,0,FieldName},Cons};
                    ({record_field,_,{atom,_,FieldName},_}, Cons) -> {cons,0,{atom,0,FieldName},Cons}
                end, {nil,0}, Fields)]
        } || {attribute,_,record,{RecordName, Fields}} <- Attrs]}.
      
