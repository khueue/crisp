%   Include the contents of this file before defining any describe/2 predicates.
%   Can be achieved by a call to include/1 at the top of each file.

%   Let describe/2 be defined over several files instead of each file
%   overwriting old definitions.

:- multifile describe/2.

%   Suppress warnings about describe/2 clauses not being defined together.
%   We need this to be able to scatter tests all over the place
%   (preferably next to the predicate under test).

:- discontiguous describe/2.
