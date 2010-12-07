%   Required definitions for all files defining test/2.

%   Let test/2 be defined over several files instead of each file
%   overwriting old definitions.

:- multifile test/2.

%   Suppress warnings about test/2 clauses not being defined together.
%   We need this to be able to scatter tests all over the place
%   (preferably next to the predicate under test).

:- discontiguous test/2.
