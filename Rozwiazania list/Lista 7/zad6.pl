
s --> [].
s --> [a], s, [b].
% expand_term((s-->"a",s,"b"|""),X).
% phrase(s,X) - czy X nalezy do gramatyki S
s2 --> [a], s2, [b],!.
s2 --> []. 
% Z liczeniem
s3(0) --> [].
s3(X) --> [a], s3(Y),{X is Y + 1}, [b].
% Wywoływanie: phrase(s3(X), [a,a,b,b]).