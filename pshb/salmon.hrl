%%	salmon.hrl

-define(Val(X), (fun() -> [#xmlElement{name = N, content = [#xmlText{value = V}|_]}] = X, {N,V} end)()).


-define(Attr(X),
(fun() ->
[#xmlAttribute{name = N, value = V}] = X, {N,V} end)()).

-define(XpathV(P, X), ?Val(xmerl_xpath:string(P, X))).
-define(XpathA(P, X), ?Attr(xmerl_xpath:string(P, X))).
