-module(fp).

-export([fingerprint_file/1,
	 cmdfingerprint_file/1,
	 fingerprint_file/2]).

cmdfingerprint_file(File) ->
	Str = os:cmd(string:join(["ssh-keygen -l -f ", File, "| awk '{print $2}'"], " ")),
	{ok, io_lib:format(Str,[])}.

fingerprint_file(File) ->
	{ok, SshBin} = file:read_file(File),
	fingerprint_bin(SshBin, openssh_public_key).

fingerprint_file(File, Type) ->
	{ok, SshBin} = file:read_file(File),
	fingerprint_bin(SshBin, Type).

fingerprint_bin(SshBin, Type) ->
	[{KeyBin,_}] = public_key:ssh_decode(SshBin, Type),
	[_ ,TargetKey|_] =string:tokens(erlang:binary_to_list(public_key:ssh_encode([{KeyBin,[]}], openssh_public_key))," "),
	Md5 = binary_to_list(crypto:hash(md5, base64:decode_to_string(TargetKey))),
        {ok, lists:flatten([io_lib:format("~2.16.0b:", [N]) || N <- Md5])}.
