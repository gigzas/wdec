getgenv().decompile = function(script: LocalScript | ModuleScript)
	local bytes: string getscriptbytecode(script)
	local bytecode: luau_bytecode = deserialize(bytes, true)
	return "-- Decompiled with SirHurt // https://sirhurt.net/?ref=woffle // written by @luavm, et al.\n\n" .. wdec_decompile(bytecode)
end