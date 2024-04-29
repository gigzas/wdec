local _ing2 = 'Galactic Space Tuna'; 
local _ing1 = string.gmatch(_ing2, _ing2) 
while type(_ing2) == 'string' do wait() end 
--[[ Original, hand-written code begins here ]]
getgenv = function() return getrawmetatable(getfenv(0)).__index end
getceleryscript = function() return _ing1("getceleryscript") end
celerytest = function(a1) return _ing1("celerytest", a1) end
rconsoleprint = function(a1) _ing1("rconsoleprint", a1) end
printbanana = function() print'banana' end
printapple = function() print'apple' end
printlemon = function() print'every villain is lemons' end
getcurrentthread = function() return _ing1("getthread") end
dumpcallstack = function() _ing1("dumpcallstack") end
getrenv = function() return _ing1("getrenv") end
getreg = function() return _ing1("getreg") end
getidentity = function() return _ing1("getidentity") end
setidentity = function(a1) assert(type(a1) == "number","Number expected") return _ing1("setidentity", a1) end
getnamecallmethod = function() return _ing1("getnamecallmethod") end
setnamecallmethod = function(a1) assert(type(a1) == "string", "String expected") return _ing1("setnamecallmethod", a1) end
readfile = function(a1) assert(type(a1) == "string", "String file path expected") local result = _ing1("readfile", a1) if result == nil then error("File does not exist") else return result end end
writefile = function(a1, a2) assert(type(a1) == "string", "String file path expected") _ing1("writefile", a1, a2) end
appendfile = function(a1, a2) assert(type(a1) == "string", "String file path expected") _ing1("appendfile", a1, a2) end
delfile = function(a1) assert(type(a1) == "string", "String file path expected") _ing1("delfile", a1) end
isfile = function(a1) assert(type(a1) == "string", "String file path expected") return _ing1("isfile", a1) end
isfolder = function(a1) assert(type(a1) == "string", "String file path expected") return _ing1("isfolder", a1) end
iscclosure = function(a1) assert(type(a1) == "function", "Function expected") return _ing1("iscclosure", a1) end
islclosure = function(a1) return not iscclosure(a1); end
isreadonly = function(a1) assert(type(a1) == "table", "Table expected") return _ing1("isreadonly", a1) end
setreadonly = function(a1, a2) assert(type(a1) == "table", "Table expected") assert(type(a2) == "boolean", "Boolean expected") _ing1("setreadonly", a1, a2) end
makereadonly = function(a1) assert(type(a1) == "table", "Table expected") _ing1("makereadonly", a1) end
makewriteable = function(a1) assert(type(a1) == "table", "Table expected") _ing1("makewriteable", a1) end
hookfunction = function(a1, a2) assert(type(a1) == "function" and type(a2) == "function", ("Function expected for hookfunction, got " .. type(a1) .. " and " .. type(a2))) return _ing1("hookfunction", a1, a2) end
getclipboard = function() return _ing1("getclipboard") end
setclipboard = function(a1)  assert(type(a1) == "string", "String expected for setclipboard") _ing1("setclipboard", a1) end
httpget = function(a1, async) assert(type(a1) == "string", "String expected for url") return _ing1("httpget", a1) end
getobjects = function(a1) return { game:GetService("InsertService"):LoadLocalAsset(a1) } end
loadstring = function(a1) local f = _ing1("loadstring", a1, string.len(a1)) if type(f) ~= "function" then return function() error(f) end end return f end
getscriptbytecode = function(a1) assert(a1.ClassName == "LocalScript", "LocalScript expected") local fmtsource = _ing1("getscriptbytecode", a1, a1:GetFullName()); local res = ""; local at = 1; while at < string.len(fmtsource) do res = res .. string.char(tonumber('0x'..fmtsource:sub(at, at+1))); at = at + 2; end return res; end
getscriptclosure = function(a1) assert(a1.ClassName == "LocalScript", "LocalScript expected") local data = getscriptbytecode(a1) return loadstring(bytecode) end
disassemble = function(a1) local dis = loadstring(httpget("https://raw.githubusercontent.com/TheSeaweedMonster/Luau/main/decompile.lua"))() return dis(a1) end
decompile = function(a1) return disassemble(a1) end
--[[decompile = function(a1) assert(a1.ClassName == "LocalScript", "LocalScript expected") return _ing1("decompile", getscriptbytecode(a1)) end]]
compile = function(a1) assert(type(a1) == "string", "String expected") return _ing1("compile", a1) end
identifyexecutor = function() return "Lucid" end
mouse1down = function() _ing1("mouse1down") end
mouse1up = function() _ing1("mouse1up") end
mouse1click = function() _ing1("mouse1click") end
mouse2down = function() _ing1("mouse2down") end
mouse2up = function() _ing1("mouse2up") end
mouse2click = function() _ing1("mouse2click") end
presskey = function(a1) _ing1("presskey", a1) end
releasekey = function(a1) _ing1("releasekey", a1) end
newcclosure = function(a1) assert(type(a1) == "function", "Function expected for newcclosure") local cclosure = string.gmatch("","") _ing1("newcclosure", a1, cclosure) return cclosure end
newlclosure = function(a1) return a1 end
checkcaller = function() return (getidentity() == 7) end
getcallingscript = function(a1) a1 = a1 and a1 + 1 or 1 local func = setfenv(a1, getfenv(a1)) return getfenv(func).script end
isnetworkowner = function(a1) return gethiddenproperty(a1, "NetworkOwnerV3") end
isluau = function() return _VERSION == "Luau" end
dumpstring = function(a1) assert(type(a1) == "string", "invalid argument #1 to '?' (string expected) ", 2) return tostring("\\" .. table_concat({string_byte(a1, 1, #a1)}, "\\")) end
getsimulationradius = function() return gethiddenproperty(game:GetService("Players").LocalPlayer, "SimulationRadius") end
setsimulationradius = function(a1) assert(type(a1) == "number", "Number expected") sethiddenproperty(game:GetService("Players").LocalPlayer, "SimulationRadius", a1) end
firetouchinterest = function(a1, a2, a3) if a3 == 0 then for _,v in pairs(getconnections(a2.Touched)) do v.Function(a1) end end end --[[ ok, I cheaped out here. ]]
fireclickdetector = function(a1) --[[ this and firetouchinterest will be replaced, since they can be done using fireevent ]]
	assert(typeof(a1) == "Instance", "Instance expected")
	
    if a1.ClassName == "ClickDetector" then
        _ing1("fireclickdetector", game:GetService("Players").LocalPlayer, a1);
    else
        for _,v in pairs(a1:GetDescendants()) do
            if v.ClassName == "ClickDetector" then
                _ing1("fireclickdetector", game:GetService("Players").LocalPlayer, v);
            end
        end
    end
end

gethiddenproperty = function(a1, a2) assert(typeof(a1) == "Instance", "Instance expected") assert(type(a2) == "string", "String expected") _ing1("setpropertyflag", a1, a1.ClassName, a2, true) local x; xpcall(function() x=a1[a2]; end, warn) _ing1("setpropertyflag", a1, a1.ClassName, a2, false) return x end
gethiddenprop = gethiddenproperty;
sethiddenproperty = function(a1, a2, a3) assert(typeof(a1) == "Instance", "Instance expected") assert(type(a2) == "string", "String expected") _ing1("setpropertyflag", a1, a1.ClassName, a2, true) xpcall(function() a1[a2]=a3; end, warn) _ing1("setpropertyflag", a1, a1.ClassName, a2, false) end
sethiddenprop = sethiddenproperty;

oldrequire = require
require = function(a1)
	if typeof(a1) == "Instance" then
		if a1.ClassName ~= "ModuleScript" then
			error'ModuleScript expected'
		end
	elseif type(a1) ~= "number" then
		error'Number expected'
	end
	local old = setidentity(2);
	local worked, res = pcall(oldrequire, a1);
	setidentity(old);
	if not worked then error(res) end
	return res
end

getconnections = function(a1)
	assert(typeof(a1) == "RBXScriptSignal", "Signal expected");
    local f = function() end
    local con = a1:Connect(f);
	local fkey1, fkey2, off_fkey1 = 0, 0, 0;
	local off_fkey2 = 0xC;
    local off_next = 0x10;
    local off_state = 0x14;
	local off_scrinfo = 0x1C;
	local reg = getreg();
	for i,v in pairs(reg) do if (v == f) then if fkey1 == 0 then fkey1 = i else fkey2 = i end end end
	--print(fkey1, fkey2);
	local next = _ing1("getrawud", con);
	if not next then return {} end
	local entries = {};
	--print("Signal: ", string.format("%08X", next));
	local scrinfo = _ing1("readuint32", next + off_scrinfo);
	--print("Signal script information: ", string.format("%08X", scrinfo));
	for i = 0x30,0x80,4 do
		local fkeyptr = _ing1("readuint32", scrinfo + i);
		local fkeyread = _ing1("readuint32", fkeyptr + off_fkey2);
		if (fkeyread == fkey1 or fkeyread == fkey2) then
			off_fkey1 = i;
			--print("MATCH! function key offset: ", string.format("%02X", i));
			break;
		end
	end
	while off_fkey1 do
        next = _ing1("readuint32", next + off_next);
        if next == 0 then break end
		local entry = newproxy(true);
		local oldstate = _ing1("readuint32", next + off_state);
        local scrinfo = _ing1("readuint32", next + off_scrinfo);
		local ptr_fkey = _ing1("readuint32", scrinfo + off_fkey1);
        local fkey = _ing1("readuint32", ptr_fkey + off_fkey2);
		getmetatable(entry).__index = function(self, name) 
			if (name == "Enabled") then local state = _ing1("readuint32", next + off_state) return (state == oldstate) elseif (name == "Function") then return reg[fkey] end 
			if (name == "Fire") then return function(self, ...) _ing1("setsignalclosure", ptr_fkey + off_fkey2, ...); end end
			return nil 
		end
        getmetatable(entry).__newindex = function(self,name,value)
            if (name == "Enabled" and type(value) == "boolean") then _ing1("setsignalstate", next + off_state, value and oldstate or 0) end
            if (name == "Function" and type(value) == "function") then _ing1("setsignalclosure", ptr_fkey + off_fkey2, value); end
        end
        if (type(reg[fkey]) == "function") then table.insert(entries, entry) end
	end
    con:disconnect();
	return entries;
--[[
	if typeof(a1) ~= "RBXScriptSignal" then error'Signal expected' end
	
    local startfunc = function() end
    local startsignal = a1:Connect(startfunc)
	
	local connections = {};
	_ing1(connections, startfunc, startsignal, "getconnections");
	
	wait(100);
    startsignal:disconnect();
    startsignal = nil;
	
	for _,v in pairs(connections) do
		if not v.Function then
			v.Function = function() end
		end
	end
	
    return connections;
	]]
end


getrawmetatable = function(a1)
	if type(getmetatable(a1)) == "string" then
		_ing1("getrawmetatable", false);
		local result = getmetatable(a1);
		_ing1("getrawmetatable", true);
		if (result == getmetatable(a1)) then
			return nil;
		end
		return result;
	else
		return getmetatable(a1);
	end
end

--[[ Original, hand-written code ends here. ]]

--[[
Ah, Here are some functions every exploit has for some reason (though I Never use or care about them)
These could pretty much all be coughed up in 3 seconds if I actually wanted to make them myself.
But to those who say celery is skidded because of this, I don't give a flying fuck

"Pasting" starts now
]]
getmodules = function()
    local a = {}
    for b,c in next,getreg() do
        if type(c) == "table" then
            for d,e in next,c do
                if typeof(e) == "Instance" and e:IsA("ModuleScript") then
                    table.insert(a,e)
                end
            end
        end
    end
    return a
end

getloadedmodules = getmodules;

getscripts = function()
    local a = {}
    for b,c in next,getreg() do
        if type(c)=="table" then
            for d,e in next,c do
                if typeof(e) == "Instance" and (e:IsA("LocalScript") or e:IsA("ModuleScript")) then
                    table.insert(a,e)
                end
            end
        end
    end
    return a
end

getinstances = function()
    local a = {}
    for b,c in next,getreg() do
        if type(c) == "table" then
            for d,e in next,c do
                if typeof(e) == "Instance" then
                    table.insert(a,e)
                end
            end
        end
    end;
    return a
end

getnilinstances = function()
    local a = {}
    for b,c in next,getreg() do
        if type(c) == "table" then
            for d,e in next,c do
                if typeof(e) == "Instance" and e.Parent == nil then
                    table.insert(a,e)
                end
            end
        end
    end;
    return a
end

-- Every exploit is supposed to function the exact same FUCKING way
-- So no, I'm not going to rewrite this to do the exact same operations
-- when no one is supposed to see this in the first place
fireproximityprompt = function(obj, amount, skip)
    if obj.ClassName == "ProximityPrompt" then 
        amount = amount or 1
        local PromptTime = obj.HoldDuration
        if skip then 
            obj.HoldDuration = 0
        end
        for i = 1, amount do 
            obj:InputHoldBegin()
            if not skip then 
                wait(obj.HoldDuration)
            end
            obj:InputHoldEnd()
        end
        obj.HoldDuration = PromptTime
    else 
        error("ProximityPrompt expected")
    end
end

-- I'm just gonna leave this here exactly as it is
request = function(options)
    local Event = Instance.new("BindableEvent");
    local RequestInternal = game:GetService("HttpService").RequestInternal;
    local Request = RequestInternal(game:GetService("HttpService"), options);
    local Start = Request.Start;
    local Response;
    Start(Request, function(state, response) 
        Response = response;
        Event:Fire();
    end);
    Event.Event:Wait();
    return Response;
end

-- this was given by Rexi, though I may have to re-implement it
firesignal = function(target, signal, ...)
	local old = setidentity(2);
	for _, signal in next, getconnections(signal) do
		if islclosure(signal.Function) then
			local scr = rawget(getfenv(signal.Function), 'script')
			if scr == target then
				pcall(signal.Function, ...);
			end
		end
	end
	setidentity(old);
end

-- from Rexi
getsenv = function(scr)
	if scr == nil then return getfenv() end
    for i, v in next, getreg() do
        if type(v) == "function" and getfenv(v).script == scr then
            return getfenv(v)
        end
    end
    error("Script environment could not be found.")
end

-- from Rexi
getscriptenvs = function()
    local envs = {}
    for i, v in next, getscripts() do
        local succ, res = pcall(getsenv, v)
        if succ then
            envs[res.script] = res
        end
    end
    return envs
end

-- from Rexi
securecall = function(Closure, Spoof, ...)
      assert(typeof(Spoof) == "Instance", "invalid argument #1 to '?' (LocalScript or ModuleScript expected, got "..type(Spoof)..") ")
      assert(Spoof.ClassName == "LocalScript" or Spoof.ClassName == "ModuleScript", "invalid argument #1 to '?' (LocalScript or ModuleScript expected, got "..type(Spoof)..") ")

      local OldScript = getfenv().script
      local OldThreadID = getidentity()

      getfenv().script = Spoof
      setidentity(2)
      local Success, Err = pcall(Closure, ...)
      setidentity(OldThreadID)
      getfenv().script = OldScript

      if not Success and Err then error(Err) end
end

-- from Rexi
getallthreads = function()
    local threads = {}
    for i, v in next, getreg() do
        if type(v) == "thread" then
            table.insert(threads, v);
        end
    end
    return threads
end

-- dont even know where this came from but its here, so
getspecialinfo = function(obj)
    assert(obj and typeof(obj) == "Instance", "getspecialinfo - Instance expected.")
    local specialinfo = {
    ["Terrain"] = {
        ["SmoothGrid"] = true,
        ["MaterialColors"] = true
    },
    ["MeshPart"] = {
        ["PhysicsData"] = true,
        ["InitialSize"] = true
    },
    ["UnionOperation"] = {
        ["AssetId"] = true,
        ["ChildData"] = true,
        ["FormFactor"] = true,
        ["InitialSize"] = true,
        ["MeshData"] = true,
        ["PhysicsData"] = true
    }
    }
    local data = {}
    for i, v in next, specialinfo[obj.ClassName] do
        data[i] = gethiddenproperty(obj, i)
    end
    return data
end

-- from Rexi
clonefunction = function(p1)
    assert(type(p1) == "function", "invalid argument #1 to '?' (expected) ", 2)
    local A = p1
    local B = xpcall(setfenv, function(p2, p3)
        return p2, p3
    end, p1, getfenv(p1))
    if B then
        return function(...)
            return A(...)
        end
    end
    return coroutine.wrap(function(...)
        while true do
            A = coroutine.yield(A(...))
        end
    end)
end

-- from Nul
cloneref = function(ref)
    if type(ref) == 'userdata' then
        return ref
    else
        return error("bad argument #1 to 'cloneref' (expected userdata, got "..tostring(type(ref))..")")
    end
end



--[[
"Pasting" ends here
]]

--[[
I shouldn't have to support synapse functions in my exploit, but fuck this community
]]
syn = {
    secure_call = securecall,
    protect_gui = nil,
    request = request,
    crypt = {
        base64 = nil
    },
    crypto = {
        base64 = nil
    },
    get_thread_identity = getidentity,
    is_beta = false,
    set_thread_identity = setidentity
}



--[[ Original, hand-written code BEGINS again, starting here ]]

rnet = {};
rnet.sendQuantity = 1;

if _ing1("authorize") then
	rnet.startcapture = function()
		_ing1("rnet_capture_start");
	end

	rnet.stopcapture = function()
		_ing1("rnet_capture_stop");
	end

	rnet.oncapture = nil;

	rnet.Capture = {};
	rnet.Capture.Connect = function(self, func)
		local connection = {};
		
		rnet.oncapture = func;
		
		connection.Disconnect = function()
			rnet.oncapture = nil;
		end
		
		connection.disconnect = connection.Disconnect;
		
		spawn(function()
			local old = rnet.oncapture;
			--[[_ing1(true, "rnet_capture_mode");]]
			while rnet.oncapture == old do
				local packet = rnet.nextPacket();
				rnet.oncapture(packet);
				wait();
				--[[
				local str = "";
				for i = 1,#packet.bytes do 
					str = str .. string.format("%02X", packet.bytes[i]);
				end
				]]
				--[[
				-- WRAP WITH BLOCK COMMENT
				-- print("SENDING BYTES " .. str);
				]]
				--[[_ing1(str, "rnet_send");
				
				while _ing1("rnet_sending_packet") do 
					game:service'RenderStepped':wait();
				end]]
			end
			--[[_ing1(false, "rnet_capture_mode");]]
		end)
		
		return connection;
	end

	rnet.Capture.connect = rnet.Capture.Connect;
end

rnet.toString = function(x)
	local str = "";
	if type(x) == "table" then
		for _,v in pairs(x) do
			str = str .. string.format("%02X ", v);
		end
	elseif type(x) == "number" then
		if x <= 0xFF then
			str = string.format("%02X", x);
		else
			str = string.format("%08X", x);
		end
	elseif type(x) == "string" then
		for _,b in pairs({s:byte(1, #s)}) do
			data = data .. string.format("%02X ", b);
		end
	end
	return str;
end

rnet.toBytes = function(str)
	local bytes = {};
	local str = str:gsub(' ', '');
	str = str:gsub('\n', '');
	str = str:gsub('\t', '');
	str = str:gsub('\r', '');
	for i = 1,string.len(str),2 do
		table.insert(bytes, tonumber('0x' .. str:sub(i, i + 1)));
	end
	return bytes;
end

rnet.Send = function(x)
	local data = rnet.toString(x):gsub(' ', '');
	
	if string.len(data) == 0 then
		error'Invalid packet data'
	end
	
	_ing1("rnet_send", data);
	
	--[[ 
	We can use anything to get a packet to send.
    As long as we can change the size of it, we just
	replace its information (a byte array) with our own.
	
	So rather than send packets from scratch, we can
	just intercept a REPORT packet (with variable length) which is far easier.
	]]
	game:GetService("Players"):Chat(string.rep("_", #x));
	--[[game:GetService("Players"):ReportAbuse(nil, "_", string.rep("_", #x));]]
	
	while _ing1("rnet_sending_packet") do 
		wait();
	end
	
	return true;
end

rnet.propertyTypes = {
	["Nil"] = 0x0,
	["String"] = 0x1,
	["StringNotCached"] = 0x2,
	["ProtectedStringServer"] = 0x3,
	["ProtectedString"] = 0x4,
	["ProtectedStringEncrypted"] = 0x5,
	["ProtectedStringStudio"] = 0x6,
	["Enum"] = 0x7,
	["EnumItem"] = 0x7,
	["BinaryString"] = 0x8,
	["Bool"] = 0x9,
	["Int"] = 0xA,
	["Float"] = 0xB,
	["Number"] = 0xC,
	["Double"] = 0xC,
	["UDim"] = 0xD,
	["UDim2"] = 0xE,
	["Ray"] = 0xF,
	["Faces"] = 0x10,
	["Axis"] = 0x11,
	["BrickColor"] = 0x12,
	["Color3"] = 0x13,
	["Color3UInt8"] = 0x14,
	["Vector2"] = 0x15,
	["Vector3"] = 0x16,
	["Vector3Extended"] = 0x17,
	["Vector2UInt16"] = 0x18,
	["Vector3UInt16"] = 0x19,
	["CFrame"] = 0x1A,
	["CFrameExtended"] = 0x1B,
	["CoordinateFrame"] = 0x1B,
	["Instance"] = 0x1C,
	["Tuple"] = 0x1D,
	["Table"] = 0x1E,
	["Array"] = 0x1E,
	["Dictionary"] = 0x1F,
	["Map"] = 0x20,
	["Content"] = 0x21,
	["SystemAddress"] = 0x22,
	["NumberSequence"] = 0x23,
	["NumberSequenceKeypoint"] = 0x24,
	["NumberRange"] = 0x25,
	["ColorSequence"] = 0x26,
	["ColorSequenceKeypoint"] = 0x27,
	["Rect2D"] = 0x28,
	["PhysicalProperties"] = 0x29,
	["Int64"] = 0x2A,
	["PathWaypoint"] = 0x2B,
	["SharedString"] = 0x2C,
	["ProtectedStringLuau"] = 0x2D,
	["DateTime"] = 0x2E,
	["SmartString"] = 0x2F
};

rnet.setFilter = function(filter)
	if type(filter) == "table" then
		if #filter > 0 then
			local str = "";
			for _,v in pairs(filter) do
				str = str .. string.format("%02X", v);
			end
			_ing1("rnet_filter", str);
		else
			_ing1("rnet_clear_filter", str);
		end
	end
end

rnet.nextPacket = function()
	local data = nil;
	while data == nil do
		data = _ing1("rnet_get_packet");
		task.wait(0);
	end
	local bytes = {};
	for i = 1,string.len(data),2 do
		table.insert(bytes, tonumber('0x' .. data:sub(i, i + 1)));
	end
	local packet = {};
	packet.bytes = bytes;
	packet.size = #bytes;
	packet.id = bytes[1];
	packet.subid = bytes[2];
	return packet;
end

rnet.setPhysicsRootPart = function(instance)
	local encoding = rnet.getInstanceBytes(instance);
	local str = "";
	for i = 1, #encoding do
		str = str .. string.format("%02X", encoding[i]);
	end
	_ing1("rnet_setrootpart", str);
end

rnet.sendPhysics = function(cf)
	--[[ "cf:GetComponents()"
	Other 9 numbers is a UpVector,RightVector,LookVector and it looks like this:
	x,y,z,uv.X,rv.X,lv.X,uv.Y,rv.Y,lv.Y,-uv.Z,-rv.Z,-lv.Z
	]]
	--[[
	local uv = cf.UpVector;
	local rv = cf.RightVector;
	local lv = cf.LookVector;
	]] 
	local rx, ry, rz = cf:ToEulerAnglesYXZ()
	local c = { cf:GetComponents() };
	_ing1("rnet_sendphysics", cf.X, cf.Y, cf.Z, rx, ry, rz, c[4], c[5], c[6], c[7], c[8], c[9], c[10], c[11], c[12]);
	
	--[[
	_ing1(-lv.Z, -rv.Z, -uv.Z, lv.Y, rv.Y, uv.Y, lv.X, rv.X, uv.X, cf.Z, cf.Y, cf.X, "rnet_sendphysics");
	]]
	
	local player = game:GetService("Players").LocalPlayer;
	if player.Character then
		local rootpart = player.Character:FindFirstChild("HumanoidRootPart");
		if rootpart then
			rootpart.CFrame = rootpart.CFrame + Vector3.new(0, .02, 0);
		end
	end
end

local packetWriter do
	packetWriter = {};
	
	local bytes = {};
	local pos = 1;
	
	function packetWriter:get()
		return bytes;
	end
	
    function packetWriter:start()
		bytes = {};
    end
	
    function packetWriter:finish()
		if rnet.sendQuantity > 1 and #bytes > 1 then
			local packetSize = #bytes;
			for i = 1,rnet.sendQuantity do
				for j = 2, packetSize do
					table.insert(bytes, bytes[j])
				end
			end
		end
		table.insert(bytes, 0);
		return bytes;
    end

    function packetWriter:writeByte(val)
        table.insert(bytes, val);
    end

    function packetWriter:writeChar(val)
        table.insert(bytes, string.byte(val));
    end

    function packetWriter:writeBytes(val)
        for i = 1,#val do
            table.insert(bytes, val[i]);
        end
    end

    function packetWriter:writeString(str)
		packetWriter:writeVarInt64(string.len(str));
        for i = 1, string.len(str) do
			packetWriter:writeChar(str:sub(i, i));
        end
    end

    function packetWriter:writeUInt16LE(val)
        packetWriter:writeBytes({bit32.band(val, 0xff), bit32.band(bit32.rshift(val, 8), 0xff)});
    end

    function packetWriter:writeUInt16BE(val)
        packetWriter:writeBytes({bit32.band(bit32.rshift(val, 8), 0xff), bit32.band(val, 0xff)});
    end

    function packetWriter:writeUInt32LE(val)
        packetWriter:writeBytes({
			bit32.band(val, 0xff), 
			bit32.band(bit32.rshift(val, 8), 0xff), 
			bit32.band(bit32.rshift(val, 16), 0xff), 
			bit32.band(bit32.rshift(val, 24), 0xff)
		});
    end

    function packetWriter:writeUInt32BE(val)
        packetWriter:writeBytes({
			bit32.band(bit32.rshift(val, 24), 0xff), 
			bit32.band(bit32.rshift(val, 16), 0xff), 
			bit32.band(bit32.rshift(val, 8), 0xff), 
			bit32.band(val, 0xff)
		});
    end

    function packetWriter:writeUInt64LE(val)
        packetWriter:writeBytes({
			bit32.band(val, 0xff), 
			bit32.band(bit32.rshift(val, 8), 0xff), 
			bit32.band(bit32.rshift(val, 16), 0xff), 
			bit32.band(bit32.rshift(val, 24), 0xff),
			bit32.band(bit32.rshift(val, 32), 0xff),
			bit32.band(bit32.rshift(val, 40), 0xff),
			bit32.band(bit32.rshift(val, 48), 0xff),
			bit32.band(bit32.rshift(val, 56), 0xff)
		});
    end

    function packetWriter:writeUInt64BE(val)
        packetWriter:writeBytes({
			bit32.band(bit32.rshift(val, 56), 0xff), 
			bit32.band(bit32.rshift(val, 48), 0xff), 
			bit32.band(bit32.rshift(val, 40), 0xff), 
			bit32.band(bit32.rshift(val, 32), 0xff), 
			bit32.band(bit32.rshift(val, 24), 0xff), 
			bit32.band(bit32.rshift(val, 16), 0xff), 
			bit32.band(bit32.rshift(val, 8), 0xff), 
			bit32.band(val, 0xff)
		});
    end

    function packetWriter:writeVarInt64(val)
        local value = val;
        repeat
            local v = bit32.band(value, 0x7F);
            value = bit32.rshift(value, 7);
            if not (value <= 0) then v = bit32.bor(v, 0x80) end
            packetWriter:writeByte(v);
        until (value <= 0)
    end

    function packetWriter:writeFloatLE(val)
		local bytes = {}
		local str = string.pack("<f", val);
		for i = 1, 4 do
			table.insert(bytes, str:byte(i, i));
		end
		for i = 1, 4, -1 do
			packetWriter:writeByte(bytes[i]);
		end
    end

    function packetWriter:writeFloatBE(val)
		local bytes = {}
		local str = string.pack("<f", val);
		for i = 1, 4 do
			table.insert(bytes, str:byte(i, i));
		end
		for i = 4, 1, -1 do
			packetWriter:writeByte(bytes[i]);
		end
    end

    function packetWriter:writeDoubleBE(val)
		local bytes = {}
		local str = string.pack("<d", val);
		for i = 1, 8 do
			table.insert(bytes, str:byte(i, i));
		end
		for i = 8, 1, -1 do
			packetWriter:writeByte(bytes[i]);
		end
    end
	
	function packetWriter:writeInstance(instance)
		local peerId = 0;
		local instanceId = 0;
		if instance then
			assert(typeof(instance) == "Instance", "Instance expected for writeInstance");
			peerId = _ing1("getpeerid", instance);
			instanceId = _ing1("getinstanceid", instance);
		end
		if peerId == 0 then
			local bytes = rnet.fetchValueBytes(instance);
			packetWriter:writeBytes(bytes);
		else
			packetWriter:writeVarInt64(peerId);
			packetWriter:writeUInt32LE(instanceId);
		end
	end
	
	function packetWriter:writeEnum(enumItem)
		local bytes = rnet.fetchValueBytes(enumItem);
		table.remove(bytes, 1);
		table.remove(bytes, 1);
		packetWriter:writeBytes(bytes);
		--[[packetWriter:writeUInt16BE(_ing1("getenumid", enumItem));
		packetWriter:writeByte(enumItem.Value);]]
	end
	
	function packetWriter:writeTable(t)
		packetWriter:writeVarInt64(#t);
		for i = 1,#t do
			packetWriter:writeObject({["Value"] = t[i], ["Type"] = typeof(t[i])});
		end
	end
	
	function packetWriter:writeDictionary(t)
		local size = 0;
		for k,v in pairs(t) do size = size + 1 end
		packetWriter:writeVarInt64(size);
		for k,v in pairs(t) do
			packetWriter:writeObject({["Value"] = k, ["Type"] = typeof(k)});
			packetWriter:writeObject({["Value"] = v, ["Type"] = typeof(v)});
		end
	end
	
	-- includeType means that writeObject will first write the value type (as a byte).
	-- The value's type is required by writeObject however.
	function packetWriter:writeObject(valueData, includeType)
		local userType = typeof(valueData.Value):lower();
		local expectedType = valueData.Type:lower();
		local value = valueData.Value;
		local typeId = nil;
		
		if (expectedType == "content") then
			includeType = true;
			typeId = rnet.propertyTypes["Float"];
		elseif (expectedType == "objects") then
			expectedType = "table";
		elseif (expectedType == "object") then
			expectedType = "instance";
		elseif (expectedType == "coordinateframe") then
			expectedType = "cframe";
		end
		
		if (userType == "string") then
			--[[ in the case of a table where we dont have an
			expected type, we need to explicitly use uncached
			string type
			]]
			typeId = rnet.propertyTypes["StringNotCached"];
		elseif (userType == "enumitem") then
		--[[ Modify the expected type if the user presents an
			EnumItem, since we have no way of telling Enums apart.
			Also, if a nil value is passed that means we exceeded
			max values, HOWEVER, this is almost always due to a
			Tuple arg (basically var-args)
			]]
			expectedType = userType;
		end
		
		if (userType == "number" and (expectedType == "float" or expectedType == "int" or expectedType == "int64" or expectedType == "double")) or
           (userType == "boolean" and (expectedType == "bool")) or 
		   (userType == "table" and (expectedType == "tuple" or expectedType == "array" or expectedType == "dictionary"))
		then
			userType = expectedType;
		end
		
		if typeId == nil then
			for k,v in pairs(rnet.propertyTypes) do
				if k:lower() == expectedType then
					typeId = v;
					break;
				end
			end
		end
		
		if (includeType == nil or includeType == true) then
			if typeId then
				packetWriter:writeByte(typeId);
			else
				error(string.format("Could not resolve type id (expected %s, got %s)", expectedType, userType))
			end
		end
		
		
		if (userType ~= expectedType and expectedType ~= "content" and expectedType ~= "tuple" and expectedType ~= "enumitem" and expectedType ~= "variant") then
			error(string.format("%s expected, got %s", valueData.Type, typeof(value)))
		end
		
		--print(string.format("Expects %s, got %s. Value: ", expectedType, userType), value);
		
		if (expectedType == "nil") then
			--[[ ID was already appended, which is 0 ]]
		elseif (expectedType == "instance" or expectedType == "object") then
			packetWriter:writeInstance(value);
		elseif (expectedType == "table" or expectedType == "array" or expectedType == "objects" or expectedType == "tuple") then
			packetWriter:writeTable(value);
		elseif (expectedType == "dictionary") then
			packetWriter:writeDictionary(value);
		elseif (expectedType == "float") then
			packetWriter:writeFloatBE(value);
		elseif (expectedType == "int") then
			packetWriter:writeVarInt64(value);
		elseif (expectedType == "int64") then
			packetWriter:writeUInt64BE(value);
		elseif (expectedType == "double") then
			packetWriter:writeDoubleBE(value);
		elseif (expectedType == "bool" or expectedType == "boolean") then
			packetWriter:writeByte(value and 1 or 0);
		elseif (expectedType == "string") then
			packetWriter:writeString(value);
		elseif (expectedType == "content") then
			packetWriter:writeVarInt64(value);
		elseif (expectedType == "enum" or expectedType == "enumitem") then
			packetWriter:writeEnum(value);
			
		else
			--[[ may also be an enumitem, with this current setup... ]]
			packetWriter:writeBytes(rnet.fetchValueBytes(value));
		end
	end
end

function getEnumId(enumItem)
	return _ing1("getenumid", enumItem)
end

local packetReader do
	packetReader = {};
	
	local bytes = {};
	local pos = 1;

	function packetReader:use(data)
		pos = 1;
		bytes = data;
	end

	function packetReader:pos()
		return pos;
	end

	function packetReader:nextByte()
		local value = bytes[pos];
		pos = pos + 1;
		return value;
	end

	function packetReader:nextChar()
		return string.char(packetReader:nextByte());
	end

	function packetReader:nextUInt16BE()
		local b = {};
		for i = 1, 2 do
			table.insert(b, packetReader:nextByte());
		end
		return (
			bit32.bor(bit32.lshift(b[1], 8),
			b[2])
		);
	end

	function packetReader:nextUInt16LE()
		local b = {};
		for i = 1, 2 do
			table.insert(b, packetReader:nextByte());
		end
		return (
			bit32.bor(bit32.lshift(b[2], 8),
			b[1])
		);
	end

	function packetReader:nextUInt32BE()
		local b = {};
		for i = 1, 4 do
			table.insert(b, packetReader:nextByte());
		end
		return (
			bit32.bor(bit32.lshift(b[1], 24), 
			bit32.bor(bit32.lshift(b[2], 16),
			bit32.bor(bit32.lshift(b[3], 8),
			b[4])))
		);
	end

	function packetReader:nextUInt32LE()
		local b = {};
		for i = 1, 4 do
			table.insert(b, packetReader:nextByte());
		end
		return (
			bit32.bor(bit32.lshift(b[4], 24), 
			bit32.bor(bit32.lshift(b[3], 16),
			bit32.bor(bit32.lshift(b[2], 8),
			b[1])))
		);
	end
	
	function packetReader:nextUInt64LE()
		local b = {};
		for i = 1, 8 do
			table.insert(b, packetReader:nextByte());
		end
		return (
			bit32.bor(bit32.lshift(b[8], 56), 
			bit32.bor(bit32.lshift(b[7], 48), 
			bit32.bor(bit32.lshift(b[6], 40), 
			bit32.bor(bit32.lshift(b[5], 32), 
			bit32.bor(bit32.lshift(b[4], 24), 
			bit32.bor(bit32.lshift(b[3], 16),
			bit32.bor(bit32.lshift(b[2], 8),
			b[1])))))))
		);
	end
	
	function packetReader:nextVarInt64()
		local result = 0
		local b = 0
		local c;
		repeat
			c = packetReader:nextByte()
			local c2 = bit32.band(c, 0x7F)
			result = bit32.bor(result, bit32.lshift(c2, b))
			b += 7
		until not bit32.btest(c, 0x80)

		return result;
	end

	function packetReader:nextString()
		local result = ""
		local len = packetReader:nextVarInt64();
		for i = 1, len do
			result = result .. packetReader:nextChar();
		end
		return result;
	end
	
	function packetReader:nextFloat()
		local bytes = {};
		for i = 1, 4 do
			table.insert(bytes, packetReader:nextByte());
		end
		local str = '';
		for i = 1, 4 do
			str = str .. string.char(bytes[4 - (i - 1)]);
		end
		local value = string.unpack("<f", str);
		return value;
	end

	function packetReader:nextDouble()
		local bytes = {};
		for i = 1, 8 do
			table.insert(bytes, packetReader:nextByte());
		end
		local str = '';
		for i = 1, 8 do
			--[[ double value is always big-endian ]]
			str = str .. string.char(bytes[8 - (i - 1)]);
		end
		local value = string.unpack("<d", str);
		return value;
	end
	
	function packetReader:nextObject()
		local type = packetReader:nextByte();
		
		if type == 0x1C then --[[ Instance ]]
			return packetReader:nextInstance();
		elseif type == 0x1E then --[[ Table ]]
			local items = {};
			local count = packetReader:nextVarInt64();
			for i = 1,count do
				table.insert(items, packetReader:nextObject());
			end
			return items;
		elseif type == 0xC then --[[ Number ]]
			return packetReader:nextDouble();
		elseif type == 0x9 then --[[ Boolean ]]
			local value = packetReader:nextByte();
			if value == 1 then
				return true;
			elseif value == 0 then
				return false;
			end
		elseif type == 0x2 then --[[ String ]]
			return packetReader:nextString();
		elseif type == 0 then --[[ nil (Instance) ]]
			local t = {};
			t.peerId = 0;
			t.id = 0;
			t.bytes = { 0 };
			return t;
		end
	end

	function packetReader:nextInstance()
		local t = {};
		local startPos = pos;
		local peerId = packetReader:nextVarInt64();
		if peerId == 0 then
			t.peerId = 0;
			t.id = 0;
			t.bytes = {};
			for i = startPos, pos - 1 do
				table.insert(t.bytes, bytes[i]);
			end
			return t;
		end
		t.peerId = peerId;
		t.id = packetReader:nextUInt32LE();
		t.bytes = {};
		for i = startPos, pos - 1 do
			table.insert(t.bytes, bytes[i]);
		end
		return t;
	end
end

rnet.getInstance = function(v)
	local t = {};
	t.peerId = 0;
	t.id = 0;
	if v then
		t.peerId = _ing1("getpeerid", v);
		t.id = _ing1("getinstanceid", v);
	end
	return t;
end

rnet.getInstanceBytes = function(v)
	local bytes = { 0 };
	local old = packetWriter:get();
	packetWriter:start();
	packetWriter:writeInstance(v);
	bytes = packetWriter:get();
	if #old > 0 then
		packetWriter:start();
		packetWriter:writeBytes(old);
	end
	return bytes;
end

rnet.replay = function()
	rnet.send(packetWriter:get());
end

rnet.readEventPacket = function(packet)
	packetReader:use(packet.bytes);
	local packetId = packetReader:nextByte();
	local packetSubId = packetReader:nextByte();
	local instance = packetReader:nextInstance();
	local eventId = packetReader:nextUInt16BE();
	local player = packetReader:nextInstance();
	local numberOfArgs = packetReader:nextByte();
	local args = {};
	local pos = packetReader:pos();
	
	for i = 1, numberOfArgs do
		local value = packetReader:nextObject();
		table.insert(args, value);
	end
	
	local argBytes = {};
	for i = pos, packetReader:pos() do
		table.insert(argBytes, packet.bytes[i]);
	end
	
	return {
		playerId = player.Id,
		playerEncoding = player.Bytes,
		instanceId = instance.Id,
		instanceEncoding = instance.Bytes,
		eventId = eventId,
		argumentBytes = argBytes,
		argumentCount = numberOfArgs,
		arguments = args
	};
end

rnet.getEvents = function(instance)
	local list = {};
	local events = _ing1("getevents", instance.ClassName);
	local str = "";
	for i = 1,string.len(events) do
		if events:sub(i, i) == '&' then
			table.insert(list, str);
			str = "";
		else
			str = str .. events:sub(i, i);
		end
	end
	return list;
end

rnet.getProperties = function(instance)
	local list = {};
	local properties = _ing1("getproperties", instance.ClassName);
	local str = "";
	for i = 1,string.len(properties) do
		if properties:sub(i, i) == '&' then
			table.insert(list, str);
			str = "";
		else
			str = str .. properties:sub(i, i);
		end
	end
	return list;
end

rnet.fetchValueBytes = function(...)
	local packet;
	local remote = game:service'RobloxReplicatedStorage'.UpdatePlayerBlockList --[[ we can use anything to do this ]]

	while wait() do
		game:service'Players':Chat('');
		remote:FireServer(...);
		repeat packet = rnet.nextPacket() until packet.bytes[1] == 0x83 and packet.bytes[2] == 0x7
		rnet.packetReader:use(packet.bytes);
		rnet.packetReader:nextByte(); -- packet id
		rnet.packetReader:nextByte(); -- sub id
		local instance = rnet.packetReader:nextInstance();
		if (instance.id == rnet.getInstance(remote).id) then
			break
		end
	end
	
	local eventId = rnet.packetReader:nextUInt16BE();
	local sender = rnet.packetReader:nextInstance();
	local nArgs = rnet.packetReader:nextVarInt64();
	local valueBytes = {}
	while rnet.packetReader:pos() < #packet.bytes do
		table.insert(valueBytes, rnet.packetReader:nextByte());
	end
	if #valueBytes > 0 then
		table.remove(valueBytes, 1);
	end
	return valueBytes;
end

rnet.fireEvent = function(instance, eventName, ...)
	assert(type(eventName) == "string", "String expected for arg #2 (eventName)");
	
	local eventId = _ing1("geteventid", eventName);
	if (eventId == 0) then
		error'Event ID was not found'
	end
	
	packetWriter:start();
	packetWriter:writeByte(0x83);
	packetWriter:writeByte(0x7);
	packetWriter:writeInstance(instance);
	packetWriter:writeUInt16BE(eventId);
	
	--[[ -- TO-DO: Auto-fill?
	
	local nExpectedArgs =  _ing1("geteventargcount", eventName);
	local userArgs = {...};
	local nUserArgs = #userArgs;
	
	for i = 1, #nExpectedArgs do
		local expectedType = _ing1("geteventargtype", eventName, i);
	end
	]]
	local args = {...};
	
	if (#args > 0) then
		for i = 1,#args do
			local expectedType = _ing1("geteventargtype", eventName, i);
			if (expectedType == "Tuple") then
				local tuple = {};
				for j = i,#args do
					table.insert(tuple, args[j]);
				end
				packetWriter:writeObject({["Value"] = tuple, ["Type"] = "table"}, false);
				break;
			else
				packetWriter:writeObject({["Value"] = args[i], ["Type"] = expectedType}, false);
			end
		end
	end
	
	local bytes = packetWriter:finish();
	--print("Sending " .. rnet.toString(bytes));
	rnet.send(bytes);
end

rnet.Sit = function(seat, humanoid)
	rnet.fireEvent(seat, "RemoteCreateSeatWeld", humanoid);
end

rnet.unsit = function(seat)
	rnet.fireEvent(seat, "RemoteDestroySeatWeld");
end

rnet.fireRemote = function(remote, count, ...)
	if count < 1 then error'Expected a number higher than 0 at argument #2' end
	remote:FireServer(...);
	
	local packet;
	repeat packet = rnet.nextPacket() until packet.bytes[1] == 0x83 and packet.bytes[2] == 0x7
	
	--[[
	83 07 [01 B7 14 00 00] [09 05] [01 53 CD 35 00] [01] [1E] [02] [1C 01 B3 CD 35 00] ...
	]]
	--[[local eventInfo = rnet.readEventPacket(packet);]]
	
	local spliceBytes = {};
	
	for i = 2,#packet.bytes - 1 do
		table.insert(spliceBytes, packet.bytes[i]);
	end
	
	packet.bytes = { packet.bytes[1] };
	
	for i = 1,count - 1 do
		for j = 1,#spliceBytes do
			table.insert(packet.bytes, spliceBytes[j]);
		end
	end
	
	table.insert(packet.bytes, 0);
	rnet.send(packet.bytes);
end

--[[
--                            \/ LocalPlayer
-- 83 07 01 B7 14 00 00 09 05 01 BF 1A 07 00 01 1C 01 28 00 00 00 00
]]

rnet.Ack = function(instance, version)
	packetWriter:start();
	packetWriter:writeByte(0x83);
	packetWriter:writeByte(0xA);
	packetWriter:writeInstance(instance);
	packetWriter:writeUInt32BE(version or 0x09680150);
	--[[
	--packetWriter:writeUInt16BE(0x968);
	--packetWriter:writeByte(1);
	--packetWriter:writeByte(version);
	]]
	rnet.send(packetWriter:finish());
end

-- Only supports RightGrip weld for tools right now
rnet.newInstance = function(instance, parent)
	-- 83 02 [0A 1E 69 00 00] [01 7D] [00 06] [01 91 23 00 00]
	packetWriter:start();
	packetWriter:writeByte(0x83);
	packetWriter:writeByte(0x2);
	packetWriter:writeInstance(instance);
	packetWriter:writeUInt16BE(0x17D); --[[  instance schema? ]]
	packetWriter:writeUInt16BE(0x6); --[[ property id? ]]
	packetWriter:writeInstance(parent);
	rnet.send(packetWriter:finish());
end

rnet.setToolGrip = function(tool, rightarm, grippos)
	local handle = tool:FindFirstChild("Handle");
	if not handle then error'Tool requires a Handle' end
	
	packetWriter:start();
	packetWriter:writeByte(0x83);
	packetWriter:writeByte(0x7);
	packetWriter:writeInstance(handle);
	packetWriter:writeUInt16BE(0xFF02); --[[ event ID ]]   
	packetWriter:writeFloatBE(grippos.X);
	packetWriter:writeFloatBE(grippos.Y);
	packetWriter:writeFloatBE(grippos.Z);
	packetWriter:writeBytes({ 0x06, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 0x03, 0x33, 0x33, 0x09, 0x05, 0xD2, 0xFF });
	packetWriter:writeInstance(rightarm);
	rnet.send(packetWriter:finish());
end

rnet.setParent = function(instance, newParent)
	rnet.setProperty(instance, "Parent", newParent);
end

rnet.spamTool = function(tool1, repeats, count)
	packetWriter:start();
	packetWriter:writeByte(0x83);
	for i = 1, repeats do
		packetWriter:writeByte(0x3);
		packetWriter:writeInstance(tool1);
		packetWriter:writeUInt16BE(0x13BB); --[[ event ID ]] 
		packetWriter:writeByte(0); --[[ client has version ? ]]
		packetWriter:writeInstance(game:GetService("Players").LocalPlayer.Character);
		packetWriter:writeByte(0x3);
		packetWriter:writeInstance(tool1);
		packetWriter:writeUInt16BE(0x13BB);
		packetWriter:writeByte(0);
		packetWriter:writeInstance(game:GetService("Players").LocalPlayer.Backpack);
		--[[
		packetWriter:writeByte(0x3);
		packetWriter:writeInstance(tool2);
		packetWriter:writeUInt16BE(0x13BB); --[ [ event ID ] ] 
		packetWriter:writeByte(0); --[ [ client has version ? ] ]
		packetWriter:writeInstance(game:GetService("Players").LocalPlayer.Character);
		packetWriter:writeByte(0x3);
		packetWriter:writeInstance(tool2);
		packetWriter:writeUInt16BE(0x13BB);
		packetWriter:writeByte(0);
		packetWriter:writeInstance(game:GetService("Players").LocalPlayer.Backpack);
		]]
	end
	local data = packetWriter:finish();
	for i = 1, count or 1 do
		rnet.send(data);
		task.wait(0);
	end
end

rnet.equipTools = function(tools)
	assert(type(tools) == "table", "Table expected")
	packetWriter:start();
	packetWriter:writeByte(0x83);
	for i = 1,#tools do
		if not tools[i] then error'Tool expected' end
		if not tools[i]:IsA("Tool") then error'Tool expected' end
		packetWriter:writeByte(0x3);
		packetWriter:writeInstance(tools[i]);
		packetWriter:writeUInt16BE(0x13BB); --[[ event ID ]] 
		packetWriter:writeByte(0); --[[ client has version ? ]]
		packetWriter:writeInstance(game:GetService("Players").LocalPlayer.Character);
	end
	rnet.send(packetWriter:finish());
end

rnet.unequipTools = function(tools)
	assert(type(tools) == "table", "Table expected")
	packetWriter:start();
	packetWriter:writeByte(0x83);
	for i = 1,#tools do
		if not tools[i] then error'Tool expected' end
		if not tools[i]:IsA("Tool") then error'Tool expected' end
		packetWriter:writeByte(0x3);
		packetWriter:writeInstance(tools[i]);
		packetWriter:writeUInt16BE(0x13BB); --[[ event ID ]] 
		packetWriter:writeByte(0); --[[ client has version ? ]]
		packetWriter:writeInstance(game:GetService("Players").LocalPlayer.Backpack);
	end
	rnet.send(packetWriter:finish());
end


rnet.equipTool = function(tool, weld, alsoUnEquip)
	if weld == nil then
		rnet.setParent(tool, game:GetService("Players").LocalPlayer.Character);
	else
		packetWriter:start();
		packetWriter:writeByte(0x83);
		
		packetWriter:writeByte(0x3);
		packetWriter:writeInstance(tool);
		packetWriter:writeUInt16BE(0x13BB); --[[ event ID ]] 
		packetWriter:writeByte(0); --[[ client has version ? ]]
		packetWriter:writeInstance(game:GetService("Players").LocalPlayer.Character);
		
		packetWriter:writeByte(0x2);
		packetWriter:writeInstance(tool.Handle);
		packetWriter:writeUInt16BE(0x17C);
		packetWriter:writeByte(0);
		packetWriter:writeByte(0x6);
		packetWriter:writeInstance(game:GetService("Players").LocalPlayer.Character["Right Arm"]);
		
		packetWriter:writeByte(0x7);
		packetWriter:writeInstance(tool.Handle);
		packetWriter:writeUInt16BE(0xFF02); --[[ event ID ]] 
		packetWriter:writeFloatBE(0);
		packetWriter:writeFloatBE(-1.00);
		packetWriter:writeFloatBE(0);
		
		--[[ I will eventually work out what these bytes are. theyre somewhat related to tool grip.
		theyre constant so we can just copy & paste it for now
		]]
		packetWriter:writeBytes({ 0x06, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 0x03, 0x33, 0x33, 0x09, 0x05, 0xD2, 0xFF });
		
		packetWriter:writeInstance(game:GetService("Players").LocalPlayer.Character["Right Arm"]);
		
		if alsoUnEquip then
			packetWriter:writeByte(0x3);
			packetWriter:writeInstance(tool);
			packetWriter:writeUInt16BE(0x13BB);
			packetWriter:writeByte(0);
			packetWriter:writeInstance(game:GetService("Players").LocalPlayer.Backpack);
		end
			
		rnet.send(packetWriter:finish());
	end
end

rnet.unequipTool = function(tool)
	rnet.setParent(tool, game:GetService("Players").LocalPlayer.Backpack);
end

rnet.Touch = function(instanceA, instanceB, isTouching, unTouch)
	packetWriter:start();
	packetWriter:writeByte(0x86);
	packetWriter:writeInstance(instanceA);
	packetWriter:writeInstance(instanceB);
	packetWriter:writeByte(isTouching or 1);
	
	if (isTouching and unTouch) then
		packetWriter:writeInstance(instanceA);
		packetWriter:writeInstance(instanceB);
		packetWriter:writeByte(0);
	end
	 
	rnet.send(packetWriter:finish());
end

rnet.playAnimation = function(id, float1, float2, float3, float4)
	packetWriter:start();
	packetWriter:writeByte(0x83);
	packetWriter:writeByte(0x7);
	packetWriter:writeInstance(game:GetService("Players").LocalPlayer.Character.Humanoid.Animator);
	packetWriter:writeUInt16BE(0x0088);
	if (type(id) == "string") then
		packetWriter:writeByte(0);
		packetWriter:writeString(id);
		packetWriter:writeByte(0);
	elseif (type(id) == "number") then
		packetWriter:writeByte(0x0B); --[[ Content type (Float??) ]]
		packetWriter:writeVarInt64(id);
	else
		packetWriter:writeByte(0);
		packetWriter:writeString("rbxassetid://0");
		packetWriter:writeByte(0);
	end
	packetWriter:writeFloatBE(float1 or 0.08);
	packetWriter:writeFloatBE(float2 or 1.00);
	packetWriter:writeFloatBE(float3 or 1.10);
	packetWriter:writeFloatBE(float4 or 0.02);
	packetWriter:writeByte(2);
	rnet.send(packetWriter:finish());
end


rnet.setProperty = function(instance, propertyName, value)
	local propertyId, expectedType = _ing1("getpropertyid", propertyName);
	if propertyId == 0 then
		error'Invalid property'
	end
	packetWriter:start();
	packetWriter:writeByte(0x83);
	packetWriter:writeByte(0x3);
	packetWriter:writeInstance(instance);
	packetWriter:writeUInt16BE(propertyId);
	packetWriter:writeByte(0); --[[ client has version ? ]]
	packetWriter:writeObject({["Value"] = value, ["Type"] = expectedType}, false);
	rnet.send(packetWriter:finish());
end


rnet.Stretch = function(x, y, z)
	packetWriter:start();
	packetWriter:writeByte(0x83);
	packetWriter:writeByte(0x3);
	packetWriter:writeInstance(game:GetService("Players").LocalPlayer.Character.Humanoid);
	packetWriter:writeUInt16BE(0x5E0);
	packetWriter:writeByte(0); --[[ client has version ? ]]
	packetWriter:writeFloatBE(x or -1.00);
	packetWriter:writeFloatBE(y or 0);
	packetWriter:writeFloatBE(z or 0.03);
	rnet.send(packetWriter:finish());
end

rnet.Walk = function(speed)
	--[[	                                                                 1.00          0.33          0.05
	83 07 [01 4A 0F 08 00] [00 88] [0B] [E4 D9 88 AC 01] [01] [00 00 00 00] [3F 80 00 00] [3E A6 C5 D2] [3D 56 1F 3E] 08 00
	83 07 [01 D6 05 12 00] [00 89] [0B] [E6 E9 89 AC 01] [00] [3D CC CC CD] [00 00 00 00] [3F 80 00 00] [00 00 00 00] [E8 07] [02] 00
	83 07 [01 4B B8 02 00] [00 88] [A3 98 F7 0E] 01 3D A3 D7 0A 3F 80 00 00 3F 8C CC CD 3C A3 D7 0A 02 00
	
	]]
	--[[
	Found Event Animator->OnCombinedUpdate(Content animation, bool playState, float fadeTime, float weight, float speed, float timePosition, int valuesUpdated). ID: 0088
	Found Event Animator->OnCombinedUpdate2(Content animation, bool playState, float fadeTime, float weight, float speed, float timePosition, AnimationPriority priority, int valuesUpdated). ID: 0089
	]]
	rnet.fireevent(
		game:GetService("Players").LocalPlayer.Character:FindFirstChildOfClass("Humanoid"):FindFirstChildOfClass("Animator"), 
		"OnCombinedUpdate2", 
		360871142,--360852708, --[[ 0x0B, 0xE4, 0xD9, 0x88, 0xAC, 0x01]]
		true, --[[false, ]]
		0.08, 1.00, speed or 1.10, 0.02, --[[0.05, 0, 1.00, 0,]]
		Enum.AnimationPriority.Core,
		0xA--[[2]]
	);
end

rnet.Destroy = function(instance)
	packetWriter:start();
	packetWriter:writeByte(0x83);
	packetWriter:writeByte(0x1);
	packetWriter:writeInstance(instance);
	packetWriter:writeBytes({ 0x00, 0x00, 0x00, 0x00, 0x00 });
	rnet.send(packetWriter:finish());
end



rnet.Disconnect = function(id)
	packetWriter:start();
	packetWriter:writeByte(0x15);
	packetWriter:writeUInt32BE(id or 0);
	rnet.send(packetWriter:finish());
end

--[[
only supports one or the other currently
]]
rnet.blockDeletes = function(toggle)
	if toggle then
		rnet.setfilter({ 0x83, 0x01 });
	else
		rnet.setfilter({});
	end
end

rnet.blockCreates = function(toggle)
	if toggle then
		rnet.setfilter({ 0x83, 0x02 });
	else
		rnet.setfilter({});
	end
end

rnet.Shutdown = function()
	rnet.fireevent(game:service'LocalizationService', "TextScraperClientMessageWithPlayerSignal", 1, game:GetService("Players").LocalPlayer, "nigga");
end
rnet.shutdown = rnet.Shutdown;

getproperties = rnet.getProperties;
getevents = rnet.getEvents;
firesignal = rnet.fireEvent;

rnet.fireevent = rnet.fireEvent;
rnet.getevents = rnet.getEvents;
rnet.getproperties = rnet.getProperties;
--[[rnet.fireeventwithself = rnet.fireEventWithSelf;]]
rnet.fireremote = rnet.fireRemote;
rnet.setfilter = rnet.setFilter;
rnet.blockdeletes = rnet.blockDeletes;
rnet.blockcreates = rnet.blockCreates;
rnet.nextpacket = rnet.nextPacket;
rnet.setphysicsrootpart = rnet.setPhysicsRootPart;
rnet.readeventpacket = rnet.readEventPacket;
rnet.sendphysics = rnet.sendPhysics;
rnet.setparent = rnet.setParent;
rnet.setproperty = rnet.setProperty;
rnet.sit = rnet.Sit;
rnet.ack = rnet.Ack;
rnet.newinstance = rnet.newInstance;
rnet.settoolgrip = rnet.setToolGrip;
rnet.spamtool = rnet.spamTool;
rnet.equiptool = rnet.equipTool;
rnet.unequiptool = rnet.unEquipTool;
rnet.send = rnet.Send;
rnet.stretch = rnet.Stretch;
rnet.walk = rnet.Walk;
rnet.touch = rnet.Touch;
rnet.tostring = rnet.toString;
rnet.tobytes = rnet.toBytes;
rnet.destroy = rnet.Destroy;
rnet.disconnect = rnet.Disconnect;
rnet.packetWriter = packetWriter;
rnet.packetwriter = packetWriter;
rnet.packetReader = packetReader;
rnet.packetreader = packetReader;
 
Drawing = {};

Drawing.clear = function()
    _ing1("drawing_clearshapes");
end

Drawing.new = function(class)
    local obj = newproxy(true);
    local mt = getmetatable(obj);

    local name = _ing1("drawing_addshape", class);
    mt.Name = name;
    mt.Type = class:lower();
    mt.Visible = true;
    mt.ZIndex = 1;
    
    if (class == "Line") then
        mt.From = Vector2.new(0,0);
        mt.To = Vector2.new(0,0);
        mt.Color = Color3.new(0,0,0);
        mt.Thickness = 1;
    elseif (class == "Square") then
        mt.Position = Vector2.new(0,0);
        mt.Size = Vector2.new(0,0);
        mt.Color = Color3.new(0,0,0);
        mt.Thickness = 1;
        mt.Rounding = 0;
        mt.Filled = false;
        mt.Transparency = 1;
    elseif (class == "Circle") then
        mt.Position = Vector2.new(0,0);
        mt.Color = Color3.new(0,0,0);
        mt.Radius = 0;
        mt.Thickness = 1;
        mt.NumberOfSides = 250;
        mt.Filled = false;
    elseif (class == "Triangle") then
        mt.PointA = Vector2.new(0,0);
        mt.PointB = Vector2.new(0,0);
        mt.PointC = Vector2.new(0,0);
        mt.Color = Color3.new(0,0,0);
        mt.Thickness = 1;
        mt.Filled = false;
    elseif (class == "Text") then
        mt.Text = "";
        mt.TextEnd = 0;
        mt.TextBounds = Vector2.new(0, 16);
        mt.Position = Vector2.new(0,0);
        mt.Color = Color3.new(0,0,0);
        mt.Center = false;
        mt.Outline = false;
        mt.ZIndex = 2;
    elseif (class == "Quad") then
        mt.PointA = Vector2.new(0,0);
        mt.PointB = Vector2.new(0,0);
        mt.PointC = Vector2.new(0,0);
        mt.PointD = Vector2.new(0,0);
        mt.Color = Color3.new(0,0,0);
        mt.Thickness = 1;
        mt.Filled = false;
    end

    mt.__newindex = function(self, prop_name, value)
        local prop_type = type(value):lower();

        if (typeof(value) == "Vector2") then
            _ing1("drawing_editshape", name, tostring(prop_name), value.X, value.Y);
        elseif (tostring(prop_name) == "Color" or typeof(value) == "Color3") then
            _ing1("drawing_editshape", name, "Color", value.r, value.g, value.b);
        else
            _ing1("drawing_editshape", name, tostring(prop_name), value);
        end
		
        mt[prop_name] = value;
    end
    
    mt.Remove = function(self)
        _ing1("drawing_removeshape", self.Name);
    end

    mt.__index = function(self, prop_name)
        return mt[prop_name];
        --[[if (prop_name == "Remove") then
            _ing1(name, "drawing_removeshape");
        else
            local value = mt[prop_name];
            return value;
        end]]
    end

    mt.__gc = function(self)
        _ing1("drawing_removeshape", self.Name);
    end

    return obj;
end 


local realdebug = debug;
debug = {};

for i, v in next, realdebug do
    debug[i] = v
end

debug.getproto = function(p, i)
    if type(p) ~= "function" then error'Function expected' end
    local f = function() end
    if _ing1("debug_getproto", p, i, f) == false then
        return nil
    end
    return f
end

debug.getprotos = function(p)
    if type(p) ~= "function" then error'Function expected' end
    local t = {};
    local sizep = _ing1("debug_getsizeprotos", p);
    for i = 1,sizep do
        table.insert(t, debug.getproto(p, i));
    end
    return t
end

debug.setproto = function(p, i, f) 
    _ing1("debug_setproto", p, i, f);
end

debug.setprotos = function(p, t)
    if type(p) ~= "function" then error'Function expected' end
    for i = 1, #t do
        debug.setproto(p, i, t[i]);
    end
end

debug.getconstant = function(p, i)
    if type(p) ~= "function" then error'Function expected' end
    local k = false;
    return _ing1("debug_getconstant", p, i, k)
end

debug.getconstants = function(p)
    if type(p) ~= "function" then error'Function expected' end
    local t = {};
    local sizek = _ing1("debug_getsizeconstants", p);
    for i = 1,sizek do
        table.insert(t, debug.getconstant(p, i));
    end
    return t
end

debug.setconstant = function(p, i, k) 
    if type(p) ~= "function" then error'Function expected' end
    _ing1("debug_setconstant", p, i, k);
end

debug.setconstants = function(p, t) 
    if type(p) ~= "function" then error'Function expected' end
    local sizek = _ing1("debug_getsizeconstants", p);
    if #t > sizek then
        sizek = #t;
    end
    for i = 1,sizek do
        debug.setconstant(p, i, t[i]);
    end
end
--[[
debug.getcode = function(p, i) 
    if type(p) ~= "function" then error'Function expected' end
    return _ing1(i, p, "debug_getcode")
end

debug.getcodearray = function(p) 
    if type(p) ~= "function" then error'Function expected' end
    local t = {};
    local sizecode = _ing1(p, "debug_getsizecode");
    for i = 1,sizecode do
        table.insert(t, debug.getcode(p, i));
    end
    return t
end

debug.setcode = function(p, i, k) 
    if type(p) ~= "function" then error'Function expected' end
    _ing1(k, i, p, "debug_setcode");
end

debug.setcodearray = function(p, t)
    if type(p) ~= "function" then error'Function expected' end
    _ing1(unpack(t), #t, p, "debug_setcodearray");
end
]]
debug.getupvalue = function(p) warn'debuginfo not available' end
debug.getupvalues = function(p) warn'debuginfo not available' end
debug.setupvalue = function(p, n, v) warn'debuginfo not available' end
debug.setupvalues = function(p, t) warn'debuginfo not available' end

debug.getstack = function(i) 
    if type(i) == "number" then
        local k = false;
        return _ing1("debug_getstack", i, k);
    end
    local t = {};
    local sizestack = _ing1("debug_getstacksize", p);
    for i = 1,sizestack do
        local k = false;
        table.insert(t, _ing1("debug_getstack", i, k));
    end
    return t
end

debug.setstack = function(o, i) 
    if type(o) == "table" then
        for j,v in pairs(o) do
            _ing1("debug_setstack", j, v);
        end
    else
        _ing1("debug_setstack", i, o);
    end
end

--[[ Original, hand-written code ENDS here ]]





--[[ This was given to me by a friend. 
These functions could be implemented in less than 3 lines each,
I don't know why scripts dont just implement it themselves.
]]
bit = {}

-- Why is this a thing? Why are we mixing bit libraries in 2022
for i, v in next, bit32 do
    bit[i] = v
end

bit.ror = bit.rrotate
bit.rol = bit.lrotate
bit.rrotate = nil
bit.lrotate = nil

bit.tobit = function(x)
    x = x % (2^32)
    if x >= 0x80000000 then x = x - (2^32) end
    return x
end

-- I already wrote a tohex function in the 'rnet' api
-- But It's professional to use code that already exists for compatibility reasons,
-- I have absolutely no idea what's supposed to go in an extended bit lib, since I dont own or use any other exploits.
bit.tohex = function(x, n)
    n = n or 8
    local up
    if n <= 0 then
if n == 0 then return '' end
up = true
n = -n
    end
    x = bit.band(x, 16^n-1)
    return ('%0'..n..(up and 'X' or 'x')):format(x)
end

bit.bswap = function(x)
    local a = bit.band(x, 0xff)
    x = bit.rshift(x, 8)
    local b = bit.band(x, 0xff)
    x = bit.rshift(x, 8)
    local c = bit.band(x, 0xff)
    x = bit.rshift(x, 8)
    local d = bit.band(x, 0xff)
    return bit.lshift(bit.lshift(bit.lshift(a, 8) + b, 8) + c, 8) + d
end

wrapfunction = function(callback)
    return (function(callback)
        return callback;
    end)(callback);
end

--[[
from Rexi -- and if rexi didnt make this then Idk, guess I'm just recycling code that was
in front of my eyes and already works perfectly, so nothing needs to be changed about it
:shrug:
]]
local _ENV = (
    getgenv and getgenv or
    getfenv and getfenv or
    { }
)();

addenv = function(index, value, use_proxy)
    if rawget(_ENV, index) then
        return rawget(_ENV, index);
    end
    if typeof(value) == "function" then
        rawset(_ENV, index, (use_proxy and wrap_function_using_proxy(value) or wrap_function(value)));
        return;
    end
    rawset(_ENV, index, value);
end

--[[ Base64, shared from a friend ]]
local base64 = { };
local b = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

base64.encode = function(data)
    return ((data:gsub('.', function(x) 
        local r,b='',x:byte()
        for i=8,1,-1 do r=r..(b%2^i-b%2^(i-1)>0 and '1' or '0') end
        return r;
    end)..'0000'):gsub('%d%d%d?%d?%d?%d?', function(x)
        if (#x < 6) then return '' end
        local c=0
        for i=1,6 do c=c+(x:sub(i,i)=='1' and 2^(6-i) or 0) end
        return b:sub(c+1,c+1)
    end)..({ '', '==', '=' })[#data%3+1])
end

base64.decode = function(data)
    data = string.gsub(data, '[^'..b..'=]', '')
    return (data:gsub('.', function(x)
        if (x == '=') then return '' end
        local r,f='',(b:find(x)-1)
        for i=6,1,-1 do r=r..(f%2^i-f%2^(i-1)>0 and '1' or '0') end
        return r;
    end):gsub('%d%d%d?%d?%d?%d?%d?%d?', function(x)
        if (#x ~= 8) then return '' end
        local c=0
        for i=1,8 do c=c+(x:sub(i,i)=='1' and 2^(8-i) or 0) end
        return string.char(c)
    end))
end

-- from Rexi
proxywrap = function(callback, original)
    local proxy = newproxy(true);
    local mt = getmetatable(proxy);
    mt.__tostring = (function() 
        return tostring((original or callback));
    end)
    mt.__index = (original or callback);
    mt.__call = (function(self, ...)
        return callback(unpack({...})); 
    end)
    return proxy;
end

-- from Rexi
hookmetamethod = function(object, methodname, func)
    local objectMt = getrawmetatable(object);
    setreadonly(objectMt, false);
    local metamethod = objectMt[methodname];
	objectMt[methodname] = function(self, ...)
		return func(self, ...);
	end
    --[[local proxy = proxywrap(hook, metamethod);
    objectMt[methodname] = (function(self, ...) 
        return proxy(self, ...);
    end)]]
    setreadonly(objectMt, true);
    return metamethod;
end

setidentity(7);


local left = false;
local replicator = game:GetService("NetworkClient"):WaitForChild("ClientReplicator");
game:GetService("NetworkClient").DescendantRemoving:Connect(function(x) if x == replicator then left = true _ing1("disconnect") end end)
--[[game.Close:Connect(function(x) left = true _ing1("disconnect") end) -- game.CloseLate ]]

while not left do
	local src = --[[getgenv().]]getceleryscript();
	if src then
		task.spawn(--[[getgenv().]]loadstring(src));
	end
	task.wait(.02)
end
