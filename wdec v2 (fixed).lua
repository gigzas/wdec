--!strict

--[[
    wdec - Luau decompiler
    written by luavm on discord
]]

---

local fmt = string.format -- string.format is annoying to write

type luau_op = {
    name: string,
    aux: boolean
}

local opcodes: { luau_op } = { -- Opcode table. Every opcode is ordered as their Opcode number (not Roblox encoded)
    {name = "NOP", aux = false},
    {name = "BREAK", aux = false},
    {name = "LOADNIL", aux = false},
    {name = "LOADB", aux = false},
    {name = "LOADN", aux = false},
    {name = "LOADK", aux = false},
    {name = "MOVE", aux = false},
    {name = "GETGLOBAL", aux = true},
    {name = "SETGLOBAL", aux = true},
    {name = "GETUPVAL", aux = false},
    {name = "SETUPVAL", aux = false},
    {name = "CLOSEUPVALS", aux = false},
    {name = "GETIMPORT", aux = true},
    {name = "GETTABLE", aux = false},
    {name = "SETTABLE", aux = false},
    {name = "GETTABLEKS", aux = true},
    {name = "SETTABLEKS", aux = true},
    {name = "GETTABLEN", aux = false},
    {name = "SETTABLEN", aux = false},
    {name = "NEWCLOSURE", aux = false},
    {name = "NAMECALL", aux = true},
    {name = "CALL", aux = false},
    {name = "RETURN", aux = false},
    {name = "JUMP", aux = false},
    {name = "JUMPBACK", aux = false},
    {name = "JUMPIF", aux = false},
    {name = "JUMPIFNOT", aux = false},
    {name = "JUMPIFEQ", aux = true},
    {name = "JUMPIFLE", aux = true},
    {name = "JUMPIFLT", aux = true},
    {name = "JUMPIFNOTEQ", aux = true},
    {name = "JUMPIFNOTLE", aux = true},
    {name = "JUMPIFNOTLT", aux = true},
    {name = "ADD", aux = false},
    {name = "SUB", aux = false},
    {name = "MUL", aux = false},
    {name = "DIV", aux = false},
    {name = "MOD", aux = false},
    {name = "POW", aux = false},
    {name = "ADDK", aux = false},
    {name = "SUBK", aux = false},
    {name = "MULK", aux = false},
    {name = "DIVK", aux = false},
    {name = "MODK", aux = false},
    {name = "POWK", aux = false},
    {name = "AND", aux = false},
    {name = "OR", aux = false},
    {name = "ANDK", aux = false},
    {name = "ORK", aux = false},
    {name = "CONCAT", aux = false},
    {name = "NOT", aux = false},
    {name = "MINUS", aux = false},
    {name = "LENGTH", aux = false},
    {name = "NEWTABLE", aux = true},
    {name = "DUPTABLE", aux = false},
    {name = "SETLIST", aux = true},
    {name = "FORNPREP", aux = false},
    {name = "FORNLOOP", aux = false},
    {name = "FORGLOOP", aux = true},
    {name = "FORGPREP_INEXT", aux = false},
    {name = "DEP_FORGLOOP_INEXT", aux = false},
    {name = "FORGPREP_NEXT", aux = false},
    {name = "NATIVECALL", aux = false},
    {name = "GETVARARGS", aux = false},
    {name = "DUPCLOSURE", aux = false},
    {name = "PREPVARARGS", aux = false},
    {name = "LOADKX", aux = true},
    {name = "JUMPX", aux = false},
    {name = "FASTCALL", aux = false},
    {name = "COVERAGE", aux = false},
    {name = "CAPTURE", aux = false},
    {name = "DEP_JUMPIFEQK", aux = false},
    {name = "DEP_JUMPIFNOTEQK", aux = false},
    {name = "FASTCALL1", aux = false},
    {name = "FASTCALL2", aux = true},
    {name = "FASTCALL2K", aux = true},
    {name = "FORGPREP", aux = false},
    {name = "JUMPXEQKNIL", aux = true},
    {name = "JUMPXEQKB", aux = true},
    {name = "JUMPXEQKN", aux = true},
    {name = "JUMPXEQKS", aux = true},
    {name = "IDIV", aux = false},
    {name = "IDIVK", aux = false},
}

local function GetOp(opcode: number): luau_op
    return opcodes[opcode + 1]
end

type closure = typeof(function() end)

type luau_instruction = { -- d and e are for ease of coding
    name : string,
    a    : number,
    b    : number,
    c    : number,
    d    : number,
    e    : number,
    aux  : number | nil
}

type luau_constant = {
    type: number,
    value: number | boolean | { number }
}

type luau_proto = {
    maxstacksize: number,
    numparams: number,
    numupvalues: number,
    isvararg: boolean,
    instructions: { luau_instruction },
    constants: { luau_constant },
    child_protos: { number },
    name: string?,
    types: {
        flags: number,
        data: { number }
    }
}

type luau_bytecode = { -- TODO: Exclude strings and somehow inline into deserializer
    version : number,
    protos : { luau_proto },
    strings : { string },
    main_proto_id: number
}

type luau_bytes = { number } -- I like custom named types for these things

type luau_constant_type = number

local function errorf(format, ...) -- C
    error(fmt(format, ...))
end

local function BytesToTable(bytes): luau_bytes
    local tbl = {}
    for i = 1, string.len(bytes) do
        table.insert(tbl, string.byte(string.sub(bytes, i, i)))
    end
    return tbl
end

local function RobloxOp(x: number): number -- Turns a Roblox encrypted opcode into a luau opcode. Ex: 105 -> 5
    return x * 203 % 256
end

local function deserialize(s_bytes: string, is_roblox: boolean): luau_bytecode?
    local t_bytes: luau_bytes = BytesToTable(s_bytes)

    local bytes_index = 1

    local function GetByte(): number
        bytes_index += 1
        return t_bytes[bytes_index - 1] and t_bytes[bytes_index - 1] or 0
    end

    local function GetInt(): number
        return bit32.bor(
            bit32.lshift(GetByte(), 0), -- Redundant lshift but it looks better
            bit32.lshift(GetByte(), 8),
            bit32.lshift(GetByte(), 16),
            bit32.lshift(GetByte(), 24)
        )
    end

    local function GetVarInt(): number
        local value: number = 0
        local shift: number = 0
    
        repeat
            local byte: number? = GetByte()
            if not byte then break end
            value = bit32.bor(value, bit32.lshift(bit32.band(byte :: number, 127), shift))
            shift = shift + 7
        until (byte :: number) < 128

        return value
    end

    local bytecode_version = GetByte() -- Should always be 3 or 4 apparently
    if bytecode_version ~= 3 and bytecode_version ~= 4 then
        errorf("Incompatible bytecode version. (%d)", bytecode_version) -- Should never happen
        return nil
    end

    local types_version = 0
    if bytecode_version == 4 then
        types_version = GetByte()
    end

    local string_table = {}
    local strings_sz = GetVarInt()
    for _ = 1, strings_sz do -- Luau stores strings as a massive array before all the actual code, so we must first read them all. At some point I plan to inline the strings into custom opcodes to reduce code complexity
        local len = GetVarInt()
        local buffer = ""
        for _ = 1, len do
            buffer ..= string.char(GetByte())
        end
        table.insert(string_table, buffer)
    end

    local protos: { luau_proto } = {}
    local protos_sz = GetVarInt()
    for _ = 1, protos_sz do -- Iterate over every proto, including main.
        local maxstacksize: number = GetByte()
        local numparams: number = GetByte()
        local numupvalues: number = GetByte()
        local isvararg: boolean = GetByte() ~= 0
        local types = {}
        -- we can recover type data if the bytecode version is 4
        if bytecode_version == 4 then
            types.flags = GetByte()
            types.data = {}
            local type_sz = GetVarInt()
            for i = 1, type_sz do
                table.insert(types.data, GetByte())
            end
        end

        local instructions_sz: number = GetVarInt()
        local instructions: { luau_instruction } = {}
        local i: number = 0
        local n_aux_instructions: number = 0
        while i < instructions_sz do
            local opcode: number = GetByte()
            if is_roblox then
                opcode = RobloxOp(opcode)
            end
            local a: number = GetByte()
            local b: number = GetByte()
            local c: number = GetByte()
            local op: luau_op = GetOp(opcode)
            if not op then break end
            local aux: number? = nil
            if op.aux then
                aux = GetInt()
                i += 1
                n_aux_instructions += 1 -- We keep track of the number of double-width instructions because the number of instructions = instruction_sz - n_aux_instructions; useful for reading debug information (which is never present either way)
            end 
            local function tosigned(int: number, bits: number)
                local max = 2^(bits - 1)
                if int >= max then
                    int = int - 2^bits
                end
                return int
            end
            
            local d: number = tosigned(bit32.bor(b, bit32.lshift(c, 8)), 16) -- d and e are for ease of coding
            local e: number = tosigned(bit32.bor(a, bit32.lshift(b, 8), bit32.lshift(c, 16)), 24)

            table.insert(instructions, { -- luau_instrctuon
                name = op.name,
                a = a,
                b = b,
                c = c,
                d = d,
                e = e,
                aux = aux
            })
            i += 1
        end

        local constants_sz: number = GetVarInt()
        local constants: { luau_constant } = {}
        for _ = 1, constants_sz do
            local const_type: luau_constant_type = GetByte()
            if const_type == 0 then -- NIL
                table.insert(constants, {
                    type = const_type,
                    value = 0
                })
            elseif const_type == 1 then -- BOOLEAN
                table.insert(constants, {
                    type = const_type,
                    value = GetByte() ~= 0
                })
            elseif const_type == 2 then -- NUMBER
                local str: string = ""
                for _ = 1, 8 do
                    str ..= string.char(GetByte())
                end
                table.insert(constants, {
                    type = const_type,
                    value = string.unpack("<d", str)
                })
            elseif const_type == 3 then -- STRING
                table.insert(constants, {
                    type = const_type,
                    value = GetVarInt()
                })
            elseif const_type == 4 then -- IMPORT
                table.insert(constants, {
                    type = const_type,
                    value = GetInt()
                })
            elseif const_type == 5 then -- TABLE
                local tbl: { number } = {}
                local tblSz = GetVarInt()
                for _ = 1, tblSz do
                    table.insert(tbl, GetVarInt()) -- Where VarInt = Key
                end
                table.insert(constants, {
                    type = const_type,
                    value = tbl
                })
            elseif const_type == 6 then -- CLOSURE
                table.insert(constants, {
                    type = const_type,
                    value = GetVarInt()
                })
            end
        end

        local child_protos: { number } = {}
        for _ = 1, GetVarInt() do -- Iterate over number of child protos
            table.insert(child_protos, GetVarInt()) -- Add index of child proto into the child proto table
        end

        -- The following code is here to skip over debug bytes in the bytecode. This debug information is usually pretty useless so I just discard it.
        -- The code is pretty verbose so if you want, you can copy-paste it into your project to read debug data.
        -- Most of this can be fetched using the `debug` library provided by most executors

        local _debug_linedefined: number = GetVarInt()
        local _debug_name: number = GetVarInt()

        local proto: luau_proto = {
            maxstacksize = maxstacksize,
            numparams = numparams,
            numupvalues = numupvalues,
            isvararg = isvararg,
            instructions = instructions,
            constants = constants,
            child_protos = child_protos,
            name = _debug_name ~= nil and string_table[_debug_name] or nil,
            types = types
        }

        table.insert(protos, proto)

        local _debug_haslines: boolean = GetByte() ~= 0
        if _debug_haslines then
            local span: number = GetVarInt()
            for _ = 1, instructions_sz do
                GetByte()
            end
            for _ = 1, bit32.rshift(instructions_sz - 1, span) + 1 do
                GetInt()
            end
        end

        -- Not present in Roblox bytecode bcause -g1
        
        local _debug_hasdebug = GetByte() ~= 0
        if _debug_hasdebug then
            for _ = 1, GetVarInt() do -- Locals
                local _lname = GetVarInt()
                local _lstartpc = GetVarInt()
                local _lendpc = GetVarInt()
                local _lreg = GetByte()
            end
            for _ = 1, GetVarInt() do -- Upvalues
                local _lname = GetVarInt()
            end
        end
    end

    local bytecode: luau_bytecode = {
        version = bytecode_version,
        strings = string_table,
        protos = protos,
        main_proto_id = GetVarInt()
    }
    return bytecode
end

type Register = {
    Type: "Register",
    Value: number
}

type Number = {
    Type: "Number",
    Value: number
}

type String = {
    Type: "String",
    Value: string
}

type Nil = {
    Type: "Nil",
    Value: nil
}

type Boolean = {
    Type: "Boolean",
    Value: boolean
}

type LBC_Import = {
    Type: "LBC_Import",
    Value: number -- no clue how to decode this :sob:
}

type LBC_Table = { -- Constant table
    Type: "LBC_Table",
    Value: { number }
}

type Table = { -- Dynamic table
    Type: "Table",
    Value: { Value }
}

type LBC_Closure = {
    Type: "LBC_Closure",
    Value: number -- No clue how to decode thsi either :pray:
}

type Closure = {
    Type: "Closure",
    Value: { Instruction },
    Params: { Local },
    IsVarArg: boolean,
    Name: string,
    UpValues: { Value },
    IsGlobal: boolean,
    ParamTypes: { number }
}

type TableIndex = {
    Type: "TableIndex",
    Table: Value,
    Index: Value
}

type Local = {
    Type: "Local",
    Register: number,
    Value: Value,
    Latest: Value,
    Id: string,
    Scope: luau_proto,
    Lifetime: number?,
    CreatedByCall: boolean? -- if a local is created by `local v0 = someFunc()`, then reassigning that local is very unlikely, so we create a new local instead
}

type Expression = {
    Type: "Expression",
    Op: string,
    Lhs: Value,
    Rhs: Value
}

type VarArgs = { -- Decompiles to `...`
    Type: "VarArgs"
}

type Unary = {
    Type: "Unary",
    Op: string,
    Value: Value
}

type Strings = { -- Needed for CONCAT
    Type: "Strings",
    Value: { Value }
}

type Global = {
    Type: "Global",
    Value: string
}

type InlineNamecall = {
    Type: "InlineNamecall",
    Args: { Value },
    Object: Value,
    Target: String
}

type InlineCall = {
    Type: "InlineCall",
    Args: { Value },
    Target: Value
}

type Value = Register | Number | String | Nil | Boolean | LBC_Import | LBC_Table | Table | LBC_Closure | Closure | Local | TableIndex | Expression | Unary | Strings | VarArgs | Global | InlineNamecall | InlineCall

type Instruction = {
    Name:   string,
    ArgA:    Value?,
    ArgB:    Value?,
    ArgC:    Value?,
    ArgD:    Value?,
    ArgE:    Value?,
    ArgAux:  Value?,
    Special: any
}

---

type VarAssignNode = {
    Type: "VarAssignNode",
    Target: Local,
    Value: ValueNode
}

type VarReassignNode = {
    Type: "VarReassignNode",
    Target: Local,
    Value: ValueNode
}

type GlobalDefinitionNode = {
    Type: "GlobalDefinitionNode",
    Target: string,
    Value: ValueNode
}

type ClosureNode = {
    Type: "ClosureNode",
    Name: string,
    Params: { Local },
    IsVarArg: boolean,
    Body: { Node },
    ParamTypes: { number }
}

type FunctionCallNode = {
    Type: "FunctionCallNode",
    Target: ValueNode,
    Args: { ValueNode },
    RetVals: { Local }
}

type ReturnNode = {
    Type: "ReturnNode",
    Values: { ValueNode }
}

type TableAssignNode = {
    Type: "TableAssignNode",
    Table: ValueNode,
    Index: ValueNode,
    Source: ValueNode
}

type NamecallNode = {
    Type: "NamecallNode",
    Args: { ValueNode },
    RetVals: { Local },
    Object: ValueNode,
    Target: StringNode
}

type BreakNode = {
    Type: "BreakNode"
}

type ContinueNode = {
    Type: "ContinueNode"
}

type IfNode = {
    Type: "IfNode",
    Condition: ValueNode,
    Body: { Node },
    ElseBody: { Node },
}

type WhileNode = {
    Type: "WhileNode",
    Condition: ValueNode,
    Body: { Node }
}

type ForRangeNode = { --- for x = y, z[, æ] do ... end
    Type: "ForRangeNode",
    Index: ValueNode,
    Limit: ValueNode,
    Step: ValueNode,
    Iterator: Local,
    Body: { Node }
}

type ForValueNode = { --- for x, ... in y, z[, æ] do ... end
    Type: "ForValueNode",
    Generator: ValueNode,
    State: ValueNode,
    Index: ValueNode, -- Mostly NilNode
    Variables: { Local },
    Body: { Node }
}

-- Values

type NumberNode = {
    Type: "NumberNode",
    Value: number
}

type StringNode = {
    Type: "StringNode",
    Value: string
}

type StringsNode = {
    Type: "StringsNode",
    Value: { ValueNode }
}

type NilNode = {
    Type: "NilNode",
}

type BooleanNode = {
    Type: "BooleanNode",
    Value: boolean
}

type ExpressionNode = {
    Type: "ExpressionNode",
    Op: string,
    Lhs: ValueNode,
    Rhs: ValueNode
}

type UnaryNode = {
    Type: "UnaryNode",
    Op: string,
    Value: ValueNode,
}

type VarArgsNode = {
    Type: "VarArgsNode"
}

type GlobalNode = {
    Type: "GlobalNode",
    Value: string
}

type TableIndexNode = {
    Type: "TableIndexNode",
    Table: ValueNode,
    Index: ValueNode
}

type TableNode = {
    Type: "TableNode",
    Body: { ValueNode }
}

type InlineNamecallNode = { -- Allow for things such as `game:GetService("Players").LocalPlayer.Name` etc
    Type: "InlineNamecallNode",
    Args: { ValueNode },
    Object: ValueNode,
    Target: StringNode
}

type InlineCallNode = { -- MULTRET call (`print(add(1, 2))`)
    Type: "InlineCallNode",
    Args: { ValueNode },
    Target: ValueNode
}

type ValueNode = NumberNode | StringNode | NilNode | BooleanNode | ExpressionNode | UnaryNode | VarArgsNode | ClosureNode | Local | TableIndexNode | GlobalNode | StringsNode | TableNode | InlineNamecallNode | InlineCallNode
type Node = VarAssignNode | VarReassignNode | GlobalDefinitionNode | ClosureNode | ValueNode | FunctionCallNode | ReturnNode | TableAssignNode | NamecallNode | BreakNode | ContinueNode | IfNode | WhileNode | ForRangeNode | ForValueNode

type AST = Node

---

type BuiltInFunction = {
    Path: { string }
}

local builtins: { BuiltInFunction } = { -- ORDER SENSITIVE
    { Path = { "assert" }},
    -- math
    { Path = { "math", "abs" }},
    { Path = { "math", "acos" }},
    { Path = { "math", "asin" }},
    { Path = { "math", "atan2" }},
    { Path = { "math", "atan" }},
    { Path = { "math", "ceil" }},
    { Path = { "math", "cosh" }},
    { Path = { "math", "cos" }},
    { Path = { "math", "deg" }},
    { Path = { "math", "exp" }},
    { Path = { "math", "floor" }},
    { Path = { "math", "fmod" }},
    { Path = { "math", "frexp" }},
    { Path = { "math", "ldexp" }},
    { Path = { "math", "log10" }},
    { Path = { "math", "log" }},
    { Path = { "math", "max" }},
    { Path = { "math", "min" }},
    { Path = { "math", "modf" }},
    { Path = { "math", "pow" }},
    { Path = { "math", "rad" }},
    { Path = { "math", "sinh" }},
    { Path = { "math", "sin" }},
    { Path = { "math", "sqrt" }},
    { Path = { "math", "tanh" }},
    { Path = { "math", "tan" }},
    -- bit32
    { Path = { "bit32", "arshift" }},
    { Path = { "bit32", "band" }},
    { Path = { "bit32", "bnot" }},
    { Path = { "bit32", "bor" }},
    { Path = { "bit32", "bxor" }},
    { Path = { "bit32", "btest" }},
    { Path = { "bit32", "extract" }},
    { Path = { "bit32", "lrotate" }},
    { Path = { "bit32", "lshift" }},
    { Path = { "bit32", "replace" }},
    { Path = { "bit32", "rrotate" }},
    { Path = { "bit32", "rshift" }},
    
    { Path = { "type" }},

    -- string
    { Path = { "string", "byte" }},
    { Path = { "string", "char" }},
    { Path = { "string", "len" }},

    { Path = { "typeof" }},
    
    -- string
    { Path = { "string", "sub" }},
    
    -- math
    { Path = { "math", "clamp" }},
    { Path = { "math", "sign" }},
    { Path = { "math", "round" }},

    -- raw*
    { Path = { "rawset" }},
    { Path = { "rawget" }},
    { Path = { "rawequal" }},

    -- table
    { Path = { "table", "insert" }},
    { Path = { "table", "unpack" }},

    -- vector constructor
    { Path = { "Vector3", "new" }},

    -- bit32.count
    { Path = { "bit32", "countlz" }},
    { Path = { "bit32", "countrz" }},

    -- select(_, ...)
    { Path = { "select" }},

    { Path = { "rawlen" }},
    
    { Path = { "bit32", "extract" }},
    
    { Path = { "getmetatable" }},
    { Path = { "setmetatable" }},

    { Path = { "tonumber" }},
    { Path = { "tostring" }},
}

local function ConvertBuiltin(id: number): GlobalNode | TableIndexNode
    local path = builtins[id].Path
    if #path == 1 then
        return {
            Type = "GlobalNode",
            Value = path[1]
        }
    end
    return {
        Type = "TableIndexNode",
        Table = {
            Type = "GlobalNode",
            Value = path[1]
        },
        Index = {
            Type = "StringNode",
            Value = path[2]
        }
    }
end

local function wdec_decompile(bytecode: luau_bytecode): string
    local function ReAssigns(instruction: luau_instruction, target: number): boolean -- Function to check if target register `target` is written to by `instruction`
        if not table.find({ -- Below is a list of instructions which can possibly write to a register
            -- TODO: finish this list; it fucks up shit if it isn't
            "GETGLOBAL", "GETUPVAL", "NEWCLOSURE",
            "ADD", "SUB", "MUL", "DIV", "ADDK",
            "SUBK", "MULK", "DIVK", "MOD", "POW",
            "MODK", "POWK", "AND", "OR", "ANDK",
            "ORK", "CONCAT", "NOT", "MINUS", "LENGTH",
            "NEWTABLE", "DUPTABLE", "GETVARARGS",
            "LOADKX", "IDIV", "IDIVK", "LOADN",
            "LOADK", "LOADB", "MOVE", "LOADNIL",
            "DUPCLOSURE", "CALL", "SETLIST",
            "FORNPREP", "GETTABLEKS"
        }, instruction.name) then
            return false -- return false if the instruction doesn't write (JUMP, etc)
        end
        if instruction.name == "CALL" then
            return instruction.c - 1 > 0 and target >= instruction.a and target <= instruction.a + instruction.c - 1
        end
        if instruction.name == "FORNPREP" then
            return target == instruction.a + 2
        end
        if table.find({
            "ADD", "SUB", "MUL", "DIV", "ADDK",
            "SUBK", "MULK", "DIVK", "MOD", "POW",
            "MODK", "POWK", "AND", "OR", "ANDK", "ORK"
        }, instruction.name) then
            if instruction.a == instruction.b and target == instruction.a then
                return false -- Here it technically gets re-assigned, BUT it's still the same variable
            end
        end
        return instruction.a == target -- Target register is always instr.a
    end
    local function References(instruction: luau_instruction, target: number): boolean -- Function to check whether `instruction` references (retrieves the value of) register `target`
        if table.find({ -- Below is a list of functions which reference only one register and it references it in instr.b
            "MOVE", "GETTABLEKS", "SETTABLEKS", "GETTABLEN", "NAMECALL", "ADDK",
            "SUBK", "MULK", "DIVK", "MODK", "POWK", "ANDK", "ORK", "NOT", "MINUS",
            "LENGTH", "CAPTURE", "FASTCALL1", "FASTCALL2K", "IDIVK", "JUMPXEQKNIL",
            "JUMPXEQKB", "JUMPXEQKN", "JUMPXEQKS"
        }, instruction.name) then
            return instruction.b == target -- Return whether it's referenced
        elseif instruction.name == "CALL" then
            return target == instruction.b or (instruction.c ~= 0 and target == instruction.a + 1 + instruction.c)
        elseif table.find({
            "JUMPIFEQ", "JUMPIFLE", "JUMPIFLT",
            "JUMPIFNOTEQ", "JUMPIFNOTLE", "JUMPIFNOTLT"
        }, instruction.name) then
            return instruction.a == target or instruction.aux == target
        elseif table.find({
            "JUMPIF", "JUMPIFNOT",
        }, instruction.name) then
            return instruction.a == target
        elseif instruction.name == "FASTCALL2" then
            return instruction.aux == target
        elseif table.find({ -- Below is a list of instructions which reference two registers
            "GETTABLE", "SETTABLE", "ADD", "SUB", "MUL", "DIV", "MOD", "POW", "AND", "OR", "IDIV"
        }, instruction.name) then
            return (instruction.a == instruction.b and instruction.a == target) or instruction.b == target or instruction.c == target -- return if the register is referenced
        elseif instruction.name == "CONCAT" then
            return target >= instruction.b and target <= instruction.c
        elseif instruction.name == "SETLIST" then
            return target >= instruction.b and target <= instruction.b + instruction.c
        end
        return false
    end
    local function WritesTo(instruction: luau_instruction, _table: number): boolean
        if not table.find({
            "SETTABLE", "SETTABLEKS", "SETTABLEN"
        }, instruction.name) then
            return false
        end
        return _table == instruction.b
    end
    local function Captures(instruction: luau_instruction, target: number): boolean
        return instruction.name == "CAPTURE" and instruction.b == target
    end

    local function GetConstant(proto: luau_proto, index: number, isglobal: boolean?): Value -- Simple function to retrieve the constant at index `index` of proto `proto`. It also fetches strings and produces Refounc-pass friendly values
        local const: luau_constant = proto.constants[index + 1]
        if const.type == 0 then
            return {
                Type = "Nil"
            }
        elseif const.type == 1 then
            return {
                Type = "Boolean",
                Value = const.value :: boolean
            }
        elseif const.type == 2 then
            return {
                Type = "Number",
                Value = const.value :: number
            }
        elseif const.type == 3 then
            if not isglobal then
                return {
                    Type = "String",
                    Value = bytecode.strings[const.value :: number]
                }
            else 
                return {
                    Type = "Global",
                    Value = bytecode.strings[const.value :: number]
                }
            end
        elseif const.type == 4 then
            return {
                Type = "LBC_Import",
                Value = const.value :: number
            }
        elseif const.type == 5 then
            return {
                Type = "LBC_Table",
                Value = const.value :: { number }
            }
        elseif const.type == 6 then
            return {
                Type = "LBC_Closure",
                Value = const.value :: number
            }
        end
        return {
            Type = "Nil"
        }
    end

    local function RefcountProto(proto: luau_proto, upvalues: { Local }, passed_regs: { [number]: Value }?): { Instruction } -- Function to filter out any immediate values from actual varables, allowing for very simplistic decompilation
        local instructions: { Instruction } = {}
        local idx = 1
        local vars: { Local } = {}
        local regs: { [number]: Value } = passed_regs or {}
        local regs_stack: { { [number]: Value } } = {}
        local multret_start: number = -1 -- Instructions with MULTRET (CALL, RETURN, etc) will always tell u the starting register, so we can use that to emit MULTRET stuff
        local function Variable(instr: Instruction, value: Value, target: number, force_emit: boolean?): boolean
            local refs = 0
            local loop_depth = 0

            if not force_emit then
                local writes = 0
                local ends: { number } = {}
                for i = value.Type == "InlineNamecall" and idx or idx + 1, #proto.instructions do
                    if Captures(proto.instructions[i], target) then
                        force_emit = true
                        break
                    end
                    if WritesTo(proto.instructions[i], target) then
                        writes += 1
                        if writes >= 2 then
                            force_emit = true
                            break
                        end
                    end
                    if loop_depth == 0 and ReAssigns(proto.instructions[i], target) then
                        break
                    elseif ReAssigns(proto.instructions[i], target) then
                        -- Check if a variable assigned to inside a scope is used outside the scope, if not, don't emit (unless other conditions met)
                        local exitted_scope = false
                        local new_i = 0
                        for j = i, #proto.instructions do
                            for _, x in next, ends do
                                if j == x then
                                    exitted_scope = true
                                    new_i = j
                                    break
                                end
                            end
                            if exitted_scope then break end
                        end
                        local outside_refs = 0
                        for j = new_i, #proto.instructions do
                            if References(proto.instructions[j], target) then
                                outside_refs += 1
                            end
                        end
                        if outside_refs <= 1 then
                            break
                        end
                    end
                    if References(proto.instructions[i], target) then
                        refs += 1
                    end
                    -- TODO: Special case for JUMPX
                    if table.find({"JUMP", "JUMPIF", "JUMPIFNOT", "JUMPIFEQ", "JUMPIFNOTEQ", "JUMPIFLT", "JUMPIFLE", "JUMPIFNOTLT", "JUMPIFNOTLE", "FORGPREP", "FORGPREP_NEXT", "FORGPREP_INEXT", "FORNPREP"}, proto.instructions[i].name) then
                        loop_depth += 1
                        local bytes_counted: number = 0
                        local ninstructions: number = 0
                        while bytes_counted < proto.instructions[i].d do
                            bytes_counted += 1
                            ninstructions += 1
                            if proto.instructions[idx + ninstructions].aux ~= nil then
                                bytes_counted += 1
                            end
                        end
                        table.insert(ends, i + ninstructions)
                    end
                    for _, j in next, ends do
                        if i == j then
                            loop_depth -= 1
                        end
                    end
                end
            end

            if (value.Type == "InlineNamecall" or value.Type == "InlineCall") and refs == 0 then
                force_emit = true
            end

            -- TODO: Check for self reassigns and ALWAYS emit them
            -- Ex: a = a + 1 (or a += 1)

            if refs > 1 or force_emit then
                if not regs[target] or regs[target].Type ~= "Local" or (regs[target] :: Local).Scope ~= proto or (regs[target] and regs[target].Type == "Local" and (regs[target] :: Local).CreatedByCall) then
                    regs[target] = {
                        Type = "Local",
                        Id = "",
                        Value = value,
                        Latest = value,
                        Register = target,
                        Scope = proto
                    }
                    instr.ArgA = regs[target]
                    table.insert(instructions, instr)
                elseif regs[target] and regs[target].Type == "Local" then
                    instr.ArgA = regs[target]
                    table.insert(instructions, instr)
                end
            else
                regs[target] = value
                table.insert(instructions, {
                    Name = "NOP"
                })
            end
            return (refs > 1 or force_emit) :: boolean
        end

        local function CloneRegs(nbytes: number)
            -- This function will replace the regs table with an exact clone for nbytes bytes, as
            -- it separates newly created variables within the scope from variables outside the scope, while
            -- still alowing for re-assigns

            -- below code converts the number of bytes to a number of instructions by counting instructions with an AUX as 2 bytes.
            local bytes_counted: number = 0
            local ninstructions: number = 0
            while bytes_counted <= nbytes and idx + ninstructions < #proto.instructions do
                bytes_counted += 1
                ninstructions += 1
                if proto.instructions[idx + ninstructions] and proto.instructions[idx + ninstructions].aux ~= nil then
                    bytes_counted += 1
                end
            end

            local regs_clone: { [number] : Value } = {}
            for k, v in next, regs do
                regs_clone[k] = v
            end
            if not regs_stack[idx + ninstructions] then
                regs_stack[idx + ninstructions] = regs_clone
            end
        end
        
        while idx <= #proto.instructions do
            for index, old_regs in next, regs_stack do
                if idx == index and old_regs then
                    table.clear(regs)
                    for k, v in next, old_regs do
                        regs[k] = v
                    end
                    regs_stack[idx] = nil
                    break
                end
            end
            local function GetUpvalues(): { Local }
                local l_upvalues: { Local } = {}
                while proto.instructions[idx + 1].name == "CAPTURE" do
                    if proto.instructions[idx + 1].a <= 1 then
                        table.insert(l_upvalues, regs[proto.instructions[idx + 1].b] :: Local)
                    elseif proto.instructions[idx + 1].a == 2 then
                        table.insert(l_upvalues, upvalues[proto.instructions[idx + 1].b + 1])
                    end
                    table.insert(instructions, { Name = "NOP" })
                    idx += 1
                end
                return l_upvalues
            end
            local current: luau_instruction = proto.instructions[idx]
            if current.name == "LOADN" then
                Variable({ -- this is a VERY gross format to call a function lmfao
                    Name = "LOADN",
                    ArgB = {
                        Type = "Number",
                        Value = current.d
                    }
                }, {
                    Type = "Number",
                    Value = current.d
                }, current.a)
            elseif current.name == "LOADB" then
                Variable({
                    Name = "LOADB",
                    ArgB = {
                        Type = "Boolean",
                        Value = current.b == 1
                    },
                    ArgC = {
                        Type = "Number",
                        Value = current.c
                    }
                }, {
                    Type = "Boolean",
                    Value = current.b == 1
                }, current.a)
            elseif current.name == "LOADK" then
                Variable({
                    Name = "LOADK",
                    ArgB = GetConstant(proto, current.d)
                }, GetConstant(proto, current.d), current.a)
            elseif current.name == "LOADNIL" then
                Variable({
                    Name = "LOADNIL",
                    ArgB = {Type = "Nil"}
                }, {Type = "Nil"}, current.a)
            elseif current.name == "MOVE" then
                Variable({
                    Name = "MOVE",
                    ArgB = regs[current.b]
                }, regs[current.b], current.a)
            elseif current.name == "GETGLOBAL" then
                Variable({
                    Name = "GETGLOBAL",
                    ArgAux = GetConstant(proto, current.aux :: number, true)
                }, GetConstant(proto, current.aux :: number, true), current.a)
            elseif current.name == "SETGLOBAL" then
                table.insert(instructions, {
                    Name = "SETGLOBAL",
                    ArgA = regs[current.a],
                    ArgAux = GetConstant(proto, current.aux :: number)
                })
            elseif current.name == "GETUPVAL" then
                Variable({
                    Name = "GETUPVAL",
                    ArgB = upvalues[current.b + 1]
                }, upvalues[current.b + 1], current.a)
            elseif current.name == "SETUPVAL" then
                table.insert(instructions, {
                    Name = "SETUPVAL",
                    ArgA = regs[current.a],
                    ArgB = upvalues[current.b + 1]
                })
            elseif current.name == "GETIMPORT" then
                local path_length: number = bit32.rshift(current.aux :: number, 30)
                local path = {table.unpack({bit32.band(bit32.rshift(current.aux :: number, 20), 0b1111111111), bit32.band(bit32.rshift(current.aux :: number, 10), 0b1111111111), bit32.band(bit32.rshift(current.aux :: number, 0), 0b1111111111)}, 1, path_length)}
                
                local import_path: Value
                
                if path_length == 1 then
                    import_path = {
                        Type = "Global",
                        Value = (GetConstant(proto, path[1]) :: String).Value
                    }
                elseif path_length == 2 then
                    import_path = {
                        Type = "TableIndex",
                        Table = {
                            Type = "Global",
                            Value = (GetConstant(proto, path[1]) :: String).Value
                        },
                        Index = GetConstant(proto, path[2]) :: String
                    }
                else
                    import_path = {
                        Type = "TableIndex",
                        Table = {
                            Type = "TableIndex",
                            Table = {
                                Type = "Global",
                                Value = (GetConstant(proto, path[1]) :: String).Value
                            },
                            Index = GetConstant(proto, path[2]) :: String
                        },
                        Index = GetConstant(proto, path[3]) :: String
                    }
                end
                Variable({
                    Name = "GETIMPORT",
                    Special = import_path
                }, import_path, current.a)
            elseif current.name == "GETTABLE" then -- TODO: Fix up table shit with chaining. Nvm I think it works????
                Variable({
                    Name = "GETTABLE",
                    Special = {
                        Type = "TableIndex",
                        Table = regs[current.b],
                        Index = regs[current.c]
                    }
                }, {
                    Type = "TableIndex",
                    Table = regs[current.b],
                    Index = regs[current.c]
                }, current.a)
            elseif current.name == "SETTABLE" then
                table.insert(instructions, {
                    Name = "SETTABLE",
                    ArgA = regs[current.a],
                    ArgB = regs[current.b],
                    ArgC = regs[current.c]
                })
            elseif current.name == "GETTABLEKS" then
                Variable({
                    Name = "GETTABLE",
                    Special = {
                        Type = "TableIndex",
                        Table = regs[current.b],
                        Index = GetConstant(proto, current.aux :: number)
                    }
                }, {
                    Type = "TableIndex",
                    Table = regs[current.b],
                    Index = GetConstant(proto, current.aux :: number)
                }, current.a)
            elseif current.name == "SETTABLEKS" then
                table.insert(instructions, {
                    Name = "SETTABLE",
                    ArgA = regs[current.a],
                    ArgB = regs[current.b],
                    ArgC = GetConstant(proto, current.aux :: number)
                })
            elseif current.name == "GETTABLEN" then
                Variable({
                    Name = "GETTABLE",
                    Special = {
                        Type = "TableIndex",
                        Table = regs[current.b],
                        Index = {
                            Type = "Number",
                            Value = current.c
                        }
                    }
                }, {
                    Type = "TableIndex",
                    Table = regs[current.b],
                    Index ={
                        Type = "Number",
                        Value = current.c
                    }
                }, current.a)
            elseif current.name == "SETTABLEN" then
                table.insert(instructions, {
                    Name = "SETTABLEN",
                    ArgA = regs[current.a],
                    ArgB = regs[current.b],
                    ArgC = {
                        Type = "Number",
                        Value = current.c
                    }
                })
            elseif current.name == "NEWCLOSURE" then
                local _proto = bytecode.protos[proto.child_protos[current.d + 1] + 1]
                local params: { Local } = {}
                local val: Closure = {
                    Type = "Closure",
                    Value = {},
                    Params = params,
                    IsVarArg = _proto.isvararg,
                    Name = _proto.name or "",
                    UpValues = upvalues,
                    IsGlobal = false,
                    ParamTypes = _proto.types.data
                }
                Variable({
                    Name = "NEWCLOSURE",
                    ArgD = val
                }, val, current.a, _proto.name ~= nil)
                local upvals: { Local } = GetUpvalues()
                local regs_params: { [number]: Value } = {}
                for i = 1, _proto.numparams do
                    table.insert(params, {
                        Type = "Local",
                        Id = "",
                        Register = i - 1,
                        Value = { Type = "Nil" },
                        Latest = { Type = "Nil" },
                        Scope = proto
                    })
                    regs_params[i - 1] = params[#params]
                end
                val.Value = RefcountProto(_proto, upvals, regs_params)

                if proto.instructions[idx + 1].name == "SETGLOBAL" then
                    local ins: luau_instruction = proto.instructions[idx + 1]
                    if ins.a == current.a then
                        val.IsGlobal = true
                        idx += 1
                    end
                end
            elseif current.name == "NAMECALL" then
                local call_instr: luau_instruction = proto.instructions[idx + 1]
                idx += 1 -- Consume the call
                table.insert(instructions, { Name = "NOP" }) -- Add a NOP to balance out the missing CALL
                local args: { Value } = {}
                local retvals: { Local } = {}
                for i = call_instr.a + 2, call_instr.a + call_instr.b - 1 do -- We add one to the args start because the first argument will be `self`
                    table.insert(args, regs[i])
                end
                if call_instr.b == 0 then
                    for i = call_instr.a + 2, call_instr.a + multret_start do
                        table.insert(args, regs[i])
                    end
                end
                for i = 1, call_instr.c - 1 do
                    table.insert(retvals, {
                        Type = "Local",
                        Id = "",
                        Register = call_instr.a + i - 1,
                        Value = { Type = "Nil" },
                        Latest = { Type = "Nil" },
                        Scope = proto,
                        CreatedByCall = true
                    })
                    table.insert(vars, retvals[#retvals])
                end
                if call_instr.c == 0 then
                    multret_start = current.a
                    regs[current.a] = {
                        Type = "InlineNamecall",
                        Args = args,
                        Target = GetConstant(proto, current.aux :: number) :: String,
                        Object = regs[current.b]
                    }
                    if not regs[call_instr.a] then
                        regs[call_instr.a] = {
                            Type = "Local",
                            Register = call_instr.a,
                            Value = regs[current.a - 1],
                            Latest = regs[current.a - 1],
                            Id = "",
                            Scope = proto
                        }
                        table.insert(vars, regs[call_instr.a] :: Local)
                    end
                end
                if call_instr.c ~= 0 then
                    local emitted: boolean = true
                    if call_instr.c == 2 then
                        emitted = Variable({
                            Name = "NAMECALL",
                            Special = { args, retvals },
                            ArgB = regs[current.b],
                            ArgAux = GetConstant(proto, current.aux :: number)
                        }, {
                            Type = "InlineNamecall",
                            Args = args,
                            Target = GetConstant(proto, current.aux :: number) :: String,
                            Object = regs[current.b]
                        }, current.a)
                    else
                        table.insert(instructions, {
                            Name = "NAMECALL",
                            Special = { args, retvals },
                            ArgB = regs[current.b],
                            ArgAux = GetConstant(proto, current.aux :: number)
                        })
                    end
                    if emitted then
                        for i, retval in next, retvals do -- explained below
                            regs[current.a + i - 1] = retval
                            table.insert(vars, regs[current.a + i - 1] :: Local)
                        end
                    end
                end
            elseif current.name == "CALL" then
                local args: { Value } = {}
                local retvals: { Local } = {}
                for i = current.a + 1, current.a + current.b - 1 do
                    table.insert(args, regs[i])
                end
                if current.b == 0 then
                    for i = current.a + 1, multret_start do
                        table.insert(args, regs[i])
                    end
                end
                for i = 1, current.c - 1 do
                    table.insert(retvals, {
                        Type = "Local",
                        Id = "",
                        Register = current.a + i - 1,
                        Value = { Type = "Nil" },
                        Latest = { Type = "Nil" },
                        Scope = proto,
                        CreatedByCall = true
                    })
                end
                local emitted: boolean = true
                local target = regs[current.a]
                if current.c > 0 then
                    if current.c == 2 then 
                        emitted = Variable({
                            Name = "CALL",
                            ArgA = target,
                            Special = { args, retvals },
                        }, {
                            Type = "InlineCall",
                            Args = args,
                            Target = target
                        }, current.a)
                        if emitted then -- Set the ArgA back to the function target
                            instructions[#instructions].ArgA = target
                        end
                    else
                        table.insert(instructions, {
                            Name = "CALL",
                            ArgA = target,
                            Special = { args, retvals },
                        })
                    end
                else
                    table.insert(instructions, { -- Create a NOP as an inline call will be emitted by a MULTRET instr
                        Name = "NOP"
                    })
                    multret_start = current.a
                    regs[current.a] = {
                        Type = "InlineCall",
                        Args = args,
                        Target = regs[current.a]
                    }
                end
                if emitted then
                    for i, retval in next, retvals do -- Return values of functions are written to where the function lives, so we have to replace the function (and its arguments) with the return values so it can be referenced later
                        regs[current.a + i - 1] = retval
                        table.insert(vars, regs[current.a + i - 1] :: Local)
                    end
                end
            elseif current.name == "RETURN" then
                local values: { Value } = {}
                for i = 1, current.b - 1 do
                    table.insert(values, regs[current.a + i - 1])
                end
                if current.b == 0 then
                    for i = current.a, multret_start do
                        table.insert(values, regs[i])
                    end
                end
                table.insert(instructions, {
                    Name = "RETURN",
                    Special = values
                })
            elseif current.name == "JUMP" then
                CloneRegs(current.d)
                table.insert(instructions, {
                    Name = "JUMP",
                    ArgD = {
                        Type = "Number",
                        Value = current.d
                    }
                })
            elseif current.name == "JUMPBACK" then
                table.insert(instructions, {
                    Name = "JUMPBACK",
                    ArgD = {
                        Type = "Number",
                        Value = current.d
                    }
                })
            elseif current.name == "JUMPIF" then
                CloneRegs(current.d)
                table.insert(instructions, {
                    Name = "JUMPIF",
                    ArgA = regs[current.a],
                    ArgD = {
                        Type = "Number",
                        Value = current.d
                    }
                })
            elseif current.name == "JUMPIFNOT" then
                CloneRegs(current.d)
                table.insert(instructions, {
                    Name = "JUMPIFNOT",
                    ArgA = regs[current.a],
                    ArgD = {
                        Type = "Number",
                        Value = current.d
                    }
                })
            elseif table.find({"JUMPIFEQ", "JUMPIFLE", "JUMPIFLT", "JUMPIFNOTEQ", "JUMPIFNOTLE", "JUMPIFNOTLT"}, current.name) then
                CloneRegs(current.d)
                table.insert(instructions, {
                    Name = current.name,
                    ArgA = regs[current.a],
                    ArgD = {
                        Type = "Number",
                        Value = current.d
                    },
                    ArgAux = regs[current.aux :: number]
                })
            elseif table.find({"ADD", "SUB", "MUL", "DIV", "MOD", "POW"}, current.name) then
                Variable({
                    Name = current.name,
                    Special = {
                        Type = "Expression",
                        Op = ({ADD="+", SUB="-", MUL="*", DIV="/", MOD="%", POW="^"})[current.name],
                        Lhs = regs[current.b],
                        Rhs = regs[current.c]
                    }
                }, {
                    Type = "Expression",
                    Op = ({ADD="+", SUB="-", MUL="*", DIV="/", MOD="%", POW="^"})[current.name],
                    Lhs = regs[current.b],
                    Rhs = regs[current.c]
                }, current.a)
            elseif table.find({"ADDK", "SUBK", "MULK", "DIVK", "MODK", "POWK"}, current.name) then
                Variable({
                    Name = current.name,
                    Special = {
                        Type = "Expression",
                        Op = ({ADDK="+", SUBK="-", MULK="*", DIVK="/", MODK="%", POWK="^"})[current.name],
                        Lhs = regs[current.b],
                        Rhs = GetConstant(proto, current.c)
                    }
                }, {
                    Type = "Expression",
                    Op = ({ADDK="+", SUBK="-", MULK="*", DIVK="/", MODK="%", POWK="^"})[current.name],
                    Lhs = regs[current.b],
                    Rhs = GetConstant(proto, current.c)
                }, current.a)
            elseif table.find({"AND", "OR"}, current.name) then
                Variable({
                    Name = current.name,
                    Special = {
                        Type = "Expression",
                        Op = ({AND="and", OR="or"})[current.name],
                        Lhs = regs[current.b],
                        Rhs = regs[current.c]
                    }
                }, {
                    Type = "Expression",
                    Op = ({AND="and", OR="or"})[current.name],
                    Lhs = regs[current.b],
                    Rhs = regs[current.c]
                }, current.a)
            elseif table.find({"ANDK", "ORK"}, current.name) then
                Variable({
                    Name = current.name,
                    Special = {
                        Type = "Expression",
                        Op = ({ANDK="and", ORK="or"})[current.name],
                        Lhs = regs[current.b],
                        Rhs = regs[current.c]
                    }
                }, {
                    Type = "Expression",
                    Op = ({ANDK="and", ORK="or"})[current.name],
                    Lhs = regs[current.b],
                    Rhs = regs[current.c]
                }, current.a)
            elseif table.find({"NOT", "MINUS", "LENGTH"}, current.name) then
                Variable({
                    Name = current.name,
                    Special = {
                        Type = "Unary",
                        Op = ({NOT="not ", MINUS="-", LENGTH="#"})[current.name],
                        Value = regs[current.b]
                    }
                }, {
                    Type = "Unary",
                    Op = ({NOT="not ", MINUS="-", LENGTH="#"})[current.name],
                    Value = regs[current.b]
                }, current.a)
            elseif current.name == "CONCAT" then
                local values: { Value } = {}
                for i = current.b, current.c do
                    table.insert(values, regs[i])
                end
                Variable({
                    Name = "CONCAT",
                    Special = {
                        Type = "Strings",
                        Value = values
                    }
                }, {
                    Type = "Strings",
                    Value = values
                }, current.a)
            elseif current.name == "NEWTABLE" then -- Actual table is defined in SETLIST
                if not current.aux or current.aux == 0 or regs[current.a] then
                    Variable({
                        Name = "NEWTABLE",
                        Special = {
                            Type = "Table",
                            Value = {}
                        },
                        ArgAux = {
                            Type = "Number",
                            Value = current.aux :: number
                        }
                    }, {
                        Type = "Table",
                        Value = {}
                    }, current.a)
                end
            elseif current.name == "DUPTABLE" then
                if regs[current.a] and regs[current.a].Type == "Local" then
                    table.insert(instructions, {
                        Name = "DUPTABLE",
                        ArgA = regs[current.a],
                        ArgD = GetConstant(proto, current.d)
                    })
                else
                    Variable({
                        Name = "DUPTABLE",
                        ArgD = GetConstant(proto, current.d)
                    }, GetConstant(proto, current.d), current.a)
                end
            elseif current.name == "SETLIST" then
                local values: { Value } = {}
                for i = 1, current.c - 1 do
                    table.insert(values, regs[current.b + i - 1])
                end
                if current.c == 0 then -- MULTRET
                    for i = current.b, multret_start do
                        table.insert(values, regs[i])
                    end
                end
                Variable({ -- target is passed to SETLIST too so realisically NEWTABLE is useless?
                    Name = "SETLIST",
                    ArgAux = {
                        Type = "Number",
                        Value = current.aux :: number
                    },
                    Special = {
                        Type = "Table",
                        Value = values
                    }
                }, {
                    Type = "Table",
                    Value = values
                }, current.a)
            elseif current.name == "FORNPREP" then
                CloneRegs(current.d)
                local limit, step, index = regs[current.a], regs[current.a + 1], regs[current.a + 2]
                local iter: Local = {
                    Type = "Local",
                    Id = "",
                    Latest = index,
                    Value = index,
                    Register = current.a + 2,
                    Scope = proto
                }
                table.insert(instructions, {
                    Name = "FORNPREP",
                    ArgA = regs[current.a + 3],
                    ArgD = {
                        Type = "Number",
                        Value = current.d
                    },
                    Special = {
                        index, limit, step, iter -- Arranged in the way you'd naturally write them (1, 10, 2 as index=1, limit=10, step=2)
                    }
                })
                regs[current.a + 2] = iter
                table.insert(vars, regs[current.a + 2] :: Local)
            elseif table.find({"FORGPREP", "FORGPREP_NEXT", "FORGPREP_INEXT"}, current.name) then
                CloneRegs(current.d)
                local generator, state, index = regs[current.a], regs[current.a + 1], regs[current.a + 2] -- Index is pretty much always a NIL I think
                local variables: { Local } = {}
                local for_end: luau_instruction
                local l = 1
                local offset = 0
                local naux = 0
                for i = idx + 1, #proto.instructions do
                    if proto.instructions[i].aux ~= nil then
                        naux += 1
                    end
                    if table.find({"FORGPREP", "FORGPREP_NEXT", "FORGPREP_INEXT"}, proto.instructions[i].name) then
                        l += 1
                    end
                    if proto.instructions[i].name == "FORGLOOP" then
                        l -= 1
                        for_end = proto.instructions[i]
                        offset = i - idx -- + naux
                    end
                    if l == 0 then
                        break
                    end
                end
                for i = 1, bit32.band(for_end.aux :: number, 0b11111111) do
                    regs[current.a + 2 + i] = {
                        Type = "Local",
                        Id = "",
                        Latest = index,
                        Value = index,
                        Register = current.a + 2 + i,
                        Scope = proto
                    }
                    table.insert(variables, regs[current.a + 2 + i] :: Local)
                    table.insert(vars, regs[current.a + 2 + i] :: Local)
                end
                table.insert(instructions, {
                    Name = "FORGPREP",
                    ArgD = {
                        Type = "Number",
                        Value = offset -- Pass a custom offset here as it is the correct one
                    },
                    Special = {
                        variables, -- for some fucking reason, the LSP I use hates when this is at the end of specials?????
                        generator, -- ^^ I assume it was tryin to cast Special into a { ValueNode } ?
                        state,
                        index
                    }
                })
            elseif current.name == "GETVARARGS" then
                multret_start = current.a
                Variable({
                    Name = "GETVARARGS",
                    Special = {
                        Type = "VarArgs"
                    }
                }, {
                    Type = "VarArgs"
                }, current.a)
            elseif current.name == "DUPCLOSURE" then -- I honestly have 0 idea why the fuck this isn't just NEWCLOSURE. DUPCLOSURE is literally NEWCLOSURE except the proto index is in the constant table ????? As if scripts have 32k protos ???? 
                local _proto = bytecode.protos[proto.constants[current.d + 1].value :: number + 1]
                local params: { Local } = {}
                local val: Closure = {
                    Type = "Closure",
                    Value = {},
                    Params = params,
                    IsVarArg = _proto.isvararg,
                    Name = _proto.name or "",
                    UpValues = upvalues,
                    IsGlobal = false,
                    ParamTypes = _proto.types.data
                }
                Variable({
                    Name = "NEWCLOSURE",
                    ArgD = val
                }, val, current.a, _proto.name ~= nil)
                local upvals: { Local } = GetUpvalues()
                local regs_params: { [number]: Value } = {}
                for i = 1, _proto.numparams do
                    table.insert(params, {
                        Type = "Local",
                        Id = "",
                        Register = i - 1,
                        Value = { Type = "Nil" },
                        Latest = { Type = "Nil" },
                        Scope = proto
                    })
                    regs_params[i - 1] = params[#params]
                end
                val.Value = RefcountProto(_proto, upvals, regs_params)
            elseif current.name == "LOADKX" then
                Variable({
                    Name = "LOADKX",
                    ArgAux = GetConstant(proto, current.aux :: number)
                }, GetConstant(proto, current.aux :: number), current.a)
            elseif current.name == "JUMPX" then
                table.insert(instructions, {
                    Name = "JUMPX",
                    ArgE = {
                        Type = "Number",
                        Value = current.e
                    }
                })
            elseif current.name == "FASTCALL" then
                local following_call: luau_instruction = proto.instructions[idx + current.c]
                local args: { Value } = {}
                local retvals: { Local } = {}
                for i = following_call.a + 1, following_call.a + following_call.b - 1 do
                    table.insert(args, regs[i])
                end
                if following_call.b == 0 then
                    for i = following_call.a + 1, multret_start do
                        table.insert(args, regs[i])
                    end
                end
                for i = 1, following_call.c - 1 do
                    table.insert(retvals, {
                        Type = "Local",
                        Id = "",
                        Register = following_call.a + i - 1,
                        Value = { Type = "Nil" },
                        Latest = { Type = "Nil" },
                        Scope = proto,
                        CreatedByCall = true
                    })
                end
                local emitted: boolean = true
                if following_call.c == 0 then
                    multret_start = following_call.a
                    regs[following_call.a] = {
                        Type = "InlineCall",
                        Args = args,
                        Target = {
                            Type = "Number",
                            Value = current.a
                        }
                    }
                    table.insert(instructions, {
                        Name = "NOP"
                    })
                elseif following_call.c == 2 then
                    emitted = Variable({
                        Name = "CALL",
                        ArgA = {
                            Type = "Number",
                            Value = current.a
                        },
                        Special = { args, retvals },
                        ArgC = {
                            Type = "Number",
                            Value = following_call.c
                        }
                    }, {
                        Type = "InlineCall",
                        Args = args,
                        Target = {
                            Type = "Number",
                            Value = current.a
                        }
                    }, following_call.a)
                    if emitted then -- Set the ArgA back to the function target
                        instructions[#instructions].ArgA = {
                            Type = "Number",
                            Value = current.a
                        }
                    end
                else
                    table.insert(instructions, {
                        Name = "CALL",
                        ArgA = { -- NOTE: Decompiler should detect that thsi is a number and convert it to a builtin call
                            Type = "Number",
                            Value = current.a
                        },
                        Special = { args, retvals },
                        ArgC = {
                            Type = "Number",
                            Value = following_call.c
                        }
                    })
                end
                if emitted then
                    for i, retval in next, retvals do
                        regs[following_call.a + i - 1] = retval
                        table.insert(vars, regs[following_call.a + i - 1] :: Local)
                    end
                end
                idx += current.c -- Skip GETUPVAL/MOVE/GETIMPORT + CALL
            elseif current.name == "FASTCALL1" then
                local following_call: luau_instruction = proto.instructions[idx + current.c]
                local args: { Value } = { regs[current.b] }
                local retvals: { Local } = {}
                for i = 1, following_call.c - 1 do
                    table.insert(retvals, {
                        Type = "Local",
                        Id = "",
                        Register = following_call.a + i - 1,
                        Value = { Type = "Nil" },
                        Latest = { Type = "Nil" },
                        Scope = proto,
                        CreatedByCall = true
                    })
                end
                local emitted: boolean = true
                if following_call.c == 0 then
                    multret_start = following_call.a
                    regs[following_call.a] = {
                        Type = "InlineCall",
                        Args = args,
                        Target = {
                            Type = "Number",
                            Value = current.a
                        }
                    }
                    table.insert(instructions, {
                        Name = "NOP"
                    })
                elseif following_call.c == 2 then
                    emitted = Variable({
                        Name = "CALL",
                        ArgA = {
                            Type = "Number",
                            Value = current.a
                        },
                        Special = { args, retvals },
                        ArgC = {
                            Type = "Number",
                            Value = following_call.c
                        }
                    }, {
                        Type = "InlineCall",
                        Args = args,
                        Target = {
                            Type = "Number",
                            Value = current.a
                        }
                    }, following_call.a)
                    if emitted then -- Set the ArgA back to the function target
                        instructions[#instructions].ArgA = {
                            Type = "Number",
                            Value = current.a
                        }
                    end
                else
                    table.insert(instructions, {
                        Name = "CALL",
                        ArgA = { -- NOTE: Decompiler should detect that thsi is a number and convert it to a builtin call
                            Type = "Number",
                            Value = current.a
                        },
                        Special = {args, retvals},
                        ArgC = {
                            Type = "Number",
                            Value = following_call.c
                        }
                    })
                end
                if emitted then
                    for i, retval in next, retvals do
                        regs[following_call.a + i - 1] = retval
                        table.insert(vars, regs[following_call.a + i - 1] :: Local)
                    end
                end
                idx += current.c -- Skip GETUPVAL/MOVE/GETIMPORT + CALL
            elseif current.name == "FASTCALL2" then
                local following_call: luau_instruction = proto.instructions[idx + current.c]
                local retvals: { Local } = {}
                for i = 1, following_call.c - 1 do
                    table.insert(retvals, {
                        Type = "Local",
                        Id = "",
                        Register = following_call.a + i - 1,
                        Value = { Type = "Nil" },
                        Latest = { Type = "Nil" },
                        Scope = proto,
                        CreatedByCall = true
                    })
                end
                local emitted: boolean = true
                if following_call.c == 0 then
                    multret_start = following_call.a
                    regs[following_call.a] = {
                        Type = "InlineCall",
                        Args = {regs[current.b], regs[current.aux :: number]},
                        Target = {
                            Type = "Number",
                            Value = current.a
                        }
                    }
                    table.insert(instructions, {
                        Name = "NOP"
                    })
                elseif following_call.c == 2 then
                    emitted = Variable({
                        Name = "CALL",
                        ArgA = {
                            Type = "Number",
                            Value = current.a
                        },
                        Special = { {regs[current.b], regs[current.aux :: number]}, retvals },
                        ArgC = {
                            Type = "Number",
                            Value = following_call.c
                        }
                    }, {
                        Type = "InlineCall",
                        Args = {regs[current.b], regs[current.aux :: number]},
                        Target = {
                            Type = "Number",
                            Value = current.a
                        }
                    }, following_call.a)
                    if emitted then -- Set the ArgA back to the function target
                        instructions[#instructions].ArgA = {
                            Type = "Number",
                            Value = current.a
                        }
                    end
                else
                    table.insert(instructions, {
                        Name = "CALL",
                        ArgA = { -- NOTE: Decompiler should detect that thsi is a number and convert it to a builtin call
                            Type = "Number",
                            Value = current.a
                        },
                        Special = {{regs[current.b], regs[current.aux :: number]}, retvals},
                        ArgC = {
                            Type = "Number",
                            Value = following_call.c
                        }
                    })
                end
                if emitted then
                    for i, retval in next, retvals do
                        regs[following_call.a + i - 1] = retval
                        table.insert(vars, regs[following_call.a + i - 1] :: Local)
                    end
                end
                idx += current.c -- Skip GETUPVAL/MOVE/GETIMPORT + CALL
            elseif current.name == "FASTCALL2K" then
                local following_call: luau_instruction = proto.instructions[idx + current.c - 1]
                local retvals: { Local } = {}
                for i = 1, following_call.c - 1 do
                    table.insert(retvals, {
                        Type = "Local",
                        Id = "",
                        Register = following_call.a + i - 1,
                        Value = { Type = "Nil" },
                        Latest = { Type = "Nil" },
                        Scope = proto,
                        CreatedByCall = true
                    })
                end
                local emitted: boolean = true
                if following_call.c == 0 then
                    multret_start = following_call.a
                    regs[following_call.a] = {
                        Type = "InlineCall",
                        Args = {regs[current.b], GetConstant(proto, current.aux :: number)},
                        Target = {
                            Type = "Number",
                            Value = current.a
                        }
                    }
                    table.insert(instructions, {
                        Name = "NOP"
                    })
                elseif following_call.c == 2 then
                    emitted = Variable({
                        Name = "CALL",
                        ArgA = {
                            Type = "Number",
                            Value = current.a
                        },
                        Special = { {regs[current.b], regs[current.aux :: number]}, retvals },
                        ArgC = {
                            Type = "Number",
                            Value = following_call.c
                        }
                    }, {
                        Type = "InlineCall",
                        Args = {regs[current.b], regs[current.aux :: number]},
                        Target = {
                            Type = "Number",
                            Value = current.a
                        }
                    }, following_call.a)
                    if emitted then -- Set the ArgA back to the function target
                        instructions[#instructions].ArgA = {
                            Type = "Number",
                            Value = current.a
                        }
                    end
                else
                    table.insert(instructions, {
                        Name = "CALL",
                        ArgA = { -- NOTE: Decompiler should detect that thsi is a number and convert it to a builtin call
                            Type = "Number",
                            Value = current.a
                        },
                        Special = {{regs[current.b], GetConstant(proto, current.aux :: number)}, retvals},
                        ArgC = {
                            Type = "Number",
                            Value = following_call.c
                        }
                    })
                end
                if emitted then
                    for i, retval in next, retvals do
                        regs[following_call.a + i - 1] = retval
                        table.insert(vars, regs[following_call.a + i - 1] :: Local)
                    end
                end
                idx += current.c - 1 -- Not sure why we subtract one here, the docs don't mention it
            elseif table.find({"JUMPXEQKNIL", "JUMPXEQKB"}, current.name) then
                table.insert(instructions, {
                    Name = current.name,
                    ArgA = regs[current.a],
                    ArgD = {
                        Type = "Number",
                        Value = current.d
                    },
                    Special = {
                        Type = "Expression",
                        Lhs = regs[current.a],
                        Op = bit32.band(current.aux :: number, 2147483648) and "~=" or "==",
                        Rhs = current.name == "JUMPXEQKNIL" and { Type = "Nil" } :: Nil or {
                            Type = "Boolean",
                            Value = bit32.band(current.aux :: number, 1) ~= 0
                        } :: Boolean
                    } :: Expression
                })
            elseif table.find({"JUMPXEQKN", "JUMPXEQKS"}, current.name) then
                table.insert(instructions, {
                    Name = current.name,
                    ArgD = {
                        Type = "Number",
                        Value = current.d
                    },
                    Special = {
                        Type = "Expression",
                        Lhs = regs[current.a],
                        Op = bit32.band(current.aux :: number, 2147483648) and "~=" or "==",
                        Rhs = GetConstant(proto, bit32.band(current.aux :: number, 16777215))
                    } :: Expression
                })
            elseif current.name == "IDIV" then
                Variable({
                    Name = "IDIV",
                    Special = {
                        Type = "Expression",
                        Op = "//",
                        Lhs = regs[current.b],
                        Rhs = regs[current.c]
                    }
                }, {
                    Type = "Expression",
                    Op = "//",
                    Lhs = regs[current.b],
                    Rhs = regs[current.c]
                }, current.a)
            elseif current.name == "IDIVK" then
                Variable({
                    Name = "IDIVK",
                    Special = {
                        Type = "Expression",
                        Op = "//",
                        Lhs = regs[current.b],
                        Rhs = GetConstant(proto, current.c)
                    }
                }, {
                    Type = "Expression",
                    Op = "//",
                    Lhs = regs[current.b],
                    Rhs = GetConstant(proto, current.c)
                }, current.a)
            else -- Here will mostly be FORNLOOP, FORGLOOP, and VARARG stuff. None of which we can recover any meaningful data from
                table.insert(instructions, {
                    Name = "NOP"
                })
            end
            if current.aux ~= nil then -- JUMP offsets don't account for AUX instructions. We just add 'em so the JUMPs become instruction offsets instead of byte offsets
                table.insert(instructions, {
                    Name = "NOP"
                })
            end
            idx += 1
        end
        return instructions
    end

    local function DecompileClosure(closure: Closure): ClosureNode
        local idx = 1

        local node: ClosureNode = {
            Type = "ClosureNode",
            Name = closure.Name,
            Params = closure.Params,
            IsVarArg = closure.IsVarArg,
            Body = {},
            ParamTypes = closure.ParamTypes
        }

        local defined_locals: { Local } = {}

        local function DecompileValue(value: Value?): ValueNode
            if not value then
                return { Type = "NilNode" } -- Should never happen, this is mostly to satisfy the static analyzer
            end
            if value.Type == "Boolean" then
                return {
                    Type = "BooleanNode",
                    Value = value.Value
                }
            elseif value.Type == "Number" then
                return {
                    Type = "NumberNode",
                    Value = value.Value
                }
            elseif value.Type == "Nil" then
                return {
                    Type = "NilNode"
                }
            elseif value.Type == "Expression" then
                return {
                    Type = "ExpressionNode",
                    Lhs = DecompileValue(value.Lhs),
                    Rhs = DecompileValue(value.Rhs),
                    Op = value.Op
                }
            elseif value.Type == "Unary" then
                return {
                    Type = "UnaryNode",
                    Value = DecompileValue(value.Value),
                    Op = value.Op
                }
            elseif value.Type == "Local" then
                return value -- Here we just pass the local as a pseudo-node because I am lazy and it doesn't matter
            elseif value.Type == "String" then
                return {
                    Type = "StringNode",
                    Value = value.Value
                } 
            elseif value.Type == "Strings" then
                local values: { ValueNode } = {}
                for _, value in value.Value do
                    table.insert(values, DecompileValue(value))
                end
                return {                    
                    Type = "StringsNode",
                    Value = values
                } 
            elseif value.Type == "Global" then
                return {
                    Type = "GlobalNode",
                    Value = value.Value
                } 
            elseif value.Type == "Closure" then
                return DecompileClosure(value)
            elseif value.Type == "TableIndex" then
                return {
                    Type = "TableIndexNode",
                    Table = DecompileValue(value.Table),
                    Index = DecompileValue(value.Index)
                }
            elseif value.Type == "Table" then
                local values: { ValueNode } = {}
                for _, v in next, value.Value do
                    table.insert(values, DecompileValue(v))
                end
                return {
                    Type = "TableNode",
                    Body = values
                }
            elseif value.Type == "VarArgs" then
                return {
                    Type = "VarArgsNode"
                }
            elseif value.Type == "InlineNamecall" then
                local args: { ValueNode } = {}
                for _, arg in next, value.Args do
                    table.insert(args, DecompileValue(arg))
                end
                return {
                    Type = "InlineNamecallNode",
                    Args = args,
                    Object = DecompileValue(value.Object),
                    Target = DecompileValue(value.Target) :: StringNode
                }
            elseif value.Type == "InlineCall" then
                local args: { ValueNode } = {}
                for _, arg in next, value.Args do
                    table.insert(args, DecompileValue(arg))
                end
                local target: ValueNode = DecompileValue(value.Target)
                if value.Target.Type == "Number" then
                    target = ConvertBuiltin(value.Target.Value)
                end
                return {
                    Type = "InlineCallNode",
                    Args = args,
                    Target = target
                }
            end
            return {
                Type = "NilNode" -- TODO: add remainding nodes. 
            }
        end

        local function Define(_local: Local, value: ValueNode): VarAssignNode | VarReassignNode
            if table.find(defined_locals, _local) then
                return {
                    Type = "VarReassignNode",
                    Target = _local,
                    Value = value
                }
            end
            table.insert(defined_locals, _local)
            return {
                Type = "VarAssignNode",
                Target = _local,
                Value = value
            }
        end

        local function BreaksScope(instructions: number): boolean -- Checks if a JUMP* instruction would continue the scope of a loop
            return false -- table.find({"JUMPBACK", "FORNLOOP", "FORGLOOP"}, closure.Value[idx + instructions - 2].Name) ~= nil
        end
        
        local function ContinuesScope(instructions: number): boolean -- Checks if a JUMP* instruction would continue the scope of a loop
            return false -- table.find({"JUMPBACK", "FORNLOOP", "FORGLOOP"}, closure.Value[idx + instructions - 3].Name) ~= nil
        end

        local function DecompileInstruction(instruction: Instruction): Node?
            if instruction.Name == "LOADNIL" then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.ArgB)))
            elseif instruction.Name == "LOADN" then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.ArgB)))
            elseif instruction.Name == "LOADB" then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.ArgB)))
            elseif instruction.Name == "LOADK" then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.ArgB)))
            elseif instruction.Name == "MOVE" then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.ArgB)))
            elseif instruction.Name == "GETGLOBAL" then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.ArgAux)))
            elseif instruction.Name == "SETGLOBAL" then
                return ({
                    Type = "GlobalDefinitionNode",
                    Target = (instruction.ArgAux :: String).Value,
                    Value = DecompileValue(instruction.ArgA)
                })
            elseif instruction.Name == "GETUPVAL" then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.ArgB)))
            elseif instruction.Name == "SETUPVAL" then
                return ({
                    Type = "VarReassignNode",
                    Target = instruction.ArgB :: Local, -- We know that this is a Local as an Upvalue CANNOT be omitted.
                    Value = DecompileValue(instruction.ArgA)
                })
            elseif instruction.Name == "NEWCLOSURE" then
                if (instruction.ArgD :: Closure).IsGlobal then
                    return ({
                        Type = "GlobalDefinitionNode",
                        Target = "", -- Name is provided by the closure passed in Value
                        Value = DecompileValue(instruction.ArgD)
                    })
                end
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.ArgD)))
            elseif instruction.Name == "CALL" then
                local args: { ValueNode } = {}
                for _, arg in next, instruction.Special[1] do
                    table.insert(args, DecompileValue(arg))
                end
                for _, var in next, instruction.Special[2] :: { Local } do
                    table.insert(defined_locals, var)
                end
                local target: ValueNode = DecompileValue(instruction.ArgA)
                if (instruction.ArgA :: Value).Type == "Number" then -- Builtin call
                    target = ConvertBuiltin((instruction.ArgA :: Number).Value)
                end
                return ( {
                    Type = "FunctionCallNode",
                    Args = args,
                    RetVals = instruction.Special[2],
                    Target = target
                })
            elseif table.find({"ADD", "SUB", "MUL", "DIV", "MOD", "POW", "ADDK", "SUBK", "MULK", "DIVK", "MODK", "POWK", "IDIV", "IDIVK", "AND", "OR", "ANDK", "ORK"}, instruction.Name) then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.Special)))
            elseif instruction.Name == "RETURN" then
                local values: { ValueNode } = {}
                for _, value in next, instruction.Special do
                    table.insert(values, DecompileValue(value))
                end
                return ( {
                    Type = "ReturnNode",
                    Values = values
                })
            elseif instruction.Name == "GETTABLE" then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.Special)))
            elseif instruction.Name == "SETTABLE" then
                return ({
                    Type = "TableAssignNode",
                    Table = DecompileValue(instruction.ArgB),
                    Index = DecompileValue(instruction.ArgC),
                    Source = DecompileValue(instruction.ArgA),
                })
            elseif instruction.Name == "NAMECALL" then
                local args: { ValueNode } = {}
                for _, arg in next, instruction.Special[1] do
                    table.insert(args, DecompileValue(arg))
                end
                
                for _, var in next, instruction.Special[2] :: { Local } do
                    table.insert(defined_locals, var)
                end
                return ({
                    Type = "NamecallNode",
                    Args = args,
                    RetVals = instruction.Special[2],
                    Object = DecompileValue(instruction.ArgB),
                    Target = DecompileValue(instruction.ArgAux) :: StringNode
                })
            elseif instruction.Name == "JUMP" then
                if BreaksScope((instruction.ArgD :: Number).Value) then -- Explicit cast; TODO: add error checking 
                    return ({
                        Type = "BreakNode"
                    })
                elseif ContinuesScope((instruction.ArgD :: Number).Value) then
                    return ({
                        Type = "ContinueNode"
                    })
                end
            elseif table.find({"JUMPIF", "JUMPIFNOT", "JUMPIFEQ", "JUMPIFLE", "JUMPIFLT", "JUMPIFNOTEQ", "JUMPIFNOTLE", "JUMPIFNOTLT", "JUMPXEQKNIL", "JUMPXEQKB", "JUMPXEQKN", "JUMPXEQKS"}, instruction.Name) then
                local opposites = {
                    JUMPIFEQ    = "~=",
                    JUMPIFNOTEQ = "==",
                    JUMPIFLT    = ">",
                    JUMPIFLE    = ">=",
                    JUMPIFNOTLE = "<=",
                    JUMPIFNOTLT = "<",
                }
                local operators = {
                    JUMPIFEQ    = "==",
                    JUMPIFNOTEQ = "~=",
                    JUMPIFLT    = "<",
                    JUMPIFLE    = "<=",
                    JUMPIFNOTLE = ">=",
                    JUMPIFNOTLT = ">",
                }
                local flipped = { -- This is not useful right now, but in the future this will be used to allow
                                  -- users to decide whether they want the variable on the left or right side of
                                  -- a given expression:
                                  -- `100 < v0` -> `v0 > 100`
                                  -- ^^ This is done by the Luau compiler as an optimization because the JUMPIFL*
                                  -- instructions are faster to compute. 
                    ["=="] = "~=",
                    ["<"]  = ">",
                    ["<="] = ">=",
                    [">"]  = "<",
                    [">="] = "<=",
                }

                local function GetSingularCondition(_instr: Instruction, flip: boolean): ValueNode
                    if _instr.Name == "JUMPIF" then
                        local cond = DecompileValue(_instr.ArgA)
                        if flip then
                            cond = {
                                Type = "UnaryNode",
                                Value = cond,
                                Op = "not "
                            }
                        end
                        return cond
                    elseif _instr.Name == "JUMPIFNOT" then
                        local cond = DecompileValue(_instr.ArgA)
                        if not flip then
                            cond = {
                                Type = "UnaryNode",
                                Value = cond,
                                Op = "not "
                            }
                        end
                        return cond
                    elseif table.find({"JUMPIFEQ", "JUMPIFLE", "JUMPIFLT", "JUMPIFNOTEQ", "JUMPIFNOTLE", "JUMPIFNOTLT"}, _instr.Name) then
                        return {
                            Type = "ExpressionNode",
                            Lhs = DecompileValue(_instr.ArgA),
                            Op = (flip and opposites or operators)[_instr.Name],
                            Rhs = DecompileValue(_instr.ArgAux)
                        }
                    else
                        return DecompileValue(_instr.Special)
                    end
                end

                local condition: ValueNode = GetSingularCondition(instruction, true)
                local body: { Node } = {}
                local elsebody: { Node } = {}

                local body_idx: number = idx
                local body_end_idx: number = idx + (instruction.ArgD :: Number).Value

                local is_while: boolean = closure.Value[body_end_idx] and closure.Value[body_end_idx].Name == "JUMPBACK" and body_end_idx + (closure.Value[body_end_idx].ArgD :: Number).Value <= idx

                if BreaksScope(body_end_idx) then -- Explicit cast; TODO: add error checking 
                    table.insert(body, {
                        Type = "BreakNode"
                    })
                elseif ContinuesScope(body_end_idx) then
                    table.insert(body, {
                        Type = "ContinueNode"
                    })
                else
                    idx = body_idx + 1
                    local did_jump = false
                    while idx < body_end_idx + 1 do
                        if closure.Value[idx] and closure.Value[idx].Name == "JUMP" and idx == body_end_idx then
                            did_jump = true
                            local _end_idx = idx + (closure.Value[idx].ArgD :: Number).Value
                            while idx < _end_idx do
                                idx += 1
                                local _node: Node? = DecompileInstruction(closure.Value[idx])
                                if _node then
                                    table.insert(elsebody, _node)
                                end
                            end
                            break
                        end
                        local _node: Node? = closure.Value[idx] and DecompileInstruction(closure.Value[idx]) or nil
                        if _node then
                            table.insert(body, _node)
                        end
                        idx += 1
                    end
                    if not did_jump then
                        idx -= 1
                    end
                end
                if is_while then
                    return ({
                        Type = "WhileNode",
                        Condition = condition,
                        Body = body
                    })
                end
                return ({
                    Type = "IfNode",
                    Body = body,
                    ElseBody = elsebody,
                    Condition = condition
                })
            elseif table.find({"NOT", "MINUS", "LENGTH"}, instruction.Name) then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.Special)))
            elseif instruction.Name == "CONCAT" then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.Special)))
            elseif instruction.Name == "NEWTABLE" then
                return (Define(instruction.ArgA :: Local, {
                    Type = "TableNode",
                    Body = {} -- NEWTABLE is only emitted when the table is empty, SETTABLE has body data
                }))
            elseif instruction.Name == "SETLIST" then
                local values: { ValueNode } = {}
                for _, value in next, instruction.Special.Value do
                    table.insert(values, DecompileValue(value))
                end
                return (Define(instruction.ArgA :: Local, {
                    Type = "TableNode",
                    Body = values
                }))
            elseif instruction.Name == "DUPTABLE" then
                return (Define(instruction.ArgA :: Local, {
                    Type = "TableNode",
                    Body = {} -- DUPTABLE uses a "template" which idrk what is so idc (I assume it has something to do with table size)
                }))
            elseif instruction.Name == "FORNPREP" then
                local body: { Node } = {}
                local end_idx = idx + (instruction.ArgD :: Number).Value
                while idx < end_idx do
                    idx += 1
                    local node: Node? = DecompileInstruction(closure.Value[idx])
                    if node then
                        table.insert(body, node)
                    end
                end
                return ({
                    Type = "ForRangeNode",
                    Index = DecompileValue(instruction.Special[1]),
                    Limit = DecompileValue(instruction.Special[2]),
                    Step = DecompileValue(instruction.Special[3]),
                    Iterator = instruction.Special[4],
                    Body = body
                })
            elseif instruction.Name == "FORGPREP" then
                local body: { Node } = {}
                local end_idx = idx + (instruction.ArgD :: Number).Value

                while idx <= end_idx do
                    idx += 1
                    local node: Node? = DecompileInstruction(closure.Value[idx])
                    if node then
                        table.insert(body, node)
                    end
                end
                return ({
                    Type = "ForValueNode",
                    Generator = DecompileValue(instruction.Special[2]),
                    State = DecompileValue(instruction.Special[3]),
                    Index = DecompileValue(instruction.Special[4]),
                    Variables = instruction.Special[1],
                    Body = body
                })
            elseif instruction.Name == "GETVARARGS" then
                return (Define(instruction.ArgA :: Local, {Type = "VarArgsNode"})) -- Stupidly easy to decompile lmfao
            elseif instruction.Name == "LOADKX" then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.ArgAux)))
            elseif instruction.Name == "JUMPX" then
                if BreaksScope((instruction.ArgD :: Number).Value) then -- Explicit cast; TODO: add error checking 
                    return ({
                        Type = "BreakNode"
                    })
                elseif ContinuesScope((instruction.ArgD :: Number).Value) then
                    return ({
                        Type = "ContinueNode"
                    })
                end
                -- uh oh
            elseif instruction.Name == "GETIMPORT" then
                return (Define(instruction.ArgA :: Local, DecompileValue(instruction.Special)))
            elseif instruction.Name == "NOP" or instruction.Name == "JUMPBACK" then
                -- Silence the UNHANDLED print for NOP and JUMPBACK as they don't need a handler
            else
                print(fmt("UNHANDLED: %s", instruction.Name))
            end
            return
        end

        while idx < #closure.Value + 1 do
            local instruction: Instruction = closure.Value[idx]
            local _node: Node? = DecompileInstruction(instruction)
            if _node then
                table.insert(node.Body, _node)
            end
            idx += 1
        end

        return node
    end

    local function ResolveType(_type: number): string
        local optional = bit32.band(_type, bit32.lshift(1, 7))
        local types = {
            [0] = "nil",
            [1] = "boolean",
            [2] = "number",
            [3] = "string",
            [4] = "{}",
            [5] = "__wdec_type_function",
            [6] = "__wdec_type_thread",
            [7] = "__wdec_type_userdata",
            [8] = "Vector3",
            
            [15] = "any",
            [256] = "__wdec_type_invalid"
        }
        return types[bit32.band(_type, 0b1111111)] .. optional
    end

    local cid = 0
    local pid = 0
    local function TranspileClosure(closure: ClosureNode, depth: number): string
        local transpiled_closure_src: string = ""

        local function Indent(depth): string
            return string.rep("    ", depth)
        end

        local function TranspileValue(value: ValueNode, depth: number, parent_expr: ExpressionNode?): string
            if value.Type == "BooleanNode" then
                return value.Value and "true" or "false"
            elseif value.Type == "ClosureNode" then -- If a closure is used as a value, it does NOT have a name
                local header: string = "function("
                for i, v in next, value.Params do
                    header ..= fmt("p%d", pid)
                    if #value.ParamTypes > 2 then
                        header ..= ": " .. ResolveType(value.ParamTypes[i + 2])
                    end
                    if i < #value.Params then
                        header ..= ", "
                    end
                    v.Id = fmt("p%d", pid)
                    pid += 1
                end
                if value.IsVarArg then
                    if #value.Params > 0 then
                        header ..= ", "
                    end
                    header ..= "..."
                end
                header ..= ")\n"
                return header .. TranspileClosure(value, depth + 1) .. Indent(depth) .. "end"
            elseif value.Type == "ExpressionNode" then
                local operator_precedence = { -- TODO: Find out if these actually are correct (I think they are)
                    ["or"]   = 0,
                    ["and"]  = 1,
                    ["=="]   = 2,
                    ["~="]   = 2,
                    ["<"]    = 2,
                    [">"]    = 2,
                    ["<="]   = 2,
                    [">="]   = 2,
                    ["+"]    = 3,
                    ["-"]    = 3,
                    ["*"]    = 4,
                    ["/"]    = 4,
                    ["//"]   = 4,
                    ["%"]    = 4,
                    ["^"]    = 5,
                }
                local format: string = ""
                if parent_expr and operator_precedence[value.Op] < operator_precedence[parent_expr.Op] then
                    format ..= "(%s"
                else
                    format ..= "%s"
                end
                format ..= " %s "
                if parent_expr and operator_precedence[value.Op] < operator_precedence[parent_expr.Op] then
                    format ..= "%s)"
                else
                    format ..= "%s"
                end
                return fmt(format, TranspileValue(value.Lhs, depth, value), value.Op, TranspileValue(value.Rhs, depth, value))
            elseif value.Type == "Local" then
                return value.Id
            elseif value.Type == "NilNode" then
                return "nil"
            elseif value.Type == "NumberNode" then
                return tostring(value.Value)
            elseif value.Type == "StringNode" then -- Raw strings are not supported.
                local s, _ = string.gsub(fmt("\"%s\"", value.Value), "\n", "\\n")
                local newstring, _ = string.gsub(s, "\t", "\\t")
                return newstring
            elseif value.Type == "GlobalNode" then
                return value.Value
            elseif value.Type == "UnaryNode" then
                return fmt("%s%s", value.Op, TranspileValue(value.Value, depth))
            elseif value.Type == "VarArgsNode" then
                return "..."
            elseif value.Type == "TableIndexNode" then
                if value.Index.Type == "StringNode" and not string.find(value.Index.Value, " ") then
                    return fmt("%s.%s", TranspileValue(value.Table, depth), value.Index.Value)
                end
                return fmt("%s[%s]", TranspileValue(value.Table, depth), TranspileValue(value.Index, depth))
            elseif value.Type == "StringsNode" then
                local cons: string = ""
                for i, _value in next, value.Value do
                    cons ..= TranspileValue(_value, depth)
                    if i < #value.Value then
                        cons ..= " .. "
                    end
                end
                return cons
            elseif value.Type == "TableNode" then -- TODO: Format tables nicer
                local cons: string = ""
                local values: { string } = {}
                local total_length: number = 0
                for i, _value in next, value.Body do
                    table.insert(values, TranspileValue(_value, depth))
                    total_length += string.len(values[#values])
                end
                for i, _value in next, value.Body do
                    cons ..= TranspileValue(_value, depth)
                    if i < #value.Body then
                        cons ..= fmt(",%s", total_length <= 120 and " " or "\n" .. Indent(depth + 1))
                    end
                end
                return fmt("{%s%s%s}", total_length <= 120 and "" or "\n" .. Indent(depth + 1), cons, total_length <= 120 and "" or "\n" .. Indent(depth))
            elseif value.Type == "InlineNamecallNode" then
                local cons: string = TranspileValue(value.Object, depth) .. ":" .. value.Target.Value .. "("
                for i, arg in next, value.Args do
                    cons ..= TranspileValue(arg, depth)
                    if i < #value.Args then
                        cons ..= ", "
                    end
                end
                return cons .. ")"
            elseif value.Type == "InlineCallNode" then
                local cons: string = TranspileValue(value.Target, depth) .. "("
                for i, arg in next, value.Args do
                    cons ..= TranspileValue(arg, depth)
                    if i < #value.Args then
                        cons ..= ", "
                    end
                end
                return cons .. ")"
            end
            return fmt("nil --[[ Unhandled type: %s ]]", value.Type)
        end

        local function TranspileNode(node: Node, depth: number): string
            if node.Type == "VarAssignNode" then
                local id = fmt("v%d", cid)

                if node.Value.Type == "TableIndexNode" then
                    local final: Node = node.Value
                    while final.Type == "TableIndexNode" do
                        final = node.Value.Index
                    end
                    if final.Type == "StringNode" and not final.Value:find(" ") then
                        id ..= "_" .. final.Value
                    end
                end
                
                node.Target.Id = id
                cid += 1
                if node.Value.Type == "ClosureNode" and node.Value.Name ~= "" then -- Special case for functions
                    node.Target.Id = node.Value.Name
                    local cons: string = Indent(depth) .. fmt("local function %s(", node.Value.Name)
                    for i, v in next, node.Value.Params do
                        cons ..= fmt("p%d", pid)
                        if #node.Value.ParamTypes > 2 then
                            cons ..= ": " .. ResolveType(node.Value.ParamTypes[i + 2])
                        end
                        if i < #node.Value.Params then
                            cons ..= ", "
                        end
                        v.Id = fmt("p%d", pid)
                        pid += 1
                    end
                    if node.Value.IsVarArg then
                        if #node.Value.Params > 0 then
                            cons ..= ", "
                        end
                        cons ..= "..."
                    end
                    cons ..= ")\n"
                    return cons .. TranspileClosure(node.Value, depth + 1) .. Indent(depth) .. "end"
                end
                return Indent(depth) .. fmt("local %s = %s", id, TranspileValue(node.Value, depth))
            elseif node.Type == "VarReassignNode" then
                if node.Value.Type == "ClosureNode" and node.Value.Name ~= "" then -- Special case for functions
                    node.Target.Id = node.Value.Name
                    local cons: string = Indent(depth) .. fmt("local function %s(", node.Value.Name)
                    for i, v in next, node.Value.Params do
                        cons ..= fmt("p%d", pid)
                        if #node.Value.ParamTypes > 2 then
                            cons ..= ": " .. ResolveType(node.Value.ParamTypes[i + 2])
                        end
                        if i < #node.Value.Params then
                            cons ..= ", "
                        end
                        v.Id = fmt("p%d", pid)
                        pid += 1
                    end
                    if node.Value.IsVarArg then
                        if #node.Value.Params > 0 then
                            cons ..= ", "
                        end
                        cons ..= "..."
                    end
                    cons ..= ")\n"
                    return cons .. TranspileClosure(node.Value, depth + 1) .. Indent(depth) .. "end"
                end
                return Indent(depth) .. fmt("%s = %s", node.Target.Id, TranspileValue(node.Value, depth))
            elseif node.Type == "FunctionCallNode" then
                local cons: string = ""
                local target = TranspileValue(node.Target, depth)
                if #node.RetVals > 0 then
                    cons ..= "local "
                    for i, val in next, node.RetVals do
                        if target == "require" and #node.RetVals == 1 then
                            -- here we want to get the name of the module, which could be something like script.Parent.SomeModule or script.Parent:WaitForChild("SomeModule")
                            local module: string = ""
                            if #node.Args == 1 and node.Args[1].Type == "InlineNamecallNode" then
                                if (((node.Args[1] :: InlineNamecallNode).Target).Value == "FindFirstChild" or ((node.Args[1] :: InlineNamecallNode).Target).Value == "FindFirstChildOfClass" or ((node.Args[1] :: InlineNamecallNode).Target).Value == "WaitForChild" or ((node.Args[1] :: InlineNamecallNode).Target).Value == "GetService") and (node.Args[1] :: InlineNamecallNode).Args[1].Type == "StringNode" and not ((node.Args[1] :: InlineNamecallNode).Args[1] :: StringNode).Value:find(" ") then
                                    module = "_" .. ((node.Args[1] :: InlineNamecallNode).Args[1] :: StringNode).Value
                                end
                            elseif #node.Args == 1 and node.Args[1].Type == "TableIndexNode" then
                                if (node.Args[1] :: TableIndexNode).Index.Type == "StringNode" and not ((node.Args[1] :: TableIndexNode).Index :: StringNode).Value:find(" ") then
                                    module = "_" .. ((node.Args[1] :: TableIndexNode).Index :: StringNode).Value    
                                end
                            end
                            cons ..= fmt("v%d_module%s", cid, module)
                            val.Id = fmt("v%d_module%s", cid, module)
                        elseif target == "pairs" or target == "ipairs" then
                            local id = fmt("v%d_", cid)
                            if i == 1 then
                                id ..= "generator"
                            elseif i == 2 then
                                id ..= "state"
                            elseif i == 3 then
                                id ..= "index"
                            end
                            cons ..= id
                            val.Id = id
                        elseif not string.match(target, "%W") then -- isalnum()
                            cons ..= fmt("v%d_%s_ret%d", cid, target, i)
                            val.Id = fmt("v%d_%s_ret%d", cid, target, i)
                        else
                            cons ..= fmt("v%d", cid)
                            val.Id = fmt("v%d", cid)
                        end
                        cid += 1
                        if i < #node.RetVals then
                            cons ..= ", "
                        end
                    end
                    cons ..= " = "
                end
                cons ..= target .. "("
                for i, arg in next, node.Args do
                    cons ..= TranspileValue(arg, depth)
                    if i < #node.Args then
                        cons ..= ", "
                    end
                end
                cons ..= ")"
                return Indent(depth) .. cons
            elseif node.Type == "NamecallNode" then
                local cons: string = ""
                if #node.RetVals > 0 then
                    cons ..= "local "
                    if #node.RetVals == 1 and (node.Target.Value == "FindFirstChild" or node.Target.Value == "FindFirstChildOfClass" or node.Target.Value == "WaitForChild" or node.Target.Value == "GetService") and node.Args[1].Type == "StringNode" and not (node.Args[1] :: StringNode).Value:find(" ") then
                        cons ..= fmt("v%d", cid) .. "_" .. (node.Args[1] :: StringNode).Value
                        node.RetVals[1].Id = fmt("v%d", cid) .. "_" .. (node.Args[1] :: StringNode).Value
                        cid += 1
                    else
                        for i, val in next, node.RetVals do
                            cons ..= fmt("v%d_%s_ret%d", cid, node.Target.Value, i)
                            val.Id = fmt("v%d_%s_ret%d", cid, node.Target.Value, i)
                            cid += 1
                            if i < #node.RetVals then
                                cons ..= ", "
                            end
                        end
                    end
                    cons ..= " = "
                end
                cons ..= TranspileValue(node.Object, depth) .. ":" .. node.Target.Value .. "("
                for i, arg in next, node.Args do
                    cons ..= TranspileValue(arg, depth)
                    if i < #node.Args then
                        cons ..= ", "
                    end
                end
                cons ..= ")"
                return Indent(depth) .. cons
            elseif node.Type == "ReturnNode" then
                local cons: string = "return"
                if #node.Values > 0 then
                    cons ..= " "
                    for i, value in next, node.Values do
                        cons ..= TranspileValue(value, depth)
                        if i < #node.Values then
                            cons ..= ", "
                        end
                    end
                end
                return Indent(depth) .. cons
            elseif node.Type == "TableAssignNode" then
                if node.Index.Type == "StringNode" and not string.find(node.Index.Value, " ") then
                    return Indent(depth) .. fmt("%s.%s = %s", TranspileValue(node.Table, depth), node.Index.Value, TranspileValue(node.Source, depth))
                end
                return Indent(depth) .. fmt("%s[%s] = %s", TranspileValue(node.Table, depth), TranspileValue(node.Index, depth), TranspileValue(node.Source, depth))
            elseif node.Type == "IfNode" then
                while #node.Body == 1 and node.Body[1].Type == "IfNode" do
                    node.Condition = {
                        Type = "ExpressionNode",
                        Lhs = node.Condition,
                        Rhs = (node.Body[1] :: IfNode).Condition,
                        Op = "and"
                    }
                    node.Body = (node.Body[1] :: IfNode).Body
                end
                local cons: string = Indent(depth) .. fmt("if %s then\n", TranspileValue(node.Condition, depth))
                for i, _node in next, node.Body do
                    cons ..= TranspileNode(_node, depth + 1)
                    if i < #node.Body then
                        cons ..= "\n"
                    end
                end
                local function Resolvebranches(_node: IfNode)
                    if #_node.ElseBody == 1 and _node.ElseBody[1].Type == "IfNode" then
                        cons ..= "\n" .. Indent(depth) .. fmt("elseif %s then\n", TranspileValue((_node.ElseBody[1] :: IfNode).Condition, depth))
                        for i, __node in next, (_node.ElseBody[1] :: IfNode).Body do
                            cons ..= TranspileNode(__node, depth + 1)
                            if i < #_node.Body then
                                cons ..= "\n"
                            end
                        end
                        Resolvebranches(_node.ElseBody[1] :: IfNode)
                        table.remove(_node.ElseBody, 1)
                    end
                end
                Resolvebranches(node)
                if #node.ElseBody > 0 then
                    cons ..= "\n" .. Indent(depth) .. "else\n"
                    for i, _node in next, node.ElseBody do
                        cons ..= TranspileNode(_node, depth + 1)
                        if i < #node.ElseBody then
                            cons ..= "\n"
                        end
                    end
                end
                return cons .. "\n" .. Indent(depth) .. "end"
            elseif node.Type == "WhileNode" then
                local cons: string = Indent(depth) .. fmt("while %s do\n", TranspileValue(node.Condition, depth))
                for i, _node in next, node.Body do
                    cons ..= TranspileNode(_node, depth + 1)
                    if i < #node.Body then
                        cons ..= "\n"
                    end
                end
                return cons .. "\n" .. Indent(depth) .. "end"
            elseif node.Type == "ForRangeNode" then
                if node.Iterator.Id == "" then
                    node.Iterator.Id = fmt("v%d", cid)
                    cid += 1
                end
                local cons: string = Indent(depth) .. fmt("for %s = %s, %s, %s do\n", node.Iterator.Id, TranspileValue(node.Index, depth), TranspileValue(node.Limit, depth), TranspileValue(node.Step, depth))
                for i, _node in next, node.Body do
                    cons ..= TranspileNode(_node, depth + 1)
                    if i < #node.Body then
                        cons ..= "\n"
                    end
                end
                return cons .. "\n" .. Indent(depth) .. "end"
            elseif node.Type == "ForValueNode" then
                local vars: string = ""
                for i, var in next, node.Variables do
                    var.Id = fmt("v%d", cid)
                    cid += 1
                    vars ..= var.Id
                    if i < #node.Variables then
                        vars ..= ", "
                    end
                end
                local cons: string = ""
                -- lol silly workaround
                if node.Index.Type ~= "NilNode" then
                    cons = Indent(depth) .. fmt("for %s in %s, %s, %s do\n", vars, TranspileValue(node.Generator, depth), TranspileValue(node.State, depth), TranspileValue(node.Index, depth))
                else
                    cons = Indent(depth) .. fmt("for %s in %s, %s do\n", vars, TranspileValue(node.Generator, depth), TranspileValue(node.State, depth))
                end
                for i, _node in next, node.Body do
                    cons ..= TranspileNode(_node, depth + 1)
                    if i < #node.Body then
                        cons ..= "\n"
                    end
                end
                return cons .. "\n" .. Indent(depth) .. "end"
            elseif node.Type == "GlobalDefinitionNode" then
                if node.Value.Type == "ClosureNode" then
                    local cons: string = Indent(depth) .. fmt("function %s(", node.Value.Name)
                    for i, v in next, node.Value.Params do
                        cons ..= fmt("v%d", cid)
                        if i < #node.Value.Params then
                            cons ..= ", "
                        end
                        v.Id = fmt("v%d", cid)
                        cid += 1
                    end
                    if node.Value.IsVarArg then 
                        if #node.Value.Params > 0 then
                            cons ..= ", "
                        end
                        cons ..= "..."
                    end
                    cons ..= ")\n"
                    return Indent(depth) .. cons .. TranspileClosure(node.Value, depth + 1) .. Indent(depth) .. "end"
                end
                return Indent(depth) .. fmt("%s = %s", node.Target, TranspileValue(node.Value, depth))
            end
            return Indent(depth) .. fmt("-- UNSUPPORTED: %s", node.Type)
        end

        for i, node in next, closure.Body do
            if i == #closure.Body and node.Type == "ReturnNode" and #node.Values == 0 then
                break -- remove trailing return if it is not needed
            end
            transpiled_closure_src ..= fmt("%s\n", TranspileNode(node, depth))
        end
        return transpiled_closure_src
    end

    local main_proto = bytecode.protos[bytecode.main_proto_id + 1]
    local main_proto_instrs: { Instruction } = RefcountProto(main_proto, {})
    -- create a dummy closure for main proto
    local main_closure: Closure = {
        Type = "Closure",
        Value = main_proto_instrs,
        Params = {},
        IsVarArg = false,
        Name = "main",
        UpValues = {},
        IsGlobal = false
    }

    local ast: ClosureNode = DecompileClosure(main_closure)
    local source: string = TranspileClosure(ast, 0)

    return string.sub(source, 1, string.len(source) - 1)
end


getgenv().decompile = function(script: LocalScript | ModuleScript)
    local bytes: string = getscriptbytecode(script)
    local bytecode: luau_bytecode = deserialize(bytes, true)
    return "-- Decompiled with SirHurt // https://sirhurt.net/?ref=woffle // written by @luavm, et al.\n\n" .. wdec_decompile(bytecode)
end