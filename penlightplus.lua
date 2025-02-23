--% Kale Ewasiuk (kalekje@gmail.com)
--% +REVDATE+
--% Copyright (C) 2021-2025 Kale Ewasiuk
--%
--% Permission is hereby granted, free of charge, to any person obtaining a copy
--% of this software and associated documentation files (the "Software"), to deal
--% in the Software without restriction, including without limitation the rights
--% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--% copies of the Software, and to permit persons to whom the Software is
--% furnished to do so, subjected to the following conditions:
--%
--% The above copyright notice and this permission notice shall be included in
--% all copies or substantial portions of the Software.
--%
--% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
--% ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
--% TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--% PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT
--% SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
--% ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
--% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
--% OR OTHER DEALINGS IN THE SOFTWARE.



-- https://lunarmodules.github.io/Penlight/

__PL_PLUS__ = true
__PL_SKIP_TEX__ = __PL_SKIP_TEX__ or false --if declared true before here, it will use regular print functions
--                                       (for troubleshooting with texlua instead of actual use in lua latex)
__PL_GLOBALS__ = __PL_GLOBALS__ or false
__PL_NO_HYPERREF__ = __PL_NO_HYPERREF__ or false

penlight.luakeys = require'luakeys'()

penlight.debug_available = false -- check if penlight debug package is available
if debug ~= nil then
  if type(debug.getinfo) == 'function' then
   penlight.debug_available = true
  end
end




-- http://lua-users.org/wiki/SplitJoin -- todo read me!!

penlight.tex = {} -- adding a sub-module for tex related stuff

local bind = bind or penlight.func.bind


function penlight.hasval(x)  -- if something has value
    if (type(x) == 'function') or (type(x) == 'CFunction') or (type(x) == 'userdata') then
        return true
    elseif (x == nil) or (x == false) or (x == 0) or (x == '') or (x == {}) then
        return false
    elseif (type(x) ~= 'boolean') and (type(x) ~= 'number') and (type(x) ~= 'string') then  -- something else? maybe ths not needed
        if #x == 0 then -- one more check, probably no needed though, I was trying to cover other classes but they all tables
            return false
        else
            return true
        end
    end
    return true
end



function penlight._Gdot(s)
    -- return a global with nots
    o = _G
    for _, a in ipairs(s:split('.')) do
        o = o[a]
    end
    return o
end


-- Some simple and helpful LaTeX functions -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- xparse defaults

penlight.tex.xTrue = '\\BooleanTrue '
penlight.tex.xFalse = '\\BooleanFalse '
penlight.tex.xNoValue = '-NoValue-'

penlight.tex._xTrue = '\\BooleanTrue '
penlight.tex._xFalse = '\\BooleanFalse '
penlight.tex._xNoValue = '-NoValue-'


--Generic LuaLaTeX utilities for print commands or environments

if not __PL_SKIP_TEX__ then
    local function check_special_chars(s) -- todo extend to toher special chars?
        if type(s) == 'string' then
            if string.find(s, '[\n\r\t\0]') then
                penlight.tex.pkgwarn('penlight', 'printing string with special (eg. newline) char, possible unexpected behaviour on string: '..s)
            end
        end
    end

    -- NOTE: usage is a bit different than default. If number is first arg, you CANT change catcode.
    --              We don't need that under normal use, use tex.print or tex.sprint if you need
    function penlight.tex.prt(s, ...) -- print something, no new line after
        check_special_chars(s)
        if type(s) == 'number' then s = tostring(s) end
        tex.sprint(s, ...)     --can print lists as well, but will NOT put new line between them or anything printed
    end

    function penlight.tex.prtn(s, ...) -- print with new line after, can print lists or nums. C-function not in Lua, apparantly
        s = s or ''
        check_special_chars(s)
        if type(s) == 'number' then s = tostring(s) end
        tex.print(s, ...)
    end

    penlight.tex.wrt = texio.write
    penlight.tex.wrtn = texio.write_nl
else
    penlight.tex.prt = io.write
    penlight.tex.prtn = print     --print with new line
    penlight.tex.wrt = io.write
    penlight.tex.wrtn = io.write_nl
end

function penlight.tex.prtl(str) -- prints a literal/lines string in latex, adds new line between them
    for line in str:gmatch"[^\n]*" do  -- gets all characters up to a new line
        penlight.tex.prtn(line)
    end
end

-- todo option to specify between character? one for first table, on for recursives?
function penlight.tex.prtt(tab, d1, d2) -- prints a table with new line between each item
    d1 = d1 or ''
    d2 = d2 or '\\leavevmode\\\\'
    for _, t in pairs(tab) do  --
        if type(t) ~= 'table' then
            if d1 == '' then
                penlight.tex.prtn(t)
            else
                penlight.tex.prt(t, d1)
            end
         else
            penlight.tex.prtn(d2)
            penlight.tex.prtt(t,d1,d2)
        end
    end
end

function penlight.tex.wrth(s1, s2) -- helpful printing, makes it easy to debug, s1 is object, s2 is note
    local wrt2 = wrt or texio.write_nl or print
    s2 = s2 or ''
    wrt2('\nvvvvv '..s2..'\n')
    if type(s1) == 'table' then
        wrt2(penlight.pretty.write(s1))
    else
        wrt2(tostring(s1))
    end
    wrt2('\n^^^^^\n')
end
penlight.wrth = penlight.tex.wrth
penlight.tex.help_wrt = penlight.tex.wrth
penlight.help_wrt = penlight.tex.wrth

function penlight.tex.prt_array2d(t)
    for _, r in ipairs(t) do
        local s = ''
        for _, v in ipairs(r) do
            s = s.. tostring(v)..', '
        end
        penlight.tex.prt(s)
        penlight.tex.prt('\n')
    end
end

-- -- -- -- --

function penlight.tex.pkgwarn(pkg, msg1, msg2)
    pkg = pkg or ''
    msg1 = msg1 or ''
    msg2 = msg2 or ''
    tex.sprint('\\PackageWarning{'..pkg..'}{'..msg1..'}{'..msg2..'}')
end

function penlight.tex.pkgerror(pkg, msg1, msg2, stop)
    pkg = pkg or ''
    msg1 = msg1 or ''
    msg2 = msg2 or ''
    stop = penlight.hasval(stop)
    tex.sprint('\\PackageError{'..pkg..'}{'..msg1..'}{'..msg2..'}')
    if stop then tex.sprint('\\stop') end -- stop on the spot (say that 10 times)
end


if not penlight.debug_available then
    penlight.tex.pkgwarn('penlightplus', 'lua debug library is not available, recommend re-compiling with the --luadebug option')
end


function penlight.tex.errorif(exp, pkg, msg1, msg2, stop)
    if penlight.hasval(exp) then
        penlight.tex.pkgerror(pkg, msg1, msg2, stop)
    end
end


--definition helpers -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function penlight.tex.defmacro(cs, val, g) -- , will not work if val contains undefined tokens (so pre-define them if using..)
    val = val or ''          -- however this works for arbitrary command names (\@hello-123 etc allowed)
    g = g or 'global'
    token.set_macro(cs, val, g)
end


function penlight.tex.defcmd(cs, val) -- fixes above issue, but only chars allowed in cs (and no @)
    val = val or ''
    tex.sprint('\\gdef\\'..cs..'{'..val..'}')
end

function penlight.tex.defcmdAT(cs, val) -- allows @ in cs,
    --however should only be used in preamble. I avoid \makeatother because I've ran into issues with cls and sty files using it.
    val = val or ''
    tex.sprint('\\makeatletter\\gdef\\'..cs..'{'..val..'}')
end



function penlight.tex.prvcmd(cs, val) -- provide command via lua
   if token.is_defined(cs) then
       -- do nothing if token is defined already --pkgwarn('penlight', 'Definition '..cs..' is being overwritten')
    else
        penlight.tex.defcmd(cs, val)
    end
end

function penlight.tex.newcmd(cs, val) -- provide command via lua
   if token.is_defined(cs) then
       penlight.tex.pkgerror('penlight: newcmd',cs..' already defined')
    else
        penlight.tex.defcmd(cs, val)
    end
end

function penlight.tex.renewcmd(cs, val) -- provide command via lua
   if token.is_defined(cs) then
        penlight.tex.defcmd(cs, val)
    else
        penlight.tex.pkgerror('penlight: renewcmd',cs..' not defined')
    end
end

function penlight.tex.deccmd(cs, def, overwrite) -- declare a definition, placeholder throws an error if it used but not set!
    overwrite = penlight.hasval(overwrite)
    local decfun
    if overwrite then decfun = penlight.tex.defcmd else decfun = penlight.tex.newcmd end
    if def == nil then
        decfun(cs, pkgerror('penlight', cs..' was declared and used in document, but never set'))
    else
        decfun(cs, def)
    end
end


--
-- -- todo add and improve this, options for args?
--local function defcmd_nest(cs) -- for option if you'd like your commands under  a parent ex. \csparent{var}
--    tex.print('\\gdef\\'..cs..'#1{\\csname '..var..'--#1--\\endcsname}')
--end
--
--
--local function defcmd(cs, val, nargs)
--    if (nargs == nil) or (args == 0) then
--        token.set_macro(cs, tostring(val), 'global')
--    else
--        local args = '#1'
--        tex.print('\\gdef\\'..cs..args..'{'..val..'}')
--        -- todo https://tex.stackexchange.com/questions/57551/create-a-capitalized-macro-token-using-csname
--        --    \expandafter\gdef\csname Two\endcsname#1#2{1:#1, two:#2} --todo do it like this
--    end
--end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



-- when nesting commands, this makes it helpful to not worry about brackets
penlight.tex._NumBkts = 0
penlight.tex._EndEnvs = {}

--prt(opencmd('textbf')..opencmd('texttt')..'bold typwriter'..close_bkt_cnt())

function penlight.tex.opencmd(cmd)
    return '\\'..cmd..add_bkt_cnt()
end

function penlight.tex.reset_bkt_cnt(n)
     n = n or 0
    _NumBkts = n
end

function penlight.tex.add_bkt_cnt(n)
    -- add open bracket n times, returns brackets
     n = n or 1
    _NumBkts = _NumBkts + n
    return ('{'):rep(n)
end

function penlight.tex.close_bkt_cnt(n)
    n = n or _NumBkts
    local s = ('}'):rep(n)
    _NumBkts = _NumBkts - n
    return s
end





function penlight.tex.openenv(env, opt)
  if opt == nil then opt = '' else opt = '['..opt..']' end
  tex.sprint('\\begin{' .. env .. '}'..opt)
  table.insert(penlight.tex._EndEnvs, 1, '\\end{'..env..'}')
end

function penlight.tex.closeenv(num)
  num = num or #penlight.tex._EndEnvs
  for i=1, num do
    tex.sprint(penlight.tex._EndEnvs[1])
    table.remove(penlight.tex._EndEnvs, 1)
  end
end



function penlight.tex.aliasluastring(s, d)
    s = s:delspace():upper():tolist()
    d = d:delspace():upper():tolist()
    for i, S in penlight.seq.enum(d:slice_assign(1,#s,s)) do
        if (S == 'F') then S = '' end  -- F is fully expanded
        penlight.tex.prtn('\\let\\plluastring'..penlight.Char(i)..'\\luastring'..S)
    end
end





function penlight.tex.get_ref_info(l)
    local n = 5
    if __PL_NO_HYPERREF__ then
        local n = 2
    end
    local r = token.get_macro('r@'..l)
    local t = {}
    if r == nil then
        t = penlight.tablex.new(n, 0)  -- make all 0s
        r = '-not found-'
    else
        t = {r:match(("(%b{})"):rep(n))}
        t = penlight.tablex.map(string.trimfl, t)
    end
    t[#t+1] = r -- add the og return of label
    --penlight.help_wrt(t, 'ref info')
    return t
end

-- todo add regex pattern for cref info
--function penlight.tex.get_ref_info_all_cref(l)
--    local r = token.get_macro('r@'..l..'@cref')
--    if r == nil then
--        return r, 0, 0
--    end
--    local sec, page =  r:match("{([^}]*)}{([^}]*)}")
--    return r, sec, page
--end


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --






-- -- -- -- math stuff
function math.mod(a, b) -- math modulo, return remainder only
    return a - (math.floor(a/b)*b)
end

function math.mod2(a) -- math modulo 2
    return math.mod(a,2)
end



-- -- -- -- string stuff
local lpeg = require"lpeg"
local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V

local number = P{"number",
    number = (V"int" * V"frac"^-1 * V"exp"^-1) / tonumber,
    int = V"sign"^-1 * (R"19" * V"digits" + V"digit"),
    digits = V"digit" * V"digits" + V"digit",
    digit = R"09",
    sign = S"+-",
    frac = P"." * V"digits",
    exp = S"eE" * V"sign"^-1 * V"digits",
    }




function penlight.char(num)
  return string.char(string.byte("a")+tonumber(num)-1)
end

function penlight.Char(num)
  return string.char(string.byte("A")+tonumber(num)-1)
end


local str_mt = getmetatable("") -- register functions with str

function str_mt.__index.gnum(s)
    return number:match(s)
end

function str_mt.__index.gextract(s, pat) --extract a pattern from string, returns both
    local s_extr = ''
    local s_rem = s
    for e in s:gmatch(pat) do
        s_extr = s_extr..e
        s_rem = s_rem:gsub(e,'')
    end
    return s_extr, s_rem
end

function str_mt.__index.gxtrct(s, pat, num, join) --extract a pattern from string, returns both
    -- todo a variant where you can specify the number of extractions, and either list of concatenate them would be helpful
    local l_extr = penlight.List{}
    local s_rem = s
    local n = 1
    num = num or 99999
    for e in s:gmatch(pat) do
        l_extr = l_extr:append(e)
        s_rem = s_rem:gsub(e,'',1)
        if n == num then break end
        n = n +1
    end
    if join then
        l_extr = l_extr:join(join)
    end
    return l_extr, s_rem
end


function str_mt.__index.gfirst(s, t) -- get the first pattern found from a table of pattern
    for _, pat in pairs(t) do
        if string.find(s, pat) then
            return pat
        end
    end
end

function str_mt.__index.appif(S, W, B, O) --append W ord to S tring if B oolean true, otherwise O ther
    --append Word to String
    if B then --if b is true
        S = S .. W
    else --consider Other word
        O = O or ''
        S = S .. O
    end
    return S
end


 function str_mt.__index.containsany(s, exp)
    if type(exp) ~= 'table' then exp = {exp} end
    for _, e in ipairs(exp) do
        if s:find(e) then return true end
    end
    return false
end

function str_mt.__index.containsanycase(s, exp)
    if type(exp) ~= 'table' then exp = {exp} end
    for _, e in ipairs(exp) do
        if s:lower():find(e:lower()) then return true end
    end
    return false
end

function str_mt.__index.totable(str)
    local t = {}
    for i = 1, #str do
        t[i] = str:sub(i, i)
    end
    return t
end

function str_mt.__index.tolist(str)
    return penlight.List(str)
end


function str_mt.__index.upfirst(str)
    return str:gsub('%a', function(x) return x:upper()  end, 1)
end

function str_mt.__index.delspace(str)
    return str:gsub('%s','')
end

function str_mt.__index.trimfl(str)
    return str:sub(2,-2)
end

function str_mt.__index.istexdim(str)
    for _, u in pairs{'pt', 'mm', 'cm', 'in', 'ex', 'em', 'mu', 'sp'} do
        if penlight.hasval(str:delspace():find('^%-?%d*%.?%d+'..u..'$')) then
            return true
        end
    end
    return false
end



function str_mt.__index.subpar(s, r)
    r = r or ' '
    return (s:gsub('\\par', r))
end


function str_mt.__index.fmt(s, t, fmt) -- format a $1 and $k string with an array or table and formats
    -- formats can be a luakeys string, or a table and are applied to table before string is formatted
    if type(t) ~= 'table' then t = {t} end
    t = penlight.tablex.strinds(t)
    t = penlight.tablex.fmt(t, fmt, true)
    return s % t
end

function str_mt.__index.parsekv(s, t) -- parsekv string
    if type(t) ~= 'table' then t = penlight.luakeys.parse(t) end
    return penlight.luakeys.parse(s, t)
end

function str_mt.__index.splitstrip(s, sep, stri) --
    sep = sep or ','
    return penlight.List(s:split(sep)):map(function(x) return penlight.stringx.strip(x, stri) end)
end


function str_mt.__index.split2(s, sep1, sep2, stri) --
    sep1 = sep1 or ','
    sep2 = sep2 or '='
    if stri == nil then stri = true end
    stri = penlight.hasval(strip)
    local splitfunc = string.split
    if stri then
        splitfunc = string.splitstrip
    end
    return penlight.List(splitfunc(s,sep1)):map(function(x) return splitfunc(x, sep2) end)
end




function str_mt.__index.hasnonum(s)
    -- string only contains letters and symbols
    --assert_string(1,s) -- todo
    return string.find(s,'^[%D]+$') == 1
end

function str_mt.__index.hasnoalpha(s)
    -- string only contains numbers and symbols
    --assert_string(1,s) -- todo
    return string.find(s,'^[%A]+$') == 1
end

function str_mt.__index.isvarlike(s)
    -- string is like a variable, does not start with a number, then followd by letter, number, or underscore
    --assert_string(1,s) -- todo
    return string.find(s,'^[%a_][%w%d_]*$') == 1
end




-- -- -- -- function stuff

function penlight.clone_function(fn)
  local dumped = string.dump(fn)
  local cloned = loadstring(dumped)
  local i = 1
  while true do
    local name = debug.getupvalue(fn, i)
    if not name then
      break
    end
    debug.upvaluejoin(cloned, i, fn, i)
    i = i + 1
  end
  return cloned
end











-- -- -- -- -- -- -- -- -- -- -- --  functions below extend the seq module


function penlight.seq.check_neg_index(i, len, fallback)
    i = tostring(i):delspace()
    if i == '' then return fallback end
    i = tonumber(i)
    if i == nil then
      local _ = 1*'"Attempted to use seqstr indexing with negative number, but length of list not provided"'
      return fallback -- fallback is the number to fall back on if i isn't provided
    end
    len = tonumber(len)
    if i < 0 then
        if len == nil then
          local _ = 1*'"Attempted to use seqstr indexing with negative number, but length of list not provided"'
          return penlight.utils.raise("Attempted to use seqstr indexing with negative number, but length of list not provided")
        end
      i = len + 1 + i -- negative index
    end
    return i
end

penlight.seq.train_element_sep = ','
penlight.seq.train_range_sep = ':'


function penlight.seq.train(s, len)
    -- parse a range given a string indexer
    -- syntax is: s = 'i1, i2, r1:r2'  where i1 and i2 are individual indexes.
    -- r1:r2 is a range (inclusive).
    -- a 'stride' can be given to ranges, eg. ::2 is 1,3,5,..., or 2::3 is 2,5,8,...
    -- negative numbers can be used to index relative to the length of the table, eg, -1 -> len
    -- if length is not given, negative indeing cannot be used
    -- returns a penlight list of numbers

    local t = penlight.List() -- list of indexes
    local check_neg = penlight.seq.check_neg_index
    local steps = s:split(penlight.seq.train_element_sep)
    --penlight.wrth(steps,'abc')
    for _, r in ipairs(steps) do
        --penlight.wrth(r,'seq.train = '..s)
        r = penlight.stringx.strip(r)
      if r == '*' then -- if the string has no numbers and no :, it is a key
          t:append(r)
      elseif string.isvarlike(r) then -- if the string has no numbers and no :, it is a key
        t:append(r)
      elseif r:find(penlight.seq.train_range_sep) then
        r = r:split(penlight.seq.train_range_sep) -- if it's a range
        t:extend(penlight.List.range(check_neg(r[1], len, 1),
                               check_neg(r[2], len, len),
                               tonumber(r[3])))
      else
        t:append(check_neg(r, len))
      end
  end
return t
end

function penlight.seq.itrain(s, len)
-- iterator version of sequence-string
    local t = penlight.seq.train(s, len)
    local i = 0
  return function ()
      i = i + 1
      if i <= #t then return t[i] end
  end
end



function penlight.seq.tbltrain(tbl, s) -- iterate over a table using the train syntax
    local inds = penlight.seq.train(s, #tbl) -- indexes to use
    local star = inds:index('*')
    if star ~= nil then
        inds:pop(star)
        inds:inject(penlight.tablex.kkeys(tbl), star)
    end
    local i = 0
    return function ()
        i = i + 1  -- i of indexes
        if i <= #inds then
          local v = tbl[inds[i]]
          --penlight.wrth(v)
          --if v == nil then penlight.test.asserteq(v, true) end -- todo make a generic lua error message function
          return  inds[i], v
      end
    end
end





function penlight.seq.prod(t1, t2)
    -- cartesian prduct of two tables (uses ipairs)
    local t_new = {}
    for _, v1 in ipairs(t1) do
        for _, v2 in ipairs(t2) do
            t_new[#t_new + 1] = {v1, v2}
        end
    end
    local i = 0
  return function ()
      i = i + 1
      if i <= #t_new then return t_new[i][1], t_new[i][2] end
  end
end







-- -- -- -- -- -- -- -- -- -- -- --  functions below extend the operator module

function penlight.operator.strgt(a,b) return tostring(a) > tostring(b) end
function penlight.operator.strlt(a,b) return tostring(a) < tostring(b) end



-- -- -- --  functions below are helpers for arrays and 2d

local function compare_elements(a, b, op, ele)
    op = op or penlight.oper.gt
    ele = ele or 1
    return op(a[ele], b[ele])
end

local function comp_2ele_func(op, ele) -- make a 2 element comparison function,
    --sort with function on element nnum
    return bind(compare_elements, _1, _2, op, ele)
end



-- table stuff below


function penlight.tablex.concatenate(t1,t2)
    -- todo is this needed
    for i=1,#t2 do
        t1[#t1+1] = t2[i]
    end
    return t1
end


function penlight.tablex.strinds(t) -- convert indices that are numbers to string indices
    local t_new = {}
    for i, v in pairs(t) do -- ensure all indexes are strings
      if type(i) == 'number' then
          t_new[tostring(i)] = v
      else
          t_new[i] = v
      end
    end
  return t_new
end



function penlight.tablex.listcontains(t, v)
    return penlight.tablex.find(t, v) ~= nil
end


-- format contents of a table
function penlight.tablex.fmt(t, fmt, strinds)
    if fmt == nil then
        return t
    end
    strinds = strinds or false -- if your fmt table should use string indexes
    if type(fmt) == 'string' then
        if not fmt:find('=') then -- if no = assume format all same
            for k, v in pairs(t) do -- apply same format to all
                if tonumber(v) ~= nil then -- only apply to numeric values
                    t[k] = string.format("%"..fmt, v)
                end
            end
            return t
        else
            fmt = fmt:parsekv('naked_as_value') -- make fmt a table from keyval str
        end
    end
    if strinds then fmt = penlight.tablex.strinds(fmt) end -- convert int inds to str inds
    for k, f in pairs(fmt) do -- apply formatting to table
        t[k] = string.format("%"..f, tostring(t[k]))
    end
    return t
end


function penlight.tablex.list2comma(t)
    t = penlight.List(t)
    local s = ''
    if #t == 1 then
        s = t[1]
    elseif #t == 2 then
        s = t:join(' and ')
    elseif #t >= 3 then
        s = t:slice(1,#t-1):join(', ')..', and '..t[#t]
    end
    return s
end


function penlight.tablex.map_slice(func, T, j1, j2)
    if type(j1) == 'string' then
        return penlight.array2d.map_slice(func, {T}, ','..j1)[1]
    else
        return penlight.array2d.map_slice(func, {T}, 1, j1, 1, j2)[1]
    end
end

penlight.array2d.map_slice1 = penlight.tablex.map_slice



function penlight.tablex.kkeys(t)
    local keys = {}
    for k, _ in penlight.utils.kpairs(t) do
        keys[#keys+1] = k
    end
    return keys
end




-- todo option for multiple filters with AND logic, like the filter files??
function penlight.tablex.filterstr(t, exp, case)
    -- case = case sensitive
    case = penlight.hasval(case)
    -- apply lua patterns to a table to filter iter
    -- str or table of str's can be passed, OR logic is used if table is passed
    if case then
        return penlight.tablex.filter(t, bind(string.containsany,_1,exp))
    else
        return penlight.tablex.filter(t, bind(string.containsanycase,_1,exp))
    end
end


function penlight.tablex.train(t,seq,reind)
    local t_new = {}
    local num = 0
    for k, v in penlight.seq.tbltrain(t, seq) do
        if reind and type(v) == 'number' then
            num = num + 1
            k = num
        end
        t_new[k] = v
    end
    return t_new
end



function penlight.trysplitcomma(s)
    strip = strip or false
    if type(s) == 'number' then s = tostring(s) end
    if type(s) == 'string' then
        return s:splitstrip(',')
    end
    return s
end


function penlight.findfiles(kv)
    if type(kv) == 'string' then kv = penlight.luakeys.parse(kv) end
    kv = penlight.tablex.update({dir={'.'}, fn={'*'}, ext={''}, sub=false}, kv)
    kv.dir = penlight.trysplitcomma(kv.dir)
    kv.fn = penlight.trysplitcomma(kv.fn)
    kv.ext = penlight.trysplitcomma(kv.ext)
    --local files_all = penlight.getallfilesdirs(kv.dir, kv.sub)
    local getfiles = penlight.dir.getfiles
    if penlight.hasval(kv.sub) then
        getfiles = function(dir, fn) return penlight.dir.getallfiles(dir, '*'..fn)  end -- need * in front so folder does not affect result
    end
    local files = penlight.List{}
    for fn, ext in penlight.seq.prod(kv.fn, kv.ext) do
        for _, dir in ipairs(kv.dir) do
            files:extend(getfiles(dir, fn..ext))
        end
    end
    files = pl.List(penlight.tablex.keys(pl.Set(files))) -- clear duplicates
    files = files:map(function(s) return s:gsub('\\','/')  end) -- change slash for latex
    return files
end

penlight.dir.findfiles = penlight.findfiles

--todo add doc
function penlight.utils.filterfiles(...)
    -- f1 is a series of filtering patterns, or condition
    -- f2 is a series of filtering patters, or condition
    -- (f1_a or f2_...) and (f2 .. ) must match
    local args = table.pack(...)
    -- dir, recursive[bool], filt1, filt2 etc...
    -- OR recursive[bool], filt1, filt2, etc..
    -- OR filt1, filt2, filt3, etc..
    -- this could allow one to omit dir
    -- if boolean given ar arg 1, assume dir = '.'
    local nstart = 3
    local r = args[2] -- recursive
    local dir = args[1] -- start dir
    if type(args[1]) == 'boolean' then
        dir = '.'
        r =  args[1]
        nstart = 2
    elseif type(args[2]) ~= 'boolean' then -- if boolean given ar arg 1, assume dir = '.'
        dir = '.'
        r =  false
        nstart = 1
    end

    local files
    if r then  files = penlight.dir.getallfiles(dir)
    else files = penlight.dir.getfiles(dir)
    end
    for i=nstart,args.n do
        files = penlight.tablex.filter(files, penlight.func.compose(bind(string.containsanycase,_1, args[i]), penlight.path.basename))
    end
    return  files
end



-- -- -- -- -- -- -- --  functions below extend the array2d module


function penlight.array2d.map_slice(func, M, i1, j1, i2, j2) -- map a function to a slice of a Matrix
    func = penlight.utils.function_arg(1, func)
    for i,j in penlight.array2d.iter(M, true, i1, j1, i2, j2) do
        M[i][j] = func(M[i][j])
    end
   return M
end

penlight.array2d.map_slice2 = penlight.array2d.map_slice

function penlight.array2d.map_cols(func, M, j1, j2) -- map function to columns of matrix
    if type(j1) == 'string' then
        return penlight.array2d.map_slice(func, M, ','..j1)
    else
        j2 = j2 or -1
        return penlight.array2d.map_slice(func, M, 1, j1, -1, j2)
    end
end

penlight.array2d.map_columns = penlight.array2d.map_cols

function penlight.array2d.map_rows(func, M, i1, i2) -- map function to rows of matrix
    if type(i1) == 'string' then
        return penlight.array2d.map_slice(func, M, i1)
    else
        i2 = i2 or -1
        return penlight.array2d.map_slice(func, M, i1, 1, i2, -1)
    end
end


-- -- -- -- -- -- -- --

function penlight.array2d.sortOP(M, op, ele) -- sort a 2d array based on operator criteria, ele is column, ie sort on which element
       M_new = {}
        for row in penlight.seq.sort(M, comp_2ele_func(op, ele)) do
            M_new[#M_new+1] = row
        end
        return M_new
end

function penlight.array2d.like(M1, v)
    v = v or 0
    r, c = penlight.array2d.size(M1)
    return penlight.array2d.new(r,c,v)
end

function penlight.array2d.from_table(t) -- turns a labelled table to a 2d, label-free array
    t_new = {}
    for k, v in pairs(t) do
        if type(v) == 'table' then
            t_new_row = {k}
            for _, v_ in ipairs(v) do
                 t_new_row[#t_new_row+1] =  v_
            end
            t_new[#t_new+1] = t_new_row
        else
            t_new[#t_new+1] = {k, v}
        end
    end
    return t_new
end

function penlight.array2d.toTeX(M, EL) --puts & between columns, can choose to end line with \\ if EL is true (end-line)
    EL = EL or false
    if EL then EL = '\\\\' else EL = '' end
    return penlight.array2d.reduce2(_1..EL.._2, _1..'&'.._2, M)..EL
end


local function parse_numpy1d(i1, i2, iS)
    i1 = tonumber(i1)
    i2 = tonumber(i2)
    if iS == ':' then
        if i1 == nil then i1 = 1 end
        if i2 == nil then i2 = -1 end
    else
        if i1 == nil then
            i1 = 1
            i2 = -1
        else
            i2 = i1
        end
    end
    return i1, i2
end

function penlight.array2d.parse_numpy2d_str(s)
    s = s:gsub('%s+', '')
    _, _, i1, iS, i2, j1, jS, j2 = string.find(s, "(%-?%d*)(:?)(%-?%d*),?(%-?%d*)(:?)(%-?%d*)")
    i1, i2 = parse_numpy1d(i1, i2, iS)
    j1, j2 = parse_numpy1d(j1, j2, jS)
    return i1, j1, i2, j2
end



if penlight.debug_available then
     penlight.COMP = penlight.comprehension.new() -- for comprehensions
    local _parse_range = penlight.clone_function(penlight.array2d.parse_range)

    function penlight.array2d.parse_range(s) -- edit parse range to do numpy string if no letter passed
        penlight.utils.assert_arg(1,s,'string')
        if not s:find'%a' then
            return penlight.array2d.parse_numpy2d_str(s)
        end
        return _parse_range(s)
    end
end



function penlight.List:inject(l2, pos)
    pos = pos or 1
    if pos < 1 then
        pos = #self + pos + 1
    end
    l2 = penlight.List(l2):reverse()
    for i in l2:iter() do
        self:insert(pos, i)
    end
    return self
end









-- https://tex.stackexchange.com/questions/38150/in-lualatex-how-do-i-pass-the-content-of-an-environment-to-lua-verbatim
penlight.tex.recordedbuf = ""
function penlight.tex.readbuf(buf)
    i,j = string.find(buf, '\\end{%w+}')
     if i==nil then -- if not ending an environment
        penlight.tex.recordedbuf = penlight.tex.recordedbuf .. buf .. "\n"
        return ""
    else
        return nil
    end
end

function penlight.tex.startrecording()
    penlight.tex.recordedbuf = ""
    luatexbase.add_to_callback('process_input_buffer', penlight.tex.readbuf, 'readbuf')
end

function penlight.tex.stoprecording()
    luatexbase.remove_from_callback('process_input_buffer', 'readbuf')
    penlight.tex.recordedbuf = penlight.tex.recordedbuf:gsub("\\end{%w+}\n","")
end



__PDFmetadata__ = {}
penlight.tex.add_xspace_intext = true


function penlight.tex.checkPDFkey(k)
    k = k:delspace():upfirst()
    local keys_allowed = 'Title Author Subject Date Language Keywords Publisher Copyright CopyrightURL Copyrighted Owner CertificateURL Coverage PublicationType Relation Source Doi ISBN URLlink Journaltitle Journalnumber Volume Issue Firstpage Lastpage CoverDisplayDate CoverDate Advisory BaseURL Identifier Nickname Thumbnails '
    if not keys_allowed:find(k ..' ') then
        penlight.tex.pkgerror('penlightplus', 'invalid PDF metadata key assigned "'..k..'"')
    end
    return k
end

function penlight.tex.makePDFtablekv(kv)
    local t_new = {}
    for k, v in pairs(penlight.luakeys.parse(kv)) do
       k = penlight.tex.checkPDFkey(k)
       v = penlight.tex.makePDFvarstr(v)
       t_new[k] = v
    end
    return t_new
end

function penlight.tex.updatePDFtable(k, v, o) -- update pdf table
    if o == nil then o = true end
    k = k:strip():upfirst()
    if penlight.hasval(o) or (__PDFmetadata__[k] == nil) then
        __PDFmetadata__[penlight.tex.checkPDFkey(k)] = penlight.tex.makePDFvarstr(v)
    end
end


penlight.tex.writePDFmetadata = function(t) -- write PDF metadata to xmpdata file
  t = t or __PDFmetadata__
  local str = ''
  for k, v in pairs(t) do
    str = str..'\\'..k..'{'..v..'}'..'\n'
  end
  penlight.utils.writefile(tex.jobname:gsub('"','')..'.xmpdata', str)
end


function penlight.tex.makePDFvarstr(s)
    s = s:gsub('%s*\\sep%s+','\0'):gsub('%s*\\and%s+','\0')  -- turn \and into \sep
    -- todo preserve \%, \{, \}, \backslash, and \copyright
    s = penlight.tex.clear_cmds_str(s)
    s = s:gsub('\0','\\sep ')
    --penlight.tex.help_wrt(s,'PDF var string')
    return s
end

function penlight.tex.clear_cmds_str(s)
    return s:gsub('%s+', ' '):gsub('\\\\',' '):gsub('\\%a+',''):gsub('{',' '):gsub('}',' '):gsub('%s+',' '):strip()
end

function penlight.tex.makeInTextstr(s)
    local s, c_and = s:gsub('%s*\\and%s+','\0')
    s = penlight.tex.clear_cmds_str(s)
    if penlight.tex.add_xspace_intext then
        s = s..'\\xspace'
    end
    if c_and == 1 then
        s = s:gsub('\0',' and ')
    elseif c_and > 1 then
        s = s:gsub('\0',', ', c_and - 1)
        s = s:gsub('\0',', and ')
    end
    --penlight.tex.help_wrt(s,'in text var string')
    return s
end








function penlight.toggle_luaexpr(expr)
    if expr then
      tex.sprint('\\toggletrue{luaexpr}')
    else
      tex.sprint('\\togglefalse{luaexpr}')
    end
end




function penlight.caseswitch(s, c, kv)
    local kvtbl = penlight.luakeys.parse(kv)
    local sw = kvtbl[c] -- the returned switch
    if sw == nil then -- if switch not found
      if s == penlight.tex.xTrue then -- if star, throw error
        penlight.tex.pkgerror('penlight', 'case: "'..c..'" not found in key-vals: "'..kv..'"')
        sw = ''
      else
        sw = kvtbl['__'] or '' -- use __ as not found case
      end
     end
    tex.sprint(sw)
end








-- global setting type stuff

function penlight.make_tex_global()
    for k,v in pairs(penlight.tex) do  -- make tex functions global
            _G[k] = v
        end
end


penlight.kpairs = penlight.utils.kpairs
penlight.npairs = penlight.utils.npairs
penlight.writefile = penlight.utils.writefile
penlight.readfile = penlight.utils.readfile
penlight.readlines = penlight.utils.readfile
penlight.filterfiles = penlight.utils.filterfiles

-- adopt table functions in tablex
penlight.tablex.concat = table.concat
penlight.tablex.insert = table.insert
penlight.tablex.maxn = table.maxn
penlight.tablex.remove = table.remove
penlight.tablex.sort = table.sort

penlight.tbx = penlight.tablex
penlight.a2d = penlight.array2d

if penlight.hasval(__PL_GLOBALS__) then
    -- iterators
    kpairs = penlight.utils.kpairs
    npairs = penlight.utils.npairs

    hasval = penlight.hasval
    COMP = penlight.COMP

    for k,v in pairs(penlight.tablex) do  -- extend the table table to contain tablex functions
        if k == 'sort' then
            table.sortk = v
        elseif k == 'move' then
            table.xmove = v
        else
         _G['table'][k] = v
        end
    end
    table.join = table.concat -- alias

	a2d = penlight.array2d
    tbx = penlight.tablex
end









-- graveyard

--todo decide on above or below



penlight.tex.list2comma = penlight.tablex.list2comma

function penlight.tex.split2comma(s, d)
    local t = penlight.List(s:split(d)):map(string.strip)
    penlight.tex.prt(penlight.tex.list2comma(t))
end

function penlight.tex.split2items(s, d)
    local t = penlight.List(s:split(d)):map(string.strip)
    for n, v in ipairs(t) do
        penlight.tex.prtn('\\item '..v)
    end
end


--
--\subsection*{Splitting strings}
--Splitting text (or a cmd) into oxford comma format via:
--\cmd{\splittocomma[expansion level]{text}{text to split on}}:
--
--\begin{LTXexample}[width=0.3\linewidth]
-- \splittocomma{  j doe  }{\and}-\\
--\splittocomma{  j doe \and s else  }{\and}-\\
--\splittocomma{  j doe \and s else \and a per }{\and}-\\
--\splittocomma{  j doe \and s else \and a per \and f guy}{\and}-
--
--\def\authors{j doe \and s else \and a per \and f guy}
--\splittocomma[o]{\authors}{\and}
--\end{LTXexample}
--
--The expansion level is up to two characters, \cmd{n|o|t|f}, to control the expansion of each argument.
--
--You can do a similar string split but to \cmd{\item} instead of commas with \cmd{\splittoitems}
--\begin{LTXexample}
--\begin{itemize}
--  \splittoitems{kale\and john}{\and}
--  \splittoitems{kale -john -someone else}{-}
--  \splittoitems{1,2,3,4}{,}
--\end{itemize}
--\end{LTXexample}



--
--\NewDocumentCommand{\splittocomma}{ O{nn} m m }{%
--  \MakeluastringCommands[nn]{#1}%
--  \luadirect{penlight.tex.split2comma(\plluastringA{#2},\plluastringB{#3})}%
--}
--
--\NewDocumentCommand{\splittoitems}{ O{NN} m m }{%
--  \MakeluastringCommands[nn]{#1}%
--  \luadirect{penlight.tex.split2items(\plluastringA{#2},\plluastringB{#3})}%
--}
