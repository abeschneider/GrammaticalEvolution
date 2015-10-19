import Base.show
import Base.convert
import Base.getindex

abstract Rule

typealias ActionType Union{Function, Void}

type Grammar
  rules::Dict{Symbol, Rule}
end

immutable Terminal <: Rule
  name::AbstractString
  value

  function Terminal(name::AbstractString, value)
    return new(name, value);
  end

  Terminal(value) = new("", value)
end

immutable ReferencedRule <: Rule
  name::AbstractString
  symbol::Symbol
  action::ActionType

  function ReferencedRule(name::AbstractString, symbol::Symbol, action)
    return new(name, symbol, action)
  end

  function ReferencedRule(name::AbstractString, symbol::Symbol)
    return new(name, symbol, nothing)
  end
end

immutable AndRule <: Rule
  name::AbstractString
  values::Array{Rule}
  action::ActionType

  function AndRule(name::AbstractString, values::Array{Rule}, action)
    return new(name, values, action)
  end
end

immutable OrRule <: Rule
  name::AbstractString
  values::Array{Rule, 1}
  action::ActionType

  function OrRule{T <: Rule}(name::AbstractString, rules::Array{T}, action::ActionType)
    return new(name, rules, action)
  end

  function OrRule(name::AbstractString, left::OrRule, right::OrRule, action::ActionType)
    return new(name, vcat(left.values, right.values), action)
  end

  function OrRule(name::AbstractString, left::Rule, right::Rule, action::ActionType)
    return new(name, [left, right], action)
  end

  function OrRule(name::AbstractString, left::OrRule, right::Rule, action::ActionType)
    return new(name, vcat(left.values, right), action)
  end

  function OrRule(name::AbstractString, left::Rule, right::OrRule, action::ActionType)
    return new(name, vcat(right.values, left), action)
  end

  function OrRule(name::AbstractString, left::OrRule, right::Terminal, action::ActionType)
    return new(name, vcat(left.values, right.value), action)
  end

  function OrRule(name::AbstractString, left::Terminal, right::OrRule, action::ActionType)
    return new(name, vcat(right.values, left.value), action)
  end

  function OrRule{T1, T2}(name::AbstractString, left::T1, right::T2, action::ActionType)
    return new(name, [left, right], action)
  end
end

immutable OneOrMoreRule <: Rule
  name::AbstractString
  value::Rule
  action::ActionType
end

immutable ZeroOrMoreRule <: Rule
  name::AbstractString
  value::Rule
  action::ActionType
end

immutable RangeRule <: Rule
  name::AbstractString
  range::UnitRange{Int64}
  action::ActionType

  function RangeRule(name::AbstractString, range::UnitRange{Int64}, action::ActionType)
    return new(name, range, action)
  end
end

immutable RepeatedRule <: Rule
  name::AbstractString
  range::UnitRange{Int64}
  value::Rule
  action::ActionType

  function RepeatedRule(name::AbstractString, range::RangeRule, value::Rule, action::ActionType)
    return new(name, range.range, value, action)
  end
end

immutable RegexRule <: Rule
  name::AbstractString
  value::Regex
  action::ActionType

  function RegexRule(name::AbstractString, value::Regex; action=nothing)
    return new(name, value, action)
  end
end

immutable OptionalRule <: Rule
  name::AbstractString
  value::Rule
  action::ActionType

  function OptionalRule(name::AbstractString, value::Rule)
    return new(name, value)
  end

  function OptionalRule(value::Rule)
    return new("", value)
  end
end

immutable ListRule <: Rule
  name::AbstractString
  entry::Rule
  delim::Rule
  action::ActionType

  function ListRule(name::AbstractString, entry::Rule, delim::Rule; action=nothing)
    return new(name, entry, delim, action)
  end
end

immutable FunctionRule <: Rule
  name::AbstractString
  fn::Symbol
  args::Array{Rule}

  function FunctionRule(name::AbstractString, fn::Symbol, args::Array{Rule})
    return new(name, fn, args)
  end
end

immutable ExprRule <: Rule
  name::AbstractString
  args::Array{Any, 1}

  function ExprRule(name::AbstractString, args::Array{Any, 1})
    return new(name, args)
  end
end

function show(io::IO, t::Terminal)
  print(io, "$(t.value)");
end

function show(io::IO, rule::AndRule)
  values = [string(r) for r in rule.values];
  joinedValues = join(values, " ");
  action = rule.action === nothing ? "" : "[$(rule.action)] "
  print(io, "($action$joinedValues)");
end

function show(io::IO, rule::OrRule)
  values = [string(r) for r in rule.values];
  joinedValues = join(values, "|");
  print(io, "($joinedValues)");
end

function show(io::IO, rule::OneOrMoreRule)
  print(io, "+($(rule.value))");
end

function show(io::IO, rule::ZeroOrMoreRule)
  print(io, "*($(rule.value))");
end

function show(io::IO, rule::RegexRule)
  print(io, "r($(rule.value.pattern))")
end

function convert{T}(::Type{Rule}, n::T)
  return Terminal(n);
end

function convert{T<:Rule}(::Type{Rule}, n::T)
  return n;
end

function convert{T}(::Type{Rule}, n::UnitRange{T})
  terminals = [Terminal(i) for i=(n.start):(n.stop)];
  return OrRule(terminals);
end

function parseDefinition(name::AbstractString, value::AbstractString, action::ActionType)
  return Terminal(name, value);
end

function parseDefinition(name::AbstractString, value::Char, action::ActionType)
  return Terminal(name, value);
end

function parseDefinition{T <: Number}(name::AbstractString, value::T, action::ActionType)
  return Terminal(name, value);
end

function parseDefinition(name::AbstractString, symbol::Symbol, action::ActionType)
  return ReferencedRule(name, symbol)
end

function parseDefinition(name::AbstractString, var::QuoteNode, action::ActionType)
  return var.value
end

function parseDefinition(name::AbstractString, regex::Regex, action::ActionType)
  # TODO: Need to do this to ensure we always match at the beginning,
  # but there should be a safer way to do this
  modRegex = Regex("^$(regex.pattern)")
  return RegexRule(name, modRegex)
end

type EmptyRule <: Rule
end

function parseDefinition(name::AbstractString, ex::Expr, action::ActionType)
  # if it's a macro (e.g. r"regex") then we want to expand it first
  if ex.head === :macrocall
    return parseDefinition(name, eval(ex))
  end

  if ex.args[1] === :|
    left = parseDefinition("$name.1", ex.args[2], nothing)
    right = parseDefinition("$name.2", ex.args[3], nothing)
    return OrRule(name, left, right, action)
  elseif ex.args[1] === :+
    # check if this is infix or prefix
    if length(ex.args) > 2
      # Addition can contain multiple entries
      values::Array{Rule} = [parseDefinition("$name.$i", arg, nothing) for (i, arg) in enumerate(ex.args[2:end])]
      return AndRule(name, values, action)
    else
      # it's prefix, so it maps to one or more rule
      return OneOrMoreRule(name, parseDefinition("$name.1", ex.args[2], nothing), action)
    end
  elseif ex.args[1] === :* && length(ex.args) == 2
    # it's a prefix, so it maps to zero or more rule
    return ZeroOrMoreRule(name, parseDefinition("$name.1", ex.args[2], nothing), action)
  elseif ex.args[1] == :^
    rule = ex.args[2]
    range = ex.args[3]
    return RepeatedRule(name, parseDefinition("$name.range", range, nothing), parseDefinition("$name.1", rule, nothing), action)
  elseif ex.args[1] == :?
    return OptionalRule(parseDefinition(name, ex.args[2], nothing), action)
  elseif ex.args[1] == :list
    entry = parseDefinition("$name.entry", ex.args[2], nothing)
    delim = parseDefinition("$name.delim", ex.args[3], nothing)
    return ListRule(name, entry, delim, action)
  elseif typeof(ex.args[1]) === Symbol
    args = [parseDefinition("$name.$i", arg, nothing) for (i, arg) in enumerate(ex.args[2:end])]
    return ExprRule(name, args)
  elseif ex.head == :(:)
    # a UnitRange
#     return OrRule(name, [Terminal(value) for value in (ex.args[1]):(ex.args[2])], nothing)
    return RangeRule(name, (ex.args[1]):(ex.args[2]), nothing)
  end

  println("?? $(ex.args)")
  return EmptyRule()
end

macro grammar(grammar_name::Symbol, ex::Expr)
  code = []
  push!(code, :(rules = Dict()))
  for definition in ex.args[2:2:end]
    if typeof(definition.args[1]) === Expr && definition.args[1].head === :ref
      name = Expr(:call, :string, Expr(:quote, definition.args[1].args[1]))
      ex = Expr(:quote, definition.args[2])
      action = Expr(:escape, definition.args[1].args[2])

      # rules[name] = parseDefinition(name, ex, action)
      push!(code, Expr(:(=), Expr(:ref, :rules, name),
                       Expr(:call, :parseDefinition, name, ex, action)))
    else
      name = Expr(:call, :string, Expr(:quote, definition.args[1]))
      ex = Expr(:quote, definition.args[2])

      # rules[name] = parseDefinition(name, ex, nothing)
      push!(code, Expr(:(=), Expr(:ref, :rules, name),
                       Expr(:call, :parseDefinition, name, ex, nothing)))
    end
  end

  # grammar_name = Grammar(rules)
  push!(code, :($(esc(grammar_name)) = Grammar(rules)))

  # group all code into a single block
  return Expr(:block, code...)
end

function *(rule::Rule) end
function ?(rule::Rule) end
function list(entry::Rule, delim::Rule) end

#end
