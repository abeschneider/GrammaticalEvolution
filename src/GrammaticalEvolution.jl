module GrammaticalEvolution

import Base.sort!
import Base.getindex
import Base.length
import Base.endof
import Base.isless
import Base.push!
import Base.pop!

export Individual, Population
export select_two_individuals, one_point_crossover, mutate!, evaluate!, generate, transform
export length, getindex, endof, setindex!, isless, genome_iterator

include("EBNF.jl")

export Grammar, @grammar, Rule, AndRule, OrRule, parseGrammar

type MaxWrapException <: Exception end

abstract Individual
abstract Population

# methods that have to be supported by subclasses of population
length{T <: Population}(pop::T) = length(pop.individuals)
getindex{T <: Population}(pop::T, indices...) = pop.individuals[indices...]
push!{T <: Population, S <: Individual}(pop::T, ind::S) = push!(pop.individuals, ind)
pop!{T <: Population}(pop::T) = pop!(pop.individuals)

# methods that have to be supported by subclasses of subpopulation
length{T <: Individual}(ind::T) = length(ind.genome)
endof{T <: Individual}(ind::T) = endof(ind.genome)
getindex{T <: Individual}(ind::T, indices...) = ind.genome[indices...]
setindex!{T <: Individual}(ind::T, value::Int64, indices) = ind.genome[indices] = value
isless{T <: Individual}(ind1::T, ind2::T) = ind1.fitness < ind2.fitness
evaluate(ind::Individual) = nothing

# TODO: this should be distributed
function evaluate!{PopulationType <: Population}(grammar::Grammar, pop::PopulationType)
  for i=1:length(pop)
    evaluate!(grammar, pop[i])
  end
end

function sort!{PopulationType <: Population}(pop::PopulationType)
  sort!(pop.individuals)
end

function select_two_individuals{T <: Individual}(individuals::Array{T,1})
  # randomly select two individuals from a population
  while true
    i1 = rand(1:length(individuals))
    i2 = rand(1:length(individuals))

    # make sure the two individuals are not the same
    if i1 != i2
      return (i1, i2)
    end
  end
end

function one_point_crossover{IndividualType <: Individual}(ind1::IndividualType, ind2::IndividualType)
  cross_point = rand(1:length(ind1))
  g1 = vcat(ind1[1:cross_point-1], ind2[cross_point:end])
  g2 = vcat(ind2[1:cross_point-1], ind1[cross_point:end])

  return (IndividualType(g1), IndividualType(g2), cross_point)
end

function mutate!(ind::Individual, mutation_rate::Float64; max_value=1000)
  for i=1:length(ind)
    if rand() < mutation_rate
      ind[i] = rand(1:max_value)
    end
  end
end

function generate{PopulationType <: Population}(grammar::Grammar, population::PopulationType, top_percent::Float64, prob_mutation::Float64, mutation_rate::Float64)
  # sort population
  sort!(population)

  # take top %
  top_num::Int64 = floor(length(population)*top_percent)
  top_performers = population[1:top_num]

  # create a new population
  genome_size = length(population[1])
  new_population = PopulationType(top_num, genome_size)

  # re-populate by mating top performers
  while length(new_population) < length(population)
    (i1, i2) = select_two_individuals(top_performers)
    (ind1, ind2) = one_point_crossover(top_performers[i1], top_performers[i2])
    push!(new_population, ind1)
    push!(new_population, ind2)
  end

  # mutate
  for j=(top_num+1):length(population)
    if rand() < prob_mutation
      mutate!(new_population[j], mutation_rate)
    end
  end

  # TODO: don't re-evaluate individuals that have already been evaluated
  # evaluate new population and re-sort
  evaluate!(grammar, new_population)
  sort!(new_population)

  # it's possible that we might have added too many individuals, so trim down if necessary
  while length(new_population) > length(population)
    pop!(new_population)
  end

  return new_population
end

# stateful iterator that keeps track of its current position, wraps the position
# when the maximum length is reached, and emits an exception when the maximum
# number of wraps occurs
function genome_iterator(size::Int64, maxwraps::Int64)
  i::Int64 = 1
  wraps::Int64 = 0

  function next()
    while true
      produce(i)

      i += 1
      if i > size
        wraps += 1
        i = 1
      end

      if wraps > maxwraps
        throw(MaxWrapException())
      end
    end
  end

  return Task(next)
end

function transform(grammar::Grammar, ind::Individual; maxwraps=2)
  pos = genome_iterator(length(ind), maxwraps)
  value = transform(grammar, grammar.rules[:start], ind, pos)
  return value
end

function transform(grammar::Grammar, rule::OrRule, ind::Individual, pos::Task)
  idx = (ind[consume(pos)] % length(rule.values))+1
  value = transform(grammar, rule.values[idx], ind, pos)

  if rule.action !== nothing
    value = rule.action(values)
  end

  return value
end

function transform(grammar::Grammar, rule::ReferencedRule, ind::Individual, pos::Task)
  return transform(grammar, grammar.rules[rule.symbol], ind, pos)
end

function transform(grammar::Grammar, rule::Terminal, ind::Individual, pos::Task)
  return rule.value
end

function transform(grammar::Grammar, rule::AndRule, ind::Individual, pos::Task)
  values = [transform(grammar, subrule, ind, pos) for subrule in rule.values]

  if rule.action !== nothing
    values = rule.action(values)
  end

  return values
end

function transform(grammar::Grammar, sym::Symbol, ind::Individual, pos::Task)
  return sym
end

function transform(grammar::Grammar, q::QuoteNode, ind::Individual, pos::Task)
  return q.value
end

function transform(grammar::Grammar, rule::ExprRule, ind::Individual, pos::Task)
  args = [transform(grammar, arg, ind, pos) for arg in rule.args]
  return Expr(args...)
end

function transform(grammar::Grammar, rule::ZeroOrMoreRule, ind::Individual, pos::Task)
  # genome value gives number of time to repeat
  reps = ind[consume(pos)]

  # invoke given rule reps times
  values = [transform(grammar, rule.rule, ind, pos) for i=1:reps]

  if rule.action !== nothing
    values = rule.action(values)
  end

  return values
end

function transform(grammar::Grammar, rule::OneOrMoreRule, ind::Individual, pos::Task)
  # genome value gives number of time to repeat
  reps = ind[consume(pos)]

  # enforce that it's at least one
  if reps == 0
    reps = 1
  end

  # invoke given rule reps times
  values = [transform(grammar, rule.rule, ind, pos) for i=1:reps]

  if rule.action !== nothing
    values = rule.action(values)
  end

  return values
end

end # module
