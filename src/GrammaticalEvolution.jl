module GrammaticalEvolution

using EBNF
import Base.sort!
import Base.getindex
import Base.length
import Base.endof
import Base.isless
import Base.push!

export Individual, Population
export select_two_individuals, one_point_crossover, mutate!, generate
export length, getindex, endof, setindex!, isless

abstract Individual
abstract Population

length{T <: Population}(pop::T) = length(pop.individuals)
getindex{T <: Population}(pop::T, index::Int64) = pop.individuals[index]
push!{T <: Population, S <: Individual}(pop::T, ind::S) = push!(pop.individuals, ind)

length{T <: Individual}(ind::T) = length(ind.genome)
endof{T <: Individual}(ind::T) = endof(ind.genome)
getindex{T <: Individual}(ind::T, indices...) = ind.genome[indices...]
setindex!{T <: Individual}(ind::T, value::Int64, indices) = ind.genome[indices] = value
isless{T <: Individual}(ind1::T, ind2::T) = ind1.fitness < ind2.fitness


function evaluate!{PopulationType <: Population}(pop::PopulationType)
  for i=1:size
    evalulate!(pop[i])
  end
end

#function isless(x::Individual, y::Individual)
#  return x
#end

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

function generate{PopulationType <: Population}(population::PopulationType, top_percent::Float64, prob_mutation::Float64, mutation_rate::Float64)
    # sort population
    sort!(population)

    # take top %
    top_num::Int64 = floor(length(population)*top_percent)
    top_performers = population.individuals[1:top_num]

    # create a new population
    genome_size = length(population[1])
    new_population = PopulationType(top_num, genome_size)

    # re-populate by mating top performers
    while length(new_population.individuals) < length(population)
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

  return new_population
end

# function run{PopulationType <: Population}(population::PopulationType, generations::Int64, top_percent::Float64, prob_mutation::Float64, mutation_rate::Float64)
#   evaluate!(population)
#   for i=1:generations
#     new_population = generate(population, top_percent, prob_mutation, mutation_rate)

#     # return new population
#     produce(new_population)
#   end
# end

end # module
