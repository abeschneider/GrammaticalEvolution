module GrammaticalEvolution

using EBNF

export Individual, Population
export select_two_individuals, one_point_crossover, mutate!

abstract Individual
abstract Population

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

function select_two_individuals{IndividualType <: Individual}(individuals::Array{IndividualType})
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
      ind[i] = rand(1, max_value)
    end
  end
end

function generate{PopulationType <: Population}(population::PopulationType, top_percent::Float64, prob_mutation::Float64, mutation_rate::Float64)
    # sort population
    sort!(population)

    # take top %
    top_num = length(population.individuals)*top_percent
    top_performers = population.individuals[1:top_num]

    # create a new population
    new_population = PopulationType(top_num)

    # re-populate by mating top performers
    while length(new_population.individuals) < size
      (i1, i2) = select_two_individuals(pop)
      (ind2, ind2) = one_point_crossover(top_performers)
      push!(new_population, ind1)
      push!(new_population, ind2)
    end

    # mutate
    for j=1:size
      if rand() < prob_mutation
        mutate!(new_population.individuals[top_num:end], mutation_rate)
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
