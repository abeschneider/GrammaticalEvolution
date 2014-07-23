using GrammaticalEvolution
using Base.Test
import Base.getindex
import Base.length
import Base.endof

type TestIndividual <: Individual
  genome::Array{Int64, 1}
  fitness::Float64

  function TestIndividual(size::Int64, max_value::Int64)
    genome = rand(1:max_value, size)
    return new(genome, -1.0)
  end

  TestIndividual(genome::Array{Int64, 1}) = new(genome, -1.0)
end

type TestPopulation{IndividualType <: Individual} <: Population
  individuals::Array{IndividualType, 1}

  function TestPopulation(population_size::Int64, genome_size::Int64)
    individuals = Array(IndividualType, 0)
    for i=1:population_size
      push!(individuals, IndividualType(genome_size, 1000))
    end

    return new(individuals)
  end
end

length(pop::TestPopulation) = length(pop.individuals)
getindex(pop::TestPopulation, index::Int64) = pop.individuals[index]

length(ind::TestIndividual) = length(ind.genome)
endof(ind::TestIndividual) = endof(ind.genome)
getindex(ind::TestIndividual, indices...) = ind.genome[indices...]
isless(ind1::TestIndividual, ind2::TestIndividual) = ind1.fitness < ind2.fitness

# test creationg of population
pop = TestPopulation{TestIndividual}(5, 10)
@test length(pop) == 5
@test length(pop[1]) == 10

# test selecting two individuals
for i=1:20
  (i1, i2) = select_two_individuals(pop.individuals)
  @test i1 != i2
  @test i1 <= length(pop)
  @test i2 <= length(pop)
end

# test crossing over two genomes
(i1, i2) = select_two_individuals(pop.individuals)
for i=1:20
  (g1, g2, cross_point) = one_point_crossover(pop[i1], pop[i2])
  @test g1[1:cross_point-1] == pop[i1].genome[1:cross_point-1]
  @test g1[cross_point:end] == pop[i2].genome[cross_point:end]
  @test g2[1:cross_point-1] == pop[i2].genome[1:cross_point-1]
  @test g2[cross_point:end] == pop[i1].genome[cross_point:end]
end
