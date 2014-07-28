using GrammaticalEvolution
using Base.Test

type TestIndividual <: Individual
  genome::Array{Int64, 1}
  fitness::Float64

  function TestIndividual(size::Int64, max_value::Int64)
    genome = rand(1:max_value, size)
    return new(genome, -1.0)
  end

  TestIndividual(genome::Array{Int64, 1}) = new(genome, -1.0)
end

type TestPopulation <: Population
  individuals::Array{TestIndividual, 1}

  function TestPopulation(population_size::Int64, genome_size::Int64)
    individuals = Array(TestIndividual, 0)
    for i=1:population_size
      push!(individuals, TestIndividual(genome_size, 1000))
    end

    return new(individuals)
  end
end

function GrammaticalEvolution.evaluate!(ind::TestIndividual)
  ind.fitness = rand()
end

# test creationg of population
pop = TestPopulation(50, 10)
@test length(pop) == 50
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

# test mutation
for mutation_rate = 0:0.1:1.0
  genome = zeros(Int64, 100000)
  ind = TestIndividual(genome)
  ind_mutated = TestIndividual(copy(genome))
  mutate!(ind_mutated, mutation_rate)

  count = 0
  for i=1:length(ind)
    count += ind[i] == ind_mutated[i] ? 0 : 1
  end

  diff = abs(count/length(genome) - mutation_rate)
  @test diff < 0.01
end

# test generate
new_population = generate(pop, 0.1, 0.2, 0.2)
@test length(new_population) == length(pop)

@grammar testgrammar1 begin
  start = a | b
  a = "a"
  b = "b"
end

pop = TestPopulation(50, 10)
result = transform(testgrammar1, pop[1])
@test result == "a" || result == "b"

@grammar testgrammar2 begin
  start = a + b
  a = "a"
  b = "b"
end

pop = TestPopulation(50, 10)
result = transform(testgrammar2, pop[1])
@test result == "ab"

@grammar testgrammar3 begin
  start = a | b
  a = "a" + "c"
  b = "b" + "d"
end

pop = TestPopulation(50, 10)
result = transform(testgrammar3, pop[1])
@test result == "ac" || result == "bd"

