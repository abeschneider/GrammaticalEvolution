type ExampleIndividual <: Individual
  genome::Array{Int64, 1}
  fitness::Float64
  code

  function ExampleIndividual(size::Int64, max_value::Int64)
    genome = rand(1:max_value, size)
    return new(genome, -1.0, nothing)
  end

  ExampleIndividual(genome::Array{Int64, 1}) = new(genome, -1.0, nothing)
end

type ExamplePopulation <: Population
  individuals::Array{ExampleIndividual, 1}

  function ExamplePopulation(population_size::Int64, genome_size::Int64)
    individuals = Array(ExampleIndividual, 0)
    for i=1:population_size
      push!(individuals, ExampleIndividual(genome_size, 1000))
    end

    return new(individuals)
  end

  function ExamplePopulation(individuals::Array{ExampleIndividual, 1})
    return new(copy(individuals))    
  end
end
