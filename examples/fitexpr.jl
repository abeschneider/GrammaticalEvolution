using GrammaticalEvolution
import GrammaticalEvolution.evaluate!

type ExprIndividual <: Individual
  genome::Array{Int64, 1}
  fitness::Float64
  code

  function ExprIndividual(size::Int64, max_value::Int64)
    genome = rand(1:max_value, size)
    return new(genome, -1.0, nothing)
  end

  ExprIndividual(genome::Array{Int64, 1}) = new(genome, -1.0, nothing)
end

type ExprPopulation <: Population
  individuals::Array{ExprIndividual, 1}

  function ExprPopulation(population_size::Int64, genome_size::Int64)
    individuals = Array(ExprIndividual, 0)
    for i=1:population_size
      push!(individuals, ExprIndividual(genome_size, 1000))
    end

    return new(individuals)
  end
end

convert_number(lst) = float(join(lst))
plus{T1 <: Number, T2 <: Number}(x::T1, y::T2) = x+y
mult{T1 <: Number, T2 <: Number}(x::T1, y::T2) = x*y

@grammar expr1 begin
  start = ex
  ex = number | sum | product | (ex) | value
  sum = Expr(:call, :+, ex, ex)
  product = Expr(:call, :*, ex, ex)
  value = :x | :y | number
  number[convert_number] = digit + '.' + digit
  digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
end

# ground truth we're matching against
gt(x, y) = 2*x + 5*y

function evaluate!(ind::ExprIndividual)
  fitness::Array{Float64, 1} = {}

  try
    ind.code = transform(expr1, ind)
    @eval fn(x, y) = $(ind.code)
  catch
    ind.fitness = Inf
    return
  end

  for x=0:10
    for y=0:10
      value = fn(x, y)

#       println("value = $value, $(gt(x, y))")
      if !isnan(value)
        insert!(fitness, length(fitness)+1, sqrt((value - gt(x, y)).^2))
      end
    end
  end

  ind.fitness = mean(fitness)
end

# run the experiment
pop = ExprPopulation(500, 1000)
for i=1:10000
  # generate a new population (based off of fitness)
  pop = generate(pop, 0.1, 0.2, 0.2)

  # can safely assume population is sorted
  println("generation: $i, max fitness=$(pop[1].fitness), code=$(pop[1].code)")
end