push!(LOAD_PATH, "../src")

using GrammaticalEvolution
import GrammaticalEvolution.evaluate!

include("ExamplePopulation.jl")

# ground truth we're matching against
gt(x, y) = 2*x + 5*y

# action for creating
convert_number(lst) = float(join(lst))

function create_grammar()
  @grammar grammar begin
    start = ex
    ex = number | sum | product | (ex) | value
    sum = Expr(:call, :+, ex, ex)
    product = Expr(:call, :*, ex, ex)
    value = :x | :y | number
    number[convert_number] = digit + '.' + digit
    digit = 0:9
  end

  return grammar
end

function GrammaticalEvolution.evaluate!(grammar::Grammar, ind::ExampleIndividual)
  fitness::Array{Float64, 1} = []

  try
    ind.code = transform(grammar, ind)
    @eval fn(x, y) = $(ind.code)
  catch e
    # println("exception = $e")
    ind.fitness = Inf
    return
  end

  for x=0:10
    for y=0:10
      value = fn(x, y)
      diff = (value - gt(x, y)).^2
      if !isnan(diff) && diff > 0
        insert!(fitness, length(fitness)+1, sqrt(diff))
      elseif diff == 0
        insert!(fitness, length(fitness)+1, 0)
      end
    end
  end

  ind.fitness = mean(fitness)
end

function main()
  # our grammar
  grammar = create_grammar()

  # create population
  pop = ExamplePopulation(500, 100)

  fitness = Inf
  generation = 1
  while fitness > 1.0
    # generate a new population (based off of fitness)
    pop = generate(grammar, pop, 0.1, 0.2, 0.2)

    # population is sorted, so first entry it the best
    fitness = pop[1].fitness
    println("generation: $generation, max fitness=$fitness, code=$(pop[1].code)")
    generation += 1
  end
end

main()