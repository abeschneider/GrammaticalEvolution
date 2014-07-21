module GrammaticalEvolution

using EBNF

abstract Individual
abstract Population

function evaluate!{PopulationType <: Population}(pop::PopulationType)
  for i=1:size
    evalulate!(pop[i])
  end
end

function run{PopulationType <: Population}(size::Int64, generations::Int64)
  pop = PopulationType(size)

  evaluate!(pop)
  for i=1:generations
    # create new population for this generation
    gen = PopulationType(size)

    # sort population
    sort!(gen)

    # take top %

    # replicate top with 2-point cross-over

    # mutate


    # return new population

  end
end

end # module
