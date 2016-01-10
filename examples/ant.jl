# using Grammatical Evolution to solve the Ant problem
push!(LOAD_PATH, "../src")

using GrammaticalEvolution
import GrammaticalEvolution.evaluate!
import GrammaticalEvolution.isless
import Base.copy
import Base.+
import Base.-

const food = convert(Int64, '1')
const gap = convert(Int64, '0')
const empty = 46 #convert(Int64, '.')
const startpos = 88 #convert(Int64, 'X')

world1 =
"""
..#..
..#..
.##..
.#...
.X...
"""

function read_map(s)
#     data = [strip(line) for line in split(s)]
    data = [strip(line) for line in s]
    height = length(data)
    width = length(data[1])
    start = ones(Int64, 2)

    map = zeros(Int64, width, height)
    for i=1:height
      for j=1:width
        if data[i][j] != startpos
          map[i, j] = convert(Int64, data[i][j])
        else
          start = [i, j]
        end
      end
    end

    return (map, start)
end

# println(read_map(world1))

type World
  width::Int64
  height::Int64
  value::Array{Int64, 2}
  start::Array{Int64, 1}

  function World(width::Int64, height::Int64)
    value = zeros(width, height)
    return new(width, height, value)
  end

  function World(map::Array{Int64, 2}, start::Array)
    return new(size(map, 1), size(map, 2), map, start)
  end
end

copy(world::World) = World(copy(world.value), world.start)

immutable Direction
  value::Int64
end

+{T <: Number}(d::Direction, v::T) = Direction(d.value + v)
-{T <: Number}(d::Direction, v::T) = Direction(d.value - v)
isless(d1::Direction, d2::Direction) = isless(d1.value, d2.value)
isless(d1::Direction, v::Int64) = isless(d1.value, value)

const north = Direction(1)
const east = Direction(2)
const south = Direction(3)
const west = Direction(4)

type AntIndividual <: Individual
  genome::Array{Int64, 1}
  # fitness::Union{Float64, Void}
  fitness::Float64
  code

  function AntIndividual(size::Int64, max_value::Int64)
    genome = rand(1:max_value, size)
    return new(genome, -1, nothing)
  end

  AntIndividual(genome::Array{Int64, 1}) = new(genome, -1, nothing)
end

type AntPopulation <: Population
  individuals::Array{AntIndividual, 1}

  function AntPopulation(individuals::Array{AntIndividual, 1})
    return new(copy(individuals))
  end

  function AntPopulation(population_size::Int64, genome_size::Int64)
    individuals = Array(AntIndividual, 0)
    for i=1:population_size
      push!(individuals, AntIndividual(genome_size, 1000))
    end

    return new(individuals)
  end
end

type Ant
  x::Int64
  y::Int64
  direction::Direction
  eaten::Int64

  function Ant(x::Int64, y::Int64)
    return new(x, y, north, 0)
  end
end

function calc_forward(world::World, ant::Ant)
  nx = ant.x
  ny = ant.y

  if ant.direction === north
    ny -= 1
    if ny < 1 ny = world.height end
  elseif ant.direction === east
    nx += 1
    if nx > world.width nx = 1 end
  elseif ant.direction === south
    ny += 1
    if ny > world.height ny = 1 end
  else
    nx -= 1
    if nx < 1 nx = world.width end
  end

  return (nx, ny)
end

function food_ahead(world::World, ant::Ant)
  (x, y) = calc_forward(world, ant)
  return world.value[x, y] == food
end

function forward(world::World, ant::Ant)
  (ant.x, ant.y) = calc_forward(world, ant)
  if world.value[ant.x, ant.y] == food
    world.value[ant.x, ant.y] = empty
    ant.eaten += 1
  elseif world.value[ant.x, ant.y] == empty
    ant.eaten -= 1
  end
end

function turn_right(ant::Ant)
  #ant.direction = ant.direction + 1 > south ? north : ant.direction+1
  ndir = ant.direction + 1
  ant.direction = ndir.value > south.value ? north : ndir;
end

function turn_left(ant::Ant)
  #ant.direction = ant.direction - 1 < 1 ? south : ant.direction-1
  ndir = ant.direction - 1
  ant.direction = ndir.value < 1 ? south : ndir
end

make_block(lst::Array) = Expr(:block, lst...)
make_loop(values::Array) = Expr(:for, :(=), :i, Expr(:(:), 1, values[1]), values[2])
make_if(values::Array) = Expr(:if, values[1], values[2], values[3])

macro make_if(condition, true_block, false_block)
  Expr(:if, condition, true_block, false_block)
end

macro make_call(fn, args...)
  Expr(:call, fn, args...)
end

macro make_for(start, stop, block)
  Expr(:for, Expr(:(=), :i, Expr(:(:), start, stop)), block)
end

@grammar ant_grammar begin
  start = block
  command = turnleft | turnright | forward | if_statement | loop
  turnleft = Expr(:call, :turn_left, :ant)
  turnright = Expr(:call, :turn_right, :ant)
  forward = Expr(:call, :forward, :world, :ant)
  if_statement = Expr(:if, condition, block, block)
  condition = food_ahead
  food_ahead = Expr(:call, :food_ahead, :world, :ant)
  loop = Expr(:for, Expr(:(=), :i, Expr(:(:), 1, digit)), block)
  block[make_block] = (command)^(1:5)
  digit = 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
end

function evaluate!(grammar::Grammar, ind::AntIndividual, world::World)
  # copy world
  world = copy(world)

  # create ant
  ant = Ant(world.start[1], world.start[2])

  # generate code
  try
    ind.code = transform(grammar, ind)
    @eval fn(world::World, ant::Ant) = $(ind.code)
  catch e
    if typeof(e) !== MaxWrapException
      Base.error_show(STDERR, e, catch_backtrace())
    end
    ind.fitness = -Inf
    return
  end

  # run code
  fn(world, ant)

  # fitness is the amount of food eaten
  ind.fitness = convert(Float64, ant.eaten)
end

# need to redefine 'isless' (eating more is better)
isless(ind1::AntIndividual, ind2::AntIndividual) = ind1.fitness > ind2.fitness


function main()
  data = open("../examples/santefe.trail")
  (map, start) = read_map(readlines(data))
  world = World(map, start)

  # create population
  pop = AntPopulation(500, 500)

  fitness = 0
  generation = 1

  evaluate!(ant_grammar, pop, world)
  while generation < 1000
    # generate a new population (based off of fitness)
    pop = generate(ant_grammar, pop, 0.1, 0.2, 0.2, world)

    # population is sorted, so first entry it the best
    fitness = pop[1].fitness
    println("generation: $generation, $(length(pop)), max fitness=$fitness\n$(pop[1].code)")
    generation += 1
  end
end

main()
