# testing module 1
defmodule MyBasicMath do

  def sum(a,b) do
    cond do
      is_number(a) == false or is_number(b) == false -> raise ArgumentError, message: "invalid arguments"
      true -> a + b
    end
  end

  def difference(a,b) do
    cond do
      is_number(a) == false or is_number(b) == false -> raise ArgumentError, message: "invalid arguments"
      true -> a - b
    end
  end

  def product(a,b) do
    cond do
      is_number(a) == false or is_number(b) == false -> raise ArgumentError, message: "invalid arguments"
      true -> a * b
    end
  end

  def division(a,b) do
    cond do
      is_number(a) == false or is_number(b) == false -> raise ArgumentError, message: "invalid arguments"
      b == 0 -> raise ArithmeticError, message: "division by 0!"
      true -> a / b
    end
  end

  def power(a,b) do
    cond do
      is_number(a) == false or is_number(b) == false -> raise ArgumentError, message: "invalid arguments"
      true -> :math.pow(a,b)
    end
  end

  def modulo(a,b) do
    cond do
      is_number(a) == false or is_number(b) == false -> raise ArgumentError, message: "invalid arguments"
      true -> rem(a,b)
    end
  end

  def random(n) do
    send(self(),:random_number)
    send(self(),:rand.uniform(n))
  end

  def best_number do
    IO.puts "14 is the best number"
  end

end

defmodule MyCoffeeShop do

  def milkshake do
    {:milkshake, "Lovely milkshake"}
  end

  def chocolate do
    {:chocolate, "Delicious chocolate"}
  end

  def cotton_candy do
    {:cotton_candy, "Yummy candy"}
  end

  def sugar_death do
    {:sugar_death, "I can't feel my stomach anymore!"}
  end

end