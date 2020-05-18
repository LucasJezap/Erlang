
# mix test
# mix test test/projekt_test.exs
# mix test test/projekt_test.exs:9 (10)
# mix test test/projekt_test.exs --only module:MyBasicMathTest
# mix test --seed 0
# mix test --trace
# mix test --stale
defmodule MyBasicMathTest do
  use ExUnit.Case, async: true

  setup_all do
    on_exit fn ->
      IO.puts "\n\nAll MyBasicMathTest tests were completed\n"
    end

    {:ok, num1: 2, num2: 3, num3: -5, num4: 7, num5: 0, num6: 4, fake1: :a, fake2: :b}
  end


  # start_supervised, stop_supervised

  test "valid sum", values do
    assert 5 == MyBasicMath.sum(values[:num1],values[:num2])
    assert 2 == MyBasicMath.sum(values[:num3],values[:num4]), "I like adding"
    refute 3 == MyBasicMath.sum(values[:num5],values[:num6])
  end

  test "invalid sum", values do
    assert_raise(ArgumentError,fn -> MyBasicMath.sum(values[:fake1],values[:num2]) end)
    assert_raise(ArgumentError,"invalid arguments",fn -> MyBasicMath.sum(values[:fake1],values[:fake2]) end)
  end

  test "valid difference", values do
    assert -1 == MyBasicMath.difference(values[:num1],values[:num2])
    assert -12 == MyBasicMath.difference(values[:num3],values[:num4])
    refute 3 == MyBasicMath.difference(values[:num5],values[:num6]), "Differences are fun"
  end

  test "invalid difference",values do
    assert_raise(ArgumentError,fn -> MyBasicMath.difference(values[:fake1],values[:num2]) end)
    assert_raise(ArgumentError,fn -> MyBasicMath.difference(values[:fake1],values[:fake2]) end)
  end

  test "valid product", values do
    assert 6 == MyBasicMath.product(values[:num1],values[:num2])
    assert -35 == MyBasicMath.product(values[:num3],values[:num4])
    refute 3 == MyBasicMath.product(values[:num5],values[:num6])
  end

  test "invalid product", values do
    assert_raise(ArgumentError,fn -> MyBasicMath.product(values[:fake1],values[:num2]) end)
    assert_raise(ArgumentError,fn -> MyBasicMath.product(values[:fake1],values[:fake2]) end)
  end

  test "valid division", values do
    assert_in_delta 2 / 3 , MyBasicMath.division(values[:num1],values[:num2]), 0.01
    assert_in_delta -5 / 7, MyBasicMath.division(values[:num3],values[:num4]), 0.01
    refute_in_delta 3, MyBasicMath.division(values[:num5],values[:num6]), 1
  end

  test "invalid division", values do
    assert_raise(ArgumentError,fn -> MyBasicMath.division(values[:fake1],values[:num2]) end)
    assert_raise(ArgumentError,fn -> MyBasicMath.division(values[:fake1],values[:fake2]) end)
    assert_raise(ArithmeticError,fn -> MyBasicMath.division(5,0) end)
  end

  # assert_receive - that's going to be received in within given period
  test "random" do
    MyBasicMath.random(10)
    assert_received :random_number
  end

  import ExUnit.CaptureIO
  # also capture_log, and refute_received
  test "best number" do
    assert capture_io(fn -> MyBasicMath.best_number end) == "14 is the best number\n"
    refute_receive :message
  end


  # there's also catch_error, catch_exit, catch_throw
  # flunk - fails with the message
   test "always fail" do
     flunk("Fail")
   end
end

  # mix test test/projekt_test.exs --only healthy
  # mix test test/projekt_test.exs --only killer:true
  # mix test test/projekt_test.exs --exclude test --include healthy
  # mix test --max-failures 1
defmodule MyCoffeeShopTest do
  use ExUnit.Case, async: true
  # @moduletag report: [...] - additional info about given tags if test fails
  describe "products" do

    @tag :pending # often used trick for showing that test is not yet finished (mix test --exclude pending)
    # @tag :skip - automatic skip of the test (without --exclude)
    @tag :healthy
    @tag killer: false
    test "milkshake" do
      assert {:milkshake, "Lovely milkshake"} == MyCoffeeShop.milkshake
    end

    @tag :healthy # unless you eat whole
    @tag killer: false
    test "chocolate" do
      assert {:chocolate, "Delicious chocolate"} == MyCoffeeShop.chocolate
    end

    @tag killer: false
    test "cotton candy" do
      assert {:cotton_candy, "Yummy candy"} == MyCoffeeShop.cotton_candy
    end

    @tag killer: true
    test "sugar death" do
      assert {:sugar_death, "I can't feel my stomach anymore!"} == MyCoffeeShop.sugar_death
    end
  end

end

# mock testing ? absolutely not
