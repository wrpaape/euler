defmodule Euler.Set1 do
  @moduledoc """
  Set1 holds solutions to problems 1 - 10.
  """


  @doc """
  1) Multiples of 3 and 5

  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.

  01/10/16

  iex> Euler.Set1.problem_1
  233168
  """
  def problem_1 do
    1..999
    |> Enum.filter(&(rem(&1, 3) == 0 or rem(&1, 5) == 0))
    |> Enum.sum
  end


  @doc """
  2) Even Fibonacci Numbers

  If each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
  
  1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

  1/10/16

  iex> Euler.Set1.problem_2
  4613732
  """
  def problem_2 do
    {0, 1}
    |> Stream.unfold(fn
      ({x, y}) when x + y >= 4_000_000 ->
        nil

      ({x, y}) ->
        next = x + y

        {next, {y, next}}
    end)
    |> Enum.filter(&(rem(&1, 2) == 0))
    |> Enum.sum
  end


  @doc """
  3) Largest Prime Factor

  The prime factors of 13195 are 5, 7, 13 and 29.

  What is the largest prime factor of the number 600851475143 

  01/11/16

  iex> Euler.Set1.problem_3
  6857
  """
  @x 600851475143
  def problem_3(last_smallest_factor \\ 2) do
    smallest_factor =
      last_smallest_factor..@x - 1
      |> Enum.find(&(rem(@x, &1) == 0))

    largest_factor =
      @x
      |> div(smallest_factor)

    2..div(largest_factor, 2) + 1
    |> Enum.any?(&(rem(largest_factor, &1) == 0))
    |> if do
      smallest_factor
      |> + 1
      |> problem_3
    else
      largest_factor
    end
  end

  @doc """
  4) Largest Palindrome Product 

  A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

  Find the largest palindrome made from the product of two 3-digit numbers.

  01/11/16

  iex> Euler.Set1.problem_4
  906609
  """
  def problem_4 do
    999..100
    |> Enum.reduce(0, fn(x, max_palindrome_product) ->
      x..100
      |> Enum.find_value(fn(y) ->
        product = 
          x * y
          |> Integer.digits

        product
        |> Enum.reverse
        |> case do
          ^product -> Integer.undigits(product)
          ________ -> nil
        end
      end)
      |> case do
        nil ->
          max_palindrome_product

        palindrome_product ->
          max_palindrome_product
          |> max(palindrome_product)
      end
    end)
  end


  @doc """
  5) Smallest Multiple

  2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

  What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

  01/11/16

  iex> Euler.Set1.problem_5
  232792560
  """
  @fact_twenty Enum.reduce(2..20, &*/2)
  def problem_5(last_smallest \\ @fact_twenty) do
    2..20
    |> Enum.reduce(last_smallest, fn(x, last_smallest) ->
      last_smallest
      |> div(x)
      |> case do
        next_smallest when next_smallest == last_smallest / x ->
          20..2
          |> Enum.all?(&(rem(next_smallest, &1) == 0))
          |> if do
            next_smallest
          else
            last_smallest
          end
        _____________________________________________________ ->
          last_smallest
      end
    end)
    |> case do
      ^last_smallest -> last_smallest
      next_smallest  -> problem_5(next_smallest)
    end
  end

  @doc """
  6) Sum Square Difference
  The sum of the squares of the first ten natural numbers is,

  12 + 22 + ... + 102 = 385
  The square of the sum of the first ten natural numbers is,

  (1 + 2 + ... + 10)2 = 552 = 3025
  Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

  Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

  01/12/16

  iex> Euler.Set1.problem_6
  25164150
  """
  def problem_6 do
    {sum_of_squares, sum} =
      1..100
      |> Enum.reduce({0, 0}, fn(x, {sum_of_squares, sum}) ->
        {sum_of_squares + :math.pow(x, 2), sum + x}
      end)

    sum
    |> :math.pow(2)
    |> - sum_of_squares
    |> trunc
  end

  @doc """
  10001st Prime
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

  What is the 10 001st prime number?

  01/12/16

  iex> Euler.Set1.problem_7
  104743
  """
  def problem_7 do
    3
    |> Stream.iterate(&(&1 + 2))
    |> Enum.reduce_while({[], 1}, fn
      (_, {acc_primes, 10_001}) ->
        {:halt, List.last(acc_primes)}

      (x, {acc_primes, count}) -> 
        next_acc = 
          acc_primes
          |> Enum.reduce_while({[], acc_primes}, fn(_, {behind, [next | ahead]}) ->
            if rem(x, next) == 0 do
              {:halt, {acc_primes, count}}
            else
              {:cont, {[next | behind], ahead}}
            end
          end)
          |> case do
            {rev_acc_primes, []} -> {Enum.reverse([x | rev_acc_primes]), count + 1}
            last_acc             -> last_acc
          end

        {:cont, next_acc}
    end)
  end

  @doc """
  8) Largest Product in a Series

  The four adjacent digits in the 1000-digit number that have the greatest product are 9 × 9 × 8 × 9 = 5832.

  73167176531330624919225119674426574742355349194934
  96983520312774506326239578318016984801869478851843
  85861560789112949495459501737958331952853208805511
  12540698747158523863050715693290963295227443043557
  66896648950445244523161731856403098711121722383113
  62229893423380308135336276614282806444486645238749
  30358907296290491560440772390713810515859307960866
  70172427121883998797908792274921901699720888093776
  65727333001053367881220235421809751254540594752243
  52584907711670556013604839586446706324415722155397
  53697817977846174064955149290862569321978468622482
  83972241375657056057490261407972968652414535100474
  82166370484403199890008895243450658541227588666881
  16427171479924442928230863465674813919123162824586
  17866458359124566529476545682848912883142607690042
  24219022671055626321111109370544217506941658960408
  07198403850962455444362981230987879927244284909188
  84580156166097919133875499200524063689912560717606
  05886116467109405077541002256983155200055935729725
  71636269561882670428252483600823257530420752963450

  Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?

  01/12/16

  iex> Euler.Set1.problem_8
  23514624000
  """
  @x """
  73167176531330624919225119674426574742355349194934
  96983520312774506326239578318016984801869478851843
  85861560789112949495459501737958331952853208805511
  12540698747158523863050715693290963295227443043557
  66896648950445244523161731856403098711121722383113
  62229893423380308135336276614282806444486645238749
  30358907296290491560440772390713810515859307960866
  70172427121883998797908792274921901699720888093776
  65727333001053367881220235421809751254540594752243
  52584907711670556013604839586446706324415722155397
  53697817977846174064955149290862569321978468622482
  83972241375657056057490261407972968652414535100474
  82166370484403199890008895243450658541227588666881
  16427171479924442928230863465674813919123162824586
  17866458359124566529476545682848912883142607690042
  24219022671055626321111109370544217506941658960408
  07198403850962455444362981230987879927244284909188
  84580156166097919133875499200524063689912560717606
  05886116467109405077541002256983155200055935729725
  71636269561882670428252483600823257530420752963450
 """
  def problem_8 do
    @x
    |> String.replace(["\n", " "], "")
    |> String.split("0", trim: true)
    |> Enum.filter_map(&(byte_size(&1) >= 13), fn(string_chunk) ->
      string_chunk
      |> String.split("", trim: true)
      |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.reduce(0, fn(digits, largest_product) ->
      {digits, rem_digits} =
        digits
        |> Enum.split(13)

        product =
          digits
          |> Enum.reduce(&*/2)

        largest =
          product
          |> max(largest_product)

      rem_digits
      |> Enum.reduce({largest, product, digits}, fn(digit, {largest, product, [head | tail]}) ->
        next_product =
          product * digit
          |> div(head)

        next_largest =
          next_product
          |> max(largest)

          {next_largest, next_product, List.insert_at(tail, 12, digit)}
      end) 
      |> elem(0)
    end)
  end

  @doc """
  9) Special Pythagorean Triplet

  A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

  a^2 + b^2 = c^2
  For example, 32 + 42 = 9 + 16 = 25 = 52.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.

  01/12/16

  iex> Euler.Set1.problem_9
  31875000
  """
  def problem_9 do
    0..998
    |> Enum.find_value(fn(a) ->
      a_sq   = a * a
      b_init = a + 1
      c_init = 1_000 - a - b_init
      b_last = b_init + div(c_init - b_init, 2) - 1

      b_init..b_last
      |> Enum.reduce_while(c_init, fn(b, c) ->
          b_sq = b * b
          c_sq = c * c

          a_sq + b_sq
          |> case do
            ^c_sq               -> {:halt, {:done, [a, b, c]}}
            lhs when lhs < c_sq -> {:cont, c - 1}
            ___________________ -> {:halt, false}
          end
      end)
      |> case do
        {:done, triple} -> Enum.reduce(triple, &*/2)
        _______________ -> false
      end
    end)
  end

  @doc """
  10) Summation of Primes

  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
  """
  @max   1_999_999
  @procs 4
  def problem_10 do
    results = [2, 3, 5]
    max_square_factor =
      @max
      |> :math.pow(0.5)
      |> trunc

    sum_sup =
      __MODULE__
      |> spawn(:init_sum_sup, [self])

    prime_sup =
      __MODULE__
      |> spawn(:init_primes_sup, [sum_sup])

    __MODULE__
    |> spawn(:base_primes, [7, max_square_factor, prime_sup])

    delta =
      @max
      |> - max_square_factor
      |> div(@procs)

    last_first =
      1..@procs - 1
      |> Enum.reduce(max_square_factor + 1, fn(first) ->
        last = first + delta
        __MODULE__
        |> spawn(:first_pass, [first, last, prime_sup])

        last + 1
      end)

    __MODULE__
    |> spawn(:first_pass, [last_first, @max, prime_sup])

    receive do
      sum_primes -> sum_primes
    end
  end

  def init_sum_sup(root_pid) do
    receive do
      base_sum ->
        @procs
        |> await_sums(root_pid, base_sum)
    end
  end

  def init_primes_sup(sum_sup_pid) do
    receive do
      {base_sum, base_primes} ->
        sum_sup_pid
        |> send(base_sum)

        @procs
        |> await_primes(base_sum)
    end


  end

  def await_sums(0, root_pid, sum) do
    root_pid
    |> send(sum)

    exit(:kill)
  end

  def await_sums(sum, rem_procs) do
    receive do
      next_sum ->
        sum + next_sum
        |> await_sums(rem_procs - 1)
    end
  end

  def await_primes(_, 0),                  do: sum
  def await_primes(base_primes, rem_procs) do
    receive do
      next_primes ->
        __MODULE__
        |> spawn(:filter_square_factors, [next_primes, base_primes])
        

    end

  end

  def sqrt(x), do: :math.pow(x, 0.5)
  def first_pass(n) do
    n
    |> rem(60)
    |> case do
      r when r in [1, 13, 17, 29, 37, 41, 49, 53] ->
        (n - 1) / 4
        |> sqrt
        |> trunc
        |> Range.new(1)
        |> Enum.reduce(false, fn(x, is_prime) ->
          n - 4 * x * x
          |> :math.pow(0.5)
          |> case do
            y when y == trunc(y) -> not is_prime
            ____________________ -> is_prime
          end
        end)

      r when r in [7, 19, 31, 43]                 ->
        (n - 1) / 3
        |> sqrt
        |> trunc
        |> Range.new(1)
        |> Enum.reduce(false, fn(x, is_prime) ->
          n - 3 * x * x
          |> :math.pow(0.5)
          |> case do
            y when y == trunc(y) -> not is_prime
            ____________________ -> is_prime
          end
        end)

      r when r in [11, 23, 47, 59]                ->
        (sqrt(2 * n + 3) - 1) / 2
        |> trunc
        |> Range.new(sqrt((n + 1) / 3) |> trunc |> + 1) 
        |> Enum.reduce(false, fn(x, is_prime) ->
          3 * x * x - n
          |> :math.pow(0.5)
          |> case do
            y when y == trunc(y) -> not is_prime
            ____________________ -> is_prime
          end
        end)

      ___________________________________________ ->
        false
    end
  end

# def problem_10
  #   |> Stream.take_every(2)
  #   |> Enum.reduce({2, []}, fn(x, acc_tup = {acc_sum, acc_primes}) ->
  #     acc_primes
  #     |> Enum.reduce_while({[], acc_primes}, fn(_, {behind, [next | ahead]}) ->
  #       if rem(x, next) == 0 do
  #         IO.inspect({x, next, next/x, length(acc_primes), length(behind)})
  #         {:halt, :not_prime}
  #       else
  #         {:cont, {[next | behind], ahead}}
  #       end
  #     end)
  #     |> case do
  #       :not_prime           -> acc_tup
  #       {rev_acc_primes, []} -> {acc_sum + x, Enum.reverse([x | rev_acc_primes])}
  #       # {rev_acc_primes, []} -> {acc_sum + x, [x | rev_acc_primes]}
  #     end
  #   end)
  # end
end
