=begin
**************************************************************************************
*                                    - Set2.rb -                                     *
*                                                                                    *
* Class 'Set2' houses solutions for problems 11-20.                                  *
**************************************************************************************
=end
class Set2
=begin
**************************************************************************************
*                                - self.problem_18 -                                 *
*                                                                                    *
* By starting at the top of the triangle below and moving to adjacent numbers on the *
* row below, the maximum total from top to bottom is 23.													   *
*                                                                                    *
*    3																																						   *
*   7 4																																						   *
*  2 4 6																																						 *
* 8 5 9 3																																						 *
*                                                                                    *
* That is, 3 + 7 + 4 + 9 = 23.																											 *
*                                                                                    *
* Find the maximum total from top to bottom of the triangle below:									 *
*                                                                                    *
*   (tri)                                                                            *
*                                                                                    *
* NOTE: As there are only 16384 routes, it is possible to solve this problem by			 *
* trying every route. However, Problem 67, is the same challenge with a triangle		 *
* containing one-hundred rows; it cannot be solved by brute force, and requires a		 *
* clever method! ;o)																																 *
**************************************************************************************
=end
  def self.problem_18(*args)
    tri = [[75],
           [95, 64],
           [17, 47, 82],
           [18, 35, 87, 10],
           [20,  4, 82, 47, 65],
           [19,  1, 23, 75,  3, 34],
           [88,  2, 77, 73,  7, 63, 67],
           [99, 65,  4, 28,  6, 16, 70, 92],
           [41, 41, 26, 56, 83, 40, 80, 70, 33],
           [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
           [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
           [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
           [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
           [63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
           [ 4, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23]]

    begin
      branches = [{acc_sum: 75, col: 0}] # holds column indices of branches sharing largest last sum

      acc = 75
      last_row = tri.length - 1

      1.upto(last_row - 1) do |row|  
        child_row = tri[row]
        max_sum = 0
        candidates = []

        branches.each do |branch|
          root_col = branch[:col]

          fork_tail_row = row + 1

          0.upto(1) do |step|
            fork_child        = {step: step}
            fork_child[:col]  = root_col + fork_child[:step]
            fork_child[:head] = child_row[fork_child[:col]]
            fork_child[:sum]  = fork_child[:head]

            col = fork_child[:col]
            fork_tail_row.upto(last_row) do |row|
              col              += fork_child[:step]
              fork_child[:sum] += tri[row][col]
            end

            if fork_child[:sum] >= max_sum
              max_sum               = fork_child[:sum]
              fork_child[:root_sum] = branch[:acc_sum]
              candidates.push(fork_child)
            end
          end
        end

        candidates.reject! { |fork_child| fork_child[:sum] < max_sum }

        puts "*********"
        puts "on row #{row}"
        branches = candidates.map do |fork_child| 
          puts "  branching: #{fork_child[:step].zero? ? "left" : "right"}"
          puts "  head:      #{fork_child[:head]}"
          puts "  row_max:   #{child_row.max}"
          puts "  sum:       #{fork_child[:sum]}"
          acc += fork_child[:head]
          {
            acc_sum: fork_child[:root_sum] + fork_child[:head],
            col:     fork_child[:col]
          }
        end
      end

      puts "acc: #{acc}"

      puts branches.inspect

    rescue Exception => e
      puts e.inspect
      puts e.backtrace
    end
  end
end
