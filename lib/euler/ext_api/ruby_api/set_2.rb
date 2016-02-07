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
    # triangle = [[75],
    #             [95, 64],
    #             [17, 47, 82],
    #             [18, 35, 87, 10],
    #             [20,  4, 82, 47, 65],
    #             [19,  1, 23, 75,  3, 34],
    #             [88,  2, 77, 73,  7, 63, 67],
    #             [99, 65,  4, 28,  6, 16, 70, 92],
    #             [41, 41, 26, 56, 83, 40, 80, 70, 33],
    #             [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    #             [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    #             [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    #             [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    #             [63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    #             [ 4, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23]]

    begin
      branches = [{root: 75, acc_sum: 75, col: 0}] # holds column indices of branches sharing largest last sum

      last_row = triangle.length - 1

      1.upto(last_row) do |child_row|  
        live_branches = live_branches.flat_map do |branch|
          col     = branch[:col]
          acc_sum = branch[:acc_sum]

          puts branch.inspect

          fork_children = [0, 1].map do |col_step|  
            child_col = col + col_step
            head_val  = triangle[row][child_col]

            child = {
              col_step: col_step,
              col:      child_col, 
              acc_sum:  acc_sum + head_val,
              rem_sum:  head_val 
            }

           (row + 1).upto(last_row) do |child_row|
              child_col       += col_step
              child[:rem_sum] += triangle[child_row][child_col]
            end

            child
          end

          puts "*****"
          puts "children:  #{fork_children}"
          puts "*****"

          if fork_children[0][:rem_sum] == fork_children[1][:rem_sum]
            fork_children
          else
            fork_children.max_by { |child| child[:rem_sum] }
          end
        end
      end

      puts live_branches.inspect

    rescue Exception => e
      puts e.inspect
      puts e.backtrace
    end
  end
end
