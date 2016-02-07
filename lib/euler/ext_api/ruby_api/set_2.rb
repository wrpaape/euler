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
      route = [75]
      sum    = 75
      prev_col = 0
      last_row = tri.length - 1

      (1..last_row).step(2) do |prim_row|
        sec_row      = prim_row + 1

        prim_tri_row = tri[prim_row]
        sec_tri_row  = tri[sec_row]

        left_col  = prev_col
        mid_col   = left_col + 1
        right_col = mid_col + 1

        left  = prim_tri_row[left_col]
        right = prim_tri_row[mid_col]


        ll_sum = 0
        lr_sum = 0
        rl_sum = 0
        rr_sum = 0

        ll_col = left_col
        lr_col = mid_col
        rl_col = mid_col
        rr_col = right_col
        
        sec_row.upto(last_row) do |row|
          tri_row  = tri[row]
          ll_sum  += tri_row[ll_col]
          lr_sum  += tri_row[lr_col]
          rl_sum  += tri_row[rl_col]
          rr_sum  += tri_row[rr_col]
          rl_col  += 1
          rr_col  += 1
        end


        ll_loss  = rl_sum + rr_sum + right
        mid_loss = ll_sum + rr_sum
        lr_loss  = mid_loss + right
        rl_loss  = mid_loss + left
        rr_loss  = ll_sum + lr_sum + left


        child_nodes = [{loss: ll_loss, route: [left_col, left_col]},
                       {loss: lr_loss, route: [left_col, mid_col]},
                       {loss: rl_loss, route: [mid_col,  mid_col]},
                       {loss: rr_loss, route: [mid_col,  right_col]}]

        next_node  = child_nodes.min_by { |node| node[:loss] }

        node_route = next_node[:route]

        route.concat(node_route)

        prev_col   = route.last
      end

      route.each { |col, row| puts "row #{row}: #{tri[row][col]}" }


    rescue Exception => e
      puts e.inspect
      puts e.backtrace
    end
  end
end
