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
      col_i      = 0
      col_route  = [col_i]
      last_row_i = tri.length - 1


      1.upto(last_row_i) do |row_i|
        shift_col_i = col_i
        left_sum  = 0
        right_sum = 0
        
        tri[row_i..-1].each do |row|
          shift_col_i += 1
          left_sum    += row[col_i]
          right_sum   += row[shift_col_i]
        end

        col_i += 1 if right_sum > left_sum

        col_route << col_i
      end

      acc_sum    = 0

      1.upto(last_row_i - 1) do |row_i|
        row      = tri[row_i]

        prev_col_i = col_route[row_i - 1]
        curr_col_i = col_route[row_i]
        next_col_i = col_route[row_i + 1]

        curr_val = row[curr_col_i]

        if next_col_i == prev_col_i + 1
          twin_col_i = prev_col_i + (next_col_i - curr_col_i)

          twin_val = row[twin_col_i]

          if twin_val > curr_val
            col_route[row_i] = twin_col_i
            curr_val         = twin_val
          end
        end
        acc_sum += curr_val
      end
      last_row = tri.last

      left_col_i  = col_route[-2]
      left_val    = last_row[left_col_i]
      right_col_i = left_col_i + 1
      right_val   = last_row[right_col_i]

      last_col_i = left_col_i
      last_val   = left_val

      if right_val > left_val
        last_col_i = right_col_i
        last_val   = right_val
      end

      col_route[-1] = last_col_i
      acc_sum      += last_val

      puts col_route.inspect
      puts col_route.map.with_index { |col_i, row_i| tri[row_i][col_i] }.inspect
      puts acc_sum




    rescue Exception => e
      puts e.inspect
      puts e.backtrace
    end
  end
end
