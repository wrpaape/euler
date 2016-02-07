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
      accs = tri.pop.map.with_index { |val, col| {sum: val, col: col, hist: [val]} }

      # rev_tri = tri.reverse.each do |row|
      #   row.push(-1)
      #   row.unshift(-1)


      tri.reverse.each do |row|
        accs = accs.flat_map do |acc|
          # puts acc[:hist].inspect

          parent_sum  = acc[:sum]
          parent_hist = acc[:hist]

          right_col = acc[:col]
          left_col  = right_col - 1

          left_val  = row[left_col]
          right_val = row[right_col]

          next_accs = []

          if left_val
            next_accs.push({
              sum: parent_sum + left_val,
              col: left_col,
              hist: parent_hist.clone.push(left_val)
            })
          end

          if right_val
            next_accs.push({
              sum: parent_sum + right_val,
              col: right_col,
              hist: parent_hist.clone.push(right_val)
            })
          end

          next_accs
        end
          # next_col = (row[left_col] > row[right_col]) ? left_col : right_col
          # next_val = row[next_col]
          # puts "left_col:  #{left_col}"
          # puts "right_col: #{right_col}"
          # puts "left_val:  #{row[left_col]}"
          # puts "right_val: #{row[right_col]}"
          # puts "next_col:  #{next_col}"
          # puts "next_val:  #{next_val}"
          # acc[:sum] += next_val
          # acc[:col] =  next_col
          # acc[:hist].push(next_val)

        merged_accs = accs.group_by { |acc| acc[:col] }.values


        accs = merged_accs.map { |crossed_accs| crossed_accs.max_by { |acc| acc[:sum] } }

      end

      max_acc = accs.max_by { |acc| acc[:sum] }

      puts max_acc.inspect



    rescue Exception => e
      puts e.inspect
      puts e.backtrace
    end
  end
end
