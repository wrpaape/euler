=begin
**************************************************************************************
*                                    ruby_api.rb                                     *
*                                                                                    *
* Houses the main class of the Ruby API, responsible for communication between       *
* Elixir Mix project 'euler' and problems solved in Ruby.                            *
**************************************************************************************
=end
# Dir["set_*.rb"].each {|file| require_relative file}

class RubyAPI
=begin
**************************************************************************************
*                                   - self.main -                                    *
*                                                                                    *
* Method responsible for fetching the problem set class specified by 'set_num',      *
* calling its problem method specified by 'prob_num', printing the results to        *
* stdout, and handling all possible errors encountered along the way.                *
**************************************************************************************
=end
  def self.main(set_num, prob_num)
    # set_class_str = "Set#{set_num}"

    # Dir["set_*.rb"].each {|file| require_relative file}

    begin
      require_relative "set_#{set_num}.rb"

    rescue LoadError
      puts "awooga 0"
      exit 1
    end

    set_class_str = "Set#{set_num}"

    unless Object.const_defined?(set_class_str)
      # TODO print error msg to stderr
      puts "awooga 1"
      exit 1
    end

    set_class = Object.const_get(set_class_str)


    prob_meth = "problem_#{prob_num}"

    unless set_class.respond_to?(prob_meth)
      # TODO print error msg to stderr
      puts prob_meth
      puts "awooga 2"
      exit 1
    end

    time_start   = Time.now
    solution     = set_class.send(prob_meth)
    time_finish  = Time.now

    time_elapsed = (time_finish - time_start) * 1_000_000 # convert s → μs

    print(solution, "\n", time_elapsed.round)             # print solution to stdout
  end
end

RubyAPI.main(*ARGV)
