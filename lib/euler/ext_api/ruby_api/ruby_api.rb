Dir["set_*.rb"].each {|file| require_relative file}

class RubyAPI
  def self.main(set_num, prob_num)
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

    set_class.send(prob_meth)
  end
end

RubyAPI.main(*ARGV)
