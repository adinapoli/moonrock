#!/usr/bin/env ruby
require 'logger'

require "baz"


# This is a comment
class MyApp < Logger
  def do_log(what_to_log, what_toLog2  , whatToLog3)
    10
    "hello"
    x = 3
    x.logger = "Doesn't have any sense"
    x.logger.message = "10"
    x.logger.bar = y.foo.baz
    9 + 0
  end

  def anotherFunc()
    10
    "Hello"
    7 - 15
  end

  def func2()
    def nested(f1, f2)
      10
    end

    10
    "Hello"


  end
end

"This stuff ends here."
