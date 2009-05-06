#require 'enumerator'
require 'csv'
require 'pp'
require 'time'

def print_eval(expr)
  retval = eval(expr)
  puts "#{expr} = #{retval}"
  retval
end

class Array
  def interpolationSearch(toFind, not_found = nil, extractor = ->(element){ element })
    return nil if self.length == 0
    
    # Returns index of toFind in self (which is a sorted array), or nil if not found
    low = 0
    high = self.length - 1
    low_elem = extractor.(self[low])
    high_elem = extractor.(self[high])
    
    low = self.length if toFind > high_elem
    
    #puts "low=#{low} ; high=#{high} ; self[low]=#{self[low] || 'nil'} ; self[high]=#{self[high] || 'nil'}"
    
    # while low_elem < toFind <= high_elem
    while low_elem < toFind && toFind <= high_elem
      # estimate, using linear interpolation, where in the array the target element is located
      #   estimation = ((toFind - low_elem) / (high_elem - low_elem)) * (high - low)
      #   but we implement it as
      #   estimation = ((toFind - low_elem) * (high - low)) / (high_elem - low_elem)
      #   because this way we don't have to manually deal with floating point to integer conversions since we're using integer division
      mid = low + ((toFind - low_elem) * (high - low) / (high_elem - low_elem)).floor
      mid_elem = extractor.(self[mid])
      
      #puts "low=#{low} ; mid=#{mid} ; high=#{high} ; self[low]=#{self[low] || 'nil'} ; self[mid]=#{self[mid] || 'nil'} ; self[high]=#{self[high] || 'nil'}"
      
      if toFind > mid_elem
        low = mid + 1
        low_elem = extractor.(self[low])
      elsif toFind < mid_elem
        #high = mid - 1     # This is incorrect. Since self[mid-1] may be less than toFind, which would break out of the while loop,
                            # the index stored in low would likely not be the index that toFind should be placed
        high = mid
        high_elem = extractor.(self[high])
      else
        low = mid
        break
      end
    end
    
    #puts "low=#{low} ; self[low]=#{self[low] || 'nil'}"
    item = self[low]
    if item && extractor.(item) == toFind
      low
    else
      # toFind ought to be inserted between the indices: (low - 1) and low
      case
        when not_found == :before
          if low > 0
            low - 1
          else
            nil
          end
        when not_found == :after
          if low < self.length
            low
          else
            nil
          end
        else
          nil
      end
    end
  end
  
  def combine(other)
    retval = Hash.new
    each_with_index do |obj, i|
      retval[obj] = other[i]
    end
    retval
  end
  
  # ActiveSupport defines a method on Array called rand() that "Returns a random element from the array."
  def random_element
    self[Kernel.rand(length)]
  end

  def random_index
    Kernel.rand(length)
  end
  
  def percentiles(list, already_sorted = false)
    self.sort! unless already_sorted
    list.map { |p| self.percentile(p, true) }
  end

  # assumes the list is already sorted
  def percentile(p, already_sorted = false)
    self.sort! unless already_sorted
    i = (length * p).ceil - 1
    if i < 0 || i >= length
      nil
    else
      self[i]
    end
  end
  
  def to_h
    Hash[self]
  end
end

class CSVNumericTable
  attr_reader :table
  attr_reader :first_row
  attr_reader :first_column
  
  def initialize(path)
    @table = CSV.read(path, {converters: :numeric})
    @first_row = @table.shift
    @first_row = @first_row.drop(1)   # to get rid of the first element representing the row-header column
    @table = @table.transpose
    @first_column = @table.shift
    @table = @table.transpose
  end
  
  def get(x, y, interpolate = false)
    col = @first_row.interpolationSearch(x, :before)
    row = @first_column.interpolationSearch(y, :before)
    
    if row && col
      if @first_row[col] == x && @first_column[row] == y
        @table[row][col]
      elsif interpolate
        if @first_row[col] == x               # only need to interpolate on the y-axis (vertical axis)
          if @first_column[row+1]             # check to see if interpolation on the y-axis is possible
            y_p = y.falls_what_percentage_between(@first_column[row], @first_column[row+1])

            column = @table[row,2].map{|row_array| row_array[col] }   # extract the 2 values we need to interpolate between
            y_p.percent_between(column[0], column[1])
          end
        elsif @first_column[row] == y         # only need to interpolate on the x-axis (horizontal axis)
          if @first_row[col+1]                # check to see if interpolation on the x-axis is possible
            x_p = x.falls_what_percentage_between(@first_row[col], @first_row[col+1])

            a, b = @table[row].values_at(col, col+1)                  # extract the 2 values we need to interpolate between
            x_p.percent_between(a, b)
          end
        else                                  # need to interpolate on both axes
          if @first_row[col+1] && @first_column[row+1]      # check to see if interpolation on both axes is possible
            x_p = x.falls_what_percentage_between(@first_row[col], @first_row[col+1])
            y_p = y.falls_what_percentage_between(@first_column[row], @first_column[row+1])

            column = @table[row,2].map{|row_array| x_p.percent_between(row_array[col], row_array[col+1]) }
            y_p.percent_between(column[0], column[1])
          end
        end
      end
    end
  end
  
  def [](row, column)
    @table[row][column]
  end
end

class Dir
  def self.directories(dirname)
    dirname = File.expand_path("#{dirname}")
    dirname += File::Separator unless dirname[-1].chr == '/'
    listing = entries(dirname).reject { |file| file =~ /^(\.|\.\.)$/ || !File.directory?(File.join(dirname, file)) }    #remove the '.' and '..' directories from the listing if they exist
    listing.collect { |file| "#{dirname}#{file}" }
  end
end

module Enumerable
  class Enumerator
    def to_gen
      Generator.new(self)
    end
  end
  
  def aggregate(accumulator, post_accumulator = nil, initial = nil)
    params = [initial].compact
    accumulator = accumulator.to_proc if accumulator.is_a?(Symbol)
    
    val = reduce(*params, &accumulator)
    
    # any block is assumed to be a post_accumulator
    if block_given?
      yield val
    elsif post_accumulator
      post_accumulator.call(val)
    else
      val
    end
  end
  
  # This method behaves like map but returns a pair (2-element array) per collection element. Each
  # pair contains the result of applying the key_f and value_f lambdas on each collection element.
  #
  # Returns an array of pairs, each of the form: [key_f.call(elem), value_f.call(elem)]
  def pmap(key_f, value_f)
    map { |o| [key_f.call(o), value_f.call(o)] }
  end

  # Requires Ruby 1.9 for the Enumerator#group_by method
  #
  # This method first groups the collection elements into sub-collections. The sub-collections
  # are determined by the grouping function: *key_f* (a lambda)
  # After grouping is performed, each sub-collection is reduced to a single value by applying the 
  # reduce_f lambda on each sub-collection.
  #
  # Returns an array of pairs, each of the form: [key_f.call(elem), reduce_f.call(elements_with_same_key_value)]
  def greduce(key_f, reduce_f)
    grouped = group_by { |elem| key_f.call(elem) }   #returns a hash
    reduced = grouped.map { |k, value_array| [k, reduce_f.call(value_array)] }
  end

=begin
  # This method allows one to reduce a collection of multi-attribute objects to an aggregate Hash.
  # The return Hash contains attribute (key)/aggregate (value) pairs containing aggregates of multiple
  # attributes from the collection of objects.
  def mreduce(initial=nil, keys, reduce_f, post_reduce_f = nil)
    aggregate_hash = inject(Hash.new(initial)) do |m,o|
      for key in keys
        m[key] = reduce_f.call(m[key], o)
      end
    end
    aggregate_hash = aggregate_hash.map { |key, aggregate| post_reduce_f.call(key, aggregate) } if post_reduce_f
    aggregate_hash
  end
=end
end

class File
  class << self
    # source should be a zip file, target should be a directory to output the contents to.
    def unzip(source, target = File.dirname(source))
      # Create the target directory.
      FileUtils.mkdir_p(target)

      Zip::ZipFile.open(source) do |zipfile|
        dir = zipfile.dir
        dir.entries('.').each do |entry|
          zipfile.extract(entry, File.join(target, entry))
        end
      end
    rescue Zip::ZipDestinationFileExistsError => e
      # I'm going to ignore this and just overwrite the files.
    rescue => e
      puts e
      puts e.backtrace.join("\n")
    end
    
    def find_r(root_dir, search_pattern, &blk)
      find(File.join(root_dir, '**', search_pattern), &blk)
    end
    
    def find(search_pattern)
      if block_given?
        Dir.glob(search_pattern).each { |f| yield File.expand_path(f) }
      else
        Dir.glob(search_pattern).map { |f| File.expand_path(f) }
      end
    end
  end
  
  # Based on Perl's File::ReadBackwards module, by Uri Guttman.
  # Found at http://rubyquiz.com/quiz64.html
  class ReadBackwards
    MAX_READ_SIZE = 1 << 10 # 1024

    def initialize( *args )
      @file = File.new(*args)
      @file.seek(0, IO::SEEK_END)

      @current_pos = @file.pos

      @read_size = @file.pos % MAX_READ_SIZE
      @read_size = MAX_READ_SIZE if @read_size.zero?

      @line_buffer = Array.new
    end

    def gets( sep_string = $/ )
      return @line_buffer.pop if @line_buffer.size > 2 or @current_pos.zero?

      @current_pos -= @read_size
      @file.seek(@current_pos, IO::SEEK_SET)

      @line_buffer[0] = "#{@file.read(@read_size)}#{@line_buffer[0]}"
      @read_size = MAX_READ_SIZE # Set a size for the next read.

      @line_buffer[0] =
      @line_buffer[0].scan(/.*?#{Regexp.escape(sep_string)}|.+/)
      @line_buffer.flatten!

      gets(sep_string)
    end
  end
end

class Integer
  def gcd(other)
    other = other.to_i
    if self < other
      return other.gcd(self)
    end

    r = self % other
    if r == 0
      return other
    else
      other.gcd r
    end
  end
end

class Numeric
  # Compute what number falls p percent between a and b, where p is the number represented by self.
  #   Assumed that b >= a ; On second thought, I don't think this is a necessary requirement
  def percent_between(a, b)
    a + (b - a) * (self.to_f / 100)
  end
  
  # Compute what percentage, p, of the range between a and b the number (represented by self) falls
  #   Assumed that b >= a ; On second thought, I don't think this is a necessary requirement
  #   Assumed that a <= number (self) <= b
  #             OR b <= number (self) <= a
  def falls_what_percentage_between(a, b)
    unless b
      # if this occurs, this function behaves as if it were called: is_what_percentage_of(a)
      b = a
      a = 0
    end
    100 * ((self.to_f - a) / (b - a))
  end
  
  def in_seconds
    # assumes self is in seconds
    self
  end

  def in_minutes
    # assumes self is in seconds
    self / 60
  end

  def in_hours
    # assumes self is in seconds
    self / 3600
  end
  
  def in_days
    # assumes self is in seconds
    self / 86400
  end

  def in_months
    # assumes self is in seconds
    self / 2592000
  end

  def in_years
    # assumes self is in seconds
    self / 31557600
  end
end

#extension to Object class found at: http://whytheluckystiff.net/articles/seeingMetaclassesClearly.html
class Object
  # The hidden singleton lurks behind everyone
  def metaclass; class << self; self; end; end
  
  # similar to class_eval. Executes a block of code within the context of the metaclass of self.
  # When we implement meta_eval with metaclass.class_eval, we can define instance methods on the singleton class (singleton methods) of the class represented by self like this:
  #   C.meta_eval { def foo; puts 'foo'; end }     -->     C.foo => outputs "foo\n"
  # But when we implement meta_eval with metaclass.instance_eval, we define instance methods on the singleton class of the singleton class (singleton methods on the singleton class) of the class represented by self:
  #   C.meta_eval { def bar; puts 'bar'; end }      -->     C.metaclass.f => outputs "bar\n"     ;    Not C.bar as we may have intended
  def meta_eval &blk; metaclass.class_eval(&blk); end    #originally, instance_eval instead of class_eval
  
  # Adds methods to a metaclass
  def meta_def name, &blk
    # we use define_method because it will always create an instance method on the class represented by self. This way, if meta_eval is implemented with either class_eval or instance_eval, the method 
    # we are defining with our call to meta_def is guaranteed to be defined as an instance method on the metaclass/singleton class.
    meta_eval { define_method name, &blk }
  end
  
  # Defines an instance method within a class
  def class_def name, &blk
    # we use define_method because it will always create an instance method on the class represented by self
    class_eval { define_method name, &blk }
  end
  
  def generator(iterator)
    Generator.new(enumerator(iterator))
  end
  
  def to_pps
    PP.pp(self, "")
  end
end

class String
  def each_match(pattern, resume_at_end_of_match = false)
    offset = 0
    while offset = index(pattern, offset)
      #see http://www.zenspider.com/Languages/Ruby/QuickRef.html
      #$&         The string matched by the last successful pattern match in this scope.
      #$`         The string to the left  of the last successful match.
      #$'         The string to the right of the last successful match.
      #$1         The Nth group of the last successful match. May be > 1.
      #$~         The information about the last match in the current scope. A MatchData object.
      
      #puts "#{$`}<<#{$&}>>#{$'} - #{$~.offset(0)}"     #print out some debug info on each pattern match
      yield offset, $~
      
      if resume_at_end_of_match
        offset = $~.offset(0)[1]    #offset(0) returns a 2-element array: [index of the start of the match, index of the character after the end of the match]
      else
        offset += 1
      end
    end
    nil
  end
  
  # A reverse match function that can function as a greedy rindex().
  def reverse_match(pattern, offset = -1, greedy = true)
    start_index = rindex(pattern, offset)
    if start_index
      first_match = $~
      if greedy
        last_character_index = first_match.offset(0)[1]
        last_match = first_match
        
        # decrement start_index until we back up one character too far, then return the last good match
        start_index -= 1
        while start_index >= 0 && self.index(pattern, start_index) == start_index && $~.offset(0)[1] == last_character_index
          last_match = $~
          start_index -= 1
        end
        
        return last_match
      else
        first_match
      end
    end
  end

  # A reverse match function that can function as a greedy rindex().
  def reverse_match_orig(pattern, offset = -1, greedy = true)
    start_index = rindex(pattern, offset)
    if start_index
      first_match = $~
      if greedy
        last_character_index = first_match.offset(0)[1]
        # look for all matches in the substring starting at index 0 and ending at the index of the last character included in the first match
        matches = self[0, last_character_index].enum_for(:each_match, pattern)
        
        # find the first match in the list of matches that has a last-character match index position equal to last_character_index
        # NOTE: this is guaranteed to return something because rindex() returned something.
        matches.each do |start_offset, m|
          if m.offset(0)[1] == last_character_index
            return m
          end
        end
      else
        first_match
      end
    end
  end
  
  #determines if (and where) a given pattern is found before or after a particular anchor offset in the string
  # [optionally] within a certain number of tokens (words, characters, etc.) of the anchor offset
  def contain?(pattern, position, root_offset, distance = nil, token = nil, inclusive = false)
    unit_offset = inclusive ? 1 : 0
    if position == :before_or_after
      if (offset = rindex(pattern, root_offset))
        match = $~
        return match if distance == nil || token == nil || slice(offset, root_offset - offset + unit_offset).scan(token).length <= distance
      end
      if (offset = index(pattern, root_offset))
        match = $~
        return match if distance == nil || token == nil || slice(root_offset, offset - root_offset + unit_offset).scan(token).length <= distance
      end
    elsif position == :before
      if (offset = rindex(pattern, root_offset))
        match = $~
        return match if distance == nil || token == nil || slice(offset, root_offset - offset + unit_offset).scan(token).length <= distance
      end
    elsif position == :after
      if (offset = index(pattern, root_offset))
        match = $~
        return match if distance == nil || token == nil || slice(root_offset, offset - root_offset + unit_offset).scan(token).length <= distance
      end
    end
    nil
  end
  
  def within?(source_offset, dest_offset, distance, token, inclusive = false)
    unit_offset = inclusive ? 1 : 0
    indices = [source_offset, dest_offset]
    min, max = indices.min, indices.max
    slice(min, max - min + unit_offset).scan(token).length <= distance
  end
  
  def segment(*indices)
    retval = Array.new
    indices = indices.flatten.push(0, length).uniq.select(&lambda {|i| i >= 0 && i <= length }).sort
    i = indices.shift
    indices.each do |j|
      retval << slice(i...j)
      i = j
    end
    retval
  end
end

class Time
  def self.from_timestamp(timestamp)
    strptime(timestamp.to_s, "%Y%m%d%H%M%S")
  end

  def to_timestamp
    strftime("%Y%m%d%H%M%S")
  end
end