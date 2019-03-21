def sapphire_init
    module ::Comparable
        def < other
            self.<=>(other) < 0
        end
        def <= other
            self.<=>(other) <= 0
        end
        def >= other
            self.<=>(other) >= 0
        end
        def > other
            self.<=>(other) > 0
        end
    end

    module ::Enumerable
        def all?
            self.each do |item|
                if not block_given? and !item
                    return false
                elsif not yield item
                    return false
                end
            end
            true
        end

        def any?
            self.each do |item|
                if not block_given? and item
                    return true
                elsif yield item
                    return true
                end
            end
            false
        end

        def collect
            array = Array.new
            self.each do |item|
                if block_given?
                    array << yield item
                else
                    array << item
                end
            end
            array
        end

        def detect fallback = nil
            self.each do |item|
                if block_given? and res = yield item
                    return res
                elsif not block_given?
                    return item
                end
            end
            fallback
        end

        def each_with_index
            i = 0
            self.each do |item|
                if block_given?
                    yield item, i
                end
            end
            self
        end

        def entries
            array = Array.new
            self.each do |item|
                array << item
            end
            array
        end

        def find_all
            array = Array.new
            self.each do |item|
                if block_given? and yield item
                    array << item
                else
                    array << item
                end
            end
            array
        end

        def grep pattern
            array = Array.new
            self.each do |item|
                if item === pattern
                    if block_given?
                        array << yield item
                    else
                        array << item
                    end
                end
            end
            array
        end

        def include? obj
            self.each do |item|
                if item == obj
                    return true
                end
            end
            false
        end
    end

    class ::Array
        include ::Enumerable
    end

    class ::NilClass
        def & other
            false
        end

        def ^ other
            !!other
        end

        def | other
            !!other
        end

        def nil?
            true
        end

        def to_s
            ""
        end
    end

    class ::TrueClass
        def & other
            !!other
        end

        def ^ other
            !other
        end

        def to_s
            "true"
        end

        def | other
            true
        end
    end

    class ::FalseClass
        def & other
            false
        end

        def ^ other
            !!other
        end

        def to_s
            "false"
        end

        def | other
            !!other
        end
    end

    class ::Numeric
        include ::Comparable

        def +@
            self
        end

        def -@
            v = 0.coerce self
            if !v.is_a? ::Array || !v.size == 2
                raise TypeError.new "coercion did not return an array"
            end
            v[0] - v[1]
        end

        def abs
            if self < 0
                -self
            else
                self
            end
        end

        def coerce other
            if self.class == other.class
                [other, self]
            else
                [x.to_f, self.to_f]
            end
        end
    end

    class ::Range
        attr_accessor :begin, :end

        def initialize range_begin, range_end, exclude_end = false
            @begin = range_begin
            @end = range_end
            @inclusive = !exclude_end
        end

        def == other
            other.is_a? ::Range and
                @begin == other.begin and
                @end == other.end and
                exclude_end? == other.exclude_end?
        end

        def === other
            if @begin.<=>(other) > 0
                false
            elsif (@inclusive and other.<=>(@end) <= 0) or (!@inclusive and other.<=>(@end) < 0)
                true
            else
                false
            end
        end

        def each &block
            if !@begin.respond_to? :succ
                raise TypeError.new "range begin does not respond to succ"
            end
            i = @begin
            loop do
                c = i.<=> @end
                if c > 0
                    return self
                elsif c == 0 and !@inclusive
                    return self
                elsif c == 0
                    block.call i
                    return self
                end
                block.call i
                i = i.succ
            end
        end

        def exclude_end?
            !@inclusive
        end

        def inspect
            if @inclusive
                "#@begin..#@end"
            else
                "#@begin...#@end"
            end
        end
    end
end
