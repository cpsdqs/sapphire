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
                ::Array.new other, self
            else
                ::Array.new x.to_f, self.to_f
            end
        end
    end

    class ::Range
        attr_accessor :begin, :end

        def initialize range_begin, range_end, inclusive = true
            @begin = range_begin
            @end = range_end
            @inclusive = inclusive
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
    end
end
