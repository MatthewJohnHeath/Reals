module Reals
import Lazy

abstract type SmallHalfInteger end
abstract type SignedHalfBit <: SmallHalfInteger end

abstract type SignedBit <: SignedHalfBit end

struct ThreeHalves <: SmallHalfInteger end
struct One <: SignedBit end
struct Half <: SignedHalfBit end
struct Zero <: SignedBit end
struct NegHalf <: SignedHalfBit end
struct NegOne <: SignedBit end
struct NegThreeHalves <: SmallHalfInteger end

function concat_all(xs::Lazy.List)
    Lazy.@lazy Lazy.isempty(xs) ? Lazy.list() : 
    Lazy.first(xs) : concat_all(Lazy.tail(xs))
end

Base.:-(::One) = NegOne()
Base.:-(::Half) = NegHalf()
Base.:-(::Zero) = Zero()
Base.:-(::NegOne) = One()
Base.:-(::NegHalf) = Half()
"""
    Base.:-(x::Lazy.LazyList)

TBW
"""
Base.:-(x::Lazy.LazyList) =  Lazy.map(-, x)


bit_average(first::SignedBit, second::SignedBit) = bit_average(second, first) # commutative
bit_average(::T, ::T) where T<:SignedBit = T()
bit_average(::NegOne, ::One) = Zero()
bit_average(::Zero, ::One) = Half()
bit_average(::NegOne, ::Zero) = NegHalf()

head(xs::Lazy.List) = Lazy.isempty(xs) ?  Zero() : Lazy.first(xs)
tail(xs::Lazy.List) = Lazy.isempty(xs) ? Lazy.list() : Lazy.tail(xs)

average_half_bits(xs::Lazy.List, ys::Lazy.List) = Lazy.@lazy bit_average(head(xs), head(ys)) : average_half_bits(tail(xs), tail(ys))


Base.:+(::Zero, h::Zero) = h
Base.:+(::Zero, h::SignedHalfBit) = h
Base.:+(b::SignedBit, ::Zero) = b
Base.:+(::One, ::One) = error("Out of range of SmallHalfInteger")
Base.:+(::One, ::NegOne) = Zero()
Base.:+(::One, ::Half) = ThreeHalves()
Base.:+(::One, ::NegHalf) = Half()
Base.:+(::NegOne, x::SignedHalfBit) = - (One() + (-x))


bit_and_carry(target::SignedBit, ::SignedHalfBit) = (target, Zero())
bit_and_carry(::ThreeHalves, ::SignedHalfBit) = (One(), One())
bit_and_carry(::Half, ::SignedHalfBit) = (Zero(), One())
bit_and_carry(::Half, ::One) = (One(), NegOne())
bit_and_carry(::NegHalf, ::SignedHalfBit) = (Zero(), NegOne())
bit_and_carry(::NegHalf, ::NegOne) = (NegOne(), One())
bit_and_carry(::NegThreeHalves, ::SignedHalfBit) = (NegOne(), NegOne())

function dehalf_bits(xs::Lazy.List, carry::SignedBit = Zero()) 
    target = carry + head(xs)
    rest = tail(xs)
    (bit, carry) = bit_and_carry(target, head(rest))
    return Lazy.@lazy bit : dehalf_bits(rest, carry)
end

average(xs::Lazy.List, ys::Lazy.List) = dehalf_bits(average_half_bits(xs, ys))

unsafe_add(xs::Lazy.List, ys::Lazy.List) = tail(average(xs, ys))

zeroes = Lazy.constantly(Zero())

Base.:*(::Zero, ::Lazy.List) = zeroes
Base.:*(::One, xs::Lazy.List) = xs
Base.:*(::NegOne, xs::Lazy.List) = -xs

function product_loop(xs::Lazy.List, ys::Lazy.List, acc::Lazy.List)
    partial_sum = unsafe_add(acc, 0 : head(xs) * ys) 
    return Lazy.@lazy head(partial_sum) : product_loop(tail(xs) , ys, tail(partial_sum))
end



end # module Reals
