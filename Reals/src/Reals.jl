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
    Lazy.@lazy Lazy.isempty(xs) ? List() : 
    Lazy.first(xs) : concat_all(Lazy.tail(xs))
end

Base.:-(::One) = NegOne()
Base.:-(::Zero) = Zero()
Base.:-(::NegOne) = One()
"""
    Base.:-(x::Lazy.LazyList)

TBW
"""
Base.:-(x::Lazy.LazyList) =  Lazy.map(-, x)


Base.:-(::Half) = NegHalf()
Base.:-(::NegHalf) = Half()

bit_average(first::SignedBit, second::SignedBit) = bit_average(second, first) # commutative
bit_average(::T, ::T) where T<:SignedBit = T()
bit_average(::NegOne, ::One) = Zero()
bit_average(::Zero, ::One) = Half()
bit_average(::NegOne, ::Zero) = NegHalf()

function average_half_bits(xs::Lazy.List, ys::Lazy.List)
    Lazy.@lazy Lazy.isempty(xs) ?  ys : Lazy.isempty(ys) ? xs :
    bit_average(Lazy.first(xs), Lazy.first(ys)) : average_half_bits(Lazy.tail(xs), Lazy.tail(ys))
end

Base.:+(::SignedBit, ::SignedHalfBit) = error("Out of range on signed-bit addition")
Base.:+(::Zero, h::Zero) = h
Base.:+(::Zero, h::SignedHalfBit) = h
Base.:+(b::SignedBit, ::Zero) = b
Base.:+(::One, ::NegOne) = Zero()
Base.:+(::NegOne, ::One) = Zero()
Base.:+(::One, ::NegHalf) = Half()
Base.:+(::NegOne, ::Half) = NegHalf()

direction(b::SignedBit) = b
direction(::Half) = One()
direction(::NegHalf) = NegOne()

as_bit_list(h::SignedHalfBit) = List(Zero(), direction(h))
as_bit_list(b::SignedBit) = List(b)

bit_and_carry(target::SignedHalfBit, next::SignedHalfBit) = bit_and_carry(target, direction(next))
bit_and_carry(target::SignedBit, ::SignedHalfBit) = (target, Zero())
bit_and_carry(::Half,::SignedBit) = (Zero(), One())
bit_and_carry(::Half, ::One) = (One(), NegOne())
bit_and_carry(::NegHalf, ::SignedBit) = (Zero(), NegOne())
bit_and_carry(::NegHalf, ::NegOne) = (NegOne(), One())

function dehalf_bits(xs::Lazy.List, carry::SignedBit = Zero())
    Lazy.isempty(xs) && return Lazy.list()  
    target = carry + Lazy.first(xs)
    tail = Lazy.tail(xs)
    Lazy.isempty(tail) && return as_bit_list(target)
    (next, carry) = bit_and_carry(target, Lazy.first(tail))
    return Lazy.@lazy next : dehalf_bits(tail, carry)
end

average(xs::Lazy.List, ys::Lazy.List) = dehalf_bits(average_half_bits(xs, ys))
zero_front_equiv(::Zero, x::SignedBit) = (Zero(), x)
zero_front_equiv(::One, ::NegOne) = (Zero(), One())
unsafe_add(xs::Lazy.List, ys::Lazy.List) = Lazy.tail(average(xs, ys))

Base.:*(::Zero, ::Lazy.List) = Lazy.list()
Base.:*(::One, xs::Lazy.List) = xs
Base.:*(::NegOne, xs::Lazy.List) = -xs

function product_summands(xs::Lazy.List, ys::Lazy.List, right_shift = 1)
    Lazy.isempty(xs) && return Lazy.list()
    head = Lazy.first(xs)
    tail = Lazy.tail(xs)
    unshifted_summand = head * ys
    summand = Lazy.repeat(Zero(), right_shift) : unshifted_summand
    return Lazy.@lazy summand : product_summands(tail, ys, right_shift + 1)
end

zeroes = Lazy.@lazy Zero() : zeroes
one = Lazy.@lazy One() : one
long_half = Zero():one

end # module Reals
