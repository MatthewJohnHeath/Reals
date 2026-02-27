module Reals
import Lazy

abstract type SignedHalfBit end

abstract type SignedBit <: SignedHalfBit end
struct One <: SignedBit end
struct Zero <: SignedBit end
struct NegOne <: SignedBit end

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

struct Half <: SignedHalfBit end
struct NegHalf <: SignedHalfBit end

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

small_real_zero = Lazy.@lazy Zero():small_real_zero
small_real_one = Lazy.@lazy One():small_real_one


end # module Reals
