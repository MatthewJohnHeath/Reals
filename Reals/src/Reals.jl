module Reals
import Lazy

abstract type SignedHalfBit end

abstract type SignedBit<:SignedHalfBit end
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

average(::T, ::T) where T<:SignedBit = T()
average(::One, ::NegOne) = Zero()
average(::NegOne, ::One) = Zero()
average(::Zero, ::One) = Half()
average(::One, ::Zero) = Half()
average(::Zero, ::NegOne) = NegHalf()
average(::NegOne, ::Zero) = NegHalf()

small_real_zero = Lazy.@lazy Zero():small_real_zero
small_real_one = Lazy.@lazy One():small_real_one


end # module Reals
