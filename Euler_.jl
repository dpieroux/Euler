__precompile__()

export is_palindromic

import Base

function is_palindromic(n)
    ds = digits(n)
    for i in 1:length(ds)÷2
        if ds[i] != ds[end+1-i]
            return false
        end
    end

    true
end

function insertSorted!(list, item)
    index = searchsortedfirst(list, item)
    insert!(list, index, item)
end

type NIter{T <: Integer}
    from :: T
    inc  :: T
end

function naturals(from::T=1, inc::T=1) where T <: Integer
    NIter{T}(from, inc)
end

Base.start(it::NIter{T}) where T <: Integer = it.from

function Base.next(it::NIter{T}, s::T) where T <: Integer
    (s,s+it.inc)
end

Base.done(::NIter, ::T) where T <: Integer = false
