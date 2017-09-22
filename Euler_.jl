__precompile__()

export is_palindromic

function is_palindromic(n)
    ds = digits(n)
    for i in 1:length(ds)÷2
        if ds[i] != ds[end+1-i]
            return false
        end
    end

    true
end
