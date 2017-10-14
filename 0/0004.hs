isPalindromic = (\s -> s == reverse s).show
main = print $ maximum [c | a <- [100..999], b<-[a..999], let c=a*b, isPalindromic c]