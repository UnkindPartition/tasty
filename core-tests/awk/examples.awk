42
"Hello \"there\"\n"
NF
$NF $1 $2 $3
(2 + 3) + 4
"abc" 4 "def"
1 (2 + - 3) - 4 - (5 - 7)
- 3 1
! 3 1
(1 < 2 && 1 > 2 || 1 <= 2 && 1 >= 2) == (1 != 2)
1 ? 2 ? 3 : 4 : 5 ? 6 : 7
$3 ~ /fo\/o/ || $2 !~ /ba\\r/
/foo/ /bar/
/one/
/one\/two\/three/
/e\/t/
$1 ~ /one/
$1 ~ /two/
$1 !~ /two/
$1 !~ /o/
length
length()
length($3)
substr("Chicken", 2)
substr("Chicken", 2, 3)
match("Chicken", /hick/)
match("Chicken", /hicky/)
toupper("Chicken")
tolower("Chicken")
