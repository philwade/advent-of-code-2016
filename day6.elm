module Day6 exposing(..)

import Html exposing(text, div)
import Dict exposing(Dict)
import Day4 exposing(listFold)

{-
--- Day 6: Signals and Noise ---

Something is jamming your communications with Santa. Fortunately, your signal is only partially jammed, and protocol in situations like this is to switch to a simple repetition code to get the message through.

In this model, the same message is sent repeatedly. You've recorded the repeating message signal (your puzzle input), but the data seems quite corrupted - almost too badly to recover. Almost.

All you need to do is figure out which character is most frequent for each position. For example, suppose you had recorded the following messages:

eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar
The most common character in the first column is e; in the second, a; in the third, s, and so on. Combining these characters returns the error-corrected message, easter.

Given the recording in your puzzle input, what is the error-corrected version of the message being sent?
-}

partOneOutput = listsFromInput input
                |> dictsFromLists
                |> List.map sortDict
                |> List.reverse
                |> List.map listToLetter
                |> String.join ""

{-
--- Part Two ---

Of course, that would be the message - if you hadn't agreed to use a modified repetition code instead.

In this modified code, the sender instead transmits what looks like random data, but for each character, the character they actually want to send is slightly less likely than the others. Even after signal-jamming noise, you can look at the letter distributions in each column and choose the least common letter to reconstruct the original message.

In the above example, the least common character in the first column is a; in the second, d, and so on. Repeating this process for the remaining characters produces the original message, advent.

Given the recording in your puzzle input and this new decoding methodology, what is the original message that Santa is trying to send?

-}
partTwoOutput = listsFromInput input
                |> dictsFromLists
                |> List.map sortDict
                |> List.map listToLetter
                |> String.join ""

main = div []
       [ text (toString partOneOutput)
       ]


listToLetter : List (String, Int) -> String
listToLetter ls = case List.head ls of
    Just (k, v) -> k
    Nothing -> "SADFACE"

-- sort by counts associated with each letter
sortDict : Dict String Int -> List (String, Int)
sortDict dict = Dict.toList dict |> List.sortBy (\(k, v) -> v)


listsFromInput : String -> List (List String)
listsFromInput input = input |> String.split "\n" |> List.map (String.split "")

-- create a set of dicts given a number
baseDicts : Int -> List (Dict String Int)
baseDicts int = List.repeat int (Dict.fromList [])

dictsFromLists : List (List String) -> List (Dict String Int)
dictsFromLists inputLists =
    case List.head inputLists of
        Just first ->
            let
                baseLength = List.length first
                dicts = baseDicts baseLength
            in
                List.foldl (\line subDicts -> List.map2 listFold line subDicts) dicts inputLists
        Nothing -> [Dict.fromList []]

sampleInput = """eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar"""

input = """vlmbsivg
wjskjzff
hmcodblh
nvohhghm
xainmzpz
frkckyrg
zqdmvmee
sbsqzlju
eqdnsvqi
xdmyqmxk
gchzzkci
unjukwld
fmnpwrnf
huvyqxkt
uxekmktg
pdzxhbfa
khlkjqhk
ravznzpy
btpqmfzt
tcnpghzn
yuplhxzr
ablaihve
gjdehjcu
dvmkpskx
lukbiacr
ttrzcksg
ecfehhzq
octwtlsc
colisfrx
svtipfzv
kaffxell
rsmdpuic
viaidaip
drbezwvb
tyhnjseu
weuopaqc
igmqdwiz
gerzveig
bnqltmlo
jgwcxybe
iaabqazu
bgflkfyt
boznsgas
agiyktkz
ivvnbvuv
tospvglb
vxfhcvkz
kshbpxws
uejxthhb
vyffdirv
ibfywjob
cpzlamql
pyzsyqvq
yrqdolxy
svyihgcn
vupxjagi
cncrrcuv
ydmvwcje
yrpqbwhn
jrgsqtpl
axzbhwaw
ggsjjmvb
qtdsykjo
cnjmerzo
lzqwtxms
yhhodrpp
ryiwahsd
kggpmwvl
lextjotq
tlxjbxef
fygwhizi
moomppou
ezavglhp
omylulyf
ahbasrtz
ccbblybl
ueyudhva
blrewxyn
ytdwdavt
rgpakvlz
bmlycpbm
ewbvxsrm
hcukzfwq
rkkixnbu
masthktz
soolsxwh
zwszloqc
wjizzawg
ojcfqylq
nnzmlcyh
pvpmbtxx
assogumc
quorbsbu
uywftned
jneaeojp
mdvwycom
cpoqljak
dgxinijd
oknnyivd
uclfujcu
tglfuqym
xixpvogp
ywpxxlzu
tzaooymi
srjmonav
pnfugqmd
lryrknay
ijyxsjyr
nscjqibj
xfbyavdq
qoqcxyek
txscvcir
rbbpybgj
tkhqbyup
gmqcfvgi
itftwfkt
uzjbjwgw
rmfoqkvx
qtnlthgs
szdpoddo
qunqnglj
vpyzxigu
yvgaspgn
ugzomjyi
xehwrqqa
gerohqzw
svumyqwg
mqyztgha
kxaoylrd
qlrivitr
mqyqiewf
jbmnazei
mxlxbhwt
blwoptvb
psdatjoo
ygetkaoq
wygpqzdo
hlclzfmf
jpbvivrw
pmukraot
swbpgume
dvkkfjph
vrodfocs
zufhfmqm
djwpfova
fkmbbzho
wecxogkb
nhkrcpkp
vjeqcaaf
xrqarugv
fwmcwuok
epbkndfk
zwpgujke
modqokkj
ikewchsd
jwrhzjlb
smlbnbef
mxbbtgqb
viwdgsdx
goqsuvzj
rbroiwxp
zdiptzal
ybfdenoy
oszdisbg
nvfjluan
lqsbzkzw
psuefwbt
yallonfl
ijiwdpgh
gglvyutq
vqoydnjd
qcryrkrs
aivepbza
ylhvocnp
itvgswxy
bztkautf
ptrfhgug
jxrrgihp
pemkoxua
gisyyymp
lojsmpxa
eqfowfiy
fjgiccrd
hbxkngmy
drqthobt
ikyiobsj
cfwzjnns
krhtlqjb
ypahmuqg
cckkvsls
cvtpertw
lokmfmqj
eicgeaew
cuenpdcs
dztpufap
qrwculkl
ynzjrfps
mucvocas
dhorhvqw
ldidkudb
xxpbrirx
ksudlgat
giljlkxr
jhyqnccn
pmcscebv
cwidmjza
rqqirtgy
zioenwys
xbjruyyl
asptiznh
jqpjaenx
lugmurst
kmerqmnn
dxyndveo
uuqchrez
upcfeanl
sercqwtv
gbyfvzic
zlozeixc
cqtrvust
qdjntgwm
xiqmxxpw
gksowkih
pltnircb
pvoulbsn
hjxqhgjg
tcewvoyn
apjfkhzx
mfnuzwxz
ovchtwej
efqrkewr
jwgxupik
gcwelfie
ammaagcn
nyhkwztj
zakrmiqp
jjrdohih
rnwxdbge
zeobprqj
zwwtpnsx
mdnygzxh
nkfluipm
efwgmsfk
jlnztudt
medtzrwv
wclfyxbb
jjetrasx
wmwuhaar
ijwzhaoh
xdlfrrnn
dvxecbjt
vwihvqzh
kgpagxiw
jvixdjuc
gfiweyxg
xejknscb
hxheizpd
dubgkfgr
wkxidfgc
tzxylcho
qvububkp
erlnhqov
weomsswt
rfpoxknw
itnvsxdi
cbnocofl
ldadkmsf
pwhhklgc
itudhryg
vlvtoljp
hjggmitm
mmzawuak
quigzvkq
rsmiptim
tsqytvda
pmprbqfm
eppsdefz
adcqcsvc
lwiyaddu
wwqpjubg
jilsipuz
wzrxuskh
laraxaqd
epuyflmt
mdclmqun
hkjrdlxu
tcilebni
rkrekccq
aazdcsnc
ngijyoqs
bymuwqsv
lsanabvd
ylnektqm
fthtunja
vvfmqqbe
shbajllw
xgdgwmdc
hrzmxpif
nskrdynl
ineiworp
nlgfboek
yrsztmql
dnrewurn
uwtdmpru
eygvelve
tfitkluc
iqpxtxpw
hztyqkrl
xastoukn
djbllswj
kcnpmzsu
kepgfdit
okvnsjlp
fhjqkxxt
cnoewgjm
uhyjofee
knpdrbij
rreiagox
hxkhodce
zmaxocze
wtpaqndq
jvcagcvr
uhkmigmp
xadnxjhe
uiaplibp
otkxrzix
yzedogvy
ekfnffhq
fakkmnxr
bztnaqpo
tmyttvbn
univukhd
caujacsj
flblfclw
czfogrtr
inxubopi
afwfgcpb
mycsrthv
kdwsxnay
koujdeqg
hirhfotk
wkxxvthj
xsosgzwi
bxilicsn
hniuyqyz
lwbgmzwc
sncerhsu
fuhbmtfk
kaftqhuy
pqvglqfj
bghwspjz
twijseky
dpfmjhis
narvsefe
eraymofq
qiopspmb
taehddxo
zttknqxx
thgkqpfz
dgufruwc
bunvatbf
xonactvk
axjukwjn
opbjnbxx
fwvipkwm
etxwpjaj
hqdclxoq
fsnugidt
zmbpypye
hoqsimwa
waghzrcr
njymehaz
zknxntkj
ntjtbato
rqhupkqa
jnjxdtdf
eubzelni
astkohuv
ptfvacsg
bzjhupen
ijeukuux
abiyvjke
psbjwvcf
sucqapsu
axbjujok
oxvdokyb
olsyowug
nitvqkcl
njswglrr
ovheurur
ayedpsqo
vvclemhs
wjrfsvyk
fdhcqdru
qrkvvfay
kuhlridl
yxnrykeh
ctkznbzx
pfaojeww
lobsvohy
wutlpeak
nhppjsax
yihmsdmd
iszriptf
qdnhytkn
vooshrhs
wzogjdin
tkseeclg
jxzgiyxq
osefrqid
ixgfrbhq
zkggjudf
dyxgeond
rpwplncj
ndpxtrgc
ubuheera
migvasdi
acgygdxb
ccxcimgx
lvdpvhdr
ghvhyfmz
dqtheasu
ebjozdgp
tluznvkr
qhvzqrzm
qbjpiipl
bplqnbti
kizmxvno
uobfxakf
syllmscz
gghcqygd
snkwjojf
vdcvdpno
qussueej
zmlxuulr
htqbwjve
ixynqepo
ovaelpay
eywkbzsc
qwoqmtig
prkxadem
jfsscvhm
hqslphpy
bfiiyruu
fmkizdns
vpsisoaf
couzrtsv
wycwxxlh
upvcymri
hqlqfvbm
wzaeftmu
boxdzesc
lmcuplex
xyuifcev
fqgvggmq
seykzeql
fewgnzoq
qjmsxyto
ebckyjih
jzrbwgjj
vwojkmpc
sqdqaffc
yukfwymc
yjptcsmz
tencuncy
rbtxxpuv
yiffiway
eefyzfqb
moxjnvoq
wijspyho
fdtsjins
rxqcbsgd
oamredol
baeulkgv
rggwhexz
qfkkitpv
uexaisra
cqwjlwxi
tjavidpj
efbavnsa
uxnnkruo
wriebzoz
wexcdrht
hzmyocox
qseogxfr
fflfanfi
vpaiudnq
olvuxneh
fcbsytua
sfudzkhx
xghrupyg
aeclhwom
byobwacz
rsditwjr
lvvqfcup
lflatigg
kuojuttb
aouuzdnw
klbwzjlw
zkambbfg
xdevxqbr
zqdlejop
vfhbkyik
zeyujeql
skvhtden
ddwgfxck
lnrqrbkk
rtnzbizt
uaxpghyx
asybsmyx
djdcnnpx
ykwhiyjk
zhvodxrh
bmzhumtw
aoutwtdv
mjadhrvo
tkugwiep
ystjfami
bfqqcmtb
bbvmzwdo
hgufdhpb
hhmamhwy
gpasfwju
zhpxvhjf
vzqggwtj
kbjrhlrm
inhayugt
fvspcluq
grxbmozt
muohwoyd
nigtpaty
ozzvzejw
nyyngjme
jmtoeyly
qyxhdpaq
iowatjzd
iszcablv
ocgavedu
xbnzrfqv
swzzzzwu
stqfxgoo
xpmubjxr
katkezfz
pudaitbg
ybarabzd
mpvqbzsr
rhinjfpm
snevfkqk
pwwccyyu
ocljrwko
dxdryyvq
rpqgkeol
wvqbnkwc
hgnwlgua
mcqmblme
fhzgsdch
kdxugdug
oltmtill
sbduaius
cqauyawa
xfznpmvh
dmivepyp
dgsngwbw
mbgsryyh
aojpsuvb
igvrbclq
lduxmyiy
dhomqxlb
myszbzrf
kbadlpoy
ftnumgci
xwzbhyfz
lzutwqtv
gimhovca
zfudpuny
nqkrsatc
uldebzzq
olzoesdw
nchgfvll
oopwvqlh
jhsbnnjw
siyclmze
frhzjbnv
nanvvjht
wtucekgs
xiyyglkr
zkakjomw
fdeeoopf
tlctovbh
cnqwlmkh
zreoslve
lcexflbp
erkaxhyf
uflifmci
byfovhfn
pmlsjebs
rmdncvno
jzxsteml
ehromnts
wwrgqdmv
gydqxmbb
kjjnrsyf
okzknoil
pqmjbnhz
rbgydndw
jfmclxjm
bfotvtud
mftuqeei
lhchcrja
czperrwe
hyymnbfh
gtmctpcn
ooteayaj
ipxmxdrg
lxljcbmk
kexfnhaz
zwvwqxwd
ccriznpe
gadlykes
nuagvrrv
xgvdaxfk
gnkzxirj
vdmrccgf
vhgjcski
qpdhafox
oijrjuwm
qxtyktrs
mkyalzfa
tnmunnod
ukjvbgjs
cywsgmfc
uqkzjgbn
ppjifbhj
gpwlnflu
alabzvvi
vtygcqph
ezxtvdqo
hzeejxxe
aavwflnw
nrfqefxa
dsgysuua
pikwyfym
nesdcxdi
ghqlicny
jcvekqdr
cyeiicma
slfxqsdt
dcrcwoek
qdmtpdxr
vlzjoqfm"""
