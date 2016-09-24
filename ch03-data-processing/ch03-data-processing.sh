
# 자료를 다운받는다
curl https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data > adult.data

# 첫 열줄을 보여준다.
head adult.data 

# 마지막 10줄을 보여준다.
tail adult.data

# 첫 5줄을 다른 파일에 저장한다.
head -5 adult.data > adult.data.small
cat adult.data.small

# 콤마 열 분리문자를 탭으로 바꾼후 다른 파일에 저장한다.
tr "," "\t" < adult.data.small > adult.data.small.tab
cat adult.data.small.tab

# 자료 길이가 몇줄인지 보여준다. (32562)
wc -l adult.data

# 간단히 줄이기
head -5 adult.data | tr "," "\t" > adult.data.small.tab

# 직업군(work class)의 도수분포 
$ cut -d ',' -f 2 < adult.data | sort | uniq -c | sort -nr
