#1
#음식탐방 선형회귀분석 정확도 높이기
num=c(117,90,54,86,102,64,65,116,157,156,236,155)
m=c(22,20,11,17,18,13,12,12,21,23,59,53)
data_a=data.frame(사례수=num,음식미식탐방=m)
data_a
plot(data_a)
model1=lm(음식미식탐방~사례수, data=data_a)
summary(model1)
#정확도가 68%임 --> 회귀분석의 정확도를 높이기 위한 과정이 필요
plot(model1)

boxplot(data_a$음식미식탐방)
#boxplot을 그려보았을 때, outlier로 보이는 값들이 있음
#하지만 데이터의 수가 작기 때문에 outlier을 제거하는 방법은 옳지 않다고 판단
#따라서 log를 취해 scale을 줄이는 방법을 사용함

plot(num, m, log="y")

#log를 취해 scale을 줄임
model=lm(log(m)~num)
summary(model)
#정확도가 72%높아짐

par(mfrow=c(2,2), mar=c(2,3,1.5,0.5))
plot(model)