#1
#음식탐방 선형회귀분석 정확도 높이기
num=c(26,297,181,449,57,28,71,26,136)
m=c(8,57,80,53,8,6,12,7,27)
data_a=data.frame(사례수=num,음식미식탐방=m)
data_a
plot(data_a)
model1=lm(음식미식탐방~사례수, data=data_a)
summary(model1)
#정확도가 55%임 --> 회귀분석의 정확도를 높이기 위한 과정이 필요
plot(model1)

#각 변수에 log를 취함--> Data Transformation
plot(num, m, log="xy")

#log를 취해 scale을 줄임
model=lm(log(m)~log(num))
summary(model)
#정확도가 86%높아짐

par(mfrow=c(2,2), mar=c(2,3,1.5,0.5))
plot(model)

#2
#치료 및 미용 서비스 체험 정확도 높이기
s=c(0,4,1,5,0,0,0,3,1)
data_b=data.frame(사례수=num,치료및미용서비스체험=s)
data_b
plot(data_b)
model_a=lm(치료및미용서비스체험~사례수, data=data_b)
summary(model_a)
#정확도가 65% --> 회귀분석의 정확도를 높이기 위한 과정이 필요
plot(model_a)

#plot을 보면 데이터 하나를 제외하고는 선형성에 크게 문제가 있어보이지 않음
#선형성에 문제를 주는 outlier을 확인하기위해 outliertest를 진행함

library(car)
outlierTest(model_a)
#8번 데이터가 p값과 Bonferroni값이 0.05보다 작음
#--> 8번 데이터가 outlier임을 알 수 있음

Num=num[-8]
S=s[-8]
model_b=lm(S~Num)
summary(model_b)
#정확도가 94%로 높아짐
plot(Num,S)


par(mfrow=c(2,2), mar=c(2,3,1.5,0.5))
plot(model_b)