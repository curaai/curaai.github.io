# On Calibration of Modern Neural Networks 

 딥러닝을 사용한 분류 문제에서는 흔히들 softmax를 사용해 가장 큰 값(probability)이 나온 것을 해당 class라 판단하고 그때의 확률 값을 confidence로 사용한다. confidence가 낮게 나오면, 해당 data에 대해 model이 학습이 더 필요하거나, 잘못 됐다고 해석할 수 있다. 가장 높은 confidence의 class를 predict class로 선정하듯 <u>confidence는 모델을 해석하는데 아주 유용하게 쓰인다.</u>

### Annotation 

 classification neural network $h$는 input $ x \in X$, $K$개의 class를 가진 label $y \in Y = {1, \dots, K}$에 대해 $h(x) = (\hat y, \hat p)$, class prediction $\hat y$ 와 $\hat y$에 해당하는 probability confidence $\hat p$ 를 예측한다. **confidence $\hat p$에 대해 설명하자면, model이 100개를 predict하고, confidence가 전부 0.8일 경우 80개가 맞게 분류되었을 것이라고 예상 할 수 있다.** *하지만 confidence에 대해 확실한 measure를 원한다.*

한정된 sample에서 1을 $M$개의 bin으로 나눈다. $m$번째 bin에는 $I_m =  (\frac{m-1}{M}, \frac{m}{M}]$ 에 해당하는 prediction result index가 들어가 있다. 이때 $m$번째 bin의 accuracy와 confidence는 다음과 같다.
$$
acc(B_m) = \frac{1}{|B_m|} \sum_{i \in B_m} \bold{1}(\hat{y}_i = y_i), \ \ \ \ \ \ \ \ conf(B_m) = \frac{1}{|B_m|} \sum_{i \in B_m} \hat{p}_i
$$

- **$m$번째 bin의 $accuracy$는 label과 prediction 같은 경우 / bin의 size이다**
- **$m$번째 bin의 $confidence$는 bin의 prediction confidence의 평균이다.**

$acc(B_m) = conf(B_m)$인 경우를 well/perpectly calibrated라 한다.

### Calibration Error

calibration이 얼마나 잘 되었는지를 ECE(Expected Calibration Error), MCE(Maximum Calibration Error)로 측정한다. 

첫번째로 **ECE는 accuracy와 confidence의 distribution의 차이를 측정한다**. $n$은 총 sample 개수이며, 각 bin의 차이값을 gap라고 부른다.
$$
\bold{ECE} = \sum\limits_{m=1}^M \frac{|B_m|}{n} \big| acc(B_m) - conf(B_m) \big|
$$
두번째는 MCE이다. **MCE는 bin중에서 가장 큰 gap을 측정한다**. gap이 high-risk한 경우에 사용하는 measure이다.
$$
\bold{MCE} = \underset{m \in {1, \dots, M}}{\max} |acc(B_m) - conf(B_m)|
$$

## Modern Neural Networks are Mis-Calibrated 

 deep neural network는 많은 곳에서 사용된다. 옛날에 비해 성능도 많이 상향됐으며, model 또한 deep & wide해졌다. 하지만 **model이 커진 만큼 classification error는 감소하였지만, mis-calibrate돼었다.**

![Fig. 1](/Users/curaai00/Documents/Review/calibration/Figure1.png)

![Fig. 2](/Users/curaai00/Documents/Review/calibration/Figure2.png) 

LeNet과 달리 ResNet은 gap이 크다. ResNet의 accuracy, avg confidence line을 봤을 때 <u>우리가 일반적으로 생각하던 confidence 0.5 threshold가 실제로는 안전하지 않을수도 있다.</u> 또한 모델의 mis-calibration에 영향을 주는 요인이 model의 크기에만 있는 것이 아니다. Depth, Width, Batch norm, Weight decay 등 여러 요소가 model의 mis-calibration에 영향을 준다.

![Fig. 3](/Users/curaai00/Documents/Review/calibration/Figure3.png)

training progress에서 test error와 loss를 분석한 그래프이다. epoch 250에서 learning rate가 10배 줄어들며, error와 loss가 함께 줄어들었다. <u>epoch이 300이 넘어서기 시작하면서 NLL이 overfitting을 하기 시작한다. network는 well-modeled probability를 희생하여 classification accuracy를 높이기 위해 학습한다(classification accuracy overfitting).</u> 

## Temperature Scaling

classification accuracy overfitting, mis-calibration을 해결하기 위해 'temperature scaling'이라는 calibration method를 사용한다. 이를 사용하기 위해선 validation set이 필요하다. 

**temperature scaling은 모든 class에 대해 single scalar parameter $T$를 logit vector $z_i$에 나눠주는 방법이다.**
$$
\hat q_i = \underset{k}{\max} \sigma_{softmax}(z_i/T)^{(k)}
$$
$\hat q_i$는 calibrated probability이다. $T$가 무한대로 발산할수록 $\hat q_i$는 1/K에 근사한다. T가 0에 근사할 수록 $\hat q_i$은 1에 가까워질 것이다. $T$는 validation set에서 NLL으로 학습한다. 이 방법의 장점은 <u>parameter $T$는 softmax output의 최대값을 바꾸지 않는다. 기존의 model의 accuracy에 영향을 주지 않는다. 그래서 이미 학습되어 있는 model에 사용할 수도 있다.</u> 

## Experiment

![Fig. 4](/Users/curaai00/Documents/Review/calibration/Figure4.png)

첫번째 그래프는 mis-calibration된 CIFAR-100 Dataset 원래의 ResNet-110 model이다. 2번째는 temperature scaling을 사용한 ResNet이다. 3, 4번째 그래프는 다른 calibration method를 적용한 것으로서 temperature scaling이 가장 좋은 성능을 보여주고 있다. 