---
-layout: post
-date:   2018-12-30 00:00:35 +0900
-tags: Object Detection
---

 YOLO는 한때 SSD, Faster R-CNN과 함께 object detection에 대표적으로 사용되던 모델이다. object detection이란 이미지에서 물체(object)를 인식하여 해당 물체를 찾아 사각형으로 크기, 위치를 표현하는 것이다(detection). 이는 여러 분야에 사용할 수 있다. 예를들어 

 - 카메라 앱에서 사람의 손과 얼굴을 인식
 - 자율주행 자동차에서 도로의 자동차와 자전거, 사람, 신호등 등을 인식
 - 방범 cctv에서 사람을 인식 
 등등 다양한 분야에 사용된다.

<!--more-->

---

## Introduction 

 YOLO가 나올당시에는 DPM, Faster R-CNN이 나온 상태였다. 둘다 처리속도가 너무 느려 RealTime으로 사용하기에는 한참 무리가 있었다. 그 이유는 Faster R-CNN의 Two-Stage 방식의 detection과정 때문인데 Two-Stage란 box를 추출해내는 network가 따로 있고, 이 Box를 recognition하는 network가 분리되어 있는 것을 말한다. 반대로 One-Stage는 Grid cell방식의 고정된 개수의 box가 만들어지는 detection과 recognition하는 network가 합쳐진 것을 말한다. 

 오늘 우리가 알아볼 YOLO는 One-Stage방식의 Detector로서 FPS가 30을 넘고 mAP또한 그 당시 준수한 성적을 낸 SOTA모델이었다. 

## Unified Detection
 
 YOLO의 Detecting mechanism을 알아보기 전에, YOLO가 RealTime Detector로 사용하기 위해 노력한 architecture의 노력을 보자.

### Arhcitecture 

 <p align="center">
 <img src="/assets/images/YOLOv1/Figure1.png" width="700" height="400"/>
 </p>

 24개의 convolutional layer를 가지고 있으며, 좀더 경량화한 버전인 YOLO tiny는 9개의 convolutional layer를 쌓았다. 그리고 2개의 fully connected layer를 추가했으며 마지막 output(feature map)은 YOLO의 detecting에 사용된다.

 ImageNet classification에 사용된 GoogLeNet에서 영감을 받아, _3x3 convolution layer뒤에 따라오는 1x1 convolutional dimension reduction layer를 둬 연산량을 감소시켰다._

 모델의 input size는 448x448로서 resize하여 들어간다. 

### Detector 

 **YOLO는 feature map을 S * S size에 (B * 5 + C) depth로 reshape한다. S * S개로 나눠진 하나의 칸을 grid cell이라고 부르며 하나의 cell에서는 B개 만큼의 Box, cell의 C개 class를 예측한다.** 
 
 <p align="center">
 <img src="/assets/images/YOLOv1/Figure2.png" width="600" height="350"/>
 </p>

- Box는 (x, y, w, h, confidence)로 구성된다. 
    - x, y는 0 ~ 1사이의 값이며 grid cell안의 x, y 중심 좌표를 나타낸다. 
    - w, h는 0 ~ 1사이의 값이며 이미지 전체 중에서 box의 크기를 나타낸다. 
    - confidence는 각 box가 얼마나 object가 있을 것 인가를 예측하는 값인데, box안에 object가 있을 확률 * 예측한 box와 실제 box가 겹칠 확률로 정의한다. 구조적으로 condition을 준 것은 아니고 저자가 그냥 _정의_ 만 했다. 
- grid cell은 C개의 class probabilities를 예측한다. 실제로 predict를 할때는 각 box의 confidence * class probabilities이다. 
- YOLO를 Pascal VOC로 검증할때에는 S=7, B=2, C=20으로 7 * 7 * 30 개의 tensor를 예측했다.

## Training

 convolutional layer는 ImageNet 1000-class dataset으로 pretrain시켰다. 
 마지막 layer의 linear activation function으로 leaky ReLU를 사용하고, loss metric으로서 기본적으로 sum-squared error를 사용했다. 

 일반적으로 이미지에서 object가 있는 부분보다 없는 부분이 훨씬 많은데, 그래서 cell의 confidnce가 0으로 압도된다. 이는 모델이 일찍 안정적으로 수렴하는 것을 방해한다. 

 이를 처방하기 위해 coordinate loss에는 가중치 $\lambda_{coord}$와 object가 없는 cell에는 $\lambda_{noobj}$를 준다. 
 yolo는 여러개의 box를 예측하기 때문에 한 object에 여러 box가 반응하는 경우가 있다. 이때는 box중 가장 높은 IOU를 가진 box를 사용한다.

### Loss Function

 $$
 \begin{align}
    & \lambda{coord} \sum^{S^2}_{i=0} \sum^B_{j=0} \mathbb{1}^{obj}_{ij} \big[ (x_i - \hat x_i)^2 + (y_i - \hat y_i)^2\big] \\
    & \lambda{coord} \sum^{S^2}_{i=0} \sum^B_{j=0} \mathbb{1}^{obj}_{ij} \big[ (\sqrt{w_i} - \sqrt{\hat w_i})^2 + (\sqrt{h_i} - \sqrt{\hat h_i})^2\big] \\
    & \sum^{S^2}_{i=0} \sum^B_{j=0} \mathbb{1}^{obj}_{ij} \big(C_i - \hat C_i\big)^2 \\
    & \lambda{noobj} \sum^{S^2}_{i=0} \sum^B_{j=0} \mathbb{1}^{noobj}_{ij} \big(C_i - \hat C_i\big)^2 \\
    & \sum^{S^2}_{i=0} \mathbb{1}^{obj}_i \sum_{c \in classes} (p_i(c) - \hat p_i(c))^2 
 \end{align}
 $$

 $1^{obj}_{ij}$ 는 obj가 존재하는 i번째 grid cell에 j번째 box를 나타내고, noobj는 obj가 존재하지 않는 경우를 말한다. 
 - Eqn. 1은 일반적인 coordinate loss이다. 
 - Eqn. 2를 보면 w, h에 root가 씌워진 것을 볼수 있는데, 이는 w,h값이 클경우 small box일 때보다 loss가 크게 생성되어 더 critical할 수 있다. 그래서 root를 통해 small devaitaion을 가지도록 유도한다. 
 - Eqn. 3에서는 confidence score에 대한 loss이다.
 - Eqn. 4는 3과 마찬가지 이지만 object가 없는 cell에 대해 loss를 구하였으며, 추가적인 가중치를 뒀다. 
 - Eqn. 5의 p()는 probability이다,
 - 논문에서는 $\lambda_{coord}=5$, $\lambda_{noobj}=0.5$로 설정하였다. 

## Limitation, Experiment

 YOLOv1는 box별로 class를 예측하는 것이 아니라, cell의 class를 예측하기 때문에 cell안에 두개 이상의 물체가 있을 경우는 올바른 에측이 불가능하다.  
 Box의 개수가 제한적이여서 cell안에 여려물체가 동시에 있을 때, 예측할 수 있는 개수가 제한적이다.

 Confidence를 분리함으로서 background filtering을 훨씬 잘한다고 한다.

 <p align="center">
 <img src="/assets/images/YOLOv1/Figure3.png" width="800" height="450"/>
 </p>
 
 성능은 Faster R-CNN에 비해 조금 떨어지지만 FPS는 45가 넘어 RealTime Detector로서 더 적합하겠다. 

