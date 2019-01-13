---
-layout: post
-date:   2019-01-13 16:30:35 +0900
-tags: Object Detection
---

 YOLO에 base를 둔, state-of-the-art 성능의 real time detector YOLOv2를 선보인다. 논문의 제목을 보면 알수 있듯이 키워드는 Better, Faster, Stronger가 있다. 차례차례 알아보자 

--- 

## Better 
 
 YOLO를 Fast R-CNN과 비교했을 때 확연히 많은 localization error가 있었다. 거기다가 다른 region proposal-based 방법들과 비교해봐도 많은 recall이 있었다. 그래서 recall을 높이고, localization error를 낮추면서 classification accuracy를 유지하는데 중점을 뒀다. 
 
 Deep learning은 크고, deep한 네트워크를 만드는 것이 trend가 되어 여러 모델을 앙상블하며 성능을 향상시킨다. 하지만 YOLOv2는 여전히 빠르면서 정확한 detector가 되는 것이 목표다. 그래서 network의 크기를 늘이는 것 대신에 network를 간편화하고, representation 성능을 높여 학습을 쉽게 하도록 만든다.
 
### Batch normalization

 다들 사용하는 batch normalization을 사용하여 model의 Regularize륻 도와주고, model의 overfitting없이 dropout을 없앨 수 있었다. 또한 mAP에서 2%만큼의 성능을 높였다. 
 
### Higher Resolution Classifier 

 SOTA detection method는 ImageNet으로 pre-trained classifier를 feature extractor로 사용한다. 하지만 AlexNet을 비롯한 대부분의 classifier는 input image가 256x256보다 작고, Origial YOLO 또한 224x224 resolution classifier network를 사용했었다. 그래서 input resolution을 448x448로 올리고, 올린 resolution으로 classification network를 ImageNet으로 10 epoch을 학습시켰다. higher resolution input에 맞는 network의 filter를 조정하여 mAP가 4% 상승했다. 
 
### Convolution With Anchor Boxes

 YOLOv1는 fully connected layer에서 coordinate를 직접예측했다. YOLOv2는 anchor라는 pre-defined box를 이용한다. anchor는 Faster R-CNN의 Region Proposal Network에서 사용한 방법이지만, 이를 YOLOv2 스타일에 맞게 사용한다.

 <p align="center">
 <img src="/assets/images/YOLOv2/anchor.png" width="400" height="400"/>
 </p>

 Anchor는 위 사진처럼 각 grid cell에서 box가 여러개 있는것처럼 pre-defined box가 있는 것이다. 자세한 내용은 아래에서 다시한번 설명한다.
 
 YOLOv1에서는 (69.5 mAP, Recall of 81%)였지만, anchor를 사용한 YOLOv2는 (69.2mAP, Recall of 88%)이다. 성능은 하락하였지만, v1에서는 98개의 box를 예측했었다면, v2에서는 1000개가 넘는 box를 예측한다. 그래서 recall이 상승하여 모델을 향상시킬 요지가 훨씬 더 많다. 
 
 Anchor를 사용할 때는 두가지 문제점이 있다.
 - hand-picked box: model은 data에 맞게 box를 조정하여 적절히 학습하겠지만, 더 좋은 anchor를 뽑으면, network가 detection을 학습하기에 훨씬 편할것 이다. 
 - model의 불안정: 대부분의 localization 불안정성은 학습초기에 box의 x, y좌표를 image에서 direct로 예측하는데서 온다.

### Dimension Clusters 

 위에서 나왔던 anchor의 hand-picked 문제를 해결하기 위해 training set의 bounding box를 k-means clustering으로 최적의 anchor dimension을 찾도록 했다. _k-means clustering은 unsupervised categorize이다. 이는 data를 분석하여 feature를 k개로 나누어 군집화한다._ 
 k-means clustring을 사용할 때 Euclidean distance는 box의 size가 커질수록 error는 더 커진다. 하지만 box의 size의 상관없이 IOU score를 높이고 싶기 때문에, distance metric을 아래와 같이 변형시켰다. 

 $$
 d(box, centroid) = 1 - IOU(box, centroid) 
 $$

 Anchor를 많이 사용하면 할수록 label과 Anchor의 Average IOU는 상승했지만, model의 complexity와 high recall을 고려하여 anchor의 개수를 5개로 결정하였다. 만약 더 큰 모델과 더 높은 성능, recall을 원한다면 anchor의 개수를 늘이는 것도 도움이 될것이다. 

### Direct location Prediction, Anchor of RPN

 original Region Proposal Network(Faster R-CNN)에서는 network predict value $(t_x, t_y)$와 anchor의 좌표 값 $(x_a, y_a, w_a, h_a)$으로 (x, y)를 계산한다. 

 $$
 \begin{align}
 & x = (t_x * w_a) - x_a \notag \\
 & y = (t_y * h_a) - y_a \notag
 \end{align}
 $$

 그래서 Faster R-CNN에서는 $t_x$가 1일 경우 anchor의 width만큼 right로 shift하고, $t_x$가 -1일 경우 같은 길이만큼 left로 shift한다.  
 위 수식은 box위치에 제한이 없어 어느 anchor box에서든 image의 모든 좌표를 예측할 수 있다. 그래서 random initialized 모델에서는 안정적으로 box를 예측하기까지 많은 시간이 걸린다. 

 x, y좌표를 직접 예측하는 것 대신에 YOLO는 좌표값을 grid cell에서 상대적으로 예측한다. logisitic activation을 통해 network의 prediction을 제한한다.
 
### Detect Box on YOLOv2 

 Network는 Feature Map의 grid cell마다 anchor개 만큼의 box를 예측한다. 
 YOLOv1와 같이 boudning box는 $(t_x, t_y, t_w, t_h, t_o)$를 예측한다. v1은 0 ~ 1사이의 값을 ground truth로 사용했다. 하지만 v2는 predict한 값을 anchor에 적용하여 image에서 box의 좌표를 직접 예측한다. 
 
 $$
 \begin{align}
    & b_x = \sigma(t_x) + c_x \notag \\
    & b_y = \sigma(t_y) + c_y \notag \\
    & b_w = p_w e^{t_w} \notag \\ 
    & b_h = p_h e^{t_h} \notag \\
    & Pr(object) * IOU(b, object) = \sigma(t_o) \notag \\
 \end{align}
 $$

 <p align="center">
 <img src="/assets/images/YOLOv2/figure3.png" width="550" height="400"/>
 </p>

 location prediction을 제약하고 parameterization하여 model이 학습하기 쉽고, 안정적이다. Anchor를 이용하며 5%의 성능향상을 이뤄냈다. $c_x, c_y$는 grid cell의 left-top corner 좌표 값이다.

 또한 grid cell에서 class를 예측하던 것을 anchor box로 옮겨, anchor box에서는 coord, objectness, class를 예측한다.

### Fine-Grained Features

 fully connected layer를 제거하고, network가 higher resolution을 가지기 위해 pooling layer를 한개 제거했다. 또한 input image의 size를 448x448에서 416x416으로 감소시켜보기도 했다. 그 이유는 feature map의 shape이 홀수가 되며 feature map grid의 정가운데 single center cell이 생기기 때문이다. Object, 특히 큰 Object는 image의 center에 나타나는 경향이 있다. input sizefmf 416x416으로 했을 때, 13x13 feature map을 얻을 수 있다. 

 현재 feature map size는 large scale의 object를 찾기엔 충분하지만, 더 작은 물체를 찾을 수 있는 가능성이 남아있다. 
 YOLO는 pass-through layer를 사용한다. 마지막 pooling전 feature를 가져와 reshape한 뒤, 마지막 feature map에 concat하는 방식이다. 정확히는 26x26x512 feature를 13x13x2048 feature map으로 reshape하여 마지막 feature map에 concatenating한다. 이는 1%의 performace 향상을 보였다. 

## Faster 

 YOLOv2가 좀더 대중적인 detection framework로 사용되기 위해서는 성능뿐만 아니라, 속도 또한 잘나와야한다. detection framework 대부분은 VGG-16을 base feature-extractor로 사용하고 있다. VGG-16은 강력하고, 정확한 classification network지만, 필요없이 complex하다. 224x224 resolution input에 30.69 BFLOP을 요구한다.  
 YOLO framework는 GoogleNet을 base를 둔 custom network를 사용한다. 이는 VGG-16보다 빠르고, 8.52 BFLOP만 사용한다. 하지만 정확도는 VGG-16보다 조금 떨어진다. 224x224 resoluiton일때, top-5 accuracy가 VGG-16 90%에 비해 88%로 조금 떨어진다. 

### Darknet-19 

 YOLOv2에서는 새로운 classification model을 제시한다. VGG model과 비슷하게 pooling step뒤에는 3x3 filter를 사용하고 channel의 개수를 2배로 늘이고, Network in Network를 따라 global average pooling을 사용한다. 최종적으로 convolutional layer 19개 pooling layer 5개로 이루어진 Darknet-19를 만들었다. 

 이는 detection의 modelling뿐만 아니라, backbone network같이 전체 네트워크를 주시하고 있었다는게 느껴진다.
 
 <p align="center">
 <img src="/assets/images/YOLOv2/table6.png" width="500" height="600"/>
 </p>

## Stronger 

 Jointly Training, detection과 classification dataset을 섞어서 data가 detection labelling이면 detection loss function을 이용해 network를 학습시키고, classification labelling이면 architecture에서 classification specific part만 학습을 시킵니다. 

### Hierarchical Classification 
 Imagenet label은 wordnet에서 가져온 것이 많아 database structure가 relative한 경우가 많다. 예를들어 "Norfolk terrirer", "Yorkshine terrior"은 모두 "terrior" 종류이고 이는 또한 "hunting dog", "dog" category에 포함된다. 그래서 wordtree를 구성하여, 가장 상위 category부터 hierarchical하게 classification을 한다. 

 아래 두 그림을 보면 이해가 갈 것이다.
 
 <p align="center">
 <img src="/assets/images/YOLOv2/figure5.png" width="500" height="400"/>
 </p>

 <p align="center">
 <img src="/assets/images/YOLOv2/figure6.png" width="600" height="700"/>
 </p>
