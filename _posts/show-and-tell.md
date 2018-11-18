## abstract

 기계번역과 object detection에서 영감을 받아 attention을 base로 한 이미지를 묘사하는 모델을 소개한다.

## introduction

 이미지의 caption을 생성하는 것은 장면의 핵심으 이해하는 task와 아주 가깝다. 하지만 이는 아주 어려운 일인데, 최근 Deep Learning 분야의 CNN과 RNN이 data에 대한 representing을 아주 잘하여, 가능해졌다. 또한 attention을 사용한다. Attention은 전체 이미지를 static representation으로 압축한다. feature의 큰 특징을 가지고 있는 부분을 필요에 따라 동적으로 조절할수 있게 되었다. 이는 특히 이미지에 여러 object가 상황별로 있을 때 중요해졌다. 
 
 이전의 작업에서는 image feature를 deep하게 쌓을 수록 정보를 정제하여, network가 object를 인식하였는데, caption을 묘사하는 부분에서는 오히려 결점이 되었다. low level의 feature가 정보를 보존하였다. 

## Image Caption Generation with Attention Mechanism 
### Model Details 

 vector를 bold로 표현하고 matric는 대문자로 표기한다. 

#### Encoder 

 싱글 raw 이미지를 입력으로 하여 K개의 개중 하나로 인코딩된 단어 C개의 길이만큼 만들어 낸다.
 
 CNN을 feature vector를 추출하기 위해 사용한다. extractor는 L개의 D차원의 vector를 생산한다.
 $$
 a = \{ \textbf{a}_1, \dots, \textbf{a}_L \}, a_i \in \textbf{R}^D
 $$
 우리는 feature를 fully connected에서 얻는 것보다 lower convolution에서 2-D image의 patch에 해당하는 부분을 짤라서 얻는데, 이는 feature vector 부분집합에서 decoder가 확실한 부분을 이미지에서 선택적으로 집중하기 위함이다. 

 #### Decoder: LSTM 

 우리는 LSTM network를 사용한다. 각 time step에서 context vector를 condition으로 줘 caption의 한단어씩 생산한다. 

 $$
 \start{pmatrix}
 \textbf{i}_t \\
 \textbf{f}_t \\
 \textbf{o}_t \\
 \textbf{g}_t \\
 \end{pmatrix}
= 
 \start{pmatrix}
 \sigma \\
 \sigma \\
 \sigma \\
 \tanh \\
 \end{pmatrix}
 T_{D + m + n, n}
 \start{pmatrix}
 \textbf{Ey}_{t-1} \\
 \textbf{h}_{t-1} \\
 \hat{\textbf{z}}_t \\
 \end{pmatrix} 
 $$