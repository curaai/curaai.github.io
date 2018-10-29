# BigGAN

GAN의 충실도와 다양성을 ImageNet dataset에서 생성되는 이미지와 차이점을 구했다. 

 - GAN의 성능을 키우기 위해 parameter 개수를 4배 늘이고, batch size를 8배 늘였다. 
  - scalability를 키우기 위해 general architecture를 변경하고, conditionaling을 높이기 위해 regularization scheme를 추가하였다.
  - 'truncated trick'이라는 방식을 사용하여 variety와 fidelity의 사이값을 조정했다. 
  - 불안정성을 없애기 위해 새로운 technique를 소개한다.

 우리와 관련된 technique은 Spectral Normalization이다. 이는 Lipschitz continuity 


## Architecture
- SA_GAN의 아키텍쳐를 base로함 
- hinge loss를 사용 
- class-conditional Batchnorm 
- D with projection 
- learning rate setting per D steps per G step
- moving average 0.9999
- orthogonal initialization 
- each model trained on 128 to 512 cores of Google TPU
- compute batchnorm statistics in G across all devices, 
- 