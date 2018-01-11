# paper-JCAL-multimodal-teaching-analytics
Processing, analysis and visualization code for Prieto et al.'s paper on multimodal teaching analytics at the Journal on Computer-Assisted Learning.

**Full reference**:
Prieto, L.P., Sharma, K., Kidzinski, Ł., Rodríguez‐Triana, M.J., Dillenbourg, P. (2018). Multimodal teaching analytics: Automated extraction of orchestration graphs from wearable sensor data. J Comput Assist Learn. https://doi.org/10.1111/jcal.12232

Dependencies
------------

https://github.com/floydhub/dl-docker : Docker image for easy install of software to run the CNNs, jupyter notebooks, etc.

https://github.com/kidzik/deep-features : To extract video features from an image using a CNN

http://audeering.com/technology/opensmile/#download : openSMILE 2.1.0, for audio feature extraction
... including the models from http://www.audeering.com/research-and-open-source/files/emotion-models-0.1.0.zip


Project Organization
------------

    ├── README.md          <- The top-level README for developers using this project.
    └── src                <- Source code for use in this project.
        ├── data           <- Scripts to download, preprocess or generate data
        │
        ├── models         <- Scripts to train models and then use trained models to make
        │                    predictions
        │
        └── visualization  <- Scripts to create exploratory and results oriented visualizations
