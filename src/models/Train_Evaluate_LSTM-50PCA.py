#!/usr/bin/python

# Usage: Train_Evaluate_7MLP-100PCA.py <label> <target-variable> <data-sources> <train-set-sessions> <test-set-sessions>
# NB: Normally, use quotes around each parameter, as they are strings with commas and stuff

# The script assumes that the dataset is in the same dir as the script, and writes its output to a file <label>.RData
# The output contains the trained model (model), args = commandArgs(trailingOnly=TRUE)

import sys
import os
import numpy
import pandas
import json
import math

from keras.models import Sequential
from keras.layers import Dense
from keras.wrappers.scikit_learn import KerasClassifier
from keras.layers import LSTM, TimeDistributed, Dropout
from keras.constraints import maxnorm
from keras.optimizers import SGD
from keras.utils.np_utils import to_categorical
from keras.callbacks import ModelCheckpoint

from sklearn.metrics import confusion_matrix, roc_auc_score, accuracy_score, f1_score, cohen_kappa_score
from sklearn.cross_validation import cross_val_score
from sklearn.preprocessing import LabelEncoder
from sklearn.cross_validation import StratifiedKFold
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from sklearn import decomposition

print 'Argument List:', str(sys.argv)

if(len(sys.argv)!=6):
    sys.exit("Wrong number of arguments. Usage:\nTrain_Evaluate_LSTM-50PCA.py <label> <target-variable> <data-sources> <train-set-sessions> <test-set-sessions>")


label = sys.argv[1]
target = sys.argv[2] # either 'Activity' or 'Social'
datasourcestring = sys.argv[3] # Comma separated combinations of all,et,acc,aud,vid
trainstring = sys.argv[4]
teststring = sys.argv[5]
# # For tests
# label='7MLP-100PCA_et_GM_LOSO_Activity_1'
# target='Activity'
# datasourcestring='et'
# trainstring='case1-day1-session2-teacher1,case1-day1-session3-teacher1,case1-day1-session4-teacher1,case2-day1-session1-teacher2,case2-day1-session2-teacher2,case2-day2-session1-teacher2,case2-day2-session2-teacher2,case2-day3-session1-teacher2,case2-day3-session2-teacher2,case2-day4-session1-teacher2,case2-day4-session2-teacher2'
# teststring='case1-day1-session1-teacher1'

# We parse the data sources to take into account, and sessions for the train and test sets
features = range(0,2) # These are only the session and timestamp
sources = datasourcestring.split(",")
for source in sources:
    if(source=='all'):
        features.extend(range(2,7557))
        break
    elif(source=='et'):
        features.extend(range(2,12))
    elif(source=='acc'):
        features.extend(range(12,152))
    elif(source=='aud'):
        features.extend(range(152,6557))
    elif(source=='vid'):
        features.extend(range(6557,7557))
    else:
        sys.exit("Wrong data sources. Possible values: all,et,acc,aud,vid")
features.extend(range(7557,7559)) # Add activity and Social
print("Selected features: "+str(len(features)))

sessiontrain = trainstring.split(",") # Gives an array of the sessions to train in
sessiontest = teststring.split(",") # Gives an array of the sessions to train in

if(len(sessiontrain)==0 | len(sessiontest)==0):
    sys.exit("Wrong train/test sessions specification. Should be a comma-separated string with the sessions identificators")

path = os.path.dirname(os.path.realpath(sys.argv[0]))
# READING AND PREPARING THE DATA
processeddatadir = path
#processeddatadir = "../src/models" # TODO: Change this for the actual script
datafile = os.path.join(processeddatadir,'completeDataset.csv')
gzdatafile = os.path.join(processeddatadir,'completeDataset.csv.gz')
fulldata = pandas.DataFrame()
if(os.path.isfile(datafile)):
    fulldata = pandas.read_csv(datafile, sep=',', quotechar='"')
elif(os.path.isfile(gzdatafile)):
    fulldata = pandas.read_csv(gzdatafile, compression='gzip', sep=',', quotechar='"')
else:
    sys.exit("Data not available in the script's folder")

# Drop the useless first column
fulldata.drop(fulldata.columns[[0]],axis=1,inplace=True)

def cleanAct(value):
    if pandas.isnull(value):
        return 'Other'
    elif value=='OFF' or value=='TDT' or value=='TEC':
        return 'Other'
    else:
        return value

def cleanSoc(value):
    if pandas.isnull(value):
        return 'Other'
    else:
        return value


# We only look for predicting 4 states of activity and 3 of social, the rest (incl.NA) we bunch in 'Other' (so in the end it is a 5- and 4-class classification problem)
fulldata['Activity'] = fulldata['Activity.win'].map(cleanAct)
fulldata['Social'] = fulldata['Social.win'].map(cleanSoc)

# Drop the useless first column
fulldata.drop(fulldata.columns[[2,3,4]],axis=1,inplace=True)
print(fulldata.columns.values[0:5],"...",fulldata.columns.values[-5:])
#fulldata.head(3)

# Now the column indices match what is expected in the arguments parsed above
# * [,0]: ''session id''
# * [,1]: ''timestamp'' within the session (in ms)
# * [,2:12]: ''eyetracking'' features (mean/sd pupil diameter, nr. of long fixations, avg. saccade speed, fixation duration, fixation dispersion, saccade duration, saccade amplitude, saccade length, saccade velocity)
# * [,12:152]: ''accelerometer'' features, including X, Y, Z (mean, sd, max, min, median, and 30 FFT coefficients of each of them) and jerk (mean, sd, max, min, median, and 30 FFT coefficients of each of it)
# * [,152:6557]: ''audio'' features extracted from an audio snippet of the 10s window, using openSMILE. Includes features about whether there is someone speaking (153:163), emotion recognition models (164:184), and brute-force audio spectrum features and characteristics used in various audio recognition challenges/tasks (185:6557)
# * [,6557:7557]: ''video'' features extracted from an image taken in the middle of the window (the 1000 values of the last layer when passing the immage through a VGG pre-trained model)
# * [,7557:7559]: ''Activity,Social'' labels we want to predict


# SELECTING THE DATASET FEATURES (DATA SOURCES BEING TRIED)
data = fulldata.ix[:,features]

# We drop the non-needed target variable
if target == 'Activity':
    data.drop('Social',axis=1,inplace=True)
elif target == 'Social':
    data.drop('Activity',axis=1,inplace=True)

print(data.shape)
#data.head(3)

# SPLITTING THE DATA
test = data.loc[data['session'].isin(sessiontest)]
train = data.loc[data['session'].isin(sessiontrain)]
print(test.shape)
print(train.shape)
# Removing null values
test = test[test.notnull().all(axis=1)]
train = train[train.notnull().all(axis=1)]
print("Removing null and NAs...")
print(test.shape)
print(train.shape)


X_train = train.values[:,range(2,train.shape[1]-1)].astype(float)
Y_train = train.values[:,(train.shape[1]-1)]
X_test = test.values[:,range(2,test.shape[1]-1)].astype(float)
Y_test = test.values[:,(test.shape[1]-1)]
Y_total = data.values[:,(data.shape[1]-1)]
print(X_train.shape, Y_train.shape, X_test.shape, Y_test.shape)

#######################################################
# DO OTHER DATA TRANSFORMATIONS NEEDED, e.g. PCA, SELECT K-BEST FEATURES, etc (NORMALLY, ON THE TRAIN SET ONLY, TO BE APPLIED LATER TO THE TEST SET)
print("Transforming data... ")
k = 50
outs = len(data[target].unique())

# We standardize on the basis of the training data
scaler = StandardScaler().fit(X_train)
X_train_st = scaler.transform(X_train)
X_test_st = scaler.transform(X_test)

# # Removing zero variance features
# selector = VarianceThreshold()
# selector.fit(X_train_st)
# X_train_nz = selector.transform(X_train_st)
# X_test_nz = selector.transform(X_test_st)
# idx = numpy.where(selector.variances_ > threshold)[0] # to get the indices
# TODO: Remove highly correlated ones (according to Cohen's d?)
## From http://lucystatistics.blogspot.com.ee/2016/03/dimension-reduction.html
# c = df.corr().abs()
# so1=argsort(np.array(c))
# s = c.unstack()
# so2 = s.order(kind="quicksort")


if X_train.shape[1]>k:
    # Apply 100-component pca
    print("PCA with "+str(k)+" components")
    pca = decomposition.PCA(n_components=k)
    X_train_pca = pca.fit_transform(X_train_st)
    X_test_pca = pca.transform(X_test_st)
    print 'Variance explained:'
    print pca.explained_variance_ratio_
    print 'Total variance explained by '+str(k)+' components:'
    print sum(pca.explained_variance_ratio_)
else:
    k=X_train.shape[1]
    pca = decomposition.PCA(n_components=k)
    X_train_pca = pca.fit_transform(X_train_st)
    X_test_pca = pca.transform(X_test_st)

#######################################################


# PREPARING THE DATA FOR KERAS TRAINING
# One hot encoding of the response variable (using dummy variables)

# encode class values as integers
encoder = LabelEncoder()
encoder.fit(Y_total)
encoded_Y_train = encoder.transform(Y_train)
# convert integers to dummy variables (i.e. one hot encoded)
dummy_y_train = to_categorical(encoded_Y_train)
#encoder.fit(Y_test)
encoded_Y_test = encoder.transform(Y_test)
# convert integers to dummy variables (i.e. one hot encoded)
dummy_y_test = to_categorical(encoded_Y_test)

##################### MODEL BUILDING --- CUSTOMIZE THIS ################
# Apply dropout regularization, it is overfitting!
def create_deeper_dropout_decay_PCA(k, outs):
    # create model
    model = Sequential()
    model.add(Dropout(0.2, input_shape=(k,)))
    model.add(Dense(300, init='uniform', activation='tanh'))
    model.add(Dropout(0.2))
    model.add(Dense(300, init='uniform', activation='tanh'))
    model.add(Dropout(0.2))
    model.add(Dense(80, init='uniform', activation='tanh'))
    model.add(Dropout(0.2))
    model.add(Dense(80, init='uniform', activation='tanh'))
    model.add(Dropout(0.2))
    model.add(Dense(20, init='uniform', activation='tanh'))
    model.add(Dropout(0.2))
    model.add(Dense(20, init='uniform', activation='tanh'))
    model.add(Dropout(0.2))
    model.add(Dense(outs, init='uniform', activation='sigmoid'))
    # Compile model, with larger learning rate and momentum, as recommended by the original paper
    sgd = SGD(lr=0.1, momentum=0.8, decay=0.0001, nesterov=False)
    model.compile(loss='categorical_crossentropy', optimizer=sgd, metrics=['accuracy'])
    return model

# LUIS
def create_LSTM_PCA(batch_size = 1, trainShape1=100, outs=5):
    # create and fit the LSTM network
    model = Sequential()
    # stateful LSTM!
    model.add(LSTM(200, batch_input_shape=(batch_size, 1, trainShape1), 
                   return_sequences=True, stateful=True))
    model.add(Dropout(0.2))
    model.add(LSTM(100, 
                   return_sequences=True, stateful=True))
    model.add(Dropout(0.2))
    model.add(LSTM(50, 
                   return_sequences=False, stateful=True))
    model.add(Dropout(0.2))
    model.add(Dense(50, activation='tanh'))
    model.add(Dropout(0.2))
    model.add(Dense(20, activation='tanh'))
    model.add(Dropout(0.2))
    model.add(Dense(outs, activation='softmax'))
    # Compile model
    model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
    return model

# LUKASZ
def create_LSTM(hidden = 50, lstm_layers = 1, input_dim = 100, output_dim = 5):
    model = Sequential()
    model.add(LSTM(input_shape = (input_dim,),input_dim=input_dim, output_dim=hidden, return_sequences=True))
    for i in range(lstm_layers-1):
        model.add(LSTM(output_dim = hidden, return_sequences=True))
    model.add(TimeDistributed(Dense(output_dim, activation='sigmoid')))
    model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
    return model

# evaluate baseline model with standardized dataset
# fix random seed for reproducibility
seed = 66
numpy.random.seed(seed)
#estimators = []
#estimators.append(('standardize', StandardScaler()))
#estimators.append(('mlp', KerasClassifier(build_fn=create_baseline, nb_epoch=10, batch_size=10, verbose=1)))
# We define a pipeline of estimators, in which first the scaler is fitted to the data, then the MLP is applied
#pipeline = Pipeline(estimators)
#kfold = StratifiedKFold(y=Y_train, n_folds=3, shuffle=True, random_state=seed)

#model = create_baseline()
#model = create_deeper_dropout_decay_PCA(k, outs)
model = create_LSTM(input_dim = k, output_dim = outs, lstm_layers = 2, hidden = 32)
#print model.summary()
#############################################################################

# To save the best model
# serialize model to JSON
model_json = model.to_json()
modelfile = os.path.join(path,label+".model.json")
with open(modelfile, "w") as json_file:
    json_file.write(model_json)
filepath=os.path.join(path,label+".weights.hdf5")
# Define that the accuracy in cv is monitored, and that weights are stored in a file when max accuracy is achieved
checkpoint = ModelCheckpoint(filepath, monitor='val_acc', verbose=1, save_best_only=True, mode='max')
callbacks_list = [checkpoint]

def slice_timeseries(X,Y,length=32):
    # TODO: consider more randomized slicing
    n = int(math.floor(X.shape[0] / length))
    maxt = n * length
    X = X[0:maxt,:].reshape((n, length, X.shape[1]))
    Y = Y[0:maxt,:].reshape((n, length, Y.shape[1]))
    return X,Y

X_train_pca_reshaped, dummy_y_train_reshaped = slice_timeseries(X_train_pca, dummy_y_train, length=16)
X_test_pca_reshaped, dummy_y_test_reshaped = slice_timeseries(X_test_pca, dummy_y_test, length=32)

# Fit the model
history = model.fit(X_train_pca_reshaped, dummy_y_train_reshaped, validation_data=(X_test_pca_reshaped,dummy_y_test_reshaped),
                    nb_epoch=2000, batch_size=16, verbose=0, callbacks=callbacks_list)
#results = cross_val_score(pipeline, X_train, dummy_y_train, cv=kfold)
#print("Standardized data Acc (in CV training data): %.2f%% (%.2f%%)" % (results.mean()*100, results.std()*100))
# evaluate the model
#scores = pipeline.evaluate(X_test, dummy_y_test)
#print pipeline.metrics_names[1]
#print scores[1]*100
# For other metrics, see http://machinelearningmastery.com/metrics-evaluate-machine-learning-algorithms-python/

# Other performance/accuracy metrics
Y_pred = model.predict(X_test_pca.reshape((1,) + X_test_pca.shape ))[0,:,:]

# Accuracy
print('Accuracy:')
acc = accuracy_score(numpy.argmax(dummy_y_test, axis=1), numpy.argmax(Y_pred, axis=1))
print(acc)


# Confusion matrix
cm = confusion_matrix(numpy.argmax(dummy_y_test, axis=1), numpy.argmax(Y_pred, axis=1))
numpy.set_printoptions(precision=2)
print('Confusion matrix:')
print(cm)

# AUC
roc = None
try:
  roc = roc_auc_score(dummy_y_test, Y_pred, average='macro')
except:
  pass
print('AUC score:')
print(roc)

# F1
f1= f1_score(numpy.argmax(dummy_y_test, axis=1), numpy.argmax(Y_pred, axis=1), average='macro')
print('F1 score:')
print(f1)


# KAppa?
kappa = cohen_kappa_score(numpy.argmax(dummy_y_test, axis=1), numpy.argmax(Y_pred, axis=1))
print('Kappa score:')
print(kappa)

#import matplotlib
#matplotlib.use('Agg')
#import matplotlib.pyplot as plt
# summarize history for accuracy
#plt.plot(history.history['acc'])
#plt.plot(history.history['val_acc'])
#plt.title('model accuracy')
#plt.ylabel('accuracy')
#plt.xlabel('epoch')
#plt.legend(['train','test'], loc='upper left')
#plt.show()
# summarize history for loss
#plt.plot(history.history['loss'])
#plt.plot(history.history['val_loss'])
#plt.title('model loss')
#plt.ylabel('loss')
#plt.xlabel('epoch')
#plt.legend(['train','test'], loc='upper left')
#plt.show()

# The models are stored already... what about the performance metrics?
# Store them in a csv
perfdata = [
    {'label': label, 'cm': json.dumps(cm.tolist()), 'acc': acc, 'auc': roc, 'f1': f1, 'kappa': kappa}
]
# Later, to recover cm:
# cm = np.array(json.loads(text))
df = pandas.DataFrame(perfdata)
filename = os.path.join(path,label+".perf.csv")
df.to_csv(filename, index=False, encoding='utf-8')
