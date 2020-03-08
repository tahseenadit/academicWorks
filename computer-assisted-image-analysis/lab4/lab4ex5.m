close all;
clear all;
load cdata;
load landsat_data;
I2 = landsat_data;
figure;
imshow(landsat_data(:,:,[1,3,4])./255);

% Show the image
R = I2(:,:,1);
% Separate the three layers, RGB
G = I2(:,:,3);
B = I2(:,:,4);


label_im = imread('hand_training.png'); % Read image with labels
figure(7);imagesc(label_im);
label_im = imresize(label_im, [512,512]);
label_im(1:100,1:100) = 1;
label_im(101:200,101:200) = 3;
label_im(101:200,201:300) = 4;
label_im(201:512,301:512) = 2;


I6(:,:,1) = R;% Create an image with three bands/features
I6(:,:,2) = G;
I6(:,:,3) = B;
[data,class] = create_training_data(I6,label_im); % Arrange the training data into vectors
figure(14);
%imshow(I3);
scatterplot2D(data,class);
% View the training feature vectors

Itest = im2testdata(I6);
C = classify(double(Itest),double(data),double(class),'quadratic');
ImC = class2im(C,size(I6,1),size(I6,2));
figure(15);
imagesc(ImC);

[data,class] = create_training_data(I2,label_im); % Arrange the training data into vectors
figure(16);
%imshow(I3);
scatterplot2D(data,class);
% View the training feature vectors

Itest = im2testdata(I2);
C = classify(double(Itest),double(data),double(class),'quadratic');
ImC = class2im(C,size(I2,1),size(I2,2));
figure(17);
imagesc(ImC);
