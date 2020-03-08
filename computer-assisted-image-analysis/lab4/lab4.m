close all;
load cdata;
figure(1);
plot(cdata(:,1),cdata(:,2),'.');
%plot(cdata(:,1),'.');
%plot(cdata(:,2),'.');

I = imread('handBW.pnm');
figure(2);imshow(I);
figure(3);imhist(I);
% Read the image
% Show the image
% Show the histogram

figure(4);
mtresh(I,100,135);

I2 = imread('hand.pnm');
% Read the image
figure(5);
imshow(I2);
% Show the image
R = I2(:,:,1);
% Separate the three layers, RGB
G = I2(:,:,2);
B = I2(:,:,3);
figure(6);
plot3(R(:),G(:),B(:),'.') % 3D scatterplot of the RGB data

label_im = imread('hand_training.png'); % Read image with labels
figure(7);imagesc(label_im);
% View the training areas

I3(:,:,1) = G;% Create an image with two bands/features
I3(:,:,2) = B;
[data,class] = create_training_data(I3,label_im); % Arrange the training data into vectors
figure(8);
%imshow(I3);
scatterplot2D(data,class);
% View the training feature vectors

Itest = im2testdata(I3);
C = classify(double(Itest),double(data),double(class));
ImC = class2im(C,size(I3,1),size(I3,2));
figure(9);
imagesc(ImC);


% exercise 4

I4(:,:,1) = G;% Create an image with single bands/features
[data,class] = create_training_data(I4,label_im); % Arrange the training data into vectors
figure(10);
%imshow(I3);
scatter(data,class);
% View the training feature vectors

Itest = im2testdata(I4);
C = classify(double(Itest),double(data),double(class));
ImC = class2im(C,size(I4,1),size(I4,2));
figure(11);
imagesc(ImC);

I5 = rgb2gray(I2);
[data,class] = create_training_data(I5,label_im); % Arrange the training data into vectors
figure(12);
%imshow(I3);
scatter(data,class);
% View the training feature vectors


Itest = im2testdata(I5);
C = classify(double(Itest),double(data),double(class));
ImC = class2im(C,size(I5,1),size(I5,2));
figure(13);
imagesc(ImC);


I6(:,:,1) = R;% Create an image with two bands/features
I6(:,:,2) = G;
I6(:,:,3) = B;
[data,class] = create_training_data(I6,label_im); % Arrange the training data into vectors
figure(14);
%imshow(I3);
scatterplot2D(data,class);
% View the training feature vectors

Itest = im2testdata(I6);
C = classify(double(Itest),double(data),double(class));
ImC = class2im(C,size(I6,1),size(I6,2));
figure(15);
imagesc(ImC);
