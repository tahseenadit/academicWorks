im=imread("bacteria.tif");
%im=imread("coins.tif");

thresholdValue = 100;
binaryImage = im < thresholdValue; 
binaryImage = imfill(binaryImage, 'holes');
figure;
imshow(binaryImage);

[labeledImage, n] = bwlabel(binaryImage, 8);

props = regionprops('table',binaryImage,'Centroid','MajorAxisLength', 'MinorAxisLength');
centers = props.Centroid;
diameters = mean([props.MajorAxisLength props.MinorAxisLength],2);
radii = diameters/2;
hold on;
viscircles(centers,radii);
hold off;


Idist = bwdist(~binaryImage);
Idist = -Idist;
figure;
imshow(Idist, []);
L= watershed(Idist);
L(~binaryImage) = 0;
rgb = label2rgb(L, 'jet', [.5 .5 .5]);
figure;
imshow(rgb);

CC = bwconncomp(L);
numberofobjects = CC.NumObjects;

%blobMeasurements = regionprops(L, im, 'all');
%numberOfBlobs = size(blobMeasurements, 1);

areas = zeros(1, numberofobjects);

for i=1:numberofobjects
    [r, c] = size(find(L==i));
    areas(i) = r*c;
end

figure;
hist(areas(:),20);