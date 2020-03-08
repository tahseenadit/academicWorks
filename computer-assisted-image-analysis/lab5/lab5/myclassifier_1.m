function S = myclassifier(im)

%This classifier is not state of the art... but should give you an idea of
%the format we expect to make it easy to keep track of your scores. Input
%is the image, output is a 1 x 3 vector of the three numbers in the image
%
%This baseline classifier tries to guess... so should score about (3^3)^-1
%on average, approx. a 4% chance of guessing the correct answer. 
%



%segment numbers 
BW = im2bw(im, 0.5);
BW2 = (bwareafilt(imcomplement(BW),[100 10000])); %Extract objects from binary image by size
%figure, imshow(BW2);
se = strel('rectangle',[3 3]); %3 3
im_erode = imerode(BW2, se); % erode to separate some connected numbers
%figure, imshow(im_erode);

%sometimes there are small areas because of the erosion -> if so remove
%small areas
CC_count = bwconncomp(im_erode);
if(CC_count.NumObjects > 3)
    im_erode = bwareafilt(im_erode,[100 10000]);
end

%determine connected components of image 
cutouts{3} = [];
CC = bwconncomp(im_erode);
stats = regionprops(CC,'BoundingBox');


%Problematic are cases, where numbers are connected -> separate just by
%separating into equal halves or thirds
%if only two numbers are connected
if(CC.NumObjects ==2)
    numPixels = cellfun(@numel,CC.PixelIdxList);
    [biggest,idx] = max(numPixels);
    rect_old = stats(idx).BoundingBox;
 
    rect1 = rect_old;
    rect1(3) = rect_old(3)/2;
    rect2 = rect1;
    rect2(1) = rect_old(1)+rect1(3);
    rect2(3) = rect_old(3)-rect_old(3)/2; %%neu
    stats(idx).BoundingBox = rect1;
    if(idx == 1)
        stats(3).BoundingBox = stats(2).BoundingBox;
        stats(idx+1).BoundingBox = rect2;
    else
        stats(idx+1).BoundingBox = rect2;
    end

end


%if all three numbers are connected
if(CC.NumObjects == 1)
    numPixels = cellfun(@numel,CC.PixelIdxList);
    [biggest,idx] = max(numPixels);
    rect_old = stats(idx).BoundingBox;
    rect1 = rect_old;  
    rect1(3) = rect_old(3)/3;
    rect2 = rect1;
    rect2(1) = rect_old(1)+rect1(3);
    rect3 = rect2;
    rect3(1) = rect2(1)+rect2(3);
    stats(idx).BoundingBox = rect1;
    stats(idx+1).BoundingBox = rect2;
    stats(idx+2).BoundingBox = rect3;
end


for i=1:3
    
    cutouts{i} = imcrop(im_erode,stats(i).BoundingBox);
    CC_cutout = bwconncomp(cutouts{i});
    if(CC_cutout.NumObjects > 1)
        numPixels = cellfun(@numel,CC_cutout.PixelIdxList);
        [biggest,idx] = min(numPixels);
        cutouts{i}(CC_cutout.PixelIdxList{idx}) = 0;
    end
end
% figure,imshow(cutouts{1});
% figure,imshow(cutouts{2});
% figure,imshow(cutouts{3});


res = [NaN NaN NaN];

%determining properties of cutouts/numbers
%eulerNumber: Number of objects in the region minus the number of holes in
%those objects -> 0 if it's a "0" and 1 if it's a "1"
%solidity: Proportion of the pixels in the convex hull that are also in the
%region (Area/ConvexArea) -> should be larger for 1s than for 2s
%aspect_ratio: height/width of bounding box -> 2 is more a square and 1 is
%a thinn rectangle.
for i=1:3
    eulerNumber = regionprops(cutouts{i},'EulerNumber');
    soliditiy = regionprops(cutouts{i},'Solidity'); 
    bounding_box = regionprops(cutouts{i},'Boundingbox');
    aspect_ratio = bounding_box(1).BoundingBox(4)/bounding_box(1).BoundingBox(3);
    
    if(eulerNumber(1).EulerNumber == 0) %number objects - number holes
        res(i) = 0;
    else
        if(aspect_ratio > 1.7)%(soliditiy(1).Solidity > 0.35)
            res(i) = 1;
        else
            res(i) = 2;
        end
    end
end

S=res;


end

