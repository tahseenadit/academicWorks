function S = myclassifier(im)

%This classifier is not state of the art... but should give you an idea of
%the format we expect to make it easy to keep track of your scores. Input
%is the image, output is a 1 x 3 vector of the three numbers in the image
%
%This baseline classifier tries to guess... so should score about (3^3)^-1
%on average, approx. a 4% chance of guessing the correct answer. 
%

% Removing noise
filim = medfilt2(im, [7,7]);
thrs = graythresh(filim);
im = ~im2bw(filim,thrs);
im = bwareaopen(im,30);

% Segmenting and labeling
[L Ne] = bwlabel(im);

result = [NaN,NaN,NaN];
indx = 1;
workwithlabel = 0;

% Check if objects are joined.
if Ne == 2    
    
    [r,c] = find(L==1);
    im1=im(min(r):max(r),min(c):max(c));
    img1=imresize(im1,[28 28]);
%     figure;
%     imshow(img1);
    
    Idist = bwdist(~img1);
    Idist = -Idist;
    L1= watershed(Idist);
    L1(~img1) = 0;
    rgb = label2rgb(L1, 'jet', [.5 .5 .5]);

    CC = bwconncomp(L1);
    ctr1 = CC.NumObjects;
    
    
    [r,c] = find(L==2);
    im2=im(min(r):max(r),min(c):max(c));
    img2=imresize(im2,[28 28]);
%     figure;
%     imshow(img2);
    
    Idist = bwdist(~img2);
    Idist = -Idist;
    L2= watershed(Idist);
    L2(~img2) = 0;
    rgb = label2rgb(L2, 'jet', [.5 .5 .5]);

    CC = bwconncomp(L2);
    ctr2 = CC.NumObjects;
    
    if ctr1 > ctr2
        I1 = img1(:, 1 : end/2);
        I2 = img1(:, end/2+1 : end );
        indx = 1;
        workwithlabel = 2;
        
    else
        I1 = img2(:, 1 : end/2);
        I2 = img2(:, end/2+1 : end );
        indx = 2;
        workwithlabel = 1;
    end
    
    img3=imresize(I1,[28 28]);
%     figure;
%     imshow(img3);
    foundwhite = 0;
    ctr3= 0;
    for i = 1 : 28
        for j = 20 
            if img3(i,j) == 1 && foundwhite == 0
                ctr3 = ctr3 + 1;
                foundwhite = 1;
            elseif img3(i,j) == 0
                foundwhite = 0;
            end 
        end
    end
    if ctr3 == 3
        result(indx) = 2;
    elseif ctr3 == 2
        result(indx) = 0;
    else
        result(indx) = 1;
    end
    
    indx = indx + 1;
    img4=imresize(I2,[28 28]);
%     figure;
%     imshow(img4);
    foundwhite = 0;
    ctr4= 0;
    for i = 1 : 28
        for j = 20 
            if img4(i,j) == 1 && foundwhite == 0
                ctr4 = ctr4 + 1;
                foundwhite = 1;
            elseif img4(i,j) == 0
                foundwhite = 0;
            end 
        end
    end
    if ctr4 == 3
        result(indx) = 2;
    elseif ctr4 == 2
        result(indx) = 0;
    else
        result(indx) = 1;
    end
end

indx = 1;

startwith = 1;

if Ne == 2
    startwith = 2;
    if workwithlabel == 1
        indx = 1;
    else
        indx = 3;
    end
end

for n=startwith:Ne
        
        if Ne == 2
            [r,c] = find(L==workwithlabel);
        else
            [r,c] = find(L==n);
        end
        n1=im(min(r):max(r),min(c):max(c));  
        img=imresize(n1,[28 28]);         
%         figure;
%         imshow(img);
        foundwhite = 0;
        ctr= 0;
        for i = 1 : 28
            for j = 20 
                if img(i,j) == 1 && foundwhite == 0
                    ctr = ctr + 1;
                    foundwhite = 1;
                elseif img(i,j) == 0
                    foundwhite = 0;
                end 
            end
        end
        if ctr == 3
            result(indx) = 2;
        elseif ctr == 2
            result(indx) = 0;
        else
            result(indx) = 1;
        end
        indx = indx + 1;
end
S = result(:,1:3);
end
