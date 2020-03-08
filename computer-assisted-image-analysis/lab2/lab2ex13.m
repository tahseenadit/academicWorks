[x,y] = meshgrid(-128:127,-128:127);
z = sqrt(x.^2+y.^2);
c = z < 40;

imdata = imread("cameraman.png");
subplot(3,1,1);
imshow(imdata);
orig = fft2(imdata);
orig = fftshift(orig);
orig = orig.*c;
i = log(1+abs(orig));
im = max(i(:));
subplot(3,1,2);
imshow(im2uint8(i/im));

f = ifftshift(orig);
f = ifft2(f);
fm = max(f(:));
fnew = abs(f/fm);
subplot(3,1,3);
imshow(fnew);