imdata = fftshift(fft2(rand(1,5)));
subplot(4,1,1);
imshow(imdata);

imdata2 = fftshift(fft2(rand(1,6)));
subplot(4,1,2);
imshow(imdata2);

f = ifft2(ifftshift(imdata));
fm = max(f(:));
fnew = abs(f/fm);
subplot(4,1,3);
imshow(fnew);

f = ifft2(ifftshift(imdata2));
fm = max(f(:));
fnew = abs(f/fm);
subplot(4,1,4);
imshow(fnew);