function batdata = processFrameThermal(J)

% Initialize the outputs:
% -----------------------
batdata.count           = 0;
batdata.brightest       = 0;
batdata.n_saturated     = 0;
batdata.areas           = 0;
batdata.total_area      = 0;
batdata.total_intensity = 0;
batdata.mean_intensity  = 0;
batdata.frame_index     = []; % Frame index - overwritten later

% Determine the threshold value for the segmentation:
% ---------------------------------------------------
thresh = max(0.4,graythresh(J));

% Segment the image and analyze the result:
% -----------------------------------------
bw = im2bw(J,thresh);
if any(bw(:))
    bw = imfill(bw,'holes');
    bw = imopen(bw,ones(5,5));
    bw = bwareaopen(bw,40);
    [~,n] = bwlabel(bw);
    
    if n>0
        stats = regionprops(bw,J,'Area','PixelValues');
        pv = cat(1,stats.PixelValues);

        batdata.count           = n;
        batdata.brightest       = max(pv)/255;
        batdata.n_saturated     = sum(pv==255);
        batdata.areas           = [stats.Area];
        batdata.total_area      = sum(batdata.areas);
        batdata.total_intensity = sum(pv)/255;
        batdata.mean_intensity  = batdata.total_intensity/batdata.total_area;
    end
end