function runAnalysisThermalBatch(input_file,output_file)

% Create a VideoReader object to connect to the file:
% ---------------------------------------------------
obj = VideoReader(input_file);

% Read the first frame (make sure the connection is good):
% --------------------------------------------------------
f = obj.read(1); %#ok<NASGU>
%f = imcrop(1,[10 20 50 60])

% Initialize the required variables:
% ----------------------------------
NoF = get(obj,'NumberOfFrames');
% idx = 1:20:NoF; % alter N in 1:N:NoF; to get # of frames in sequence
idx = 1:20:21; % alter N in 1:N:NoF; to get # of frames in sequence
N = length(idx);

% Initialize the outputs:
% -----------------------
batdata(N).count           = 0;
batdata(N).brightest       = 0;
batdata(N).n_saturated     = 0;
batdata(N).areas           = 0;
batdata(N).total_area      = 0;
batdata(N).total_intensity = 0;
batdata(N).mean_intensity  = 0;
batdata(N).frame_index     = 0;

% Analyze the video:
% ------------------
for k = 1:N
    f = obj.read(idx(k));

    % Find the annotations in the frame and remove them:
    % --------------------------------------------------
    the_letters = f(:,:,1)==255 & f(:,:,2)==255 & f(:,:,3)==255;
    J = f(:,:,1);
    J(the_letters) = 0;
    
    batdata(k) = processFrameThermal(J);
    batdata(k).frame_index = idx(k);
    
    if  mod(k,100)==0 % Save the data after every 100 frames for safety
        save(output_file,'batdata')
    end

end
save(output_file,'batdata') % Save the data one last time

% Is this needed?:

% A = [batdata.n_saturated]; %#ok<NASGU>
% 
% % Save the data:
% % --------------
% save(output_file,'A')
