% batchProcessMatFiles
clear
% Specify the data file folder:
% -----------------------------
%data_folder = 'C:\[foldername]\IN_2011-2012 (Entrance)';
%data_folder = 'C:\[foldername]\IN_2012-2013 (Entrance)';
%data_folder = 'C:\[foldername]\IN_2013-2014 (Deeper)';
%data_folder = 'C:\[foldername]\IN_2013-2014 (Entrance)';
%data_folder = 'C:\[foldername]\VA_2011-2012';
%data_folder = 'C:\[foldername]\VA_2012-2013';
data_folder = 'C:\[foldername]\VA_2013-2014';

% NOTE: set the specified data folder as the Matlab file path before
% running this code

% Read the contents of the folder and identify the MAT-files:
% -----------------------------------------------------------
data_files     = dir(data_folder);
data_files     = {data_files.name};      % All files in the folder
[~,ext]        = strtok(data_files,'.'); % Get the extensions for each file
is_mat         = strcmp(ext,'.mat');     % Identify which extensions are "AVI"
mat_files      = data_files(is_mat);     % Select only the AVI-files
mat_file_names = strtok(mat_files,'.');  % Get the file names without the extension

for ix = 1:length(mat_files)   %Load all data, backwards to force preallocation
    loadedData(ix) = load(mat_file_names{ix});
end
mergedData = cat(1,loadedData);

for k = 1:length(mat_files)
    for j = 1:length(mergedData(1,k).batdata)
    y(k,j) = mergedData(1,k).batdata(1,j).n_saturated;
    %x(k,j) = mergedData(1,k).batdata(1,j).frame_index
    end
end

figure(1)
plot(y(:,:)','r+')
hold all
plot(mean(y,1))
plot(median(y,1))
plot(mode(y,1))
hold off

figure(2)
contour3(y)
surf(y)
colormap hot

figure(3)
plot(median(y,1))
hold all
plot(mean(y,1))
plot(mode(y,1))
xlabel('Time of Day (Frame since midnight)')
ylabel('# saturated pixels')

%filename = 'IN_2011-2012 (Entrance).csv';
%filename = 'IN_2012-2013 (Entrance).csv';
%filename = 'IN_2013-2014 (Deeper).csv';
%filename = 'IN_2013-2014 (Entrance).csv';
%filename = 'VA_2011-2012.csv';
%filename = 'VA_2012-2013.csv';
filename = 'VA_2013-2014.csv';


csvwrite(filename,y)
