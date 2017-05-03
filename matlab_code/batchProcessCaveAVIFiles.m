% batchProcessAVIFiles

% Specify the data file folder:
% -----------------------------
data_folder = 'C:\Users\David Hayman\Documents\Cambridge\CSU 2013\Video\cavethermal\test_batch\data';
data_folder = pwd; % Remove

% Read the contents of the folder and identify the AVI-files:
% -----------------------------------------------------------
data_files     = dir(data_folder);
data_files     = {data_files.name};      % All files in the folder
[~,ext]        = strtok(data_files,'.'); % Get the extensions for each file
is_avi         = strcmp(ext,'.avi');     % Identify which extensions are "AVI"
avi_files      = data_files(is_avi);     % Select only the AVI-files
avi_file_names = strtok(avi_files,'.');  % Get the file names without the extension

% Build a list of output file names, based on the input names:
% ------------------------------------------------------------
output_files = strcat('output_',avi_file_names,'.mat');

% Execute the batch processing of the files:
% ------------------------------------------
for k = 1:length(avi_files)
    runAnalysisThermalBatch(fullfile(data_folder,avi_files{k}), ...
                            fullfile(data_folder,output_files{k}))
end
