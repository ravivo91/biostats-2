% Read the data from the AllSubjectsResultsFlash file
filePath = 'AllSubjectsResultsFlash.xlsx'; % Update with the correct path
allData = readtable(filePath);

% Extract the axial length and variables from the table
axial_length = allData.Axial_Length_OD;
variables = {
    allData.OFF_Flash_ERG_b_wave_Time, allData.OFF_Flash_ERG_PhNR_Time,... 
    allData.ON_Flash_ERG_a_wave_Time, allData.ON_Flash_ERG_b_wave_Time,...
    allData.ON_Flash_ERG_PhNR_Time, allData.OFF_Flash_ERG_b_wave_Amp,... 
    allData.OFF_Flash_ERG_PhNR_Amp, allData.ON_Flash_ERG_a_wave_Amp,... 
    allData.ON_Flash_ERG_b_wave_Amp, allData.ON_Flash_ERG_PhNR_Amp...
};
variableNames = {
    'OFF Flash ERG b wave Time', 'OFF Flash ERG PhNR Time',... 
    'ON Flash ERG a wave Time', 'ON Flash ERG b wave Time',... 
    'ON Flash ERG PhNR Time', 'OFF Flash ERG b wave Amp',... 
    'OFF Flash ERG PhNR Amp', 'ON Flash ERG a wave Amp',... 
    'ON Flash ERG b wave Amp', 'ON Flash ERG PhNR Amp'...
};

% Define the path for the folder to save the figures
figuresFolder = 'Flash Correlation'; % Update with the correct path

% Create the figures folder if it doesn't exist
if ~exist(figuresFolder, 'dir')
    mkdir(figuresFolder);
end

% Loop through each variable and create a plot
for i = 1:length(variables)
    fig = figure;  % Assign the figure to a variable 'fig'
    scatter(axial_length, variables{i}); % Scatter plot of the variable against axial length
    xlabel('Axial Length');
    ylabel(variableNames{i});
    title(['Axial Length vs ' variableNames{i}]);
    
    % Linear regression to find best-fit line
    coeffs = polyfit(axial_length, variables{i}, 1);
    fittedX = linspace(min(axial_length), max(axial_length), 200);
    fittedY = polyval(coeffs, fittedX);
    
    % Add the best-fit line to the plot
    hold on;
    plot(fittedX, fittedY, 'r-', 'LineWidth', 2);
    legend('Data', 'Best-fit line');
    hold off;
    
    % Save the figure to the specified folder with a unique name
    saveFileName = fullfile(figuresFolder, sprintf('%s.png', variableNames{i}));
    exportgraphics(fig, saveFileName); % Use 'fig' here to refer to the correct figure handle
    close(fig); % Close the figure after saving to avoid clutter
end
