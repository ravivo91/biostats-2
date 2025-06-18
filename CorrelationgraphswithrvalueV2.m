% Define the path for the folder to save the figures
figuresFolder = 'Flicker Correlation'; % Update with the correct path

% Create the figures folder if it doesn't exist
if ~exist(figuresFolder, 'dir')
    mkdir(figuresFolder);
end

% Read data from the file
filePath = 'AllSubjectsResultsFlicker.xlsx'; % Update with the correct path
allData = readtable(filePath);

% Assuming 'AxialLength' is a column in the file
axial_length = allData.Axial_Length_OD;

% Define the variables based on the headers you've provided
variables = {
    allData.OFF_Flicker_ERG_Amp_10Hz, allData.ON_Flicker_ERG_Amp_10Hz,...
    allData.OFF_Flicker_VEP_Amp_10Hz, allData.ON_Flicker_VEP_Amp_10Hz,...
    allData.OFF_Flicker_ERG_Amp_20Hz, allData.ON_Flicker_ERG_Amp_20Hz,...
    allData.OFF_Flicker_VEP_Amp_20Hz, allData.ON_Flicker_VEP_Amp_20Hz,...
    allData.OFF_Flicker_ERG_Amp_40Hz, allData.ON_Flicker_ERG_Amp_40Hz,...
    allData.OFF_Flicker_VEP_Amp_40Hz, allData.ON_Flicker_VEP_Amp_40Hz,...
    allData.OFF_Flicker_ERG_Phase_10Hz, allData.ON_Flicker_ERG_Phase_10Hz,...
    allData.OFF_Flicker_VEP_Phase_10Hz, allData.ON_Flicker_VEP_Phase_10Hz,...
    allData.OFF_Flicker_ERG_Phase_20Hz, allData.ON_Flicker_ERG_Phase_20Hz,...
    allData.OFF_Flicker_VEP_Phase_20Hz, allData.ON_Flicker_VEP_Phase_20Hz,...
    allData.OFF_Flicker_ERG_Phase_40Hz, allData.ON_Flicker_ERG_Phase_40Hz,...
    allData.OFF_Flicker_VEP_Phase_40Hz, allData.ON_Flicker_VEP_Phase_40Hz...
};

variableNames = {
    'OFF Flicker ERG Amp 10Hz', 'ON Flicker ERG Amp 10Hz',...
    'OFF Flicker VEP Amp 10Hz', 'ON Flicker VEP Amp 10Hz',...
    'OFF Flicker ERG Amp 20Hz', 'ON Flicker ERG Amp 20Hz',...
    'OFF Flicker VEP Amp 20Hz', 'ON Flicker VEP Amp 20Hz',...
    'OFF Flicker ERG Amp 40Hz', 'ON Flicker ERG Amp 40Hz',...
    'OFF Flicker VEP Amp 40Hz', 'ON Flicker VEP Amp 40Hz',...
    'OFF Flicker ERG Phase 10Hz', 'ON Flicker ERG Phase 10Hz',...
    'OFF Flicker VEP Phase 10Hz', 'ON Flicker VEP Phase 10Hz',...
    'OFF Flicker ERG Phase 20Hz', 'ON Flicker ERG Phase 20Hz',...
    'OFF Flicker VEP Phase 20Hz', 'ON Flicker VEP Phase 20Hz',...
    'OFF Flicker ERG Phase 40Hz', 'ON Flicker ERG Phase 40Hz',...
    'OFF Flicker VEP Phase 40Hz', 'ON Flicker VEP Phase 40Hz'...
};

% Prepare to store correlation results
correlationResults = cell(length(variables), 4);
correlationResults(1,:) = {'Variable', 'Pearson r', 'p-value', 'Sample Size'};

% Loop through each variable and create a plot
for i = 1:length(variables)
    % Clean the data by removing NaNs
    idx = ~isnan(axial_length) & ~isnan(variables{i});
    axial_length_clean = axial_length(idx);
    variable_clean = variables{i}(idx);

    if length(axial_length_clean) > 2  % Proceed only if there are enough points
        % Calculate Pearson correlation coefficient manually
        n = length(axial_length_clean);
        sum_x = sum(axial_length_clean);
        sum_y = sum(variable_clean);
        sum_xy = sum(axial_length_clean .* variable_clean);
        sum_x2 = sum(axial_length_clean .^ 2);
        sum_y2 = sum(variable_clean .^ 2);

        numerator = (n * sum_xy) - (sum_x * sum_y);
        denominator = sqrt((n * sum_x2 - sum_x^2) * (n * sum_y2 - sum_y^2));
        r = numerator / denominator;

        % Fisher Z transformation for p-value (using erf approximation)
        zr = 0.5 * log((1 + r) / (1 - r));
        se = 1 / sqrt(n - 3);
        z = zr / se;
        p = 1 - erf(abs(z) / sqrt(2));

        % Store correlation results
        correlationResults{i+1, 1} = variableNames{i};
        correlationResults{i+1, 2} = r;
        correlationResults{i+1, 3} = p;
        correlationResults{i+1, 4} = n;

        % Create scatter plot with linear fit
        fig = figure('Visible','off');  % Create a new figure for each plot
        scatter(axial_length_clean, variable_clean); % Scatter plot of the variable against axial length
        xlabel('Axial Length');
        ylabel(variableNames{i});
        title(['Axial Length vs ' variableNames{i}]);

        % Linear regression to find best-fit line
        coeffs = polyfit(axial_length_clean, variable_clean, 1);
        fittedX = linspace(min(axial_length_clean), max(axial_length_clean), 200);
        fittedY = polyval(coeffs, fittedX);

        % Add the best-fit line to the plot
        hold on;
        plot(fittedX, fittedY, 'r-', 'LineWidth', 2);
        legend('Data', 'Best-fit line');

        % Add r and p-value to the plot as text annotation
        textPosX = min(axial_length_clean) + 0.05 * (max(axial_length_clean) - min(axial_length_clean));
        textPosY = max(variable_clean) - 0.05 * (max(variable_clean) - min(variable_clean));
        text(textPosX, textPosY, sprintf('r = %.2f\np = %.4f', r, p), 'FontSize', 10, 'BackgroundColor', 'white', 'EdgeColor', 'black', 'Margin', 5);

        hold off;

        % Save the figure to the specified folder with a unique name
        saveFileName = fullfile(figuresFolder, sprintf('%s.png', variableNames{i}));
        saveas(fig, saveFileName);
    else
        % If not enough data points, store NaN
        correlationResults{i+1, 1} = variableNames{i};
        correlationResults{i+1, 2} = NaN;
        correlationResults{i+1, 3} = NaN;
        correlationResults{i+1, 4} = length(axial_length_clean);
    end
end

% Save correlation results to CSV
resultsTable = cell2table(correlationResults(2:end,:), 'VariableNames', correlationResults(1,:));
writetable(resultsTable, fullfile(figuresFolder, 'CorrelationResults.csv'));
